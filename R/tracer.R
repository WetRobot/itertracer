




#' @rdname tracer_callbacks
#' @export
#' @details
#' - `tracer_callbacks_defaults` returns a list of default callback functions
#'   used by [tracer] when the user does not define custom callbacks
#' @importFrom git2r repository commits in_repository
#' @importFrom utils sessionInfo
tracer_callbacks_defaults <- function() {
  callbacks <- list(
    sample_chains = function(chain_space, chain_iterator_fun, n_parallel = 1L) {
      n_chains <- length(chain_space)
      stopifnot(
        length(n_chains) == 1,
        n_chains > 0,
        n_chains %% 1 == 0,
        .Platform[["OS.type"]] %in% c("unix", "windows")
      )

      if (n_parallel > 1) {
        if (.Platform[["OS.type"]] == "unix") {
          parallel::mclapply(X = chain_space,
                             FUN = chain_iterator_fun,
                             mc.cores = n_parallel)
        } else if (.Platform[["OS.type"]] == "windows") {
          cl <- parallel::makeCluster(spec = n_parallel)
          on.exit(parallel::stopCluster(cl))

          parallel::clusterCall(cl, fun = function(x) .libPaths(x), .libPaths())

          gr <- as.environment(as.list(globalenv()))
          parent.env(gr) <- globalenv()
          parallel::clusterExport(cl,
                                  varlist = ls(envir = gr, all.names = TRUE),
                                  envir = gr)

          parallel::parLapply(cl = cl, X = chain_space, fun = chain_iterator_fun)
        } else {
          stop("No parallel computation method defined for .Platform[[OS.type]]",
               " = ", deparse(.Platform[["OS.type"]]))
        }

      } else {
        lapply(chain_space, chain_iterator_fun)
      }
    },
    on_init = function(arg_list) {
      n_burn <- arg_list[["n_burn"]]
      n_skip <- arg_list[["n_skip"]]

      t <- as.character(Sys.time())
      n_iter <- arg_list[["n_iter"]]
      n_chains <- arg_list[["n_chains"]]

      if (!dir.exists("tracer")) {
        dir.create("tracer")
      }
      sample_dir <- paste0("tracer/", gsub("[^0-9]", "_", t), "/")
      dir.create(sample_dir)

      msg <- paste0("* tracer: ", t,": starting processing ", n_iter,
                    " iterations in ", n_chains, " chain(s); saving iters ",
                    "to directory ", deparse(sample_dir))
      msg <- paste0(
        msg, "; using seed(s) ",
        paste0(deparse(arg_list[["seeds"]]), collapse = "")
      )
      msg <- paste0(
        msg, "; n_burn = ", n_burn, ", n_skip = ", n_skip
      )
      message(msg)
      asterisks <- paste0(rep("*", 80), collapse = "")
      msg <- c(
        "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n",
        asterisks,
        "\n",
        asterisks,
        "\n\n\n",
        msg, "\n",
        "* tracer: sessionInfo():\n"
      )
      cat(msg, file = "tracer_log.txt", append = TRUE)
      sample_dir_msg <- msg[which(grepl("\\*", msg))[1]:length(msg)]
      cat(sample_dir_msg,
          file = paste0(sample_dir, "tracer_log.txt"), append = FALSE)

      sink(file = paste0(sample_dir, "tracer_log.txt"), append = TRUE)
      print(utils::sessionInfo())
      if (git2r::in_repository()) {
        cat("\n * tracer: git status:\n")
        cat("** last commit sha:", git2r::commits(n = 1L)[[1L]][["sha"]], "\n")
        summary(git2r::repository())
      }
      sink()
      sink(file = "tracer_log.txt", append = TRUE)
      print(utils::sessionInfo())
      if (git2r::in_repository()) {
        cat("\n * tracer: git status:\n")
        cat("** last commit sha:", git2r::commits(n = 1L)[[1L]][["sha"]], "\n")
        summary(git2r::repository())
      }
      sink()

      arg_list[["init_time"]] <- proc.time()
      arg_list[["sample_dir"]] <- sample_dir
      arg_list
    },
    param_order = function(arg_list) {
      sample(names(arg_list[["param_list"]]))
    },
    on_iter_start = function(arg_list) {
      arg_list[["iter_start_time"]] <- proc.time()
      arg_list
    },
    save_iter = function(arg_list) {
      param_list <- arg_list[["param_list"]]
      sample_dir <- arg_list[["sample_dir"]]
      chain_no <- arg_list[["chain_no"]]
      iter_no <- arg_list[["iter_no"]]
      n_burn <- arg_list[["n_burn"]]
      n_iter <- arg_list[["n_iter"]]
      n_nonburn <- n_iter - n_burn
      n_skip <- arg_list[["n_skip"]]

      if (iter_no %in% seq(from = n_iter, to = 1L, by = -(n_skip + 1L))) {
        lapply(names(param_list), function(param_nm) {
          current_iter_path <- paste0(
            sample_dir, param_nm, "_chain_", chain_no,
            "_iter_", iter_no, ".rds"
          )
          saveRDS(
            object = param_list[[param_nm]],
            file = current_iter_path,
            compress = FALSE
          )
          if (iter_no > n_nonburn) {
            del_iter_path <- paste0(
              sample_dir, param_nm, "_chain_", chain_no,
              "_iter_", iter_no - n_nonburn, ".rds"
            )
            if (file.exists(del_iter_path)) {
              file.remove(del_iter_path)
            }
          }

          NULL
        })
      }
      NULL
    },
    on_iter_finish = function(arg_list) {
      iter_no <- arg_list[["iter_no"]]
      n_iter <- arg_list[["n_iter"]]
      init_time <- arg_list[["init_time"]]
      chain_no <- arg_list[["chain_no"]]
      iter_start_time <- arg_list[["iter_start_time"]]
      sample_dir <- arg_list[["sample_dir"]]

      iter_quantiles <- seq(from = 0L, to = n_iter, length.out = 11L)[2:10]
      if (iter_no %in% iter_quantiles || iter_no %in% 1:10 || iter_no %% 1000L == 0L) {

        total_elapsed <- time_elapsed(init_time)
        iter_elapsed <- time_elapsed(iter_start_time)
        ETA_in_seconds <- seconds_elapsed(iter_start_time) * (n_iter - iter_no)
        ETA_in_duration <- format_seconds(ETA_in_seconds)
        time_now <- Sys.time()
        ETA_in_time <- time_now + ETA_in_seconds

        msg <- paste0(
          "* tracer: ", time_now, ": ",
          "chain ", chain_no, " ",
          "iteration ", iter_no, " ",
          "complete; ",
          "elapsed this iter: ", iter_elapsed, "; ",
          "total elapsed: ", total_elapsed, "; ",
          "ETA: ", ETA_in_time, " (duration: ", ETA_in_duration, ")"
        )
        message(msg)
        msg <- paste0(msg, "\n")
        cat(msg, file = "tracer_log.txt", append = TRUE)
        cat(msg, file = paste0(sample_dir, "tracer_log.txt"), append = TRUE)
        invisible(NULL)
      }
      arg_list
    },
    break_if = function(arg_list) {
      return(FALSE) # default behaviour: no breaking. below is an example.

      init_time <- arg_list[["init_time"]]
      iter_no <- arg_list[["iter_no"]]

      s_from_start <- seconds_elapsed(proc_time = init_time)
      max_time <- 86400L # 24 hrs in seconds
      went_over_time <- s_from_start >= max_time
      if (went_over_time) {
        msg <- paste0(
          "* tracer: reached max_time = ", max_time, " at iter = ", iter_no, " at ",
          as.character(Sys.time())
        )
        message(msg)
        cat(msg, "\n", file = "tracer_log.txt", append = TRUE)
      }
      went_over_time
    },
    on_chain_completion = function(arg_list) {
      init_time <- arg_list[["init_time"]]
      n_iter <- arg_list[["n_iter"]]
      n_chains <- arg_list[["n_chains"]]
      chain_no <- arg_list[["chain_no"]]
      iter_no <- arg_list[["iter_no"]]
      sample_dir <- arg_list[["sample_dir"]]

      t <- as.character(Sys.time())
      msg <- paste0("* tracer: ", t,": chain ", chain_no, " of ", n_iter,
                    " iterations completed in ",
                    time_elapsed(init_time))
      message(msg)
      cat("\n", msg, "\n", file = "tracer_log.txt", append = TRUE)
      cat("\n", msg, "\n", file = paste0(sample_dir, "tracer_log.txt"),
          append = TRUE)
      arg_list
    },
    on_sampling_completion = function(arg_list) {
      init_time <- arg_list[["init_time"]]
      n_iter <- arg_list[["n_iter"]]
      n_chains <- arg_list[["n_chains"]]
      sample_dir <- arg_list[["sample_dir"]]

      t <- as.character(Sys.time())
      msg <- paste0("* tracer: ", t,": all ", n_chains," chain(s) and ", n_iter,
                    " iterations completed in ",
                    time_elapsed(init_time))
      message(msg)
      cat("\n", msg, "\n", file = "tracer_log.txt", append = TRUE)
      cat("\n", msg, "\n", file = paste0(sample_dir, "tracer_log.txt"),
          append = TRUE)
      arg_list
    },
    on_exit = function(tracer_object, arg_list, tracer_arg_list) {
      sample_dir <- arg_list[["sample_dir"]]
      if (!is.null(tracer_object) && dir.exists(sample_dir)) {
        tracer_object_path <- paste0(sample_dir, "tracer_object.rds")
        tracer_log_path <- paste0(sample_dir, "tracer_log.txt")
        saveRDS(object = tracer_object, file = tracer_object_path)
        sample_dir_file_paths <-  dir(sample_dir, full.names = TRUE)
        sample_dir_file_paths <- normalizePath(sample_dir_file_paths)
        keep_file_paths <- c(tracer_object_path, tracer_log_path)
        keep_file_paths <- normalizePath(keep_file_paths)
        del_file_paths <- setdiff(
          sample_dir_file_paths, keep_file_paths
        )
        if (length(del_file_paths)) {
          file.remove(del_file_paths)
        }
      }
      invisible(NULL)
    }
  )
}


#' @title [tracer] Callback Functions
#' @description
#'
#' Handle callback functions passed to [tracer].
#'


#' @rdname tracer_callbacks
#' @export
#' @param sample_chains
#' function to go through the separate chains; e.g. an application of
#' [parallel::mclapply]
#' @param on_init
#' called on initiation of [tracer]
#' @param param_order
#' when stepping through parameters within an iteration, this function is called
#' to determine the order of the parameters
#' @param on_iter_start
#' called on start of an iteration before anything else
#' @param save_iter
#' called to save the result of an iteration
#' @param on_iter_finish
#' called on finishing an iteration
#' @param break_if
#' iterating is terminated when this function returns TRUE; you can e.g.
#' set a maximum time this way
#' @param on_chain_completion
#' called when all the iterations have been completed in a chain
#' @param on_sampling_completion
#' called when all chains have finished / have been interrupted
#' @param on_exit
#' called on exit even when an error is raised by using [on.exit]
#' @details
#' - `tracer_callbacks` accepts functions as arguments and returns a list
#'   of callback functions to be used by [tracer], supplemented by
#'   defaults from [tracer_callbacks_defaults] as necessary
tracer_callbacks <- function(
  sample_chains = NULL,
  on_init = NULL,
  param_order = NULL,
  on_iter_start = NULL,
  save_iter = NULL,
  on_iter_finish = NULL,
  break_if = NULL,
  on_chain_completion = NULL,
  on_sampling_completion = NULL,
  on_exit = NULL
) {
  mc <- match.call()

  defaults <- tracer_callbacks_defaults()

  fun_nms <- names(formals(tracer_callbacks))
  fun_list <- mget(fun_nms)
  fun_list[vapply(fun_list, is.null, logical(1))] <- NULL
  fun_nms <- names(fun_list)

  lapply(names(fun_list), function(fun_nm) {
    user_fun <- fun_list[[fun_nm]]
    if (!is.function(user_fun)) {
      msg <- paste0(
        deparse(fun_nm), " must be a function, was of class(es) ",
        deparse(class(user_fun))
      )
      stop(simpleError(msg, mc))
    }
    user_fun_arg_nms <- names(formals(user_fun))
    expected_arg_nms <- names(formals(defaults[[fun_nm]]))

    miss_arg_nms <- setdiff(expected_arg_nms, user_fun_arg_nms)
    if (length(miss_arg_nms)) {
      msg <- paste0(
        "Missing following arguments for user-supplied callback '", fun_nm,
        "': ", deparse(miss_arg_nms)
      )
      stop(simpleError(msg, call = mc))
    }
    NULL
  })

  callbacks <- defaults
  callbacks[names(fun_list)] <- fun_list
  callbacks
}
stopifnot(
  identical(names(formals(tracer_callbacks)), names(tracer_callbacks_defaults()))
)


random_seeds <- function(n) {
  stopifnot(
    length(n) == 1,
    is.numeric(n),
    n > 0,
    n %% 1 == 0
  )
  as.integer(stats::runif(n = n, min = 1L, max = 1e9L + 1L))
}


#' @title Trace Iterations
#' @description
#'
#' Run an iterative process and collect a trace of the iterations.
#'
#' @param param_init `[list]`
#'
#' - a named list of arrays for where to start sampling for each parameter
#' - there must be an element in `step_funs` for each element here
#'   (matching by names)
#' - all elements must be one of
#'   + numeric array
#'   + list of arrays, with one array for each chain --- hence the length
#'     must be equal to `n_chains`
#' @param step_funs [list]``
#'
#' - a list of functions for sampling each random variables' step (one tracer
#'   iteration = one step for each variable)
#' - matched with `param_init` based on names
#' - the order of calling each step fun is determined by callback `param_order`
#' @param fixed `[list]`
#'
#' - passed to `step_funs` and `callbacks` in `arg_list[["fixed"]]`
#' - `fixed` is a list, so if you modify any of it's elements in
#'   `step_funs` or `callbacks`, you actually create a new list and
#'   you cannot pass changes in `fixed` to other functions
#' @param shared `[environment, list]`
#'
#' - `list`: a list of `environment` objects, one for each chain
#' - `environment`:
#'   + `shared` is passed to all `step_funs` and `callbacks`
#'     in `arg_list[["shared"]]`
#'   + since it's an environment, you can save any results computed in one
#'     function to be used by others (**NOTE**: in parallel computation you of
#'     course cannot share results between different threads)
#' @param callbacks `[list]`
#'
#' - named list of functions called at various points of the process (pre-iter,
#'   post-iter, etc.)
#' - supplied to [tracer_callbacks]
#' @param n_iter number of iterations
#' @param n_chains number of separate chains to sample
#' @param n_burn number of iterations out of \code{n_iter} to run before
#' samples will be saved and included in output
#' @param n_skip keep every `(n_skip + 1)`'th sample
#' @param n_parallel number of parallel processes to run
#' @param seeds vector of numbers or NULL; if not NULL, each of these is
#' passed to each separate chain; if NULL these are picked automatically
#' (and each chain has a different seed)
#' @param read_cache if TRUE, will read the trace at the end of iterating
#' into R; else the samples will be left in the trace folder only --- but
#' see `clean_cache`
#' @param clean_cache if TRUE, will delete any folder created to store samples
#' @examples
#' ## beta-binomial Gibbs sampling, multi-core
#'
#' g <- tracer(
#'   fixed = list(n = 20L),
#'   param_init = lapply(list(x = 10, theta = 0.5), as.array),
#'   step_funs = list(
#'     x = function(arg_list) {
#'       rbinom(
#'         n = 1,
#'         size = arg_list[["fixed"]][["n"]],
#'         prob = arg_list[["param_list"]][["theta"]]
#'       )
#'     },
#'     theta = function(arg_list) {
#'       rbeta(
#'         n = 1,
#'         shape1 = 2 + arg_list[["param_list"]][["x"]],
#'         shape2 = 4 + arg_list[["fixed"]][["n"]] - arg_list[["param_list"]][["x"]]
#'       )
#'     }
#'   ),
#'   n_chains = 1L,
#'   n_iter = 300,
#'   n_burn = 50L,
#'   n_skip = 4L,
#'   n_parallel = 1L
#' )
#'
#' @import data.table
#' @export
tracer <- function(
  param_init,
  step_funs,
  fixed = list(),
  shared = new.env(parent = emptyenv()),
  callbacks = list(),
  n_iter = 200L,
  n_chains = 1L,
  n_parallel = 1L,
  n_burn = 0L,
  n_skip = 0L,
  seeds = NULL,
  read_cache = TRUE,
  clean_cache = FALSE
) {
  stopifnot(
    inherits(param_init, "list"),
    names(param_init) %in% names(step_funs)
  )

  lapply(names(param_init), function(param_nm) {
    param <- param_init[[param_nm]]
    is_list <- inherits(param, "list")
    is_numeric_array <- is.array(param) && is.numeric(param)
    is_data_table <- data.table::is.data.table(param)
    if (!is_list && !is_numeric_array && !is_data_table) {
      stop("param_init[[", deparse(param_nm), "]] neither numeric array, ",
           "data.table, nor list")
    }
    if (is_list) {
      if (length(param) != n_chains) {
        stop("length(param_init[[", deparse(param_nm),"]]) != n_chains")
      }
      elem_is_numeric_array <- vapply(param, function(elem) {
        is.array(elem) && is.numeric(elem)
      }, logical(1))
      elem_is_dt <- vapply(param, data.table::is.data.table, logical(1))
      if (!all(elem_is_numeric_array | elem_is_dt)) {
        stop("elements param_init[[", deparse(param_nm), "]][",
             deparse(which(!elem_is_numeric_array))
             ,"] were not numeric arrays or data.tables")
      }
    }
  })

  stopifnot(
    inherits(step_funs, "list"),
    vapply(step_funs, is.function, logical(1)),
    vapply(step_funs, function(fun) {
      identical(names(formals(fun)), "arg_list")
    }, logical(1)),
    names(step_funs) %in% names(param_init),

    inherits(fixed, "list"),

    inherits(callbacks, "list"),
    vapply(callbacks, is.function, logical(1)),

    inherits(shared, c("environment", "list")),

    length(n_iter) == 1,
    n_iter > 0,
    n_iter %% 1 == 0,

    length(n_chains) == 1,
    n_chains > 0,
    n_chains %% 1 == 0,

    length(n_burn) == 1,
    n_burn >= 0,
    n_burn %% 1 == 0,
    n_iter > n_burn,

    length(n_chains) == 1,
    n_chains >= 1,
    n_chains %% 1 == 0,

    length(n_parallel) == 1,
    n_parallel >= 1,
    n_parallel %% 1 == 0,

    length(n_skip) == 1,
    n_skip >= 0,
    n_skip %% 1 == 0,
    n_iter > n_skip,

    is.null(seeds) || (is.numeric(seeds) && length(seeds) == n_chains),

    length(read_cache) == 1,
    read_cache %in% c(TRUE, FALSE),

    length(clean_cache) == 1,
    clean_cache %in% c(TRUE, FALSE)
  )

  if (inherits(shared, "list")) {
    stopifnot(
      all(vapply(shared, is.environment, logical(1L))),
      length(shared) == n_chains
    )
  }

  # prep -----------------------------------------------------------------------
  call <- match.call()
  tracer_arg_list <- mget(names(formals("tracer")))

  callbacks <- do.call(tracer_callbacks, callbacks, quote = TRUE)

  arg_list <- list(
    param_list = param_init,
    n_iter = n_iter,
    n_chains = n_chains,
    n_burn = n_burn,
    n_skip = n_skip,
    fixed = fixed,
    seeds = if (is.null(seeds)) random_seeds(n = n_chains) else seeds,
    iter_set = rev(as.integer(
      seq(from = n_iter, to = n_burn + 1L, by = -(n_skip + 1L))
    )),
    shared = shared
  )
  arg_list <- callbacks[["on_init"]](arg_list)
  arg_list[["param_init"]] <- lapply(param_init, function(param) {
    if (length(dim(param)) == 1L) {
      as.vector(param)
    } else {
      param
    }
  })

  out <- NULL
  on.exit(
    callbacks[["on_exit"]](
      tracer_object = out,
      arg_list = arg_list,
      tracer_arg_list = tracer_arg_list
    )
  )

  # sampling -------------------------------------------------------------------
  gc()
  chain_space <- 1:n_chains
  arg_lists <- callbacks[["sample_chains"]](
    chain_space = chain_space,
    n_parallel = n_parallel,
    chain_iterator_fun = function(chain_no) {
      set.seed(seed = arg_list[["seeds"]][chain_no])
      arg_list[["chain_no"]] <- chain_no
      arg_list[["param_list"]] <- lapply(
        arg_list[["param_list"]],
        function(param_list_elem) {
          if (inherits(param_list_elem, "list")) {
            param_list_elem <- param_list_elem[[chain_no]]
          }
          param_list_elem
        }
      )
      if (inherits(arg_list[["shared"]], "list")) {
        arg_list[["shared"]] <- arg_list[["shared"]][[chain_no]]
      }

      for (iter_no in seq(max(arg_list[["iter_set"]]))) {
        sampling_start_time <- proc.time()
        arg_list[["iter_no"]] <- iter_no
        arg_list <- callbacks[["on_iter_start"]](arg_list)

        param_nms <- callbacks[["param_order"]](arg_list)
        for (param_nm in param_nms) {
          arg_list[["param_list"]][[param_nm]] <- eval(substitute(
            step_funs[[param_nm]](arg_list),
            list(param_nm = param_nm)
          ))
        }

        callbacks[["save_iter"]](arg_list)

        if (callbacks[["break_if"]](arg_list)) {
          break()
        }

        arg_list <- callbacks[["on_iter_finish"]](arg_list)

      }

      arg_list <- callbacks[["on_chain_completion"]](arg_list)

      arg_list
    }
  )

  arg_list[["param_init"]] <- lapply(arg_list[["param_init"]], function(obj) {
    if (is.numeric(obj)) {
      obj <- as.array(obj)
    }
    obj
  })
  arg_list <- callbacks[["on_sampling_completion"]](arg_list)

  gc()

  # read results from storage --------------------------------------------------
  trace_list <- NULL
  if (read_cache) {
    trace_list <- read_trace_files(
      dir = arg_list[["sample_dir"]],
      iter_set = arg_list[["iter_set"]],
      param_nm_set = names(arg_list[["param_list"]]),
      chain_set = chain_space
    )
  }
  if (clean_cache) {
    unlink(arg_list[["sample_dir"]], recursive = TRUE)
  }

  # final touches --------------------------------------------------------------
  arg_call_nms <- intersect(names(tracer_arg_list), names(arg_list))
  tracer_arg_list[arg_call_nms] <- arg_list[arg_call_nms]
  out <- list(
    call = match.call(),
    call_args = tracer_arg_list,
    trace_list = trace_list
  )
  class(out) <- c("tracer", "list")

  return(out)
}



#' @export
#' @importFrom utils str
print.tracer <- function(x, ...) {
  cat("--- tracer object ---\n")
  cat("params:\n")
  param_nms <- names(x[["trace_list"]])
  lapply(param_nms, function(param_nm) {
    cat(paste0("  ", param_nm, ": "))
    utils::str(x[["trace_list"]][[param_nm]])
  })
  for (elem_nm in c("n_iter", "n_burn", "n_skip", "n_chains", "seeds")) {
    cat(elem_nm, ": ", deparse(x[["call_args"]][[elem_nm]]), "\n", sep = "")
  }
  invisible(NULL)
}
