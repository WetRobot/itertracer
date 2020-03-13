
read_trace_files <- function(
  dir = NULL,
  param_nm_set = NULL,
  iter_set = NULL,
  chain_set = NULL,
  pattern = "^%%PARAM%%\\Q_chain_\\E%%CHAIN%%\\Q_iter_\\E%%ITER%%\\Q.rds\\E$"
) {
  requireNamespace("data.table")
  requireNamespace("stringr")
  if (is.null(dir)) {
    dir <- utils::tail(dir("gibbs_trace/", full.names = TRUE), 1)
    message("* read_trace_files: dir was NULL, selected dir = ", deparse(dir))
  }

  stopifnot(
    length(dir) == 1,
    is.character(dir),
    dir.exists(dir),

    is.null(param_nm_set) || is.character(param_nm_set),

    is.null(iter_set) || is.integer(iter_set),

    is.null(chain_set) || is.integer(chain_set),

    length(pattern) == 1,
    is.character(pattern),
    grepl("\\Q%%PARAM%%\\E", pattern),
    grepl("\\Q%%CHAIN%%\\E", pattern),
    grepl("\\Q%%ITER%%\\E", pattern),
    substr(pattern, 1, 1) == "^",
    substr(pattern, nchar(pattern), nchar(pattern)) == "$"
  )

  param_nm_re <- "([a-zA-Z]+)"
  if (!is.null(param_nm_set)) {
    param_nm_re <- paste0("(\\\\Q", param_nm_set, "\\\\E)", collapse = "|")
    param_nm_re <- paste0("(", param_nm_re, ")")
  }
  iter_re <- "\\\\d+"
  chain_re <- "\\\\d+"
  if (!is.null(chain_set)) {
    chain_re <- paste0("(", chain_set, ")", collapse = "|")
    chain_re <- paste0("(", chain_re, ")")
  }

  keep_re <- gsub("%%PARAM%%", param_nm_re, pattern)
  keep_re <- gsub("%%ITER%%", iter_re, keep_re)
  keep_re <- gsub("%%CHAIN%%", chain_re, keep_re)
  info <- data.table::data.table(
    file_name = dir(path = dir, pattern = keep_re)
  )
  data.table::set(info, j = "file_path",
                  value = normalizePath(paste0(dir, "/", info[["file_name"]])))

  param_nm_res <- unlist(strsplit(
    gsub("(%%CHAIN%%)|(%%ITER%%)", "\\\\d+", pattern),
    split = "%%PARAM%%"
  ))
  file_param_nms <- sub(param_nm_res[2], "",
                        sub(param_nm_res[1], "", info[["file_name"]]))
  if (is.null(param_nm_set)) {
    param_nm_set <- sort(unique(file_param_nms))
  } else {
    stopifnot(
      length(param_nm_set) > 0,
      is.character(param_nm_set)
    )
  }

  info[, "PARAM" := file_param_nms]
  data.table::setkeyv(info, names(info))
  info <- info[info[["PARAM"]] %in% param_nm_set, ]

  iter_res <- unlist(strsplit(gsub(
    "%%PARAM%%",
    paste0("(", paste0("(\\\\Q", param_nm_set, "\\\\E)", collapse = "|"), ")"),
    gsub("(%%CHAIN%%)", "\\\\d+", pattern)
  ), split = "%%ITER%%"))
  file_iters <- as.integer(
    sub(iter_res[2], "", sub(iter_res[1], "", info[["file_name"]]))
  )

  if (is.null(iter_set)) {
    iter_set <- sort(unique(file_iters))
  } else {
    stopifnot(
      is.numeric(iter_set),
      iter_set %% 1 == 0,
      iter_set > 0,
      length(iter_set) > 0
    )
  }

  info[, "ITER" := file_iters]
  info <- info[info[["ITER"]] %in% iter_set, ]

  chain_res <- unlist(strsplit(gsub(
    "%%PARAM%%",
    paste0("(", paste0("(\\\\Q", param_nm_set, "\\\\E)", collapse = "|"), ")"),
    gsub("(%%ITER%%)", "\\\\d+", pattern)
  ), split = "%%CHAIN%%"))
  file_chains <- as.integer(
    sub(chain_res[2], "", sub(chain_res[1], "", info[["file_name"]]))
  )
  if (is.null(chain_set)) {
    chain_set <- sort(unique(file_chains))
  } else {
    stopifnot(
      is.numeric(chain_set),
      chain_set %% 1 == 0,
      chain_set > 0,
      length(chain_set) > 0
    )
  }

  info[, "CHAIN" := file_chains]
  info <- info[info[["CHAIN"]] %in% chain_set, ]

  min_allowed_iter <- min(info[
    j = lapply(.SD, min), .SDcols = "ITER", keyby = c("CHAIN", "PARAM")
    ][[1L]])
  max_allowed_iter <- max(info[
    j = lapply(.SD, min), .SDcols = "ITER", keyby = c("CHAIN", "PARAM")
    ][[1L]])
  info <- info[
    data.table::between(info[["ITER"]], min_allowed_iter, max_allowed_iter),
    ]

  test_list <- list(
    test_pattern = gsub(
      "%%PARAM%%",
      paste0("(", param_nm_set, ")", collapse = "|"),
      gsub("(%%CHAIN%%)|(%%ITER%%)", "\\\\d+", pattern)
    )
  )

  eval(substitute(stopifnot(
    grepl(
      pattern = test_pattern,
      x = info[["file_name"]]
    )
  ), env = test_list))

  no_file_name <- is.na(info[["file_name"]])
  n_missing_file_paths <- info[no_file_name, .N]
  if (n_missing_file_paths > 0) {
    on.exit(print(info[no_file_name, ]))
    stop(n_missing_file_paths, " file paths did not exist; ",
         "see table printout below.")
  }

  param_nm_set <- unique(info[["PARAM"]])
  iter_set <- unique(info[["ITER"]])
  chain_set <- unique(info[["CHAIN"]])
  samples <- lapply(param_nm_set, function(param_nm) {

    first_sample <- readRDS(file = info[
      i = info[["PARAM"]] == param_nm & !duplicated(info[["PARAM"]]),
      j = file_path[1]
      ])
    if (data.table::is.data.table(first_sample)) {
      out <- cbind(chain = integer(0L), iter = integer(0L), first_sample[0L, ])
      assign_expr <- parse(
        text = "out <- rbind(
        out, cbind(chain = chain_pos, iter = iter_pos, readRDS(file_path))
        )"
      )[[1L]]
    } else {
      sample_dim <- dim(as.array(first_sample))
      arr_dim <- c(sample_dim, length(iter_set), length(chain_set))
      out <- array(first_sample[1] - first_sample[1], dim = arr_dim)
      assign_expr <- parse(text = paste0(
        "out[", paste0(rep(",", length(sample_dim)), collapse = " "),
        "iter_pos, chain_pos] <- readRDS(file_path)"
      ))[[1]]
    }
    ENV <- new.env()

    j_dt <- data.table::setDT(list(PARAM = param_nm))
    for (chain_pos in seq_along(chain_set)) {
      data.table::set(j_dt, j = "CHAIN", value = chain_set[chain_pos])
      for (iter_pos in seq_along(iter_set)) {
        data.table::set(j_dt, j = "ITER", value = iter_set[iter_pos])
        file_path <- info[
          i = j_dt,
          on = names(j_dt),
          j = file_path
          ]
        eval(assign_expr)
      }
    }

    out
  })
  names(samples) <- param_nm_set

  samples
}





read_latest_trace_files <- function(
  dir,
  n_latest_iters = 1000L,
  param_nm_set = NULL,
  chain_set = NULL,
  pattern = "%%PARAM%%\\Q_chain_\\E%%CHAIN%%\\Q_iter_\\E%%ITER%%\\Q.rds\\E"
) {
  stopifnot(
    is.integer(n_latest_iters),
    length(n_latest_iters) == 1,
    n_latest_iters > 0
  )

  keep_pattern <- gsub("%%PARAM%%", "[a-zA-Z]+", pattern)
  keep_pattern <- gsub("%%ITER%%", "[0-9]+", keep_pattern)
  keep_pattern <- gsub("%%CHAIN%%", "[0-9]+", keep_pattern)
  dt <- data.table::data.table(
    file_name = dir(dir, pattern = keep_pattern)
  )

  iter_res <- unlist(strsplit(
    gsub(
      "%%CHAIN%%",
      "[0-9]+",
      gsub("%%PARAM%%", "[a-zA-Z]+", pattern)
    ),
    split = "%%ITER%%"
  ))
  data.table::set(
    dt,
    j = "iter",
    value = sub(iter_res[2], "", sub(iter_res[1], "", dt[["file_name"]]))
  )
  max_iter <- max(as.integer(dt[["iter"]]))
  stopifnot(
    max_iter > n_latest_iters
  )
  message("* read_latest_trace_files: found ", max_iter, " iters")
  iter_set <- (max_iter - n_latest_iters + 1L):max_iter
  do.call(read_trace_files, mget(names(formals(read_trace_files))))
}











