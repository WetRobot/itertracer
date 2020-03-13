



seconds_elapsed <- function(proc_time) {
  stopifnot(
    inherits(proc_time, "proc_time")
  )
  (proc.time() - proc_time)["elapsed"]
}





format_seconds <- function(seconds) {

  if (seconds < 1) {
    t <- paste0(round(1000L * seconds), " ms")
  } else if (seconds < 60) {
    t <- formatC(seconds, digits = 2L, flag = "0", format = "f")
    t <- paste0(t, " s")
  } else {
    seconds <- round(seconds)
    minutes <- seconds %/% 60L
    seconds <- seconds %% 60L
    hours <- minutes %/% 60L
    days <- hours %/% 24L
    hours <- hours %% 24L
    minutes <- minutes %% 60L

    days <- formatC(days, width = 2L, flag = "0")
    hours <- formatC(hours, width = 2L, flag = "0")
    minutes <- formatC(minutes, width = 2L, flag = "0")
    seconds <- formatC(seconds, width = 2L, flag = "0")

    t <- paste0(days, ":", hours, ":", minutes, ":", seconds)
  }
  t
}





time_elapsed <- function(proc_time) {
  seconds <- seconds_elapsed(proc_time)
  format_seconds(seconds)
}





simplify_array_list <- function(array_list) {
  stopifnot(
    is.list(array_list),
    vapply(array_list, function(arr) {
      identical(dim(arr), dim(array_list[[1]]))
    }, logical(1))
  )
  n_iter <- length(array_list)
  out <- array_list
  if (is.numeric(array_list[[1]])) {
    stopifnot(
      vapply(array_list, function(arr) {
        all(dim(arr) == dim(array_list[[1]]))
      }, logical(1))
    )

    array_list[] <- lapply(array_list, as.array)

    d <- dim(array_list[[1]])
    n_dim <- length(d)

    out <- array(NA + array_list[[1]][1], dim = c(d, n_iter))
    s <- 1:(prod(d))
    max_s <- s[length(s)]
    wh <- s - max_s
    for (i in 1:n_iter) {
      wh <- wh + max_s
      out[wh] <- array_list[[i]]
    }

  }
  out
}


