`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

get_pipeline_config <- function() {
  getOption("pipeline_config", list())
}

get_pipeline_value <- function(..., default = NULL) {
  keys <- c(...)
  value <- get_pipeline_config()

  for (key in keys) {
    if (is.null(value) || is.null(value[[key]])) {
      return(default)
    }
    value <- value[[key]]
  }

  value %||% default
}

apply_pipeline_data_filter <- function(d) {
  session_max <- get_pipeline_value("data", "session_max", default = NULL)
  trial_max <- get_pipeline_value("data", "trial_max", default = NULL)

  if (!is.null(session_max) && "session" %in% names(d)) {
    d <- d |> dplyr::filter(session <= session_max)
  }

  if (!is.null(trial_max) && "trial" %in% names(d)) {
    d <- d |> dplyr::filter(trial <= trial_max)
  }

  d
}

make_pipeline_config <- function(mode = c("full", "test")) {
  mode <- match.arg(mode)

  full <- list(
    data = list(
      session_max = NULL,
      trial_max = NULL
    ),
    behavioral = list(
      chains = 4,
      cores = 4,
      iter = 3000,
      warmup = 2000,
      refresh = 10
    ),
    bayesianforager = list(
      nsim = 100,
      workers = max(1L, floor(parallel::detectCores() / 2))
    ),
    rl = list(
      alone = list(
        modelcomp = list(
          chains = 4,
          cores = 4,
          iter = 4000,
          warmup = 2000,
          refresh = 100,
          postpredict_nsim = 100,
          sessions = 18,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75)
        ),
        parrecov = list(
          nsim = 10,
          chains = 4,
          cores = 4,
          iter = 2000,
          warmup = 1000,
          refresh = 100,
          sessions = 18,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75, 90, 105)
        ),
        modelrecov = list(
          nsim = 10,
          chains = 1,
          cores = 1,
          iter = 2000,
          warmup = 1000,
          refresh = 100,
          sessions = 18,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75, 90, 105)
        )
      ),
      nocatches = list(
        numsims = list(
          nsim = 100,
          sessions = 18,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75),
          grid_step = 0.01
        ),
        modelcomp = list(
          chains = 4,
          cores = 4,
          iter = 4000,
          warmup = 2000,
          refresh = 100,
          postpredict_nsim = 100,
          sessions = 18,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75)
        ),
        modelrecov = list(
          nsim = 10,
          chains = 1,
          cores = 1,
          iter = 2000,
          warmup = 1000,
          refresh = 100,
          sessions = 18,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75, 90, 105)
        )
      ),
      catches = list(
        numsims = list(
          nsim = 100,
          sessions = 18,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75),
          grid_step = 0.01
        ),
        modelcomp = list(
          chains = 4,
          cores = 4,
          iter = 4000,
          warmup = 2000,
          refresh = 100,
          postpredict_nsim = 100,
          sessions = 18,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75)
        ),
        parrecov = list(
          nsim = 10,
          chains = 4,
          cores = 4,
          iter = 2000,
          warmup = 1000,
          refresh = 100,
          sessions = 18,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75, 90, 105)
        ),
        modelrecov = list(
          nsim = 10,
          chains = 1,
          cores = 1,
          iter = 2000,
          warmup = 1000,
          refresh = 100,
          sessions = 18,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75, 90, 105)
        )
      )
    )
  )

  test <- list(
    data = list(
      session_max = 2,
      trial_max = 36
    ),
    behavioral = list(
      chains = 2,
      cores = 2,
      iter = 50,
      warmup = 25,
      refresh = 10
    ),
    bayesianforager = list(
      nsim = 10,
      workers = 1
    ),
    rl = list(
      alone = list(
        modelcomp = list(
          chains = 2,
          cores = 2,
          iter = 50,
          warmup = 25,
          refresh = 10,
          postpredict_nsim = 5,
          sessions = 2,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75)
        ),
        parrecov = list(
          nsim = 3,
          chains = 2,
          cores = 2,
          iter = 50,
          warmup = 25,
          refresh = 10,
          sessions = 2,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75)
        ),
        modelrecov = list(
          nsim = 2,
          chains = 1,
          cores = 1,
          iter = 50,
          warmup = 25,
          refresh = 10,
          sessions = 2,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75)
        )
      ),
      nocatches = list(
        numsims = list(
          nsim = 5,
          sessions = 2,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75),
          grid_step = 0.5
        ),
        modelcomp = list(
          chains = 2,
          cores = 2,
          iter = 50,
          warmup = 25,
          refresh = 10,
          postpredict_nsim = 5,
          sessions = 2,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75)
        ),
        modelrecov = list(
          nsim = 2,
          chains = 1,
          cores = 1,
          iter = 50,
          warmup = 25,
          refresh = 10,
          sessions = 2,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75)
        )
      ),
      catches = list(
        numsims = list(
          nsim = 5,
          sessions = 2,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75),
          grid_step = 0.5
        ),
        modelcomp = list(
          chains = 2,
          cores = 2,
          iter = 50,
          warmup = 25,
          refresh = 10,
          postpredict_nsim = 5,
          sessions = 2,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75)
        ),
        parrecov = list(
          nsim = 3,
          chains = 2,
          cores = 2,
          iter = 50,
          warmup = 25,
          refresh = 10,
          sessions = 2,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75)
        ),
        modelrecov = list(
          nsim = 2,
          chains = 1,
          cores = 1,
          iter = 50,
          warmup = 25,
          refresh = 10,
          sessions = 2,
          trials = 12,
          nplayers = 5,
          durations_vec = c(75)
        )
      )
    )
  )

  if (mode == "full") full else test
}
