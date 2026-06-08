library(cmdstanr)
setwd(here::here())

stan_files <- list.files("code", pattern = "\\.stan$", recursive = TRUE, full.names = TRUE)

for (f in stan_files) {
  message("Compiling: ", f)
  tryCatch(
    cmdstan_model(f),
    error = function(e) message("Failed: ", f, "\n", conditionMessage(e))
  )
}
