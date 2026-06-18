library(cmdstanr)
setwd(here::here())

stan_files <- list.files("code", pattern = "\\.stan$", recursive = TRUE, full.names = TRUE)

for (f in stan_files) {
  comp_condition <- ifelse(grepl("alone", f), "alone", ifelse(grepl("nocatches", f), "nocatches", "catches"))
  comp_model <- gsub("\\.stan$", "", basename(f))
  message(sprintf("Compiling model %s of %s: %s in %s", which(stan_files == f), length(stan_files), comp_model, comp_condition))
  tryCatch(
    capture.output(cmdstan_model(f, quiet = TRUE), type = "output"),
    error = function(e) message("Failed: ", f, "\n", conditionMessage(e))
  )
}
