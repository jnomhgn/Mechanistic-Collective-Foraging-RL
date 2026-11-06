#### Setup ####

setwd(here::here())

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(tidybayes)
library(posterior)
library(cmdstanr)

source(file.path("code", "pipeline_config.R"))
pipeline_mode <- Sys.getenv("PIPELINE_MODE", unset = "full")
options(
	pipeline_mode = pipeline_mode,
	pipeline_config = make_pipeline_config(pipeline_mode)
)

resultsdir <- file.path("results", "diagnostics")
if(!dir.exists(resultsdir)){dir.create(resultsdir, recursive = TRUE)}

# Results folders and model definitions per condition
conditions <- list(
  NC = list(fitdir = file.path("results", "rl", "nocatches", "modelcomp", "adaptive"),
            getmodels = file.path("code", "rl", "nocatches", "functions", "getmodels.R")),
  C  = list(fitdir = file.path("results", "rl", "catches", "modelcomp"),
            getmodels = file.path("code", "rl", "catches", "functions", "getmodels.R"))
)

# Factor level labels
d <- read.csv(file.path("data", "processed", "data_discrete_1s.csv"),
              colClasses = c(rep(NA, 8), rep("character", 2), rep(NA, 4)))
d <- apply_pipeline_data_filter(d)
max.levels <- sort(unique(d$max))
ratio.levels <- sort(unique(d$ratio))


#### Social learning weights per cell ####

# Condition NC: winning model and its environment-varying parameters
source(conditions$NC$getmodels)
load(file.path(conditions$NC$fitdir, "modelcomp.Rdata"))
winner.nc <- gsub(".hierarch", "", unname(winner), fixed = TRUE)
models.fixed <- getmodels(hierarch = F)
pop.names <- names(models.fixed$free.pars.pop[[grep(paste0("^", winner.nc, "\\.fixed$"), unlist(models.fixed$name))]])
weight.pars <- unique(sub("\\[.*$", "", grep("\\[", pop.names, value = TRUE)))
fit.nc <- readRDS(file.path(conditions$NC$fitdir, paste0(winner.nc, ".hierarch.fit.rds")))

# Mean and 90% HPDI of each weight in each max x ratio cell
draws.nc <- posterior::as_draws_matrix(fit.nc$draws(weight.pars))
weights.nc <- as.data.frame(draws.nc) %>%
  mutate(draw = row_number()) %>%
  pivot_longer(-draw, names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("weight", "m", "r"), sep = "\\[|,|\\]", extra = "drop") %>%
  group_by(weight, m, r) %>%
  mean_hdi(value, .width = .9) %>%
  transmute(condition = "NC", weight,
            max = max.levels[as.integer(m)], ratio = ratio.levels[as.integer(r)],
            mean = value, lower = .lower, upper = .upper)

# Condition C: same steps
source(conditions$C$getmodels)
load(file.path(conditions$C$fitdir, "modelcomp.Rdata"))
winner.c <- gsub(".hierarch", "", unname(winner), fixed = TRUE)
models.fixed <- getmodels(hierarch = F)
pop.names <- names(models.fixed$free.pars.pop[[grep(paste0("^", winner.c, "\\.fixed$"), unlist(models.fixed$name))]])
weight.pars <- unique(sub("\\[.*$", "", grep("\\[", pop.names, value = TRUE)))
fit.c <- readRDS(file.path(conditions$C$fitdir, paste0(winner.c, ".hierarch.fit.rds")))

draws.c <- posterior::as_draws_matrix(fit.c$draws(weight.pars))
weights.c <- as.data.frame(draws.c) %>%
  mutate(draw = row_number()) %>%
  pivot_longer(-draw, names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("weight", "m", "r"), sep = "\\[|,|\\]", extra = "drop") %>%
  group_by(weight, m, r) %>%
  mean_hdi(value, .width = .9) %>%
  transmute(condition = "C", weight,
            max = max.levels[as.integer(m)], ratio = ratio.levels[as.integer(r)],
            mean = value, lower = .lower, upper = .upper)

weights <- bind_rows(weights.nc, weights.c)
write.csv(weights, file = file.path(resultsdir, "weights.csv"), row.names = F)


#### Plot ####

# Condition NC
p.nc.max <- ggplot(weights.nc, aes(x = max, y = mean, ymin = lower, ymax = upper, col = ratio, group = ratio)) +
  geom_pointrange(position = position_dodge(width = .3)) +
  geom_line(position = position_dodge(width = .3)) +
  scale_color_viridis_d(name = "Catch Ratio") +
  facet_grid(weight ~ ., scales = "free_y") +
  labs(x = "Max Catch", y = "Social learning weight") +
  theme_linedraw(base_size = 11)

p.nc.ratio <- ggplot(weights.nc, aes(x = ratio, y = mean, ymin = lower, ymax = upper, col = max, group = max)) +
  geom_pointrange(position = position_dodge(width = .3)) +
  geom_line(position = position_dodge(width = .3)) +
  scale_color_viridis_d(name = "Max Catch") +
  facet_grid(weight ~ ., scales = "free_y") +
  labs(x = "Catch Ratio", y = "Social learning weight") +
  theme_linedraw(base_size = 11)

p.nc <- ggarrange(p.nc.max, p.nc.ratio, ncol = 2)
ggexport(p.nc, width = 1920, height = 1080, filename = file.path(resultsdir, "weights_nc.jpeg"))

# Condition C
p.c.max <- ggplot(weights.c, aes(x = max, y = mean, ymin = lower, ymax = upper, col = ratio, group = ratio)) +
  geom_pointrange(position = position_dodge(width = .3)) +
  geom_line(position = position_dodge(width = .3)) +
  scale_color_viridis_d(name = "Catch Ratio") +
  facet_grid(weight ~ ., scales = "free_y") +
  labs(x = "Max Catch", y = "Social learning weight") +
  theme_linedraw(base_size = 11)

p.c.ratio <- ggplot(weights.c, aes(x = ratio, y = mean, ymin = lower, ymax = upper, col = max, group = max)) +
  geom_pointrange(position = position_dodge(width = .3)) +
  geom_line(position = position_dodge(width = .3)) +
  scale_color_viridis_d(name = "Max Catch") +
  facet_grid(weight ~ ., scales = "free_y") +
  labs(x = "Catch Ratio", y = "Social learning weight") +
  theme_linedraw(base_size = 11)

p.c <- ggarrange(p.c.max, p.c.ratio, ncol = 2)
ggexport(p.c, width = 1920, height = 1080, filename = file.path(resultsdir, "weights_c.jpeg"))
