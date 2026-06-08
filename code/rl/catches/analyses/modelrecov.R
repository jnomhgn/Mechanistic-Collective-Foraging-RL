#### Setup ####

source(file.path("code", "pipeline_config.R"))

chains = get_pipeline_value("rl", "catches", "modelrecov", "chains", default = 1)
cores = get_pipeline_value("rl", "catches", "modelrecov", "cores", default = 1)
iter = get_pipeline_value("rl", "catches", "modelrecov", "iter", default = 2000)
warmup = get_pipeline_value("rl", "catches", "modelrecov", "warmup", default = 1000)
refresh = get_pipeline_value("rl", "catches", "modelrecov", "refresh", default = 100)
nsim = get_pipeline_value("rl", "catches", "modelrecov", "nsim", default = 10)
exp_sessions = get_pipeline_value("rl", "catches", "modelrecov", "sessions", default = 18)
exp_trials = get_pipeline_value("rl", "catches", "modelrecov", "trials", default = 12)
exp_nplayers = get_pipeline_value("rl", "catches", "modelrecov", "nplayers", default = 5)
exp_durations = get_pipeline_value("rl", "catches", "modelrecov", "durations_vec", default = c(75, 90, 105))

# Source functions
dir_functions <- file.path("code", "rl", "catches", "functions")
function.list = file.path(dir_functions, list.files(dir_functions))
sapply(function.list, source, .GlobalEnv)

# Setup directories
if(!dir.exists(file.path("results", "rl", "catches", "modelrecov"))){dir.create(file.path("results", "rl", "catches", "modelrecov"), recursive = TRUE)}


resultsdir <- file.path("results", "rl", "catches", "modelrecov")

#### Prepare model recovery ####

# Read data
path = file.path("data", "processed", "data_discrete_1s.csv")
d = read.csv(path,colClasses = c(rep(NA, 8), rep("character", 2), rep(NA, 4)))
d = apply_pipeline_data_filter(d)

# Rename and add player id that is unique across sessions
d = d %>% mutate(decision = correct) %>% mutate(reward = catch) %>% mutate(nplayers = 5) %>%
  mutate(id = (session - 1) * 5  + player) %>% select(-player)

# Get socials trials with both observed decisions and observed payoffs
d = d %>% filter(social.fac == 3)

# Compute observed decisions and observed payoffs
d=d %>%
  arrange(session, trial, id, time.rounded) %>%
  # Observed decisions
  group_by(session, trial, id) %>%
  mutate(decision.lag = lag(decision)) %>%
  group_by(session, trial, time.rounded) %>%
  mutate(obs.dec.2 = sum(decision.lag)) %>% # Number of individuals choosing higher yielding patch
  ungroup() %>%
  mutate(obs.dec.2 = obs.dec.2 - decision.lag) %>% # Subtract individual's own choice
  ungroup() %>%
  mutate(obs.dec.1 = nplayers - 1 - obs.dec.2) %>% # Lower yielding patch
  mutate(obs.dec.1.norm = obs.dec.1 / (nplayers-1), obs.dec.2.norm = obs.dec.2 / (nplayers-1)) %>% # normalize
  relocate(obs.dec.1, obs.dec.2,.after = nplayers) %>%
  # Observed payoffs
  arrange(session, trial, id, time.rounded) %>%
  group_by(session, trial, id) %>%
  mutate(reward.lag = lag(reward)) %>% 
  group_by(session, trial, time.rounded, decision.lag) %>%
  mutate(obs.rew = sum(reward.lag)) %>% # Rewards obtained by all players at each patch
  group_by(session, trial, id) %>%
  mutate(obs.rew = obs.rew - reward.lag) %>% # Minus rewards obtained by individuals themselves
  mutate(obs.rew.norm = ifelse(decision.lag == 0, obs.rew / obs.dec.1, obs.rew / obs.dec.2)) %>% # normalize
  mutate(decision = decision + 1) %>% # Transform [0, 1] to [1, 2]
  arrange(session, trial, id, time.rounded) 


# Account for missing social information for stan
d = d %>% 
  mutate(obs.dec.1.norm = ifelse(is.na(obs.dec.1.norm), 100, obs.dec.1.norm)) %>%
  mutate(obs.dec.2.norm = ifelse(is.na(obs.dec.2.norm), 100, obs.dec.2.norm)) %>%
  mutate(obs.rew.norm=ifelse(is.na(obs.rew.norm), 100, obs.rew.norm))

# Put experimental data in list
stan.data.d = with(d, list(
  OBSERVATIONS=nrow(d),
  SESSIONS=max(unique(session)), session=session,
  TRIALS=max(unique(trial)), trial=trial,
  MAXIMUM=length(unique(max.fac)), maximum=max.fac,
  RATIO=length(unique(ratio.fac)), ratio=ratio.fac,
  PLAYERS=unique(nplayers),
  ID=max(id), id=id,
  TIMES=max(unique(time.rounded)), time=time.rounded,
  DECISIONS=length(unique(decision)), decision=decision, 
  obsdec=cbind(obs.dec.1.norm, obs.dec.2.norm),
  obsrew=obs.rew.norm,
  REWARDS=length(unique(reward)), reward=reward
))


# MCMC Settings are loaded from pipeline_config.R

# Clear any leftover output sinks from earlier failed runs before compiling Stan models
while (sink.number() > 0) sink()

#### Simulation Setup ####

# Experimental parameters (identical for all simulations)
exp.pars = list(
  sessions = exp_sessions,
  trials = exp_trials,
  nplayers = exp_nplayers, # number of players per session
  durations.vec = exp_durations  # The simulation functions sample trial lengths from this vector (equally)
  # and randomly assigns them to the different environments
)
# Add unique ids for players (rows are sessions)
exp.pars$id = with(exp.pars, matrix(1:(sessions*nplayers), ncol=nplayers, byrow = T))

# Environmental parameters (identical for all simulations)
max = c(.5, .7, .9)
ratio = c(.5, .65, .8, .95)
env.pars = expand.grid(max=max, ratio=ratio)
env.pars = list(max=env.pars$max, ratio=env.pars$ratio)

#### Functions to run local model comparison ####
modelfit <- function(msim, mfit, sim, models, stan.data, chains, cores, iter, warmup, refresh){
  fit = models$compiled[[mfit]]$sample(data = stan.data,
                  chains = chains, parallel_chains = cores, iter_sampling = iter - warmup, iter_warmup = warmup, refresh = refresh)
  fit$save_object(file = file.path(resultsdir, paste(models$name[[msim]], models$name[[mfit]], sim, "fit", "rds", sep = ".")))
}

# Function to compute PSIS-LOO for each model fit sequentially
computeloo <- function(msim, sim, models, stan.data){

  # Results list
  results = list()

   for(mfit in 1:length(models$stan.loglik)){

    # Load model fit
    fit = readRDS(file.path(resultsdir, paste(models$name[[msim]], models$name[[mfit]], sim, "fit", "rds", sep = ".")))

    # Following is taken from http://mc-stan.org/loo/articles/loo2-with-rstan.html
    # Extract log likelihood values from model fit
    ll = extract_log_lik_cmd(fit, parameter_name = "log_lik", merge_chains = FALSE)

    # Drop log likelihood of observations where time == 0
    indx = which(stan.data$time != 0, arr.ind = T)
    ll = ll[, , indx]

    # Compute relative effect sample sizes
    if(length(dim(ll)) == 2){
      r_eff = loo::relative_eff(exp(ll), cores = 1, chain_id=rep(1L, nrow(ll)))
      } else {
      r_eff = loo::relative_eff(exp(ll), cores = 1)
    }

    # Compute psis loo
    loo.model = paste("loo", mfit, sep = ".")
    assign(loo.model, loo::loo(ll, r_eff = r_eff, cores = 1))
    remove(ll)

    # Save to results
    results[[models$name[[mfit]]]] = get(loo.model)
    rm(list = loo.model)

   }

  # Compare models
  comparison = loo::loo_compare(results)

  # Add model name
  comparison = as.data.frame(comparison)

  # Note winning model (to reload for parameter recovery)
  winner = comparison %>% filter(row_number() == 1 & elpd_diff == 0) %>% rownames() %>% unlist()

  # Return comparison and winner individually
  return(list(comparison = comparison, winner = winner))
}


#### Run Model Recovery ####

# Get models
models = getmodels(hierarch = F)

# Compile models to avoid recompiling
models$compiled = sapply(1:length(models$stan.loglik), function(x) cmdstan_model(stan_file = models$stan.loglik[[x]]))

# Load partial results if they exist, otherwise start fresh
partial_file = file.path(resultsdir, "modelrecov_partial.csv")
if(file.exists(partial_file)){
  results = read.csv(partial_file)
} else {
  results = data.frame(msim=integer(), sim=integer(), mfit=integer(), win=integer())
}

log_file = file.path(resultsdir, "modelrecov.log")
log_progress = function(msg) {
  line = paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg, "\n")
  cat(line)
  cat(line, file = log_file, append = TRUE)
}

# Loop through models to simulate from
for(msim in 1:length(models$name)){

  done_sims = unique(results$sim[results$msim == msim])
  if(all(1:nsim %in% done_sims)) next

  # Fit model to experimental data (or load existing fit)
  exp_fit_file = file.path(resultsdir, paste(models$name[[msim]], "fit", "rds", sep = "."))
  if(!file.exists(exp_fit_file)){
    log_progress(paste("Fitting model", models$name[[msim]], "to experimental data"))
    fit.exp = models$compiled[[msim]]$sample(data = stan.data.d,
               chains = chains, parallel_chains = cores,
               iter_sampling = iter - warmup, iter_warmup = warmup, refresh = 0)
    fit.exp$save_object(file = exp_fit_file)
  } else {
    log_progress(paste("Loading existing fit for model", models$name[[msim]]))
    fit.exp = readRDS(exp_fit_file)
  }

  # Get parameters to simulate from
  draws = tidy_draws(fit.exp)
  rl.pars = draws[, names(draws) %in% names(models$free.pars.pop[[msim]])]
  rl.pars = apply(rl.pars, 2, mean)
  rl.pars = append(rl.pars, models$fixed.pars[[msim]])

  # Extract columns with environment-specific alphas and collapse to vector or matrix
  if (grepl(".2", models$name[[msim]])) {

    # All alpha* entries with bracketed indices
    par.names <- grep("^alpha[^\\[]*\\[[^]]+\\]$", names(rl.pars), value = TRUE)
    par.bases <- unique(sub("\\[.*$", "", par.names))

    for (base in par.bases) {
      base_names <- grep(paste0("^", base, "\\[[^]]+\\]$"), par.names, value = TRUE)
      if (length(base_names) == 0) next

      # Extract index strings inside brackets, e.g. "1" or "2,3"
      idx_str <- sub("^.*\\[([^]]+)\\].*$", "\\1", base_names)
      idx_split <- strsplit(idx_str, ",")

      is_matrix <- any(lengths(idx_split) > 1)

      if (!is_matrix) {
        # Vector case: order by single index and collapse
        idx <- as.integer(unlist(idx_split))
        ord <- order(idx)
        vec <- unlist(rl.pars[base_names], use.names = FALSE)[ord]

        rl.pars[base_names] <- NULL
        rl.pars[[base]] <- vec
      } else {
        # Matrix case: fill by (row, col) indices
        rc <- t(sapply(idx_split, function(x) as.integer(x)))
        nrow <- max(rc[, 1])
        ncol <- max(rc[, 2])

        mat <- matrix(NA_real_, nrow = nrow, ncol = ncol)
        vals <- unlist(rl.pars[base_names], use.names = FALSE)

        for (i in seq_along(vals)) {
          mat[rc[i, 1], rc[i, 2]] <- vals[i]
        }

        rl.pars[base_names] <- NULL
        rl.pars[[base]] <- mat
      }
    }
  }

  # Prep simulation
  f = get(models$sim[[msim]])
  sim.pars = c(exp.pars, env.pars, rl.pars)

  # Loop through simulations
  for(sim in 1:nsim){

    if(sim %in% done_sims) next

    log_progress(paste0("Model ", models$name[[msim]], " | sim ", sim, "/", nsim, " — simulating and fitting"))

    # Simulate data
    sim.data = f(sim.parameters = sim.pars)

    # Account for missing social information for stan
    sim.data = sim.data %>%
      mutate(obs.dec.1.norm = ifelse(is.na(obs.dec.1.norm), 100, obs.dec.1.norm)) %>%
      mutate(obs.dec.2.norm = ifelse(is.na(obs.dec.2.norm), 100, obs.dec.2.norm)) %>%
      mutate(obs.rew.norm = ifelse(is.na(obs.rew.norm), 100, obs.rew.norm))

    # Put in list
    stan.data.sim = with(sim.data, list(
      OBSERVATIONS=nrow(sim.data),
      SESSIONS=max(unique(session)), session=session,
      TRIALS=max(unique(trial)), trial=trial,
      MAXIMUM=length(unique(max.fac)), maximum=max.fac,
      RATIO=length(unique(ratio.fac)), ratio=ratio.fac,
      PLAYERS=unique(nplayers),
      ID=max(player), id=player,
      TIMES=max(unique(time)), time=time,
      DECISIONS=length(unique(decision)), decision=decision,
      obsdec=cbind(obs.dec.1.norm, obs.dec.2.norm),
      obsrew=obs.rew.norm,
      REWARDS=length(unique(reward)), reward=reward
    ))

    # Run local model comparison in parallel
    plan(multisession, workers = max(1L, min(length(models$stan.loglik), floor((max(1L, parallel::detectCores() - 1L)) / max(1L, cores)))))
    future_lapply(1:length(models$stan.loglik), function(mfit) {
      modelfit(msim, mfit, sim, models, stan.data.sim, chains, cores, iter, warmup, refresh)
    }, future.seed = TRUE)
    plan(sequential)

    # Compute PSIS-LOO for each model fit sequentially
    modelcomp = computeloo(msim, sim, models, stan.data = stan.data.sim)

    log_progress(paste0("Model ", models$name[[msim]], " | sim ", sim, "/", nsim, " — winner: ", modelcomp$winner))

    # Save per-iteration model comparison checkpoint
    saveRDS(modelcomp, file = file.path(resultsdir, paste(models$name[[msim]], "mcomp", sim, "rds", sep = ".")))

    # Delete temporary fit files
    for(mfit in 1:length(models$stan.loglik)){
      tmp_file = file.path(resultsdir, paste(models$name[[msim]], models$name[[mfit]], sim, "fit", "rds", sep = "."))
      if(file.exists(tmp_file)) file.remove(tmp_file)
    }

    # Append to results and write partial CSV
    win = rep(0, length(models$name))
    win[which(models$name == modelcomp$winner)] = 1
    new_rows = data.frame(
      msim = rep(msim, length(models$name)),
      sim  = rep(sim,  length(models$name)),
      mfit = 1:length(models$name),
      win  = win
    )
    results = rbind(results, new_rows)
    write.csv(results, partial_file, row.names = FALSE)

  }

}

# Save final results
saveRDS(results, file.path(resultsdir, "modelrecov.rds"))
write.csv(results, file.path(resultsdir, "modelrecov.csv"), row.names = FALSE)

#### Plot model recovery results ####
if(!file.exists(file.path(resultsdir, "modelrecov.jpeg"))){

  # For each simulated model, count how often the other models won the comparison
  comp = results %>%
    group_by(msim, mfit) %>%
    reframe(p = sum(win) / nsim) %>%
    ungroup() %>%
    mutate(msim = factor(msim, labels = unlist(models$name))) %>%
    mutate(mfit = factor(mfit, labels = unlist(models$name)))

  # Plot confusion matrix
  p = comp %>%
    ggplot(aes(x=mfit, y=msim, fill=p)) +
    geom_tile(alpha=.75, col="black", linewidth=1) +
    geom_text(aes(label=p), size=10) +
    labs(x="Fitted Model", y="Simulated Model", fill="p(fit|sim) \n") +
    scale_fill_viridis()+
    theme_gray(base_size = 11)
    ggexport(p, width = 2560, height = 1440, 
      filename = file.path(resultsdir, "modelrecov.jpeg"))

}else{
  print("Skipping. Model recovery plot already exists." )
}
