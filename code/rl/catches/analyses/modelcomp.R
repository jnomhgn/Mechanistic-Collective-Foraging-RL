#### Setup ####

# Source functions
function.list = paste0("code/rl/catches/functions/", list.files("code/rl/catches/functions"))
sapply(function.list, source, .GlobalEnv)

# Setup directories
if(!dir.exists("results/rl/catches")){dir.create("results/rl/catches")}
if(!dir.exists("results/rl/catches/modelcomp")){dir.create("results/rl/catches/modelcomp")}
if(!dir.exists("results/rl/catches/modelcomp/diagnostics")){dir.create("results/rl/catches/modelcomp/diagnostics")}
if(!dir.exists("results/rl/catches/modelcomp/diagnostics/detailed")){dir.create("results/rl/catches/modelcomp/diagnostics/detailed")}

resultsdir = "results/rl/catches/modelcomp"


#### Prepare model comparison ####

# Read data
path = "data/processed/data_discrete_1s.csv"
d = read.csv(path,colClasses = c(rep(NA, 8), rep("character", 2), rep(NA, 4)))

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


# MCMC Settings
# MCMC 
chains = 4
cores = 4
iter = 4000
warmup = 2000
refresh = 100

#### Functions For Model Comparison ####

# Function that runs model comparison in parallel
fitmodel <- function(mfit, models, stan.data.d, chains, cores, iter, warmup, refresh, log.file){

  # Create log file
  log.file = paste(resultsdir,
              paste("log", models$name[[mfit]], "txt", sep="."),
              sep = "/")
  if(!file.exists(log.file)){file.create(log.file)}

  # Print progress to log.txt
  prgrss = paste("Currently fitting model", models$name[[mfit]])
  write("", log.file, append = TRUE, ncolumns = 1)
  write(prgrss, log.file, append = TRUE, ncolumns = 1)

  # Fit model
  sink(log.file, append = T)
  fit = sampling(object = models$compiled[[mfit]], data = stan.data.d,
                  chains = chains, cores = cores, iter = iter, warmup = warmup, refresh = refresh)
  sink()
  saveRDS(fit, paste(resultsdir, paste(models$name[[mfit]], "fit", "rds", sep = "."), sep = "/"))
  
  # Plot some diagnostics for population means
  diag.list = diagnostics.plot(model.fit = fit, plot.pars = names(models$free.pars.pop[[mfit]]))
  ggexport(plotlist = diag.list, width = 1920, height = 1080,
            filename = paste(resultsdir, "diagnostics",
                            paste(models$name[[mfit]], "diagnostics", "jpeg",  sep = "."), sep = "/"))
  
  # Plot detailed traceplots
  if(!dir.exists(
    paste(resultsdir, "diagnostics/detailed",models$name[[mfit]] , sep = "/"))){
    dir.create(paste(resultsdir, "diagnostics/detailed",models$name[[mfit]] , sep = "/"))
  }
  draws = tidy_draws(fit)
  par.names = names(draws)
  par.names = par.names[which(! names(draws) %in% c(".chain", ".iteration",".draw", "lp__",
                                                    "accept_stat__", "stepsize__",    "treedepth__",  "n_leapfrog__",
                                                    "divergent__",   "energy__"))]
  par.names = par.names[!grepl("log_lik", par.names)] 
  for (param in par.names) {
    tplot <- traceplot(fit, pars = param) + ggtitle(paste("Trace plot for", param))
    ggsave(paste(resultsdir, "diagnostics/detailed",models$name[[mfit]],  paste0("traceplot_", param, ".png"), sep = "/"), tplot)
  }
  
  # Save diagnostics for all parameters
  fit.summary = summary(fit)$summary
  write.csv(fit.summary, file = paste(resultsdir, "diagnostics",
                                      paste(models$name[[mfit]], "diagnostics", "csv",  sep = "."), sep = "/"))
  
  # Print core diagnostics to log.txt
  # Extract effective sample size (ESS) and R-hat values
  ess <- fit.summary[, "n_eff"]
  rhat <- fit.summary[, "Rhat"]
  
  # Compute the required values
  num_low_ess <- sum(ess < 150, na.rm = TRUE)
  num_high_rhat <- sum(rhat > 1.01, na.rm = TRUE)
  min_ess <- min(ess, na.rm = TRUE)
  max_rhat <- max(rhat, na.rm = TRUE)
  
  # Prepare log message
  log_message <- sprintf(
    "Number of parameters with ESS < 150: %d\nNumber of parameters with Rhat > 1.01: %d\nLowest ESS: %.2f\nLargest Rhat: %.3f\n",
    num_low_ess, num_high_rhat, min_ess, max_rhat
  )
  write("", log.file, append = TRUE, ncolumns = 1)
  write(log_message, log.file, append = TRUE, ncolumns = 1)
  
  
  # Plot posterior
  draws = tidy_draws(fit) %>% select(names(models$free.pars.pop[[mfit]]))
  palpha = mcmc_areas(draws, pars = names(models$free.pars.pop[[mfit]])[grepl("alpha", names(models$free.pars.pop[[mfit]]))])
  pbeta= mcmc_areas(draws, pars = names(models$free.pars.pop[[mfit]])[!grepl("alpha", names(models$free.pars.pop[[mfit]]))])
  p = ggarrange(palpha, pbeta, ncol = 2)
  ggexport(p, width = 2550, height = 1440,
            filename = paste(resultsdir, "diagnostics", paste(models$name[[mfit]], "draws", "jpeg", sep = "."), sep = "/"))
  
}


# Function to compute PSIS-LOO for each model fit sequentially
computeloo <-function(models, adaptivity, log.file){

  # Results list
  results = list()

  # Loop over models
  for(mfit in 1:length(models$stan.loglik)){

    # Create log file for each model or append to it
    log.file = paste(resultsdir,
                paste("log", models$name[[mfit]], "txt", sep="."),
                sep = "/")
    if(!file.exists(log.file)){file.create(log.file)}

    # Load model fit
    fit = readRDS(paste(resultsdir, paste(models$name[[mfit]], "fit", "rds", sep = "."), sep = "/"))
     
    # Following is taken from http://mc-stan.org/loo/articles/loo2-with-rstan.html
    # Extract log likelihood values from model fit
    ll = extract_log_lik(fit, parameter_name = "log_lik", merge_chains = FALSE)
    remove(fit)
    
    # Drop log likelihood of observations that were set to 0 in stan (time == 0)
    indx = unique(which(ll != 0, arr.ind = T)[, 3])
    ll = ll[, , indx]
    
    # Compute relative effect sample sizes
    r_eff = relative_eff(exp(ll), cores = 2)
    
    # Compute psis loo
    loo.model = paste("loo", mfit, sep = ".")
    assign(loo.model, loo(ll, r_eff = r_eff, cores = 4))
    remove(ll)
    
    # Save diagnostics
    jpeg(paste(resultsdir, "diagnostics", paste(models$name[[mfit]], "paretok", "jpeg", sep = "."), sep = "/"))
    plot(get(loo.model))    
    dev.off()
    
    sink(log.file, append = T)
    print(get(loo.model))
    sink()
    
    
    # Save to results
    results[[models$name[[mfit]]]] = get(loo.model)
    rm(list = loo.model)


  }

  # Compare models
  comparison = loo_compare(results)
  
  # Add model name
  comparison = as.data.frame(comparison)
  
  # Note winning model (to reload for parameter recovery)
  winner = comparison %>% filter(row_number() == 1 & elpd_diff == 0) %>% rownames() %>% unlist()
  
  # Save Variables
  save(list = c("results", "comparison", "winner"), file = paste(resultsdir, "modelcomp.Rdata", sep = "/"))
 
  # Save and print comparison
  write.csv(x = comparison, file = paste(resultsdir, "modelcomp.csv", sep = "/"))
  
  # Return comparison and winner individually
  return(list(comparison = comparison, winner = winner))

}



#### Run model comparison ####

# Results list
results = list()

# Run model comparison in parallel if results do not exist
if(!file.exists(paste(resultsdir, "modelcomp.Rdata", sep = "/"))){

  # Get models
  models = getmodels(hierarch = T)

  # Compile models to avoid recompiling 
  models$compiled = sapply(1:length(models$stan.loglik), function(x) stan_model(file = models$stan.loglik[[x]], model_name = models$name[[x]]))

  plan(multisession, workers = as.integer((parallel::detectCores() - 1) / 4))

  # Fit models in parallel
  future_lapply(1:length(models$stan.loglik), function(mfit) {
    fitmodel(mfit, models, stan.data.d, chains, cores, iter, warmup, refresh, log.file)
  })

  # Compute PSIS-LOO sequentially
  results = computeloo(models, adaptivity, log.file)

  # Extract results from list
  list2env(results, globalenv())

}else{
  print("Results for model comparison already exist. Skipping computation.")
  # Load results
  load(file = paste(resultsdir, "modelcomp.Rdata", sep = "/"))
}

# print(comparison[, ])

#### Posterior predictions ####

# Only run if posterior predictions do not exist yet
if(!file.exists(paste(resultsdir, "postpredict_acctime.csv", sep = "/")) &
   !file.exists(paste(resultsdir, "postpredict_acc.csv", sep = "/"))){

  # Get initial distribution of players from data
  decfreq.init = d %>% filter(time.rounded == 0) %>% select(id, max, max.fac, ratio, ratio.fac, decision, duration)

  # Number of times experiments were simulated from each model
  nsim=100

  # Simulations for accuracy over time

  # Experimental parameters (identical for all simulations)
  exp.pars = list(
    sessions = 18,
    trials = 12,
    nplayers = 5, # number of players per session
    durations.vec = c(75)  # The simulation functions sample trial lengths from this vector (equally) 
    # and randomly assigns them to the different environments
  )
  # Add unique ids for players (rows are sessions)
  exp.pars$id = with(exp.pars, matrix(1:(sessions*nplayers), ncol=nplayers, byrow = T))

  # Environmental parameters (identical for all simulations)
  max = round(c(.5, .7, .9), digits = 2)
  ratio = round(c(.5, .65, .8, .95), digits = 2)
  env.pars = expand.grid(max=max, ratio=ratio)
  env.pars = list(max=env.pars$max, ratio=env.pars$ratio)

  # Set the winning model / the best, simplest model
  winner = unname(winner)

  # Load fit
  fit = readRDS(paste(resultsdir, paste(winner, "fit", "rds", sep = "."), sep = "/"))

  # Get index of winning model in fixed effects model lsit (used for simulation)
  winner = gsub(pattern = ".hierarch", replacement = "", x = winner)
  models = getmodels(hierarch = F)
  winnerindx = grep(paste0("^", winner, "\\.fixed$"), unlist(models$name))

  # Extract draws
  draws = tidy_draws(fit)
  rl.pars = draws[, names(draws) %in% names(models$free.pars.pop[winnerindx][[1]])] 
  rl.pars = apply(rl.pars, 2, mean)
  rl.pars = append(rl.pars, models$fixed.pars[winnerindx][[1]])

  # Extract columns with environment-specific social learning weights and put in matrix.
  # If adaptive model
  if(grepl(pattern = "2", winner)){

    # Extract columns into matrix with corresponding indices
    par.names <- grep("^alpha[^\\[]*\\[", names(rl.pars), value = TRUE)

    # Get number of different parameters that vary by environment
    par.num = length(par.names) / 12

    # Loop through parameters that vary by environment
    for (p in 1:par.num) {
      par.names.subset = par.names[((p - 1) * 12 + 1):(p * 12)]
      par.indx <- do.call(rbind, regmatches(par.names.subset, gregexpr("\\d+", par.names.subset)))
      par.mat <- matrix(NA, nrow = max(as.integer(par.indx[,1])), ncol = max(as.integer(par.indx[,2])))

      for (i in seq_along(par.names.subset)) {
        row <- as.integer(par.indx[i, 1])
        col <- as.integer(par.indx[i, 2])
        par.mat[row, col] <- rl.pars[par.names.subset[i]][[1]]
      }

      # Remove alphaVSD entries from rl.pars and rename matrix
      rl.pars = rl.pars[!names(rl.pars) %in% par.names.subset]
      par.name = unique(sub("\\[.*$", "", par.names.subset))

      # Add matrix to rlpars
      rl.pars[par.name] = list(par.mat)
    }
  }

  # Remove fit (to save memory)
  remove(fit)

  # Prep simulation
  f = get(models$sim[[winnerindx]])
  sim.pars = c(exp.pars, env.pars, rl.pars, decfreq.init = list(decfreq.init))

  # Simulate
  results = list()
  for(sim in 1:nsim){
    print(paste("Simulation", sim, "of", nsim))
    sim.data = f(sim.parameters = sim.pars, postpredict = T, duration.actual = F) %>% mutate(sim=sim, decision = decision - 1)
    results[[sim]] = sim.data
  }
  results = bind_rows(results)

  # Summarise simulated data
  plot.data = results %>%
    group_by(sim, time, ratio, max) %>% 
    reframe(acc = mean(decision))  %>%
    group_by(time, ratio, max) %>%
    reframe(mu =mean(acc), se = sd(acc) / sqrt(nsim)) %>%
    mutate(lower = mu - se, upper = mu + se)

  write.csv(plot.data, file = paste(resultsdir, "postpredict_acctime.csv", sep = "/"))

  # Plot posterior means + hdis 
  ratio.labs = paste("Catch Ratio:", sort(unique(plot.data$ratio)))
  names(ratio.labs) = sort(unique(plot.data$ratio))

  max.labs = paste("Max Catch", sort(unique(plot.data$max)))
  names(max.labs) = sort(unique(plot.data$max))

  facet.labeller = labeller(ratio.fac = ratio.labs, max.fac = max.labs)

  p = plot.data %>% 
    ggplot(aes(x=time, y=mu, ymin=lower, ymax=upper, col=as.factor(max), fill=as.factor(max))) +
    scale_color_viridis(name="Max Catch", discrete = T) +
    scale_fill_viridis(name="Max Catch", discrete = T)+
    ylim(0, 1) +
    geom_ribbon(alpha=.5) +
    geom_line() +
    geom_hline(yintercept = .5, lty=2) +
    theme_linedraw(base_size = 11) +
    theme(text = element_text(size=rel(5)),
          strip.text.x = element_text(size=rel(7)),
          strip.text.y = element_text(size=rel(7)), 
          axis.text.x = element_text(size=rel(7)),
          axis.title.x = element_text(size=rel(7)),
          axis.text.y = element_text(size=rel(7)),
          axis.title.y = element_text(size=rel(7)),
          legend.text = element_text(size=rel(7)), 
          legend.title = element_text(size=rel(7)),
          plot.title = element_text(hjust = 0.5, size = rel(8)),
          plot.margin = margin(1,1,1,1, "cm")) +
    labs(x="Time", y="Mean Accuracy \n", 
        col="Probability Maximum") +
    facet_wrap( ~ ratio, ncol=4, labeller = facet.labeller)
  ggexport(p, width=1920, height=1080, 
          filename = paste(resultsdir,"postpredict_acctime.jpeg", sep = "/"))

  # Posterior predictions for accuracy

  # Experimental parameters (identical for all simulations)
  exp.pars = list(
    sessions = 18,
    trials = 12,
    nplayers = 5 # number of players per session
  )
  # Add unique ids for players (rows are sessions)
  exp.pars$id = with(exp.pars, matrix(1:(sessions*nplayers), ncol=nplayers, byrow = T))

  # Update sim parameters
  sim.pars = c(exp.pars, rl.pars, decfreq.init= list(decfreq.init))

  # Simulate
  results = list()
  for(sim in 1:nsim){
    print(paste("Simulation", sim, "of", nsim))
    sim.data = f(sim.parameters = sim.pars, postpredict = T, duration.actual = T) %>% mutate(sim=sim, decision = decision - 1)
    results[[sim]] = sim.data
    
  }
  results = bind_rows(results)

  # Summarise simulated data for accuracy
  plot.data.catch = results %>%
    group_by(sim, ratio, max) %>% 
    reframe(acc = mean(decision))  %>%
    group_by(ratio, max) %>%
    reframe(mu =mean(acc)) %>%
    mutate(social.fac=3, social="catches") %>%
    relocate(c(social.fac, social))

  plot.data.nocatch = read.csv(file = paste(resultsdir, "../..", "nocatches", "modelcomp", "nonadaptive", "postpredict_acc.csv", sep = "/"))

  plot.data = bind_rows(plot.data.nocatch, plot.data.catch)

  write.csv(plot.data, file = paste(resultsdir, "postpredict_acc.csv", sep = "/"), row.names = F)

  # Plot posterior means + hdis 
  ratio.labs = paste("Catch Ratio:", sort(unique(plot.data$ratio)))
  names(ratio.labs) = sort(unique(plot.data$ratio))

  max.labs = paste("Max Catch", sort(unique(plot.data$max)))
  names(max.labs) = sort(unique(plot.data$max))

  facet.labeller = labeller(ratio.fac = ratio.labs, max.fac = max.labs)

  p = plot.data %>% 
    ggplot(aes(x=social, y=mu, col=as.factor(max), fill=as.factor(max))) +
    scale_color_viridis(name="Max Catch", discrete = T) +
    scale_fill_viridis(name="Max Catch", discrete = T)+
    ylim(0, 1) +
    geom_point(size=7) +
    geom_hline(yintercept = .5, lty=2) +
    theme_linedraw(base_size = 11) +
    theme(text = element_text(size=rel(5)),
          strip.text.x = element_text(size=rel(7)),
          strip.text.y = element_text(size=rel(7)), 
          axis.text.x = element_text(size=rel(7)),
          axis.title.x = element_text(size=rel(7)),
          axis.text.y = element_text(size=rel(7)),
          axis.title.y = element_text(size=rel(7)),
          legend.text = element_text(size=rel(7)), 
          legend.title = element_text(size=rel(7)),
          plot.title = element_text(hjust = 0.5, size = rel(8)),
          plot.margin = margin(1,1,1,1, "cm")) +
    labs(x="Time", y="Mean Accuracy \n", 
        col="Probability Maximum") +
    facet_wrap( ~ ratio, ncol=4, labeller = facet.labeller)
  p
  ggexport(p, width=1920, height=1080, 
          filename = paste(resultsdir, "postpredict_acc.jpeg", sep="/"))
  }else{
    print("Posterior predictions already exist. Skipping computation.")
  }

  
  