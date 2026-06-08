#### Setup ####

source(file.path("code", "pipeline_config.R"))

chains = get_pipeline_value("rl", "nocatches", "modelcomp", "chains", default = 4)
cores = get_pipeline_value("rl", "nocatches", "modelcomp", "cores", default = 4)
iter = get_pipeline_value("rl", "nocatches", "modelcomp", "iter", default = 4000)
warmup = get_pipeline_value("rl", "nocatches", "modelcomp", "warmup", default = 2000)
refresh = get_pipeline_value("rl", "nocatches", "modelcomp", "refresh", default = 100)
postpredict_nsim = get_pipeline_value("rl", "nocatches", "modelcomp", "postpredict_nsim", default = 100)
exp_sessions = get_pipeline_value("rl", "nocatches", "modelcomp", "sessions", default = 18)
exp_trials = get_pipeline_value("rl", "nocatches", "modelcomp", "trials", default = 12)
exp_nplayers = get_pipeline_value("rl", "nocatches", "modelcomp", "nplayers", default = 5)
exp_durations = get_pipeline_value("rl", "nocatches", "modelcomp", "durations_vec", default = c(75))

# Source functions
dir_functions <- file.path("code", "rl", "nocatches", "functions")
function.list = file.path(dir_functions, list.files(dir_functions))
# function.list = function.list[sapply(function.list, function(x) !grepl("2", x))]
sapply(function.list, source, .GlobalEnv)

# Setup directories
if(!dir.exists(file.path("results", "rl", "nocatches"))){dir.create(file.path("results", "rl", "nocatches"))}
if(!dir.exists(file.path("results", "rl", "nocatches", "modelcomp"))){dir.create(file.path("results", "rl", "nocatches", "modelcomp"))}

if(!dir.exists(file.path("results", "rl", "nocatches", "modelcomp", "adaptive"))){dir.create(file.path("results", "rl", "nocatches", "modelcomp", "adaptive"))}
if(!dir.exists(file.path("results", "rl", "nocatches", "modelcomp", "adaptive", "diagnostics"))){dir.create(file.path("results", "rl", "nocatches", "modelcomp", "adaptive", "diagnostics"))}
if(!dir.exists(file.path("results", "rl", "nocatches", "modelcomp", "adaptive", "diagnostics", "detailed"))){dir.create(file.path("results", "rl", "nocatches", "modelcomp", "adaptive", "diagnostics", "detailed"))}

resultsdir <- file.path("results", "rl", "nocatches", "modelcomp")


#### Prepare model comparison ####

# Read data
path = file.path("data", "processed", "data_discrete_1s.csv")
d = read.csv(path,colClasses = c(rep(NA, 8), rep("character", 2), rep(NA, 4)))
d = apply_pipeline_data_filter(d)

# Rename and add player id that is unique across sessions
d = d %>% mutate(decision = correct) %>% mutate(reward = catch) %>% mutate(nplayers = 5) %>%
  mutate(id = (session - 1) * 5  + player) %>% select(-player)

# Get social trials
d = d %>% filter(social.fac == 2)

# Compute observed decisions 
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
  mutate(decision = decision + 1)# Transform [0, 1] to [1, 2]

# Account for missing social information for stan
d = d %>% 
  mutate(obs.dec.1.norm = ifelse(is.na(obs.dec.1.norm), 100, obs.dec.1.norm)) %>%
  mutate(obs.dec.2.norm = ifelse(is.na(obs.dec.2.norm), 100, obs.dec.2.norm))

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
  REWARDS=length(unique(reward)), reward=reward
))


# MCMC Settings
# Values are loaded from pipeline_config.R

# Clear any leftover output sinks from earlier failed runs before compiling Stan models
while (sink.number() > 0) sink()


#### Functions for model fitting and loo computation ####

# Function that runs model comparison in parallel
fitmodel <- function(mfit, models, stan.data.d, adaptivity, chains, cores, iter, warmup, refresh){

  # Create log file
  log.file = file.path(resultsdir, adaptivity,
              paste("log", models$name[[mfit]], "txt", sep = "."))
  if(!file.exists(log.file)){file.create(log.file)}

  # Print progress to log.txt
  prgrss = paste("Currently fitting model", models$name[[mfit]])
  write("", log.file, append = TRUE, ncolumns = 1)
  write(prgrss, log.file, append = TRUE, ncolumns = 1)

  # Fit model
  sink.depth = sink.number()
  sink(log.file, append = T)
  on.exit(while (sink.number() > sink.depth) sink(), add = TRUE)
  fit = models$compiled[[mfit]]$sample(data = stan.data.d,
                  chains = chains, parallel_chains = cores, iter_sampling = iter - warmup, iter_warmup = warmup, refresh = refresh)
  while (sink.number() > sink.depth) sink()
  fit$save_object(file = file.path(resultsdir, adaptivity, paste(models$name[[mfit]], "fit", "rds", sep = ".")))
  
  # Plot some diagnostics for population means
  diag.list = diagnostics.plot(model.fit = fit, plot.pars = names(models$free.pars.pop[[mfit]]))
  ggexport(plotlist = diag.list, width = 1920, height = 1080,
            filename = file.path(resultsdir, adaptivity, "diagnostics",
                            paste(models$name[[mfit]], "diagnostics", "jpeg",  sep = ".")))
  
  # Plot detailed traceplots
  if(!dir.exists(file.path(resultsdir, adaptivity, "diagnostics", "detailed", models$name[[mfit]]))){
    dir.create(file.path(resultsdir, adaptivity, "diagnostics", "detailed", models$name[[mfit]]), recursive = TRUE)
  }
  draws = tidy_draws(fit)
  par.names = names(draws)
  par.names = par.names[which(! names(draws) %in% c(".chain", ".iteration",".draw", "lp__",
                                                    "accept_stat__", "stepsize__",    "treedepth__",  "n_leapfrog__",
                                                    "divergent__",   "energy__"))]
  par.names = par.names[!grepl("log_lik", par.names)] 
  for (param in par.names) {
    tplot <- traceplot_cmd(fit, pars = param) + ggplot2::ggtitle(paste("Trace plot for", param))
    ggsave(file.path(resultsdir, adaptivity, "diagnostics", "detailed", models$name[[mfit]], paste0("traceplot_", param, ".png")), tplot)
  }
  
  # Save diagnostics for all parameters
  fit.summary = fit_summary_cmd(fit)
  write.csv(fit.summary, file = file.path(resultsdir, adaptivity, "diagnostics",
                                      paste(models$name[[mfit]], "diagnostics", "csv",  sep = ".")))
  
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
            filename = file.path(resultsdir, adaptivity, "diagnostics", paste(models$name[[mfit]], "draws", "jpeg", sep = ".")))
  
}

# Function to compute PSIS-LOO for each model fit sequentially
computeloo <-function(models, adaptivity, stan.data){

  # Results list
  results = list()

  # Loop over models
  for(mfit in 1:length(models$stan.loglik)){

    # Create log file for each model or append to it
    log.file = file.path(resultsdir, adaptivity,
          paste("log", models$name[[mfit]], "txt", sep = "."))
    if(!file.exists(log.file)){file.create(log.file)}

    # Load model fit
    fit = readRDS(file.path(resultsdir, adaptivity, paste(models$name[[mfit]], "fit", "rds", sep = ".")))
     
    # Following is taken from http://mc-stan.org/loo/articles/loo2-with-rstan.html
    # Extract log likelihood values from model fit
    ll = extract_log_lik_cmd(fit, parameter_name = "log_lik", merge_chains = FALSE)
    remove(fit)

    # Drop log likelihood of observations where time == 0
    indx = which(stan.data$time != 0, arr.ind = T)
    ll = ll[, , indx]

    # Compute relative effect sample sizes
    if(length(dim(ll)) == 2){
      r_eff = loo::relative_eff(exp(ll), cores = 1, chain_id = rep(1L, nrow(ll)))
    }else{
      r_eff = loo::relative_eff(exp(ll), cores = 1)
    }
    
    # Compute psis loo
    loo.model = paste("loo", mfit, sep = ".")
    assign(loo.model, loo::loo(ll, r_eff = r_eff, cores = 1))
    remove(ll)
    
    # Save diagnostics
    jpeg(file.path(resultsdir, adaptivity, "diagnostics", paste(models$name[[mfit]], "paretok", "jpeg", sep = ".")))
    plot(get(loo.model))    
    dev.off()
    
    sink.depth = sink.number()
    sink(log.file, append = T)
    on.exit(while (sink.number() > sink.depth) sink(), add = TRUE)
    print(get(loo.model))
    while (sink.number() > sink.depth) sink()
    
    
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
  
  # Save Variables
  save(list = c("results", "comparison", "winner"), file = file.path(resultsdir, adaptivity, "modelcomp.Rdata"))

  # Save and print comparison
  write.csv(x = comparison, file = file.path(resultsdir, adaptivity, "modelcomp.csv"))
  
  # Return comparison and winner individually
  return(list(comparison = comparison, winner = winner))

}

#### Run model comparison including adaptive models ####

# Adaptivity toggle
adaptivity = "adaptive"

# Results list
results = list()

# Run model comparison in parallel if results do not exist
if(!file.exists(file.path(resultsdir, adaptivity, "modelcomp.Rdata"))){

  # Get models
  models = getmodels(hierarch = T)

  # Select models
  models= lapply(models, function(x) x[which(models$name %in% c("arl.hierarch", "dbn1.hierarch", "vsn1.hierarch", "dbn2.hierarch", "vsn2.hierarch"))])

  # Compile models to avoid recompiling 
  models$compiled = sapply(1:length(models$stan.loglik), function(x) cmdstan_model(stan_file = models$stan.loglik[[x]]))

  plan(multisession, workers = max(1L, min(length(models$stan.loglik), floor((max(1L, parallel::detectCores() - 1L)) / max(1L, cores)))))

  # Fit models in parallel
  future_lapply(1:length(models$stan.loglik), function(mfit) {
    fitmodel(mfit, models, stan.data.d, adaptivity, chains, cores, iter, warmup, refresh)
  }, future.seed = TRUE)
  plan(sequential)

  # Compute PSIS-LOO sequentially
  results = computeloo(models, adaptivity, stan.data=stan.data.d)

  # Extract results from list
  list2env(results, globalenv())

}else{
  print("Model comparison reulsts for adaptive models already exist. Skipping computation.")
  # Load results
  load(file = file.path(resultsdir, adaptivity, "modelcomp.Rdata"))
}

# print(comparison[, ])


#### Posterior predictions ####

# Run only if not already done
if(!file.exists(file.path(resultsdir, adaptivity, "postpredict_acctime.csv")) &
  !file.exists(file.path(resultsdir, adaptivity, "postpredict_acc.csv"))){

  # Get initial distribution of players from data
  decfreq.init = d %>% filter(time.rounded == 0) %>% select(id, session, max, max.fac, ratio, ratio.fac, decision, duration)

  # Number of times experiments were simulated from each model
  nsim = postpredict_nsim

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
  max = round(c(.5, .7, .9), digits = 2)
  ratio = round(c(.5, .65, .8, .95), digits = 2)
  env.pars = expand.grid(max=max, ratio=ratio)
  env.pars = list(max=env.pars$max, ratio=env.pars$ratio)

  # Set the winning model / the best, simplest model
  winner = unname(winner)

  # Load fit
  fit = readRDS(file.path(resultsdir, "adaptive", paste(winner, "fit", "rds", sep = ".")))

  # Get and index winning model in fixed effects model lsit (used for simulation)
  winner = gsub(pattern = ".hierarch", replacement = "", x = winner)
  models = getmodels(hierarch = F)
  winnerindx = grep(paste0("^", winner, "\\.fixed$"), unlist(models$name))
  # models = lapply(models, function(x) x[winnerindx])

  # Extract draws
  draws = tidy_draws(fit)
  vn = colnames(draws)
  pop.names = names(models$free.pars.pop[winnerindx][[1]])

  # Extract mean of the back-transformed (native-scale) individual-level estimates
  extract.mean = function(popname){
    base  = sub("\\[.*$", "", popname)     # e.g. "alphaVSD"
    inner = sub("^[^\\[]*", "", popname)    # ""  or  "[m,r]"
    idcols = if (inner == "") {
      grep(paste0("^id", base, "\\["), vn, value = TRUE)                       # idX[i]
    } else {
      inner = gsub("\\[|\\]", "", inner)                                       # "m,r"
      grep(sprintf("^id%s\\[[0-9]+,%s\\]$", base, inner), vn, value = TRUE)    # idX[i,m,r]
    }
    if (length(idcols) == 0) return(mean(draws[[popname]]))   # no random effect: fall back to scalar
    mean(as.matrix(draws[, idcols]))
  }
  rl.pars = vapply(pop.names, extract.mean, numeric(1))

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

      # Remove environment-varying entries from rl.pars and rename matrix
      rl.pars = rl.pars[!names(rl.pars) %in% par.names.subset]
      par.name = unique(sub("\\[.*$", "", par.names.subset))

      # Add matrix to rlpars
      rl.pars[par.name] = list(par.mat)
    }
  }

  # Prep simulation
  f = get(models$sim[winnerindx][[1]])
  sim.pars = c(exp.pars, env.pars, rl.pars, decfreq.init=list(decfreq.init))

  # Simulate
  results = list()
  for(sim in 1:nsim){
    print(paste("Simulation", sim, "of", nsim))
    sim.data = f(sim.parameters = sim.pars, postpredict = T) %>% mutate(sim=sim, decision = decision - 1)
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

  write.csv(plot.data, file = file.path(resultsdir, "adaptive", "postpredict_acctime.csv"))

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
    labs(x="Time", y="Mean Accuracy \n", 
        col="Probability Maximum") +
    facet_wrap( ~ ratio, ncol=4, labeller = facet.labeller)
    ggexport(p, width=1920, height=1080,
      filename = file.path(resultsdir, "adaptive","postpredict_acctime.jpeg"))


  # Posterior predictions for accuracy

  # Experimental parameters (identical for all simulations). Now without duration limit.
  exp.pars = list(
    sessions = exp_sessions,
    trials = exp_trials,
    nplayers = exp_nplayers # number of players per session
  )
  # Add unique ids for players (rows are sessions)
  exp.pars$id = with(exp.pars, matrix(1:(sessions*nplayers), ncol=nplayers, byrow = T))

  # Prep simulation
  f = get(models$sim[winnerindx][[1]])
  sim.pars = c(exp.pars, env.pars, rl.pars, decfreq.init=list(decfreq.init))

  # Simulate
  results = list()
  for(sim in 1:nsim){
    print(paste("Simulation", sim, "of", nsim))
    sim.data = f(sim.parameters = sim.pars, postpredict = T, duration.actual = T) %>% mutate(sim=sim, decision = decision - 1)
    results[[sim]] = sim.data
    
  }
  results = bind_rows(results)

  # Summarise simulated data for accuracy over time
  plot.data.nocatch.adaptive = results %>%
    group_by(sim, ratio, max) %>% 
    reframe(acc = mean(decision))  %>%
    group_by(ratio, max) %>%
    reframe(mu =mean(acc)) %>%
    mutate(social.fac=2, social="nocatches") %>%
    relocate(c(social.fac, social))
  plot.data.nocatch.adaptive$adaptive = TRUE

  # Load the asocial (alone-condition) baseline to plot alongside the adaptive model.
  alone_file = file.path(resultsdir, "..", "..", "alone", "modelcomp", "postpredict_acc.csv")
  if (file.exists(alone_file)) {
    plot.data.nocatch.baseline = read.csv(file = alone_file)
  } else {
    message("No alone postpredict_acc.csv found; proceeding with adaptive-only results.")
    plot.data.nocatch.baseline = NULL
  }

  plot.data = bind_rows(plot.data.nocatch.baseline, plot.data.nocatch.adaptive)

  write.csv(plot.data, file = file.path(resultsdir, "adaptive", "postpredict_acc.csv"), row.names = F)

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
    labs(x="Time", y="Mean Accuracy \n", 
        col="Probability Maximum") +
    facet_wrap( ~ ratio, ncol=4, labeller = facet.labeller)
  p
    ggexport(p, width=1920, height=1080, 
      filename = file.path(resultsdir, "adaptive", "postpredict_acc.jpeg"))
  }else{
    sprintf("Posterior predictions for %s model already exist. Skipping simulation.", adaptivity) %>% print()
  }