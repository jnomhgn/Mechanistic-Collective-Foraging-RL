#### Setup ####

source(file.path("code", "pipeline_config.R"))

chains = get_pipeline_value("rl", "alone", "modelcomp", "chains", default = 4)
cores = get_pipeline_value("rl", "alone", "modelcomp", "cores", default = 4)
iter = get_pipeline_value("rl", "alone", "modelcomp", "iter", default = 4000)
warmup = get_pipeline_value("rl", "alone", "modelcomp", "warmup", default = 2000)
refresh = get_pipeline_value("rl", "alone", "modelcomp", "refresh", default = 100)
postpredict_nsim = get_pipeline_value("rl", "alone", "modelcomp", "postpredict_nsim", default = 100)
exp_sessions = get_pipeline_value("rl", "alone", "modelcomp", "sessions", default = 18)
exp_trials = get_pipeline_value("rl", "alone", "modelcomp", "trials", default = 12)
exp_nplayers = get_pipeline_value("rl", "alone", "modelcomp", "nplayers", default = 5)
exp_durations = get_pipeline_value("rl", "alone", "modelcomp", "durations_vec", default = c(75))

# Source functions
dir_functions <- file.path("code", "rl", "alone", "functions")
function.list = file.path(dir_functions, list.files(dir_functions))
sapply(function.list, source, .GlobalEnv)

# Create results directories
if(!dir.exists(file.path("results", "rl"))){dir.create(file.path("results", "rl"))}
if(!dir.exists(file.path("results", "rl", "alone"))){dir.create(file.path("results", "rl", "alone"))}
if(!dir.exists(file.path("results", "rl", "alone", "modelcomp"))){dir.create(file.path("results", "rl", "alone", "modelcomp"))}
if(!dir.exists(file.path("results", "rl", "alone", "modelcomp", "diagnostics"))){dir.create(file.path("results", "rl", "alone", "modelcomp", "diagnostics"))}
if(!dir.exists(file.path("results", "rl", "alone", "modelcomp", "diagnostics", "detailed"))){dir.create(file.path("results", "rl", "alone", "modelcomp", "diagnostics", "detailed"), recursive = TRUE)}


#### Load experimental data ####

# Read data
path = file.path("data", "processed", "data_discrete_1s.csv")
d = read.csv(path,colClasses = c(rep(NA, 8), rep("character", 2), rep(NA, 4)))
d = apply_pipeline_data_filter(d)

# Set player id unique across sessions
d = d %>% mutate(id = (session - 1) * 5  + player) %>% select(-player)

# Get solo trials
d = d %>% filter(social.fac == 1)

# Transform
d=d %>%
  mutate(decision = correct) %>% mutate(reward = catch) %>% mutate(nplayers = 5) %>%
  mutate(decision = decision + 1) # Transform [0, 1] to [1, 2]


#### Prepare model estimation ####

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
  REWARDS=length(unique(reward)), reward=reward
))

# Clear any leftover output sinks from earlier failed runs before compiling Stan models
while (sink.number() > 0) sink()


#### Model comparison ####

# Function that runs model comparison in parallel over models

modelfit <- function(mfit, models, stan.data.d, chains, cores, iter, warmup, refresh){

  # Create log file for each model
  log.file = file.path("results", "rl", "alone", "modelcomp", paste("log", models$name[[mfit]], "txt", sep = "."))
  if(!file.exists(log.file)){file.create(log.file)}

  # Print progress to log.txt
  prgrss = paste("Currently fitting model", models$name[[mfit]])
  write("", log.file, append = TRUE, ncolumns = 1)
  write(prgrss, log.file, append = TRUE, ncolumns = 1)

  # Fit model
  sink.depth = sink.number()
  sink(log.file, append = T)
  on.exit(while (sink.number() > sink.depth) sink(), add = TRUE)
  fit = models$compiled[[mfit]]$sample(data = stan.data.d, chains = chains, parallel_chains = cores, iter_sampling = iter - warmup, iter_warmup = warmup, refresh = refresh)
  while (sink.number() > sink.depth) sink()
  fit$save_object(file = file.path("results", "rl", "alone", "modelcomp", paste(models$name[[mfit]], "fit", "rds", sep = ".")))

  # Plot some diagnostics for population means
  diagnostics.list = diagnostics.plot(model.fit = fit, plot.pars = names(models$free.pars.pop[[mfit]]))
  ggexport(plotlist = diagnostics.list, width = 1920, height = 1080,
                  filename = file.path("results", "rl", "alone", "modelcomp", "diagnostics",
                                  paste(models$name[[mfit]], "diagnostics", "jpeg",  sep = ".")))

  # Plot detailed traceplots
  if(!dir.exists(file.path("results", "rl", "alone", "modelcomp", "diagnostics", "detailed", models$name[[mfit]]))){
    dir.create(file.path("results", "rl", "alone", "modelcomp", "diagnostics", "detailed", models$name[[mfit]]), recursive = TRUE)
    }
  draws = tidy_draws(fit)
  par.names = names(draws)
  par.names = par.names[which(! names(draws) %in% c(".chain", ".iteration",".draw", "lp__",
                                                    "accept_stat__", "stepsize__",    "treedepth__",  "n_leapfrog__",
                                                    "divergent__",   "energy__"))]
  par.names = par.names[!grepl("log_lik", par.names)]
  for (param in par.names) {
    tplot <- traceplot_cmd(fit, pars = param) + ggplot2::ggtitle(paste("Trace plot for", param))
    ggsave(file.path("results", "rl", "alone", "modelcomp", "diagnostics", "detailed", models$name[[mfit]],  paste0("traceplot_", param, ".png")), tplot)
  }

  # Save diagnostics for all parameters
  fit.summary = fit_summary_cmd(fit)
  write.csv(fit.summary, file = file.path("results", "rl", "alone", "modelcomp", "diagnostics",
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


}

# Function to compute PSIS-LOO for each model fit sequentially
computeloo <- function(models, stan.data){

  # Results list
  results = list()

   for(mfit in 1:length(models$stan.loglik)){
     
    # Create log file for each model or append to it
    log.file = file.path("results", "rl", "alone", "modelcomp", paste("log", models$name[[mfit]], "txt", sep = "."))
    if(!file.exists(log.file)){file.create(log.file)}
     
    # Load model fit
    fit = readRDS(file.path("results", "rl", "alone", "modelcomp", paste(models$name[[mfit]], "fit", "rds", sep = ".")))
     
    # Following is taken from http://mc-stan.org/loo/articles/loo2-with-rstan.html
    # Extract log likelihood values from model fit
    ll = extract_log_lik_cmd(fit, parameter_name = "log_lik", merge_chains = FALSE) 
     
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
    jpeg(file.path("results", "rl", "alone", "modelcomp", "diagnostics", paste(models$name[[mfit]], "paretok", "jpeg", sep = ".")),
      width = 2550, height = 1440, units = "px")
    plot(get(loo.model))
    dev.off()

    # Print model comp info to log.txt
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
  save(list=c("results", "comparison", "winner"), file = file.path("results", "rl", "alone", "modelcomp", "modelcomp.Rdata"))

  # Save and print comparison
  write.csv(x = comparison, file = file.path("results", "rl", "alone", "modelcomp", "modelcomp.csv"))
  
  # Return comparison and winner individually
  return(list(comparison = comparison, winner = winner))
}



# Run model comparison in parallel if results do not exist
if(!file.exists(file.path("results", "rl", "alone", "modelcomp", "modelcomp.Rdata"))){

  models = getmodels(hierarch = T)

  # Compile models to avoid recompiling
  models$compiled = sapply(1:length(models$stan.loglik), function(x) cmdstan_model(stan_file = models$stan.loglik[[x]]))

  # Results list
  results = list()

  plan(multisession, workers = max(1L, min(length(models$stan.loglik), floor((max(1L, parallel::detectCores() - 1L)) / max(1L, cores)))))

  # Fit models in parallel
  future_lapply(1:length(models$stan.loglik), function(mfit) {
    modelfit(mfit, models, stan.data.d, chains, cores, iter, warmup, refresh)
  }, future.seed = TRUE)
  plan(sequential)

  # Compute PSIS-LOO sequentially
  results = computeloo(models, stan.data=stan.data.d)

  # Extract results from list
  list2env(results, globalenv())

}else{

  # Print
  print("Results for model comparison already exist. Skipping computation.")

  # Load results
  load(file = file.path("results", "rl", "alone", "modelcomp", "modelcomp.Rdata"))
}

# print(comparison[, ])


#### Posterior predictions ####

# Only run if they do not already exist
if(!file.exists(file.path("results", "rl", "alone", "modelcomp", "postpredict_acctime.csv")) &
  !file.exists(file.path("results", "rl", "alone", "modelcomp", "postpredict_acc.csv"))){

  # Posterior predictions for Accuracy over time

  # Get initial distribution of players from data
  decfreq.init = d %>% filter(time.rounded == 0) %>% select(id, max, max.fac, ratio, ratio.fac, decision)

  # Number of times experiments are simulated from each model
  nsim=postpredict_nsim

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

  # Set the winning model / the best, simplest model. Edit if needed
  winner = "m4.1"

  # Load fit
  fit = readRDS(file.path("results", "rl", "alone", "modelcomp", paste(winner, "fit", "rds", sep = ".")))

  # Get and index winning model in fixed effects model lsit (used for simulation)
  winner = gsub(pattern = ".hierarch", replacement = "", x = winner)
  models = getmodels(hierarch = F)
  winnerindx = grep(paste0("^", winner), unlist(models$name))

  # Extract draws
  draws = tidy_draws(fit)
  rl.pars = draws[, names(draws) %in% names(models$free.pars[winnerindx][[1]])] 
  rl.pars = apply(rl.pars, 2, mean)
  rl.pars = append(rl.pars, models$fixed.pars[winnerindx][[1]])

  # Prep simulation
  f = get(models$sim[[winnerindx]])
  sim.pars = c(exp.pars, env.pars, rl.pars, decfreq.init=list(decfreq.init))

  # Simulate
  results = list()
  for(sim in 1:nsim){
    print(paste("Simulation", sim, "of", nsim))
    sim.data = f(sim.parameters = sim.pars, postpredict = T) %>% mutate(sim=sim, decision = decision - 1)
    results[[sim]] = sim.data

  }
  results = bind_rows(results)

  # Summarise simulated data for accuracy over time
  plot.data = results %>%
    group_by(sim, time, ratio, max) %>%
    reframe(acc = mean(decision))  %>%
    group_by(time, ratio, max) %>%
    reframe(mu =mean(acc), se = sd(acc) / sqrt(nsim)) %>%
    mutate(lower = mu - se, upper = mu + se)

  write.csv(plot.data, file = file.path("results", "rl", "alone", "modelcomp", "postpredict_acctime.csv"))
  write.csv(plot.data, file = file.path("results", "rl", "alone", "modelcomp", "postpredict_alone.csv"))

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
      filename = file.path("results", "rl", "alone", "modelcomp", "postpredict_acctime.jpeg"))



  # Posterior predictions for accuracy

  # Get initial distribution of players from data and actual duration - environment combination
  decfreq.init = d %>% filter(time.rounded == 0) %>% select(id, session, max, max.fac, ratio, ratio.fac, decision, duration)

  # Number of times experiments were simulated from each model
  nsim=postpredict_nsim

  # Experimental parameters (identical for all simulations)
  exp.pars = list(
    sessions = exp_sessions,
    trials = exp_trials,
    nplayers = exp_nplayers # number of players per session
  )
  # Add unique ids for players (rows are sessions)
  exp.pars$id = with(exp.pars, matrix(1:(sessions*nplayers), ncol=nplayers, byrow = T))

  # Set the winning model / the best, simplest model
  winner = "m4.1"

  # Load fit
  fit = readRDS(file.path("results", "rl", "alone", "modelcomp", paste(winner, "fit", "rds", sep = ".")))

  # Get and index winning model in fixed effects model lsit (used for simulation)
  winner = gsub(pattern = ".hierarch", replacement = "", x = winner)
  models = getmodels(hierarch = F)
  winnerindx = grep(paste0("^", winner), unlist(models$name))

  # Extract draws
  draws = tidy_draws(fit)
  rl.pars = draws[, names(draws) %in% names(models$free.pars[winnerindx][[1]])] 
  rl.pars = apply(rl.pars, 2, mean)
  rl.pars = append(rl.pars, models$fixed.pars[winnerindx][[1]])

  # Prep simulation
  f = get(models$sim[[winnerindx]])
  sim.pars = c(exp.pars, rl.pars, decfreq.init= list(decfreq.init))

  # Simulate
  results = list()
  for(sim in 1:nsim){
    print(paste("Simulation", sim, "of", nsim))
    sim.data = f(sim.parameters = sim.pars, postpredict = T, duration.actual =T) %>% mutate(sim=sim, decision = decision - 1)
    results[[sim]] = sim.data

  }
  results = bind_rows(results)

  # Summarise simulated data for accuracy over time
  plot.data = results %>%
    group_by(sim, ratio, max) %>%
    reframe(acc = mean(decision))  %>%
    group_by(ratio, max) %>%
    reframe(mu =mean(acc)) %>%
    mutate(social.fac=1, social="alone") %>%
    relocate(c(social.fac, social))

  write.csv(plot.data, file = file.path("results", "rl", "alone", "modelcomp", "postpredict_acc.csv"), row.names = F)

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
      filename = file.path("results", "rl", "alone", "modelcomp", "postpredict_acc.jpeg"))

}else{
  print("Posterior predictions already exist. Skipping computation.")
}
