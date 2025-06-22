#### Setup ####

# Source functions
function.list = paste0("rl/code/nocatches/functions/", list.files("rl/code/nocatches/functions"))
function.list = function.list[sapply(function.list, function(x) !grepl("2", x))]
sapply(function.list, source, .GlobalEnv)

# Setup directories
if(!dir.exists("rl/results/nocatches")){dir.create("rl/results/nocatches")}
if(!dir.exists("rl/results/nocatches/modelcomp")){dir.create("rl/results/nocatches/modelcomp")}
if(!dir.exists("rl/results/nocatches/modelcomp/nonadaptive")){dir.create("rl/results/nocatches/modelcomp/nonadaptive")}
if(!dir.exists("rl/results/nocatches/modelcomp/nonadaptive/diagnostics")){dir.create("rl/results/nocatches/modelcomp/nonadaptive/diagnostics")}
if(!dir.exists("rl/results/nocatches/modelcomp/nonadaptive/diagnostics/detailed")){dir.create("rl/results/nocatches/modelcomp/nonadaptive/diagnostics/detailed")}

if(!dir.exists("rl/results/nocatches/modelcomp/adaptive")){dir.create("rl/results/nocatches/modelcomp/adaptive")}
if(!dir.exists("rl/results/nocatches/modelcomp/adaptive/diagnostics")){dir.create("rl/results/nocatches/modelcomp/adaptive/diagnostics")}
if(!dir.exists("rl/results/nocatches/modelcomp/adaptive/diagnostics/detailed")){dir.create("rl/results/nocatches/modelcomp/adaptive/diagnostics/detailed")}


resultsdir = "rl/results/nocatches/modelcomp"


#### Prepare model comparison ####

# Read data
path = "data/processed/data_discrete_1s.csv"
d = read.csv(path,colClasses = c(rep(NA, 8), rep("character", 2), rep(NA, 4)))

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
chains = 4
cores = 4
iter = 4000
warmup = 2000
refresh = 100


#### Run model comparison for nonadaptive models ####

if(!file.exists(paste(resultsdir, "nonadaptive", "modelcomp.Rdata", sep = "/"))){
  
  # Get models
  models = getmodels(hierarch = T)
  
  # Select subset of models for social condition with observed decisions only
  models= lapply(models, function(x) x[which(models$name %in% c("arl.hierarch", "dbn1.hierarch", "vsn1.hierarch"))])
  
  # Compile models to avoid recompiling 
  models$compiled = sapply(1:length(models$stan.loglik), function(x) stan_model(file = models$stan.loglik[[x]], model_name = models$name[[x]]))
  
  # Results list
  results = list()
  
  # Create log file
  log.file = paste(paste(resultsdir, "nonadaptive",
                         "log.txt", sep = "/"))
  if(!file.exists(log.file)){file.create(log.file)}
  
  # Loop over models
  for(mfit in 1:length(models$stan.loglik)){
    
    # Print info about current simulation / fitting to html
    # Print info about current simulation to html
    prgrss = paste("\n Currently fitting model", models$stan.loglik[[mfit]], "\n")
    
    
    # Write log to text file fot when knitting
    write("", log.file, append = TRUE, ncolumns = 1)
    write(prgrss, log.file, append = TRUE, ncolumns = 1)
    
    # Fit model
    sink(log.file, append = T)
    fit = sampling(object = models$compiled[[mfit]], data = stan.data.d,
                   chains = chains, cores = cores, iter = iter, warmup = warmup, refresh = refresh)
    sink()
    saveRDS(fit, paste(resultsdir, "nonadaptive", paste(models$name[[mfit]], "fit", "rds", sep = "."), sep = "/"))
    
    # Plot some diagnostics for population means
    diag.list = diagnostics.plot(model.fit = fit, plot.pars = names(models$free.pars.pop[[mfit]]))
    ggexport(plotlist = diag.list, width = 1920, height = 1080,
             filename = paste(resultsdir, "nonadaptive", "diagnostics",
                              paste(models$name[[mfit]], "diagnostics", "jpeg",  sep = "."), sep = "/"))
    
    # Plot detailed traceplots
    if(!dir.exists(
      paste(resultsdir, "nonadaptive/diagnostics/detailed",models$name[[mfit]] , sep = "/"))){
      dir.create(paste(resultsdir, "nonadaptive/diagnostics/detailed",models$name[[mfit]] , sep = "/"))
    }
    draws = tidy_draws(fit)
    par.names = names(draws)
    par.names = par.names[which(! names(draws) %in% c(".chain", ".iteration",".draw", "lp__",
                                                      "accept_stat__", "stepsize__",    "treedepth__",  "n_leapfrog__",
                                                      "divergent__",   "energy__"))]
    par.names = par.names[!grepl("log_lik", par.names)] 
    for (param in par.names) {
      tplot <- traceplot(fit, pars = param) + ggtitle(paste("Trace plot for", param))
      ggsave(paste(resultsdir, "nonadaptive/diagnostics/detailed",models$name[[mfit]],  paste0("traceplot_", param, ".png"), sep = "/"), tplot)
    }
    
    # Save diagnostics for all parameters
    fit.summary = summary(fit)$summary
    write.csv(fit.summary, file = paste(resultsdir, "nonadaptive/diagnostics",
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
             filename = paste(resultsdir, "nonadaptive", "diagnostics", paste(models$name[[mfit]], "draws", "jpeg", sep = "."), sep = "/"))
    
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
    jpeg(paste(resultsdir, "nonadaptive", "diagnostics", paste(models$name[[mfit]], "paretok", "jpeg", sep = "."), sep = "/"))
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
  save(results, comparison, winner, file = paste(resultsdir, "nonadaptive", "modelcomp.Rdata", sep = "/"))
  # Save and print comparison
  write.csv(x = comparison, file = paste(resultsdir, "nonadaptive", "modelcomp.csv", sep = "/"))
  
}else{
  
  # Read comparison
  load(file = paste(resultsdir, "nonadaptive", "modelcomp.Rdata", sep = "/"))
  
}

print(comparison[, ])


#### Posterior predictions ####

# Get initial distribution of players from data
decfreq.init = d %>% filter(time.rounded == 0) %>% select(id, max, max.fac, ratio, ratio.fac, decision)

# Number of times experiments were simulated from each model
nsim=100

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
fit = readRDS(paste(resultsdir, "nonadaptive", paste(winner, "fit", "rds", sep = "."), sep = "/"))

# Get and index winning model in fixed effects model lsit (used for simulation)
winner = gsub(pattern = ".hierarch", replacement = "", x = winner)
models = getmodels(hierarch = F)
winnerindx = grep(pattern = winner, models$name)
models = lapply(models, function(x) x[winnerindx])

# Extract draws
draws = tidy_draws(fit)
rl.pars = draws[, names(draws) %in% names(models$free.pars.pop[[1]])] 
rl.pars = apply(rl.pars, 2, mean)
rl.pars = append(rl.pars, models$fixed.pars[[winnerindx]])

# Prep simulation
f = get(models$sim[[1]])
sim.pars = c(exp.pars, env.pars, rl.pars, list(decfreq.init))

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

write.csv(plot.data, file = paste(resultsdir, "nonadaptive", "postpredict_acctime.csv", sep = "/"))
write.csv(plot.data, file = paste("rl/results/figures", "postpredict_nocatches.csv", sep = "/"))

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
         filename = paste(resultsdir, "nonadaptive","postpredict_acctime.jpeg", sep="/"))


# Posterior predictions for accuracy

# Get initial distribution of players from data and actual duration - environment combination
decfreq.init = d %>% filter(time.rounded == 0) %>% select(id, session, max, max.fac, ratio, ratio.fac, decision, duration)

# Number of times experiments were simulated from each model
nsim=100

# Experimental parameters (identical for all simulations)
exp.pars = list(
  sessions = 18,
  trials = 12,
  nplayers = 5 # number of players per session
)
# Add unique ids for players (rows are sessions)
exp.pars$id = with(exp.pars, matrix(1:(sessions*nplayers), ncol=nplayers, byrow = T))

# Set the winning model / the best, simplest model
winner = paste(winner, "hierarch", sep = ".")

# Load fit
fit = readRDS(paste(resultsdir, "nonadaptive", paste(winner, "fit", "rds", sep = "."), sep = "/"))

# Get and index winning model in fixed effects model lsit (used for simulation)
winner = gsub(pattern = ".hierarch", replacement = "", x = winner)
models = getmodels(hierarch = F)
winnerindx = grep(pattern = winner, models$name)
models = lapply(models, function(x) x[winnerindx])

# Extract draws
draws = tidy_draws(fit)
rl.pars = draws[, names(draws) %in% names(models$free.pars.pop[[1]])] 
rl.pars = apply(rl.pars, 2, mean)
rl.pars = append(rl.pars, models$fixed.pars[[1]])

# Prep simulation
f = get(models$sim[[1]])
sim.pars = c(exp.pars, rl.pars, decfreq.init= list(decfreq.init))

# Simulate
results = list()
for(sim in 1:nsim){
  print(paste("Simulation", sim, "of", nsim))
  sim.data = f(sim.parameters = sim.pars, postpredict = T, duration.actual = T) %>% mutate(sim=sim, decision = decision - 1)
  results[[sim]] = sim.data
  
}
results = bind_rows(results)

# Summarise simulated data for accuracy over time
plot.data.nocatch = results %>%
  group_by(sim, ratio, max) %>% 
  reframe(acc = mean(decision))  %>%
  group_by(ratio, max) %>%
  reframe(mu =mean(acc)) %>%
  mutate(social.fac=2, social="nocatches") %>%
  relocate(c(social.fac, social))

plot.data.alone = read.csv(file = paste(resultsdir, "../..", "alone", "modelcomp", "postpredict_acc.csv", sep = "/"))

plot.data = bind_rows(plot.data.alone, plot.data.nocatch)

write.csv(plot.data.nocatch, file = paste(resultsdir, "nonadaptive", "postpredict_acc.csv", sep="/"), row.names = F)

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
         filename = paste(resultsdir, "nonadaptive", "postpredict_acc.jpeg", sep="/"))


#### Run model comparison for including adaptive models ####
if(!file.exists(paste(resultsdir, "adaptive", "modelcomp.Rdata", sep = "/"))){
  
  # Get models
  models = getmodels(hierarch = T)
  
  # Select subset of models for social condition with observed decisions only
  models= lapply(models, function(x) x[which(models$name %in% c("arl.hierarch", "dbn1.hierarch", "vsn1.hierarch", "dbn2.hierarch", "vsn2.hierarch"))])
  
  # Compile models to avoid recompiling 
  models$compiled = sapply(1:length(models$stan.loglik), function(x) stan_model(file = models$stan.loglik[[x]], model_name = models$name[[x]]))
  
  # Results list
  results = list()
  
  # Create log file
  log.file = paste(paste(resultsdir, "adaptive",
                         "log.txt", sep = "/"))
  if(!file.exists(log.file)){file.create(log.file)}
  
  # Loop over models
  for(mfit in 1:length(models$stan.loglik)){
    
    # Print info about current simulation / fitting to html
    # Print info about current simulation to html
    prgrss = paste("\n Currently fitting model", models$stan.loglik[[mfit]], "\n")
    
    
    # Write log to text file fot when knitting
    write("", log.file, append = TRUE, ncolumns = 1)
    write(prgrss, log.file, append = TRUE, ncolumns = 1)
    
    # Fit model
    sink(log.file, append = T)
    fit = sampling(object = models$compiled[[mfit]], data = stan.data.d,
                   chains = chains, cores = cores, iter = iter, warmup = warmup, refresh = refresh)
    sink()
    saveRDS(fit, paste(resultsdir, "adaptive", paste(models$name[[mfit]], "fit", "rds", sep = "."), sep = "/"))
    
    # Plot some diagnostics for population means
    diag.list = diagnostics.plot(model.fit = fit, plot.pars = names(models$free.pars.pop[[mfit]]))
    ggexport(plotlist = diag.list, width = 1920, height = 1080,
             filename = paste(resultsdir, "adaptive/diagnostics",
                              paste(models$name[[mfit]], "diagnostics", "jpeg",  sep = "."), sep = "/"))
    
    # Plot detailed traceplots
    if(!dir.exists(
      paste(resultsdir, "adaptive/diagnostics/detailed",models$name[[mfit]] , sep = "/"))){
      dir.create(paste(resultsdir, "adaptive/diagnostics/detailed",models$name[[mfit]] , sep = "/"))
    }
    draws = tidy_draws(fit)
    par.names = names(draws)
    par.names = par.names[which(! names(draws) %in% c(".chain", ".iteration",".draw", "lp__",
                                                      "accept_stat__", "stepsize__",    "treedepth__",  "n_leapfrog__",
                                                      "divergent__",   "energy__"))]
    par.names = par.names[!grepl("log_lik", par.names)] 
    for (param in par.names) {
      tplot <- traceplot(fit, pars = param) + ggtitle(paste("Trace plot for", param))
      ggsave(paste(resultsdir, "adaptive/diagnostics/detailed",models$name[[mfit]],  paste0("traceplot_", param, ".png"), sep = "/"), tplot)
    }
    
    # Save diagnostics for all parameters
    fit.summary = summary(fit)$summary
    write.csv(fit.summary, file = paste(resultsdir, "adaptive/diagnostics",
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
             filename = paste(resultsdir, "adaptive", "diagnostics", paste(models$name[[mfit]], "draws", "jpeg", sep = "."), sep = "/"))
    
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
    jpeg(paste(resultsdir, "adaptive", "diagnostics", paste(models$name[[mfit]], "paretok", "jpeg", sep = "."), sep = "/"))
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
  save(results, comparison, winner, file = paste(resultsdir, "adaptive", "modelcomp.Rdata", sep = "/"))
  # Save and print comparison
  write.csv(x = comparison, file = paste(resultsdir, "adaptive", "modelcomp.csv", sep = "/"))
  
}else{
  
  # Read comparison
  load(file = paste(resultsdir, "adaptive", "modelcomp.Rdata", sep = "/"))
  
}

print(comparison[, ])


#### Posterior predictions ####

# # Get initial distribution of players from data
# decfreq.init = d %>% filter(time.rounded == 0) %>% select(id, max, max.fac, ratio, ratio.fac, decision)
# 
# # Number of times experiments were simulated from each model
# nsim=100
# 
# # Experimental parameters (identical for all simulations)
# exp.pars = list(
#   sessions = 18,
#   trials = 12,
#   nplayers = 5, # number of players per session
#   durations.vec = c(75)  # The simulation functions sample trial lengths from this vector (equally) 
#   # and randomly assigns them to the different environments
# )
# # Add unique ids for players (rows are sessions)
# exp.pars$id = with(exp.pars, matrix(1:(sessions*nplayers), ncol=nplayers, byrow = T))
# 
# # Environmental parameters (identical for all simulations)
# max = round(c(.5, .7, .9), digits = 2)
# ratio = round(c(.5, .65, .8, .95), digits = 2)
# env.pars = expand.grid(max=max, ratio=ratio)
# env.pars = list(max=env.pars$max, ratio=env.pars$ratio)
# 
# # Set the winning model / the best, simplest model
# winner = unname(winner)
# 
# # Load fit
# fit = readRDS(paste(resultsdir, "adaptive", paste(winner, "fit", "rds", sep = "."), sep = "/"))
# 
# # Get and index winning model in fixed effects model lsit (used for simulation)
# winner = gsub(pattern = ".hierarch", replacement = "", x = winner)
# models = getmodels(hierarch = F)
# winnerindx = grep(pattern = winner, models$name)
# models = lapply(models, function(x) x[winnerindx])
# 
# # Extract draws
# draws = tidy_draws(fit)
# rl.pars = draws[, names(draws) %in% names(models$free.pars.pop[[1]])] 
# rl.pars = apply(rl.pars, 2, mean)
# rl.pars = append(rl.pars, models$fixed.pars[[winnerindx]])
# 
# # Prep simulation
# f = get(models$sim[[1]])
# sim.pars = c(exp.pars, env.pars, rl.pars, list(decfreq.init))
# 
# # Simulate
# results = list()
# for(sim in 1:nsim){
#   print(paste("Simulation", sim, "of", nsim))
#   sim.data = f(sim.parameters = sim.pars, postpredict = T) %>% mutate(sim=sim, decision = decision - 1)
#   results[[sim]] = sim.data
#   
# }
# results = bind_rows(results)
# 
# # Summarise simulated data
# plot.data = results %>%
#   group_by(sim, time, ratio, max) %>% 
#   reframe(acc = mean(decision))  %>%
#   group_by(time, ratio, max) %>%
#   reframe(mu =mean(acc), se = sd(acc) / sqrt(nsim)) %>%
#   mutate(lower = mu - se, upper = mu + se)
# 
# write.csv(plot.data, file = paste(resultsdir, "adaptive", "postpredict_acctime.csv", sep = "/"))
# 
# # Plot posterior means + hdis 
# ratio.labs = paste("Catch Ratio:", sort(unique(plot.data$ratio)))
# names(ratio.labs) = sort(unique(plot.data$ratio))
# 
# max.labs = paste("Max Catch", sort(unique(plot.data$max)))
# names(max.labs) = sort(unique(plot.data$max))
# 
# facet.labeller = labeller(ratio.fac = ratio.labs, max.fac = max.labs)
# 
# p = plot.data %>% 
#   ggplot(aes(x=time, y=mu, ymin=lower, ymax=upper, col=as.factor(max), fill=as.factor(max))) +
#   scale_color_viridis(name="Max Catch", discrete = T) +
#   scale_fill_viridis(name="Max Catch", discrete = T)+
#   ylim(0, 1) +
#   geom_ribbon(alpha=.5) +
#   geom_line() +
#   geom_hline(yintercept = .5, lty=2) +
#   theme_linedraw(base_size = 11) +
#   theme(text = element_text(size=rel(5)),
#         strip.text.x = element_text(size=rel(7)),
#         strip.text.y = element_text(size=rel(7)), 
#         axis.text.x = element_text(size=rel(7)),
#         axis.title.x = element_text(size=rel(7)),
#         axis.text.y = element_text(size=rel(7)),
#         axis.title.y = element_text(size=rel(7)),
#         legend.text = element_text(size=rel(7)), 
#         legend.title = element_text(size=rel(7)),
#         plot.title = element_text(hjust = 0.5, size = rel(8)),
#         plot.margin = margin(1,1,1,1, "cm")) +
#   labs(x="Time", y="Mean Accuracy \n", 
#        col="Probability Maximum") +
#   facet_wrap( ~ ratio, ncol=4, labeller = facet.labeller)
# ggexport(p, width=1920, height=1080,
#          filename = paste("social", "results", "modelcomp_dec","full_adaptive","postpredict_nocatches.jpeg", sep="/"))
# 
# 
# # Posterior predictions for accuracy
# 
# # Get initial distribution of players from data and actual duration - environment combination
# decfreq.init = d %>% filter(time.rounded == 0) %>% select(id, session, max, max.fac, ratio, ratio.fac, decision, duration)
# 
# # Number of times experiments were simulated from each model
# nsim=100
# 
# # Experimental parameters (identical for all simulations)
# exp.pars = list(
#   sessions = 18,
#   trials = 12,
#   nplayers = 5 # number of players per session
# )
# # Add unique ids for players (rows are sessions)
# exp.pars$id = with(exp.pars, matrix(1:(sessions*nplayers), ncol=nplayers, byrow = T))
# 
# # Set the winning model / the best, simplest model
# winner = unname(winner)
# 
# # Load fit
# fit = readRDS(paste("social/results/modelcomp_dec/full_adaptive", paste(winner, "fit", "rds", sep = "."), sep = "/"))
# 
# # Get and index winning model in fixed effects model lsit (used for simulation)
# winner = gsub(pattern = ".hierarch", replacement = "", x = winner)
# models = getmodels(hierarch = F)
# winnerindx = grep(pattern = winner, models$name)
# models = lapply(models, function(x) x[winnerindx])
# 
# # Extract draws
# draws = tidy_draws(fit)
# rl.pars = draws[, names(draws) %in% names(models$free.pars.pop[[1]])] 
# rl.pars = apply(rl.pars, 2, mean)
# rl.pars = append(rl.pars, models$fixed.pars[[1]])
# 
# # Prep simulation
# f = get(models$sim[[1]])
# sim.pars = c(exp.pars, rl.pars, decfreq.init= list(decfreq.init))
# 
# # Simulate
# results = list()
# for(sim in 1:nsim){
#   print(paste("Simulation", sim, "of", nsim))
#   sim.data = f(sim.parameters = sim.pars, postpredict = T, duration.actual = T) %>% mutate(sim=sim, decision = decision - 1)
#   results[[sim]] = sim.data
#   
# }
# results = bind_rows(results)
# 
# # Summarise simulated data for accuracy over time
# plot.data.nocatch = results %>%
#   group_by(sim, ratio, max) %>% 
#   reframe(acc = mean(decision))  %>%
#   group_by(ratio, max) %>%
#   reframe(mu =mean(acc)) %>%
#   mutate(social.fac=2, social="nocatches") %>%
#   relocate(c(social.fac, social))
# 
# plot.data.alone = read.csv(file = paste("asocial", "results", "modelcomp", "postpredict_acc_alone.csv", sep = "/"))
# 
# plot.data = bind_rows(plot.data.alone, plot.data.nocatch)
# 
# write.csv(plot.data, file = paste("social", "results", "modelcomp_dec", "full_nonadaptive", "postpredict_acc_nocatch.csv", sep="/"), row.names = F)
# 
# # Plot posterior means + hdis 
# ratio.labs = paste("Catch Ratio:", sort(unique(plot.data$ratio)))
# names(ratio.labs) = sort(unique(plot.data$ratio))
# 
# max.labs = paste("Max Catch", sort(unique(plot.data$max)))
# names(max.labs) = sort(unique(plot.data$max))
# 
# facet.labeller = labeller(ratio.fac = ratio.labs, max.fac = max.labs)
# 
# p = plot.data %>% 
#   ggplot(aes(x=social, y=mu, col=as.factor(max), fill=as.factor(max))) +
#   scale_color_viridis(name="Max Catch", discrete = T) +
#   scale_fill_viridis(name="Max Catch", discrete = T)+
#   ylim(0, 1) +
#   geom_point(size=7) +
#   geom_hline(yintercept = .5, lty=2) +
#   theme_linedraw(base_size = 11) +
#   theme(text = element_text(size=rel(5)),
#         strip.text.x = element_text(size=rel(7)),
#         strip.text.y = element_text(size=rel(7)), 
#         axis.text.x = element_text(size=rel(7)),
#         axis.title.x = element_text(size=rel(7)),
#         axis.text.y = element_text(size=rel(7)),
#         axis.title.y = element_text(size=rel(7)),
#         legend.text = element_text(size=rel(7)), 
#         legend.title = element_text(size=rel(7)),
#         plot.title = element_text(hjust = 0.5, size = rel(8)),
#         plot.margin = margin(1,1,1,1, "cm")) +
#   labs(x="Time", y="Mean Accuracy \n", 
#        col="Probability Maximum") +
#   facet_wrap( ~ ratio, ncol=4, labeller = facet.labeller)
# p
# ggexport(p, width=1920, height=1080, 
#          filename = paste("social", "results", "modelcomp_dec", "full_nonadaptive", "postpredict_acc_nocatches.jpeg", sep="/"))
# 
