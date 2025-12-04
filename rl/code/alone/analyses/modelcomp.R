#### Setup ####

# Source functions
function.list = paste0("rl/code/alone/functions/", list.files("rl/code/alone/functions/"))
sapply(function.list, source, .GlobalEnv)

# Create results directories
if(!dir.exists("rl/results")){dir.create("rl/results")}
if(!dir.exists("rl/results/alone")){dir.create("rl/results/alone")}
if(!dir.exists("rl/results/alone/modelcomp")){dir.create("rl/results/alone/modelcomp")}
if(!dir.exists("rl/results/alone/modelcomp/diagnostics")){dir.create("rl/results/alone/modelcomp/diagnostics")}
if(!dir.exists("rl/results/alone/modelcomp/diagnostics/detailed")){dir.create("rl/results/alone/modelcomp/diagnostics/detailed")}


#### Load experimental data ####

# Read data
path = "data/processed/data_discrete_1s.csv"
d = read.csv(path,colClasses = c(rep(NA, 8), rep("character", 2), rep(NA, 4)))

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

# MCMC Settings
chains = 4
cores = 4
iter = 4000
warmup = 2000
refresh = 100


#### Model comparison ####

if(!file.exists("rl/results/alone/modelcomp/modelcomp.Rdata")){

  models = getmodels(hierarch = T)

  # Compile models to avoid recompiling
  models$compiled = sapply(1:length(models$stan.loglik), function(x) stan_model(file = models$stan.loglik[[x]], model_name = models$name[[x]]))

  # Results list
  results = list()

  # Create log file
  log.file = paste(paste("rl/results/alone/modelcomp", "log.txt", sep = "/"))
  if(!file.exists(log.file)){file.create(log.file)}

  # Loop over models
  for(mfit in 1:length(models$stan.loglik)){

    # Print progress to log.txt
    prgrss = paste("Currently fitting model", models$name[[mfit]])
    write("", log.file, append = TRUE, ncolumns = 1)
    write(prgrss, log.file, append = TRUE, ncolumns = 1)

    # Fit model
    fit = sampling(object = models$compiled[[mfit]], data = stan.data.d,
                   chains = chains, cores = cores, iter = iter, warmup = warmup, refresh = refresh)
    saveRDS(fit, paste("rl/results/alone/modelcomp", paste(models$name[[mfit]], "fit", "rds", sep = "."), sep = "/"))

    # Plot some diagnostics for population means
    diagnostics.list = diagnostics.plot(model.fit = fit, plot.pars = names(models$free.pars.pop[[mfit]]))
    ggexport(plotlist = diagnostics.list, width = 1920, height = 1080,
                   filename = paste("rl/results/alone/modelcomp/diagnostics",
                                    paste(models$name[[mfit]], "diagnostics", "jpeg",  sep = "."), sep = "/"))

    # Plot detailed traceplots
    if(!dir.exists(
      paste("rl/results/alone/modelcomp/diagnostics/detailed",models$name[[mfit]] , sep = "/"))){
      dir.create(paste("rl/results/alone/modelcomp/diagnostics/detailed",models$name[[mfit]] , sep = "/"))
      }
    draws = tidy_draws(fit)
    par.names = names(draws)
    par.names = par.names[which(! names(draws) %in% c(".chain", ".iteration",".draw", "lp__",
                                                      "accept_stat__", "stepsize__",    "treedepth__",  "n_leapfrog__",
                                                      "divergent__",   "energy__"))]
    par.names = par.names[!grepl("log_lik", par.names)]
    for (param in par.names) {
      tplot <- traceplot(fit, pars = param) + ggtitle(paste("Trace plot for", param))
      ggsave(paste("rl/results/alone/modelcomp/diagnostics/detailed",models$name[[mfit]],  paste0("traceplot_", param, ".png"), sep = "/"), tplot)
    }



    # Save diagnostics for all parameters
    fit.summary = summary(fit)$summary
    write.csv(fit.summary, file = paste("rl/results/alone/modelcomp/diagnostics",
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

    # Following is taken from http://mc-stan.org/loo/articles/loo2-with-rstan.html
    # Extract log likelihood values from model fit
    ll = extract_log_lik(fit, parameter_name = "log_lik", merge_chains = FALSE)

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
    jpeg(paste("rl/results/alone/modelcomp/diagnostics", paste(models$name[[mfit]], "paretok", "jpeg", sep = "."), sep = "/"),
         width = 2550, height = 1440, units = "px")
    plot(get(loo.model))
    dev.off()

    # Print model comp info to log.txt
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
  save(list=c("results", "comparison", "winner"), file = paste("rl/results/alone/modelcomp", "modelcomp.Rdata", sep = "/"))

  # Save and print comparison
  write.csv(x = comparison, file = paste("rl/results/alone/modelcomp", "modelcomp.csv", sep = "/"))

}else{

  # Read comparison
  load(file = paste("rl/results/alone/modelcomp", "modelcomp.Rdata", sep = "/"))

}

print(comparison[, ])


#### Posterior predictions ####

# Posterior predictions for Accuracy over time

# Get initial distribution of players from data
decfreq.init = d %>% filter(time.rounded == 0) %>% select(id, max, max.fac, ratio, ratio.fac, decision)

# Number of times experiments are simulated from each model
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

# Set the winning model / the best, simplest model. Edit if needed
winner = "m4.1"

# Index model in list
models = getmodels(hierarch = F)
models = lapply(models, function(x) x[which(models$name %in% winner)])
winnerindx = which(models$name == winner)

# Load fit
fit = readRDS(paste("rl/results/alone/modelcomp", paste(winner, "fit", "rds", sep = "."), sep = "/"))

# Extract draws
draws = tidy_draws(fit)
rl.pars = draws[, names(draws) %in% names(models$free.pars[[1]])]
rl.pars = apply(rl.pars, 2, mean)
rl.pars = append(rl.pars, models$fixed.pars[[winnerindx]])

# Prep simulation
f = get(models$sim[[winnerindx]])
sim.pars = c(exp.pars, env.pars, rl.pars, list(decfreq.init))

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

write.csv(plot.data, file = paste("rl/results/alone/modelcomp", "postpredict_acctime.csv", sep = "/"))
write.csv(plot.data, file = paste("rl/results/alone/modelcomp", "postpredict_alone.csv", sep = "/"))

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
       filename = paste("rl/results/alone/modelcomp","postpredict_acctime.jpeg", sep="/"))



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
winner = "m4.1"

# Index model in list
models = getmodels(hierarch = F)
models = lapply(models, function(x) x[which(models$name %in% winner)])
winnerindx = which(models$name == winner)

# Load fit
fit = readRDS(paste("rl/results/alone/modelcomp", paste(winner, "fit", "rds", sep = "."), sep = "/"))

# Extract draws
draws = tidy_draws(fit)
rl.pars = draws[, names(draws) %in% names(models$free.pars[[1]])]
rl.pars = apply(rl.pars, 2, mean)
rl.pars = append(rl.pars, models$fixed.pars[[1]])

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

write.csv(plot.data, file = paste("rl/results/alone/modelcomp", "postpredict_acc.csv", sep = "/"), row.names = F)

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
       filename = paste("rl/results/alone/modelcomp","postpredict_acc.jpeg", sep="/"))
