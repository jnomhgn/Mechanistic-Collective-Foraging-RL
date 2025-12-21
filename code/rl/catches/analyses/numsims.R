#### Setup ####

# Source functions
function.list = paste0("code/rl/catches/functions/", list.files("code/rl/catches/functions"))
sapply(function.list, source, .GlobalEnv)

# Setup directories
if(!dir.exists("results/rl/catches")){dir.create("results/rl/catches")}
if(!dir.exists("results/rl/catches/numsims")){dir.create("results/rl/catches/numsims")}
resultsdir = "results/rl/catches/numsims"

#### Prepare simulations ####

# Number of simulated experiments
nsim = 100

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

#### Function for Simulation ####

# Simulation function
sim_fun = function(sim, sim.pars, rl.pars, f, models, mod, parcomb) {

  print(paste("Model:", models$name[[mod]], " Parcomb:", parcomb, " Sim:", sim))

  # Simulate data
  sim.data = f(sim.parameters = sim.pars)

  # Add sim parameters
  sim.data = sim.data %>%
    mutate(decision = decision - 1) %>%
    cbind(model=models$name[[mod]], parcomb=parcomb) %>%
    bind_cols(rl.pars[parcomb, ]) %>%
    # mutate(alphaS = ifelse("alphaDBD" %in% colnames(rl.pars), alphaDBD, alphaDBR)) %>%
    mutate(sim = sim) %>%
    relocate(model, parcomb, sim)

  # Compute mean individual accuracy and rewards
  rew.acc = sim.data %>%
    # Compute individual accuracy and rewards on each trial
    group_by(sim, model, alphaDBR, alphaVSR, alphaVSD, alphaVSDR, sigmaVSDR, session, trial, player, duration, ratio, max) %>% # for each individual trial
    reframe(acc = sum(decision),
            rew = sum(reward)) %>%
    mutate(acc = acc / (duration + 1)) %>%
    mutate(rew = rew / (duration + 1)) %>%
    # Compute average individual accuracy
    group_by(sim, model, alphaDBR, alphaVSR, alphaVSD, alphaVSDR, sigmaVSDR, ratio, max) %>%
    reframe(acc.mean = mean(acc), rew.mean = mean(rew))

  # Compute mean individual accuracy ~ time
  acc.time = sim.data %>%
    # Compute average individual accuracy at each time step
    group_by(sim, model, alphaDBR, alphaVSR, alphaVSD, alphaVSDR, sigmaVSDR, time, ratio, max) %>%
    reframe(frac.corr = mean(decision))

  # Compute mean switch rate
  switches = sim.data %>%
    group_by(model, alphaDBR, alphaVSR, alphaVSD, alphaVSDR, sigmaVSDR, sim, session, trial, player, duration, ratio, max) %>%
    mutate(switches = ifelse(lag(decision) != decision, 1, 0)) %>%
    reframe(switches = sum(switches, na.rm=T) / duration) %>%
    group_by(model, alphaDBR, alphaVSR, alphaVSD, alphaVSDR, sigmaVSDR, sim, ratio, max) %>%
    reframe(switches.mean = mean(switches))

  # Compute mean switches ~ time
  switches.time = sim.data %>%
    group_by(model, alphaDBR, alphaVSR, alphaVSD, alphaVSDR, sigmaVSDR, sim, session, trial, player, duration, ratio, max) %>%
    mutate(switch = ifelse(lag(decision) != decision, 1, 0)) %>%
    group_by(model, alphaDBR, alphaVSR, alphaVSD, alphaVSDR, sigmaVSDR, sim, time, ratio, max) %>%
    reframe(switch.frac = sum(switch, na.rm = T) / max(player))

  list(rew.acc=rew.acc, acc.time=acc.time, switches=switches, switches.time=switches.time)
}


#### Run simulations for v1 ####

# Add caching
if(!file.exists(paste(resultsdir, "numsims_v1.rds", sep = "/"))){

  # Get model list
  models = getmodels()

  # Select models with single social learning parameter (and arl model)
  models=lapply(models,function(x) x[models$name %in% c("arl.fixed","dbr1.fixed", "vsr1.fixed")] )

  # Create rl pars
  rl.pars = list()

  # Load ARL model fit for the no catches condition
  fit = readRDS(paste(resultsdir, "../../nocatches/modelcomp/nonadaptive/arl.hierarch.fit.rds", sep = "/"))

  # Get ARL parameter estimates
  draws = fit %>%
    tidy_draws() %>%
    select(names(models$free.pars.pop[[which(models$name %in% c("arl.fixed"))]]))
  arl.pars.catches = apply(draws, 2, mean)

  # Remove fit
  remove(fit)

  # Set social learning weights for decision- and reward- based DB and VS 
  srl.pars = c("alphaDBR", "alphaVSR", "alphaVSD", "alphaVSDR", "sigmaVSDR") 
  srl.pars = sapply(srl.pars, function(x) seq(0, 1, by = 0.01)) %>%
    `colnames<-`(srl.pars)
  srl.pars[, c("alphaVSD", "alphaVSDR", "sigmaVSDR")] = NA

  # Merge
  rl.pars.catches = models$fixed.par[[which(models$name %in% c("arl.fixed"))]] %>% as.data.frame() %>% 
    cbind(t(arl.pars.catches)) %>% cbind(srl.pars)

  log.txt = paste(resultsdir, "log_v1.txt", sep = "/")
  if(!file.exists(log.txt)){file.create(log.txt)}
  
  sink(log.txt, append = T)
  
  # Results list
  results = list(
    rew.acc = list(),
    acc.time = list(),
    switches = list(), switches.time = list()
  )
  
  for(mod in 1:length(models$name)){
    
    # Get function
    f = get(models$sim[[mod]])
    
    for(parcomb in 1:nrow(rl.pars.catches)){
      
      
      rl.pars = rl.pars.catches
      # Get rl.pars and add to sim pars
      sim.pars = c(exp.pars, env.pars, rl.pars[parcomb, ])
      
      # Run simulations in parallel
      plan(multisession, workers = parallel::detectCores()-1)
      sim_results = future.apply::future_lapply(
        1:nsim,
        sim_fun,
        sim.pars = sim.pars,
        rl.pars = rl.pars,
        f = f,
        models = models,
        mod = mod,
        parcomb = parcomb
      )

      # Extract results
      results$rew.acc = append(results$rew.acc, lapply(sim_results, `[[`, "rew.acc"))
      results$acc.time = append(results$acc.time, lapply(sim_results, `[[`, "acc.time"))
      results$switches = append(results$switches, lapply(sim_results, `[[`, "switches"))
      results$switches.time = append(results$switches.time, lapply(sim_results, `[[`, "switches.time"))
    }
  }
  
  results = lapply(results, function(x) bind_rows(x))
  saveRDS(results, file = 
            paste(resultsdir, "numsims_v1.rds", sep = "/"))
  
  sink()
  
}else{
  results = readRDS(file = paste(resultsdir, "numsims_v1.rds", sep = "/"))
}

#### Plot results for v1 ####

# List to save plots to
plot.list = list()

# Facet labels
max.labs = paste("Maximum Yield", sort(unique(results$rew.acc$max)))
names(max.labs) = sort(unique(results$rew.acc$max))

ratio.labs = paste("Yield Ratio", sort(unique(results$rew.acc$ratio)))
names(ratio.labs) = sort(unique(results$rew.acc$ratio))

facet.labeller = labeller(ratio=ratio.labs, max=max.labs)

# plot.data = results$rew.acc %>%
#       group_by(sim, ratio, max) %>%
#       mutate(delta = acc.mean - acc.mean[which(model == "arl.fixed")]) %>%
#       # Summarise sampling distribution
#       group_by(model, alphaS, ratio, max) %>%
#       reframe(acc.delta= mean(delta),
#               lower = quantile(delta, probs = .05),
#               upper = quantile(delta, probs = .95))
plot.data.c = results$rew.acc %>% 
  mutate(alphaS = ifelse(model == "dbr1.fixed", alphaDBR, alphaVSR)) %>%  
  filter(model != "arl.fixed" | alphaS == 0) %>% # Drop unnecessary simulations
  group_by(model, alphaS, ratio, max) %>% # Group by model, social learning weight, ratio, and max
  reframe(acc.mean.vec = list(acc.mean[which(sim%in% c(1:nsim))])) %>% # Vector of nsim accuracies for each group
  ungroup() %>%
  group_by(ratio, max) %>%
  mutate(
    acc.mean.vec.arl = list(acc.mean.vec[model == "arl.fixed"][[1]]) # Extract the arl.fixed vector
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    pairwise = list(as.vector(outer(acc.mean.vec, acc.mean.vec.arl, "-"))), # Compute pairwise differences
    acc.delta = mean(pairwise),
    lower = quantile(pairwise, probs = 0.05),
    upper = quantile(pairwise, probs = 0.95)
  ) %>%
  select(-c(acc.mean.vec, acc.mean.vec.arl, pairwise)) %>%
  filter(model != "arl.fixed")
write.csv(plot.data.c, file = paste(resultsdir, "accdiff_v1.csv", sep = "/"))


# Merge data
plot.data.nc = read.csv(file.path("results", "rl", "nocatches", "numsims", "accdiff.csv")) %>%
  filter(model != "arl.fixed") %>% select(-c(X))
plot.data = rbind(plot.data.nc, plot.data.c)
write.csv(plot.data, file = paste(resultsdir, "accdiff_v1.csv", sep = "/"))


# Reward-based DB vs. VS
p = plot.data.c %>% filter(model %in% c("dbr1.fixed", "vsr1.fixed")) %>%
  ggplot(aes(x=alphaS, fill=as.factor(model), col=as.factor(model))) + 
  labs(y="Individual Accuracy \n") +
  scale_y_continuous(breaks=seq(-0.5, 1, by=.1))+
  scale_x_continuous(name=bquote(paste("\n ", alpha[DBr], " / ", alpha[VSr])), breaks = seq(0, 1, by=.1))+
  scale_fill_viridis(discrete = T, begin = .1, end = .8, 
                     name="Model", labels=c("DBr", "VSr")) +
  scale_color_viridis(discrete = T, begin = .1, end = .8, 
                      name="Model", labels=c("DBr", "VSr")) +
  geom_hline(yintercept = 0, lty=2)+
  geom_hline(yintercept = 0.05, lty=2)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.5)+
  geom_line(aes(y=acc.delta), lwd=2) +
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
  facet_grid(ratio ~ max, labeller = facet.labeller)
p
#plot.list =  append(plot.list, list(p))
ggexport(p, width = 2800, height = 1440, 
         filename = paste(resultsdir, "accdiff_v1.jpeg", sep = "/"))

# Individual Accuracy over time
# plot.data = results$acc.time %>%
#   group_by(model, alphaS, ratio, max, time) %>%
#   reframe(acc.mean = mean(frac.corr)) %>%
#   filter((model == "arl.fixed" & alphaS == 0) | model != "arl.fixed" ) %>%
#   group_by(ratio, max, time) %>%
#   mutate(acc.delta = acc.mean - acc.mean[which(model == "arl.fixed")])
plot.data.c = results$acc.time %>% 
  mutate(alphaS = ifelse(model == "dbr1.fixed", alphaDBR, alphaVSR)) %>%  
  filter(model != "arl.fixed" | alphaS == 0) %>% # Drop unnecessary simulations
  group_by(model, alphaS, ratio, max, time) %>% # Group by model, social learning weight, ratio, max and time
  reframe(acc.mean.vec = list(frac.corr[which(sim%in% c(1:nsim))])) %>%  # Vector of nsim accuracies for each group
  ungroup() %>%
  group_by(ratio, max, time) %>%
  mutate(
    acc.mean.vec.arl = list(acc.mean.vec[model == "arl.fixed"][[1]]) # Extract the arl.fixed vector
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    pairwise = list(as.vector(outer(acc.mean.vec, acc.mean.vec.arl, "-"))), # Compute pairwise differences
    acc.delta = mean(pairwise)
  ) %>%
  select(-c(acc.mean.vec, acc.mean.vec.arl, pairwise)) %>%
  filter(model != "arl.fixed")


write.csv(plot.data.c, file = paste(resultsdir, "acctimediff_v1.csv", sep = "/"))


# Merge data
plot.data.nc = read.csv(file.path("results", "rl", "nocatches", "numsims", "acctimediff.csv")) %>%
  filter(model != "arl.fixed") %>% select(-c(X))
plot.data = rbind(plot.data.nc, plot.data.c)
write.csv(plot.data, file = paste(resultsdir, "acctimediff_v1.csv", sep = "/"))

# Reward-based DB vs. VS
p1=plot.data.c %>% filter(model == "dbr1.fixed") %>%
  ggplot(aes(x=time, y=acc.delta, group=alphaS, col=alphaS)) +
  geom_line(alpha=1, lty=2) +
  geom_hline(yintercept = 0, lty=2)+
  geom_hline(yintercept = 0.1, lty=2) +
  scale_y_continuous(limits = c(-.3, .3),  breaks=seq(-1, 1, by=0.1))+
  
  scale_color_viridis(name=bquote(paste(alpha[DBr], " / ", alpha[VSr], "\n")), end = 1) +
  theme(text = element_text(size=20),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1, "cm")) +
  labs(x="\n Time", y="Difference in Individual Accuracy \n") +
  theme_linedraw(base_size = 11) +
  theme(text = element_text(size=rel(5)),
        strip.text.x = element_text(size=rel(7)),
        strip.text.y = element_text(size=rel(7)), 
        axis.text.x = element_text(size=rel(7)),
        axis.title.x = element_text(size=rel(7)),
        axis.text.y = element_text(size=rel(7)),
        axis.title.y = element_text(size=rel(7)),
        legend.text = element_text(size=rel(3.5)), 
        legend.key.size = unit(2, "cm"),
        legend.title = element_text(size=rel(7)),
        plot.title = element_text(hjust = 0.5, size = rel(8)),
        plot.margin = margin(1,1,1,1, "cm")) +
  facet_grid(ratio ~ max, labeller = facet.labeller)
p1

p2=plot.data.c %>% filter(model == "vsr1.fixed") %>%
  ggplot(aes(x=time, y=acc.delta, group=alphaS, col=alphaS)) +
  geom_line(alpha=1, lty=2) +
  geom_hline(yintercept = 0, lty=2)+
  geom_hline(yintercept = 0.1, lty=2) +
  scale_y_continuous(limits = c(-.3, .3),  breaks=seq(-1, 1, by=0.1))+
  
  scale_color_viridis(name=bquote(paste(alpha[DBr], " / ", alpha[VSr], "\n")), end = 1) +
  theme(text = element_text(size=20),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1, "cm")) +
  labs(x="\n Time", y="") +
  theme_linedraw(base_size = 11) +
  theme(text = element_text(size=rel(5)),
        strip.text.x = element_text(size=rel(7)),
        strip.text.y = element_text(size=rel(7)), 
        axis.text.x = element_text(size=rel(7)),
        axis.title.x = element_text(size=rel(7)),
        axis.text.y = element_text(size=rel(7)),
        axis.title.y = element_text(size=rel(7)),
        legend.text = element_text(size=rel(3.5)), 
        legend.key.size = unit(2, "cm"),
        legend.title = element_text(size=rel(7)),
        plot.title = element_text(hjust = 0.5, size = rel(8)),
        plot.margin = margin(1,1,1,1, "cm")) +
  facet_grid(ratio ~ max, labeller = facet.labeller)
p2
p = ggarrange(p1, p2, ncol = 2, common.legend = T, legend = "right",
              labels = c("a", "b"), font.label = list(size=rel(30)))
ggexport(p, width = 2800, height = 1440, 
         filename = paste(resultsdir, "acctimediff_v1.jpeg", sep = "/"))

# Switch-rate over time
# plot.data = results$switches.time %>%
#   group_by(model, alphaS, ratio, max, time) %>%
#   reframe(switch.mean = mean(switch.frac)) %>%
#   filter((model == "arl.fixed" & alphaS == 0) | model != "arl.fixed" ) %>%
#   group_by(ratio, max, time) %>%
#   mutate(switch.delta = switch.mean - switch.mean[which(model == "arl.fixed")])
plot.data.c = results$switches.time %>% 
  mutate(alphaS = ifelse(model == "dbr1.fixed", alphaDBR, alphaVSR)) %>%  
  filter(model != "arl.fixed" | alphaS == 0) %>% # Drop unnecessary simulations
  group_by(model, alphaS, ratio, max, time) %>% # Group by model, social learning weight, ratio, max and time
  reframe(switch.mean.vec = list(switch.frac[which(sim%in% c(1:nsim))])) %>%  # Vector of nsim accuracies for each group
  ungroup() %>%
  group_by(ratio, max, time) %>%
  mutate(
    switch.mean.vec.arl = list(switch.mean.vec[model == "arl.fixed"][[1]]) # Extract the arl.fixed vector
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    pairwise = list(as.vector(outer(switch.mean.vec, switch.mean.vec.arl, "-"))), # Compute pairwise differences
    switch.delta = mean(pairwise)
  ) %>%
  select(-c(switch.mean.vec, switch.mean.vec.arl, pairwise)) %>%
  filter(model != "arl.fixed")


write.csv(plot.data.c, file = paste(resultsdir, "switchtimediff_v1.csv", sep = "/"))

# Merge data
plot.data.nc = read.csv(file.path("results", "rl", "nocatches", "numsims", "switchtimediff.csv")) %>%
  filter(model != "arl.fixed") %>% select(-c(X))
plot.data = rbind(plot.data.nc, plot.data.c)
write.csv(plot.data, file = paste(resultsdir, "switchtimediff_v1.csv", sep = "/"))

# Reward-based DB vs. VS
p1 = plot.data.c %>% filter(model == "dbr1.fixed") %>%
  ggplot(aes(x=time, y=switch.delta, group=alphaS, col=alphaS)) +
  geom_line(alpha=1, lty=2) +
  #geom_line(data=plot.data %>% filter(model == "m2.1" & alphaS == 0)) +
  labs(x="\n Time", y="Difference in Switch Rate \n") +
  #scale_y_continuous(limits = c(0,.6),  breaks=seq(0, 1, by=.2))+
  scale_color_viridis(name=bquote(paste(alpha[DBr], " / ", alpha[VSr],  "\n")), end = 1) +
  theme_linedraw(base_size = 11) +
  theme(text = element_text(size=rel(5)),
        strip.text.x = element_text(size=rel(7)),
        strip.text.y = element_text(size=rel(7)), 
        axis.text.x = element_text(size=rel(7)),
        axis.title.x = element_text(size=rel(7)),
        axis.text.y = element_text(size=rel(7)),
        axis.title.y = element_text(size=rel(7)),
        legend.text = element_text(size=rel(3.5)), 
        legend.key.size = unit(2, "cm"),
        legend.title = element_text(size=rel(7)),
        plot.title = element_text(hjust = 0.5, size = rel(8)),
        plot.margin = margin(1,1,1,1, "cm")) +
  facet_grid(ratio ~ max, labeller = facet.labeller)
p1

p2 = plot.data.c %>% filter(model == "vsr1.fixed") %>%
  ggplot(aes(x=time, y=switch.delta, group=alphaS, col=alphaS)) +
  geom_line(alpha=1, lty=2) +
  #geom_line(data=plot.data %>% filter(model == "m2.1" & alphaS == 0)) +
  labs(x="\n Time", y="") +
  #scale_y_continuous(limits = c(0,.6),  breaks=seq(0, 1, by=.2))+
  scale_color_viridis(name=bquote(paste(alpha[DBr], " / ", alpha[VSr],  "\n")), end = 1) +
  theme_linedraw(base_size = 11) +
  theme(text = element_text(size=rel(5)),
        strip.text.x = element_text(size=rel(7)),
        strip.text.y = element_text(size=rel(7)), 
        axis.text.x = element_text(size=rel(7)),
        axis.title.x = element_text(size=rel(7)),
        axis.text.y = element_text(size=rel(7)),
        axis.title.y = element_text(size=rel(7)),
        legend.text = element_text(size=rel(3.5)), 
        legend.key.size = unit(2, "cm"),
        legend.title = element_text(size=rel(7)),
        plot.title = element_text(hjust = 0.5, size = rel(8)),
        plot.margin = margin(1,1,1,1, "cm")) +
  facet_grid(ratio ~ max, labeller = facet.labeller)
p2

p = ggarrange(p1, p2, ncol = 2, common.legend = T, legend = "right",
              labels = c("a", "b"), font.label = list(size=rel(30)))
ggexport(p, width = 2800, height = 1440, 
         filename = paste(resultsdir, "switchtimediff_v1.jpeg", sep = "/"))


#### Run Simulations For v2 ####

# Add caching
if(!file.exists(paste(resultsdir, "numsims_v2.rds", sep = "/"))){

  # Get model list
  models = getmodels()

  # Select models 
  models=lapply(models,function(x) x[models$name %in% c(
                                                      "vsn2.fixed", # Winning model from NC condition
                                                      "vsndbr2.fixed", "vsnvsr1.fixed" # Models building on winning model
                                                      )])

  # Load VSN2 model fit for the no catches condition
  fit = readRDS(paste(resultsdir, "../../nocatches/modelcomp/adaptive/vsn2.hierarch.fit.rds", sep = "/"))

  # Get parameter estimates
  draws = fit %>%
    tidy_draws() %>%
    select(names(models$free.pars.pop[[which(models$name %in% c("vsn2.fixed"))]]))
  srl.pars.nocatches = apply(draws, 2, mean)

  # Extract columns with environment-specific social learning weights and put in matrix.

  # Extract columns into matrix with corresponding indices
  par.names <- grep("^alpha[^\\[]*\\[", names(srl.pars.nocatches), value = TRUE)

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
      par.mat[row, col] <- srl.pars.nocatches[par.names.subset[i]][[1]]
    }

    # Remove alphaVSD entries from rl.pars and rename matrix
    srl.pars.nocatches = srl.pars.nocatches[!names(srl.pars.nocatches) %in% par.names.subset]
    par.name = unique(sub("\\[.*$", "", par.names.subset))

    # Add matrix to rlpars
    srl.pars.nocatches[par.name] = list(par.mat)
  }

  # Add fixed parameters
  srl.pars.nocatches = append(srl.pars.nocatches, models$fixed.par[[which(models$name %in% c("vsn2.fixed"))]])

  # Remove fit
  remove(fit)

  log.txt = paste(resultsdir, "log_v2.txt", sep = "/")
  if(!file.exists(log.txt)){file.create(log.txt)}
  
  sink(log.txt, append = T)
  
  # Results list
  results = list(
    rew.acc = list(),
    acc.time = list(),
    switches = list(), switches.time = list()
  )
  
  for(mod in 1:length(models$name)){

    # Create rl pars
    rl.pars.catches = list()

    # Define parameter grid
    if(models$name[[mod]] == "vsn2.fixed"){
      rl.pars.catches = as_tibble(lapply(srl.pars.nocatches[-which(names(srl.pars.nocatches) %in% c("alphaVSD"))], function(x) x[1])) %>%
        dplyr::mutate(alphaVSD = list(srl.pars.nocatches$alphaVSD))
      rl.pars.catches[c("alphaDBR", "alphaVSR", "alphaVSDR", "sigmaVSDR")] = NA
    }else if(models$name[[mod]] == "vsndbr2.fixed"){
      alphaDBR.seq = seq(0, 1, by = .01)
      vsndbr.pars.catches = tibble(sapply(alphaDBR.seq, function(x) list(matrix(x, nrow = length(unique(max)), ncol = length(unique(ratio)))))) %>% `colnames<-`("alphaDBR")
      rl.pars.catches = as_tibble(lapply(srl.pars.nocatches[-which(names(srl.pars.nocatches) %in% c("alphaVSD"))], function(x) x[1])) %>%
        dplyr::mutate(alphaVSD = list(srl.pars.nocatches$alphaVSD)) %>%
        bind_cols(vsndbr.pars.catches)
      rl.pars.catches[c("alphaVSR", "alphaVSDR", "sigmaVSDR")] = NA
    }else{
      vsnvsr.pars.catches = expand.grid(alphaVSDR = seq(0, 1, by = .01), sigmaVSDR = seq(0, 1, by = .01))
      rl.pars.catches = as_tibble(lapply(srl.pars.nocatches[-which(names(srl.pars.nocatches) %in% c("alphaVSD"))], function(x) x[1])) %>%
        bind_cols(vsnvsr.pars.catches)
      rl.pars.catches[c("alphaDBR", "alphaVSR", "alphaVSD")] = NA
    }
    
    # Get function
    f = get(models$sim[[mod]])

    for(parcomb in 1:nrow(rl.pars.catches)){
      
      # Get rl.pars and add to sim pars
      sim.pars = c(exp.pars, env.pars, sapply(rl.pars.catches[parcomb, ], function(x) x))

      # Run simulations in parallel
      plan(multisession, workers = parallel::detectCores()-1)
      sim_results = future.apply::future_lapply(
        1:nsim,
        sim_fun,
        sim.pars = sim.pars,
        rl.pars = rl.pars.catches,
        f = f,
        models = models,
        mod = mod,
        parcomb = parcomb
      )

      # Extract results
      results$rew.acc = append(results$rew.acc, lapply(sim_results, `[[`, "rew.acc"))
      results$acc.time = append(results$acc.time, lapply(sim_results, `[[`, "acc.time"))
      results$switches = append(results$switches, lapply(sim_results, `[[`, "switches"))
      results$switches.time = append(results$switches.time, lapply(sim_results, `[[`, "switches.time"))
    }
  }
  
  results = lapply(results, function(x) bind_rows(x))
  saveRDS(results, file = 
            paste(resultsdir, "numsims_v2.rds", sep = "/"))
  
  sink()
  
}else{
  results = readRDS(file = paste(resultsdir, "numsims_v2.rds", sep = "/"))
}

#### Plot Results For v2 ####

# List to save plots to
plot.list = list()

# Facet labels
max.labs = paste("Maximum Yield", sort(unique(results$rew.acc$max)))
names(max.labs) = sort(unique(results$rew.acc$max))

ratio.labs = paste("Yield Ratio", sort(unique(results$rew.acc$ratio)))
names(ratio.labs) = sort(unique(results$rew.acc$ratio))

facet.labeller = labeller(ratio=ratio.labs, max=max.labs)

plot.data.c.vsndbr = results$rew.acc %>% 
  filter(model != "vsnvsr1.fixed") %>% 
  select(-c(alphaVSD, alphaVSR, alphaVSDR, sigmaVSDR)) %>% # Unused
  mutate(alphaDBR = map_dbl(alphaDBR, ~ if (is.null(.x)) NA_real_ else .x[1, 1])) %>%  # "Constant" across environments 
  mutate(alphaS = alphaDBR) %>% 
  group_by(model, alphaS, ratio, max) %>% # Group by model, social learning weight, ratio, and max
  reframe(acc.mean.vec = list(acc.mean[which(sim%in% c(1:nsim))])) %>% # Vector of nsim accuracies for each group
  ungroup() %>%
  group_by(ratio, max) %>%
  mutate(
    acc.mean.vec.vsn = list(acc.mean.vec[model == "vsn2.fixed"][[1]]) # Extract the arl.fixed vector
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    pairwise = list(as.vector(outer(acc.mean.vec, acc.mean.vec.vsn, "-"))), # Compute pairwise differences
    acc.delta = mean(pairwise),
    lower = quantile(pairwise, probs = 0.05),
    upper = quantile(pairwise, probs = 0.95)
  ) %>%
  select(-c(acc.mean.vec, acc.mean.vec.vsn, pairwise)) %>%
  filter(model != "vsn2.fixed")

plot.data.c.vsnvsr = results$rew.acc %>% 
  filter(model != "vsndbr2.fixed") %>% 
  select(-c(alphaVSD, alphaVSR, alphaDBR)) %>% # Unused
  group_by(model, alphaVSDR, sigmaVSDR, ratio, max) %>% # Group by model, social learning weight, ratio, and max
  reframe(acc.mean.vec = list(acc.mean[which(sim%in% c(1:nsim))])) %>% # Vector of nsim accuracies for each group
  ungroup() %>% 
  group_by(ratio, max) %>%
  mutate(
    acc.mean.vec.vsn = list(acc.mean.vec[model == "vsn2.fixed"][[1]]) # Extract the arl.fixed vector
  ) %>% 
  ungroup() %>%
  rowwise() %>%
  mutate(
    pairwise = list(as.vector(outer(acc.mean.vec, acc.mean.vec.vsn, "-"))), # Compute pairwise differences
    acc.delta = mean(pairwise),
    lower = quantile(pairwise, probs = 0.05),
    upper = quantile(pairwise, probs = 0.95)
  ) %>%
  select(-c(acc.mean.vec, acc.mean.vec.vsn, pairwise)) %>%
  filter(model != "vsn2.fixed")

# Bind data
plot.data.c = bind_rows(plot.data.c.vsndbr, plot.data.c.vsnvsr)
write.csv(plot.data.c, file = paste(resultsdir, "accdiff_v2.csv", sep = "/"))

# Reward-based DB 
p = plot.data.c %>% filter(model %in% c("vsndbr2.fixed")) %>%
  ggplot(aes(x=alphaS, fill=as.factor(model), col=as.factor(model))) + 
  labs(y="Individual Accuracy \n") +
  scale_y_continuous(breaks=seq(-0.5, 1, by=.1))+
  scale_x_continuous(name=bquote(paste("\n ", alpha[DBr], " / ", alpha[VSr])), breaks = seq(0, 1, by=.1))+
  scale_fill_viridis(discrete = T, begin = .1, end = .8, 
                     name="Model", labels=c("VSnDBr")) +
  scale_color_viridis(discrete = T, begin = .1, end = .8, 
                      name="Model", labels=c("VSnDBr")) +
  geom_hline(yintercept = 0, lty=2)+
  geom_hline(yintercept = 0.05, lty=2)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.5)+
  geom_line(aes(y=acc.delta), lwd=2) +
  theme_linedraw(base_size = 11) +
  facet_grid(ratio ~ max, labeller = facet.labeller)
p
#plot.list =  append(plot.list, list(p))
ggexport(p, width = 2800, height = 1440, 
         filename = paste(resultsdir, "accdiff_v2_vsndbr.jpeg", sep = "/"))

# Location-based VS Reward-based VS 
p = plot.data.c %>% filter(model %in% c("vsnvsr1.fixed")) %>%
  ggplot(aes(x=alphaVSDR, y=sigmaVSDR, fill=acc.delta)) +
  geom_tile() +
  scale_fill_viridis(name="Individual Accuracy \n") +
  theme_linedraw(base_size = 11) +
  facet_grid(ratio ~ max, labeller = facet.labeller)
p
#plot.list =  append(plot.list, list(p))
ggexport(p, width = 2800, height = 1440, 
         filename = paste(resultsdir, "accdiff_v2_vsnvsr.jpeg", sep = "/"))