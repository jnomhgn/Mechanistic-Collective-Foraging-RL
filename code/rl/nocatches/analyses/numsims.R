#### Setup ####

# Source functions
dir_functions <- file.path("code", "rl", "nocatches", "functions")
function.list = file.path(dir_functions, list.files(dir_functions))
function.list = function.list[sapply(function.list, function(x) !grepl("2", x))]
sapply(function.list, source, .GlobalEnv)

# Setup directories
if(!dir.exists(file.path("results", "rl", "nocatches"))){dir.create(file.path("results", "rl", "nocatches"))}
if(!dir.exists(file.path("results", "rl", "nocatches", "numsims"))){dir.create(file.path("results", "rl", "nocatches", "numsims"))}
resultsdir <- file.path("results", "rl", "nocatches", "numsims")


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

# Get model list
models = getmodels()

# Drop versions with adaptive social learning weights
# models=lapply(models,function(x) x[!grepl(pattern = "2", models$name)] )

# Select models with single social learning parameter (and arl model)
models=lapply(models,function(x) x[models$name %in% c("arl.fixed", "dbn1.fixed", "vsn1.fixed")] )

# Load ARL model fit for the alone condition
fit = readRDS(file.path(resultsdir, "..", "..", "alone", "modelcomp", "m4.1.fit.rds"))

# Get ARL parameter estimates
draws = fit %>%
  tidy_draws() %>%
  select(names(models$free.pars.pop[[which(models$name %in% c("arl.fixed"))]]))
arl.pars.nocatches = apply(draws, 2, mean)

# Remove fit
remove(fit)

# Set social learning weights for decision- and reward- based DB and VS
srl.pars = c("alphaDBD", "alphaVSD")
srl.pars = sapply(srl.pars, function(x) seq(0, 1, by = .01)) %>%
  `colnames<-`(srl.pars)

# Merge
rl.pars.nocatches = models$fixed.par[[which(models$name %in% c("arl.fixed"))]] %>% as.data.frame() %>%
  cbind(t(arl.pars.nocatches)) %>% cbind(srl.pars)

log.txt = file.path(resultsdir, "log.txt")
if(!file.exists(log.txt)){file.create(log.txt)}

#### Run numerical simulations



# Add caching
if(!file.exists(file.path(resultsdir, "numsims.rds"))){

  sink(log.txt, append = T)

  # Results list
  results = list(
    rew.acc = list(),
    acc.time = list(),
    switches = list(), switches.time = list()
  )

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
      mutate(alphaS = ifelse("alphaDBD" %in% colnames(rl.pars), alphaDBD, alphaDBR)) %>%
      mutate(sim = sim) %>%
      relocate(model, parcomb, sim)

    # Compute mean individual accuracy and rewards
    rew.acc = sim.data %>%
      # Compute individual accuracy and rewards on each trial
      group_by(sim, model, alphaS, session, trial, player, duration, ratio, max) %>% # for each individual trial
      reframe(acc = sum(decision),
              rew = sum(reward)) %>%
      mutate(acc = acc / (duration + 1)) %>%
      mutate(rew = rew / (duration + 1)) %>%
      # Compute average individual accuracy
      group_by(sim, model, alphaS, ratio, max) %>%
      reframe(acc.mean = mean(acc), rew.mean = mean(rew))

    # Compute mean individual accuracy ~ time
    acc.time = sim.data %>%
      # Compute average individual accuracy at each time step
      group_by(sim, model, alphaS, time, ratio, max) %>%
      reframe(frac.corr = mean(decision))

    # Compute mean switch rate
    switches = sim.data %>%
      group_by(model, alphaS, sim, session, trial, player, duration, ratio, max) %>%
      mutate(switches = ifelse(lag(decision) != decision, 1, 0)) %>%
      reframe(switches = sum(switches, na.rm=T) / duration) %>%
      group_by(model, alphaS, sim, ratio, max) %>%
      reframe(switches.mean = mean(switches))

    # Compute mean switches ~ time
    switches.time = sim.data %>%
      group_by(model, alphaS, sim, session, trial, player, duration, ratio, max) %>%
      mutate(switch = ifelse(lag(decision) != decision, 1, 0)) %>%
      group_by(model, alphaS, sim, time, ratio, max) %>%
      reframe(switch.frac = sum(switch, na.rm = T) / max(player))

    list(rew.acc=rew.acc, acc.time=acc.time, switches=switches, switches.time=switches.time)
  }

  for(mod in 1:length(models$name)){

    # Get function
    f = get(models$sim[[mod]])

    for(parcomb in 1:nrow(rl.pars.nocatches)){

      rl.pars = rl.pars.nocatches

      sim.pars = c(exp.pars, env.pars, rl.pars[parcomb, ])

      # Simulate in parallel
      plan(multisession, workers = parallel::detectCores()-1)
      sim_results = future.apply::future_lapply(
        1:nsim,
        sim_fun,
        sim.pars=sim.pars,
        rl.pars=rl.pars,
        f=f,
        models=models,
        mod=mod,
        parcomb=parcomb
      )

      # Extract results
      results$rew.acc = append(results$rew.acc, lapply(sim_results, `[[`, "rew.acc"))
      results$acc.time = append(results$acc.time, lapply(sim_results, `[[`, "acc.time"))
      results$switches = append(results$switches, lapply(sim_results, `[[`, "switches"))
      results$switches.time = append(results$switches.time, lapply(sim_results, `[[`, "switches.time"))
    }
  }
  results = lapply(results, function(x) bind_rows(x))
  saveRDS(results, file = file.path(resultsdir, "numsims.rds"))

  sink()

}else{
  print("Results from numerical simulations already exist. Skipping simulations.")
  results = readRDS(file = file.path(resultsdir, "numsims.rds"))
}


#### Plot results ####

# Only run if results do not already exist
if(!file.exists(file.path(resultsdir, "accdiff.csv")) & !file.exists(file.path(resultsdir, "acctimediff.csv")) & !file.exists(file.path(resultsdir, "switchtimediff.csv"))){

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

  plot.data = results$rew.acc %>% filter(model != "arl.fixed" | alphaS == 0) %>% # Drop unnecessary simulations
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
    select(-c(acc.mean.vec, acc.mean.vec.arl, pairwise))

  write.csv(plot.data, file = file.path(resultsdir, "accdiff.csv"))

  # Decision-based DB vs. VS
  p = plot.data %>% filter(model %in% c("dbn1.fixed", "vsn1.fixed")) %>%
    ggplot(aes(x=alphaS, fill=as.factor(model), col=as.factor(model))) +
    labs(y="Individual Accuracy \n") +
    scale_y_continuous(breaks=seq(-0.5, 1, by=.1))+
    scale_x_continuous(name=bquote(paste("\n ", alpha[DBn], " / ", alpha[VSn])), breaks = seq(0, 1, by=.1))+
    scale_fill_viridis(discrete = T, begin = .1, end = .8,
                      name="Model", labels=c("DBn", "VSn")) +
    scale_color_viridis(discrete = T, begin = .1, end = .8,
                        name="Model", labels=c("DBn", "VSn")) +
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
      filename = file.path(resultsdir, "accdiff.jpeg"))

  # Individual Accuracy over time
  plot.data = results$acc.time %>% filter(model != "arl.fixed" | alphaS == 0) %>% # Drop unnecessary simulations
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
    select(-c(acc.mean.vec, acc.mean.vec.arl, pairwise))

  # plot.data = results$acc.time %>%
  #   group_by(model, alphaS, ratio, max, time) %>%
  #   reframe(frac.corr.mean = mean(frac.corr)) %>%
  #   group_by(model, ratio, max, time) %>%
  #   mutate(delta.mean = frac.corr.mean - frac.corr.mean[which(alphaS == 0)])

  write.csv(plot.data, file = file.path(resultsdir, "acctimediff.csv"))

  # Decision-based DB vs. VS
  p1=plot.data %>% filter(model == "dbn1.fixed") %>%
    ggplot(aes(x=time, y=acc.delta, group=alphaS, col=alphaS)) +
    geom_line(alpha=1, lty=2) +
    geom_hline(yintercept = 0, lty=2)+
    geom_hline(yintercept = 0.1, lty=2) +
    scale_y_continuous(breaks=seq(-1, 1, by=0.1))+
    scale_color_viridis(name=bquote(paste(alpha[DBn], " / ", alpha[VSn], "\n")), end = 1) +
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

  p2=plot.data %>% filter(model == "vsn1.fixed") %>%
    ggplot(aes(x=time, y=acc.delta, group=alphaS, col=alphaS)) +
    geom_line(alpha=1, lty=2) +
    geom_hline(yintercept = 0, lty=2)+
    geom_hline(yintercept = 0.1, lty=2) +
    scale_y_continuous(breaks=seq(-1, 1, by=0.1))+

    scale_color_viridis(name=bquote(paste(alpha[DBn], " / ", alpha[VSn], "\n")), end = 1) +
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
      filename = file.path(resultsdir, "acctimediff.jpeg"))

  # Switch-rate over time
  # plot.data = results$switches.time %>%
  #   group_by(model, alphaS, ratio, max, time) %>%
  #   reframe(switch.frac.mean = mean(switch.frac)) %>%
  #   group_by(model, ratio, max, time) %>%
  #   mutate(delta.mean = switch.frac.mean - switch.frac.mean[which(alphaS == 0)])

  plot.data = results$switches.time %>% filter(model != "arl.fixed" | alphaS == 0) %>% # Drop unnecessary simulations
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
    select(-c(switch.mean.vec, switch.mean.vec.arl, pairwise))


  # plot.data = results$switches.time %>%
  #   group_by(model, alphaS, ratio, max, time) %>%
  #   reframe(switch.mean = mean(switch.frac)) %>%
  #   filter((model == "arl.fixed" & alphaS == 0) | model != "arl.fixed" ) %>%
  #   group_by(ratio, max, time) %>%
  #   mutate(switch.delta = switch.mean - switch.mean[which(model == "arl.fixed")])

  write.csv(plot.data, file = file.path(resultsdir, "switchtimediff.csv"))

  # Decision-based DB vs. VS
  p1 = plot.data %>% filter(model == "dbn1.fixed") %>%
    ggplot(aes(x=time, y=switch.delta, group=alphaS, col=alphaS)) +
    geom_line(alpha=1, lty=2) +
    #geom_line(data=plot.data %>% filter(model == "m2.1" & alphaS == 0)) +
    labs(x="\n Time", y="Difference in Switch Rate \n") +
    #scale_y_continuous(limits = c(0,.6),  breaks=seq(0, 1, by=.2))+
    scale_color_viridis(name=bquote(paste(alpha[DBn], " / ", alpha[VSn],  "\n")), end = 1) +
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

  p2 = plot.data %>% filter(model == "vsn1.fixed") %>%
    ggplot(aes(x=time, y=switch.delta, group=alphaS, col=alphaS)) +
    geom_line(alpha=1, lty=2) +
    #geom_line(data=plot.data %>% filter(model == "m2.1" & alphaS == 0)) +
    labs(x="\n Time", y="") +
    #scale_y_continuous(limits = c(0,.6),  breaks=seq(0, 1, by=.2))+
    scale_color_viridis(name=bquote(paste(alpha[DBn], " / ", alpha[VSn],  "\n")), end = 1) +
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
      filename = file.path(resultsdir, "switchtimediff.jpeg"))

}else{
  print("Plots from numerical simulations already exist. Skipping.")
}
