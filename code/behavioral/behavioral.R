#### Setup ####

# Create directories
resultsdir <- file.path("results", "behavioral")
if(!dir.exists(resultsdir)){dir.create(resultsdir, recursive = TRUE)}


#### Run analysis #### 
if(!file.exists(file.path(resultsdir, "acc.rds"))){

  # Read data
  path <- file.path("data", "processed", "data_discrete_1s.csv")
  d = read.csv(path, colClasses = c(rep(NA, 8), rep("character", 2), rep(NA, 4)))

  # Set player id unique across sessions
  d = d %>% mutate(id = (session - 1) * 5  + player) %>%
    mutate(id = factor(id, levels = c(1:max(id)))) %>%
    mutate(decision = correct) %>% mutate(reward = catch) %>% mutate(nplayers = 5) %>%
    mutate(time = time.rounded)

  d = d %>%
    mutate(max.fac = factor(max.fac, levels = c(1:3))) %>%
    mutate(ratio.fac = factor(ratio.fac, levels = c(1:4))) %>%
    mutate(social.fac = factor(social.fac, levels = c(1:3))) %>%
  mutate(cond = interaction(max.fac, ratio.fac, social.fac)) %>%
  mutate(session = factor(session, levels=1:max(session)))
  
  # Formula
  formula = bf(decision ~ 0 + cond + (1|session) + (1|id)) 
  
  # Set priors
  get_prior(formula = formula, data = d, family = bernoulli(link = "logit"))
  priors = c(prior(normal(0, 1.5), class = b),
             prior(normal(0, .5), class = sd))
  
  # Fit model
  acc = brm(formula = formula, prior = priors,
            data = d, family = bernoulli(link = logit),
            chains = 4, cores = 4,
            iter=3000, warmup=2000, refresh=10)
  
  # Get draws
  acc.draws = as_draws_df(acc)
  
  # Compute diagnostics
  diagnostics = data.frame(
    bulk_ess = apply(acc.draws, 2, posterior::ess_bulk),
    tail_ess = apply(acc.draws, 2, posterior::ess_tail),
    rhat = apply(acc.draws, 2, posterior::rhat)
  )
  
  dignostics = sapply(1:ncol(acc.draws), function(x) {
    column <- acc.draws[x, ]
    c(bulk_ess = ess_bulk(column), tail_ess = ess_tail(column))
  })
  
  # Save diagnostics
  write.csv(diagnostics, file.path(resultsdir, "diagnostics.csv"), row.names = TRUE)
  
  prior_summary(acc)
  acc
  
  # Save fit
  saveRDS(acc, file.path(resultsdir, "acc.rds"))

}else{
  print("Model fit for behavioral analysis already exists. Skipping model fitting.")
  acc =  readRDS(file.path(resultsdir, "acc.rds"))
}


#### Posterior predictions ####

# Only run if predictions do not already exist
if(!file.exists(file.path(resultsdir, "acc.csv"))){

  # Save draws in wide format
  acc.draws = tidy_draws(acc)
  write.csv(acc.draws, row.names = F, file = file.path(resultsdir, "acc_draws_wide.csv"))

  # Save draws in long format
  acc.draws = acc.draws %>% dplyr::select(contains(c("b"))) %>%
    #mutate(b_cond1.1.1 = 0) %>%
    mutate(draw = row_number()) %>%
    #pivot_longer(contains("b_cond"), names_to = "cond", values_to = "condoffset") %>%
    #mutate(post = b_Intercept + condoffset) %>%
    pivot_longer(-draw, names_to = "cond", values_to = "cond_intercept") %>%
    #mutate(post = inv_logit_scaled(post)) %>%
    #mutate(cond_intercept = inv_logit_scaled(cond_intercept)) %>%
    mutate(cond = gsub("b_cond", "", cond)) %>%
    separate(cond, into = c("max.fac", "ratio.fac", "social.fac"))

  write.csv(acc.draws, row.names = F, file = file.path(resultsdir, "acc_draws_long.csv"))

  # Plot posterior means + hdis 
  ratio.labs = paste("Catch Ratio:", sort(unique(d$ratio)))
  names(ratio.labs) = sort(unique(d$ratio.fac))

  max.labs = paste("Max Catch", sort(unique(d$max)))
  names(max.labs) = sort(unique(d$max.fac))

  facet.labeller = labeller(ratio.fac = ratio.labs, max.fac = max.labs)

  # Save plot data
  acc.draws = acc.draws %>% 
    mutate(cond_intercept = inv_logit_scaled(cond_intercept)) %>%
    group_by(max.fac, ratio.fac, social.fac) %>%
    mean_hdi(cond_intercept, .width = .9) %>%
    mutate(max.fac = factor(max.fac, levels=1:3))

  d.plot = d %>%
    group_by(session, trial, id, duration, max.fac, ratio.fac, social.fac) %>%
    reframe(acc = sum(decision) ) %>%
    mutate(acc = acc / (duration + 1)) %>%
    select(-c(session, trial, duration)) %>%
    pivot_wider(names_from = id, names_prefix = "id", values_from = acc)

  plot.data = left_join(acc.draws, d.plot, by = join_by(max.fac, ratio.fac, social.fac))
  write.csv(plot.data, file = file.path(resultsdir, "acc.csv"))


  # Save plot
  p = ggplot() +
    geom_point(data = d.plot %>% pivot_longer(-c(max.fac, ratio.fac, social.fac), 
                                              names_to = "id", values_to = "acc"),
              aes(x=social.fac,col=social.fac, y=acc),
              position = position_jitter(width = .15),
              size=2, shape=21, fill="white") +
    geom_line(data = acc.draws, 
              aes(x=social.fac, y=cond_intercept, group = interaction(max.fac, ratio.fac))) +
    geom_pointrange(data = acc.draws, 
                    aes(x=social.fac, y=cond_intercept, ymin=.lower, ymax=.upper,
                        fill=social.fac),
                    shape=21, col="black", size=.75) +
    geom_hline(yintercept = .5, lty=2) +
    scale_y_continuous(name = "Accuracy") +
    scale_x_discrete(name = "Condition", labels = c("Alone", "No Catches", "Catches")) +
    scale_fill_viridis(name = "Condition", labels = c("Alone", "No Catches", "Catches"),
                      discrete = T, end = .8) +
    scale_color_viridis(name = "Condition", labels = c("Alone", "No Catches", "Catches"),
                        discrete = T, end = .8) +
    #scale_x_discrete(expand = c(0.15, 0)) +
    theme_linedraw(base_size = 8) +
    theme(legend.position = "none") +
    facet_grid(ratio.fac ~ max.fac, labeller = facet.labeller)
  p
    ggsave(plot = p, dpi=300, width = 6.5, height = 6.5, units = "in",
      filename = file.path(resultsdir, "acc.png"))
}else{

  print("Draws and predictions from posterior already exist. Skipping.")

}
