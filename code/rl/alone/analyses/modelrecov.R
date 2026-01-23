#### Setup ####

# Source functions
function.list = paste0("code/rl/alone/functions/", list.files("code/rl/alone/functions/"))
sapply(function.list, source, .GlobalEnv)

# Create results directories
if(!dir.exists("results/rl/alone/modelrecov/diagnostics/detailed")){dir.create("results/rl/alone/modelrecov/diagnostics/detailed", recursive = T)}

resultsdir = "results/rl/alone/modelrecov"

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

#### Stan Setup ####

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
chains = 1
cores = 1
iter = 2000
warmup = 1000
refresh = 100

#### Simulation Setup ####

# Number of simulations
nsim=10

# Experimental parameters (identical for all simulations)
exp.pars = list(
  sessions = 18,
  trials = 12,
  nplayers = 5, # number of players per session
  durations.vec = c(75, 90, 105)  # The simulation functions sample trial lengths from this vector (equally)
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
modelfit <- function(msim, mfit, sim, models, stan.data.d, chains, cores, iter, warmup, refresh){

  # Create log file for each model
  log.file = paste(resultsdir, paste(models$name[[msim]], models$name[[mfit]], sim, "log", sep = "."), sep = "/")
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
  saveRDS(fit, paste(resultsdir, paste(models$name[[msim]], models$name[[mfit]], sim, "fit", "rds", sep = "."), sep = "/"))

}

# Function to compute PSIS-LOO for each model fit sequentially
computeloo <- function(msim, sim, models, stan.data){

  # Results list
  results = list()

   for(mfit in 1:length(models$stan.loglik)){
     
    # Create log file for each model or append to it
    log.file = paste(resultsdir, paste(models$name[[msim]], models$name[[mfit]], sim, "log", sep="."), sep = "/")
    if(!file.exists(log.file)){file.create(log.file)}
     
    # Load model fit
    fit = readRDS(paste(resultsdir, paste(models$name[[msim]], models$name[[mfit]], sim, "fit", "rds", sep = "."), sep = "/"))
     
    # Following is taken from http://mc-stan.org/loo/articles/loo2-with-rstan.html
    # Extract log likelihood values from model fit
    ll = extract_log_lik(fit, parameter_name = "log_lik", merge_chains = FALSE)

    # Drop log likelihood of observations where time == 0
    indx = which(stan.data$time != 0, arr.ind = T)
    ll = ll[, , indx]

    # Compute relative effect sample sizes
    r_eff = relative_eff(exp(ll), cores = 1)
    
    # Compute psis loo
    loo.model = paste("loo", mfit, sep = ".")
    assign(loo.model, loo(ll, r_eff = r_eff, cores = 1))
    remove(ll)

    # Save diagnostics
    jpeg(paste(resultsdir, "diagnostics", paste(models$name[[msim]], models$name[[mfit]], sim, "paretok", "jpeg", sep = "."), sep = "/"),
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
  saveRDS(list(results = results, comparison = comparison, winner = winner), file = paste(resultsdir, paste(models$name[[msim]], sim, "rds", sep = "."), sep = "/"))

  # Save and print comparison
  write.csv(x = comparison, file = paste(resultsdir, paste(models$name[[msim]], sim, "csv", sep = "."), sep = "/"))
  
  # Return comparison and winner individually
  return(list(comparison = comparison, winner = winner))
}


#### Run model recovery ####

# Get models
models = getmodels(hierarch = F)

# Compile models to avoid recompiling
models$compiled = sapply(1:length(models$stan.loglik), function(x) stan_model(file = models$stan.loglik[[x]], model_name = models$name[[x]]))

if(!file.exists(paste(resultsdir, "modelrecov.rds", sep = "/"))){

  # Run model recovery

  # Results list
  results = list()

  # Loop through models to simulate from
  for(msim in 1:length(models$name)){

    # Fit model to experimental data
    fit.exp = sampling(models$compiled[[msim]], data = stan.data.d,
                       chains = chains, cores = cores,
                       iter = iter, warmup = warmup, refresh = 0)
    
    # Save fit
    saveRDS(fit.exp, file = paste(resultsdir, paste(models$name[[msim]], "fit", "rds", sep = "."), sep = "/"))

    # Get parameters to simulate from
    draws = tidy_draws(fit.exp)
    rl.pars = draws[, names(draws) %in% names(models$free.pars[[msim]])]
    rl.pars = apply(rl.pars, 2, mean)
    rl.pars = append(rl.pars, models$fixed.pars[[msim]])

    # Extract columns with environment-specific alphas and collapse to vector or matrix
    if (models$name[[msim]] %in% c("m4.2", "m4.3")) {

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

      # Simulate data
      sim.data = f(sim.parameters = sim.pars)

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
        REWARDS=length(unique(reward)), reward=reward
      ))

      # Run local model comparison in parallel
      plan(multisession, workers = max(1L, min(length(models$stan.loglik), floor((max(1L, parallel::detectCores() - 1L)) / max(1L, cores)))))
      future_lapply(1:length(models$stan.loglik), function(mfit) {
        modelfit(msim, mfit, sim, models, stan.data.sim, chains, cores, iter, warmup, refresh)
      })
      plan(sequential)

      # Compute PSIS-LOO for each model fit sequentially
      modelcomp = computeloo(msim, sim, models, stan.data=stan.data.sim)

      # Save local model comparison results
      saveRDS(modelcomp, file = paste(resultsdir, paste(models$name[[msim]], "mcomp", sim, "rds", sep = "."), sep = "/"))

      # Add to results
      win = rep(0, length(models$name))
      win[which(models$name == modelcomp$winner)] = 1
      results$msim = append(results$msim, rep(msim, length(models$name)))
      results$sim = append(results$sim, rep(sim, length(models$name)))
      results$mfit = append(results$mfit, 1:length(models$name))
      results$win = append(results$win, win)

    }
    
  }

  # Save model recovery results
  saveRDS(results, paste(resultsdir, "modelrecov.rds", sep = "/"))

  # Convert results to data frame
  results = as.data.frame(results)

  # Save model recovery results as csv
  write.csv(results, paste(resultsdir, "modelrecov.csv", sep = "/"), row.names = F)

}else{
  print("Loading. Model recovery results already exist." )
  # Load model recovery results
  results = read.csv(paste(resultsdir, "modelrecov.csv", sep = "/"))
}

#### Plot model recovery results ####
if(!file.exists(paste(resultsdir, "modelrecov.jpeg", sep= "/"))){

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
          filename = paste(resultsdir, "modelrecov.jpeg", sep = "/"))

}else{
  print("Skipping. Model recovery plot already exists." )
}
