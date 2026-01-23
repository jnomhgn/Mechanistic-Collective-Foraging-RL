#### Setup ####

# Source functions
dir_functions <- file.path("code", "rl", "catches", "functions")
function.list = file.path(dir_functions, list.files(dir_functions))
sapply(function.list, source, .GlobalEnv)

# Create results directory (ensure it exists)
resultsdir <- file.path("results", "rl", "catches", "parrecov")
if(!dir.exists(resultsdir)){dir.create(resultsdir, recursive = TRUE)}

#### Prepare parameter recovery ####

# Number of simulated experiments
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

# MCMC
chains = 4
cores = 4
iter = 2000
warmup = 1000
refresh = 100

# Load winning model
load(file = file.path(resultsdir, "..", "modelcomp", "modelcomp.Rdata"))

# Parse name
winner = gsub(pattern = ".hierarch", "", winner)
winner = paste(winner, "fixed", sep = ".")

# Get list of models
models = getmodels(hierarch = FALSE) 

# Get winning model
models = lapply(models, function(x) x[grepl(winner, models$name)])

# Compile 
models$compiled = sapply(1:length(models$name), function(x)
  stan_model(file = models$stan[[x]], model_name = models$name[[x]]))


#### Run parameter recovery ####
# Loop over models
for(mod in 1:length(models$name)){
  
  if(!file.exists(file.path(resultsdir, paste(models$name[[mod]], "rds", sep = ".")))){
    
    # Results list
    results = list()
    
    # Get simulation function for model
    f = get(models$sim[[mod]])
    
    # Generate sobol sequence
    rl.pars = sobol(n = nsim, dim = length(models$free.pars.pop[[mod]]), init = T)
    
    # Rescale to parameter range
    rl.pars = sapply(1:ncol(rl.pars), function(x)
      models$free.pars.pop[[mod]][[x]][[1]] + 
        (models$free.pars.pop[[mod]][[x]][[2]] - models$free.pars.pop[[mod]][[x]][[1]]) * rl.pars[, x]
    ) 
    
    # Account for different variable types when nsim == 1 or > 1
    if(nsim == 1){rl.pars = t(rl.pars)}else{rl.pars = rl.pars}
    
    # Rename
    rl.pars = rl.pars %>%
      as.data.frame() %>% 
      `colnames<-`(names(models$free.pars.pop[[mod]])) %>%
      cbind(models$fixed.pars[[mod]])
    
    # Loop over simulations
    for(sim in 1:nsim){
      
      # Write log to text file for when knitting
      prgrss = paste("Simulating from model", models$name[[mod]], ". Simulation", sim, "out of", nsim)
      log.file = file.path(resultsdir, "log.txt")
      if(!file.exists(log.file)){file.create(log.file)}
      write(prgrss, log.file, append = TRUE, ncolumns = 1)
      
      # Check if there are any nested parameters (Shouldn't be the case for non-adaptive models)
      if(any(sapply(models$free.pars.struct[[mod]], function(x) is.list(x)))){
        
        # Get nested lists in models free pars (pars with offsets)
        indx = sapply(models$free.pars.struct[[mod]], function(x) is.list(x))
        indx = indx[indx == TRUE]
        
        # "Unpack" nested list index to index rl.pars data frame
        indx.df = lapply(names(indx), function(x) grepl(x, names(rl.pars)))
        names(indx.df) = names(indx)
        
        
        # Get rl.pars and add to sim pars
        sim.pars = c(exp.pars, env.pars,
                     lapply(indx.df, function(x) matrix(data = rl.pars[sim, x], nrow = length(unique(max)))), # nested pars
                     rl.pars[sim, which(!apply(as.data.frame(indx.df), 1, any))] # non-nested pars
        )
        
        # Unlist individual learning rates
        if(models$name[[mod]] != "m4.3"){
          sim.pars$alphaQN = unlist(sim.pars$alphaQN)
          sim.pars$alphaQP = unlist(sim.pars$alphaQP)
        }
      }else{
        # Get rl.pars and add to sim pars
        sim.pars = c(exp.pars, env.pars, rl.pars[sim, ])
      }
      
      # Simulate data
      sim.data = f(sim.parameters = sim.pars, postpredict = F, duration.actual = F)
      
      # Account for missing social information for stan
      sim.data = sim.data %>% 
        mutate(obs.dec.1.norm = ifelse(is.na(obs.dec.1.norm), 100, obs.dec.1.norm)) %>%
        mutate(obs.dec.2.norm = ifelse(is.na(obs.dec.2.norm), 100, obs.dec.2.norm)) %>%
        mutate(obs.rew.norm=ifelse(is.na(obs.rew.norm), 100, obs.rew.norm))
      
      # Put data in list
      stan.data = with(sim.data, list(
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
      
      # Fit model
      fit = sampling(object = models$compiled[[mod]], data = stan.data,
                     chains = chains, cores = cores, iter = iter, warmup = warmup, refresh = refresh)
      
      # # Plot and save some diagnostics
      # diag.list = diagnostics.plot(model.fit = fit, plot.pars = names(models$free.pars[[mod]]))
      # ggexport(plotlist = diag.list, width = 1920, height = 1080,
      #          filename = file.path(resultsdir, "diagnostics", paste(models$name[[mod]], "sim", sim, "diagnostics", "jpeg",  sep = ".")))
      # 
      
      # Summarise posterior
      # Get generating parameters
      true.pars = rl.pars[sim, names(models$free.pars.pop[[mod]])] %>% 
        pivot_longer(everything(), names_to = "par", values_to = "true.pars")
      
      fit.sum = fit %>% 
        tidy_draws() %>%
        reframe(alphaQN = inv_logit_scaled(logit_alphaQN), alphaQP = logit_alphaQP,
                betaQ = exp(log_betaQ), betaC=betaC,
                alphaVSDR = inv_logit_scaled(logit_alphaVSDR),
                sigmaVSDR = inv_logit_scaled(logit_sigmaVSDR)) %>%
        select(names(models$free.pars.pop[[mod]])) %>%
        pivot_longer(everything(), names_to = "par", values_to = ".mean") %>%
        group_by(par) %>%
        mean_hdci() %>%
        left_join(., true.pars, by = join_by(par)) %>%
        mutate(sim = sim)
      
      
      # Add to results list
      results[[sim]] = fit.sum
      
      
    }
    
    # Plot results for model we just simulated from
    results = bind_rows(results) %>% 
      mutate(par = factor(par, levels = names(models$free.pars.pop[[mod]]))) # factorise for ordering when plotting
    
    # Save results for model 
    saveRDS(results, file = file.path(resultsdir, paste(models$name[[mod]], "rds", sep = ".")))
    
    
  }else{
    print("Results for parameter recovery already exist. Skipping.")
    # Save results for model 
    results = readRDS(file = file.path(resultsdir, paste(models$name[[mod]], "rds", sep = ".")))
    
  }

  # Plot only if results do not already exist
  if(!file.exists(file.path(resultsdir, paste(models$name[[mod]], "jpeg", sep = ".")))){
  
    labels = c(
      expression(alpha["Q,-"]),
      expression(alpha["Q,+"]),
      expression(beta["Q"]), 
      expression(beta["C"]),
      expression(alpha["VSDR"]), 
      expression(sigma["VSDR"])
    )
    
    results = results %>%
      mutate(par = factor(par, 
                          levels = sort(unique(results$par)),
                          labels = labels)
      )  
    
    p = results %>%
      ggplot(aes(x=true.pars, y=.mean)) + 
      geom_pointrange(aes(y=.mean, ymin=.lower, ymax=.upper)) +
      stat_cor(aes(y=.mean, label = after_stat(r.label)), method = "pearson", size=rel(7)) +
      labs(x="\n Generating Parameter Value", y="Estimated Parameter Value \n") +
      theme_linedraw(base_size = 11) +
      theme(text = element_text(size=rel(5)),
            strip.text.x = element_text(size=rel(10)),
            strip.text.y = element_text(size=rel(10)), 
            axis.text.x = element_text(size=rel(7)),
            axis.title.x = element_text(size=rel(10)),
            axis.text.y = element_text(size=rel(7)),
            axis.title.y = element_text(size=rel(10)),
            legend.text = element_text(size=rel(7)), 
            legend.title = element_text(size=rel(7)),
            plot.title = element_text(hjust = 0.5, size = rel(8)),
            plot.margin = margin(1,1,1,1, "cm")) +
      facet_wrap(~ par, scales = "free", , labeller = label_parsed) 
    p
    
    
        ggexport(p, width = 2560, height = 1440,
          filename = file.path(resultsdir, paste(models$name[[mod]], "jpeg", sep = ".")))
    print(p)

  }else{
    print("Plots for parameter recovery already exist. Skipping.")
  }
  
}

