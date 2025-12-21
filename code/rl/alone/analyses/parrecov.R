#### Setup ####

# Source functions
function.list = paste0("code/rl/alone/functions/", list.files("code/rl/alone/functions/"))
sapply(function.list, source, .GlobalEnv)

# Create results directories
if(!dir.exists("results/rl")){dir.create("results/rl")}
if(!dir.exists("results/rl/alone")){dir.create("results/rl/alone")}
if(!dir.exists("results/rl/alone/parrecov")){dir.create("results/rl/alone/parrecov")}

#### Prepare parameter recovery ####

# Load winning model
load(file = paste("results/rl/alone/modelcomp", "modelcomp.Rdata", sep = "/"))
#remove(results, comparison, comparison.named)

# Parse name
winner = gsub(pattern = ".hierarch", "", winner)

# Override winning model
winner = "m4.1"

# Get list of models
models = getmodels(hierarch = FALSE) # Parameter recovery with fixed effects versions

# Index winning model in list
models = lapply(models, function(x) x[which(models$name == winner)])

# Set model index for later
if(length(models$name) == 1){mod = 1}else{warning("Your list of winning models contains more than one model. Something went wrong!")}

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


#### Run parameter recovery ####

if(!file.exists(paste("results/rl/alone/parrecov", paste(models$name[[mod]], "Rdata", sep = "."), sep = "/"))){

  # Results list
  results = list()

  # Get simulation function for model
  f = get(models$sim[[mod]])

  # Generate sobol sequence
  rl.pars = sobol(n = nsim, dim = length(models$free.pars[[mod]]), init = T)

  # Rescale to parameter range
  rl.pars = sapply(1:ncol(rl.pars), function(x)
    models$free.pars[[mod]][[x]][[1]] +
      (models$free.pars[[mod]][[x]][[2]] - models$free.pars[[mod]][[x]][[1]]) * rl.pars[, x]
  )

  # Account for different variable types when nsim == 1 or > 1
  if(nsim == 1){rl.pars = t(rl.pars)}else{rl.pars = rl.pars}

  # Rename
  rl.pars = rl.pars %>%
    as.data.frame() %>%
    `colnames<-`(names(models$free.pars[[mod]])) %>%
    cbind(models$fixed.pars[[mod]])

  # Loop over simulations
  for(sim in 1:nsim){

    # Write log to text file fot when knitting
    prgrss = paste("Simulating from model", models$name[[mod]], ". Simulation", sim, "out of", nsim)
    log.file = paste(paste("results/rl/alone/parrecov",
                           "log.txt", sep = "/"))
    if(!file.exists(log.file)){file.create(log.file)}
    write(prgrss, log.file, append = TRUE, ncolumns = 1)

    # Check if there are any nested parameters
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
    sim.data = f(sim.parameters = sim.pars)

    # Put data in list
    stan.data = with(sim.data, list(
      OBSERVATIONS=nrow(sim.data),
      SESSIONS=max(unique(session)), session=session,
      TRIALS=max(unique(trial)), trial=trial,
      MAXIMUM=length(unique(max.fac)), maximum=max.fac,
      RATIO=length(unique(ratio.fac)), ratio=ratio.fac,
      PLAYERS=unique(nplayers),
      TIMES=max(unique(time)), time=time,
      DECISIONS=length(unique(decision)), decision=decision,
      REWARDS=length(unique(reward)), reward=reward
    ))

    # Fit model
    fit = stan(file = models$stan[[mod]], data = stan.data,
               chains = chains, cores = cores, iter = iter, warmup = warmup, refresh = refresh)

    # Summarise posterior
    # Get generating parameters
    true.pars = rl.pars[sim, names(models$free.pars[[mod]])] %>%
      pivot_longer(everything(), names_to = "par", values_to = "true.pars")

    fit.sum = fit %>%
      tidy_draws() %>%
      select(names(models$free.pars[[mod]])) %>%
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
    mutate(par = factor(par, levels = names(models$free.pars[[mod]]))) # factorise for ordering when plotting

  # Save results for model
  save(results, file = paste("results/rl/alone/parrecov", paste(models$name[[mod]], "Rdata", sep = "."), sep = "/"))


}else{

  # Save results for model
  load(file = paste("results/rl/alone/parrecov", paste(models$name[[mod]], "Rdata", sep = "."), sep = "/"))

}

#### Plot results  ####

# Plot only if results do not already exist (no .jpeg file)
if(!any(grepl("\\.jpeg$", list.files(paste("results/rl/alone/parrecov", sep = "/"))))){

  labels = c(
    expression(alpha["Q,-"]),
    expression(alpha["Q,+"]),
    expression(beta["Q"]), 
    expression(beta["C"])
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
          filename = paste("results/rl/alone/parrecov", paste(models$name[[mod]], "jpeg", sep = "."), sep = "/"))
  print(p)
}else{
  print("Parameter recovery plots already exist. Skipping.")
}