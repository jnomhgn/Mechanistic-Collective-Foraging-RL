# Function to simulate synthetic data
dbndbr1.fixed.sim <- function(sim.parameters){
  
  with(data = sim.parameters, expr = {
    
    # Create list to save all the simulated data to
    sim.data <- list()
    
    # Create levels to index resource abundance offsets with
    max.fac = ifelse(max == .5, 1, 
                     ifelse(max == .7, 2, 3))
    ratio.fac = ifelse(ratio == .5, 1,
                       ifelse(ratio == .65, 2,
                              ifelse(ratio == .8, 3,
                                     4)))
    
    # Loop over sessions
    for(session in 1:sessions){
      
      # Permute duration for each session (so that counterbalanced across trials)
      durations = sample(rep(durations.vec, each=trials/length(durations.vec)),
                         size=trials, replace = FALSE)
      
      # Loop over trials
      for(trial in 1:trials){
        
        # Create list to save trial data to
        vars = c("Q1", "Q2", "p1", "p2", "decision", "reward")
        trial.data = lapply(vars, function(x) array(NA, dim = c(durations[trial]+1, nplayers)))
        names(trial.data) = vars
        
        # Derive catch probabilities for trial
        reward.probs = c(ratio[trial] * max[trial], max[trial])
        
        # Create local Q-values and initialize / reset on each trial
        Q <- matrix(Q.init, nrow = nplayers, ncol = 2 )
        
        # Initialize choice trace (only last trial in our case) to 0 (Katahira 2018)
        C = matrix(C.init, nrow = nplayers, ncol = 2 )
        
        # Initialize matrix of social choice probabilities
        p.soc = matrix(NA, nrow = nplayers, ncol = 2)
        
        # Track distribution of decisions
        dec.freq = array(NA, dim = c(nplayers))
        
        # Track distribution of rewards
        rew.freq = array(NA, dim = c(nplayers))
        
        # Loop over trial
        for(time in 0:durations[trial]){
          
          # Social information is computed at the beginning of each timestep. Isn't available / used on the first time step
          if(time != 0){
            
            # Number of other group members choosing each option
            obs.dec = sapply(1:2, function(x) dec.freq == x)
            obs.dec = t(sapply(1:nplayers, function(x) colSums(obs.dec[-x, ])))
            
            # Select only for chosen option for convex combination
            obs.dec = sapply(1:nplayers, function(x) obs.dec[x, dec.freq[x]])
            
            # Number of other group members at same patch obtaining rewards
            obs.rew = sapply(1:nplayers, function(x) sum(rew.freq[dec.freq == dec.freq[x]]) - rew.freq[x])
            
            # Normalize both
            obs.rew = sapply(1:nplayers, function(x) obs.rew[x] / obs.dec[x])
            obs.dec = obs.dec / (nplayers-1)
            
            # Convex combination
            for(x in 1:nplayers){
              if(is.na(obs.rew[x])){
                p.soc[x, dec.freq[x]] = obs.dec[x]
                p.soc[x, 3 - dec.freq[x]] = 1 - obs.dec[x]
              }else{
                p.soc[x, dec.freq[x]] = (1 - sigmaDBDR) * obs.dec[x] + sigmaDBDR * obs.rew[x]
                p.soc[x, 3 - dec.freq[x]] = 1 - p.soc[x, dec.freq[x]]
              }
              
            }


          }
          
          # Loop over individuals
          for(player in 1:nplayers){
            
            # Derive choice probability
            p = softmax(betaQ * Q[player, ] + betaC * C[player, ])
            
            # Bias decision using social information
            if(time != 0){
              p = p + alphaDBDR * (p.soc[player, ] - p) 
            }
            
            
            # Decide
            decision = sample(c(1, 2), size = 1, prob = p)
            dec.freq[player] = decision
            
            
            # Sample reward
            reward = rbinom(n = 1, size = 1, prob = reward.probs[decision])
            rew.freq[player] = reward
            
            
            # Store info for t
            trial.data$decision[time+1, player] = decision
            trial.data$reward[time+1, player] = reward
            trial.data$Q1[time+1, player] = Q[player, 1]
            trial.data$Q2[time+1, player] = Q[player, 2]
            trial.data$p1[time+1, player] = p[1]
            trial.data$p2[time+1, player] = p[2]
            
            
            # Update Q-values for t + 1
            if(reward == 0){
              Q[player, decision] = Q[player, decision] + alphaQN * (reward - Q[player, decision])
            }else{
              Q[player, decision] = Q[player, decision] + alphaQP * (reward - Q[player, decision])
            }
            
            # Update choice trace for t + 1
            C[player, ] = c(0, 0) # Indicator function. 
            C[player, decision] = 1 # Chosen option increases choice trace
            
          }
          
        }
        
        # Transform trial data
        trial.data = lapply(1:length(trial.data), function(x)
          trial.data[[x]] %>%
            as.data.frame() %>%
            `colnames<-`(id[session, ]) %>%
            mutate(time = 0:durations[trial]) %>%
            pivot_longer(-time, names_to = "player", values_to = names(trial.data)[x],
                         names_transform = list(player=as.numeric)) 
        )
        
        trial.data = bind_cols(trial.data, .name_repair = "minimal") 
        trial.data = trial.data[!duplicated(names(trial.data))] 
        trial.data = trial.data %>% 
          cbind(session, trial, max=max[trial], max.fac=max.fac[trial],
                ratio=ratio[trial], ratio.fac=ratio.fac[trial],
                duration=durations[trial],
                nplayers=nplayers)
        
        # Store trial data in results list
        sim.data = append(sim.data, list(trial.data))
      }
      
    }
    
    # Bind 
    sim.data = bind_rows(sim.data) %>% 
      relocate(session, trial, player, time, duration, max, ratio) %>% 
      arrange(session, trial, player, time)
    
    # Compute observed decisions and observed payoffs
    sim.data=sim.data %>%
      mutate(decision = decision - 1) %>% # Transform [1, 2] to [0, 1]
      arrange(session, trial, player, time) %>%
      # Observed decisions
      group_by(session, trial, player) %>%
      mutate(decision.lag = lag(decision)) %>%
      group_by(session, trial, time) %>%
      mutate(obs.dec.2 = sum(decision.lag)) %>% # Number of individuals choosing higher yielding patch
      ungroup() %>%
      mutate(obs.dec.2 = obs.dec.2 - decision.lag) %>% # Subtract individual's own choice
      ungroup() %>%
      mutate(obs.dec.1 = nplayers - 1 - obs.dec.2) %>% # Lower yielding patch
      mutate(obs.dec.1.norm = obs.dec.1 / (nplayers-1), obs.dec.2.norm = obs.dec.2 / (nplayers-1)) %>% # normalize
      relocate(obs.dec.1, obs.dec.2,.after = nplayers) %>%
      # Observed payoffs
      arrange(session, trial, player, time) %>%
      group_by(session, trial, player) %>%
      mutate(reward.lag = lag(reward)) %>% 
      group_by(session, trial, time, decision.lag) %>%
      mutate(obs.rew = sum(reward.lag)) %>% # Rewards obtained by all players at each patch
      group_by(session, trial, player) %>%
      mutate(obs.rew = obs.rew - reward.lag) %>% # Minus rewards obtained by individuals themselves
      mutate(obs.rew.norm = ifelse(decision.lag == 0, obs.rew / obs.dec.1, obs.rew / obs.dec.2)) %>% # normalize
      # mutate(obs.rew.norm = ifelse(decision.lag == 0, 
      #                              ifelse(obs.dec.1 == 0, 0, obs.rew / obs.dec.1),
      #                              ifelse(obs.dec.2 == 0, 0, obs.rew / obs.dec.2))) %>% # normalize
      mutate(decision = decision + 1) %>% # Transform [0, 1] to [1, 2]
      arrange(session, trial, player, time) 
    
    return(sim.data)
  })
}