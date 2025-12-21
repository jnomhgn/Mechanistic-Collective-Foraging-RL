# Function to simulate synthetic data
m4.3.fixed.sim <- function(sim.parameters){
  
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
        vars = c("Q1", "Q2", "p1", "p2", "decision", "reward",
                 "qpe1", "qpe2")
        trial.data = lapply(vars, function(x) array(NA, dim = c(durations[trial]+1, nplayers)))
        names(trial.data) = vars
        
        # Derive catch probabilities for trial
        reward.probs = c(ratio[trial] * max[trial], max[trial])
        
        # Initialize individual Q-values
        Q <- matrix(Q.init, nrow = nplayers, ncol = 2 )
        
        # Initialize choice trace (only last trial in our case) to 0 (Katahira 2018)
        C = matrix(C.init, nrow = nplayers, ncol = 2 )

        
        # Loop over trial
        for(time in 0:durations[trial]){

          
          # Loop over individuals
          for(player in 1:nplayers){
            
            # Derive choice probability
            p = softmax(betaQ * Q[player, ] + betaC * C[player, ])
            
            # Decide
            decision = sample(c(1, 2), size = 1, prob = p)

            
            # Sample reward
            reward = rbinom(n = 1, size = 1, prob = reward.probs[decision])

            
            # Store info for t
            trial.data$decision[time+1, player] = decision
            trial.data$reward[time+1, player] = reward
            trial.data$Q1[time+1, player] = Q[player, 1]
            trial.data$Q2[time+1, player] = Q[player, 2]
            trial.data$p1[time+1, player] = p[1]
            trial.data$p2[time+1, player] = p[2]

             
            # Update Q-values for t + 1
            if(reward == 0){
              Q[player, decision] = Q[player, decision] + alphaQN[[max.fac[trial], ratio.fac[trial]]] * (reward - Q[player, decision])
            }else{
              Q[player, decision] = Q[player, decision] + alphaQP[[max.fac[trial], ratio.fac[trial]]] * (reward - Q[player, decision])
            }
            
            # Update choice trace for t + 1
            C[player, ] = c(0, 0) # Indicator function. 
            C[player, decision] = 1 # Chosen option increases choice trace
            
          }
          
        }
        
        # Transform
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

    
    
    return(sim.data)
  })
}