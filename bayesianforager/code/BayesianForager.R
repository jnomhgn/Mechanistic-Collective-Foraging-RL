
#Optimal Asocial foraging: Bayesian Inference simulations
library(reticulate)

# Create results directories
getwd()
if(!dir.exists("bayesianforager/results")){dir.create("bayesianforager/results")}
resultsdir = "bayesianforager/results"

#Numerical simulation of difference between two beta distributions
diff_beta <- function(a1,b1,a2,b2, N_sample){
  p1 <- rbeta(N_sample, a1, b1)
  p2 <- rbeta(N_sample, a2, b2)
  return(p2-p1)
}

#Analytical solution for the probability that patch 2 is better
#Following the derivation found here:https://www.evanmiller.org/bayesian-ab-testing.html
prob_2_better <- function(a1,b1,a2,b2){
  return(sum(sapply(0:(a2-1), function(i) (beta(a1+i, b1+b2))/( (b2+i)*beta(1+i,b2)*beta(a1,b1) ) ) ))
}

#Each second, agents decide between different patches to forage at, based on the belief distributions
#over reward probabilities in both patches

Sim_fct <- function(Nsim = 100,
                    Tmax  = 75,        #Seconds per round
                    N      = 1,        #Number of foragers
                    N_options = 2,     #Number of patches
                    max_catch = 0.5,   #Maximum catch probability
                    pond_ratio = 0.5,  #Catch prob ratios
                    type = 2,          #1= numerical simulation; 2 = analytical solution
                    init_better        #Agent initialized in better patch 
)       
{
  
  #Overall list
  Combined_list <- list()
  
  #Loop over 100 simulations
  for (sim in 1:Nsim) {
  
  p_catch    <- max_catch*pond_ratio^(0:(N_options-1))    #Assign initial catch probabilities for each lake
  Beta_counts<- array(1, dim = c(N,N_options,2 ) )        #Counts for beta distribution
  patch      <- ifelse(init_better == 1, 1, 2)            #Assign patch
  
  #Create output object to record choices and payoffs participants received as well as beta counts for belief distribution
  Result <- list(id=rep(1:N, each  = Tmax),
                 time = rep(1:Tmax, N),
                 Patch=NA,
                 Payoff=NA,
                 Best = NA)
  
  # Start simulation loop
  for (t in 1:Tmax) {
    
    #Record best patch
    Result$Best[which(Result$time==t)]  <- which.max(p_catch)
    
    #Loop over all individuals
    for (id in 1:N){
      
      #Record patch
      Result$Patch[which(Result$id==id & Result$time==t)]  <- patch[id]
      
      #Catch fish with certain probability  
      payoff <- rbinom(1, 1, p_catch[patch[id]])
      
      #Record Payoff
      Result$Payoff[which(Result$id==id & Result$time==t)]  <- payoff
      
      #Update counts based on payoff
      if (payoff ==1){
        Beta_counts[id,patch[id],1] <- Beta_counts[id,patch[id],1] + 1
      }else{
        Beta_counts[id,patch[id],2] <- Beta_counts[id,patch[id],2] + 1
      }
     
      if ( type == 1){ 
      #Agents switch probabilistically based on the posterior distribution of the difference between both patches
       p_diff <- diff_beta(Beta_counts[id,1,1],
                           Beta_counts[id,1,2],
                           Beta_counts[id,2,1],
                           Beta_counts[id,2,2], 
                           1e3)
       choice <- ifelse(sample(p_diff, 1)>0, 2, 1)
      } else {
      #Alternatively, agents infer which patch is better 
      p2 <- prob_2_better(Beta_counts[id,1,1],
                          Beta_counts[id,1,2],
                          Beta_counts[id,2,1],
                          Beta_counts[id,2,2])
      
      choice <- sample(c(1,2), 1, prob = c(1-p2,p2))
      }
      #Update patch
      patch[id] <- choice
      
    }#individual id
    
  }#t
  
  Combined_list[[sim]]<- Result
  
  }#sim
  
  return(Combined_list)    
}#sim_funct

#Get correct initial conditions from empirical data
data_long = read.csv("data/processed/data_long.csv")
d <- data_long[which(data_long$cond == 1 & data_long$time==0),]

#Pass to mclapply; it makes sense to select as many cores as there are parameter combinations in case you have access to a computer cluster ("mc.cores" argument)

library(parallel)

result <- mclapply(
  1:nrow(d) ,
  function(i) Sim_fct(100, d$duration[i], 1, 2, d$max[i], d$ratio[i], 2, d$correct[i]),
  mc.cores=30)

#Calculate mean accuracies for each environment
mean_accuracies <- array(NA, c(3,4))

for (i in seq(0.5,0.9,0.2)) {
  for (j in seq(0.5,0.95,0.15)) {
    indices <- which(d$max == i & d$ratio == j)
    accuracies <- c()
    for (ind in indices) {
      for (sim in 1:100) {
      accuracies <- c(accuracies, ifelse( result[[ind]][[sim]]$Patch == 1, 1,0  ) )
      }
    }
    mean_accuracies[which(seq(0.5,0.9,0.2)==i), which(seq(0.5,0.95,0.15) == j)] <- mean(accuracies)
  }
}

#Calculate mean accuracies for each environment over time
accuracies_time <- array(NA, c(3,4, 75))

for (i in seq(0.5,0.9,0.2)) {
  for (j in seq(0.5,0.95,0.15)) {
    indices <- which(d$max == i & d$ratio == j)
    accuracies <- matrix(NA, 100 * length(indices), 105)
    counter = 1
    for (ind in indices) {
      for (sim in 1:100) {
        duration <- length(result[[ind]][[sim]]$Patch)
        accuracies[counter,1:duration] <-  ifelse( result[[ind]][[sim]]$Patch == 1, 1,0  ) 
        counter <- counter + 1
      }
    }
    
    accuracies_time[which(seq(0.5,0.9,0.2)==i), which(seq(0.5,0.95,0.15) == j),] <- apply(accuracies[,1:75], 2, mean)
  }
}

# Export as numpy arrays 
np <- import("numpy")
mean_accuracies <- np$array(mean_accuracies)
np$save(file.path(resultsdir, "mean_accuracies") , mean_accuracies)
accuracies_time <- np$array(accuracies_time)
np$save(file.path(resultsdir, "accuracies_time") , accuracies_time)