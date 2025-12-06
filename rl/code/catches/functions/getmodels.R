getmodels <- function(hierarch=FALSE){
  
  if(hierarch == FALSE){
    
  
    models = list(
      
      # Solo, DB, VS
      name = list(
        "arl.fixed",
        "dbn1.fixed",
        "dbn2.fixed",
        "vsn1.fixed",
        "vsn2.fixed",
        "dbr1.fixed", 
        "vsr1.fixed",
        "dbnvsr1.fixed",
        "vsndbr1.fixed",
        "vsnvsr1.fixed",
        "dbndbr1.fixed"
      ),
      
      sim = list(
        "arl.fixed.sim",
        "dbn1.fixed.sim",
        "dbn2.fixed.sim",
        "vsn1.fixed.sim",
        "vsn2.fixed.sim",
        "dbr1.fixed.sim", 
        "vsr1.fixed.sim",
        "dbnvsr1.fixed.sim",
        "vsndbr1.fixed.sim",
        "vsnvsr1.fixed.sim",
        "dbndbr1.fixed.sim"
      ),
      
      # Without gq for loglik
      stan = list(
        "rl/code/catches/stan/arl.fixed.stan",
        "rl/code/catches/stan/dbn1.fixed.stan",
        "rl/code/catches/stan/dbn2.fixed.stan",
        "rl/code/catches/stan/vsn1.fixed.stan",
        "rl/code/catches/stan/vsn2.fixed.stan",
        "rl/code/catches/stan/dbr1.fixed.stan",
        "rl/code/catches/stan/vsr1.fixed.stan",
        "rl/code/catches/stan/dbnvsr1.fixed.stan",
        "rl/code/catches/stan/vsndbr1.fixed.stan",
        "rl/code/catches/stan/vsnvsr1.fixed.stan",
        "rl/code/catches/stan/dbndbr1.fixed.stan"
        
      ),
      # With gq for loglik
      stan.loglik = list(
        "rl/code/catches/stan/arl.fixed.ll.stan",
        "rl/code/catches/stan/dbn1.fixed.ll.stan",
        "rl/code/catches/stan/dbn2.fixed.ll.stan",
        "rl/code/catches/stan/vsn1.fixed.ll.stan",
        "rl/code/catches/stan/vsn2.fixed.ll.stan",
        "rl/code/catches/stan/dbr1.fixed.ll.stan",
        "rl/code/catches/stan/vsr1.fixed.ll.stan",
        "rl/code/catches/stan/dbnvsr1.fixed.ll.stan",
        "rl/code/catches/stan/vsndbr1.fixed.ll.stan",
        "rl/code/catches/stan/vsnvsr1.fixed.ll.stan",
        "rl/code/catches/stan/dbndbr1.fixed.ll.stan"
      ),
      
      # Fixed parameters
      fixed.pars = list(
        # arl
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbn1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbn2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsn1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsn2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbnvsr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsndbr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsnvsr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbndbr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        )
      ),
      
      # Free parameters
      free.pars.pop = list(
        # arl
        list(
          "alphaQN" = c(0, 1), # Individual learning rate for negative rpes 
          "alphaQP" = c(0, 1), # Individual learning rate for positive rpes
          "betaQ" = c(0, 10),     # Inverse temp
          "betaC" = c(-4, 4)      # Autocorrelation
        ),
        # dbn1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaDBD" = c(0, 1)    # Social learning rate DB
        ),
        # dbn2
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          
          "alphaDBD[1,1]" = c(0, 1),
          "alphaDBD[2,1]" = c(0, 1),
          "alphaDBD[3,1]" = c(0, 1),
          "alphaDBD[1,2]" = c(0, 1),
          "alphaDBD[2,2]" = c(0, 1),
          "alphaDBD[3,2]" = c(0, 1),
          "alphaDBD[1,3]" = c(0, 1),
          "alphaDBD[2,3]" = c(0, 1),
          "alphaDBD[3,3]" = c(0, 1),
          "alphaDBD[1,4]" = c(0, 1),
          "alphaDBD[2,4]" = c(0, 1),
          "alphaDBD[3,4]" = c(0, 1)
        ),
        # vsn1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSD" = c(0, 1)     # Social learning rate VS
        ),
        # vsn2
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          
          "alphaVSD[1,1]" = c(0, 1),
          "alphaVSD[2,1]" = c(0, 1),
          "alphaVSD[3,1]" = c(0, 1),
          "alphaVSD[1,2]" = c(0, 1),
          "alphaVSD[2,2]" = c(0, 1),
          "alphaVSD[3,2]" = c(0, 1),
          "alphaVSD[1,3]" = c(0, 1),
          "alphaVSD[2,3]" = c(0, 1),
          "alphaVSD[3,3]" = c(0, 1),
          "alphaVSD[1,4]" = c(0, 1),
          "alphaVSD[2,4]" = c(0, 1),
          "alphaVSD[3,4]" = c(0, 1)
        ),
        
        #  dbr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaDBR" = c(0, 1)    # Social learning rate DB
        ),
        
        #  vsr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSR" = c(0, 1)    # Social learning rate VS
        ),
        
        #  dbnvsr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSR" = c(0, 1),    # Social learning rate VS
          "alphaDBD" = c(0, 1)    # Social learning rate DB
          
        ),
        
        #  vsndbr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSD" = c(0, 1),    # Social learning rate VS
          "alphaDBR" = c(0, 1)    # Social learning rate DB
        ),
        
        #  vsnvsr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSDR" = c(0, 1),    # Social learning rate VS
          "sigmaVSDR" = c(0, 1)    # relative influence of r/d
        ),
        
        #  dbndbr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaDBDR" = c(0, 1),    # Social learning rate DB
          "sigmaDBDR" = c(0, 1)    # relative influence of r/d
        )
        
        
      ),
      
      # Just which parameters are nested like learningrate[MAXIMUM]
      free.pars.pop.struct = list(
        # arl
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC"
        ),
        # dbn1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaDBD"
        ),
        # dbn2
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaDBD" = list(
            "alphaDBD[1,1]",
            "alphaDBD[2,1]",
            "alphaDBD[3,1]",
            "alphaDBD[1,2]",
            "alphaDBD[2,2]",
            "alphaDBD[3,2]",
            "alphaDBD[1,3]",
            "alphaDBD[2,3]",
            "alphaDBD[3,3]",
            "alphaDBD[1,4]",
            "alphaDBD[2,4]",
            "alphaDBD[3,4]"
          )
        ),
        # vsn1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSD"
        ),
        # vsn2
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSD" = list(
            "alphaVSD[1,1]",
            "alphaVSD[2,1]",
            "alphaVSD[3,1]",
            "alphaVSD[1,2]",
            "alphaVSD[2,2]",
            "alphaVSD[3,2]",
            "alphaVSD[1,3]",
            "alphaVSD[2,3]",
            "alphaVSD[3,3]",
            "alphaVSD[1,4]",
            "alphaVSD[2,4]",
            "alphaVSD[3,4]"
          )
        ),
        # dbr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaDBR" = c(0, 1)    # Social learning rate DB
        ),
        
        # vsr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSR" = c(0, 1)    # Social learning rate VS
        ),
        
        # dbnvsr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSR" = c(0, 1),    # Social learning rate VS
          "alphaDBD" = c(0, 1)    # Social learning rate DB
        ),
        
        # vsndbr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSD" = c(0, 1),    # Social learning rate VS
          "alphaDBR" = c(0, 1)    # Social learning rate DB
        ),
        
        # vsnvsr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSDR" = c(0, 1),    # Social learning rate VS
          "sigmaVSDR" = c(0, 1)    # relative influence of r/d
        ),
        
        #  dbndbr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaDBDR" = c(0, 1),    # Social learning rate DB
          "sigmaDBDR" = c(0, 1)    # relative influence of r/d
        )
      )
    )
    
    
  }else if(hierarch == TRUE){
    models = list(
      
      # Solo, DB, VS
      name = list(
        "arl.hierarch",
        "dbn1.hierarch",
        "dbn2.hierarch",
        "vsn1.hierarch",
        "vsn2.hierarch",
        "dbr1.hierarch", 
        "dbr2.hierarch",
        "vsr1.hierarch",
        "vsr2.hierarch",
        "dbnvsr1.hierarch",
        "dbnvsr2.hierarch",
        "vsndbr1.hierarch",
        "vsndbr2.hierarch",
        "vsnvsr1.hierarch",
        "vsnvsr2.hierarch",
        "dbndbr1.hierarch",
        "dbndbr2.hierarch"
      ),
      
      sim = list(
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA, 
        NA,
        NA,
        NA,
        NA,
        NA
      ),
      
      # Without gq for loglik
      stan = list(
        "rl/code/catches/stan/arl.hierarch.stan",
        "rl/code/catches/stan/dbn1.hierarch.stan",
        "rl/code/catches/stan/dbn2.hierarch.stan",
        "rl/code/catches/stan/vsn1.hierarch.stan",
        "rl/code/catches/stan/vsn2.hierarch.stan",
        "rl/code/catches/stan/dbr1.hierarch.stan",
        NA,
        "rl/code/catches/stan/vsr1.hierarch.stan",
        NA, 
        "rl/code/catches/stan/dbnvsr1.hierarch.stan",
        NA, 
        "rl/code/catches/stan/vsndbr1.hierarch.stan",
        NA,
        "rl/code/catches/stan/vsnvsr1.hierarch.stan",
        NA,
        "rl/code/catches/stan/dbndbr1.hierarch.stan",
        NA
        
        
      ),
      # With gq for loglik
      stan.loglik = list(
        "rl/code/catches/stan/arl.hierarch.ll.stan",
        "rl/code/catches/stan/dbn1.hierarch.ll.stan",
        "rl/code/catches/stan/dbn2.hierarch.ll.stan",
        "rl/code/catches/stan/vsn1.hierarch.ll.stan",
        "rl/code/catches/stan/vsn2.hierarch.ll.stan",
        "rl/code/catches/stan/dbr1.hierarch.ll.stan",
        "rl/code/catches/stan/dbr2.hierarch.ll.stan", 
        "rl/code/catches/stan/vsr1.hierarch.ll.stan",
        "rl/code/catches/stan/vsr2.hierarch.ll.stan",
        "rl/code/catches/stan/dbnvsr1.hierarch.ll.stan",
        "rl/code/catches/stan/dbnvsr2.hierarch.ll.stan",
        "rl/code/catches/stan/vsndbr1.hierarch.ll.stan",
        "rl/code/catches/stan/vsndbr2.hierarch.ll.stan",
        "rl/code/catches/stan/vsnvsr1.hierarch.ll.stan",
        "rl/code/catches/stan/vsnvsr2.hierarch.ll.stan",
        "rl/code/catches/stan/dbndbr1.hierarch.ll.stan",
        "rl/code/catches/stan/dbndbr2.hierarch.ll.stan"
      ),
      
      # Fixed parameters
      fixed.pars = list(
        # arl
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbn1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbn2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsn1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsn2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbr2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsr2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbnvsr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbnvsr2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsndbr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsndbr2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsnvsr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vsnvsr2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbndbr1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # dbndbr2
        list(
          "Q.init" = .5,
          "C.init" = 0
        )
      ),
      
      # Free parameters
      free.pars.pop = list(
        # arl
        list(
          "alphaQN" = c(0, 1), # Individual learning rate for negative rpes 
          "alphaQP" = c(0, 1), # Individual learning rate for positive rpes
          "betaQ" = c(0, 10),     # Inverse temp
          "betaC" = c(-4, 4)      # Autocorrelation
        ),
        # dbn1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaDBD" = c(0, 1)    # Social learning rate DB
        ),
        # dbn2
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          
          "alphaDBD[1,1]" = c(0, 1),
          "alphaDBD[2,1]" = c(0, 1),
          "alphaDBD[3,1]" = c(0, 1),
          "alphaDBD[1,2]" = c(0, 1),
          "alphaDBD[2,2]" = c(0, 1),
          "alphaDBD[3,2]" = c(0, 1),
          "alphaDBD[1,3]" = c(0, 1),
          "alphaDBD[2,3]" = c(0, 1),
          "alphaDBD[3,3]" = c(0, 1),
          "alphaDBD[1,4]" = c(0, 1),
          "alphaDBD[2,4]" = c(0, 1),
          "alphaDBD[3,4]" = c(0, 1)
        ),
        # vsn1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSD" = c(0, 1)     # Social learning rate VS
        ),
        # vsn2
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          
          "alphaVSD[1,1]" = c(0, 1),
          "alphaVSD[2,1]" = c(0, 1),
          "alphaVSD[3,1]" = c(0, 1),
          "alphaVSD[1,2]" = c(0, 1),
          "alphaVSD[2,2]" = c(0, 1),
          "alphaVSD[3,2]" = c(0, 1),
          "alphaVSD[1,3]" = c(0, 1),
          "alphaVSD[2,3]" = c(0, 1),
          "alphaVSD[3,3]" = c(0, 1),
          "alphaVSD[1,4]" = c(0, 1),
          "alphaVSD[2,4]" = c(0, 1),
          "alphaVSD[3,4]" = c(0, 1)
        ),
        
        #  dbr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaDBR" = c(0, 1)    # Social learning rate DB
        ),

        # dbr2
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          
          "alphaDBR[1,1]" = c(0, 1),
          "alphaDBR[2,1]" = c(0, 1),
          "alphaDBR[3,1]" = c(0, 1),
          "alphaDBR[1,2]" = c(0, 1),
          "alphaDBR[2,2]" = c(0, 1),
          "alphaDBR[3,2]" = c(0, 1),
          "alphaDBR[1,3]" = c(0, 1),
          "alphaDBR[2,3]" = c(0, 1),
          "alphaDBR[3,3]" = c(0, 1),
          "alphaDBR[1,4]" = c(0, 1),
          "alphaDBR[2,4]" = c(0, 1),
          "alphaDBR[3,4]" = c(0, 1)
        ),
        
        #  vsr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSR" = c(0, 1)    # Social learning rate VS
        ),

        # vsr2
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          
          "alphaVSR[1,1]" = c(0, 1),
          "alphaVSR[2,1]" = c(0, 1),
          "alphaVSR[3,1]" = c(0, 1),
          "alphaVSR[1,2]" = c(0, 1),
          "alphaVSR[2,2]" = c(0, 1),
          "alphaVSR[3,2]" = c(0, 1),
          "alphaVSR[1,3]" = c(0, 1),
          "alphaVSR[2,3]" = c(0, 1),
          "alphaVSR[3,3]" = c(0, 1),
          "alphaVSR[1,4]" = c(0, 1),
          "alphaVSR[2,4]" = c(0, 1),
          "alphaVSR[3,4]" = c(0, 1)
        ),
        
        #  dbnvsr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSR" = c(0, 1),    # Social learning rate VS
          "alphaDBD" = c(0, 1)    # Social learning rate DB
          
        ),

        #  dbnvsr2
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),

          "alphaVSR[1,1]" = c(0, 1),
          "alphaVSR[2,1]" = c(0, 1),
          "alphaVSR[3,1]" = c(0, 1),
          "alphaVSR[1,2]" = c(0, 1),
          "alphaVSR[2,2]" = c(0, 1),
          "alphaVSR[3,2]" = c(0, 1),
          "alphaVSR[1,3]" = c(0, 1),
          "alphaVSR[2,3]" = c(0, 1),
          "alphaVSR[3,3]" = c(0, 1),
          "alphaVSR[1,4]" = c(0, 1),
          "alphaVSR[2,4]" = c(0, 1),
          "alphaVSR[3,4]" = c(0, 1),

          "alphaDBD[1,1]" = c(0, 1),
          "alphaDBD[2,1]" = c(0, 1),
          "alphaDBD[3,1]" = c(0, 1),
          "alphaDBD[1,2]" = c(0, 1),
          "alphaDBD[2,2]" = c(0, 1),
          "alphaDBD[3,2]" = c(0, 1),
          "alphaDBD[1,3]" = c(0, 1),
          "alphaDBD[2,3]" = c(0, 1),
          "alphaDBD[3,3]" = c(0, 1),
          "alphaDBD[1,4]" = c(0, 1),
          "alphaDBD[2,4]" = c(0, 1),
          "alphaDBD[3,4]" = c(0, 1)
          
        ),
        
        #  vsndbr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSD" = c(0, 1),    # Social learning rate VS
          "alphaDBR" = c(0, 1)    # Social learning rate DB
        ),

        #  vsndbr2
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),

          "alphaVSD[1,1]" = c(0, 1),
          "alphaVSD[2,1]" = c(0, 1),
          "alphaVSD[3,1]" = c(0, 1),
          "alphaVSD[1,2]" = c(0, 1),
          "alphaVSD[2,2]" = c(0, 1),
          "alphaVSD[3,2]" = c(0, 1),
          "alphaVSD[1,3]" = c(0, 1),
          "alphaVSD[2,3]" = c(0, 1),
          "alphaVSD[3,3]" = c(0, 1),
          "alphaVSD[1,4]" = c(0, 1),
          "alphaVSD[2,4]" = c(0, 1),
          "alphaVSD[3,4]" = c(0, 1),

          "alphaDBR[1,1]" = c(0, 1),
          "alphaDBR[2,1]" = c(0, 1),
          "alphaDBR[3,1]" = c(0, 1),
          "alphaDBR[1,2]" = c(0, 1),
          "alphaDBR[2,2]" = c(0, 1),
          "alphaDBR[3,2]" = c(0, 1),
          "alphaDBR[1,3]" = c(0, 1),
          "alphaDBR[2,3]" = c(0, 1),
          "alphaDBR[3,3]" = c(0, 1),
          "alphaDBR[1,4]" = c(0, 1),
          "alphaDBR[2,4]" = c(0, 1),
          "alphaDBR[3,4]" = c(0, 1)
        ),
        
        #  vsnvsr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSDR" = c(0, 1),    # Social learning rate VS
          "sigmaVSDR" = c(0, 1)    # relative influence of r/d
        ),

        #  vsnvsr2
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),

          "sigmaVSDR" = c(0, 1),    # relative influence of r/d

          "alphaVSDR[1,1]" = c(0, 1), # Social learning rate VS
          "alphaVSDR[2,1]" = c(0, 1),
          "alphaVSDR[3,1]" = c(0, 1),
          "alphaVSDR[1,2]" = c(0, 1),
          "alphaVSDR[2,2]" = c(0, 1),
          "alphaVSDR[3,2]" = c(0, 1),
          "alphaVSDR[1,3]" = c(0, 1),
          "alphaVSDR[2,3]" = c(0, 1),
          "alphaVSDR[3,3]" = c(0, 1),
          "alphaVSDR[1,4]" = c(0, 1),
          "alphaVSDR[2,4]" = c(0, 1),
          "alphaVSDR[3,4]" = c(0, 1)
        ),
        
        #  dbndbr1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaDBDR" = c(0, 1),    # Social learning rate DB
          "sigmaDBDR" = c(0, 1)    # relative influence of r/d
        ),

       #  dbndbr2
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),

          "sigmaDBDR" = c(0, 1),    # relative influence of r/d

          "alphaDBDR[1,1]" = c(0, 1), # Social learning rate VS
          "alphaDBDR[2,1]" = c(0, 1),
          "alphaDBDR[3,1]" = c(0, 1),
          "alphaDBDR[1,2]" = c(0, 1),
          "alphaDBDR[2,2]" = c(0, 1),
          "alphaDBDR[3,2]" = c(0, 1),
          "alphaDBDR[1,3]" = c(0, 1),
          "alphaDBDR[2,3]" = c(0, 1),
          "alphaDBDR[3,3]" = c(0, 1),
          "alphaDBDR[1,4]" = c(0, 1),
          "alphaDBDR[2,4]" = c(0, 1),
          "alphaDBDR[3,4]" = c(0, 1)
        )
        
        
      ),
      
      # Just which parameters are nested like learningrate[MAXIMUM]
      free.pars.pop.struct = list(
        # arl
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC"
        ),
        # dbn1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaDBD"
        ),
        # dbn2
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaDBD" = list(
            "alphaDBD[1,1]",
            "alphaDBD[2,1]",
            "alphaDBD[3,1]",
            "alphaDBD[1,2]",
            "alphaDBD[2,2]",
            "alphaDBD[3,2]",
            "alphaDBD[1,3]",
            "alphaDBD[2,3]",
            "alphaDBD[3,3]",
            "alphaDBD[1,4]",
            "alphaDBD[2,4]",
            "alphaDBD[3,4]"
          )
        ),
        # vsn1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSD"
        ),
        # vsn2
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSD" = list(
            "alphaVSD[1,1]",
            "alphaVSD[2,1]",
            "alphaVSD[3,1]",
            "alphaVSD[1,2]",
            "alphaVSD[2,2]",
            "alphaVSD[3,2]",
            "alphaVSD[1,3]",
            "alphaVSD[2,3]",
            "alphaVSD[3,3]",
            "alphaVSD[1,4]",
            "alphaVSD[2,4]",
            "alphaVSD[3,4]"
          )
        ),
        # dbr1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaDBR"    # Social learning rate DB
        ),

        # dbr2
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaDBR" = list(
            "alphaDBR[1,1]",
            "alphaDBR[2,1]",
            "alphaDBR[3,1]",
            "alphaDBR[1,2]",
            "alphaDBR[2,2]",
            "alphaDBR[3,2]",
            "alphaDBR[1,3]",
            "alphaDBR[2,3]",
            "alphaDBR[3,3]",
            "alphaDBR[1,4]",
            "alphaDBR[2,4]",
            "alphaDBR[3,4]"
          )
        ),

        
        # vsr1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSR"    # Social learning rate VS
        ),

        # vsr2
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSR" = list(
            "alphaVSR[1,1]",
            "alphaVSR[2,1]",
            "alphaVSR[3,1]",
            "alphaVSR[1,2]",
            "alphaVSR[2,2]",
            "alphaVSR[3,2]",
            "alphaVSR[1,3]",
            "alphaVSR[2,3]",
            "alphaVSR[3,3]",
            "alphaVSR[1,4]",
            "alphaVSR[2,4]",
            "alphaVSR[3,4]"
          )
        ),
        
        # dbnvsr1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSR",    # Social learning rate VS
          "alphaDBD"    # Social learning rate DB
        ),
        
        # dbnvsr2
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSR" = list(
            "alphaVSR[1,1]",
            "alphaVSR[2,1]",
            "alphaVSR[3,1]",
            "alphaVSR[1,2]",
            "alphaVSR[2,2]",
            "alphaVSR[3,2]",
            "alphaVSR[1,3]",
            "alphaVSR[2,3]",
            "alphaVSR[3,3]",
            "alphaVSR[1,4]",
            "alphaVSR[2,4]",
            "alphaVSR[3,4]"
          ),
          "alphaDBD" = list(
            "alphaDBD[1,1]",
            "alphaDBD[2,1]",
            "alphaDBD[3,1]",
            "alphaDBD[1,2]",
            "alphaDBD[2,2]",
            "alphaDBD[3,2]",
            "alphaDBD[1,3]",
            "alphaDBD[2,3]",
            "alphaDBD[3,3]",
            "alphaDBD[1,4]",
            "alphaDBD[2,4]",
            "alphaDBD[3,4]"
          )
        ),
        
        # vsndbr1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSD",    # Social learning rate VS
          "alphaDBR"    # Social learning rate DB
        ),

        # vsndbr2
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",

          "alphaVSD" = list(
            "alphaVSD[1,1]",
            "alphaVSD[2,1]",
            "alphaVSD[3,1]",
            "alphaVSD[1,2]",
            "alphaVSD[2,2]",
            "alphaVSD[3,2]",
            "alphaVSD[1,3]",
            "alphaVSD[2,3]",
            "alphaVSD[3,3]",
            "alphaVSD[1,4]",
            "alphaVSD[2,4]",
            "alphaVSD[3,4]"
          ),

          "alphaDBR" = list(
            "alphaDBR[1,1]",
            "alphaDBR[2,1]",
            "alphaDBR[3,1]",
            "alphaDBR[1,2]",
            "alphaDBR[2,2]",
            "alphaDBR[3,2]",
            "alphaDBR[1,3]",
            "alphaDBR[2,3]",
            "alphaDBR[3,3]",
            "alphaDBR[1,4]",
            "alphaDBR[2,4]",
            "alphaDBR[3,4]"
          )
        ),
        
        # vsnvsr1
        list(
          "alphaQN" ,
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSDR",    # Social learning rate VS
          "sigmaVSDR"   # relative influence of r/d
        ),

        # vsnvsr2
        list(
          "alphaQN" ,
          "alphaQP",
          "betaQ",
          "betaC",
          "sigmaVSDR",   # relative influence of r/d


          "alphaVSDR" = list( # Social learning rate VS
            "alphaVSDR[1,1]",
            "alphaVSDR[2,1]",
            "alphaVSDR[3,1]",
            "alphaVSDR[1,2]",
            "alphaVSDR[2,2]",
            "alphaVSDR[3,2]",
            "alphaVSDR[1,3]",
            "alphaVSDR[2,3]",
            "alphaVSDR[3,3]",
            "alphaVSDR[1,4]",
            "alphaVSDR[2,4]",
            "alphaVSDR[3,4]"
          )
        ),
        
        #  dbndbr1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaDBDR",    # Social learning rate DB
          "sigmaDBDR"   # relative influence of r/d
        ),

        # dbndbr2
        list(
          "alphaQN" ,
          "alphaQP",
          "betaQ",
          "betaC",
          "sigmaDBDR",   # relative influence of r/d


          "alphaDBDR" = list( # Social learning rate VS
            "alphaDBDR[1,1]",
            "alphaDBDR[2,1]",
            "alphaDBDR[3,1]",
            "alphaDBDR[1,2]",
            "alphaDBDR[2,2]",
            "alphaDBDR[3,2]",
            "alphaDBDR[1,3]",
            "alphaDBDR[2,3]",
            "alphaDBDR[3,3]",
            "alphaDBDR[1,4]",
            "alphaDBDR[2,4]",
            "alphaDBDR[3,4]"
          )
        )
      )
    )
  }
  
  
  return(models)
}
