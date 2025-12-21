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
        "code/rl/nocatches/stan/arl.fixed.stan",
        "code/rl/nocatches/stan/dbn1.fixed.stan",
        "code/rl/nocatches/stan/dbn2.fixed.stan",
        "code/rl/nocatches/stan/vsn1.fixed.stan",
        "code/rl/nocatches/stan/vsn2.fixed.stan",
        "code/rl/nocatches/stan/dbr1.fixed.stan",
        "code/rl/nocatches/stan/vsr1.fixed.stan",
        "code/rl/nocatches/stan/dbnvsr1.fixed.stan",
        "code/rl/nocatches/stan/vsndbr1.fixed.stan",
        "code/rl/nocatches/stan/vsnvsr1.fixed.stan",
        "code/rl/nocatches/stan/dbndbr1.fixed.stan"
        
      ),
      # With gq for loglik
      stan.loglik = list(
        "code/rl/nocatches/stan/arl.fixed.ll.stan",
        "code/rl/nocatches/stan/dbn1.fixed.ll.stan",
        "code/rl/nocatches/stan/dbn2.fixed.ll.stan",
        "code/rl/nocatches/stan/vsn1.fixed.ll.stan",
        "code/rl/nocatches/stan/vsn2.fixed.ll.stan",
        "code/rl/nocatches/stan/dbr1.fixed.ll.stan",
        "code/rl/nocatches/stan/vsr1.fixed.ll.stan",
        "code/rl/nocatches/stan/dbnvsr1.fixed.ll.stan",
        "code/rl/nocatches/stan/vsndbr1.fixed.ll.stan",
        "code/rl/nocatches/stan/vsnvsr1.fixed.ll.stan",
        "code/rl/nocatches/stan/dbndbr1.fixed.ll.stan"
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
        "vsr1.hierarch",
        "dbnvsr1.hierarch",
        "vsndbr1.hierarch",
        "vsnvsr1.hierarch",
        "dbndbr1.hierarch"
      ),
      
      sim = list(
        NA,
        NA,
        NA,
        NA,
        NA,
        NA
      ),
      
      # Without gq for loglik
      stan = list(
        "code/rl/nocatches/stan/arl.hierarch.stan",
        "code/rl/nocatches/stan/dbn1.hierarch.stan",
        "code/rl/nocatches/stan/dbn2.hierarch.stan",
        "code/rl/nocatches/stan/vsn1.hierarch.stan",
        "code/rl/nocatches/stan/vsn2.hierarch.stan",
        "code/rl/nocatches/stan/dbr1.hierarch.stan",
        "code/rl/nocatches/stan/vsr1.hierarch.stan",
        "code/rl/nocatches/stan/dbnvsr1.hierarch.stan",
        "code/rl/nocatches/stan/vsndbr1.hierarch.stan",
        "code/rl/nocatches/stan/vsnvsr1.hierarch.stan",
        "code/rl/nocatches/stan/dbndbr1.hierarch.stan"
        
        
      ),
      # With gq for loglik
      stan.loglik = list(
        "code/rl/nocatches/stan/arl.hierarch.ll.stan",
        "code/rl/nocatches/stan/dbn1.hierarch.ll.stan",
        "code/rl/nocatches/stan/dbn2.hierarch.ll.stan",
        "code/rl/nocatches/stan/vsn1.hierarch.ll.stan",
        "code/rl/nocatches/stan/vsn2.hierarch.ll.stan",
        "code/rl/nocatches/stan/dbr1.hierarch.ll.stan",
        "code/rl/nocatches/stan/vsr1.hierarch.ll.stan",
        "code/rl/nocatches/stan/dbnvsr1.hierarch.ll.stan",
        "code/rl/nocatches/stan/vsndbr1.hierarch.ll.stan",
        "code/rl/nocatches/stan/vsnvsr1.hierarch.ll.stan",
        "code/rl/nocatches/stan/dbndbr1.hierarch.ll.stan"
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
  }
  
  
  return(models)
}
