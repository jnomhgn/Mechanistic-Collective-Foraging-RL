getmodels <- function(hierarch=FALSE){
  
  if(hierarch == FALSE){
    
  
    models = list(
      
      # Solo, DB, VS
      name = list(
        "arl",
        "db1",
        "db2",
        "vs1",
        "vs2"
      ),
      
      sim = list(
        "arl.fixed.sim",
        "db1.fixed.sim",
        "db2.fixed.sim",
        "vs1.fixed.sim",
        "vs2.fixed.sim"
      ),
      
      # Without gq for loglik
      stan = list(
        "social/code/stan/arl.fixed.stan",
        "social/code/stan/db1.fixed.stan",
        "social/code/stan/db2.fixed.stan",
        "social/code/stan/vs1.fixed.stan",
        "social/code/stan/vs2.fixed.stan"
      ),
      # With gq for loglik
      stan.loglik = list(
        "social/code/stan/arl.fixed.ll.stan",
        "social/code/stan/db1.fixed.ll.stan",
        "social/code/stan/db2.fixed.ll.stan",
        "social/code/stan/vs1.fixed.ll.stan",
        "social/code/stan/vs2.fixed.ll.stan"
      ),
      
      # Fixed parameters
      fixed.pars = list(
        # arl
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # db1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # db2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vs1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vs2
        list(
          "Q.init" = .5,
          "C.init" = 0
        )
      ),
      
      # Free parameters
      free.pars = list(
        # arl
        list(
          "alphaQN" = c(0, 1), # Individual learning rate for negative rpes
          "alphaQP" = c(0, 1), # Individual learning rate for positive rpes
          "betaQ" = c(0, 10),     # Inverse temp
          "betaC" = c(-4, 4)      # Autocorrelation
        ),
        # db1
        list(
          "alphaQN" = c(0, 1), 
          "alphaQP" = c(0, 1), 
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaDBD" = c(0, 1)    # Social learning rate DB
        ),
        # db2
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
        # vs1
        list(
          "alphaQN" = c(0, 1), 
          "alphaQP" = c(0, 1), 
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          
          "alphaVSD" = c(0, 1)     # Social learning rate VS
        ),
        # vs2
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
        )
        
      ),
      
      # Just which parameters are nested like learningrate[MAXIMUM]
      free.pars.struct = list(
        # arl
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC"
        ),
        # db1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaDBD"
        ),
        # db2
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
        # vs1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSD"
        ),
        # vs2
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
        )
      )
    )
    
    
  }else if(hierarch == TRUE){
    models = list(
      
      # Solo, DB, VS
      name = list(
        "arl.hierarch",
        "db1.hierarch",
        "db2.hierarch",
        "vs1.hierarch",
        "vs2.hierarch"
      ),
      
      sim = list(
        NA,
        NA,
        NA,
        NA,
        NA
      ),
      
      # Without gq for loglik
      stan = list(
        "social/code/stan/arl.hierarch.stan",
        "social/code/stan/db1.hierarch.stan",
        "social/code/stan/db2.hierarch.stan",
        "social/code/stan/vs1.hierarch.stan",
        "social/code/stan/vs2.hierarch.stan"
      ),
      # With gq for loglik
      stan.loglik = list(
        "social/code/stan/arl.hierarch.ll.stan",
        "social/code/stan/db1.hierarch.ll.stan",
        "social/code/stan/db2.hierarch.ll.stan",
        "social/code/stan/vs1.hierarch.ll.stan",
        "social/code/stan/vs2.hierarch.ll.stan"
      ),
      
      # Fixed parameters
      fixed.pars = list(
        # arl
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # db1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # db2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vs1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # vs2
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
        # db1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaDBD" = c(0, 1)    # Social learning rate DB
        ),
        # db2
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
        # vs1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4),
          "alphaVSD" = c(0, 1)     # Social learning rate VS
        ),
        # vs2
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
        # db1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaDBD"
        ),
        # db2
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
        # vs1
        list(
          "alphaQN",
          "alphaQP",
          "betaQ",
          "betaC",
          "alphaVSD"
        ),
        # vs2
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
        )
      )
    )
  }
  
  
  return(models)
}
