getmodels <- function(hierarch=FALSE){
  
  if(hierarch == FALSE){
    
    
    models = list(
      
      # Solo, DB, VS
      name = list(
        "m1.1",
        "m2.1",
        "m3.1",
        "m4.1",
        "m4.2",
        "m4.3"
      ),
      
      sim = list(
        "m1.1.fixed.sim",
        "m2.1.fixed.sim",
        "m3.1.fixed.sim",
        "m4.1.fixed.sim",
        "m4.2.fixed.sim",
        "m4.3.fixed.sim"
      ),
      
      # Without gq for loglik
      stan = list(
        "code/rl/alone/stan/m1.1.fixed.stan",
        "code/rl/alone/stan/m2.1.fixed.stan",
        "code/rl/alone/stan/m3.1.fixed.stan",
        "code/rl/alone/stan/m4.1.fixed.stan",
        "code/rl/alone/stan/m4.2.fixed.stan",
        "code/rl/alone/stan/m4.3.fixed.stan"
      ),
      # With gq for loglik
      stan.loglik = list(
        "code/rl/alone/stan/m1.1.fixed.ll.stan",
        "code/rl/alone/stan/m2.1.fixed.ll.stan",
        "code/rl/alone/stan/m3.1.fixed.ll.stan",
        "code/rl/alone/stan/m4.1.fixed.ll.stan",
        "code/rl/alone/stan/m4.2.fixed.ll.stan",
        "code/rl/alone/stan/m4.3.fixed.ll.stan"
      ),
      
      # Fixed parameters
      fixed.pars = list(
        # M1.1
        list(
          "Q.init" = .5
        ),
        # M2.1
        list(
          "Q.init" = .5
        ),
        # M3.1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # M4.1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # M4.2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # M4.3
        list(
          "Q.init" = .5,
          "C.init" = 0
        )
      ),
      
      # Free parameters
      free.pars = list(
        # M1.1
        list(
          "alphaQ" = c(0, 1), # Individual learning rate
          "betaQ" = c(0, 10)     # Inverse temp
        ),
        # M2.1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10)
        ),
        # M3.1
        list(
          "alphaQ" = c(0, 1), # Individual learning rate
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        ),
        # M4.1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        ),
        # M4.2
        list(
          "alphaQN[1]" = c(0, 1),
          "alphaQN[2]" = c(0, 1),
          "alphaQN[3]" = c(0, 1),
          "alphaQP[1]" = c(0, 1),
          "alphaQP[2]" = c(0, 1),
          "alphaQP[3]" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        ),
        # M4.3
        list(
          "alphaQN[1,1]" = c(0, 1),
          "alphaQN[2,1]" = c(0, 1),
          "alphaQN[3,1]" = c(0, 1),
          "alphaQN[1,2]" = c(0, 1),
          "alphaQN[2,2]" = c(0, 1),
          "alphaQN[3,2]" = c(0, 1),
          "alphaQN[1,3]" = c(0, 1),
          "alphaQN[2,3]" = c(0, 1),
          "alphaQN[3,3]" = c(0, 1),
          "alphaQN[1,4]" = c(0, 1),
          "alphaQN[2,4]" = c(0, 1),
          "alphaQN[3,4]" = c(0, 1),
          "alphaQP[1,1]" = c(0, 1),
          "alphaQP[2,1]" = c(0, 1),
          "alphaQP[3,1]" = c(0, 1),
          "alphaQP[1,2]" = c(0, 1),
          "alphaQP[2,2]" = c(0, 1),
          "alphaQP[3,2]" = c(0, 1),
          "alphaQP[1,3]" = c(0, 1),
          "alphaQP[2,3]" = c(0, 1),
          "alphaQP[3,3]" = c(0, 1),
          "alphaQP[1,4]" = c(0, 1),
          "alphaQP[2,4]" = c(0, 1),
          "alphaQP[3,4]" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        )
        
      ),
      
      # Just which parameters are nested like learningrate[MAXIMUM]
      free.pars.struct = list(
        # M1.1
        list(
          "alphaQ" = c(0, 1), # Individual learning rate
          "betaQ" = c(0, 10)     # Inverse temp
        ),
        # M2.1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10)
        ),
        # M3.1
        list(
          "alphaQ" = c(0, 1), # Individual learning rate
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        ),
        # M4.1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        ),
        # M4.2
        list(
          "alphaQN" = list(
            "alphaQN[1]",
            "alphaQN[2]",
            "alphaQN[3]"
          ),
          "alphaQP" = list(
            "alphaQP[1]",
            "alphaQP[2]",
            "alphaQP[3]"
          ),
          "betaQ",
          "betaC"
        ),
        # M4.3
        list(
          "alphaQN" = list(
            "alphaQN[1,1]" = c(0, 1),
            "alphaQN[2,1]" = c(0, 1),
            "alphaQN[3,1]" = c(0, 1),
            "alphaQN[1,2]" = c(0, 1),
            "alphaQN[2,2]" = c(0, 1),
            "alphaQN[3,2]" = c(0, 1),
            "alphaQN[1,3]" = c(0, 1),
            "alphaQN[2,3]" = c(0, 1),
            "alphaQN[3,3]" = c(0, 1),
            "alphaQN[1,4]" = c(0, 1),
            "alphaQN[2,4]" = c(0, 1),
            "alphaQN[3,4]" = c(0, 1)
          ),
          "alphaQP" = list(
            "alphaQP[1,1]" = c(0, 1),
            "alphaQP[2,1]" = c(0, 1),
            "alphaQP[3,1]" = c(0, 1),
            "alphaQP[1,2]" = c(0, 1),
            "alphaQP[2,2]" = c(0, 1),
            "alphaQP[3,2]" = c(0, 1),
            "alphaQP[1,3]" = c(0, 1),
            "alphaQP[2,3]" = c(0, 1),
            "alphaQP[3,3]" = c(0, 1),
            "alphaQP[1,4]" = c(0, 1),
            "alphaQP[2,4]" = c(0, 1),
            "alphaQP[3,4]" = c(0, 1)
          ),
          "betaQ",
          "betaC"
        )
      )
    )
    
    
  }else if(hierarch == TRUE){
    
    models = list(
      
      # Solo, DB, VS
      name = list(
        "m1.1",
        "m2.1",
        "m3.1",
        "m4.1",
        "m4.2",
        "m4.3"
      ),
      
      sim = list(
        "m1.1.hierarch.sim",
        "m2.1.hierarch.sim",
        "m3.1.hierarch.sim",
        "m4.1.hierarch.sim",
        "m4.2.hierarch.sim",
        "m4.3.hierarch.sim"
      ),
      
      # Without gq for loglik
      stan = list(
        "code/rl/alone/stan/m1.1.hierarch.stan",
        "code/rl/alone/stan/m2.1.hierarch.stan",
        "code/rl/alone/stan/m3.1.hierarch.stan",
        "code/rl/alone/stan/m4.1.hierarch.stan",
        "code/rl/alone/stan/m4.2.hierarch.stan",
        "code/rl/alone/stan/m4.3.hierarch.stan"
      ),
      # With gq for loglik
      stan.loglik = list(
        "code/rl/alone/stan/m1.1.hierarch.ll.stan",
        "code/rl/alone/stan/m2.1.hierarch.ll.stan",
        "code/rl/alone/stan/m3.1.hierarch.ll.stan",
        "code/rl/alone/stan/m4.1.hierarch.ll.stan",
        "code/rl/alone/stan/m4.2.hierarch.ll.stan",
        "code/rl/alone/stan/m4.3.hierarch.ll.stan"
      ),
      
      # Fixed parameters
      fixed.pars = list(
        # M1.1
        list(
          "Q.init" = .5
        ),
        # M2.1
        list(
          "Q.init" = .5
        ),
        # M3.1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # M4.1
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # M4.2
        list(
          "Q.init" = .5,
          "C.init" = 0
        ),
        # M4.3
        list(
          "Q.init" = .5,
          "C.init" = 0
        )
      ),
      
      # Free parameters
      free.pars.pop = list(
        # M1.1
        list(
          "alphaQ" = c(0, 1), # Individual learning rate
          "betaQ" = c(0, 10)     # Inverse temp
        ),
        # M2.1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10)
        ),
        # M3.1
        list(
          "alphaQ" = c(0, 1), # Individual learning rate
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        ),
        # M4.1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        ),
        # M4.2
        list(
          "alphaQN[1]" = c(0, 1),
          "alphaQN[2]" = c(0, 1),
          "alphaQN[3]" = c(0, 1),
          "alphaQP[1]" = c(0, 1),
          "alphaQP[2]" = c(0, 1),
          "alphaQP[3]" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        ),
        # M4.3
        list(
          "alphaQN[1,1]" = c(0, 1),
          "alphaQN[2,1]" = c(0, 1),
          "alphaQN[3,1]" = c(0, 1),
          "alphaQN[1,2]" = c(0, 1),
          "alphaQN[2,2]" = c(0, 1),
          "alphaQN[3,2]" = c(0, 1),
          "alphaQN[1,3]" = c(0, 1),
          "alphaQN[2,3]" = c(0, 1),
          "alphaQN[3,3]" = c(0, 1),
          "alphaQN[1,4]" = c(0, 1),
          "alphaQN[2,4]" = c(0, 1),
          "alphaQN[3,4]" = c(0, 1),
          
          "alphaQP[1,1]" = c(0, 1),
          "alphaQP[2,1]" = c(0, 1),
          "alphaQP[3,1]" = c(0, 1),
          "alphaQP[1,2]" = c(0, 1),
          "alphaQP[2,2]" = c(0, 1),
          "alphaQP[3,2]" = c(0, 1),
          "alphaQP[1,3]" = c(0, 1),
          "alphaQP[2,3]" = c(0, 1),
          "alphaQP[3,3]" = c(0, 1),
          "alphaQP[1,4]" = c(0, 1),
          "alphaQP[2,4]" = c(0, 1),
          "alphaQP[3,4]" = c(0, 1),
          
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        )
      ),
      
      # Just which parameters are nested like learningrate[MAXIMUM]
      free.pars.pop.struct = list(
        # M1.1
        list(
          "alphaQ" = c(0, 1), # Individual learning rate
          "betaQ" = c(0, 10)     # Inverse temp
        ),
        # M2.1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10)
        ),
        # M3.1
        list(
          "alphaQ" = c(0, 1), # Individual learning rate
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        ),
        # M4.1
        list(
          "alphaQN" = c(0, 1),
          "alphaQP" = c(0, 1),
          "betaQ" = c(0, 10),
          "betaC" = c(-4, 4)
        ),
        # M4.2
        list(
          "alphaQN" = list(
            "alphaQN[1]",
            "alphaQN[2]",
            "alphaQN[3]"
          ),
          "alphaQP" = list(
            "alphaQP[1]",
            "alphaQP[2]",
            "alphaQP[3]"
          ),
          "betaQ",
          "betaC"
        ),
        # M 4.3
        list(
          "alphaQN" = list(
            "alphaQN[1,1]" = c(0, 1),
            "alphaQN[2,1]" = c(0, 1),
            "alphaQN[3,1]" = c(0, 1),
            "alphaQN[1,2]" = c(0, 1),
            "alphaQN[2,2]" = c(0, 1),
            "alphaQN[3,2]" = c(0, 1),
            "alphaQN[1,3]" = c(0, 1),
            "alphaQN[2,3]" = c(0, 1),
            "alphaQN[3,3]" = c(0, 1),
            "alphaQN[1,4]" = c(0, 1),
            "alphaQN[2,4]" = c(0, 1),
            "alphaQN[3,4]" = c(0, 1)
          ),
          "alphaQP" = list(
            "alphaQP[1,1]" = c(0, 1),
            "alphaQP[2,1]" = c(0, 1),
            "alphaQP[3,1]" = c(0, 1),
            "alphaQP[1,2]" = c(0, 1),
            "alphaQP[2,2]" = c(0, 1),
            "alphaQP[3,2]" = c(0, 1),
            "alphaQP[1,3]" = c(0, 1),
            "alphaQP[2,3]" = c(0, 1),
            "alphaQP[3,3]" = c(0, 1),
            "alphaQP[1,4]" = c(0, 1),
            "alphaQP[2,4]" = c(0, 1),
            "alphaQP[3,4]" = c(0, 1)
          ),
          "betaQ",
          "betaC"
        )
        
      )
    )
    
  }
  
  
  return(models)
}
