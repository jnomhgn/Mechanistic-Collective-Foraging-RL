diagnostics.plot <- function(model.fit, plot.pars){
  
  # Diagnostics plot list 
  diag.list = list()
  
  # Info for plotting 
  posterior <- draws_array_cmd(model.fit, variables = plot.pars)
  np <- nuts_params_cmd(model.fit)
  
  # Plots taken from from https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html
  
  
  # Global plot
  color_scheme_set("darkgray")
  if(is.null(np)){
    global = mcmc_parcoord(posterior)
  }else{
    global = mcmc_parcoord(posterior, np = np)
  }
  diag.list = append(diag.list, list(global))
  
  # Pairs plot
  if(is.null(np)){
    pairs.plot = mcmc_pairs(posterior, pars = plot.pars, off_diag_args = list(size = 0.75))
  }else{
    pairs.plot = mcmc_pairs(posterior, np = np, pars = plot.pars, off_diag_args = list(size = 0.75))
  }
  diag.list = append(diag.list, list(pairs.plot))
  
  # Traceplot
  color_scheme_set("mix-brightblue-gray")
  if(is.null(np)){
    trace.plot = mcmc_trace(posterior, pars = plot.pars)
  }else{
    trace.plot = mcmc_trace(posterior, np=np,
                            pars = plot.pars)
  }
  diag.list = append(diag.list, list(trace.plot))
  
  # Rhat plot
  rhats = rhat_cmd(model.fit, pars = plot.pars)
  color_scheme_set("brightblue") # see help("color_scheme_set")
  if(length(plot.pars) <= 20){
    rhat.plot = mcmc_rhat(rhats) + yaxis_text(hjust = 0) +
      scale_x_continuous(breaks = c(1, 1.01, 1.05, 1.1)) + geom_vline(xintercept = 1.01) 
  }else{
    rhat.plot = mcmc_rhat(rhats) + 
      scale_x_continuous(breaks = c(1, 1.01, 1.05, 1.1)) + geom_vline(xintercept = 1.01) 
  }
  diag.list = append(diag.list, list(rhat.plot))
  
  # Effective sample size ratio
  neff.ratios <- neff_ratio_cmd(model.fit, pars = plot.pars)
  if(length(plot.pars)<20){
    neff.plot = mcmc_neff(neff.ratios, size = 2) + yaxis_text(hjust = 0)
  }else{
    neff.plot = mcmc_neff(neff.ratios, size = 2)
  }
  diag.list = append(diag.list, list(neff.plot))
  
  # Return plot list
  return(diag.list)
  
}