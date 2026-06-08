plot.par.recov <- function(model.name, results, plot.pars, par.symbols){
  
  p = results %>%
    ggplot(aes(x=true.pars, y=.mean)) + 
    geom_pointrange(aes(y=.mean, ymin=.lower, ymax=.upper)) +
    stat_cor(aes(y=.mean, label = after_stat(r.label)), method = "pearson") +
    labs(x="Generating Parameter Value", y="Posterior") +
    theme_gray(base_size = 11) +
    theme(text = element_text(size=rel(5)),
          plot.title = element_text(hjust = 0.5, size = rel(8)),
          strip.text = element_text(size = rel(5)),
          plot.margin = margin(1,1,1,1, "cm")) +
    facet_wrap(~ par, scales = "free") 
  p = annotate_figure(p, top = model.name)
  
  return(p)
  
}
