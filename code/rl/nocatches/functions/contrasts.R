# All pairwise differences between levels of one factor
pairwise.contrasts = function(marg, labels, factor.name){
  pairs = combn(ncol(marg), 2)
  lapply(seq_len(ncol(pairs)), function(k){
    diff = marg[, pairs[2, k]] - marg[, pairs[1, k]]
    mean_hdi(diff, .width = .9) %>%
      transmute(effect = factor.name,
                contrast = paste(labels[pairs[2, k]], "-", labels[pairs[1, k]]),
                contrast_definition = paste(factor.name, labels[pairs[2, k]], "-", factor.name, labels[pairs[1, k]]),
                mean = y, lower = ymin, upper = ymax)
  }) %>% bind_rows()
}

# Conditional contrasts: ratio differences at each max level
conditional.contrasts = function(arr, max.labels, ratio.labels){
  ratio.pairs = combn(length(ratio.labels), 2)
  pair.grid = expand.grid(j = seq_len(ncol(ratio.pairs)), m = seq_along(max.labels))
  lapply(seq_len(nrow(pair.grid)), function(k){
    m = pair.grid$m[k]
    r = ratio.pairs[, pair.grid$j[k]]
    diff = arr[, m, r[2]] - arr[, m, r[1]]
    mean_hdi(diff, .width = .9) %>%
      transmute(effect = "ratio | max",
                contrast = paste0(ratio.labels[r[2]], " - ", ratio.labels[r[1]], " | ", max.labels[m]),
                contrast_definition = paste0("ratio ", ratio.labels[r[2]], " - ratio ", ratio.labels[r[1]], " at max ", max.labels[m]),
                mean = y, lower = ymin, upper = ymax)
  }) %>% bind_rows()
}

# Interaction contrasts: difference between ratio differences at two max levels
interaction.contrasts = function(arr, max.labels, ratio.labels){
  max.pairs = combn(length(max.labels), 2)
  ratio.pairs = combn(length(ratio.labels), 2)
  pair.grid = expand.grid(i = seq_len(ncol(max.pairs)), j = seq_len(ncol(ratio.pairs)))
  lapply(seq_len(nrow(pair.grid)), function(k){
    m = max.pairs[, pair.grid$i[k]]
    r = ratio.pairs[, pair.grid$j[k]]
    diff = (arr[, m[2], r[2]] - arr[, m[2], r[1]]) -
           (arr[, m[1], r[2]] - arr[, m[1], r[1]])
    mean_hdi(diff, .width = .9) %>%
      transmute(effect = "max x ratio",
                contrast = paste0("(", ratio.labels[r[2]], " - ", ratio.labels[r[1]], ")x(",
                                  max.labels[m[2]], " - ", max.labels[m[1]], ")"),
                contrast_definition = paste0("(ratio ", ratio.labels[r[2]], " - ratio ", ratio.labels[r[1]], ") at max ",
                                             max.labels[m[2]], " vs max ", max.labels[m[1]]),
                mean = y, lower = ymin, upper = ymax)
  }) %>% bind_rows()
}

# Contrasts of one environment-varying parameter
parameter.contrasts = function(par, fit, max.levels, ratio.levels){

  # Draws of population-level weights as array [draw, max, ratio]
  alpha.draws = posterior::as_draws_matrix(fit$draws(par))
  alpha.arr = array(NA, dim = c(nrow(alpha.draws), length(max.levels), length(ratio.levels)))
  for(m in seq_along(max.levels)){
    for(r in seq_along(ratio.levels)){
      alpha.arr[, m, r] = alpha.draws[, sprintf("%s[%d,%d]", par, m, r)]
    }
  }

  # Marginal means per draw
  marg.max = apply(alpha.arr, c(1, 2), mean)
  marg.ratio = apply(alpha.arr, c(1, 3), mean)

  bind_rows(
    pairwise.contrasts(marg.max, max.levels, "max"),
    pairwise.contrasts(marg.ratio, ratio.levels, "ratio"),
    conditional.contrasts(alpha.arr, max.levels, ratio.levels),
    interaction.contrasts(alpha.arr, max.levels, ratio.levels)
  ) %>% mutate(parameter = par, .before = 1)
}
