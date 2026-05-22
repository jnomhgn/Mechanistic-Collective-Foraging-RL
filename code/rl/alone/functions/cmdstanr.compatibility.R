# Identify cmdstanr fit objects before dispatching to backend-specific methods.
is_cmdstanr_fit <- function(fit) {
  inherits(fit, "CmdStanMCMC")
}

# Stand in for `as.array()` when draws need to come from either cmdstanr or rstan fits.
draws_array_cmd <- function(fit, variables = NULL) {
  if (is_cmdstanr_fit(fit)) {
    return(fit$draws(variables = variables, format = "draws_array"))
  }

  draws <- as.array(fit)

  if (!is.null(variables)) {
    draws <- draws[, , variables, drop = FALSE]
  }

  draws
}

# Stand in for `traceplot()` using bayesplot draws from either backend.
traceplot_cmd <- function(fit, pars) {
  bayesplot::mcmc_trace(
    draws_array_cmd(fit, variables = pars),
    pars = pars
  )
}

# Stand in for `summary(fit)$summary` with rstan-like column names for downstream code.
fit_summary_cmd <- function(fit, variables = NULL) {
  if (is_cmdstanr_fit(fit)) {
    fit.summary <- fit$summary(variables = variables)
    fit.summary <- as.data.frame(fit.summary)
    fit.summary$n_eff <- fit.summary$ess_bulk
    fit.summary$Rhat <- fit.summary$rhat
    rownames(fit.summary) <- fit.summary$variable
    return(fit.summary)
  }

  if (is.null(variables)) {
    return(summary(fit)$summary)
  }

  summary(fit, pars = variables)$summary
}

# Stand in for `loo::extract_log_lik()` returning the same merged or per-chain layout.
extract_log_lik_cmd <- function(fit, parameter_name = "log_lik", merge_chains = FALSE) {
  if (is_cmdstanr_fit(fit)) {
    draws <- fit$draws(
      variables = parameter_name,
      format = if (merge_chains) "draws_matrix" else "draws_array"
    )

    if (merge_chains) {
      return(as.matrix(draws))
    }

    return(as.array(draws))
  }

  loo::extract_log_lik(fit, parameter_name = parameter_name, merge_chains = merge_chains)
}

# Stand in for `bayesplot::nuts_params()` with a NULL fallback when diagnostics are unavailable.
nuts_params_cmd <- function(fit) {
  tryCatch(
    bayesplot::nuts_params(fit),
    error = function(e) NULL
  )
}

# Stand in for `rhat()` using the unified summary object.
rhat_cmd <- function(fit, pars) {
  fit.summary <- fit_summary_cmd(fit, variables = pars)
  rhats <- fit.summary[, "Rhat"]
  names(rhats) <- rownames(fit.summary)
  rhats
}

# Stand in for `neff_ratio()` using the unified summary and draw count.
neff_ratio_cmd <- function(fit, pars) {
  fit.summary <- fit_summary_cmd(fit, variables = pars)
  posterior.draws <- draws_array_cmd(fit, variables = pars)
  neff.ratios <- fit.summary[, "n_eff"] / posterior::ndraws(posterior.draws)
  names(neff.ratios) <- rownames(fit.summary)
  neff.ratios
}
