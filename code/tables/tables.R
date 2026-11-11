#### Setup ####

setwd(here::here())

resultsdir <- file.path("results", "tables")
if (!dir.exists(resultsdir)) dir.create(resultsdir, recursive = TRUE)

#### Parameter estimates table ####

diagnostics_files <- list(
  A  = file.path("results", "rl", "alone", "modelcomp", "diagnostics", "m4.1.diagnostics.csv"),
  NC = file.path("results", "rl", "nocatches", "modelcomp", "adaptive", "diagnostics", "vsn2.hierarch.diagnostics.csv"),
  C  = file.path("results", "rl", "catches", "modelcomp", "diagnostics", "vsndbr2.hierarch.diagnostics.csv")
)

model_names <- list(
  A  = "$ARL_{H,\\pm}$",
  NC = "$VS_{l[m,r]}$",
  C  = "$VS_{l[m,r]}DB_{r[m,r]}$"
)

# Map raw Stan variable names to the LaTeX parameter symbols used in the write-up
symbol_map <- c(
  "alphaQN" = "$\\alpha_{P,-}$",
  "alphaQP" = "$\\alpha_{P,+}$",
  "betaQ"   = "$\\beta_{Q}$",
  "betaC"   = "$\\beta_{H}$",
  "alphaVSD[1,1]" = "$\\alpha_{VSl[.5, .5]}$",
  "alphaVSD[2,1]" = "$\\alpha_{VSl[.7, .5]}$",
  "alphaVSD[3,1]" = "$\\alpha_{VSl[.9, .5]}$",
  "alphaVSD[1,2]" = "$\\alpha_{VSl[.5, .65]}$",
  "alphaVSD[2,2]" = "$\\alpha_{VSl[.7, .65]}$",
  "alphaVSD[3,2]" = "$\\alpha_{VSl[.9, .65]}$",
  "alphaVSD[1,3]" = "$\\alpha_{VSl[.5, .8]}$",
  "alphaVSD[2,3]" = "$\\alpha_{VSl[.7, .8]}$",
  "alphaVSD[3,3]" = "$\\alpha_{VSl[.9, .8]}$",
  "alphaVSD[1,4]" = "$\\alpha_{VSl[.5, .95]}$",
  "alphaVSD[2,4]" = "$\\alpha_{VSl[.7, .95]}$",
  "alphaVSD[3,4]" = "$\\alpha_{VSl[.9, .95]}$",
  "alphaDBR[1,1]" = "$\\alpha_{DBr[.5, .5]}$",
  "alphaDBR[2,1]" = "$\\alpha_{DBr[.7, .5]}$",
  "alphaDBR[3,1]" = "$\\alpha_{DBr[.9, .5]}$",
  "alphaDBR[1,2]" = "$\\alpha_{DBr[.5, .65]}$",
  "alphaDBR[2,2]" = "$\\alpha_{DBr[.7, .65]}$",
  "alphaDBR[3,2]" = "$\\alpha_{DBr[.9, .65]}$",
  "alphaDBR[1,3]" = "$\\alpha_{DBr[.5, .8]}$",
  "alphaDBR[2,3]" = "$\\alpha_{DBr[.7, .8]}$",
  "alphaDBR[3,3]" = "$\\alpha_{DBr[.9, .8]}$",
  "alphaDBR[1,4]" = "$\\alpha_{DBr[.5, .95]}$",
  "alphaDBR[2,4]" = "$\\alpha_{DBr[.7, .95]}$",
  "alphaDBR[3,4]" = "$\\alpha_{DBr[.9, .95]}$"
)

asocial_vars <- c("alphaQN", "alphaQP", "betaQ", "betaC")

# Social parameters, ordered r-major (matches the write-up's row order)
vs_vars <- c(
  "alphaVSD[1,1]", "alphaVSD[2,1]", "alphaVSD[3,1]",
  "alphaVSD[1,2]", "alphaVSD[2,2]", "alphaVSD[3,2]",
  "alphaVSD[1,3]", "alphaVSD[2,3]", "alphaVSD[3,3]",
  "alphaVSD[1,4]", "alphaVSD[2,4]", "alphaVSD[3,4]"
)
db_vars <- c(
  "alphaDBR[1,1]", "alphaDBR[2,1]", "alphaDBR[3,1]",
  "alphaDBR[1,2]", "alphaDBR[2,2]", "alphaDBR[3,2]",
  "alphaDBR[1,3]", "alphaDBR[2,3]", "alphaDBR[3,3]",
  "alphaDBR[1,4]", "alphaDBR[2,4]", "alphaDBR[3,4]"
)

param_vars <- list(
  A  = asocial_vars,
  NC = c(asocial_vars, vs_vars),
  C  = c(asocial_vars, vs_vars, db_vars)
)

# Condition A
pars_a <- read.csv(diagnostics_files$A)
pars_a[, 3:13] <- round(pars_a[, 3:13], 3)
pars_a <- pars_a[match(param_vars$A, pars_a$variable), c("variable", "mean", "q5", "q95")]
colnames(pars_a) <- c("variable", "mean", "lower", "upper")
pars_a$parameter <- symbol_map[pars_a$variable]
pars_a$value <- sprintf("%.3f [%.3f, %.3f]", pars_a$mean, pars_a$lower, pars_a$upper)
pars_a_rows <- sprintf(" & & %s & %s \\\\", pars_a$parameter, pars_a$value)
pars_a_rows[1] <- sprintf(
  "\\multirow{%d}{*}{%s} & \\multirow{%d}{*}{%s} & %s & %s \\\\",
  nrow(pars_a), "A", nrow(pars_a), model_names$A, pars_a$parameter[1], pars_a$value[1]
)

# Condition NC
pars_nc <- read.csv(diagnostics_files$NC)
pars_nc[, 3:13] <- round(pars_nc[, 3:13], 3)
pars_nc <- pars_nc[match(param_vars$NC, pars_nc$variable), c("variable", "mean", "q5", "q95")]
colnames(pars_nc) <- c("variable", "mean", "lower", "upper")
pars_nc$parameter <- symbol_map[pars_nc$variable]
pars_nc$value <- sprintf("%.3f [%.3f, %.3f]", pars_nc$mean, pars_nc$lower, pars_nc$upper)
pars_nc_rows <- sprintf(" & & %s & %s \\\\", pars_nc$parameter, pars_nc$value)
pars_nc_rows[1] <- sprintf(
  "\\multirow{%d}{*}{%s} & \\multirow{%d}{*}{%s} & %s & %s \\\\",
  nrow(pars_nc), "NC", nrow(pars_nc), model_names$NC, pars_nc$parameter[1], pars_nc$value[1]
)

# Condition C
pars_c <- read.csv(diagnostics_files$C)
pars_c[, 3:13] <- round(pars_c[, 3:13], 3)
pars_c <- pars_c[match(param_vars$C, pars_c$variable), c("variable", "mean", "q5", "q95")]
colnames(pars_c) <- c("variable", "mean", "lower", "upper")
pars_c$parameter <- symbol_map[pars_c$variable]
pars_c$value <- sprintf("%.3f [%.3f, %.3f]", pars_c$mean, pars_c$lower, pars_c$upper)
pars_c_rows <- sprintf(" & & %s & %s \\\\", pars_c$parameter, pars_c$value)
pars_c_rows[1] <- sprintf(
  "\\multirow{%d}{*}{%s} & \\multirow{%d}{*}{%s} & %s & %s \\\\",
  nrow(pars_c), "C", nrow(pars_c), model_names$C, pars_c$parameter[1], pars_c$value[1]
)

pars_body_lines <- c(pars_a_rows, "\\midrule", pars_nc_rows, "\\midrule", pars_c_rows)

pars_table_lines <- c(
  "\\begin{table}[h!]",
  "    \\centering",
  "    \\footnotesize",
  "    \\begin{tabular}{c c l l}",
  "        \\toprule",
  "        Condition & Model & Parameter & Mean \\& 90\\% HPDI \\\\",
  "        \\midrule",
  paste0("        ", pars_body_lines),
  "        \\bottomrule",
  "    \\end{tabular}",
  "    \\caption{Inferred RLM parameters for the winning models in the A, NC, and C conditions reported as means and 90\\% HPDIs.}",
  "    \\label{tab:rlpars}",
  "\\end{table}"
)

cat(pars_table_lines, sep = "\n")

#### Model comparison table ####

modelcomp_files <- list(
  A  = file.path("results", "rl", "alone", "modelcomp", "modelcomp.csv"),
  NC = file.path("results", "rl", "nocatches", "modelcomp", "adaptive", "modelcomp.csv"),
  C  = file.path("results", "rl", "catches", "modelcomp", "modelcomp.csv")
)

# Map raw model file names to the LaTeX model labels used in the write-up,
# in the row order the table should display
model_labels <- list(
  A = c(
    "m4.2" = "ARL_{H, \\pm[m]}",
    "m4.3" = "ARL_{H, \\pm[m, r]}",
    "m4.1" = "ARL_{H, \\pm}",
    "m3.1" = "ARL_{H, \\Bar{\\pm}}",
    "m2.1" = "ARL_{\\Bar{H}, \\pm}",
    "m1.1" = "ARL_{\\Bar{H}, \\Bar{\\pm}}"
  ),
  NC = c(
    "vsn2.hierarch" = "VS_{l[m,r]}",
    "vsn1.hierarch" = "VS_l",
    "dbn2.hierarch" = "DB_{l[m,r]}",
    "dbn1.hierarch" = "DB_l",
    "arl.hierarch"  = "ARL_{H, \\pm}"
  ),
  C = c(
    "vsndbr2.hierarch" = "VS_{l[m,r]}DB_{r[m,r]}",
    "vsn2.hierarch"    = "VS_{l[m,r]}",
    "vsnvsr2.hierarch" = "VS_{l[m,r]}VS_{r[m,r]}",
    "vsn1.hierarch"    = "VS_l",
    "vsndbr1.hierarch" = "VS_{l}DB_{r}",
    "vsnvsr1.hierarch" = "VS_{l}VS_{r}",
    "dbnvsr2.hierarch" = "DB_{l[m,r]}VS_{r[m,r]}",
    "dbnvsr1.hierarch" = "DB_{l}VS_{r}",
    "dbndbr2.hierarch" = "DB_{l[m,r]}DB_{r[m,r]}",
    "dbn2.hierarch"    = "DB_{l[m,r]}",
    "vsr2.hierarch"    = "VS_{r[m,r]}",
    "dbn1.hierarch"    = "DB_l",
    "dbndbr1.hierarch" = "DB_{l}DB_{r}",
    "vsr1.hierarch"    = "VS_r",
    "arl.hierarch"     = "ARL_{H, \\pm}",
    "dbr1.hierarch"    = "DB_r",
    "dbr2.hierarch"    = "DB_{r[m,r]}"
  )
)

# Condition A
modelcomp_a <- read.csv(modelcomp_files$A)
colnames(modelcomp_a)[1] <- "model"
modelcomp_a <- modelcomp_a[modelcomp_a$model %in% names(model_labels$A), ]
modelcomp_a_rows <- sprintf(
  "%s & %s & %.3f & %.3f \\\\",
  c("A", rep("", nrow(modelcomp_a) - 1)), paste0("$", model_labels$A[modelcomp_a$model], "$"), modelcomp_a$elpd_diff, modelcomp_a$se_diff
)

# Condition NC
modelcomp_nc <- read.csv(modelcomp_files$NC)
colnames(modelcomp_nc)[1] <- "model"
modelcomp_nc <- modelcomp_nc[modelcomp_nc$model %in% names(model_labels$NC), ]
modelcomp_nc_rows <- sprintf(
  "%s & %s & %.3f & %.3f \\\\",
  c("NC", rep("", nrow(modelcomp_nc) - 1)), paste0("$", model_labels$NC[modelcomp_nc$model], "$"), modelcomp_nc$elpd_diff, modelcomp_nc$se_diff
)

# Condition C
modelcomp_c <- read.csv(modelcomp_files$C)
colnames(modelcomp_c)[1] <- "model"
modelcomp_c <- modelcomp_c[modelcomp_c$model %in% names(model_labels$C), ]
modelcomp_c_rows <- sprintf(
  "%s & %s & %.3f & %.3f \\\\",
  c("C", rep("", nrow(modelcomp_c) - 1)), paste0("$", model_labels$C[modelcomp_c$model], "$"), modelcomp_c$elpd_diff, modelcomp_c$se_diff
)

modelcomp_body_lines <- c(modelcomp_a_rows, "\\midrule", modelcomp_nc_rows, "\\midrule", modelcomp_c_rows)

modelcomp_table_lines <- c(
  "% Results model comparison",
  "\\begin{table}[h!]",
  "    %\\caption{Results of the model comparison}",
  "\\begin{tabularx}{1\\linewidth}{ ",
  "    >{\\raggedright\\arraybackslash}X",
  "    >{\\raggedright\\arraybackslash}X ",
  "    >{\\raggedright\\arraybackslash}X  ",
  "    >{\\raggedright\\arraybackslash}X  }",
  "         \\toprule",
  "         Condition & Model & $\\hat{elpd}_{diff}$ &  $SE(\\hat{elpd}_{diff})$\\\\",
  "         \\midrule",
  paste0("         ", modelcomp_body_lines),
  "        \\bottomrule",
  "    \\end{tabularx}",
  "    \\caption{Results of the RL model comparisons for the A, NC, and C conditions. $\\hat{elpd}_{diff}$ denotes the difference in the estimated expected log pointwise predictive density $\\hat{elpd}_{PSIS-loo}$ relative to the best model and $SE(\\hat{elpd}_{diff})$ denotes its standard error.}",
  "    \\label{tab:modelcomp}",
  "\\end{table}"
)

cat(modelcomp_table_lines, sep = "\n")

#### Contrasts table ####

contrast_files <- list(
  NC = file.path("results", "rl", "nocatches", "modelcomp", "adaptive", "contrasts.csv"),
  C  = file.path("results", "rl", "catches", "modelcomp", "contrasts.csv")
)

# LaTeX symbols for the contrasted parameters
contrast_symbols <- c(
  "alphaVSD" = "$\\alpha_{VSl[m,r]}$",
  "alphaDBR" = "$\\alpha_{DBr[m,r]}$"
)

# Condition NC
contrasts_nc <- read.csv(contrast_files$NC)
contrasts_nc_symbol <- contrast_symbols[unique(contrasts_nc$parameter)]
contrasts_nc$label <- paste0(contrasts_nc$effect, ": ", contrasts_nc$contrast)
contrasts_nc$value <- sprintf("%.3f [%.3f, %.3f]", contrasts_nc$mean, contrasts_nc$lower, contrasts_nc$upper)
# Typeset minus signs in math mode and pad non-negative numbers with an invisible minus
contrasts_nc$value <- gsub("-", "$-$", contrasts_nc$value, fixed = TRUE)
contrasts_nc$value <- gsub("(^|\\[|, )([0-9])", "\\1\\\\phantom{$-$}\\2", contrasts_nc$value)

# Condition C
contrasts_c <- read.csv(contrast_files$C)
contrasts_c$label <- paste0(contrasts_c$effect, ": ", contrasts_c$contrast)
contrasts_c$value <- sprintf("%.3f [%.3f, %.3f]", contrasts_c$mean, contrasts_c$lower, contrasts_c$upper)
contrasts_c$value <- gsub("-", "$-$", contrasts_c$value, fixed = TRUE)
contrasts_c$value <- gsub("(^|\\[|, )([0-9])", "\\1\\\\phantom{$-$}\\2", contrasts_c$value)
contrasts_c_vsd <- contrasts_c[contrasts_c$parameter == "alphaVSD", ]
contrasts_c_dbr <- contrasts_c[contrasts_c$parameter == "alphaDBR", ]

# One row per contrast, one value column per weight
contrast_labels <- unique(contrasts_nc$label)
contrast_rows <- sprintf(
  "%s & %s & %s & %s \\\\",
  contrast_labels,
  contrasts_nc$value[match(contrast_labels, contrasts_nc$label)],
  contrasts_c_vsd$value[match(contrast_labels, contrasts_c_vsd$label)],
  contrasts_c_dbr$value[match(contrast_labels, contrasts_c_dbr$label)]
)

# Comment out conditional contrasts
contrast_effects <- contrasts_nc$effect[match(contrast_labels, contrasts_nc$label)]
contrast_rows[contrast_effects == "ratio | max"] <- paste0("% ", contrast_rows[contrast_effects == "ratio | max"])
contrast_rows[contrast_effects == "max | ratio"] <- paste0("% ", contrast_rows[contrast_effects == "max | ratio"])

contrast_table_lines <- c(
  "\\begin{table}[h!]",
  "    \\centering",
  "    \\footnotesize",
  "    \\resizebox{\\linewidth}{!}{%",
  "    \\begin{tabular}{l r r r}",
  "        \\toprule",
  "        & \\multicolumn{1}{c}{NC} & \\multicolumn{2}{c}{C} \\\\",
  "        \\cmidrule(lr){2-2} \\cmidrule(lr){3-4}",
  paste0("        Contrast & \\multicolumn{1}{c}{", contrasts_nc_symbol, "} & \\multicolumn{1}{c}{", contrast_symbols["alphaVSD"], "} & \\multicolumn{1}{c}{", contrast_symbols["alphaDBR"], "} \\\\"),
  "        \\midrule",
  paste0("        ", contrast_rows),
  "        \\bottomrule",
  "    \\end{tabular}}",
  "    \\caption{Contrasts of the social learning weights in the NC and C conditions reported as means and 90\\% HPDIs. Rows labeled max and ratio give differences between the maximum catch and catch ratio levels, respectively, marginal over the levels of the other factor. Rows labeled max x ratio give interaction contrasts with $(r_2 - r_1)\\times(m_2 - m_1)$ defined as the difference between catch ratio levels $r_2$ and $r_1$ at maximum catch $m_2$, minus the same difference at maximum catch $m_1$.}",
  "    \\label{tab:contrasts}",
  "\\end{table}"
)

cat(contrast_table_lines, sep = "\n")

#### Behavioral point estimates table ####

acc_file <- file.path("results", "behavioral", "acc_point_estimates.csv")

acc_estimates <- read.csv(acc_file)
acc_estimates$value <- sprintf("%.3f [%.3f, %.3f]", acc_estimates$mean, acc_estimates$lower, acc_estimates$upper)
acc_estimates$cell <- paste(acc_estimates$max, acc_estimates$ratio)

# Condition A
acc_a <- acc_estimates[acc_estimates$social == "Alone", ]

# Condition NC
acc_nc <- acc_estimates[acc_estimates$social == "No Catches", ]

# Condition C
acc_c <- acc_estimates[acc_estimates$social == "Catches", ]

# One row per environment, one value column per condition
acc_rows <- sprintf(
  " & %s & %s & %s & %s \\\\",
  acc_a$ratio,
  acc_a$value,
  acc_nc$value[match(acc_a$cell, acc_nc$cell)],
  acc_c$value[match(acc_a$cell, acc_c$cell)]
)

# Max label on first row of each max level
max_first <- !duplicated(acc_a$max)
n_ratio <- length(unique(acc_a$ratio))
acc_rows[max_first] <- paste0("\\multirow{", n_ratio, "}{*}{", acc_a$max[max_first], "}", acc_rows[max_first])

acc_table_lines <- c(
  "\\begin{table}[h!]",
  "    \\centering",
  "    \\footnotesize",
  "    \\begin{tabular}{c c l l l}",
  "        \\toprule",
  "        Max catch & Catch ratio & A & NC & C \\\\",
  "        \\midrule",
  paste0("        ", acc_rows),
  "        \\bottomrule",
  "    \\end{tabular}",
  "    \\caption{Inferred probabilities of foraging at the better lake in the A, NC, and C conditions reported as means and 90\\% HPDIs.}",
  "    \\label{tab:acc}",
  "\\end{table}"
)

cat(acc_table_lines, sep = "\n")

#### Behavioral contrasts tables ####

behavioral_contrast_file <- file.path("results", "behavioral", "contrasts.csv")

behavioral_contrasts <- read.csv(behavioral_contrast_file)
behavioral_contrasts$value <- sprintf("%.3f [%.3f, %.3f]", behavioral_contrasts$mean, behavioral_contrasts$lower, behavioral_contrasts$upper)
# Typeset minus signs in math mode and pad non-negative numbers with an invisible minus
behavioral_contrasts$value <- gsub("-", "$-$", behavioral_contrasts$value, fixed = TRUE)
behavioral_contrasts$value <- gsub("(^|\\[|, )([0-9])", "\\1\\\\phantom{$-$}\\2", behavioral_contrasts$value)

# Condition A
contrasts_alone <- behavioral_contrasts[behavioral_contrasts$social %in% "Alone", ]
contrasts_alone$label <- paste0(contrasts_alone$effect, ": ", contrasts_alone$contrast)

# One row per contrast
alone_labels <- unique(contrasts_alone$label)
alone_rows <- sprintf(
  "%s & %s \\\\",
  alone_labels,
  contrasts_alone$value[match(alone_labels, contrasts_alone$label)]
)

# Comment out conditional contrasts
alone_effects <- contrasts_alone$effect[match(alone_labels, contrasts_alone$label)]
alone_rows[alone_effects == "ratio | max"] <- paste0("% ", alone_rows[alone_effects == "ratio | max"])
alone_rows[alone_effects == "max | ratio"] <- paste0("% ", alone_rows[alone_effects == "max | ratio"])

alone_table_lines <- c(
  "\\begin{table}[h!]",
  "    \\centering",
  "    \\footnotesize",
  "    \\begin{tabular}{l r}",
  "        \\toprule",
  "        Contrast & \\multicolumn{1}{c}{Mean \\& 90\\% HPDI} \\\\",
  "        \\midrule",
  paste0("        ", alone_rows),
  "        \\bottomrule",
  "    \\end{tabular}",
  "    \\caption{Contrasts of the inferred probability of foraging at the better lake in the A condition reported as means and 90\\% HPDIs. Rows labeled max and ratio give differences between the maximum catch and catch ratio levels, respectively, marginal over the levels of the other factor. Rows labeled max x ratio give interaction contrasts with $(r_2 - r_1)\\times(m_2 - m_1)$ defined as the difference between catch ratio levels $r_2$ and $r_1$ at maximum catch $m_2$, minus the same difference at maximum catch $m_1$.}",
  "    \\label{tab:contrasts_alone}",
  "\\end{table}"
)

cat(alone_table_lines, sep = "\n")

# Social contrasts averaged over and within environments
contrasts_social <- behavioral_contrasts[behavioral_contrasts$effect %in% c("social", "social | max, ratio"), ]
contrasts_social$pair <- sub(" \\| .*$", "", contrasts_social$contrast)
contrasts_social$environment <- ifelse(contrasts_social$effect == "social", "marginal", sub("^.* \\| ", "", contrasts_social$contrast))
contrasts_social$key <- paste(contrasts_social$pair, contrasts_social$environment)

# Social pairs and environments in file order
social_pairs <- unique(contrasts_social$pair)
environments <- unique(contrasts_social$environment)

# One value column per social pair
social_values <- matrix(NA, nrow = length(environments), ncol = length(social_pairs))
for(p in seq_along(social_pairs)){
  social_values[, p] <- contrasts_social$value[match(paste(social_pairs[p], environments), contrasts_social$key)]
}

# Max and ratio of each environment
environment_max <- ifelse(environments == "marginal", "marginal", sub(",.*$", "", environments))
environment_ratio <- ifelse(environments == "marginal", "marginal", sub("^.*, ", "", environments))

# One row per environment
social_rows <- sprintf(
  " & %s & %s \\\\",
  environment_ratio,
  apply(social_values, 1, paste, collapse = " & ")
)

# Max label on first row of each max level
social_max_first <- !duplicated(environment_max)
social_n_ratio <- length(unique(environment_ratio[environment_max != "marginal"]))
social_max_labels <- ifelse(environment_max == "marginal", "marginal", paste0("\\multirow{", social_n_ratio, "}{*}{", environment_max, "}"))
social_rows[social_max_first] <- paste0(social_max_labels[social_max_first], social_rows[social_max_first])

# Rule between marginal and environment-specific contrasts
social_marginal <- environment_max == "marginal"
social_rows <- c(social_rows[social_marginal], "\\midrule", social_rows[!social_marginal])

# Abbreviated social pair headers
social_headers <- gsub("No Catches", "NC", social_pairs)
social_headers <- gsub("Catches", "C", social_headers)
social_headers <- gsub("Alone", "A", social_headers)
social_headers <- gsub("-", "$-$", social_headers, fixed = TRUE)

social_table_lines <- c(
  "\\begin{table}[h!]",
  "    \\centering",
  "    \\footnotesize",
  paste0("    \\begin{tabular}{c c ", paste(rep("r", length(social_pairs)), collapse = " "), "}"),
  "        \\toprule",
  paste0("        Max catch & Catch ratio & ", paste0("\\multicolumn{1}{c}{", social_headers, "}", collapse = " & "), " \\\\"),
  "        \\midrule",
  paste0("        ", social_rows),
  "        \\bottomrule",
  "    \\end{tabular}",
  "    \\caption{Contrasts of the inferred probability of foraging at the better lake between the A, NC, and C conditions reported as means and 90\\% HPDIs. The row labeled as marginal gives the differences between social information conditions marginal over the levels of maximum catch and catch ratio. Rows labeled with maximum catch and catch ratio give the differences between social information conditions at specific levels of maximum catch and catch ratio.}",
  "    \\label{tab:contrasts_social}",
  "\\end{table}"
)

cat(social_table_lines, sep = "\n")

#### Write tables ####

writeLines(pars_table_lines, file.path(resultsdir, "pars.tex"))
writeLines(modelcomp_table_lines, file.path(resultsdir, "modelcomp.tex"))
writeLines(contrast_table_lines, file.path(resultsdir, "contrasts.tex"))
writeLines(acc_table_lines, file.path(resultsdir, "acc.tex"))
writeLines(alone_table_lines, file.path(resultsdir, "contrasts_alone.tex"))
writeLines(social_table_lines, file.path(resultsdir, "contrasts_social.tex"))
