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

# Read a diagnostics csv and round its numeric summary columns
read_diagnostics <- function(path) {
  d <- read.csv(path)
  d[, 3:13] <- apply(d[, 3:13], 2, function(x) round(x, 3))
  d
}

# Pull mean/lower/upper (q5/q95) for a set of variables, in the given order
extract_pars <- function(d, vars) {
  out <- d[match(vars, d$variable), c("variable", "mean", "q5", "q95")]
  colnames(out) <- c("variable", "mean", "lower", "upper")
  out
}

# Format as "mean [lower, upper]" with fixed 3 decimal places
format_value <- function(pars_table) {
  sprintf("%.3f [%.3f, %.3f]", pars_table$mean, pars_table$lower, pars_table$upper)
}

# Build the multirow LaTeX rows for one condition/model block
build_latex_block <- function(pars_table, condition, model_name) {
  n <- nrow(pars_table)
  rows <- character(n)
  rows[1] <- sprintf(
    "\\multirow{%d}{*}{%s} & \\multirow{%d}{*}{%s} & %s & %s \\\\",
    n, condition, n, model_name, pars_table$parameter[1], pars_table$value[1]
  )
  if (n > 1) {
    rows[-1] <- sprintf(" & & %s & %s \\\\", pars_table$parameter[-1], pars_table$value[-1])
  }
  rows
}

# Extract parameters for one condition and build its LaTeX block in one go
build_condition_block <- function(condition) {
  d <- read_diagnostics(diagnostics_files[[condition]])
  pars_table <- extract_pars(d, param_vars[[condition]])
  pars_table$parameter <- symbol_map[pars_table$variable]
  pars_table$value <- format_value(pars_table)
  build_latex_block(pars_table, condition, model_names[[condition]])
}

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

pars_a_rows  <- build_condition_block("A")
pars_nc_rows <- build_condition_block("NC")
pars_c_rows  <- build_condition_block("C")

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

# Format elpd_diff / se_diff to a fixed 3 decimal places
format_num <- function(x) {
  sprintf("%.3f", x)
}

# Extract and format the model comparison rows for one condition
build_modelcomp_block <- function(condition) {
  d <- read.csv(modelcomp_files[[condition]])
  colnames(d)[1] <- "model"

  labels <- model_labels[[condition]]
  d <- d[match(names(labels), d$model), ]

  condition_col <- c(condition, rep("", nrow(d) - 1))
  sprintf(
    "%s & %s & %s & %s \\\\",
    condition_col, labels[d$model], format_num(d$elpd_diff), format_num(d$se_diff)
  )
}

modelcomp_a_rows  <- build_modelcomp_block("A")
modelcomp_nc_rows <- build_modelcomp_block("NC")
modelcomp_c_rows  <- build_modelcomp_block("C")

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
  "    \\caption{Results of the RL model comparisons for the A, NC, and C conditions. $\\hat{elpd}_{diff}$ denotes the difference in the estimated expected log pointwise predictive density $\\hat{elpd}_{PSIS-loo}$ relative to the best model and $SE(\\hat{elpd}_{diff})$ denotes its standard error. We explored different ARL models not discussed in the main text. We first crossed the absence (``$\\Bar{H}$\") / presence (``$H$\") of the persistence parameter $\\beta_H$ with the absence (``$\\Bar{\\pm}$\") / presence (``$\\pm$\") of the asymmetric personal learning weights $\\alpha_{P-}$ and $\\alpha_{P+}$. Then we explored models that allowed these asymmetric personal learning weights to vary depending on either the maximum catch (``$\\pm[m]$\"), or the maximum catch as well as the catch ratio (``$\\pm[m,r]$\"). To keep our further analyses tractable, we settled on model $ARL_{H, \\pm}$ which performed substantially better than the less complex models but only marginally worse than the more complex ones. }",
  "    \\label{tab:modelcomp}",
  "\\end{table}"
)

cat(modelcomp_table_lines, sep = "\n")

#### Write tables ####

writeLines(pars_table_lines, file.path(resultsdir, "pars.tex"))
writeLines(modelcomp_table_lines, file.path(resultsdir, "modelcomp.tex"))
