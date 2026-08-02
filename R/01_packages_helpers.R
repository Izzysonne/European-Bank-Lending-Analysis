# ============================================================
# European Bank Lending Analysis
# Master's Thesis
# Author: Issabella Sonne
# ============================================================

# -----------------------------
# Packages
# -----------------------------
library(tidyverse)
library(janitor)
library(lubridate)
library(eurostat)
library(fixest)
library(modelsummary)
library(ggplot2)

# Set your local working directory
# (Change this to your own folder when running locally)

setwd("/Volumes/External/Thesis/WORKING DIRECTORY")

# -----------------------------
# Helper Functions
# -----------------------------

# Winsorize variables to reduce the effect of outliers
winsor_vec <- function(x, p = 0.01) {
  if (all(is.na(x))) return(x)

  lo <- quantile(
    x,
    probs = p,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )

  hi <- quantile(
    x,
    probs = 1 - p,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )

  pmin(pmax(x, lo), hi)
}

# Safe logarithm transformation
safe_log <- function(x) {
  ifelse(!is.na(x) & x > 0, log(x), NA_real_)
}

# Plot marginal effects from interaction models
plot_marginal_effects_dummy <- function(
  model,
  dummy_name,
  title = "Marginal effect of securitisation",
  x_labels = c("Weak banks", "Strong banks"),
  file = NULL
) {

  b <- coef(model)
  V <- vcov(model)

  main <- "log_stock_gdp_l1"

  inter1 <- paste0(main, ":", dummy_name)
  inter2 <- paste0(dummy_name, ":", main)

  inter <- if (inter1 %in% names(b)) inter1 else inter2

  eff0 <- b[main]
  se0 <- sqrt(V[main, main])

  eff1 <- b[main] + b[inter]
  se1 <- sqrt(
    V[main, main] +
      V[inter, inter] +
      2 * V[main, inter]
  )

  df <- tibble(
    group = factor(
      c(0, 1),
      levels = c(0, 1),
      labels = x_labels
    ),
    effect = c(eff0, eff1),
    se = c(se0, se1)
  ) %>%
    mutate(
      lo = effect - 1.96 * se,
      hi = effect + 1.96 * se
    )

  p <- ggplot(df, aes(group, effect)) +
    geom_point(size = 3) +
    geom_errorbar(
      aes(ymin = lo, ymax = hi),
      width = 0.15
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    labs(
      title = title,
      x = NULL,
      y = "Marginal effect of log(Sec stock/GDP) on log(loans)"
    ) +
    theme_minimal()

  if (!is.null(file))
    ggsave(file, p, width = 7, height = 5)

  p
}
