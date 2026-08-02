# Load helper functions
source("R/01_packages_helpers.R")

# Load prepared dataset
d <- readr::read_csv("bank_panel_regression_ready_2009_2022_clean_winsor.csv")

# ============================================================
# 12) Export tables
# ============================================================
library(modelsummary)

modelsummary(
  list(
    m0, m1, m2, m3, m4),
  statistic = "p.value",
  output = "Table_Baseline_new.docx",
  stars = TRUE,
)

modelsummary(
  list(
    t0, t1, t2, t3, t4),
  statistic = "p.value",
  output = "Tier1 baseline.docx",
  stars = TRUE,
)


modelsummary(
  list(
    m0_cov2, m1_cov2, m2_cov2, m3_cov2, m4_cov2
  ),
  statistic = "p.value",
  output = "Table_Robust_coverage.docx",
  stars = TRUE
)

modelsummary(
  list(
    m_corp, m_noncorp
  ),
  statistic = "p.value",
  output = "Table_Robust_loan types.docx",
  stars = TRUE
)


modelsummary(
  list(
    m_weakcap_t3, m_highnpl_t3
  ),
  statistic = "p.value",
  output = "Table_Robust_hetero_tercile.docx",
  stars = TRUE
)


modelsummary(
  list(
    m_weak, m_risky
  ),
  statistic = "p.value",
  output = "Table_Het.docx",
  stars = TRUE
)

modelsummary(
  list(
    m_ct, m_ct_het
  ),
  statistic = "p.value",
  output = "Table_Country_Year.docx",
  stars = TRUE
)


# ============================================================
# 14) Summary stats
# ============================================================

library(modelsummary)

sumstats_data <- d %>%
  select(
    log_total_loans,
    stock_gdp,
    log_corporate_loans,
    log_total_loans_noncorp,
    log_stock_gdp_l1,
    gdp_growth_l1,
    unemp_rate_l1,
    log_assets_l1,
    equity_assets_l1_w,
    tier1_rwa_l1_w,
    deposit_assets_l1_w,
    npl_raw_l1_w,
    roa_l1_w,
    loans_components_present
  )

sumstats_data_country <- country_year %>%
  select(
    log_total_loans_ct,
    log_stock_gdp_l1,
    gdp_growth_l1,
    unemp_rate_l1
  )

datasummary(
  All(sumstats_data_country) ~ Mean + Median + P25 + P75 + SD + N,
  data = sumstats_data_country,
  fmt = 3,
  title = "Summary Statistics",
  output = "Table1_SummaryStatistics_Country.docx",
  coef_map = c(
    log_total_loans_ct = "Log total loans",
    log_stock_gdp_l1 = "Log securitisation stock / GDP",
    stock_gdp = 'Securitization stock /GDP',
    gdp_growth_l1 = "GDP growth",
    unemp_rate_l1 = "Unemployment rate"
  )
)

datasummary(
  All(sumstats_data) ~ Mean + Median + P25 + P75 + SD + N,
  data = sumstats_data,
  fmt = 3,
  title = "Summary Statistics",
  output = "Table1_SummaryStatistics.docx",
  coef_map = c(
    log_total_loans = "Log total loans",
    log_corporate_loans = 'Log corporate loans',
    log_total_loans_noncorp = 'log non corporate loans',
    loans_components_present = "Loan components present",
    log_assets_l1 = "Log total assets",
    equity_assets_l1_w = "Equity / assets",
    tier1_rwa_l1_w = "Tier 1 / RWA",
    deposit_assets_l1_w = "Deposits / assets",
    npl_raw_l1_w = "NPL ratio",
    roa_l1_w = "Return on assets",
    log_stock_gdp_l1 = "Log securitisation stock / GDP",
    stock_gdp = 'Securitization stock /GDP',
    gdp_growth_l1 = "GDP growth",
    unemp_rate_l1 = "Unemployment rate"

  )
)
