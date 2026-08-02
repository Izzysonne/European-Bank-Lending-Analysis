# Load helper functions
source("R/01_packages_helpers.R")

# Load prepared dataset
d <- readr::read_csv("bank_panel_regression_ready_2009_2022_clean_winsor.csv")

# ============================================================
# 9) Robustness
# ============================================================

# A) Loan coverage >= 2
d_cov2 <- d %>% filter(loans_components_present >= 2)
m0_cov2 <- feols(
  log_total_loans ~ log_stock_gdp_l1
  | bvd_id_number + year,
  data = d_cov2,
  cluster = ~ country
)

summary(bf_raw$other_exposure_to_securitizations)

m1_cov2 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
    log_assets_l1
  | bvd_id_number + year,
  data = d_cov2,
  cluster = ~ country
)


m2_cov2 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
    log_assets_l1 +
    tier1_rwa_l1_w +
    deposit_assets_l1_w
  | bvd_id_number + year,
  data = d_cov2,
  cluster = ~ country
)

m3_cov2 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
    log_assets_l1 +
    tier1_rwa_l1_w +
    deposit_assets_l1_w +
    npl_raw_l1_w +
    roa_l1_w
  | bvd_id_number + year,
  data = d_cov2,
  cluster = ~ country
)


m4_cov2 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
    log_assets_l1 +
    tier1_rwa_l1_w +
    deposit_assets_l1_w +
    npl_raw_l1_w +
    roa_l1_w +
    loans_components_present
  | bvd_id_number + year,
  data = d_cov2,
  cluster = ~ country
)


etable(m0_cov2, m1_cov2, m2_cov2, m3_cov2, m4_cov2)


# B) Corporate lending outcome 
m_corp <- feols(
  log_corporate_loans ~ log_stock_gdp_l1 +
    log_assets_l1 + equity_assets_l1_w + deposit_assets_l1_w +
    npl_raw_l1_w + roa_l1_w + loans_components_present 
  | bvd_id_number + year,
  data = d, cluster = ~ country
)

m_noncorp <- feols(
  log_total_loans_noncorp ~ log_stock_gdp_l1 +
    log_assets_l1 +
    equity_assets_l1_w +
    deposit_assets_l1_w +
    npl_raw_l1_w +
    roa_l1_w +
    loans_components_present
  | bvd_id_number + year,
  data = d,
  cluster = ~ country
)



etable(m_corp, m_noncorp)


# ---------
# 2) Weak capital: tercile split (robustness)
#    Compare bottom tercile (weakest) vs others
# ---------
d <- d %>%
  group_by(year) %>%
  mutate(
    cap_tercile = ntile(equity_assets_l1_w, 3),
    weak_capital_t3 = as.integer(cap_tercile == 1)  # weakest tercile
  ) %>%
  ungroup()

m_weakcap_t3 <- feols(
  log_total_loans ~ log_stock_gdp_l1 * weak_capital_t3 +
    log_assets_l1 + equity_assets_l1_w + deposit_assets_l1_w +
    npl_raw_l1_w + roa_l1_w + loans_components_present
  | bvd_id_number + year,
  data = d,
  cluster = ~ ref_area
)

# ---------
# 4) High NPL: tercile split (robustness)
#    Compare top tercile (riskiest) vs others
# ---------
d <- d %>%
  group_by(year) %>%
  mutate(
    npl_tercile = ntile(npl_raw_l1_w, 3),
    high_npl_t3 = as.integer(npl_tercile == 3)  # highest tercile
  ) %>%
  ungroup()

m_highnpl_t3 <- feols(
  log_total_loans ~ log_stock_gdp_l1 * high_npl_t3 +
    log_assets_l1 + equity_assets_l1_w + deposit_assets_l1_w +
    npl_raw_l1_w + roa_l1_w + loans_components_present
  | bvd_id_number + year,
  data = d,
  cluster = ~ ref_area
)

etable(m_weakcap_t3, m_highnpl_t3)
