
# ============================================================
# 8) Main regressions (bank FE + year FE, cluster by country)
# ============================================================
m0 <- feols(
  log_total_loans ~ log_stock_gdp_l1
  | bvd_id_number + year,
  data = d,
  cluster = ~ country
)


m1 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
    log_assets_l1
  | bvd_id_number + year,
  data = d,
  cluster = ~ country
)


m2 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
    log_assets_l1 +
    equity_assets_l1_w +
    deposit_assets_l1_w
  | bvd_id_number + year,
  data = d,
  cluster = ~ country
)

m3 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
    log_assets_l1 +
    equity_assets_l1_w +
    deposit_assets_l1_w +
    npl_raw_l1_w +
    roa_l1_w
  | bvd_id_number + year,
  data = d,
  cluster = ~ country
)


m4 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
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



etable(
  m0, m1, m2, m3, m4
)
#===============================================================
# ============================================================
# 8) BASELINE regressions TIER 1 (bank FE + year FE, cluster by country)
# ============================================================
t0 <- feols(
  log_total_loans ~ log_stock_gdp_l1
  | bvd_id_number + year,
  data = d,
  cluster = ~ country
)


t1 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
    log_assets_l1
  | bvd_id_number + year,
  data = d,
  cluster = ~ country
)


t2 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
    log_assets_l1 +
    tier1_rwa_l1_w +
    deposit_assets_l1_w
  | bvd_id_number + year,
  data = d,
  cluster = ~ country
)

t3 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
    log_assets_l1 +
    tier1_rwa_l1_w +
    deposit_assets_l1_w +
    npl_raw_l1_w +
    roa_l1_w
  | bvd_id_number + year,
  data = d,
  cluster = ~ country
)


t4 <- feols(
  log_total_loans ~ log_stock_gdp_l1 +
    log_assets_l1 +
    tier1_rwa_l1_w +
    deposit_assets_l1_w +
    npl_raw_l1_w +
    roa_l1_w +
    loans_components_present
  | bvd_id_number + year,
  data = d,
  cluster = ~ country
)



etable(
  t0, t1, t2, t3, t4
)
