
# ============================================================
# 10) Heterogeneity (clean default: no macro controls)
# ============================================================

# 10A) Capital strength dummy (within-year median split)
d <- d %>%
  group_by(year) %>%
  mutate(Weak_capital = as.integer(equity_assets_l1_w < median(equity_assets_l1_w, na.rm = TRUE))) %>%
  ungroup()

m_weak <- feols(
  log_total_loans ~ log_stock_gdp_l1 * Weak_capital +
    log_assets_l1 + equity_assets_l1_w + deposit_assets_l1_w +
    npl_raw_l1_w + roa_l1_w + loans_components_present
  | bvd_id_number + year,
  data = d, cluster = ~ country
)

# 10B) Risk dummy (within-year median split)
d <- d %>%
  group_by(year) %>%
  mutate(High_risk = as.integer(npl_raw_l1_w >= median(npl_raw_l1_w, na.rm = TRUE))) %>%
  ungroup()


d <- d %>%
  group_by(year) %>%
  mutate(
    npl_tercile = ntile(npl_raw_l1_w, 3),
    high_npl = as.integer(npl_tercile == 3)
  ) %>%
  ungroup()

m_risky <- feols(
  log_total_loans ~ log_stock_gdp_l1 * High_risk +
    log_assets_l1 + equity_assets_l1_w + deposit_assets_l1_w +
    npl_raw_l1_w + roa_l1_w + loans_components_present
  | bvd_id_number + year,
  data = d, cluster = ~ country
)

etable(m_weak, m_risky)
