
# ============================================================
# 11) Country–year validation analysis
# ============================================================

country_year <- d %>%
  group_by(ref_area, year) %>%
  summarise(
    total_loans_ct     = sum(total_loans, na.rm = TRUE),
    corporate_loans_ct = sum(corporate_loans, na.rm = TRUE),
    mortgage_loans_ct  = sum(mortgage_loans, na.rm = TRUE),
    n_banks            = n_distinct(bvd_id_number),
    .groups = "drop"
  ) %>%
  mutate(
    log_total_loans_ct = safe_log(total_loans_ct),
    log_corp_loans_ct  = safe_log(corporate_loans_ct),
    log_mort_loans_ct  = safe_log(mortgage_loans_ct)
  ) %>%
  left_join(
    fvc_macro %>% select(ref_area, year, stock_gdp, log_stock_gdp, gdp_growth, unemp_rate),
    by = c("ref_area","year")
  ) %>%
  arrange(ref_area, year) %>%
  group_by(ref_area) %>%
  mutate(
    log_stock_gdp_l1 = lag(log_stock_gdp, 1),
    gdp_growth_l1    = lag(gdp_growth, 1),
    unemp_rate_l1    = lag(unemp_rate, 1)
  ) %>%
  ungroup()



# Country FE + year FE, clustered by country
m_ct <- feols(
  log_total_loans_ct ~ log_stock_gdp_l1 + gdp_growth_l1 + unemp_rate_l1 | ref_area + year,
  data = country_year,
  cluster = ~ ref_area
)

etable(m_ct)

# ============================================================
# Financial development heterogeneity (COUNTRY LEVEL)
# ============================================================

# Average securitisation depth by country (time-invariant)
country_sec_depth <- country_year %>%
  group_by(ref_area) %>%
  summarise(
    avg_sec_depth = mean(stock_gdp, na.rm = TRUE),
    .groups = "drop"
  )

# Merge and define high securitisation countries
country_year <- country_year %>%
  left_join(country_sec_depth, by = "ref_area") %>%
  mutate(
    high_sec_country = as.integer(avg_sec_depth > median(avg_sec_depth, na.rm = TRUE))
  )

# Country-level interaction model
m_ct_het <- feols(
  log_total_loans_ct ~ log_stock_gdp_l1 * high_sec_country + high_sec_country + gdp_growth_l1 + unemp_rate_l1 | ref_area + year,
  data = country_year,
  cluster = ~ ref_area
)

etable(m_ct, m_ct_het)
