
# ============================================================
# 0) Eurozone mapping: BankFocus country name -> ISO2 (ref_area)
# ============================================================
country_map <- c(
  "AT"="Austria","BE"="Belgium","CY"="Cyprus","DE"="Germany",
  "EE"="Estonia","ES"="Spain","FI"="Finland","FR"="France",
  "GR"="Greece","IE"="Ireland","IT"="Italy","LT"="Lithuania",
  "LU"="Luxembourg","LV"="Latvia","MT"="Malta","NL"="Netherlands",
  "PT"="Portugal","SI"="Slovenia","SK"="Slovakia"
)
country_to_iso <- setNames(names(country_map), unname(country_map))
ez_iso <- names(country_map)

# ============================================================
# 1) BankFocus: load + clean + convert percent to fractions
# ============================================================
bf_raw <- readr::read_csv("bankfocus_master_thesis.csv", show_col_types = FALSE) %>%
  clean_names()

names(bf_raw)

bf <- bf_raw %>%
  mutate(
    fiscal_year  = as.integer(fiscal_year),
    year         = fiscal_year,
    closing_date = as.Date(as.character(closing_date), format = "%Y%m%d"),
    across(
      c(total_assets, total_equity, costumer_deposits, bank_deposits,
        other_wholesale_deposits, senior_debt, subordinated,
        mortgage_loans, consumer_loans, corporate_loans, other_loans,
        operation_revenue, operation_profit, earnings_before_taxes, net_income,
        rw_as, tier_1, managed_securitized_assets, other_exposure_to_securitizations,
        n_employees, n_branches, npl_ratio, roe, roa),
      as.numeric
    )
  ) %>%
  # Convert percent-style ratios to fractions (per your data: ROA ~ 0.32, ROE ~ 9.57, NPL ~ 2.8)
  mutate(
    npl_ratio = npl_ratio / 100,
    roe       = roe / 100,
    roa       = roa / 100
  )

# ============================================================
# 2) Deduplicate bank-year: C1 > C2 > C* > others; then completeness; then latest date
# ============================================================
bf <- bf %>%
  mutate(
    consol_rank = case_when(
      consolidation_code == "C1" ~ 3L,
      consolidation_code == "C2" ~ 2L,
      consolidation_code == "C*" ~ 1L,
      TRUE ~ 0L
    )
  )

score_vars <- c(
  "total_assets","total_equity",
  "costumer_deposits","bank_deposits","other_wholesale_deposits",
  "senior_debt","subordinated",
  "mortgage_loans","consumer_loans","corporate_loans","other_loans",
  "operation_revenue","operation_profit","earnings_before_taxes","net_income",
  "rw_as","tier_1", 'managed_securitized_assets', 'other_exposure_to_securitizations',
  "npl_ratio","roa","roe",
  "n_employees","n_branches"
)

bf <- bf %>%
  mutate(completeness_score = rowSums(!is.na(across(any_of(score_vars)))))

panel <- bf %>%
  arrange(bvd_id_number, year,
          desc(consol_rank), desc(completeness_score), desc(closing_date)) %>%
  group_by(bvd_id_number, year) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    used_consolidation = consolidation_code,
    used_c1 = used_consolidation == "C1"
  )

# ============================================================
# 3) Construct loans + logs
# ============================================================
panel <- panel %>%
  mutate(
    ref_area = unname(country_to_iso[country]),
    loans_components_present = rowSums(!is.na(across(c(
      mortgage_loans, consumer_loans, corporate_loans, other_loans
    )))),
    total_loans = rowSums(across(
      c(mortgage_loans, consumer_loans, corporate_loans, other_loans),
      ~ replace_na(., 0)
    )),
    total_loans = if_else(loans_components_present == 0, NA_real_, total_loans),
    log_total_loans = safe_log(total_loans),
    log_assets = safe_log(total_assets),
    
    total_loans_noncorp = if_else(
      !is.na(total_loans) & !is.na(corporate_loans),
      total_loans - corporate_loans,
      NA_real_
    ),
    
    log_total_loans_noncorp = if_else(
      !is.na(total_loans_noncorp) & total_loans_noncorp > 0,
      log(total_loans_noncorp),
      NA_real_
    ),
        
      
    log_corporate_loans = safe_log(corporate_loans),
    log_mortgage_loans  = safe_log(mortgage_loans),
    log_consumer_loans  = safe_log(consumer_loans)
  )

panel_ez <- panel %>%
  filter(ref_area %in% ez_iso, year >= 2009, year <= 2022)

d0 <- panel_ez %>%
  filter(!is.na(total_assets), total_assets >= 10e6) %>%
  group_by(bvd_id_number) %>%
  filter(sum(!is.na(log_total_loans)) >= 3) %>%
  ungroup()

# ============================================================
# 4) ECB FVC securitisation data (your CSV)
# ============================================================
fvc <- readr::read_csv("fvc_raw.csv", show_col_types = FALSE) %>%
  clean_names()



fvc_l40 <- fvc %>%
  filter(fvc_item == "L40", unit == "EUR") %>%
  mutate(year = as.integer(substr(time_period, 1, 4))) %>%
  filter(year >= 2009, year <= 2022)

issuance <- fvc_l40 %>%
  filter(data_type == "4") %>%
  group_by(ref_area, year) %>%
  summarise(issuance = sum(obs_value, na.rm = TRUE), .groups = "drop")

stock <- fvc_l40 %>%
  filter(data_type == "1") %>%
  mutate(q = as.integer(substr(time_period, 7, 7))) %>%   # "YYYY-Qx"
  group_by(ref_area, year) %>%
  slice_max(order_by = q, n = 1, with_ties = FALSE) %>%   # take last quarter
  ungroup() %>%
  transmute(ref_area, year, stock = obs_value)


#stock <- fvc_l40 %>%
#  filter(data_type == "1") %>%
#  group_by(ref_area, year) %>%
#  summarise(stock = sum(obs_value, na.rm = TRUE), .groups = "drop")

fvc_combined <- full_join(issuance, stock, by = c("ref_area","year")) %>%
  filter(ref_area != "U2")



# ============================================================
# 5) Eurostat macro controls (GDP level, GDP growth, unemployment)
# ============================================================
gdp_raw <- get_eurostat("nama_10_gdp", time_format = "num")
unique(gdp_raw$unit)

gdp_level <- gdp_raw %>%
  filter(na_item == "B1GQ", unit == "CP_MEUR", TIME_PERIOD >= 2009, TIME_PERIOD <= 2022) %>%
  transmute(year = as.integer(TIME_PERIOD), ref_area = geo,
            gdp_meur = values, gdp_eur = gdp_meur * 1e6)

gdp_real <- gdp_raw %>%
  filter(na_item == "B1GQ", unit == "CLV10_MEUR", TIME_PERIOD >= 2008, TIME_PERIOD <= 2022) %>%
  transmute(year = as.integer(TIME_PERIOD), ref_area = geo, gdp_real_meur = values) %>%
  arrange(ref_area, year) %>%
  group_by(ref_area) %>%
  mutate(gdp_growth = 100 * (gdp_real_meur / lag(gdp_real_meur) - 1)) %>%
  ungroup() %>%
  filter(year >= 2009)

unemp_raw <- get_eurostat("une_rt_a", time_format = "num")

unemp <- unemp_raw %>%
  filter(sex == "T", age == "Y15-74", unit == "PC_ACT",
         TIME_PERIOD >= 2009, TIME_PERIOD <= 2022) %>%
  transmute(year = as.integer(TIME_PERIOD), ref_area = geo, unemp_rate = values)

# ============================================================
# 6) Build securitisation measures (stock/GDP)
# ============================================================
fvc_macro <- fvc_combined %>%
  left_join(
    gdp_level %>% select(ref_area, year, gdp_meur),
    by = c("ref_area","year")
  ) %>%
  mutate(
    stock_gdp = if_else(
      !is.na(stock) & !is.na(gdp_meur) & gdp_meur > 0,
      stock / gdp_meur,
      NA_real_
    ),
    stock_gdp_pct = 100 * stock_gdp,
    log_stock_gdp = safe_log(stock_gdp)
  ) %>%
  left_join(gdp_real, by = c("ref_area","year")) %>%
  left_join(unemp, by = c("ref_area","year"))

# ============================================================
# 7) Merge into bank panel, construct ratios, lags, winsorise
# ============================================================
d <- d0 %>%
  left_join(
    fvc_macro %>% select(ref_area, year, stock, issuance, stock_gdp, log_stock_gdp,
                         stock_gdp_pct, gdp_growth, unemp_rate),
    by = c("ref_area","year")
  ) %>%
  arrange(bvd_id_number, year) %>%
  group_by(bvd_id_number) %>%
  mutate(
    # ratios
    equity_assets  = total_equity / total_assets,
    tier1_rwa      = tier_1 / rw_as,
    deposit_assets = costumer_deposits / total_assets,
    
    # lags (main regressor + controls)
    log_stock_gdp_l1 = lag(log_stock_gdp, 1),
    log_assets_l1    = lag(log_assets, 1),
    equity_assets_l1 = lag(equity_assets, 1),
    tier1_rwa_l1     = lag(tier1_rwa, 1),
    deposit_assets_l1= lag(deposit_assets, 1),
    npl_raw_l1       = lag(npl_ratio, 1),
    roa_l1           = lag(roa, 1),
    gdp_growth_l1    = lag(gdp_growth, 1),
    unemp_rate_l1    = lag(unemp_rate, 1),
    
    # loan growth robustness
    dlog_total_loans = log_total_loans - lag(log_total_loans, 1)
  ) %>%
  ungroup()

# Winsorise key lagged ratios (and growth outcome)
wins_p <- 0.01
d <- d %>%
  mutate(
    equity_assets_l1_w  = winsor_vec(equity_assets_l1, p = wins_p),
    tier1_rwa_l1_w      = winsor_vec(tier1_rwa_l1, p = wins_p),
    deposit_assets_l1_w = winsor_vec(deposit_assets_l1, p = wins_p),
    npl_raw_l1_w        = winsor_vec(npl_raw_l1, p = wins_p),
    roa_l1_w            = winsor_vec(roa_l1, p = wins_p),
    dlog_total_loans_w  = winsor_vec(dlog_total_loans, p = wins_p),
    stock_gdp_w         = winsor_vec(stock_gdp, p = wins_p)
  )

readr::write_csv(d, "bank_panel_regression_ready_2009_2022_clean_winsor.csv")
