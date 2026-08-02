# ============================================================
# 13) Plots (basic distributions + securitisation time series)
# ============================================================

#---------------------------------------
#Bar chart different countries 2010 vs 2019
#-----------------------------------------

countries_show <- c("NL", "ES", "IT", "DE", "FR")
years_show <- c(2010, 2019)

sec_bar <- fvc_macro %>%
  filter(
    ref_area %in% countries_show,
    year %in% years_show
  ) %>%
  mutate(year = factor(year))

sec_bar <- sec_bar %>%
  mutate(
    ref_area = factor(ref_area, levels = c("NL","ES", "IT", "FR", "DE"))
  )
library(dplyr)

sec_bar <- sec_bar %>%
  mutate(ref_area = recode(ref_area,
                           "DE" = "Germany",
                           "ES" = "Spain",
                           "FR" = "France",
                           "IT" = "Italy",
                           "NL" = "Netherlands"
  ))



ggplot(sec_bar, aes(x = ref_area, y = 100 * stock_gdp, fill = year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    #title = "Securitisation Stock-to-GDP in Selected Countries",
    #subtitle = "Comparison between 2010 and 2019",
    x = "Country",
    y = "Percentage of Stock per GDP",
    fill = "Year"
  ) +
  scale_fill_manual(values = c("dodgerblue", "red2")) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = seq(0, 100, by = 10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_line(color = "black")
  )






#------------------------------------------------------------------
#Aggregate Securitization Plot
#------------------------------------------------------------------

sec_agg <- fvc_macro %>%
  group_by(year) %>%
  summarise(
    stock_total_eur = sum(stock, na.rm = TRUE),
    gdp_total_eur   = sum(gdp_meur, na.rm = TRUE),
    stock_gdp_ez    = stock_total_eur / gdp_total_eur,
    stock_gdp_ez_pct = 100 * stock_gdp_ez,
    .groups = "drop"
  )


ggplot(sec_agg, aes(year, stock_gdp_ez_pct)) +
  geom_line(linewidth = 1, color = "dodgerblue") +
  geom_point(size = 2, shape = 21, fill = "dodgerblue", color = "dodgerblue", stroke = 0.5) +
  labs(
    #title = "Aggregate Securitisation Stock (% of GDP)",
    x = "Year",
    y = "Securitisation Stock (% of GDP)"
  ) +
  scale_x_continuous(
    breaks = seq(min(sec_agg$year), max(sec_agg$year), by = 1),
    labels = function(x) ifelse(x %% 2 == 0, x, "")
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black")
  )







summary(fvc_macro$stock_gdp_pct)

#------------------------------------------------------------
# Aggregate Total Loans
#------------------------------------------------------------------

lend_agg <- d %>%
  group_by(year) %>%
  summarise(
    total_loans_ez = sum(total_loans, na.rm = TRUE),
    log_total_loans_ez = log(total_loans_ez),
    .groups = "drop"
  )


ggplot(lend_agg, aes(x = year, y = log_total_loans_ez)) +
  geom_line(linewidth = 1, color = "dodgerblue") +
  geom_point(size = 2, shape = 21, fill = "dodgerblue", color = "dodgerblue", stroke = 0.5) +
  labs(
    title = "Aggregate Bank Lending in the Eurozone",
    x = "Year",
    y = "Log Total Loans"
  )  +
  scale_x_continuous(
    breaks = seq(min(lend_agg$year), max(lend_agg$year), by = 1),
    labels = function(x) ifelse(x %% 2 == 0, x, "")
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "")
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black")
  )


# -----------------------------
# Prepare country-year total lending (selected countries)
# -----------------------------
countries_show <- c("ES", "NL", "IT", "FR", "DE")

lend_ct <- d %>%
  filter(ref_area %in% countries_show) %>%
  group_by(ref_area, year) %>%
  summarise(
    total_loans_ct = sum(total_loans, na.rm = TRUE),
    .groups = "drop"
  )

# Base year for index
base_year <- 2010

# Create index = 100 in base_year
lend_ct <- lend_ct %>%
  group_by(ref_area) %>%
  mutate(
    base_loans = total_loans_ct[year == base_year][1],
    lend_index = 100 * (total_loans_ct / base_loans)
  ) %>%
  ungroup()

# -----------------------------
# Base R plot: Lending index over time
# -----------------------------
cols <- c("ES"="steelblue", "NL"="darkorange", "IT"="darkgreen", "FR"="purple", "DE"="brown")

# X and Y limits
xlim <- range(lend_ct$year, na.rm = TRUE)
ylim <- range(lend_ct$lend_index, na.rm = TRUE)

png("Fig_LendingIndex_SelectedCountries.png", width = 900, height = 600, res = 120)

plot(NA, xlim = xlim, ylim = ylim,
     xlab = "Year",
     ylab = paste0("Lending index (", base_year, " = 100)"),
     main = "Aggregate Bank Lending Index in Selected Countries")

# Add lines country by country
for(ct in countries_show){
  tmp <- lend_ct[lend_ct$ref_area == ct, ]
  tmp <- tmp[order(tmp$year), ]
  lines(tmp$year, tmp$lend_index, col = cols[ct], lwd = 2)
}

legend("topleft", legend = countries_show, col = cols[countries_show], lwd = 2, bty = "n")
grid(col = "grey80", lty = "dotted")

dev.off()




# -----------------------------
# Compute annual growth: dlog(total loans) at country-year level
# -----------------------------
lend_growth <- lend_ct %>%
  mutate(log_loans = log(total_loans_ct)) %>%
  arrange(ref_area, year) %>%
  group_by(ref_area) %>%
  mutate(dlog_loans = log_loans - lag(log_loans)) %>%
  ungroup()

# Average annual growth per country (in %)
growth_summary <- lend_growth %>%
  group_by(ref_area) %>%
  summarise(
    avg_growth_pct = 100 * mean(dlog_loans, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(ref_area = factor(ref_area, levels = countries_show)) %>%
  arrange(ref_area)

# -----------------------------
# Base R bar plot: average annual lending growth
# -----------------------------
png("Fig_AvgAnnualLendingGrowth_SelectedCountries.png", width = 900, height = 600, res = 120)

barplot(
  height = growth_summary$avg_growth_pct,
  names.arg = growth_summary$ref_area,
  col = cols[as.character(growth_summary$ref_area)],
  border = NA,
  main = "Average Annual Lending Growth (Selected Countries)",
  ylab = "Average annual growth in total loans (%)",
  xlab = "Country"
)

abline(h = 0, col = "grey40")
grid(nx = NA, ny = NULL, col = "grey80", lty = "dotted")

dev.off()
