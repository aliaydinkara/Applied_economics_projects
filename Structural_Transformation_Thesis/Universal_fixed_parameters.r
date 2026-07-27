# ============================================================
# VERSION 2: TWO-TIER STATIC MODEL
# LOG TOP NEST + 3-GOOD CES LOWER NEST
# Agriculture vs (Manufacturing + CS + OS)
# ============================================================

rm(list = ls())
gc()

library(tidyverse)
library(janitor)

setwd("/Users/aliaydinkara/Desktop/Documents/BSE/Master\'s\ thesis/Data\ and\ Code/4-sector")

# ============================================================
# 1. LOAD DATA
# ============================================================

raw_panel <- read_csv("ETD_prod_emp_pwt_EXTENDED_HPRAW.csv") %>%
  clean_names()


a_bar_lookup <- read_csv("a_bar_scenarios_fullETD.csv") %>%
  clean_names() %>%
  select(iso3, a_bar_full)

raw_panel <- raw_panel %>%
  left_join(a_bar_lookup, by = "iso3")

# ============================================================
# ADD MACRO VARIABLES FROM MASTER PANEL
# ============================================================

master_panel <- read_csv("Master_panel_wide.csv") %>%
  clean_names() %>%
  select(
    iso3,
    year,
    rgdpna,
    hc,
    ctfp,
    pop_age_0_14,
    pop_age_15_64,
    pop_age_65plus
  ) %>%
  mutate(
    population =
      pop_age_0_14 +
      pop_age_15_64 +
      pop_age_65plus,
    
    gdppc =
      rgdpna / population
  )

# ============================================================
# 2. BUILD FOUR-SECTOR PANEL
# CS = TRADE SERVICES ONLY
# OS = ALL OTHER NON-AG, NON-MANUFACTURING SECTORS
# ============================================================

model_panel_raw <- raw_panel %>%
  mutate(
    
    # observed labour shares
    L_a_obs  = emp_share_agriculture,
    L_m_obs  = emp_share_manufacturing,
    L_cs_obs = emp_share_trade_services,
    
    # residual OS aggregation
    va_q15_os =
      va_q15_mining +
      va_q15_utilities +
      va_q15_construction +
      va_q15_transport_services +
      va_q15_business_services +
      va_q15_financial_services +
      va_q15_government_services +
      va_q15_other_services,
    
    emp_os =
      emp_thousands_mining +
      emp_thousands_utilities +
      emp_thousands_construction +
      emp_thousands_transport_services +
      emp_thousands_business_services +
      emp_thousands_financial_services +
      emp_thousands_government_services +
      emp_thousands_other_services,
    
    L_os_obs = emp_os / emp_total_thousands,
    
    # sector productivity in local currency
    A_a_lcu  = labour_productivity_va15_per_worker_agriculture,
    A_m_lcu  = labour_productivity_va15_per_worker_manufacturing,
    A_cs_lcu = labour_productivity_va15_per_worker_trade_services,
    A_os_lcu = ifelse(emp_os > 0, va_q15_os / emp_os, NA_real_)
  )

# ============================================================
# 3. PPP CONVERSION
# ============================================================

model_panel_ppp <- model_panel_raw %>%
  mutate(
    A_a_ppp  = A_a_lcu  / ppp_2015_scalar,
    A_m_ppp  = A_m_lcu  / ppp_2015_scalar,
    A_cs_ppp = A_cs_lcu / ppp_2015_scalar,
    A_os_ppp = A_os_lcu / ppp_2015_scalar
  )

# ============================================================
# 4. COUNTRY-SPECIFIC DEVELOPMENT NORMALIZATION
# All sectors normalized to own initial manufacturing productivity
# Preserves within-country productivity structure
# ============================================================

own_benchmark <- model_panel_ppp %>%
  group_by(iso3) %>%
  slice_min(year) %>%
  ungroup() %>%
  select(
    iso3,
    A_benchmark = A_m_ppp
  )

model_panel <- model_panel_ppp %>%
  left_join(own_benchmark, by = "iso3") %>%
  mutate(
    A_a  = A_a_ppp  / A_benchmark,
    A_m  = A_m_ppp  / A_benchmark,
    A_cs = A_cs_ppp / A_benchmark,
    A_os = A_os_ppp / A_benchmark,
    
    a_bar = a_bar_full / A_benchmark
  )

# ============================================================
# 5. COUNTRY LIST
# ============================================================

country_list <- c(
  "BFA","BWA","CMR","EGY","ETH","GHA","KEN", "IND",
  "LSO","MAR","MOZ","MUS","MWI","NAM","NGA",
  "RWA","SEN","TUN","TZA","UGA","ZAF","ZMB"
)

results_all <- list()

# ============================================================
# 6. UNIVERSAL FIXED PARAMETERS
# ============================================================

alpha_a  <- 0.30
alpha_m  <- 0.04
alpha_cs <- 0.35
alpha_os <- 1 - alpha_m - alpha_cs

rho_lower <- -0.5

cs_bar <- 0.10
os_bar <- 0.20
# ============================================================
# 7. LOOP OVER UNIVERSAL SPECS AND COUNTRIES
# ============================================================

for(country_choice in country_list) {
    
    df_country <- model_panel %>%
      filter(iso3 == country_choice) %>%
      arrange(year)
    
    L_a_data  <- df_country$L_a_obs
    L_m_data  <- df_country$L_m_obs
    L_cs_data <- df_country$L_cs_obs
    L_os_data <- df_country$L_os_obs
    
    A_a  <- df_country$A_a
    A_m  <- df_country$A_m
    A_cs <- df_country$A_cs
    A_os <- df_country$A_os
    
    a_bar <- df_country$a_bar
    
    keep <- is.finite(L_a_data) &
      is.finite(L_m_data) &
      is.finite(L_cs_data) &
      is.finite(L_os_data) &
      is.finite(A_a) &
      is.finite(A_m) &
      is.finite(A_cs) &
      is.finite(A_os) &
      is.finite(a_bar)
    
    df_country <- df_country[keep, ]
    
    if(nrow(df_country) == 0) next
    
    L_a_data  <- L_a_data[keep]
    L_m_data  <- L_m_data[keep]
    L_cs_data <- L_cs_data[keep]
    L_os_data <- L_os_data[keep]
    
    A_a  <- A_a[keep]
    A_m  <- A_m[keep]
    A_cs <- A_cs[keep]
    A_os <- A_os[keep]
    
    a_bar <- a_bar[keep]
    
    L_total <- 1
    
    L_a_hat <-
      (1 - alpha_a) * (a_bar / A_a) +
      alpha_a * (
        L_total +
          cs_bar / A_cs +
          os_bar / A_os
      )
    
    R_cs <- (
      (A_m / A_cs) *
        (alpha_m / alpha_cs)
    )^(1 / (rho_lower - 1))
    
    R_os <- (
      (A_m / A_os) *
        (alpha_m / alpha_os)
    )^(1 / (rho_lower - 1))
    
    denom <-
      1 +
      R_cs * (A_m / A_cs) +
      R_os * (A_m / A_os)
    
    L_m_hat <-
      (
        L_total - L_a_hat +
          cs_bar / A_cs +
          os_bar / A_os
      ) / denom
    
    L_cs_hat <-
      R_cs * (A_m / A_cs) * L_m_hat -
      cs_bar / A_cs
    
    L_os_hat <-
      R_os * (A_m / A_os) * L_m_hat -
      os_bar / A_os
    
    infeasible <- any(!is.finite(L_a_hat))  ||
      any(!is.finite(L_m_hat))  ||
      any(!is.finite(L_cs_hat)) ||
      any(!is.finite(L_os_hat)) ||
      any(L_a_hat < 0) ||
      any(L_m_hat < 0) ||
      any(L_cs_hat < 0) ||
      any(L_os_hat < 0)
    
    if(infeasible) {
      sse <- 1e10
    } else {
      sse <- sum(
        (L_a_hat  - L_a_data)^2 +
          (L_m_hat  - L_m_data)^2 +
          (L_cs_hat - L_cs_data)^2 +
          (L_os_hat - L_os_data)^2,
        na.rm = TRUE
      )
    }
    
    rmse <- sqrt(sse / (nrow(df_country) * 4))
    
    results_all[[paste(country_choice, rho_lower, sep = "_")]] <-
      tibble(
        country   = country_choice,
        years     = paste(min(df_country$year), max(df_country$year), sep = "-"),
        alpha_a   = alpha_a,
        alpha_m   = alpha_m,
        alpha_cs  = alpha_cs,
        alpha_os  = alpha_os,
        rho_lower = rho_lower,
        cs_bar    = cs_bar,
        os_bar    = os_bar,
        RMSE       = round(rmse, 3),
        feasible   = !infeasible
      )
  }

# ============================================================
# 8. RESULTS
# ============================================================

results_table <- bind_rows(results_all)

# ------------------------------------------------------------
# Country results
# ------------------------------------------------------------

results_table <- results_table %>%
  arrange(RMSE)

print(results_table, n = Inf)

# ------------------------------------------------------------
# Failed countries
# ------------------------------------------------------------

failed_table <- results_table %>%
  filter(!feasible) %>%
  select(country)

print(failed_table)

# ------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------

summary_table <- results_table %>%
  summarise(
    feasible_n  = sum(feasible),
    failed      = sum(!feasible),
    mean_rmse   = round(mean(RMSE[feasible], na.rm = TRUE), 3),
    median_rmse = round(median(RMSE[feasible], na.rm = TRUE), 3),
    min_rmse    = round(min(RMSE[feasible], na.rm = TRUE), 3),
    max_rmse    = round(max(RMSE[feasible], na.rm = TRUE), 3),
    n_good_005  = sum(RMSE <= 0.05 & feasible),
    n_good_010  = sum(RMSE <= 0.10 & feasible),
    n_good_015  = sum(RMSE <= 0.15 & feasible)
  )

print(summary_table)

# ------------------------------------------------------------
# Fit categories
# ------------------------------------------------------------

fit_categories <- results_table %>%
  filter(feasible) %>%
  mutate(
    fit_group = case_when(
      RMSE < 0.10 ~ "Good (<0.10)",
      RMSE < 0.15 ~ "Moderate (0.10-0.15)",
      TRUE        ~ "Weak (>0.15)"
    )
  ) %>%
  group_by(fit_group) %>%
  summarise(
    countries = paste(country, collapse = ", "),
    .groups = "drop"
  )

print(fit_categories)

results_display <- results_table %>%
  select(
    country,
    RMSE,
    feasible
  ) %>%
  arrange(RMSE)

print(results_display, n = Inf)

# ============================================================
# 9. PRODUCTIVITY SERIES: WELL-FIT COUNTRIES
# ============================================================

good_countries <- results_table %>%
  filter(feasible, RMSE <= 0.16) %>%
  pull(country)

print(good_countries)

# ------------------------------------------------------------
# Export productivity series
# ------------------------------------------------------------

productivity_series <- model_panel %>%
  filter(iso3 %in% good_countries) %>%
  select(
    iso3,
    year,
    A_a,
    A_m,
    A_cs,
    A_os
  ) %>%
  arrange(iso3, year)

print(productivity_series, n = Inf)

write_csv(
  productivity_series,
  "productivity_series_wellfit_countries.csv"
)

# ------------------------------------------------------------
# Long format for plotting
# ------------------------------------------------------------

productivity_long <- productivity_series %>%
  pivot_longer(
    cols = c(A_a, A_m, A_cs, A_os),
    names_to = "sector",
    values_to = "productivity"
  )

# ------------------------------------------------------------
# Individual country plots
# ------------------------------------------------------------

dir.create(
  "Productivity_Plots",
  showWarnings = FALSE
)

for(ctry in good_countries){
  
  p <- productivity_long %>%
    filter(iso3 == ctry) %>%
    ggplot(
      aes(
        x = year,
        y = productivity,
        colour = sector
      )
    ) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
      title = paste(
        ctry,
        "- Sectoral Productivity"
      ),
      x = NULL,
      y = "Productivity (Manufacturing 1990 = 1)"
    )
  
  ggsave(
    filename = paste0(
      "Productivity_Plots/",
      ctry,
      "_productivity.png"
    ),
    plot = p,
    width = 7,
    height = 5
  )
}

# ------------------------------------------------------------
# Combined facet plot
# ------------------------------------------------------------

p_all <- productivity_long %>%
  filter(iso3 %in% good_countries) %>%
  ggplot(
    aes(
      year,
      productivity,
      colour = sector
    )
  ) +
  geom_line(linewidth = 1) +
  
  facet_wrap(
    ~ iso3,
    scales = "free_y",
    ncol = 4
  ) +
  
  scale_colour_manual(
    values = c(
      "A_a"  = "#B22222",
      "A_cs" = "#2E8B57",
      "A_m"  = "#0B4F9C",
      "A_os" = "#8A2BE2"
    )
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    strip.background = element_rect(
      fill = "#0B2A5B",
      colour = "black"
    ),
    
    strip.text = element_text(
      colour = "white",
      face = "bold"
    ),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    panel.grid.minor = element_blank(),
    
    legend.position = "bottom"
  ) +
  
  labs(
    title = "Sectoral Productivity Trajectories",
    subtitle = "Productivity normalized to manufacturing productivity in initial year",
    x = NULL,
    y = "Relative productivity",
    colour = NULL
  )

ggsave(
  "Productivity_Plots/Productivity_Facets_All.png",
  p_all,
  width = 14,
  height = 10
)

# POLICY QUESTIONS

#New variables

gdp_growth <- master_panel %>%
  group_by(iso3) %>%
  summarise(
    gdppc_growth =
      100 * (last(gdppc) / first(gdppc) - 1),
    .groups = "drop"
  )

cs_growth <- productivity_series %>%
  group_by(iso3) %>%
  summarise(
    cs_growth =
      100 * (last(A_cs) / first(A_cs) - 1),
    .groups = "drop"
  )

growth_df <- cs_growth %>%
  left_join(
    gdp_growth,
    by = "iso3"
  )

growth_df <- growth_df %>%
  filter(!is.na(gdppc_growth))

growth_df_pos <- growth_df %>%
  filter(cs_growth > 0)

summary(
  lm(
    gdppc_growth ~ cs_growth,
    data = growth_df_pos
  )
)

summary(
  lm(
    gdppc_growth ~ cs_growth,
    data = growth_df
  )
)

ggplot(
  growth_df,
  aes(
    x = cs_growth,
    y = gdppc_growth,
    label = iso3
  )
) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(size = 3, nudge_y = 5) +
  labs(
    x = "CS Productivity Growth (%)",
    y = "GDP per Capita Growth (%)"
  )

# ============================================================
# CS PRODUCTIVITY GROWTH vs AGRICULTURAL EXIT
# ============================================================

cs_growth <- model_panel %>%
  group_by(iso3) %>%
  summarise(
    cs_growth =
      100 * (last(A_cs) / first(A_cs) - 1),
    .groups = "drop"
  )

ag_exit <- model_panel %>%
  group_by(iso3) %>%
  summarise(
    ag_exit =
      100 * (first(L_a) - last(L_a)),
    .groups = "drop"
  )

plot_df <- cs_growth %>%
  left_join(
    ag_exit,
    by = "iso3"
  )

print(plot_df)

# ----------------------------------------------------------
# REGRESSION
# ----------------------------------------------------------

summary(
  lm(
    ag_exit ~ cs_growth,
    data = plot_df
  )
)

# ----------------------------------------------------------
# PLOT
# ----------------------------------------------------------

ggplot(
  plot_df,
  aes(
    x = cs_growth,
    y = ag_exit,
    label = iso3
  )
) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  geom_text(
    nudge_y = 1
  ) +
  labs(
    x = "CS Productivity Growth (%)",
    y = "Agricultural Exit (pp)"
  ) +
  theme_thesis()

# LABOR SHARE AND GDP GROWTH

cs_share_2018 <- model_panel %>%
  group_by(iso3) %>%
  summarise(
    cs_share_2018 =
      100 * last(L_cs_obs),
    .groups = "drop"
  )

gdp_growth_log <- master_panel %>%
  group_by(iso3) %>%
  summarise(
    log_gdppc_growth =
      100 * (
        log(last(gdppc)) -
          log(first(gdppc))
      ),
    .groups = "drop"
  )

plot_df <- cs_share_2018 %>%
  left_join(
    gdp_growth_log,
    by = "iso3"
  ) %>%
  filter(!is.na(log_gdppc_growth))

summary(
  lm(
    log_gdppc_growth ~ cs_share_2018,
    data = plot_df
  )
)


# PLOT
ggplot(
  plot_df,
  aes(
    x = cs_share_2018,
    y = log_gdppc_growth,
    label = iso3
  )
) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  geom_text(
    size = 3,
    nudge_y = 5
  ) +
  labs(
    x = "Consumer Services Employment Share, 2018 (%)",
    y = "GDP per Capita Growth (LOG) (%)"
  ) +
  theme_minimal()