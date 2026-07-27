rm(list = ls())
# Ali Aydin Karamustafa
# Predicting Yearly Increase in Rental Prices in Barcelona
# ===========================
#   INSTALL & LOAD PACKAGES
# ===========================
install.packages("tidyverse")
install.packages("readxl")
install.packages("janitor")
install.packages("zoo")
install.packages("corrr")
install.packages("ggplot2")
install.packages("corrplot")
library(tidyverse)   # dplyr, readr, tidyr — essential for wrangling
library(readxl)      # for Excel files
library(janitor)     # clean_names(), remove_empty(), very helpful
library(lubridate)   # handle dates/years cleanly if needed
library(stringr)     # for string handling
library(purrr)       # for looping over many files
library(ggplot2)
library(corrr)
library(corrplot)
library(magrittr)
# ===========================
#   LOADING & CLEANING DATA
# ===========================
# -----OUTCOME VARIABLE:RENTAL PRICES------
rental_raw <- read_excel('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/rental_prices_2013-2024.xlsx',
  skip = 19
) %>%
  clean_names() %>%
  select(-1) %>%
  rename(barri=1) %>%
  drop_na(barri)
names(rental_raw)
names(rental_raw)[2:13] <- as.character(2024:2013)

# Pivot to long format
rental_raw <- rental_raw %>%
  mutate(across(`2013`:`2024`, as.numeric))
rental_long <- rental_raw %>%
  pivot_longer(
    cols = `2013`:`2024`,
    names_to = "year",
    values_to = "rent"
  ) %>%
  mutate(year = as.integer(year))

# Drop neighborhoods with >2 missing values  
rental_clean <- rental_long %>%
  group_by(barri) %>%
  filter(sum(is.na(rent)) <= 2) %>%
  ungroup()

# Interpolate missing values
library(zoo)
rental_clean <- rental_clean %>%
  group_by(barri) %>%
  mutate(rent = zoo::na.approx(rent, na.rm = FALSE)) %>%
  ungroup()
rental_clean <- rental_clean %>%
  group_by(barri) %>%
  mutate(rent = zoo::na.locf(rent, na.rm = FALSE)) %>%
  mutate(rent = zoo::na.locf(rent, fromLast = TRUE)) %>%
  ungroup()

#-----regressors-------------
# ADDRESS CHANGE RATE 2013-2021
# ACR 2013
address_raw13 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/address_change_rate_2013.csv') 
# View(address_raw13)
names(address_raw13)

address_clean13 <- address_raw13 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, address_change_rate = nombre) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    address_change_rate  = as.numeric(address_change_rate)     
  )

address_clean13 <- address_clean13 %>%
  group_by(barri, year) %>%
  summarise(address_change_rate = mean(address_change_rate, na.rm = TRUE), .groups = "drop")

# ACR 2014
address_raw14 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/address_change_rate_2014.csv') 
# View(address_raw14)
names(address_raw14)

address_clean14 <- address_raw14 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, address_change_rate = nombre) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    address_change_rate  = as.numeric(address_change_rate)     
  ) %>%
  group_by(barri, year) %>%
  summarise(address_change_rate = mean(address_change_rate, na.rm = TRUE), .groups = "drop")
# View(address_clean14)

# ACR 2015
address_raw15 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/address_change_rate_2015.csv') 
# View(address_raw15)
names(address_raw15)

address_clean15 <- address_raw15 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, address_change_rate = nombre) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    address_change_rate  = as.numeric(address_change_rate)     
  ) %>%
  group_by(barri, year) %>%
  summarise(address_change_rate = mean(address_change_rate, na.rm = TRUE), .groups = "drop")
# View(address_clean15)

# ACR 2016
address_raw16 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/address_change_rate_2016.csv') 
# View(address_raw16)
names(address_raw16)

address_clean16 <- address_raw16 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, address_change_rate = nombre) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    address_change_rate  = as.numeric(address_change_rate)     
  ) %>%
  group_by(barri, year) %>%
  summarise(address_change_rate = mean(address_change_rate, na.rm = TRUE), .groups = "drop")
# View(address_clean16)

# ACR 2017
address_raw17 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/address_change_rate_2017.csv') 
# View(address_raw17)
names(address_raw17)

address_clean17 <- address_raw17 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, address_change_rate = nombre) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    address_change_rate  = as.numeric(address_change_rate)     
  ) %>%
  group_by(barri, year) %>%
  summarise(address_change_rate = mean(address_change_rate, na.rm = TRUE), .groups = "drop")
# View(address_clean17)

# ACR 2018
address_raw18 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/address_change_rate_2018.csv') 
# View(address_raw18)
names(address_raw18)

address_clean18 <- address_raw18 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, address_change_rate = nombre) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    address_change_rate  = as.numeric(address_change_rate)     
  ) %>%
  group_by(barri, year) %>%
  summarise(address_change_rate = mean(address_change_rate, na.rm = TRUE), .groups = "drop")
# View(address_clean18)

# ACR 2019
address_raw19 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/address_change_rate_2019.csv') 
# View(address_raw19)
names(address_raw19)

address_clean19 <- address_raw19 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, address_change_rate = taxa_mil_hab) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    address_change_rate  = as.numeric(address_change_rate)     
  ) %>%
  group_by(barri, year) %>%
  summarise(address_change_rate = mean(address_change_rate, na.rm = TRUE), .groups = "drop")
# View(address_clean19)

# ACR 2020
address_raw20 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/address_change_rate_2020.csv') 
# View(address_raw20)
names(address_raw20)

address_clean20 <- address_raw20 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, address_change_rate = taxa_mil_hab) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    address_change_rate  = as.numeric(address_change_rate)     
  ) %>%
  group_by(barri, year) %>%
  summarise(address_change_rate = mean(address_change_rate, na.rm = TRUE), .groups = "drop")
# View(address_clean20)

# ACR 2021
address_raw21 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/address_change_rate_2021.csv') 
# View(address_raw21)
names(address_raw21)

address_clean21 <- address_raw21 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, address_change_rate = taxa_mil_hab) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    address_change_rate  = as.numeric(address_change_rate)     
  ) %>%
  group_by(barri, year) %>%
  summarise(address_change_rate = mean(address_change_rate, na.rm = TRUE), .groups = "drop")
# View(address_clean21)

# merge ACR across years
address_change_all <- dplyr::bind_rows(
  address_clean13,
  address_clean14,
  address_clean15,
  address_clean16,
  address_clean17,
  address_clean18,
  address_clean19,
  address_clean20,
  address_clean21
)
# View(address_change_all)

# AVERAGE TAXABLE INCOME
# ATI 2015
avginc_raw15 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/avg_taxable_income_2015.csv') 
# View(avginc_raw15)
names(avginc_raw15)
names(avginc_raw15 %>% clean_names())

avginc_clean15 <- avginc_raw15 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, avg_inc = import_renda_bruta) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    avg_inc  = as.numeric(avg_inc)     
  ) %>%
  group_by(barri, year) %>%
  summarise(avg_inc = mean(avg_inc, na.rm = TRUE), .groups = "drop")
# View(avginc_clean15)

# ATI 2016
avginc_raw16 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/avg_taxable_income_2016.csv') 
# View(avginc_raw16)
names(avginc_raw16)
names(avginc_raw16 %>% clean_names())

avginc_clean16 <- avginc_raw16 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, avg_inc = import_renda_bruta) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    avg_inc  = as.numeric(avg_inc)     
  ) %>%
  group_by(barri, year) %>%
  summarise(avg_inc = mean(avg_inc, na.rm = TRUE), .groups = "drop")
# View(avginc_clean16)

# ATI 2017
avginc_raw17 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/avg_taxable_income_2017.csv') 
# View(avginc_raw17)
names(avginc_raw17)
names(avginc_raw17 %>% clean_names())

avginc_clean17 <- avginc_raw17 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, avg_inc = import_renda_bruta) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    avg_inc  = as.numeric(avg_inc)     
  ) %>%
  group_by(barri, year) %>%
  summarise(avg_inc = mean(avg_inc, na.rm = TRUE), .groups = "drop")
# View(avginc_clean17)

# ATI 2018
avginc_raw18 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/avg_taxable_income_2018.csv') 
# View(avginc_raw18)
names(avginc_raw18)
names(avginc_raw18 %>% clean_names())

avginc_clean18 <- avginc_raw18 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, avg_inc = import_renda_bruta) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    avg_inc  = as.numeric(avg_inc)     
  ) %>%
  group_by(barri, year) %>%
  summarise(avg_inc = mean(avg_inc, na.rm = TRUE), .groups = "drop")
# View(avginc_clean18)

# ATI 2019
avginc_raw19 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/avg_taxable_income_2019.csv') 
# View(avginc_raw19)
names(avginc_raw19)
names(avginc_raw19 %>% clean_names())

avginc_clean19 <- avginc_raw19 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, avg_inc = import_renda_bruta) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    avg_inc  = as.numeric(avg_inc)     
  ) %>%
  group_by(barri, year) %>%
  summarise(avg_inc = mean(avg_inc, na.rm = TRUE), .groups = "drop")
# View(avginc_clean19)

# ATI 2020
avginc_raw20 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/avg_taxable_income_2020.csv') 
# View(avginc_raw20)
names(avginc_raw20)
names(avginc_raw20 %>% clean_names())

avginc_clean20 <- avginc_raw20 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, avg_inc = import_renda_bruta) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    avg_inc  = as.numeric(avg_inc)     
  ) %>%
  group_by(barri, year) %>%
  summarise(avg_inc = mean(avg_inc, na.rm = TRUE), .groups = "drop")
# View(avginc_clean20)

# ATI 2021
avginc_raw21 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/avg_taxable_income_2021.csv')
# View(avginc_raw21)
names(avginc_raw21)
names(avginc_raw21 %>% clean_names())

avginc_clean21 <- avginc_raw21 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, avg_inc = import_renda_bruta) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    avg_inc  = as.numeric(avg_inc)     
  ) %>%
  group_by(barri, year) %>%
  summarise(avg_inc = mean(avg_inc, na.rm = TRUE), .groups = "drop")
# View(avginc_clean21)

# ATI 2022
avginc_raw22 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/avg_taxable_income_2022.csv')
# View(avginc_raw22)
names(avginc_raw22)
names(avginc_raw22 %>% clean_names())

avginc_clean22 <- avginc_raw22 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, avg_inc = import_renda_bruta) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    avg_inc  = as.numeric(avg_inc)     
  ) %>%
  group_by(barri, year) %>%
  summarise(avg_inc = mean(avg_inc, na.rm = TRUE), .groups = "drop")
# View(avginc_clean22)

# ATI 2023
avginc_raw23 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/avg_taxable_income_2023.csv')
# View(avginc_raw23)
names(avginc_raw23)
names(avginc_raw23 %>% clean_names())

avginc_clean23 <- avginc_raw23 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, avg_inc = import_renda_bruta) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    avg_inc  = as.numeric(avg_inc)     
  ) %>%
  group_by(barri, year) %>%
  summarise(avg_inc = mean(avg_inc, na.rm = TRUE), .groups = "drop")
# View(avginc_clean23)

# merge ATI across years
avginc_all <- dplyr::bind_rows(
  avginc_clean15,
  avginc_clean16,
  avginc_clean17,
  avginc_clean18,
  avginc_clean19,
  avginc_clean20,
  avginc_clean21,
  avginc_clean22,
  avginc_clean23
)
# View(avginc_all)

# BIRTHS
# births 2013
births_raw2013 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2013.csv')
# View(births_raw2013)
names(births_raw2013)
names(births_raw2013 %>% clean_names())

births_clean2013 <- births_raw2013 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births  = as.numeric(births)     
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2013)

# births 2014
births_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2014.csv')
# View(births_raw2014)
names(births_raw2014)
names(births_raw2014 %>% clean_names())

births_clean2014 <- births_raw2014 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births  = as.numeric(births)     
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2014)

# births 2015
births_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2014.csv')
# View(births_raw2014)
names(births_raw2014)
names(births_raw2014 %>% clean_names())

births_clean2014 <- births_raw2014 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births  = as.numeric(births)     
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2014)

# births 2015
births_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2015.csv')
# View(births_raw2015)
names(births_raw2015)
names(births_raw2015 %>% clean_names())

births_clean2015 <- births_raw2015 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births = as.numeric(births)
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2015)

# births 2016
births_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2016.csv')
# View(births_raw2016)
names(births_raw2016)
names(births_raw2016 %>% clean_names())

births_clean2016 <- births_raw2016 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births = as.numeric(births)
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2016)

# births 2017
births_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2017.csv')
# View(births_raw2017)
names(births_raw2017)
names(births_raw2017 %>% clean_names())

births_clean2017 <- births_raw2017 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births = as.numeric(births)
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2017)

# births 2018
births_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2018.csv')
# View(births_raw2018)
names(births_raw2018)
names(births_raw2018 %>% clean_names())

births_clean2018 <- births_raw2018 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births = as.numeric(births)
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2018)

# births 2019
births_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2019.csv')
# View(births_raw2019)
names(births_raw2019)
names(births_raw2019 %>% clean_names())

births_clean2019 <- births_raw2019 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births = as.numeric(births)
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2019)

# births 2020
births_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2020.csv')
# View(births_raw2020)
names(births_raw2020)
names(births_raw2020 %>% clean_names())

births_clean2020 <- births_raw2020 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births = as.numeric(births)
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2020)

# births 2021
births_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2021.csv')
# View(births_raw2021)
names(births_raw2021)
names(births_raw2021 %>% clean_names())

births_clean2021 <- births_raw2021 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births = as.numeric(births)
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2021)

# births 2022
births_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2022.csv')
# View(births_raw2022)
names(births_raw2022)
names(births_raw2022 %>% clean_names())

births_clean2022 <- births_raw2022 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births = as.numeric(births)
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2022)

# births 2023
births_raw2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2023.csv')
# View(births_raw2023)
names(births_raw2023)
names(births_raw2023 %>% clean_names())

births_clean2023 <- births_raw2023 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births = as.numeric(births)
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2023)

# births 2024
births_raw2024 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/births_2024.csv')
# View(births_raw2024)
names(births_raw2024)
names(births_raw2024 %>% clean_names())

births_clean2024 <- births_raw2024 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, births = valor) %>%  
  mutate(
    barri = as.character(barri),
    year  = as.integer(year),
    births = as.numeric(births)
  ) %>%
  group_by(barri, year) %>%
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
# View(births_clean2024)

# binding births
births_all <- bind_rows(
  births_clean2013,
  births_clean2014,
  births_clean2015,
  births_clean2016,
  births_clean2017,
  births_clean2018,
  births_clean2019,
  births_clean2020,
  births_clean2021,
  births_clean2022,
  births_clean2023,
  births_clean2024
)

# CPI (COUNTRY WIDE)
library(readr)
library(dplyr)
library(janitor)

cpi_raw <- read_delim(
  '/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/CPI_Spain_2013-2024.csv',
  delim = "\t",
  locale = locale(decimal_mark = ",", grouping_mark = ".")
) %>%
  clean_names()

str(cpi_raw)
head(cpi_raw$total)

# compute yearly CPI averages
cpi_yearly_clean <- cpi_raw %>%
  mutate(year = as.integer(substr(periodo, 1, 4))) %>%
  group_by(year) %>%
  summarise(cpi_index = mean(total, na.rm = TRUE), .groups = "drop") %>%
  filter(year >= 2013, year <= 2024)
# View(cpi_yearly_clean)

# DEATHS
# deaths 2013
deaths_raw2013 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2013.csv')
# View(deaths_raw2013)
names(deaths_raw2013)
names(deaths_raw2013 %>% clean_names())

deaths_clean2013 <- deaths_raw2013 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2013)

# deaths 2014
deaths_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2014.csv')
# View(deaths_raw2014)
names(deaths_raw2014)
names(deaths_raw2014 %>% clean_names())

deaths_clean2014 <- deaths_raw2014 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2014)

# deaths 2015
deaths_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2015.csv')
# View(deaths_raw2015)
names(deaths_raw2015)
names(deaths_raw2015 %>% clean_names())

deaths_clean2015 <- deaths_raw2015 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2015)

# deaths 2016
deaths_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2016.csv')
# View(deaths_raw2016)
names(deaths_raw2016)
names(deaths_raw2016 %>% clean_names())

deaths_clean2016 <- deaths_raw2016 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2016)

# deaths 2017
deaths_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2017.csv')
# View(deaths_raw2017)
names(deaths_raw2017)
names(deaths_raw2017 %>% clean_names())

deaths_clean2017 <- deaths_raw2017 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2017)

# deaths 2018
deaths_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2018.csv')
# View(deaths_raw2018)
names(deaths_raw2018)
names(deaths_raw2018 %>% clean_names())

deaths_clean2018 <- deaths_raw2018 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2018)

# deaths 2019
deaths_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2019.csv')
# View(deaths_raw2019)
names(deaths_raw2019)
names(deaths_raw2019 %>% clean_names())

deaths_clean2019 <- deaths_raw2019 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2019)

# deaths 2020
deaths_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2020.csv')
# View(deaths_raw2020)
names(deaths_raw2020)
names(deaths_raw2020 %>% clean_names())

deaths_clean2020 <- deaths_raw2020 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2020)

# deaths 2021
deaths_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2021.csv')
# View(deaths_raw2021)
names(deaths_raw2021)
names(deaths_raw2021 %>% clean_names())

deaths_clean2021 <- deaths_raw2021 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2021)

# deaths 2022
deaths_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2022.csv')
# View(deaths_raw2022)
names(deaths_raw2022)
names(deaths_raw2022 %>% clean_names())

deaths_clean2022 <- deaths_raw2022 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2022)

# deaths 2023
deaths_raw2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2023.csv')
# View(deaths_raw2023)
names(deaths_raw2023)
names(deaths_raw2023 %>% clean_names())

deaths_clean2023 <- deaths_raw2023 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2023)

# deaths 2024
deaths_raw2024 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/deaths_2024.csv')
# View(deaths_raw2024)
names(deaths_raw2024)
names(deaths_raw2024 %>% clean_names())

deaths_clean2024 <- deaths_raw2024 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, deaths = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    deaths = as.numeric(deaths)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    deaths = ifelse(is.na(deaths), mean(deaths, na.rm = TRUE), deaths)
  ) %>%
  summarise(deaths = sum(deaths), .groups = "drop")
# View(deaths_clean2024)

# binding deaths
deaths_all <- bind_rows(
  deaths_clean2013,
  deaths_clean2014,
  deaths_clean2015,
  deaths_clean2016,
  deaths_clean2017,
  deaths_clean2018,
  deaths_clean2019,
  deaths_clean2020,
  deaths_clean2021,
  deaths_clean2022,
  deaths_clean2023,
  deaths_clean2024
)

# DISPOSABLE FAMILY INCOME 2022
disp_fam_inc_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/disposable_family_income_2022[invariant].csv')
# View(disp_fam_inc_raw2022)
names(disp_fam_inc_raw2022)
names(disp_fam_inc_raw2022 %>% clean_names())

disp_fam_inc_clean2022 <- disp_fam_inc_raw2022 %>%
  clean_names() %>%
  select(
    barri = nom_barri,
    disp_fam_inc = import_euros 
  ) %>%  
  mutate(
    barri = as.character(barri),
    disp_fam_inc = as.numeric(disp_fam_inc),
    year = 2022
  ) %>%
  group_by(barri, year) %>%
  summarise(
    disp_fam_inc = mean(disp_fam_inc, na.rm = TRUE),
    .groups = "drop"
  )

# View(disp_fam_inc_clean2022)

# EDUCATION LEVEL
# edu 2013
edu_raw2013 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2013.csv') %>%
  clean_names()
names(edu_raw2013)
# View(edu_raw2013)

edu_clean2013 <- edu_raw2013 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2013,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2013 <- edu_clean2013 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2013)

# edu 2014
edu_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2014.csv') %>%
  clean_names()
names(edu_raw2014)
# View(edu_raw2014)

edu_clean2014 <- edu_raw2014 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2014,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2014 <- edu_clean2014 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2014)

# edu 2015
edu_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2015.csv') %>%
  clean_names()
names(edu_raw2015)
# View(edu_raw2015)

edu_clean2015 <- edu_raw2015 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2015,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2015 <- edu_clean2015 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2015)

# edu 2016
edu_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2016.csv') %>%
  clean_names()
names(edu_raw2016)
# View(edu_raw2016)

edu_clean2016 <- edu_raw2016 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2016,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2016 <- edu_clean2016 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2016)

# edu 2017
edu_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2017.csv') %>%
  clean_names()
names(edu_raw2017)
# View(edu_raw2017)

edu_clean2017 <- edu_raw2017 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2017,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2017 <- edu_clean2017 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2017)

# edu 2018
edu_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2018.csv') %>%
  clean_names()
names(edu_raw2018)
# View(edu_raw2018)

edu_clean2018 <- edu_raw2018 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2018,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2018 <- edu_clean2018 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2018)

# edu 2019
edu_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2019.csv') %>%
  clean_names()
names(edu_raw2019)
# View(edu_raw2019)

edu_clean2019 <- edu_raw2019 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2019,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2019 <- edu_clean2019 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2019)

# edu 2020
edu_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2020.csv') %>%
  clean_names()
names(edu_raw2020)
# View(edu_raw2020)

edu_clean2020 <- edu_raw2020 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2020,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2020 <- edu_clean2020 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2020)

# edu 2021
edu_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2021.csv') %>%
  clean_names()
names(edu_raw2021)
# View(edu_raw2021)

edu_clean2021 <- edu_raw2021 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2021,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2021 <- edu_clean2021 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2021)

# edu 2022
edu_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2022.csv') %>%
  clean_names()
names(edu_raw2022)
# View(edu_raw2022)

edu_clean2022 <- edu_raw2022 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2022,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2022 <- edu_clean2022 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2022)

# edu 2023
edu_raw2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2023.csv') %>%
  clean_names()
names(edu_raw2023)
# View(edu_raw2023)

edu_clean2023 <- edu_raw2023 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2023,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2023 <- edu_clean2023 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2023)

# edu 2024
edu_raw2024 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/education_2024.csv') %>%
  clean_names()
names(edu_raw2024)
# View(edu_raw2024)

edu_clean2024 <- edu_raw2024 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = 2024,
    edu_level = as.integer(niv_educa_esta),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, edu_level, value) %>%
  group_by(barri, year, edu_level) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

edu_wide2024 <- edu_clean2024 %>%
  pivot_wider(
    names_from   = edu_level,
    values_from  = value,
    names_prefix = "edu_lvl_",
    values_fill  = list(value = 0)
  )
# View(edu_wide2024)

edu_all <- bind_rows(
  edu_wide2013,
  edu_wide2014,
  edu_wide2015,
  edu_wide2016,
  edu_wide2017,
  edu_wide2018,
  edu_wide2019,
  edu_wide2020,
  edu_wide2021,
  edu_wide2022,
  edu_wide2023,
  edu_wide2024
)

# View(edu_all)

# EMIGRATION
# emigration 2013
emigration_raw2013 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2013.csv')
# View(emigration_raw2013)
names(emigration_raw2013)
names(emigration_raw2013 %>% clean_names())

emigration_clean2013 <- emigration_raw2013 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2013)

# emigration 2014
emigration_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2014.csv') 
# View(emigration_raw2014)
names(emigration_raw2014)
names(emigration_raw2014 %>% clean_names())

emigration_clean2014 <- emigration_raw2014 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2014)

# emigration 2015
emigration_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2015.csv') 
# View(emigration_raw2015)
names(emigration_raw2015)
names(emigration_raw2015 %>% clean_names())

emigration_clean2015 <- emigration_raw2015 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2015)

# emigration 2016
emigration_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2016.csv') 
# View(emigration_raw2016)
names(emigration_raw2016)
names(emigration_raw2016 %>% clean_names())

emigration_clean2016 <- emigration_raw2016 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2016)

# emigration 2017
emigration_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2017.csv') 
# View(emigration_raw2017)
names(emigration_raw2017)
names(emigration_raw2017 %>% clean_names())

emigration_clean2017 <- emigration_raw2017 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2017)

# emigration 2018
emigration_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2018.csv') 
# View(emigration_raw2018)
names(emigration_raw2018)
names(emigration_raw2018 %>% clean_names())

emigration_clean2018 <- emigration_raw2018 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2018)

# emigration 2019
emigration_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2019.csv') 
# View(emigration_raw2019)
names(emigration_raw2019)
names(emigration_raw2019 %>% clean_names())

emigration_clean2019 <- emigration_raw2019 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2019)

# emigration 2020 
emigration_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2020.csv') 
# View(emigration_raw2020)
names(emigration_raw2020)
names(emigration_raw2020 %>% clean_names())

emigration_clean2020 <- emigration_raw2020 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2020)

# emigration 2021 
emigration_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2021.csv') 
# View(emigration_raw2021)
names(emigration_raw2021)
names(emigration_raw2021 %>% clean_names())

emigration_clean2021 <- emigration_raw2021 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2021)

# emigration 2022
emigration_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2022.csv') 
# View(emigration_raw2022)
names(emigration_raw2022)
names(emigration_raw2022 %>% clean_names())

emigration_clean2022 <- emigration_raw2022 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2022)

# emigration 2023
emigration_raw2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2023.csv') 
# View(emigration_raw2023)
names(emigration_raw2023)
names(emigration_raw2023 %>% clean_names())

emigration_clean2023 <- emigration_raw2023 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2023)

# emigration 2024 
emigration_raw2024 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/emigration_2024.csv') 
# View(emigration_raw2024)
names(emigration_raw2024)
names(emigration_raw2024 %>% clean_names())

emigration_clean2024 <- emigration_raw2024 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, emigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    emigrants = as.numeric(emigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(emigrants = sum(emigrants), .groups = "drop")
# View(emigration_clean2024)

# bind all years
emigration_all <- bind_rows(
  emigration_clean2013,
  emigration_clean2014,
  emigration_clean2015,
  emigration_clean2016,
  emigration_clean2017,
  emigration_clean2018,
  emigration_clean2019,
  emigration_clean2020,
  emigration_clean2021,
  emigration_clean2022,
  emigration_clean2023,
  emigration_clean2024
)

# View(emigration_all)

# GINI INDEX
# gini 2015
gini_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/gini_index_2015.csv')
# View(gini_raw2015)
names(gini_raw2015)
names(gini_raw2015 %>% clean_names())

gini_clean2015 <- gini_raw2015 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, gini_index = index_gini) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(year),
    gini_index = as.numeric(gini_index)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    gini_index = ifelse(is.na(gini_index), mean(gini_index, na.rm = TRUE), gini_index)
  ) %>%
  summarise(gini_index = mean(gini_index), .groups = "drop")
# View(gini_clean2015)

# gini 2016
gini_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/gini_index_2016.csv')
# View(gini_raw2016)
names(gini_raw2016)
names(gini_raw2016 %>% clean_names())

gini_clean2016 <- gini_raw2016 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, gini_index = index_gini) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    gini_index = as.numeric(gini_index)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    gini_index = ifelse(is.na(gini_index), mean(gini_index, na.rm = TRUE), gini_index)
  ) %>%
  summarise(gini_index = mean(gini_index), .groups = "drop")
# View(gini_clean2016)

# gini 2017
gini_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/gini_index_2017.csv')
# View(gini_raw2017)
names(gini_raw2017)
names(gini_raw2017 %>% clean_names())

gini_clean2017 <- gini_raw2017 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, gini_index = index_gini) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    gini_index = as.numeric(gini_index)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    gini_index = ifelse(is.na(gini_index), mean(gini_index, na.rm = TRUE), gini_index)
  ) %>%
  summarise(gini_index = mean(gini_index), .groups = "drop")
# View(gini_clean2017)

# gini 2018
gini_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/gini_index_2018.csv')
# View(gini_raw2018)
names(gini_raw2018)
names(gini_raw2018 %>% clean_names())

gini_clean2018 <- gini_raw2018 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, gini_index = index_gini) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    gini_index = as.numeric(gini_index)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    gini_index = ifelse(is.na(gini_index), mean(gini_index, na.rm = TRUE), gini_index)
  ) %>%
  summarise(gini_index = mean(gini_index), .groups = "drop")
# View(gini_clean2018)

# gini 2019
gini_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/gini_index_2019.csv')
# View(gini_raw2019)
names(gini_raw2019)
names(gini_raw2019 %>% clean_names())

gini_clean2019 <- gini_raw2019 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, gini_index = index_gini) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    gini_index = as.numeric(gini_index)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    gini_index = ifelse(is.na(gini_index), mean(gini_index, na.rm = TRUE), gini_index)
  ) %>%
  summarise(gini_index = mean(gini_index), .groups = "drop")
# View(gini_clean2019)

# gini 2020
gini_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/gini_index_2020.csv')
# View(gini_raw2020)
names(gini_raw2020)
names(gini_raw2020 %>% clean_names())

gini_clean2020 <- gini_raw2020 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, gini_index = index_gini) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    gini_index = as.numeric(gini_index)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    gini_index = ifelse(is.na(gini_index), mean(gini_index, na.rm = TRUE), gini_index)
  ) %>%
  summarise(gini_index = mean(gini_index), .groups = "drop")
# View(gini_clean2020)

# gini 2021
gini_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/gini_index_2021.csv')
# View(gini_raw2021)
names(gini_raw2021)
names(gini_raw2021 %>% clean_names())

gini_clean2021 <- gini_raw2021 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, gini_index = index_gini) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    gini_index = as.numeric(gini_index)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    gini_index = ifelse(is.na(gini_index), mean(gini_index, na.rm = TRUE), gini_index)
  ) %>%
  summarise(gini_index = mean(gini_index), .groups = "drop")
# View(gini_clean2021)

# gini 2022
gini_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/gini_index_2022.csv')
# View(gini_raw2022)
names(gini_raw2022)
names(gini_raw2022 %>% clean_names())

gini_clean2022 <- gini_raw2022 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, gini_index = index_gini) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    gini_index = as.numeric(gini_index)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    gini_index = ifelse(is.na(gini_index), mean(gini_index, na.rm = TRUE), gini_index)
  ) %>%
  summarise(gini_index = mean(gini_index), .groups = "drop")
# View(gini_clean2022)

# gini 2023
gini_raw2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/gini_index_2023.csv')
# View(gini_raw2023)
names(gini_raw2023)
names(gini_raw2023 %>% clean_names())

gini_clean2023 <- gini_raw2023 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, gini_index = index_gini) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    gini_index = as.numeric(gini_index)
  ) %>%
  group_by(barri, year) %>%
  mutate(
    gini_index = ifelse(is.na(gini_index), mean(gini_index, na.rm = TRUE), gini_index)
  ) %>%
  summarise(gini_index = mean(gini_index), .groups = "drop")
# View(gini_clean2023)

# bind all years
gini_all <- bind_rows(
  gini_clean2015,
  gini_clean2016,
  gini_clean2017,
  gini_clean2018,
  gini_clean2019,
  gini_clean2020,
  gini_clean2021,
  gini_clean2022,
  gini_clean2023
)

# View(gini_all)

# HOUSEHOLD NATIONALITY
# HN 2013
household_nat_raw2013 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2013.csv') %>%
  clean_names()
names(household_nat_raw2013)
# View(household_nat_raw2013)

household_nat_clean2013 <-household_nat_raw2013 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"   # just in case
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2013)

# HN 2014
household_nat_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2014.csv') %>%
  clean_names()
names(household_nat_raw2014)
# View(household_nat_raw2014)

household_nat_clean2014 <- household_nat_raw2014 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2014)

# HN 2015
household_nat_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2015.csv') %>%
  clean_names()
names(household_nat_raw2015)
# View(household_nat_raw2015)

household_nat_clean2015 <- household_nat_raw2015 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2015)

# HN 2016
household_nat_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2016.csv') %>%
  clean_names()
names(household_nat_raw2016)
# View(household_nat_raw2016)

household_nat_clean2016 <- household_nat_raw2016 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2016)

# HN 2017
household_nat_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2017.csv') %>%
  clean_names()
names(household_nat_raw2017)
# View(household_nat_raw2017)

household_nat_clean2017 <- household_nat_raw2017 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2017)

# HN 2018
household_nat_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2018.csv') %>%
  clean_names()
names(household_nat_raw2018)
# View(household_nat_raw2018)

household_nat_clean2018 <- household_nat_raw2018 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2018)

# HN 2019
household_nat_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2019.csv') %>%
  clean_names()
names(household_nat_raw2019)
# View(household_nat_raw2019)

household_nat_clean2019 <- household_nat_raw2019 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2019)

# HN 2020
household_nat_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2020.csv') %>%
  clean_names()
names(household_nat_raw2020)
# View(household_nat_raw2020)

household_nat_clean2020 <- household_nat_raw2020 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2020)

# HN 2021
household_nat_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2021.csv') %>%
  clean_names()
names(household_nat_raw2021)
# View(household_nat_raw2021)

household_nat_clean2021 <- household_nat_raw2021 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2021)

# HN 2022
household_nat_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2022.csv') %>%
  clean_names()
names(household_nat_raw2022)
# View(household_nat_raw2022)

household_nat_clean2022 <- household_nat_raw2022 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2022)

# HN 2023
household_nat_raw2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2023.csv') %>%
  clean_names()
names(household_nat_raw2023)
# View(household_nat_raw2023)

household_nat_clean2023 <- household_nat_raw2023 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2023)

# HN 2024
household_nat_raw2024 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/household_nationality_2024.csv') %>%
  clean_names()
names(household_nat_raw2024)
# View(household_nat_raw2024)

household_nat_clean2024 <- household_nat_raw2024 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    households = valor, 
    cat_nat_raw = nacionalitat_domicili
  ) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    households = as.numeric(households),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1") ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2") ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3") ~ "RestWorld",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(households = sum(households, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = households,
    names_prefix = "hh_nat_",
    values_fill  = list(households = 0)
  )
# View(household_nat_clean2024)

# binding all years
household_nat_all <- bind_rows(
  household_nat_clean2013,
  household_nat_clean2014,
  household_nat_clean2015,
  household_nat_clean2016,
  household_nat_clean2017,
  household_nat_clean2018,
  household_nat_clean2019,
  household_nat_clean2020,
  household_nat_clean2021,
  household_nat_clean2022,
  household_nat_clean2023,
  household_nat_clean2024
)

# View(household_nat_all)

# HOUSING SALES BARCELONA, ALL YEARS
library(tidyverse)
library(readxl)
library(janitor)

# 1. Read raw Excel (all Spain)
housing_sales_raw <- read_excel('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/housing_sales_barcelona_2013-2024.XLS'
) %>%
  clean_names()
names(housing_sales_raw)
# View(housing_sales_raw)

# 2. Filter to Barcelona city row(s)
housing_sales_bcn <- housing_sales_raw %>%
  slice(5083)   
# View(housing_sales_bcn)

# 3. Take only the quarter columns (from x3 onward)
housing_sales_all <- housing_sales_bcn %>%
  select(starts_with("x")) %>%           # x1, x2, x3, ... but x1 is row id
  select(-x1, -x2) %>%                   # drop id / text cols, keep x3:...
  pivot_longer(
    cols      = everything(),
    names_to  = "col",
    values_to = "sales"
  ) %>%
  mutate(
    col_pos = row_number(),              # 1,2,3,... in original column order
    year    = 2004 + (col_pos - 1) %/% 4, # 4 quarters per year starting 2004
    quarter = 1   + (col_pos - 1) %% 4,
    sales   = as.numeric(sales)
  ) %>%
  filter(year >= 2013, year <= 2024) %>%  # keep only 2013–2024
  group_by(year) %>%
  summarise(housing_sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  mutate(barri = "Barcelona") %>%
  select(year, barri, housing_sales) %>%
  arrange(year)

# View(housing_sales_all)

# IMMIGRATION
# immigration 2013
immigration_raw2013 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2013.csv') 
# View(immigration_raw2013)
names(immigration_raw2013)

immigration_clean2013 <- immigration_raw2013 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri    = as.character(barri),
    year     = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2013)

# immigration 2014
immigration_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2014.csv') 
# View(immigration_raw2014)
names(immigration_raw2014)

immigration_clean2014 <- immigration_raw2014 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2014)

# immigration 2015
immigration_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2015.csv') 
# View(immigration_raw2015)
names(immigration_raw2015)

immigration_clean2015 <- immigration_raw2015 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2015)

# immigration 2016
immigration_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2016.csv') 
# View(immigration_raw2016)
names(immigration_raw2016)

immigration_clean2016 <- immigration_raw2016 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2016)

# immigration 2017
immigration_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2017.csv') 
# View(immigration_raw2017)
names(immigration_raw2017)

immigration_clean2017 <- immigration_raw2017 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2017)

# immigration 2018
immigration_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2018.csv') 
# View(immigration_raw2018)
names(immigration_raw2018)

immigration_clean2018 <- immigration_raw2018 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2018)

# immigration 2019
immigration_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2019.csv') 
# View(immigration_raw2019)
names(immigration_raw2019)

immigration_clean2019 <- immigration_raw2019 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2019)

# immigration 2020
immigration_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2020.csv') 
# View(immigration_raw2020)
names(immigration_raw2020)

immigration_clean2020 <- immigration_raw2020 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2020)

# immigration 2021
immigration_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2021.csv') 
# View(immigration_raw2021)
names(immigration_raw2021)

immigration_clean2021 <- immigration_raw2021 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2021)

# immigration 2022
immigration_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2022.csv') 
# View(immigration_raw2022)
names(immigration_raw2022)

immigration_clean2022 <- immigration_raw2022 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2022)

# immigration 2023
immigration_raw2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2023.csv') 
# View(immigration_raw2023)
names(immigration_raw2023)

immigration_clean2023 <- immigration_raw2023 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2023)

# immigration 2024
immigration_raw2024 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/immigration_2024.csv') 
# View(immigration_raw2024)
names(immigration_raw2024)

immigration_clean2024 <- immigration_raw2024 %>%
  clean_names() %>%
  select(barri = nom_barri, year = any, immigrants = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(year),
    immigrants = as.numeric(immigrants)
  ) %>%
  group_by(barri, year) %>%
  summarise(immigrants = sum(immigrants), .groups = "drop")
# View(immigration_clean2024)

# binding all years
immigration_all <- bind_rows(
  immigration_clean2013,
  immigration_clean2014,
  immigration_clean2015,
  immigration_clean2016,
  immigration_clean2017,
  immigration_clean2018,
  immigration_clean2019,
  immigration_clean2020,
  immigration_clean2021,
  immigration_clean2022,
  immigration_clean2023,
  immigration_clean2024
)
# View(immigration_all)

# NUMBER OF CONTRACTS
contracts_raw <- read_excel('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/number_of_contracts_2013-2024.xlsx',
  skip = 18
  ) %>%
  clean_names() %>%
  select(-1) %>%
  rename(barri=1) %>%
  drop_na(barri)
names(contracts_raw)
names(contracts_raw)[2:13] <- as.character(2013:2024)
# View(contracts_raw)

# Pivot to long format
contracts_raw <- contracts_raw %>%
  mutate(across(`2013`:`2024`, as.numeric))
contracts_clean <- contracts_raw %>%
  pivot_longer(
    cols = `2013`:`2024`,
    names_to = "year",
    values_to = "contracts"
  ) %>%
  mutate(year = as.integer(year))
# View(contracts_clean)

# POPULATION
# pop 2013
pop_raw2013 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2013.csv') 
# View(pop_raw2013)
names(pop_raw2013)

pop_clean2013 <- pop_raw2013 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri = as.character(barri),
    year = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2013)

# pop 2014
pop_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2014.csv') 
# View(pop_raw2014)
names(pop_raw2014)

pop_clean2014 <- pop_raw2014 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2014)

# pop 2015
pop_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2015.csv') 
# View(pop_raw2015)
names(pop_raw2015)

pop_clean2015 <- pop_raw2015 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2015)

# pop 2016
pop_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2016.csv') 
# View(pop_raw2016)
names(pop_raw2016)

pop_clean2016 <- pop_raw2016 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2016)

# pop 2017
pop_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2017.csv') 
# View(pop_raw2017)
names(pop_raw2017)

pop_clean2017 <- pop_raw2017 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2017)

# pop 2018
pop_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2018.csv') 
# View(pop_raw2018)
names(pop_raw2018)

pop_clean2018 <- pop_raw2018 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2018)

# pop 2019
pop_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2019.csv') 
# View(pop_raw2019)
names(pop_raw2019)

pop_clean2019 <- pop_raw2019 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2019)

# pop 2020
pop_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2020.csv') 
# View(pop_raw2020)
names(pop_raw2020)

pop_clean2020 <- pop_raw2020 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2020)

# pop 2021
pop_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2021.csv') 
# View(pop_raw2021)
names(pop_raw2021)

pop_clean2021 <- pop_raw2021 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2021)

# pop 2022
pop_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2022.csv') 
# View(pop_raw2022)
names(pop_raw2022)

pop_clean2022 <- pop_raw2022 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2022)

# pop 2023
pop_raw2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2023.csv') 
# View(pop_raw2023)
names(pop_raw2023)

pop_clean2023 <- pop_raw2023 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2023)

# pop 2024
pop_raw2024 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_2024.csv') 
# View(pop_raw2024)
names(pop_raw2024)

pop_clean2024 <- pop_raw2024 %>%
  clean_names() %>%
  select(barri = nom_barri, year = data_referencia, population = valor) %>%
  mutate(
    barri      = as.character(barri),
    year       = as.integer(substr(year, 1, 4)),
    population = as.numeric(population)
  ) %>%
  group_by(barri, year) %>%
  summarise(population = sum(population), .groups = "drop")
# View(pop_clean2024)

# binding all years
pop_all <- bind_rows(
  pop_clean2013,
  pop_clean2014,
  pop_clean2015,
  pop_clean2016,
  pop_clean2017,
  pop_clean2018,
  pop_clean2019,
  pop_clean2020,
  pop_clean2021,
  pop_clean2022,
  pop_clean2023,
  pop_clean2024
)
# View(pop_all)

# POPULATION BY AGE
# pop age 2013
pop_age_raw2013 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2013.csv') %>%
  clean_names()
names(pop_age_raw2013)
# View(pop_age_raw2013)

pop_age_clean2013 <- pop_age_raw2013 %>%
  mutate(
    barri     = as.character(nom_barri),
    year      = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value     = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2013 <- pop_age_clean2013 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2013)

# pop age 2014
pop_age_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2014.csv') %>%
  clean_names()
names(pop_age_raw2014)
# View(pop_age_raw2014)

pop_age_clean2014 <- pop_age_raw2014 %>%
  mutate(
    barri         = as.character(nom_barri),
    year          = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value         = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2014 <- pop_age_clean2014 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2014)

# pop age 2015
pop_age_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2015.csv') %>%
  clean_names()
names(pop_age_raw2015)
# View(pop_age_raw2015)

pop_age_clean2015 <- pop_age_raw2015 %>%
  mutate(
    barri         = as.character(nom_barri),
    year          = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value         = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2015 <- pop_age_clean2015 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2015)

# pop age 2016
pop_age_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2016.csv') %>%
  clean_names()
names(pop_age_raw2016)
# View(pop_age_raw2016)

pop_age_clean2016 <- pop_age_raw2016 %>%
  mutate(
    barri         = as.character(nom_barri),
    year          = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value         = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2016 <- pop_age_clean2016 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2016)

# pop age 2017
pop_age_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2017.csv') %>%
  clean_names()
names(pop_age_raw2017)
# View(pop_age_raw2017)

pop_age_clean2017 <- pop_age_raw2017 %>%
  mutate(
    barri         = as.character(nom_barri),
    year          = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value         = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2017 <- pop_age_clean2017 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2017)

# pop age 2018
pop_age_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2018.csv') %>%
  clean_names()
names(pop_age_raw2018)
# View(pop_age_raw2018)

pop_age_clean2018 <- pop_age_raw2018 %>%
  mutate(
    barri         = as.character(nom_barri),
    year          = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value         = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2018 <- pop_age_clean2018 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2018)

# pop age 2019
pop_age_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2019.csv') %>%
  clean_names()
names(pop_age_raw2019)
# View(pop_age_raw2019)

pop_age_clean2019 <- pop_age_raw2019 %>%
  mutate(
    barri         = as.character(nom_barri),
    year          = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value         = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2019 <- pop_age_clean2019 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2019)

# pop age 2020
pop_age_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2020.csv') %>%
  clean_names()
names(pop_age_raw2020)
# View(pop_age_raw2020)

pop_age_clean2020 <- pop_age_raw2020 %>%
  mutate(
    barri         = as.character(nom_barri),
    year          = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value         = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2020 <- pop_age_clean2020 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2020)

# pop age 2021
pop_age_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2021.csv') %>%
  clean_names()
names(pop_age_raw2021)
# View(pop_age_raw2021)

pop_age_clean2021 <- pop_age_raw2021 %>%
  mutate(
    barri         = as.character(nom_barri),
    year          = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value         = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2021 <- pop_age_clean2021 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2021)

# pop age 2022
pop_age_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2022.csv') %>%
  clean_names()
names(pop_age_raw2022)
# View(pop_age_raw2022)

pop_age_clean2022 <- pop_age_raw2022 %>%
  mutate(
    barri         = as.character(nom_barri),
    year          = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value         = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2022 <- pop_age_clean2022 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2022)

# pop age 2023
pop_age_raw2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2023.csv') %>%
  clean_names()
names(pop_age_raw2023)
# View(pop_age_raw2023)

pop_age_clean2023 <- pop_age_raw2023 %>%
  mutate(
    barri         = as.character(nom_barri),
    year          = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value         = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2023 <- pop_age_clean2023 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2023)
      
# pop age 2024
pop_age_raw2024 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_age_groups_2024.csv') %>%
  clean_names()
names(pop_age_raw2024)
# View(pop_age_raw2024)

pop_age_clean2024 <- pop_age_raw2024 %>%
  mutate(
    barri         = as.character(nom_barri),
    year          = as.integer(substr(data_referencia, 1, 4)),
    pop_age_group = as.integer(edat_q),
    value         = as.numeric(valor)
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(year, barri, pop_age_group, value) %>%
  group_by(barri, year, pop_age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

pop_age_group_wide2024 <- pop_age_clean2024 %>%
  pivot_wider(
    names_from   = pop_age_group,
    values_from  = value,
    names_prefix = "pop_age_",
    values_fill  = list(value = 0)
  )
# View(pop_age_group_wide2024)

# binding all rows
pop_age_group_all <- bind_rows(
  pop_age_group_wide2013,
  pop_age_group_wide2014,
  pop_age_group_wide2015,
  pop_age_group_wide2016,
  pop_age_group_wide2017,
  pop_age_group_wide2018,
  pop_age_group_wide2019,
  pop_age_group_wide2020,
  pop_age_group_wide2021,
  pop_age_group_wide2022,
  pop_age_group_wide2023,
  pop_age_group_wide2024
)
# View(pop_age_group_all)

# POPULATION BY NATIONALITY
# pop nat 2013
pop_nat_raw2013 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2013.csv') %>%
  clean_names()
names(pop_nat_raw2013)
# View(pop_nat_raw2013)

pop_nat_clean2013 <-pop_nat_raw2013 %>%
  select(
    barri = nom_barri, 
    year = data_referencia, 
    value = valor, 
    sex = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%     # <-- THIS REMOVES THE NA CATEGORY ENTIRELY
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2013)

# pop nat 2014
pop_nat_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2014.csv') %>%
  clean_names()
names(pop_nat_raw2014)
# View(pop_nat_raw2014)

pop_nat_clean2014 <- pop_nat_raw2014 %>%
  select(
    barri       = nom_barri, 
    year        = data_referencia, 
    value       = valor, 
    sex         = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2014)

# pop nat 2015
pop_nat_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2015.csv') %>%
  clean_names()
names(pop_nat_raw2015)
# View(pop_nat_raw2015)

pop_nat_clean2015 <- pop_nat_raw2015 %>%
  select(
    barri       = nom_barri, 
    year        = data_referencia, 
    value       = valor, 
    sex         = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2015)

# pop nat 2016
pop_nat_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2016.csv') %>%
  clean_names()
names(pop_nat_raw2016)
# View(pop_nat_raw2016)

pop_nat_clean2016 <- pop_nat_raw2016 %>%
  select(
    barri       = nom_barri, 
    year        = data_referencia, 
    value       = valor, 
    sex         = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2016)

# pop nat 2017
pop_nat_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2017.csv') %>%
  clean_names()
names(pop_nat_raw2017)
# View(pop_nat_raw2017)

pop_nat_clean2017 <- pop_nat_raw2017 %>%
  select(
    barri       = nom_barri, 
    year        = data_referencia, 
    value       = valor, 
    sex         = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2017)

# pop nat 2018
pop_nat_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2018.csv') %>%
  clean_names()
names(pop_nat_raw2018)
# View(pop_nat_raw2018)

pop_nat_clean2018 <- pop_nat_raw2018 %>%
  select(
    barri       = nom_barri, 
    year        = data_referencia, 
    value       = valor, 
    sex         = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2018)

# pop nat 2019
pop_nat_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2019.csv') %>%
  clean_names()
names(pop_nat_raw2019)
# View(pop_nat_raw2019)

pop_nat_clean2019 <- pop_nat_raw2019 %>%
  select(
    barri       = nom_barri, 
    year        = data_referencia, 
    value       = valor, 
    sex         = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2019)

# pop nat 2020
pop_nat_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2020.csv') %>%
  clean_names()
names(pop_nat_raw2020)
# View(pop_nat_raw2020)

pop_nat_clean2020 <- pop_nat_raw2020 %>%
  select(
    barri       = nom_barri, 
    year        = data_referencia, 
    value       = valor, 
    sex         = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2020)

# pop nat 2021
pop_nat_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2021.csv') %>%
  clean_names()
names(pop_nat_raw2021)
# View(pop_nat_raw2021)

pop_nat_clean2021 <- pop_nat_raw2021 %>%
  select(
    barri       = nom_barri, 
    year        = data_referencia, 
    value       = valor, 
    sex         = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2021)

# pop nat 2022
pop_nat_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2022.csv') %>%
  clean_names()
names(pop_nat_raw2022)
# View(pop_nat_raw2022)

pop_nat_clean2022 <- pop_nat_raw2022 %>%
  select(
    barri       = nom_barri, 
    year        = data_referencia, 
    value       = valor, 
    sex         = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2022)

# pop nat 2023
pop_nat_raw2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2023.csv') %>%
  clean_names()
names(pop_nat_raw2023)
# View(pop_nat_raw2023)

pop_nat_clean2023 <- pop_nat_raw2023 %>%
  select(
    barri       = nom_barri, 
    year        = data_referencia, 
    value       = valor, 
    sex         = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2023)

# pop nat 2024
pop_nat_raw2024 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_nationality:sex _ 2024.csv') %>%
  clean_names()
names(pop_nat_raw2024)
# View(pop_nat_raw2024)

pop_nat_clean2024 <- pop_nat_raw2024 %>%
  select(
    barri       = nom_barri, 
    year        = data_referencia, 
    value       = valor, 
    sex         = sexe,
    cat_nat_raw = nacionalitat_g
  ) %>%
  mutate(
    barri  = as.character(barri),
    year   = as.integer(substr(year, 1, 4)),
    value  = as.numeric(value),
    cat_nat = case_when(
      cat_nat_raw %in% c("Espanya", "España", "1")                  ~ "Spain",
      cat_nat_raw %in% c("Unió Europea", "UE", "2")                 ~ "EU",
      cat_nat_raw %in% c("Resta del món", "Resto del mundo", "3")   ~ "RestWorld",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cat_nat)) %>%
  group_by(barri, year, cat_nat) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = cat_nat,
    values_from  = value,
    names_prefix = "pop_nat_",
    values_fill  = list(value = 0)
  )
# View(pop_nat_clean2024)

# binding all years
pop_nat_all <- bind_rows(
  pop_nat_clean2013,
  pop_nat_clean2014,
  pop_nat_clean2015,
  pop_nat_clean2016,
  pop_nat_clean2017,
  pop_nat_clean2018,
  pop_nat_clean2019,
  pop_nat_clean2020,
  pop_nat_clean2021,
  pop_nat_clean2022,
  pop_nat_clean2023,
  pop_nat_clean2024
)
# View(pop_nat_all)

# POPULATION BY SEX
# pop sex 2013
pop_sex_raw2013 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2013.csv') %>% 
  clean_names()
names(pop_sex_raw2013)
# View(pop_sex_raw2013)

pop_sex_clean2013 <- pop_sex_raw2013 %>%
  select(
    barri      = nom_barri,
    year       = data_referencia,
    sex_raw    = sexe,
    value      = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1") ~ "male",
      sex_raw %in% c("Dones", "D", "2") ~ "female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%              # drop any weird codes
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )
# View(pop_sex_clean2013)

# pop sex 2014
pop_sex_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2014.csv') %>% 
  clean_names()
names(pop_sex_raw2014)
# View(pop_sex_raw2014)

pop_sex_clean2014 <- pop_sex_raw2014 %>%
  select(
    barri   = nom_barri,
    year    = data_referencia,
    sex_raw = sexe,
    value   = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1")  ~ "male",
      sex_raw %in% c("Dones", "D", "2")  ~ "female",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )

# View(pop_sex_clean2014)

# pop sex 2015
pop_sex_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2015.csv') %>% 
  clean_names()
names(pop_sex_raw2015)
# View(pop_sex_raw2015)

pop_sex_clean2015 <- pop_sex_raw2015 %>%
  select(
    barri   = nom_barri,
    year    = data_referencia,
    sex_raw = sexe,
    value   = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1")  ~ "male",
      sex_raw %in% c("Dones", "D", "2")  ~ "female",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )

# View(pop_sex_clean2015)

# pop sex 2016
pop_sex_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2016.csv') %>% 
  clean_names()
names(pop_sex_raw2016)
# View(pop_sex_raw2016)

pop_sex_clean2016 <- pop_sex_raw2016 %>%
  select(
    barri   = nom_barri,
    year    = data_referencia,
    sex_raw = sexe,
    value   = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1")  ~ "male",
      sex_raw %in% c("Dones", "D", "2")  ~ "female",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )

# View(pop_sex_clean2016)

# pop sex 2017
pop_sex_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2017.csv') %>% 
  clean_names()
names(pop_sex_raw2017)
# View(pop_sex_raw2017)

pop_sex_clean2017 <- pop_sex_raw2017 %>%
  select(
    barri   = nom_barri,
    year    = data_referencia,
    sex_raw = sexe,
    value   = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1")  ~ "male",
      sex_raw %in% c("Dones", "D", "2")  ~ "female",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )

# View(pop_sex_clean2017)

# pop sex 2018
pop_sex_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2018.csv') %>% 
  clean_names()
names(pop_sex_raw2018)
# View(pop_sex_raw2018)

pop_sex_clean2018 <- pop_sex_raw2018 %>%
  select(
    barri   = nom_barri,
    year    = data_referencia,
    sex_raw = sexe,
    value   = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1")  ~ "male",
      sex_raw %in% c("Dones", "D", "2")  ~ "female",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )

# View(pop_sex_clean2018)

# pop sex 2019
pop_sex_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2019.csv') %>% 
  clean_names()
names(pop_sex_raw2019)
# View(pop_sex_raw2019)

pop_sex_clean2019 <- pop_sex_raw2019 %>%
  select(
    barri   = nom_barri,
    year    = data_referencia,
    sex_raw = sexe,
    value   = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1")  ~ "male",
      sex_raw %in% c("Dones", "D", "2")  ~ "female",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )

# View(pop_sex_clean2019)

# pop sex 2020
pop_sex_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2020.csv') %>% 
  clean_names()
names(pop_sex_raw2020)
# View(pop_sex_raw2020)

pop_sex_clean2020 <- pop_sex_raw2020 %>%
  select(
    barri   = nom_barri,
    year    = data_referencia,
    sex_raw = sexe,
    value   = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1")  ~ "male",
      sex_raw %in% c("Dones", "D", "2")  ~ "female",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )

# View(pop_sex_clean2020)

# pop sex 2021
pop_sex_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2021.csv') %>% 
  clean_names()
names(pop_sex_raw2021)
# View(pop_sex_raw2021)

pop_sex_clean2021 <- pop_sex_raw2021 %>%
  select(
    barri   = nom_barri,
    year    = data_referencia,
    sex_raw = sexe,
    value   = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1")  ~ "male",
      sex_raw %in% c("Dones", "D", "2")  ~ "female",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )

# View(pop_sex_clean2021)

# pop sex 2022
pop_sex_raw2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2022.csv') %>% 
  clean_names()
names(pop_sex_raw2022)
# View(pop_sex_raw2022)

pop_sex_clean2022 <- pop_sex_raw2022 %>%
  select(
    barri   = nom_barri,
    year    = data_referencia,
    sex_raw = sexe,
    value   = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1")  ~ "male",
      sex_raw %in% c("Dones", "D", "2")  ~ "female",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )

# View(pop_sex_clean2022)

# pop sex 2023
pop_sex_raw2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2023.csv') %>% 
  clean_names()
names(pop_sex_raw2023)
# View(pop_sex_raw2023)

pop_sex_clean2023 <- pop_sex_raw2023 %>%
  select(
    barri   = nom_barri,
    year    = data_referencia,
    sex_raw = sexe,
    value   = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1")  ~ "male",
      sex_raw %in% c("Dones", "D", "2")  ~ "female",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )

# View(pop_sex_clean2023)

# pop sex 2024
pop_sex_raw2024 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_by_sex_2024.csv') %>% 
  clean_names()
names(pop_sex_raw2024)
# View(pop_sex_raw2024)

pop_sex_clean2024 <- pop_sex_raw2024 %>%
  select(
    barri   = nom_barri,
    year    = data_referencia,
    sex_raw = sexe,
    value   = valor
  ) %>%
  mutate(
    barri = as.character(barri),
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    sex = case_when(
      sex_raw %in% c("Homes", "H", "1")  ~ "male",
      sex_raw %in% c("Dones", "D", "2")  ~ "female",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(sex)) %>%
  group_by(barri, year, sex) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = sex,
    values_from  = value,
    names_prefix = "pop_",
    values_fill  = list(value = 0)
  )

# View(pop_sex_clean2024)

# binding all years
pop_sex_all <- bind_rows(
  pop_sex_clean2013,
  pop_sex_clean2014,
  pop_sex_clean2015,
  pop_sex_clean2016,
  pop_sex_clean2017,
  pop_sex_clean2018,
  pop_sex_clean2019,
  pop_sex_clean2020,
  pop_sex_clean2021,
  pop_sex_clean2022,
  pop_sex_clean2023,
  pop_sex_clean2024
)
# View(pop_sex_all)

# POPULATION DENSITY
# pop density 2013
pop_dens_raw2013 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_density_2013.csv') %>%
  clean_names()
names(pop_dens_raw2013)
# View(pop_dens_raw2013)

pop_dens_clean2013 <- pop_dens_raw2013 %>%
  select(
    year       = any,
    barri      = nom_barri,
    dens_gross = densitat_hab_ha,
    dens_net   = densitat_neta_hab_ha
  ) %>%
  mutate(
    year       = as.integer(year),
    dens_gross = as.numeric(dens_gross),
    dens_net   = as.numeric(dens_net)
  )

# View(pop_dens_clean2013)

# pop density 2014
pop_dens_raw2014 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_density_2014.csv') %>% clean_names()
names(pop_dens_raw2014)
# View(pop_dens_raw2014)

pop_dens_clean2014 <- pop_dens_raw2014 %>%
  select(
    year       = any,
    barri      = nom_barri,
    dens_gross = densitat_hab_ha,
    dens_net   = densitat_neta_hab_ha
  ) %>%
  mutate(
    year       = as.integer(year),
    dens_gross = as.numeric(dens_gross),
    dens_net   = as.numeric(dens_net)
  )
# View(pop_dens_clean2014)

# pop density 2015
pop_dens_raw2015 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_density_2015.csv') %>% clean_names()
names(pop_dens_raw2015)
# View(pop_dens_raw2015)

pop_dens_clean2015 <- pop_dens_raw2015 %>%
  select(
    year       = any,
    barri      = nom_barri,
    dens_gross = densitat_hab_ha,
    dens_net   = densitat_neta_hab_ha
  ) %>%
  mutate(
    year       = as.integer(year),
    dens_gross = as.numeric(dens_gross),
    dens_net   = as.numeric(dens_net)
  )
# View(pop_dens_clean2015)

# pop density 2016
pop_dens_raw2016 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_density_2016.csv') %>% clean_names()
names(pop_dens_raw2016)
# View(pop_dens_raw2016)

pop_dens_clean2016 <- pop_dens_raw2016 %>%
  select(
    year       = any,
    barri      = nom_barri,
    dens_gross = densitat_hab_ha,
    dens_net   = densitat_neta_hab_ha
  ) %>%
  mutate(
    year       = as.integer(year),
    dens_gross = as.numeric(dens_gross),
    dens_net   = as.numeric(dens_net)
  )
# View(pop_dens_clean2016)

# pop density 2017
pop_dens_raw2017 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_density_2017.csv') %>% clean_names()
names(pop_dens_raw2017)
# View(pop_dens_raw2017)

pop_dens_clean2017 <- pop_dens_raw2017 %>%
  select(
    year       = any,
    barri      = nom_barri,
    dens_gross = densitat_hab_ha,
    dens_net   = densitat_neta_hab_ha
  ) %>%
  mutate(
    year       = as.integer(year),
    dens_gross = as.numeric(dens_gross),
    dens_net   = as.numeric(dens_net)
  )
# View(pop_dens_clean2017)

# pop density 2018
pop_dens_raw2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_density_2018.csv') %>% clean_names()
names(pop_dens_raw2018)
# View(pop_dens_raw2018)

pop_dens_clean2018 <- pop_dens_raw2018 %>%
  select(
    year       = any,
    barri      = nom_barri,
    dens_gross = densitat_hab_ha,
    dens_net   = densitat_neta_hab_ha
  ) %>%
  mutate(
    year       = as.integer(year),
    dens_gross = as.numeric(dens_gross),
    dens_net   = as.numeric(dens_net)
  )
# View(pop_dens_clean2018)

# pop density 2019
pop_dens_raw2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_density_2019.csv') %>% clean_names()
names(pop_dens_raw2019)
# View(pop_dens_raw2019)

pop_dens_clean2019 <- pop_dens_raw2019 %>%
  select(
    year       = any,
    barri      = nom_barri,
    dens_gross = densitat_hab_ha,
    dens_net   = densitat_neta_hab_ha
  ) %>%
  mutate(
    year       = as.integer(year),
    dens_gross = as.numeric(dens_gross),
    dens_net   = as.numeric(dens_net)
  )
# View(pop_dens_clean2019)

# pop density 2020
pop_dens_raw2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_density_2020.csv') %>% clean_names()
names(pop_dens_raw2020)
# View(pop_dens_raw2020)

pop_dens_clean2020 <- pop_dens_raw2020 %>%
  select(
    year       = any,
    barri      = nom_barri,
    dens_gross = densitat_hab_ha,
    dens_net   = densitat_neta_hab_ha
  ) %>%
  mutate(
    year       = as.integer(year),
    dens_gross = as.numeric(dens_gross),
    dens_net   = as.numeric(dens_net)
  )
# View(pop_dens_clean2020)

# pop density 2021
pop_dens_raw2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/population_density_2021.csv') %>% clean_names()
names(pop_dens_raw2021)
# View(pop_dens_raw2021)

pop_dens_clean2021 <- pop_dens_raw2021 %>%
  select(
    year       = any,
    barri      = nom_barri,
    dens_gross = densitat_hab_ha,
    dens_net   = densitat_neta_hab_ha
  ) %>%
  mutate(
    year       = as.integer(year),
    dens_gross = as.numeric(dens_gross),
    dens_net   = as.numeric(dens_net)
  )
# View(pop_dens_clean2021)

# bind all years 2013–2021
pop_dens_all <- bind_rows(
  pop_dens_clean2013,
  pop_dens_clean2014,
  pop_dens_clean2015,
  pop_dens_clean2016,
  pop_dens_clean2017,
  pop_dens_clean2018,
  pop_dens_clean2019,
  pop_dens_clean2020,
  pop_dens_clean2021
)
# View(pop_dens_all)

# CHECKING CONSISTENCY OF NEIGHBORHOOD NAMES
normalize_barri <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^[:alnum:]]+", " ") %>%  # remove hyphens, apostrophes, etc.
    str_squish()
}

# reference list of barris (from your main panel)
barris_ref <- pop_all %>%               
  distinct(barri) %>%
  mutate(barri_norm = normalize_barri(barri))

# names from the raw tourist file for 2018 (before any join!)
tourist_barris_2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/tourist housing units_2018(3).csv') %>%   # tourist 2018 Q3 file
  clean_names() %>%
  distinct(barri) %>%
  mutate(barri_norm = normalize_barri(barri))

# barris that appear in tourist but not in reference (after normalization)
anti_join(tourist_barris_2018, barris_ref, by = "barri_norm")

# barris that appear in reference but not in tourist
anti_join(barris_ref, tourist_barris_2018, by = "barri_norm")

library(readr)
library(stringr)

# TOURIST HOUSING UNITS
all_barris <- unique(pop_all$barri)
# tourist housing 2018
tourist_2018 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/tourist housing units_2018(3).csv', show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    barri = str_squish(barri),
    barri = case_when(
      str_to_lower(barri) %in% c("el poble sec", "poble sec") ~ "el Poble-sec",
      str_starts(str_to_lower(barri), "sant pere")            ~
        "Sant Pere, Santa Caterina i la Ribera",
      TRUE ~ barri
    )
  ) %>%
  group_by(barri) %>%
  summarise(tourist_units = n_distinct(n_expedient), .groups = "drop") %>%
  mutate(year = 2018) %>%
  select(year, barri, tourist_units) %>%
  right_join(tibble(barri = all_barris), by = "barri") %>%
  mutate(
    year = 2018,
    tourist_units = replace_na(tourist_units, 0)
  )
# View(tourist_2018)

# tourist housing 2019
tourist_2019 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/tourist housing units_2019(3).csv') %>%
  clean_names() %>%
  mutate(
    barri = str_squish(barri),
    barri = case_when(
      str_to_lower(barri) %in% c("el poble sec", "poble sec") ~ "el Poble-sec",
      str_starts(str_to_lower(barri), "sant pere")            ~
        "Sant Pere, Santa Caterina i la Ribera",
      TRUE ~ barri
    )
  ) %>%
  group_by(barri) %>%
  summarise(tourist_units = n_distinct(n_expedient), .groups = "drop") %>%
  mutate(year = 2019) %>%
  select(year, barri, tourist_units) %>%
  right_join(tibble(barri = all_barris), by = "barri") %>%
  mutate(
    year = 2019,
    tourist_units = replace_na(tourist_units, 0)
  )
# View(tourist_2019)

# tourist housing 2020
tourist_2020 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/tourist housing units_2020(3).csv') %>%
  clean_names() %>%
  mutate(
    barri = str_squish(barri),
    barri = case_when(
      str_to_lower(barri) %in% c("el poble sec", "poble sec") ~ "el Poble-sec",
      str_starts(str_to_lower(barri), "sant pere")            ~
        "Sant Pere, Santa Caterina i la Ribera",
      TRUE ~ barri
    )
  ) %>%
  group_by(barri) %>%
  summarise(tourist_units = n_distinct(n_expedient), .groups = "drop") %>%
  mutate(year = 2020) %>%
  select(year, barri, tourist_units) %>%
  right_join(tibble(barri = all_barris), by = "barri") %>%
  mutate(
    year = 2020,
    tourist_units = replace_na(tourist_units, 0)
  )
# View(tourist_2020)

# tourist housing 2021
tourist_2021 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/tourist housing units_2021(3).csv') %>%
  clean_names() %>%
  mutate(
    barri = str_squish(barri),
    barri = case_when(
      str_to_lower(barri) %in% c("el poble sec", "poble sec") ~ "el Poble-sec",
      str_starts(str_to_lower(barri), "sant pere")            ~
        "Sant Pere, Santa Caterina i la Ribera",
      TRUE ~ barri
    )
  ) %>%
  group_by(barri) %>%
  summarise(tourist_units = n_distinct(n_expedient), .groups = "drop") %>%
  mutate(year = 2021) %>%
  select(year, barri, tourist_units) %>%
  right_join(tibble(barri = all_barris), by = "barri") %>%
  mutate(
    year = 2021,
    tourist_units = replace_na(tourist_units, 0)
  )
# View(tourist_2021)

# tourist housing 2022
tourist_2022 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/tourist housing units_2022(3).csv') %>%
  clean_names() %>%
  mutate(
    barri = str_squish(barri),
    barri = case_when(
      str_to_lower(barri) %in% c("el poble sec", "poble sec") ~ "el Poble-sec",
      str_starts(str_to_lower(barri), "sant pere")            ~
        "Sant Pere, Santa Caterina i la Ribera",
      TRUE ~ barri
    )
  ) %>%
  group_by(barri) %>%
  summarise(tourist_units = n_distinct(n_expedient), .groups = "drop") %>%
  mutate(year = 2022) %>%
  select(year, barri, tourist_units) %>%
  right_join(tibble(barri = all_barris), by = "barri") %>%
  mutate(
    year = 2022,
    tourist_units = replace_na(tourist_units, 0)
  )
# View(tourist_2022)

# tourist housing 2023
tourist_2023 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/tourist housing units_2023(3).csv') %>%
  clean_names() %>%
  mutate(
    barri = str_squish(barri),
    barri = case_when(
      str_to_lower(barri) %in% c("el poble sec", "poble sec") ~ "el Poble-sec",
      str_starts(str_to_lower(barri), "sant pere")            ~
        "Sant Pere, Santa Caterina i la Ribera",
      TRUE ~ barri
    )
  ) %>%
  group_by(barri) %>%
  summarise(tourist_units = n_distinct(n_expedient), .groups = "drop") %>%
  mutate(year = 2023) %>%
  select(year, barri, tourist_units) %>%
  right_join(tibble(barri = all_barris), by = "barri") %>%
  mutate(
    year = 2023,
    tourist_units = replace_na(tourist_units, 0)
  )
# View(tourist_2023)

# tourist housing 2024
tourist_2024 <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/tourist housing units_2024(3).csv') %>%
  clean_names() %>%
  mutate(
    barri = str_squish(nom_barri),
    barri = case_when(
      str_to_lower(barri) %in% c("el poble sec", "poble sec") ~ "el Poble-sec",
      str_starts(str_to_lower(barri), "sant pere")            ~
        "Sant Pere, Santa Caterina i la Ribera",
      TRUE ~ barri
    )
  ) %>%
  group_by(barri) %>%
  summarise(tourist_units = n_distinct(n_expedient), .groups = "drop") %>%
  mutate(year = 2024) %>%
  select(year, barri, tourist_units) %>%
  right_join(tibble(barri = all_barris), by = "barri") %>%
  mutate(
    year = 2024,
    tourist_units = replace_na(tourist_units, 0)
  )
# View(tourist_2024)

# bind all years
tourist_units_all <- bind_rows(
  tourist_2018,
  tourist_2019,
  tourist_2020,
  tourist_2021,
  tourist_2022,
  tourist_2023,
  tourist_2024
)

# View(tourist_units_all)

# UNEMPLOYMENT, 2013-2024
unemp_raw <- read_csv('/Users/aliaydinkara/Desktop/Documents/BSE/Fall term/Data Science/Final project/data_raw:/unemployment_monthly_2011-2025.csv',
                      show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(across(contains("2015"), ~as.numeric(.))) 
names(unemp_raw)
# View(unemp_raw)

# keep only true barris
unemp_barris <- unemp_raw %>%
  filter(tipus_de_territori == "Barri")

# fix glitch in 2015 data
sapply(unemp_raw %>% select(contains("2015")), class)

# pivot all month columns long ----
unemp_long <- unemp_barris %>%
  pivot_longer(
    cols = where(is.numeric),      # <- only the month columns
    names_to = "month_raw",
    values_to = "unemp"
  )

# Count observations by year in month_raw
unemp_long %>%
  mutate(year_check = str_extract(month_raw, "\\d{4}")) %>%
  count(year_check) %>%
  arrange(year_check)

# extract YEAR from the month column names ----
unemp_long <- unemp_long %>%
  mutate(
    barri = as.character(territori),
    # extract last 4 digits = year
    year = as.integer(str_extract(month_raw, "\\d{4}")),
    unemp = as.numeric(unemp)
  ) %>%
  filter(!is.na(year))   # drop weird NA rows

# Check what years are being extracted
unemp_long %>%
  distinct(month_raw, year) %>%
  filter(year == 2015) %>%
  head(20)

# compute yearly averages ----
unemp_yearly <- unemp_long %>%
  group_by(barri, year) %>%
  summarise(unemp_avg = mean(unemp, na.rm = TRUE), .groups = "drop") %>%
  arrange(barri, year)

# keep only 2013–2024
unemp_yearly <- unemp_yearly %>%
  filter(year >= 2013, year <= 2024)

View(unemp_yearly)
# ===========================
#   CREATE FULL DATA PANEL
# ===========================
# Standardization function for neighborhood names
standardize_barri <- function(name) {
  case_when(
    str_detect(name, regex("Poble Sec", ignore_case = TRUE)) ~ "el Poble-sec",
    str_detect(name, "Sant Gervasi- Galvany") ~ "Sant Gervasi - Galvany",  # Add space after hyphen
    str_detect(name, "Sant Gervasi- la Bonanova") ~ "Sant Gervasi - la Bonanova",
    str_detect(name, "Sants-Badal") ~ "Sants - Badal",  # Add spaces around hyphen
    str_detect(name, "la Marina del Prat Vermell") ~ "la Marina del Prat Vermell",  # Strip the AEI suffix
    TRUE ~ name
  )
}

# Apply to EVERY dataset before joining
rental_clean <- rental_clean %>% mutate(barri = standardize_barri(barri))
address_change_all <- address_change_all %>% mutate(barri = standardize_barri(barri))
avginc_all <- avginc_all %>% mutate(barri = standardize_barri(barri))
births_all <- births_all %>% mutate(barri = standardize_barri(barri))
contracts_clean <- contracts_clean %>% mutate(barri = standardize_barri(barri))
deaths_all <- deaths_all %>% mutate(barri = standardize_barri(barri))
disp_fam_inc_clean2022 <- disp_fam_inc_clean2022 %>% mutate(barri = standardize_barri(barri))
edu_all <- edu_all %>% mutate(barri = standardize_barri(barri))
emigration_all <- emigration_all %>% mutate(barri = standardize_barri(barri))
gini_all <- gini_all %>% mutate(barri = standardize_barri(barri))
household_nat_all <- household_nat_all %>% mutate(barri = standardize_barri(barri))
immigration_all <- immigration_all %>% mutate(barri = standardize_barri(barri))
pop_age_group_all <- pop_age_group_all %>% mutate(barri = standardize_barri(barri))
pop_all <- pop_all %>% mutate(barri = standardize_barri(barri))
pop_dens_all <- pop_dens_all %>% mutate(barri = standardize_barri(barri))
pop_nat_all <- pop_nat_all %>% mutate(barri = standardize_barri(barri))
pop_sex_all <- pop_sex_all %>% mutate(barri = standardize_barri(barri))
tourist_units_all <- tourist_units_all %>% mutate(barri = standardize_barri(barri))
unemp_yearly <- unemp_yearly %>% mutate(barri = standardize_barri(barri))

# CHECK: Get all unique barri names from each dataset
all_datasets <- list(
  rental_clean, address_change_all, avginc_all, births_all, contracts_clean,
  deaths_all, disp_fam_inc_clean2022, edu_all, emigration_all, gini_all,
  household_nat_all, immigration_all, pop_age_group_all, pop_all, pop_dens_all,
  pop_nat_all, pop_sex_all, tourist_units_all, unemp_yearly
)

all_barri_names <- unique(unlist(lapply(all_datasets, function(df) unique(df$barri))))

# Compare to pop_all (your backbone)
setdiff(all_barri_names, unique(pop_all$barri))  # Names in other datasets but not in pop_all
setdiff(unique(pop_all$barri), all_barri_names)  # Names in pop_all but not in others

library(dplyr)
# Create the master backbone of barri × year
master_panel <- expand.grid(
  barri = unique(pop_all$barri),    # The most complete list of barris
  year  = 2013:2024
) %>% 
  arrange(barri, year)

# Join all cleaned datasets one by one
master_panel <- master_panel %>%
  left_join(rental_clean,            by = c("barri", "year")) %>%
  left_join(address_change_all,      by = c("barri", "year")) %>%
  left_join(avginc_all,              by = c("barri", "year")) %>%
  left_join(births_all,              by = c("barri", "year")) %>%
  left_join(contracts_clean,         by = c("barri", "year")) %>%
  left_join(deaths_all,              by = c("barri", "year")) %>%
  left_join(disp_fam_inc_clean2022,  by = c("barri", "year")) %>%  # only 2022
  left_join(edu_all,                 by = c("barri", "year")) %>%
  left_join(emigration_all,          by = c("barri", "year")) %>%
  left_join(gini_all,                by = c("barri", "year")) %>%
  left_join(household_nat_all,       by = c("barri", "year")) %>%
  left_join(immigration_all,         by = c("barri", "year")) %>%
  left_join(pop_age_group_all,       by = c("barri", "year")) %>%
  left_join(pop_all,                 by = c("barri", "year")) %>%
  left_join(pop_dens_all,            by = c("barri", "year")) %>%
  left_join(pop_nat_all,             by = c("barri", "year")) %>%
  left_join(pop_sex_all,             by = c("barri", "year")) %>%
  left_join(tourist_units_all,       by = c("barri", "year")) %>%
  left_join(unemp_yearly,            by = c("barri", "year")) %>%
  left_join(cpi_yearly_clean,        by = "year") %>%
  left_join(housing_sales_all,       by = "year")

# Aggregate age groups into broader categories
master_panel <- master_panel %>%
  mutate(
    pop_age_0_18  = rowSums(select(., pop_age_0:pop_age_3), na.rm = TRUE),   # 0-18
    pop_age_19_35 = rowSums(select(., pop_age_4:pop_age_7), na.rm = TRUE),   # 19-35
    pop_age_36_64 = rowSums(select(., pop_age_8:pop_age_13), na.rm = TRUE),  # 36-64
    pop_age_65plus = rowSums(select(., pop_age_14:pop_age_20), na.rm = TRUE) # 65+
  ) %>%
  select(-c(pop_age_0:pop_age_21))  # Drop the original 22 age columns

master_panel <- master_panel %>%
  rename(barri = barri.x)

master_panel <- master_panel %>% select(-barri.y)

# Find barris with ALL years missing rent
barris_to_drop <- master_panel %>%
  group_by(barri) %>%
  summarise(all_missing = all(is.na(rent))) %>%
  filter(all_missing) %>%
  pull(barri)

# Drop only those barris
master_panel <- master_panel %>%
  filter(!barri %in% barris_to_drop)

View(master_panel)
# =========================================================
# DATA CLEANING $ CHECKING FOR VERY HIGH MULTICOLLINEARITY
# =========================================================
library(dplyr)
library(corrplot)

# Use data from 2015-2023 (your best coverage period)
corr_data <- master_panel %>%
  filter(year %in% 2015:2023) %>%
  select(where(is.numeric)) %>%
  select(-year)

# Compute correlation using PAIRWISE complete observations
# Calculates each correlation using only rows where BOTH variables are present
corr_mat <- cor(corr_data, use = "pairwise.complete.obs")

# Plot 1: Full correlation matrix (no clustering, includes all variables)
corrplot(corr_mat, 
         method = "color", 
         type = "lower", 
         tl.cex = 0.5, 
         tl.col = "black",
         title = "Full Correlation Matrix (2015-2023)",
         mar = c(0,0,2,0),
         na.label = " ")

# Find high correlations (>0.85)
high_corr_indices <- which(abs(corr_mat) > 0.85 & corr_mat != 1, arr.ind = TRUE)
high_corr_pairs <- data.frame(
  var1 = rownames(corr_mat)[high_corr_indices[,1]],
  var2 = colnames(corr_mat)[high_corr_indices[,2]],
  correlation = corr_mat[high_corr_indices]
)
print(high_corr_pairs)

# Check correlations between household and population nationality variables
hh_pop_cors <- corr_mat[
  c("hh_nat_Spain", "hh_nat_EU", "hh_nat_RestWorld"),
  c("pop_nat_Spain", "pop_nat_EU", "pop_nat_RestWorld")
]
print(hh_pop_cors)

# Drop highly correlated variables and creating pct_female variable
master_panel <- master_panel %>%
  select(-disp_fam_inc, -hh_nat_Spain) %>%
  mutate(pct_female = pop_female / population * 100) %>%
  select(-pop_female, -pop_male)

# Plotting rent trends
rental_long %>%
  group_by(year) %>%
  summarise(avg_rent = mean(rent, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_rent)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1200), expand = c(0, 0)) +
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021, 2023), limits = c(2013, 2024))+
  labs(title = "Average Rent Prices in Barcelona\n(2013-2024)",
       x = "Year", y = "Average Rent (€)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Join rent trends with CPI
rental_long %>%
  group_by(year) %>%
  summarise(avg_rent = mean(rent, na.rm = TRUE)) %>%
  left_join(cpi_yearly_clean, by = "year") %>%
  mutate(
    rent_indexed = (avg_rent / first(avg_rent)) * 100,
    cpi_indexed = (cpi_index / first(cpi_index)) * 100
  ) %>%
  pivot_longer(cols = c(rent_indexed, cpi_indexed), 
               names_to = "measure", values_to = "index") %>%
  ggplot(aes(x = year, y = index, color = measure)) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021, 2023), limits = c(2013, 2024))+
  labs(title = "Rent vs CPI Growth (Indexed to 2013)",
       x = "Year", y = "Index (2013 = 100)") +
  theme_minimal()

# ============================================
# SPLIT TESTING & TRAINING DATA FOR LINEAR REG
# ============================================
install.packages("rsample")
install.packages("recipes")
library(rsample)
library(recipes)

# Set seed for reproducibility
set.seed(123)

# Create train/test split (80/20)
# Use initial_time_split to respect temporal structure
data_split <- initial_time_split(
  master_panel %>% arrange(barri, year),
  prop = 0.8
)

train_data <- training(data_split)
test_data <- testing(data_split)

# Check the split
cat("Training observations:", nrow(train_data), "\n")
cat("Testing observations:", nrow(test_data), "\n")
cat("Training years:", min(train_data$year), "-", max(train_data$year), "\n")
cat("Testing years:", min(test_data$year), "-", max(test_data$year), "\n")

# Remove rows with missing rent (outcome) from both
train_data <- train_data %>% filter(!is.na(rent))
test_data <- test_data %>% filter(!is.na(rent))

cat("\nAfter removing missing rent:\n")
cat("Training observations:", nrow(train_data), "\n")
cat("Testing observations:", nrow(test_data), "\n")

# ==========================
#  RUNNING LINEAR REGRESSION
# ==========================
library(tidyverse)

# Prepare data - log transform rent
train_lm <- train_data %>%
  mutate(log_rent = log(rent)) %>%
  select(-barri, -rent)  # Remove ID and original rent

test_lm <- test_data %>%
  mutate(log_rent = log(rent)) %>%
  select(-barri, -rent)

# Check which columns have zero variance in train_lm
zero_var_cols <- sapply(train_lm, function(x) length(unique(na.omit(x))) <= 1)
names(train_lm)[zero_var_cols]

# Fit linear model
lm_model <- lm(log_rent ~ ., data = train_lm)

# Summary
summary(lm_model)

# Calculate training performance
train_pred_log <- predict(lm_model, newdata = train_lm)
train_pred <- exp(train_pred_log)
train_actual <- train_data$rent[!is.na(train_lm$log_rent)]

train_results <- data.frame(
  actual = train_actual,
  predicted = train_pred
) %>%
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    R2 = cor(actual, predicted, use = "complete.obs")^2
  )
print(train_results)

# Create training results table
train_results_table <- data.frame(
  Metric = c("RMSE (€)", "MAE (€)", "R²"),
  Value = c(
    round(train_results$RMSE, 2),
    round(train_results$MAE, 2),
    round(train_results$R2, 3)
  )
)

kable(train_results_table, 
      caption = "Linear Regression Performance on Training Set",
      align = c('l', 'r')) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE, color = "black") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  column_spec(1:2, bold = FALSE) %>%
  footnote(general = "RMSE and MAE in euros; R² represents variance explained",
           general_title = "")

# Predictions on test set (back-transform to original scale)
test_pred_log <- predict(lm_model, newdata = test_lm)
test_pred <- exp(test_pred_log)  # Convert back to euros
test_actual <- test_data$rent

# Calculate performance metrics
test_results <- data.frame(
  actual = test_actual,
  predicted = test_pred
) %>%
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    R2 = cor(actual, predicted, use = "complete.obs")^2
  )

print(test_results)

# Plot: Predicted vs Actual
ggplot(data.frame(actual = test_actual, predicted = test_pred), 
       aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Linear Regression: Predicted vs Actual Rent",
       x = "Actual Rent (€)", y = "Predicted Rent (€)") +
  theme_minimal()

# Plot: Residuals
residuals <- test_actual - test_pred
ggplot(data.frame(predicted = test_pred, residuals = residuals),
       aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual Plot", 
       x = "Predicted Rent (€)", y = "Residuals") +
  theme_minimal()

# Make table of results
install.packages("kableExtra")
library(knitr)
library(kableExtra)

# Create nice table
library(kableExtra)

results_table <- data.frame(
  Metric = c("RMSE (€)", "MAE (€)", "R²"),
  Value = c(
    round(test_results$RMSE, 2),
    round(test_results$MAE, 2),
    round(test_results$R2, 3)
  )
)

kable(results_table, 
      caption = "Linear Regression Performance on Test Set",
      align = c('l', 'r')) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE, color = "black") %>%  # Darker header
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  column_spec(1:2, bold = FALSE) %>%
  footnote(general = "RMSE and MAE in euros; R² represents variance explained",
           general_title = "")


# ======================================================
# SPLIT TESTING & TRAINING DATA FOR PENALIZED REG: LASSO
# ======================================================
library(rsample)

set.seed(123)

# First split: 60% train, 40% temp
initial_split <- initial_split(master_panel %>% arrange(barri, year), prop = 0.6)
train_data <- training(initial_split)
temp_data <- testing(initial_split)

# Second split: split temp into 20% validation, 20% test
val_test_split <- initial_split(temp_data, prop = 0.5)
val_data <- training(val_test_split)
test_data <- testing(val_test_split)

# Remove missing rent
train_data <- train_data %>% filter(!is.na(rent))
val_data <- val_data %>% filter(!is.na(rent))
test_data <- test_data %>% filter(!is.na(rent))

cat("Training:", nrow(train_data), "\n")
cat("Validation:", nrow(val_data), "\n")
cat("Testing:", nrow(test_data), "\n")

# =============
# RUNNING LASSO
# =============
library(glmnet)
library(tidyverse)

# Check which variables have most missing values
na_counts <- colSums(is.na(master_panel))
sort(na_counts, decreasing = TRUE)

# Drop the 6 worst variables for complete cases
master_panel_lasso <- master_panel %>%
  select(-tourist_units, -address_change_rate, -avg_inc, -gini_index, 
         -dens_gross, -dens_net)

# Redo train/val/test split
set.seed(123)
initial_split <- initial_split(master_panel_lasso %>% arrange(barri, year), prop = 0.6)
train_data <- training(initial_split)
temp_data <- testing(initial_split)

val_test_split <- initial_split(temp_data, prop = 0.5)
val_data <- training(val_test_split)
test_data <- testing(val_test_split)

# Remove missing rent
train_data <- train_data %>% filter(!is.na(rent))
val_data <- val_data %>% filter(!is.na(rent))
test_data <- test_data %>% filter(!is.na(rent))

# Prepare data for glmnet (log transform and remove complete cases)
train_lasso <- train_data %>%
  mutate(log_rent = log(rent)) %>%
  select(-barri, -rent) %>%
  na.omit()

val_lasso <- val_data %>%
  mutate(log_rent = log(rent)) %>%
  select(-barri, -rent) %>%
  na.omit()

test_lasso <- test_data %>%
  mutate(log_rent = log(rent)) %>%
  select(-barri, -rent) %>%
  na.omit()

cat("Training:", nrow(train_lasso), "| Validation:", nrow(val_lasso), "| Test:", nrow(test_lasso), "\n")

# Create matrices (glmnet requires matrix input)
x_train <- model.matrix(log_rent ~ ., data = train_lasso)[, -1]
y_train <- train_lasso$log_rent

x_val <- model.matrix(log_rent ~ ., data = val_lasso)[, -1]
y_val <- val_lasso$log_rent

x_test <- model.matrix(log_rent ~ ., data = test_lasso)[, -1]
y_test <- test_lasso$log_rent

# Fit LASSO with cross-validation
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)

# Plot lambda vs MSE
plot(lasso_model)

# Best lambda
best_lambda <- lasso_model$lambda.min
cat("Best lambda:", best_lambda, "\n")

# Validation performance
val_pred_log <- predict(lasso_model, s = best_lambda, newx = x_val)
val_pred <- exp(val_pred_log)
val_actual <- val_lasso$log_rent %>% exp()  # Back-transform

val_results <- data.frame(
  actual = val_actual,
  predicted = as.vector(val_pred)
) %>%
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
R2 = cor(actual, predicted, use = "complete.obs")^2
)

print("Validation Set Performance:")
print(val_results)

# Test performance
test_pred_log <- predict(lasso_model, s = best_lambda, newx = x_test)
test_pred <- exp(test_pred_log)
test_actual <- test_lasso$log_rent %>% exp()

test_results_lasso <- data.frame(
  actual = test_actual,
  predicted = as.vector(test_pred)
) %>%
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    R2 = cor(actual, predicted, use = "complete.obs")^2
  )

print("Test Set Performance:")
print(test_results_lasso)

# See which variables were kept (non-zero coefficients)
lasso_coef <- coef(lasso_model, s = best_lambda)
selected_vars <- lasso_coef[lasso_coef[,1] != 0, ]
print("Selected variables:")
print(selected_vars)

library(kableExtra)

# Combined validation and test results
lasso_results_table <- data.frame(
  Metric = c("RMSE (€)", "MAE (€)", "R²", "Variables Retained"),
  Validation = c(
    round(val_results$RMSE, 2),
    round(val_results$MAE, 2),
    round(val_results$R2, 3),
    "26/26"
  ),
  Test = c(
    round(test_results_lasso$RMSE, 2),
    round(test_results_lasso$MAE, 2),
    round(test_results_lasso$R2, 3),
    "—"
  )
)

kable(lasso_results_table, 
      caption = "LASSO Performance",
      align = c('l', 'r', 'r')) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE)

# ==================
# STRICTER LASSO (lambda.1se)
# ==================

# Fit with same data, use lambda.1se
lasso_strict <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)
best_lambda_strict <- lasso_strict$lambda.1se

cat("Strict lambda (1se):", best_lambda_strict, "\n")
cat("Original lambda (min):", best_lambda, "\n")

# Check which variables remain
lasso_coef_strict <- coef(lasso_strict, s = best_lambda_strict)
selected_vars_strict <- lasso_coef_strict[lasso_coef_strict[,1] != 0, ]
cat("Variables retained:", length(selected_vars_strict) - 1, "\n")  # -1 for intercept
print(selected_vars_strict)

# Validation performance
val_pred_strict_log <- predict(lasso_strict, s = best_lambda_strict, newx = x_val)
val_pred_strict <- exp(val_pred_strict_log)

val_results_strict <- data.frame(
  actual = val_actual,
  predicted = as.vector(val_pred_strict)
) %>%
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    R2 = cor(actual, predicted, use = "complete.obs")^2
  )

print("Validation (Strict LASSO):")
print(val_results_strict)

# Test performance
test_pred_strict_log <- predict(lasso_strict, s = best_lambda_strict, newx = x_test)
test_pred_strict <- exp(test_pred_strict_log)

test_results_strict <- data.frame(
  actual = test_actual,
  predicted = as.vector(test_pred_strict)
) %>%
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    R2 = cor(actual, predicted, use = "complete.obs")^2
  )

print("Test (Strict LASSO):")
print(test_results_strict)

# ==================
# RANDOM FOREST
# ==================
library(randomForest)

# Prepare data (RF handles NAs better, but let's be consistent)
train_rf <- train_data %>%
  mutate(log_rent = log(rent)) %>%
  select(-barri, -rent)

val_rf <- val_data %>%
  mutate(log_rent = log(rent)) %>%
  select(-barri, -rent)

test_rf <- test_data %>%
  mutate(log_rent = log(rent)) %>%
  select(-barri, -rent)

# Train Random Forest
set.seed(123)
rf_model <- randomForest(
  log_rent ~ .,
  data = train_rf,
  ntree = 500,
  mtry = floor(sqrt(ncol(train_rf) - 1)),  # Default for regression
  importance = TRUE,
  na.action = na.omit
)

print(rf_model)

# Validation predictions
val_pred_rf_log <- predict(rf_model, newdata = val_rf)
val_pred_rf <- exp(val_pred_rf_log)
val_actual_rf <- val_data$rent[!is.na(val_pred_rf_log)]

val_results_rf <- data.frame(
  actual = val_actual_rf,
  predicted = val_pred_rf[!is.na(val_pred_rf_log)]
) %>%
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    R2 = cor(actual, predicted, use = "complete.obs")^2
  )

print("Validation (Random Forest):")
print(val_results_rf)

# Test predictions
test_pred_rf_log <- predict(rf_model, newdata = test_rf)
test_pred_rf <- exp(test_pred_rf_log)
test_actual_rf <- test_data$rent[!is.na(test_pred_rf_log)]

test_results_rf <- data.frame(
  actual = test_actual_rf,
  predicted = test_pred_rf[!is.na(test_pred_rf_log)]
) %>%
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    R2 = cor(actual, predicted, use = "complete.obs")^2
  )

print("Test (Random Forest):")
print(test_results_rf)

# Variable importance plot
varImpPlot(rf_model, n.var = 15, main = "Top 15 Most Important Variables (Random Forest)")

# Create RF results table
rf_results_table <- data.frame(
  Metric = c("RMSE (€)", "MAE (€)", "R²"),
  Validation = c(
    round(val_results_rf$RMSE, 2),
    round(val_results_rf$MAE, 2),
    round(val_results_rf$R2, 3)
  ),
  Test = c(
    round(test_results_rf$RMSE, 2),
    round(test_results_rf$MAE, 2),
    round(test_results_rf$R2, 3)
  )
)

kable(rf_results_table, 
      caption = "Random Forest Performance",
      align = c('l', 'r', 'r')) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE)

# Variable importance plot with better formatting
varImpPlot(rf_model, 
           n.var = 15, 
           main = "Top 15 Most Important Variables (Random Forest)",
           cex = 0.8,  # Smaller text
           pch = 19,   # Solid circles
           col = "steelblue",  # Blue points
           bg = "lightblue")   # Light blue background for points

# Or use ggplot for more control
library(ggplot2)

# Extract importance
importance_df <- as.data.frame(importance(rf_model)) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(`%IncMSE`)) %>%
  slice(1:15)
ggplot(importance_df, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_point(color = "steelblue", size = 3) +
  geom_segment(aes(xend = Variable, yend = 0), color = "steelblue", size = 1) +
  coord_flip() +
  labs(title = "Top 15 Most Important Variables (Random Forest)",
       x = "", y = "% Increase in MSE") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

summary(lm_model)

# ==================
# RF WITH FULL DATASET
# ==================
library(randomForest)

# Use ORIGINAL master_panel (all variables)
set.seed(123)

# Same train/val/test split but from full master_panel
initial_split_full <- initial_split(master_panel %>% arrange(barri, year), prop = 0.6)
train_full <- training(initial_split_full) %>% filter(!is.na(rent))
temp_full <- testing(initial_split_full)

val_test_split_full <- initial_split(temp_full, prop = 0.5)
val_full <- training(val_test_split_full) %>% filter(!is.na(rent))
test_full <- testing(val_test_split_full) %>% filter(!is.na(rent))

# Prepare for RF
train_rf_full <- train_full %>%
  mutate(log_rent = log(rent)) %>%
  select(-barri, -rent)

test_rf_full <- test_full %>%
  mutate(log_rent = log(rent)) %>%
  select(-barri, -rent)

# Train RF with na.roughfix
rf_full <- randomForest(
  log_rent ~ .,
  data = train_rf_full,
  ntree = 500,
  importance = TRUE,
  na.action = na.roughfix  # Handle NAs
)

# Test predictions
test_pred_full_log <- predict(rf_full, newdata = test_rf_full)
test_pred_full <- exp(test_pred_full_log)
test_actual_full <- test_full$rent

test_results_full <- data.frame(
  actual = test_actual_full[1:length(test_pred_full)],
  predicted = test_pred_full
) %>%
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    R2 = cor(actual, predicted, use = "complete.obs")^2
  )

print("RF with ALL variables:")
print(test_results_full)

# Variable importance
varImpPlot(rf_full, n.var = 15)

# Validation predictions for RF with full variables
val_rf_full <- val_full %>%
  mutate(log_rent = log(rent)) %>%
  select(-barri, -rent)

val_pred_full_log <- predict(rf_full, newdata = val_rf_full)
val_pred_full <- exp(val_pred_full_log)
val_actual_full <- val_full$rent

val_results_full <- data.frame(
  actual = val_actual_full[1:length(val_pred_full)],
  predicted = val_pred_full
) %>%
  summarise(
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    R2 = cor(actual, predicted, use = "complete.obs")^2
  )

print("Validation (RF with ALL variables):")
print(val_results_full)

# Create combined table
rf_full_results_table <- data.frame(
  Metric = c("RMSE (€)", "MAE (€)", "R²"),
  Validation = c(
    round(val_results_full$RMSE, 2),
    round(val_results_full$MAE, 2),
    round(val_results_full$R2, 3)
  ),
  Test = c(
    round(test_results_full$RMSE, 2),
    round(test_results_full$MAE, 2),
    round(test_results_full$R2, 3)
  )
)

kable(rf_full_results_table, 
      caption = "Random Forest Performance",
      align = c('l', 'r', 'r')) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE)

# Simple version with proper spacing
par(mar = c(5, 8, 4, 2))  # Increase left margin for labels

varImpPlot(rf_full, 
           n.var = 15, 
           main = "Top 15 Most Important Variables",
           cex = 0.7,
           pch = 16,
           col = "darkblue")
