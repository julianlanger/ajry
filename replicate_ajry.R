library(dplyr)
library(readxl)
library(ggplot2)
library(ggrepel)
library(modelsummary)

rm(list = ls())

# replicating figure 1 ----------------------------------------------------
ajry_f1 <- read_xls('raw_data/ajry.xls',
                   sheet = 'F1') 

ajry_f1 <- ajry_f1 %>% 
  rename(log_gdp_pc = lrgdpch, ?
         freedom_house = fhpolrigaug)

glimpse(ajry_f1)

ggplot(ajry_f1, aes(x = log_gdp_pc, y = freedom_house)) +
  geom_point(size = 0.5) +
  geom_text(aes(label = code), size = 2, hjust = 0, vjust = 0) +
  geom_smooth(method = 'lm', 
              color = 'black', 
              size = 0.5, 
              alpha = 0.2) +
  labs(x = 'Log GDP per Capita (1990-1999)',
       y = 'FH Measure of Democracy (1990-1999)') +
  theme_bw()

# add ggrepel
ggplot(ajry_f1, aes(x = log_gdp_pc, y = freedom_house)) +
  geom_point(size = 0.5) +
  geom_text_repel(aes(label = code), size = 2, max.overlaps = 20) +
  geom_smooth(method = 'lm', 
              color = 'black', 
              size = 0.5, 
              alpha = 0.2) +
  labs(x = 'Log GDP per Capita (1990-1999)',
       y = 'FH Measure of Democracy (1990-1999)') +
  theme_bw()

# replicating figure 2 ----------------------------------------------------
ajry_f2 = read_xls("./raw_data/ajry.xls", 
                   sheet = "F2") 

glimpse(ajry_f2)

ajry_f2 <- ajry_f2 %>% 
  rename(freedom_house_change = s5fhpolrigaug,
         log_gdp_pc_change = s5lrgdpch)

ggplot(ajry_f2, aes(x = log_gdp_pc_change, y = freedom_house_change)) +
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", color = 'black', size = 0.5, alpha = 0.2) +
  geom_text(aes(label = code), size = 2,  hjust = 0, vjust = 0) +
  labs(x = "Change in GDP per Capita (1970-1995)",
       y = "Change in FH Measure of Democracy (1970-1995)") + 
  theme_bw()

# loading data for estimation ---------------------------------------------
ajry_df <- read_xls('raw_data/ajry.xls',
                    sheet = 2) 

head(ajry_df)

ajry_df <- ajry_df %>% 
  rename(log_gdp_pc = lrgdpch,
         freedom_house = fhpolrigaug)

# generate lagged variables -----------------------------------------------
ajry_df <- ajry_df %>% 
  group_by(code_numeric) %>% 
  mutate(lag_log_gdp_pc = lag(log_gdp_pc, order_by = year_numeric),
         lag_freedom_house = lag(freedom_house, order_by = year_numeric),
         lag2_nsave = lag(nsave, 2, order_by = year_numeric),
         lag_worldincome = lag(worldincome, order_by = year_numeric))


# filter sample -----------------------------------------------------------
ajry_sample <- ajry_df %>% 
  filter(sample == 1)

summary_sample <- ajry_sample %>% 
  filter(!is.na(freedom_house), 
         !is.na(lag_freedom_house), 
         !is.na(lag_log_gdp_pc))

# create summary statistics -----------------------------------------------
mean_na <- function(x) mean(x, na.rm = TRUE)
sd_na <- function(x) sd(x, na.rm = TRUE)
n_countries <- summary_sample %>% 
  ungroup() %>% 
  summarize(n_countries = n_distinct(code_numeric)) %>% 
  pull(n_countries)
no_countries <- function(x) as.integer(n_countries)


datasummary(freedom_house + lag_log_gdp_pc + ~ mean_na + 
              sd_na + 
              N + no_countries, 
            data = summary_sample)


# pooled ols and fe ols with lm -------------------------------------------
pooled <- lm(freedom_house ~ -1 + lag_freedom_house + lag_log_gdp_pc + 
               factor(year_numeric), data = ajry_sample)

summary(pooled)
modelsummary(pooled)


# get clustered standard errors 
library(multiwayvcov)
vcov_country_pool <- cluster.vcov(pooled, ajry_sample$code_numeric)

library(lmtest)
coeftest(pooled, vcov_country_pool)

# old school fe
fe_est <- lm(freedom_house ~ -1 + lag_freedom_house + lag_log_gdp_pc + 
               factor(year_numeric) + factor(code_numeric), data = ajry_sample)

# variance covariance matrix
vcov_country_fe <- cluster.vcov(fe_est, ajry_sample$code_numeric)

# standard errors
se_fe <- sqrt(diag(vcov_country_fe))

# testing significance
coeftest(fe_est, vcov_country_fe)

# pooled and fe estimates with fixest -------------------------------------
library(fixest)

pooled_fix <- feols(freedom_house ~ lag_freedom_house + lag_log_gdp_pc |
      year_numeric, 
      data = ajry_sample,
      cluster = ~code_numeric)

summary(pooled_fix)
broom::tidy(pooled_fix)

fe_fix <- feols(freedom_house ~ lag_freedom_house + lag_log_gdp_pc |
                      year_numeric + code_numeric, 
                    data = ajry_sample,
                    cluster = ~code_numeric,
                dof = dof(fixef.K = 'full'))

summary(fe_fix)
broom::tidy(fe_fix)

# integrating iv ----------------------------------------------------------
iv <- feols(freedom_house ~ lag_freedom_house | year_numeric + code_numeric | 
                  lag_log_gdp_pc ~ lag2_nsave,
                data = ajry_sample, 
                cluster = ~code_numeric,
                dof = dof(fixef.K = 'full'))

summary(iv)
summary(iv, stage = 1)

broom::tidy(iv)
broom::tidy(iv, stage = 1)

# creating a table --------------------------------------------------------


