rm(list=ls())
library(tidyverse)
library(lubridate)
library(gridExtra)

ll <- data.table::fread("data/linelist_latest.csv")
ll$Case1 <- as.POSIXct(ll$Case1/1000, origin="1970-01-01", tz="UTC") %>%
  format(format = "%Y-%m-%d %H:%M:%S")
ll$EventDate <- ymd_hms(ll$EventDate) %>% as.Date()
ll$ChartDate <- ymd_hms(ll$ChartDate) %>% as.Date()
ll$Age <- as.numeric(ll$Age)

age_lo <- c(18, 40, 65, 0)
age_hi <- c(39, 64, 200, 200)
age_grp <- c("0 to 39", "40 to 64", "Above 65", "All ages")

chd_all <- data.frame()

for (i in 1:4) {
  chd <- ll %>%
    filter(ChartDate >= ymd("2020-03-01"), ChartDate <= ymd("2021-04-03")) %>%
    filter(Age >= age_lo[i], Age <= age_hi[i]) %>%
    group_by(ChartDate) %>%
    summarise(n = n(),
              hosp_n = sum(Hospitalized == "YES", na.rm = T),
              death_n = sum(Died == "Yes", na.rm = T)) %>%
    ungroup %>%
    complete(ChartDate = seq(ymd("2020-03-01"), ymd("2021-04-03"), by = 1),
             fill = list(n = 0, hosp_n = 0, death_n = 0)) %>%
    mutate(Week = epiweek(ChartDate),
           Week = ifelse(Week != 53 & year(ChartDate) != 2020, 53+Week, Week))
  
  weekly_chd <- chd %>%
    group_by(Week) %>%
    summarise(across(n:death_n, sum),
              week_end_date = max(ChartDate))
  
  weekly_chd$age_grp <- age_grp[i]
  chd_all <- bind_rows(chd_all, weekly_chd)
}

wave1 <- chd_all %>%
  filter(Week %in% c(11:19)) %>%
  group_by(age_grp) %>%
  summarise(across(n:death_n, mean, .names = "{col}_w1"))
  
chd_all_wave <- chd_all %>%
  left_join(wave1, by = "age_grp") %>%
  group_by(age_grp) %>%
  mutate(cfr = death_n/n,
         cfr_ma3 = stats::filter(death_n, rep(1/4, 4))/stats::filter(n, rep(1/4, 4)),
         cfr_w1 = death_n_w1/n_w1)
ggplot(chd_all_wave %>% filter(cfr_ma3 != 0)) +
  geom_line(aes(x=week_end_date, y=log(cfr_ma3/cfr_w1, 2), colour=age_grp), lwd = 2) +
  scale_y_continuous(breaks = (-4):2,
                     labels = c(paste0("1/", 2^(4:1)),
                                2^(0:2))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%y") +
  labs(title = "3-week CFR over first wave CFR", y = "",
       x = "")

### Test data ----
test_dat <- data.table::fread("~/Downloads/data_table_for_daily_test_volume__florida.csv",
                              skip = 2, na.strings = "null")
test_dat$Date <- mdy(test_dat$Date)
test_dat <- test_dat %>%
  filter(Date >= ymd("2020-03-01"), Date <= ymd("2021-04-03")) %>%
  mutate(Week = epiweek(Date),
         Week = ifelse(Week != 53 & year(Date) != 2020, 53+Week, Week))

weekly_test <- test_dat %>%
  group_by(Week) %>%
  summarise(test = sum(`Daily Test Volume`),
            week_end_date = max(Date))

wave1_test <- weekly_test %>%
  filter(Week %in% c(11:20)) %>%
  summarise(test_w1 = mean(test))

weekly_test$test_w1 <- wave1_test$test_w1
ggplot(weekly_test) +
  geom_line(aes(x=week_end_date, y=test/test_w1), lwd = 2) +
  # scale_y_continuous(breaks = (-4):2,
  #                    labels = c(paste0("1/", 2^(4:1)),
  #                               2^(0:2))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%y") +
  labs(title = "3-week CFR over first wave CFR", y = "",
       x = "")
