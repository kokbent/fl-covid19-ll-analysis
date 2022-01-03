library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(ggpubr)
source("code/functions.R")

ll <- read_ll()
fdoh <- data.table::fread("data/Test, Hospitalization, Death per daily report - Main.csv")
cdc_case <- data.table::fread("data/data_table_for_daily_case_trends__florida3.csv")
cdc_death <- data.table::fread("data/data_table_for_daily_death_trends__florida3.csv")
hhs <- data.table::fread("data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries.csv")
fdoh$Date <- mdy(fdoh$Date)

ll1 <- ll %>%
  filter(ChartDate >= ymd("2020-03-01"), ChartDate <= ymd("2021-05-01")) %>%
  group_by(ChartDate) %>%
  summarise(n = n(),
            hosp_n = sum(Hospitalized == "YES", na.rm = T),
            death_n = sum(Died == "Yes", na.rm = T)) %>%
  ungroup %>%
  complete(ChartDate = seq(ymd("2020-03-01"), ymd("2021-05-01"), by = 1),
           fill = list(n = 0, hosp_n = 0, death_n = 0))

cdc_case1 <- cdc_case %>%
  mutate(date = mdy(Date)) %>%
  filter(date >= ymd("2020-03-01"), date <= ymd("2021-05-01"))
cdc_death1 <- cdc_death %>%
  mutate(date = mdy(Date)) %>%
  filter(date >= ymd("2020-03-01"), date <= ymd("2021-05-01"))

hhs1 <- hhs %>%
  filter(state == "FL") %>%
  mutate(date = ymd(date)) %>%
  filter(date >= ymd("2020-03-01"), date <= ymd("2021-05-01"))
hhs2 <- hhs1 %>% 
  select(date, hosp_adult = previous_day_admission_adult_covid_confirmed,
         hosp_ped = previous_day_admission_pediatric_covid_confirmed,
         hospital_onset_covid, inpatient_beds_used_covid,
         deaths_covid) %>%
  arrange(date)
hhs2$hosp <- hhs2$hosp_adult + hhs2$hosp_ped

fdoh <- fdoh %>%
  filter(Date <= ymd("2021-05-01"))

comb_df_case <- cdc_case1 %>%
  select(date, case_cdchhs = `New Cases`) %>%
  full_join(fdoh %>% select(date = Date, case_fdoh = RCase_Incd)) %>%
  full_join(ll1 %>% select(date = ChartDate, case_ll = n))
case_long <- comb_df_case %>%
  pivot_longer(-date, names_to = "source", values_to = "case") %>%
  mutate(source = str_remove(source, "case_"))

comb_df_hosp <- hhs2 %>%
  select(date, hosp_cdchhs = hosp) %>%
  full_join(fdoh %>% select(date = Date, hosp_fdoh = RNewHosp)) %>%
  full_join(ll1 %>% select(date = ChartDate, hosp_ll = hosp_n))
hosp_long <- comb_df_hosp %>%
  pivot_longer(-date, names_to = "source", values_to = "hosp") %>%
  mutate(source = str_remove(source, "hosp_"))

comb_df_death <- cdc_death1 %>%
  select(date, death_cdchhs = `New Deaths`) %>%
  full_join(fdoh %>% select(date = Date, death_fdoh = RNewDeath)) %>%
  full_join(ll1 %>% select(date = ChartDate, death_ll = death_n))
death_long <- comb_df_death %>%
  pivot_longer(-date, names_to = "source", values_to = "death") %>%
  mutate(source = str_remove(source, "death_"))

comb_df <- case_long %>%
  full_join(hosp_long) %>%
  full_join(death_long)
comb_df_long <- comb_df %>%
  pivot_longer(case:death, names_to = "metric")

# Weekly
comb_df_long2 <- comb_df_long %>%
  mutate(week = epiweek(date),
         week = ifelse(week != 53 & year(date) != 2020, 53+week, week)) %>%
  group_by(source, week, metric) %>%
  summarise(value = sum(value),
            week_end_date = max(date))
  # summarise(value = ifelse(all(is.na(value)), NA, sum(value, na.rm = T)),
  #           week_end_date = max(date))
comb_df_long2 <- comb_df_long2 %>%
  mutate(source = factor(source, levels = c("ll", "fdoh", "cdchhs")))

dates <- rep(ymd("2020-03-01"), 17)
month(dates) <- c(3:12, 1:7)
year(dates) <- c(rep(2020, 10), rep(2021, 7))
dates_label <- c("Mar\n2020", month.abb[4:12], "Jan\n2021", month.abb[2:7])
p1l <- ggplot() +
  geom_line(aes(x=week_end_date, y=value, colour=source), 
            data=comb_df_long2 %>% 
              filter(metric == "case", source != "hhs"),
            lwd = 1) +
  labs(x = "", y = "Cases ('0000)",
       title = "(a) Weekly new cases") +
  scale_x_date(breaks = dates,
               labels = dates_label) +
  scale_colour_manual(name = "Data Source",
                      labels = c("FDOH Linelist", "FDOH daily report",
                                 "CDC or HHS"),
                      values = c("black", "blue", "red")) +
  ggpubr::theme_pubclean(base_size = 13) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.border = element_rect(fill = NA, size = 1))

l <- ggpubr::get_legend(p1l)
p1 <- ggplot() +
  geom_line(aes(x=week_end_date, y=value, colour=source), 
            data=comb_df_long2 %>% 
              filter(metric == "case", source != "hhs"),
            lwd = 1, show.legend = F) +
  labs(x = "", y = "Cases ('000)",
       title = "(a) Weekly new cases") +
  scale_x_date(breaks = dates,
               labels = dates_label) +
  scale_y_continuous(breaks = 0:3 * 30000, labels = 0:3 * 30) +
  scale_colour_manual(name = "Data Source",
                      labels = c("FDOH Linelist", "FDOH daily report",
                                 "CDC or HHS"),
                      values = c("black", "blue", "red")) +
  ggpubr::theme_pubclean(base_size = 12) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.border = element_rect(fill = NA, size = 1))

p2 <- ggplot() +
  geom_line(aes(x=week_end_date, y=value, colour=source), 
            data=comb_df_long2 %>% 
              filter(metric == "hosp"),
            lwd = 1, show.legend = F) +
  labs(x = "", y = "Admission ('000)",
       title = "(b) Weekly new hospital admission") +
  scale_x_date(breaks = dates,
               labels = dates_label) +
  scale_y_continuous(breaks = 0:4 * 2000, 
                     labels = paste0(0:4 * 2, ".0")) +
  scale_colour_manual(name = "Data Source",
                      labels = c("FDOH Linelist", "FDOH daily report",
                                 "CDC or HHS"),
                      values = c("black", "blue", "red")) +
  ggpubr::theme_pubclean(base_size = 12) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.border = element_rect(fill = NA, size = 1))

p3 <- ggplot() +
  geom_line(aes(x=week_end_date, y=value, colour=source), 
            data=comb_df_long2 %>% 
              filter(metric == "death"),
            lwd = 1, show.legend = F) +
  labs(x = "", y = "Deaths ('000)",
       title = "(c) Weekly new deaths") +
  scale_x_date(breaks = dates,
               labels = dates_label) +
  scale_y_continuous(breaks = 0:4 * 500, labels = 0:4 * 0.5) +
  scale_colour_manual(name = "Data Source",
                      labels = c("FDOH Linelist", "FDOH daily report",
                                 "CDC or HHS"),
                      values = c("black", "blue", "red")) +
  ggpubr::theme_pubclean(base_size = 12) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.border = element_rect(fill = NA, size = 1))

g <- gridExtra::arrangeGrob(p1, p2, p3, l, nrow = 4,
                            heights = c(10, 10, 10, 1.5))
ggsave("fig/source_comp.png", g, width = 16, height = 17, units = "cm")



