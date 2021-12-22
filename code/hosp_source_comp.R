library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
# library(ggrepel)

ll <- read_ll()
hhs <- data.table::fread("data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries.csv")
fdoh <- data.table::fread("data/Test, Hospitalization, Death per daily report - Main.csv")
fdoh$Date <- mdy(fdoh$Date)

ll1 <- ll %>%
  filter(ChartDate >= ymd("2020-03-01"), ChartDate <= ymd("2021-05-15")) %>%
  group_by(ChartDate) %>%
  summarise(n = n(),
            hosp_n = sum(Hospitalized == "YES", na.rm = T),
            death_n = sum(Died == "Yes", na.rm = T)) %>%
  ungroup %>%
  complete(ChartDate = seq(ymd("2020-03-01"), ymd("2021-05-15"), by = 1),
           fill = list(n = 0, hosp_n = 0, death_n = 0))

hhs1 <- hhs %>%
  filter(state == "FL") %>%
  mutate(date = ymd(date)) %>%
  filter(date >= ymd("2020-03-01"), date <= ymd("2021-05-15"))
hhs2 <- hhs1 %>% 
  select(date, hosp_adult = previous_day_admission_adult_covid_confirmed,
         hosp_ped = previous_day_admission_pediatric_covid_confirmed,
         hospital_onset_covid, inpatient_beds_used_covid,
         deaths_covid) %>%
  arrange(date)
hhs2$hosp <- hhs2$hosp_adult + hhs2$hosp_ped


comb_df <- hhs2 %>%
  select(date, hosp_hhs = hosp) %>%
  full_join(fdoh %>% select(date = Date, hosp_fdoh = RNewHosp)) %>%
  full_join(ll1 %>% select(date = ChartDate, hosp_ll = hosp_n))
comb_df_long <- comb_df %>%
  pivot_longer(-date, names_to = "source", values_to = "hosp") %>%
  mutate(source = factor(source, levels = c("hosp_ll", "hosp_fdoh", "hosp_hhs"),
                         labels = c("FDOH Linelist", "FDOH Daily report", "HHS Data")))

ggplot() +
  geom_line(aes(x=date, y=hosp, colour=source), data=comb_df_long) +
  labs(x = "", y = "Daily Incidence") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_colour_manual(name = "Data Source",
                      values = c("black", "blue", "red")) +
  theme_classic() +
  theme(legend.position = "top")

# Weekly
comb_df_long2 <- comb_df_long %>%
  mutate(week = epiweek(date),
         week = ifelse(week != 53 & year(date) != 2020, 53+week, week)) %>%
  group_by(source, week) %>%
  summarise(hosp = sum(hosp),
            week_end_date = max(date))

dates <- rep(ymd("2020-03-01"), 17)
month(dates) <- c(3:12, 1:7)
year(dates) <- c(rep(2020, 10), rep(2021, 7))
ggplot() +
  geom_line(aes(x=week_end_date, y=hosp, colour=source), data=comb_df_long2) +
  labs(x = "", y = "Weekly new admission") +
  scale_x_date(breaks = dates,
               labels = c("Mar\n2020", month.abb[4:12], "Jan\n2021", month.abb[2:7])) +
  scale_colour_manual(name = "Data Source",
                      values = c("black", "blue", "red")) +
  theme_classic() +
  theme(legend.position = "top")
