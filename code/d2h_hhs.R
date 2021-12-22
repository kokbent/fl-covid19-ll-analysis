library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

df <- data.table::fread("~/../Downloads/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries.csv")
df

df1 <- df %>%
  filter(state == "FL") %>%
  mutate(date = ymd(date))
df2 <- df1 %>% 
  select(date, 
         hosp_adult = `previous_day_admission_adult_covid_confirmed`,
         hosp_adult_coverage = `previous_day_admission_adult_covid_confirmed_coverage`,
         hosp_ped = previous_day_admission_pediatric_covid_confirmed,
         hospital_onset_covid, inpatient_beds_used_covid,
         deaths_covid) %>%
  arrange(date)

ggplot(df2) +
  geom_line(aes(x=date, y=hosp_adult_coverage)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y")

colnames(df1)[str_detect(colnames(df1), "previous_day_admission_adult_covid_confirmed")]

df3 <- df1 %>%
  select(date,
         contains("previous_day_admission_adult_covid_confirmed")) %>%
  select(date, 
         !contains("coverage"))

df4 <- df3 %>%
  filter(date >= ymd("2020-11-01")) %>%
  pivot_longer(-date)

df4[,c("age_lo", "age_hi")] <- str_match(df4$name, "previous_day_admission_adult_covid_confirmed_(.*)[-+](.*)")[,2:3]
df4 <- df4 %>%
  filter(!is.na(age_lo)) %>%
  mutate(age_hi = as.numeric(age_hi),
         age_grp = case_when(
           age_hi <= 39 ~ "18 to 39",
           age_hi <= 59 ~ "40 to 59",
           T ~ "Above 60"
         )) %>%
  group_by(date, age_grp) %>%
  summarise(hosp = sum(value))

hhs_age <- df4 %>%
  mutate(Week = epiweek(date),
         Week = ifelse(Week != 53 & year(date) != 2020, 53+Week, Week)) %>%
  group_by(Week, age_grp) %>%
  summarise(hosp = sum(hosp),
            week_end_date = max(date))

#### LL CHD_ALL
ll$EventDate <- ymd_hms(ll$EventDate) %>% as.Date()
ll$ChartDate <- ymd_hms(ll$ChartDate) %>% as.Date()
ll$Age <- as.numeric(ll$Age)

age_lo <- c(18, 40, 60)
age_hi <- c(39, 59, 200)
age_grp <- c("18 to 39", "40 to 59", "Above 60")

chd_all <- data.frame()

for (i in 1:3) {
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

#### Bind HHS and CHD_ALL
hhs_age_adj <- hhs_age %>%
  mutate(Week = Week - 1) %>%
  rename(hhs_hosp = hosp)
chd_all_hhs <- chd_all %>%
  left_join(hhs_age_adj %>% select(Week, age_grp, hhs_hosp),
            by = c("Week", "age_grp"))

pal <- colorRamp(viridis::cividis(50))
ggplot(chd_all_hhs %>% filter(Week >= 44)) +
  geom_point(aes(x=n, y=death_n/hhs_hosp, colour=week_end_date)) +
  # geom_path(aes(x=n, y=death_n/hhs_hosp, group=age_grp)) +
  geom_smooth(aes(x=n, y=death_n/hhs_hosp, group=age_grp), method="lm", 
              se=F, lty=2, colour="#444444") +
  geom_label_repel(aes(x=n, y=death_n/hhs_hosp, label=age_grp), 
                   data=chd_all_hhs %>% filter(Week >= 44) %>% 
                     group_by(age_grp) %>% filter(n == max(n)),
                   nudge_x = 1000) +
  scale_y_log10() +
  scale_color_date(name = "", low = "orange", high = "blue")
