library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
# library(ggrepel)

ll <- read_ll()
fdoh <- data.table::fread("data/Test, Hospitalization, Death per daily report - Main.csv")
cdc <- data.table::fread("data/data_table_for_daily_death_trends__florida3.csv")
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

cdc1 <- cdc %>%
  mutate(date = mdy(Date)) %>%
  filter(date >= ymd("2020-03-01"), date <= ymd("2021-05-15"))

comb_df <- cdc1 %>%
  select(date, death_cdc = `New Deaths`) %>%
  full_join(fdoh %>% select(date = Date, death_fdoh = RNewDeath)) %>%
  full_join(ll1 %>% select(date = ChartDate, death_ll = death_n))
comb_df_long <- comb_df %>%
  pivot_longer(-date, names_to = "source", values_to = "death") %>%
  mutate(source = factor(source, levels = c("death_ll", "death_fdoh", "death_cdc"),
                         labels = c("FDOH Linelist", "FDOH Daily report", "CDC Data")))


ggplot() +
  geom_line(aes(x=date, y=death, colour=source), data=comb_df_long) +
  labs(x = "", y = "Daily new death") +
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
  summarise(death = sum(death),
            week_end_date = max(date))

dates <- rep(ymd("2020-03-01"), 17)
month(dates) <- c(3:12, 1:7)
year(dates) <- c(rep(2020, 10), rep(2021, 7))
ggplot() +
  geom_line(aes(x=week_end_date, y=death, colour=source), data=comb_df_long2) +
  labs(x = "", y = "Weekly new deaths") +
  scale_x_date(breaks = dates,
               labels = c("Mar\n2020", month.abb[4:12], "Jan\n2021", month.abb[2:7])) +
  scale_colour_manual(name = "Data Source",
                      values = c("black", "blue", "red")) +
  theme_classic() +
  theme(legend.position = "top")
