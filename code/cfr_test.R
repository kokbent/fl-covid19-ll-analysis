rm(list=ls())
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(ggrepel)
source("code/functions.R")

ll <- read_ll()

age_lo <- c(0, 40, 65, 0)
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

waves <- data.frame(wave = c("1st", "2nd", "3rd"),
                    start_week = c(11, 20, 41),
                    end_week = c(19, 40, 66))
waves <- waves %>%
  left_join(chd_all %>% 
              select(start_week = Week, start_date = week_end_date) %>%
              distinct()) %>%
  left_join(chd_all %>% 
              select(end_week = Week, end_date = week_end_date) %>%
              distinct())
waves <- waves %>% mutate(
  start_date = start_date - 6,
  mid_date = start_date + (end_date - start_date)/2
)

wave1 <- chd_all %>%
  filter(Week %in% c(11:19)) %>%
  group_by(age_grp) %>%
  summarise(across(n:death_n, mean, .names = "{col}_w1"))
  
chd_all_wave <- chd_all %>%
  left_join(wave1, by = "age_grp") %>%
  group_by(age_grp) %>%
  mutate(cfr = death_n/n,
         cfr_ma3 = stats::filter(death_n, rep(1/3, 3))/stats::filter(n, rep(1/3, 3)),
         cfr_w1 = death_n_w1/n_w1)

dates <- rep(ymd("2020-03-01"), 8)
month(dates) <- c(seq(3, 11, by=2), seq(1, 5, by=2))
year(dates) <- c(rep(2020, 5), rep(2021, 3))
dates_label <- c("Mar\n2020", month.abb[seq(5, 11, by=2)], 
                 "Jan\n2021", month.abb[seq(3, 5, by=2)])

pal <- viridis::inferno(4, end = 0.80)
pal <- rev(pal)

p1 <- ggplot() +
  geom_line(aes(x=week_end_date, y=log(cfr_ma3/cfr_w1, 2), colour=age_grp), 
            data = chd_all_wave %>% filter(cfr_ma3 != 0), lwd=2,
            show.legend = F) +
  geom_text_repel(aes(x=week_end_date, y=log(cfr_ma3/cfr_w1, 2), 
                       label=age_grp, colour=age_grp), 
                   data=chd_all_wave %>% filter(Week == max(Week) - 1),
                   nudge_x = 10, direction = "y", hjust = 0,
                  show.legend = F) +
  geom_segment(aes(x=start_date, xend=end_date, y=-4.2, yend=-4.2), 
               data = waves, colour="gray40", 
               arrow = arrow(angle = 20, unit(0.1, "inches"), ends="both", type="closed")) +
  geom_text(aes(x=mid_date, y=-4.4, label=paste0(wave, " wave")), data=waves,
            colour="gray40") +
  scale_y_continuous(breaks = (-4):2,
                     labels = c(paste0("1/", 2^(4:1)),
                                2^(0:2))) +
  scale_x_date(breaks = dates, labels = dates_label,
               limits = c(ymd("2020-03-07"), ymd("2021-05-15"))) +
  scale_colour_manual(values = pal) +
  labs(title = "(a) Normalized 3-week moving average of CFR (by first wave CFR)", y = "",
       x = "") +
  ggpubr::theme_pubclean(base_size = 11) +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.8),
        legend.background = element_rect(fill = NA),
        legend.direction = "vertical",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(fill = NA, size = 1),
        # panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(0.95)))

ggsave("fig/cfr.png", width = 16, height = 8, units = "cm")

### Test data ----
test_dat <- data.table::fread("data/data_table_for_daily_test_volume__florida.csv",
                              skip = 2, na.strings = "N/A")
test_dat$Date <- mdy(test_dat$Date)
test_dat <- test_dat %>%
  filter(Date >= ymd("2020-03-01"), Date <= ymd("2021-05-15")) %>%
  mutate(Week = epiweek(Date),
         Week = ifelse(Week != 53 & year(Date) != 2020, 53+Week, Week))

weekly_test <- test_dat %>%
  group_by(Week) %>%
  summarise(test = sum(`Daily Test Volume`),
            week_end_date = max(Date))

wave1_test <- weekly_test %>%
  filter(Week %in% c(11:19)) %>%
  summarise(test_w1 = mean(test))

weekly_test$test_w1 <- wave1_test$test_w1

p2 <- ggplot() +
  geom_line(aes(x=week_end_date, y=test/test_w1), 
            data = weekly_test, lwd=2) +
  geom_segment(aes(x=start_date, xend=end_date, y=-0.4, yend=-0.4), 
               data = waves, colour="gray40", 
               arrow = arrow(angle = 20, unit(0.1, "inches"), ends="both", type="closed")) +
  geom_text(aes(x=mid_date, y=-0.8, label=paste0(wave, " wave")), data=waves,
            colour="gray40") +
  scale_x_date(breaks = dates, labels = dates_label,
               limits = c(ymd("2020-03-07"), ymd("2021-05-15"))) +
  labs(title = "(b) Normalized 7-day moving average of test volume (by first wave volume)", y = "",
       x = "") +
  ggpubr::theme_pubclean(base_size = 11) +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.8),
        legend.background = element_rect(fill = NA),
        legend.direction = "vertical",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(fill = NA, size = 1),
        # panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(0.95)))

g <- grid.arrange(p1, p2, nrow = 2)
ggsave("fig/cfr_test.png", g, width = 16, height = 17, units = "cm")
