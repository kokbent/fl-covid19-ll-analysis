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
              death_n = sum(Died == "Yes", na.rm = T),
              death_wh = sum(Died == "Yes" & Hospitalized == "YES", na.rm = T),
              death_woh = sum(Died == "Yes" & Hospitalized == "NO", na.rm = T)) %>%
    mutate(Week = epiweek(ChartDate),
           Week = ifelse(Week != 53 & year(ChartDate) != 2020, 53+Week, Week)) %>%
    ungroup
  
  outb1_summ <- chd %>%
    filter(Week %in% c(11:20)) %>%
    group_by(Week) %>%
    summarise(n = sum(n), 
              hosp_n = sum(hosp_n),
              death_n = sum(death_n),
              death_wh = sum(death_wh),
              death_woh = sum(death_woh)) %>%
    ungroup() %>%
    summarise(n = mean(n), 
              hosp_n = mean(hosp_n),
              death_n = mean(death_n),
              death_wh = mean(death_wh),
              death_woh = mean(death_woh))
  
  chd_weekly <- chd %>%
    group_by(Week) %>%
    summarise(n = sum(n), 
              hosp_n = sum(hosp_n),
              death_n = sum(death_n),
              death_wh = sum(death_wh),
              death_woh = sum(death_woh)) %>%
    mutate(n_ob1 = outb1_summ$n,
           hosp_n_ob1 = outb1_summ$hosp_n,
           death_n_ob1 = outb1_summ$death_n,
           death_wh_ob1 = outb1_summ$death_wh,
           death_woh_ob1 = outb1_summ$death_woh,
           cr = n / n_ob1,
           hr = hosp_n / hosp_n_ob1,
           dr = death_n / death_n_ob1,
           dwhr = death_wh / death_wh_ob1,
           dwohr = death_woh / death_woh_ob1,
           cr2hr = cr / hr,
           cr2dr = cr / dr)
  
  chd_weekly$week_end_date <- ymd("2020-01-01")
  week(chd_weekly$week_end_date) <- chd_weekly$Week
  wday(chd_weekly$week_end_date) <- 6
  
  chd_long <- chd_weekly %>%
    select(Week, week_end_date, cr:dwohr) %>%
    pivot_longer(-c(Week, week_end_date),
                 names_to = "type", values_to = "ratio")
  chd_long$type <- factor(chd_long$type,
                          levels = c("cr", "hr", "dr", "dwhr", "dwohr"))
  chd_long$age_grp <- age_grp[i]
  
  chd_all <- bind_rows(chd_all, chd_long)
}

chd_all$age_grp <- factor(chd_all$age_grp,
                          levels = c("All ages", "0 to 39", "40 to 64", "Above 65"))

p1l <- chd_all %>%
  filter(age_grp == "All ages") %>%
  filter(!type %in% c("dwhr", "dwohr")) %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=ratio, colour=type), size=0.8) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous(breaks = 0:4 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "Ratio", title = "(a) Overall case, hospitalization and death ratios") +
  scale_colour_manual(name = NA, values = c("black", "blue", "red"),
                      labels = c("Case", "Hospitalized", "Death")) +
  ggpubr::theme_pubclean(base_size = 9) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(1.0)))

l1 <- ggpubr::get_legend(p1l)

p1 <- chd_all %>%
  filter(age_grp == "All ages") %>%
  filter(!type %in% c("dwhr", "dwohr")) %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=ratio, colour=type), size=0.8, show.legend = F) +
  geom_segment(aes(x=ymd("2020-03-08"), xend=ymd("2020-05-15"), y=3, yend=3), colour="gray40",
               arrow = arrow(angle = 20, unit(0.1, "inches"), ends="both", type="closed")) +
  geom_hline(yintercept = 1, lty = 2) +
  annotate("text", x=ymd("2020-04-11"), y=4, label = "First outbreak", size=2.5, colour="gray40") +
  annotate("text", x=ymd("2020-05-01"), y=12, 
           label = "Ratio = Weekly number\nover mean weekly\nnumber during first\noutbreak", 
           size=2.5, colour="gray40") +
  scale_y_continuous(breaks = 0:4 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "Ratio", title = "(a) Overall case, hospitalization and death ratios") +
  scale_colour_manual(name = NA, values = c("black", "blue", "red"),
                      labels = c("Case", "Hospitalized", "Death")) +
  ggpubr::theme_pubclean(base_size = 9) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(1.0)))


chd_age <- ll %>%
  filter(ChartDate >= ymd("2020-03-01"), ChartDate <= ymd("2021-04-03")) %>%
  mutate(Week = epiweek(ChartDate),
         Week = ifelse(Week != 53 & year(ChartDate) != 2020, 53+Week, Week)) %>%
  group_by(Week) %>%
  summarise(case = median(Age, na.rm = T),
            hosp = median(Age[Hospitalized == "YES"], na.rm = T),
            died = median(Age[Died == "Yes"], na.rm = T),
            week_end_date = max(ChartDate)) %>%
  ungroup %>%
  pivot_longer(-c("Week", "week_end_date"), names_to = "type", values_to = "med_age")
chd_age$type <- factor(chd_age$type, levels = c("case", "hosp", "died"))

p2 <- chd_age %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=med_age, colour=type), size=0.8, show.legend = F) +
  # scale_y_continuous(breaks = 0:4 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "Median Age", title = "(b) Median age") +
  scale_colour_manual(name = NA, values = c("black", "blue", "red"),
                      labels = c("Case", "Hospitalized", "Death")) +
  ggpubr::theme_pubclean(base_size = 9) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(1.0)))

#### Plot by metrics
pal <- viridis::inferno(4, end = 0.80)
pal[2:4] <- rev(pal[2:4])

metrics <- c(
  "cr" = "Case",
  "hr" = "Hospitalization",
  "dr" = "Death"
)

p3l <- chd_all %>%
  filter(type == "cr") %>%
  filter(age_grp != "All ages") %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=ratio, colour=age_grp), lwd=1.0) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous(breaks = 0:6 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "", title = "(c) Case ratios by age group") +
  scale_colour_manual(values = pal[2:4]) +
  ggpubr::theme_pubclean(base_size = 9) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(1.0)))

l2 <- ggpubr::get_legend(p3l)

p3 <- chd_all %>%
  filter(type == "cr") %>%
  filter(age_grp != "All ages") %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=ratio, colour=age_grp), lwd=1.0, show.legend = F) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous(breaks = 0:6 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "", title = "(c) Case ratios by age group") +
  scale_colour_manual(values = pal[2:4]) +
  ggpubr::theme_pubclean(base_size = 9) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(1.0)))

p4 <- chd_all %>%
  filter(type == "dr") %>%
  filter(age_grp != "All ages") %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=ratio, colour=age_grp), lwd=1.0, show.legend = F) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous(breaks = 0:6 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "", title = "(d) Death ratios by age group") +
  scale_colour_manual(values = pal[2:4]) +
  ggpubr::theme_pubclean(base_size = 9) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(1.0)))

b <- grid::rectGrob(gp = grid::gpar(col=NA))

g <- grid.arrange(
  p1, p2, p3, p4, l1, l2,
  layout_matrix = matrix(c(5, 1, 2, 6, 3, 4), nrow = 3),
  widths = c(8, 8),
  heights = c(1, 8, 8)
)

ggsave("fig/ratios.png", g, width = 16, height = 17, units = "cm")

####

chd_weekly %>% filter(Week %in% 23:37) %>% summarise(cr = sum(n) / (10*unique(n_ob1)),
                                                     dr = sum(death_n) / (10*unique(death_n_ob1)))

chd_weekly %>% filter(Week %in% 42) %>% summarise(cr = sum(n) / n_ob1,
                                                  dr = sum(death_n) / death_n_ob1)
