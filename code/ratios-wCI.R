rm(list=ls())
library(tidyverse)
library(lubridate)
library(gridExtra)

ll <- data.table::fread("../fl-covid19/data/linelist_latest.csv")
ll$Case1 <- as.POSIXct(ll$Case1/1000, origin="1970-01-01", tz="UTC") %>%
  format(format = "%Y-%m-%d %H:%M:%S")
ll$EventDate <- ymd_hms(ll$EventDate) %>% as.Date()
ll$ChartDate <- ymd_hms(ll$ChartDate) %>% as.Date()
ll$Age <- as.numeric(ll$Age)

age_lo <- c(0, 40, 65, 0)
age_hi <- c(39, 64, 200, 200)
age_grp <- c("0 to 39", "40 to 64", "Above 65", "All ages")

#### Function
ts_uncertainty <- function (v, d = 5) {
  df <- data.frame(i = 1:length(v))
  df$ini <- df$i - d
  df$ini[df$ini <= 0] <- 1
  df$fin <- df$i + d
  df$fin[df$fin > length(v)] <- length(v)
  
  mat <- sapply(rep(1:length(v), 2000), 
                function (x) sample(v[df$ini[x]:df$fin[x]], 1)) %>%
    matrix(nrow = length(v))
  
  return((colSums(mat) / (length(v) / 7))  %>% 
           quantile(c(.025, .975)))
}
####

chd_all <- data.frame()

for (i in 1:4) {
  chd <- ll %>%
    filter(ChartDate >= ymd("2020-03-01"), ChartDate <= ymd("2021-04-03")) %>%
    filter(Age >= age_lo[i], Age <= age_hi[i]) %>% 
    group_by(ChartDate) %>%
    summarise(n = n(), 
              hosp_n = sum(Hospitalized == "YES", na.rm = T),
              death_n = sum(Died == "Yes", na.rm = T)) %>%
    mutate(Week = epiweek(ChartDate),
           Week = ifelse(Week != 53 & year(ChartDate) != 2020, 53+Week, Week)) %>%
    ungroup
  
  outb1_summ <- chd %>%
    filter(Week %in% c(11:20)) %>%
    summarise(
      CI = list(ts_uncertainty(n)),
      n = mean(n)*7,
      nlo = CI[[1]][1],
      nhi = CI[[1]][2],
      CI = list(ts_uncertainty(hosp_n)),
      hosp_n = mean(hosp_n)*7,
      hosp_nlo = CI[[1]][1],
      hosp_nhi = CI[[1]][2],
      CI = list(ts_uncertainty(death_n)),
      death_n = mean(death_n)*7,
      death_nlo = CI[[1]][1],
      death_nhi = CI[[1]][2])
  
  chd_weekly <- chd %>%
    group_by(Week) %>%
    summarise(week_end_date = max(ChartDate), 
              n = sum(n), 
              hosp_n = sum(hosp_n),
              death_n = sum(death_n)) %>%
    mutate(cr = n / outb1_summ$n,
           crlo = n / outb1_summ$nhi,
           crhi = n / outb1_summ$nlo,
           hr = hosp_n / outb1_summ$hosp_n,
           hrlo = hosp_n / outb1_summ$hosp_nhi,
           hrhi = hosp_n / outb1_summ$hosp_nlo,
           dr = death_n / outb1_summ$death_n,
           drlo = death_n / outb1_summ$death_nhi,
           drhi = death_n / outb1_summ$death_nlo,
           cr2hr = cr / hr,
           cr2dr = cr / dr)
  
  chd_long <- chd_weekly %>%
    select(Week, week_end_date, cr:drhi) %>%
    pivot_longer(-c(Week, week_end_date),
                 names_to = "type", values_to = "ratio")
  # chd_long$type <- factor(chd_long$type,
  #                         levels = c("cr", "hr", "dr"))
  chd_long$age_grp <- age_grp[i]
  
  chd_all <- bind_rows(chd_all, chd_long)
}

chd_all$age_grp <- factor(chd_all$age_grp,
                          levels = c("All ages", "0 to 39", "40 to 64", "Above 65"))

chd_all1 <- chd_all %>%
  filter(type %in% c("cr", "hr", "dr"))

chd_all2 <- chd_all %>%
  filter(str_detect(type, "lo|hi")) %>%
  mutate(type1 = str_sub(type, 1, 2),
         type2 = str_sub(type, 3, 4)) %>%
  select(-type) %>%
  pivot_wider(names_from = type2, values_from = ratio) %>%
  rename(type = type1)
chd_all <- chd_all1 %>%
  left_join(chd_all2)
chd_all$type <- factor(chd_all$type,
                       levels = c("cr", "hr", "dr"))


p1l <- chd_all %>%
  filter(age_grp == "All ages") %>%
  filter(!type %in% c("dwhr", "dwohr")) %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=ratio, colour=type), size=0.8) +
  geom_ribbon(aes(x=week_end_date, ymin=lo, ymax=hi, fill=type), alpha=0.4) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous(breaks = 0:6 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "Ratio", title = "(a) Overall case, hospitalization and death ratios") +
  scale_colour_manual(name = NA, values = c("black", "blue", "red"),
                      labels = c("Case", "Hospitalized", "Death")) +
  scale_fill_manual(name = NA, values = c("black", "blue", "red"),
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
  geom_ribbon(aes(x=week_end_date, ymin=lo, ymax=hi, fill=type), alpha=0.4, show.legend = F) +
  geom_segment(aes(x=ymd("2020-03-08"), xend=ymd("2020-05-09"), y=3, yend=3), colour="gray40",
               arrow = arrow(angle = 20, unit(0.1, "inches"), ends="both", type="closed")) +
  geom_hline(yintercept = 1, lty = 2) +
  annotate("text", x=ymd("2020-04-11"), y=4, label = "First wave", size=2.5, colour="gray40") +
  annotate("text", x=ymd("2020-05-01"), y=12, 
           label = "Ratio = Weekly \nnumber over \nmean weekly\nnumber during\nfirst wave", 
           size=2.5, colour="gray40") +
  scale_y_continuous(breaks = 0:6 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "Ratio", title = "(a) Overall case, hospitalization and death ratios") +
  scale_colour_manual(name = NA, values = c("black", "blue", "red"),
                      labels = c("Case", "Hospitalized", "Death")) +
  scale_fill_manual(name = NA, values = c("black", "blue", "red"),
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
  geom_ribbon(aes(x=week_end_date, ymin=lo, ymax=hi, fill=age_grp), alpha=0.4) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous(breaks = 0:8 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "", title = "(c) Case ratios by age group") +
  scale_colour_manual(values = pal[2:4]) +
  scale_fill_manual(values = pal[2:4]) +
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
  geom_ribbon(aes(x=week_end_date, ymin=lo, ymax=hi, fill=age_grp), alpha=0.4, show.legend = F) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous(breaks = 0:8 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "", title = "(c) Case ratios by age group") +
  scale_colour_manual(values = pal[2:4]) +
  scale_fill_manual(values = pal[2:4]) +
  ggpubr::theme_pubclean(base_size = 9) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(1.0)))

p4 <- chd_all %>%
  filter(type == "dr") %>%
  filter(age_grp != "All ages") %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=ratio, colour=age_grp), lwd=1.0, show.legend = F) +
  geom_ribbon(aes(x=week_end_date, ymin=lo, ymax=hi, fill=age_grp), alpha=0.4, show.legend = F) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous(breaks = 0:8 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "", title = "(d) Death ratios by age group") +
  scale_colour_manual(values = pal[2:4]) +
  scale_fill_manual(values = pal[2:4]) +
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

ggsave("fig/ratios_2021.png", g, width = 16, height = 17, units = "cm")
ggsave("fig/ratios_2021.eps", g, device = cairo_ps,
       width = 16, height = 17, units = "cm", dpi = 800)
