rm(list=ls())
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(gridExtra)
source("code/functions.R")

ll <- read_ll()

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

chd_all <- make_chd_all(ll)
chd_all1 <- chd_all %>%
  filter(type %in% c("cr", "dr"))

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
                       levels = c("cr", "dr"))

dates <- rep(ymd("2020-03-01"), 9)
month(dates) <- c(seq(3, 11, by=2), seq(1, 7, by=2))
year(dates) <- c(rep(2020, 5), rep(2021, 4))
dates_label <- c("Mar\n2020", month.abb[seq(5, 11, by=2)], 
                 "Jan\n2021", month.abb[seq(3, 7, by=2)])

p1l <- chd_all %>%
  filter(age_grp == "All ages") %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=ratio, colour=type), size=0.8) +
  geom_ribbon(aes(x=week_end_date, ymin=lo, ymax=hi, fill=type), alpha=0.4) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous(breaks = 0:6 * 4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "Ratio", title = "(a) Overall normalized weekly cases and deaths") +
  scale_colour_manual(name = NA, values = c("black", "red"),
                      labels = c("Case", "Death")) +
  scale_fill_manual(name = NA, values = c("black", "red"),
                    labels = c("Case", "Death")) +
  ggpubr::theme_pubclean(base_size = 11) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(1.0)))

l1 <- ggpubr::get_legend(p1l)

p1 <- chd_all %>%
  filter(age_grp == "All ages") %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=ratio, colour=type), size=0.8, show.legend = F) +
  geom_ribbon(aes(x=week_end_date, ymin=lo, ymax=hi, fill=type), alpha=0.4, show.legend = F) +
  geom_segment(aes(x=ymd("2020-03-08"), xend=ymd("2020-05-09"), y=3, yend=3), colour="gray40",
               arrow = arrow(angle = 20, unit(0.1, "inches"), ends="both", type="closed")) +
  geom_hline(yintercept = 1, lty = 2) +
  annotate("text", x=ymd("2020-04-11"), y=4, label = "First wave", size=3, colour="gray40") +
  annotate("text", x=ymd("2020-07-01"), y=23, 
           label = "Numbers are normalized \nby mean weekly number \nduring the first wave", 
           size=3, colour="gray40") +
  scale_y_continuous(breaks = 0:6 * 4) +
  scale_x_date(breaks = dates, labels = dates_label) +
  labs(x = "", y = "", title = "(a) Normalized weekly cases and deaths") +
  scale_colour_manual(name = NA, values = c("black", "red"),
                      labels = c("Case", "Death")) +
  scale_fill_manual(name = NA, values = c("black", "red"),
                    labels = c("Case", "Death")) +
  ggpubr::theme_pubclean(base_size = 11) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(fill = NA, size = 1),
        # panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(0.95)))

chd_age <- ll %>%
  filter(ChartDate >= ymd("2020-03-01"), ChartDate <= ymd("2021-05-01")) %>%
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
  filter(type != "hosp") %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=med_age, colour=type), size=0.8, show.legend = F) +
  # scale_y_continuous(breaks = 0:4 * 4) +
  scale_x_date(breaks = dates, labels = dates_label) +
  labs(x = "", y = "", title = "(b) Median age") +
  scale_colour_manual(name = NA, values = c("black", "red"),
                      labels = c("Case", "Death")) +
  ggpubr::theme_pubclean(base_size = 11) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(fill = NA, size = 1),
        # panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(0.95)))

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
  scale_x_date(breaks = dates, labels = dates_label) +
  labs(x = "", y = "", title = "(c) Case ratios by age group") +
  scale_colour_manual(values = pal[2:4]) +
  scale_fill_manual(values = pal[2:4]) +
  ggpubr::theme_pubclean(base_size = 11) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(0.95)))

l2 <- ggpubr::get_legend(p3l)

p3 <- chd_all %>%
  filter(type == "cr") %>%
  filter(age_grp != "All ages") %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=ratio, colour=age_grp), lwd=1.0, show.legend = F) +
  geom_ribbon(aes(x=week_end_date, ymin=lo, ymax=hi, fill=age_grp), alpha=0.4, show.legend = F) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous(breaks = 0:8 * 4) +
  scale_x_date(breaks = dates, labels = dates_label) +
  labs(x = "", y = "", title = "(c) Normalized cases by age group") +
  scale_colour_manual(values = pal[2:4]) +
  scale_fill_manual(values = pal[2:4]) +
  ggpubr::theme_pubclean(base_size = 11) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(fill = NA, size = 1),
        # panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(0.95)))

p4 <- chd_all %>%
  filter(type == "dr") %>%
  filter(age_grp != "All ages") %>%
  ggplot() +
  geom_line(aes(x=week_end_date, y=ratio, colour=age_grp), lwd=1.0, show.legend = F) +
  geom_ribbon(aes(x=week_end_date, ymin=lo, ymax=hi, fill=age_grp), alpha=0.4, show.legend = F) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous(breaks = 0:8 * 4) +
  scale_x_date(breaks = dates, labels = dates_label) +
  labs(x = "", y = "", title = "(d) Normalized deaths by age group") +
  scale_colour_manual(values = pal[2:4]) +
  scale_fill_manual(values = pal[2:4]) +
  ggpubr::theme_pubclean(base_size = 11) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(fill = NA, size = 1),
        # panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
        plot.title = element_text(size = rel(1.0)))

# b <- grid::rectGrob(gp = grid::gpar(col=NA))

g <- grid.arrange(
  p1, p2, p3, p4, l1, l2,
  layout_matrix = matrix(c(5, 1, 2, 6, 3, 4), nrow = 3),
  widths = c(8, 8),
  heights = c(1, 8, 8)
)

ggsave("fig/ratios_2021.png", g, width = 16, height = 17, units = "cm")
ggsave("fig/ratios_2021.eps", g, device = cairo_ps,
       width = 16, height = 17, units = "cm", dpi = 800)
