read_ll <- function () {
  ll <- data.table::fread("data/linelist_latest.csv")
  ll$Case1 <- as.POSIXct(ll$Case1/1000, origin="1970-01-01", tz="UTC") %>%
    format(format = "%Y-%m-%d %H:%M:%S")
  ll$EventDate <- ymd_hms(ll$EventDate) %>% as.Date()
  ll$ChartDate <- ymd_hms(ll$ChartDate) %>% as.Date()
  ll$Age <- as.numeric(ll$Age)
  
  return(ll)
}

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

make_chd_all <- function (ll) {
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
  
  return(chd_all)
}

