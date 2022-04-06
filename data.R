library(tidyverse)
library(googlesheets4)
library(googledrive)
library(gargle)
library(curl)
library(lubridate)

options(gargle_oauth_cache = ".kawalcovidauth", gargle_oauth_email = "mail.aldilas@gmail.com")
gs4_auth(cache = ".kawalcovidauth")
data <- range_speedread("1ma1T9hWbec1pXlwZ89WakRk-OfVUQZsOCFl4FwZxzVw", range = "Timeline!B:AI", na = "")
n <- rowSums(is.na(data)) == ncol(data)
s <- split(subset(data, !n), cumsum(n)[!n])

stat <- read_sheet("1ma1T9hWbec1pXlwZ89WakRk-OfVUQZsOCFl4FwZxzVw", sheet = "Statistik Harian") %>% rename(date = 1, case_daily = 2, case_cum = 5, recovered_daily = 8, recovered_cum = 9, death_daily = 12, death_cum = 13, test_daily = 25)
stat$date <- as.Date(stat$date, format = "%d %b")

# Data provinsi
provCaseCum <- s$`0`
provCaseCum$date <- seq(ymd("2020-03-18"), by = "days", length.out = nrow(provCaseCum))
provCaseCum <- pivot_longer(provCaseCum, cols = -date, names_to = "provinsi", values_to = "case.cum")

provCaseDaily <- bind_rows(s$`1`, s$`2`)
provCaseDaily <- provCaseDaily[-1,]
provCaseDaily$date <- seq(ymd("2020-03-15"), by = "days", length.out = nrow(provCaseDaily))
provCaseDaily <- mutate_each(provCaseDaily, funs(if(is.character(.)) as.numeric(.) else .))
provCaseDaily[is.na(provCaseDaily)] <- 0
provCaseDaily <- pivot_longer(provCaseDaily, cols = -date, names_to = "provinsi", values_to = "case.daily")

provActiveDaily <- s$`3`
provActiveDaily <- provActiveDaily[-1,]
provActiveDaily$date <- seq(ymd("2020-03-21"), by = "days", length.out = nrow(provActiveDaily))
provActiveDaily <- pivot_longer(provActiveDaily, cols = -date, names_to = "provinsi", values_to = "active.daily")

provRecoverCum <- s$`4`
provRecoverCum <- provRecoverCum[-1,]
provRecoverCum$date <- seq(ymd("2020-03-21"), by = "days", length.out = nrow(provRecoverCum))
provRecoverCum <- mutate_each(provRecoverCum, funs(if(is.character(.)) as.numeric(.) else .))
provRecoverCum[is.na(provRecoverCum)] <- 0
provRecoverCum <- pivot_longer(provRecoverCum, cols = -date, names_to = "provinsi", values_to = "recover.cum")

provRecoverDaily <- bind_rows(s$'5', s$`6`)
provRecoverDaily <- provRecoverDaily[-1,]
provRecoverDaily$date <- seq(ymd("2020-03-21"), by = "days", length.out = nrow(provRecoverDaily))
provRecoverDaily <- mutate_each(provRecoverDaily, funs(if(is.character(.)) as.numeric(.) else .))
provRecoverDaily[is.na(provRecoverDaily)] <- 0
provRecoverDaily <- pivot_longer(provRecoverDaily, cols = -date, names_to = "provinsi", values_to = "recover.daily")

provDeathCum <- s$`7`
provDeathCum <- provDeathCum[-1,]
provDeathCum$date <- seq(ymd("2020-03-18"), by = "days", length.out = nrow(provDeathCum))
provDeathCum <- mutate_each(provDeathCum, funs(if(is.character(.)) as.numeric(.) else .))
provDeathCum[is.na(provDeathCum)] <- 0
provDeathCum <- pivot_longer(provDeathCum, cols = -date, names_to = "provinsi", values_to = "death.cum")

provDeathDaily <- s$`8`
provDeathDaily <- provDeathDaily[-1,]
provDeathDaily$date <- seq(ymd("2020-03-21"), by = "days", length.out = nrow(provDeathDaily))
provDeathDaily <- mutate_each(provDeathDaily, funs(if(is.character(.)) as.numeric(.) else .))
provDeathDaily[is.na(provDeathDaily)] <- 0
provDeathDaily <- pivot_longer(provDeathDaily, cols = -date, names_to = "provinsi", values_to = "death.daily")

prov.dat <- full_join(provCaseCum, provCaseDaily, by = c("date", "provinsi")) %>% 
  full_join(provActiveDaily, by = c("date", "provinsi")) %>% 
  full_join(provRecoverCum, by = c("date", "provinsi")) %>% 
  full_join(provRecoverDaily, by = c("date", "provinsi")) %>% 
  full_join(provDeathCum, by = c("date", "provinsi")) %>% 
  full_join(provDeathDaily, by = c("date", "provinsi"))

provCaseDat <- read_sheet("1ma1T9hWbec1pXlwZ89WakRk-OfVUQZsOCFl4FwZxzVw", range = "Kasus per Provinsi!B2:H38", col_names = TRUE)
provCaseDat[4,1] <- "Kepulauan Bangka Belitung"
provCaseDat[6,1] <- "Daerah Istimewa Yogyakarta"
provCaseDat[7,1] <- "Daerah Khusus Ibukota Jakarta"
