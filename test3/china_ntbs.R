rm(list=ls())

# Set data directory ------------------------------------------------------



user <- "john"

if(user == "john"){
  DATA_DIR <- "/Users/John/Dropbox (MIT)/china_bilateral/china_ntbs_trade/"
  DATA_DIR2 <- "/Users/John/Dropbox (MIT)/china_imports/"
}

setwd("/Users/John/Dropbox (MIT)/R_scripts")



# Required packages -------------------------------------------------------

library(tidyr)
library(dplyr)

# Load NTB data

ntb <- as.data.frame(read_excel(paste(DATA_DIR, "china_ntb_complete.xls", sep="")))


ntb <- ntb[, c(2:3, 5:6,8, 10, 12, 21)]


ntb$year_init <- substring(ntb$Initiation, 1, 4)
ntb$year_infor <- substring(ntb$`In force`, 1, 4)
ntb$year_wth <- substring(ntb$Withdrawn, 1, 4)


ntb$year_init[is.na(ntb$year_init)] <- ntb$year_infor[is.na(ntb$year_init)]

colnames(ntb) <- c("country", "type", "init", "infor", "withdwn", "prod_desc", "HS", "mean_mfn",
                   "year_init", "year_infor", "year_wth")

ntb <- ntb %>% select(country, type, HS, year_init, year_infor, year_wth)
ntb <- separate_rows(ntb, HS)

ntb$country[which(ntb$country == "Korea, Republic of")] <- "Republic of Korea"
ntb$country[which(ntb$country == "Saudi Arabia, Kingdom of")] <- "Saudi Arabia"
ntb$country[which(ntb$country == "Chinese Taipei")] <- "Taiwan"

ntb <- filter(ntb, country != "Hong Kong, China, Macao, China")

ntb <- ntb %>% 
  mutate(country=strsplit(country, ",")) %>% 
  unnest(country)

# Create measure of number of NTBs initiated per 
# HS line per year 

# expand to get every possible combination of HS
# codes and years initiated
ntb_expanded <- expand(ntb, HS, year_init)

# now select variables to merge with expanded df including a count variable
# for use in summing ntbs initiated by year and HS
ntb$count <- 1
ntb_restricted <- select(ntb, HS, year_init, count) # 

ntb_merged <- left_join(ntb_expanded, ntb_restricted, by = c("HS" = "HS", "year_init" = "year_init"))

ntb_merged$count[is.na(ntb_merged$count)] <- 0

ntb_merged <- ntb_merged %>% group_by(HS, year_init) %>%
  mutate(sum_ntbs = sum(count)) 

# get rid of duplicate HS-year-sum_ntbs combos
ntb_merged <- unique(ntb_merged)

# Repeat process for ntbs withdrawn

wth_expanded <- expand(ntb, HS, year_wth)
View(wth_expanded)

ntb_wthdwn <- select(ntb, HS, year_wth) # 
View(ntb_wthdwn)

ntb_wthdwn$wth = ifelse(is.na(ntb_wthdwn$year_wth), 0, 1)


wthdwn_merged <- left_join(wthdwn_expanded, ntb_wthdwn, by = c("HS" = "HS", "year_wth" = "year_wth"))
#wthdwn_merged$count[is.na(wthdwn_merged$count)] <- 0

wthdwn_merged <- wthdwn_merged %>% group_by(HS, year_wth) %>%
  mutate(sum_wth = sum(na.omit(wth))) 

wthdwn_merged <- unique(wthdwn_merged)

wthdwn_merged <- filter(wthdwn_merged, year_wth != "NA")
wthdwn_merged <- filter(wthdwn_merged, wth != "NA")

wthdwn_merged <- select(wthdwn_merged, HS, year_wth, sum_wth)

merge_2 <- left_join(ntb_merged, wthdwn_merged, by = c("HS" = "HS", "year_init" = "year_wth"))

merge_2$sum_wth[is.na(merge_2$sum_wth)] <- 0

merge_2 <- filter(merge_2, year_init != "NA")

merge_2 <- merge_2 %>% group_by(HS) %>%
  mutate(cum_ntbs = cumsum(sum_ntbs)) %>%
  mutate(cum_wth = cumsum(sum_wth)) %>%
  mutate(ntb_year = cum_ntbs - cum_wth)

merge_2 <- select(merge_2, HS, year_init, ntb_year, sum_ntbs)
colnames(merge_2)[2] <- "year"
merge_2 <- filter(merge_2, HS != "")
merge_2$year <- as.numeric(merge_2$year)

# get rid of observations with "ex" in HS for now, return to these later

merge_2 <- merge_2[str_detect(merge_2$HS, "ex") == FALSE,]

merge_2$HS2 <- substring(merge_2$HS, 1, 2)
merge_2$HS4 <- substring(merge_2$HS, 1, 4)
merge_2$HS5 <- substring(merge_2$HS, 1, 5)
merge_2$HS6 <- substring(merge_2$HS, 1, 6)

merge_2$HS6 <- ifelse(str_length(merge_2$HS6) < 6, NA, merge_2$HS6)
merge_2$HS5 <- ifelse(str_length(merge_2$HS5) < 5, NA, merge_2$HS5)
merge_2$HS4 <- ifelse(str_length(merge_2$HS4) < 4, NA, merge_2$HS4)

merge_2 <- merge_2 %>% group_by(HS2) %>%
  mutate(sum_HS2 = cumsum(sum_ntbs))

merge_2 <- merge_2 %>% group_by(HS4) %>%
  mutate(sum_HS4 = cumsum(sum_ntbs))

merge_2 <- merge_2 %>% group_by(HS5) %>%
  mutate(sum_HS5 = cumsum(sum_ntbs))

merge_2 <- merge_2 %>% group_by(HS6) %>%
  mutate(sum_HS6 = cumsum(sum_ntbs))

merge_2$sum_HS4 <- ifelse(is.na(merge_2$HS4), NA, merge_2$sum_HS4)
merge_2$sum_HS5 <- ifelse(is.na(merge_2$HS5), NA, merge_2$sum_HS5)
merge_2$sum_HS6 <- ifelse(is.na(merge_2$HS6), NA, merge_2$sum_HS6)



# Load trade data ---------------------------------------------------------

# Load China trade data

china_trade <- as.data.frame(read.csv(paste(DATA_DIR2, "China.csv", sep="")))

china_trade <- filter(china_trade, partner_uncode != 0)

china_trade$HS2 <- substring(china_trade$code, 1, 2)
china_trade$HS4 <- substring(china_trade$code, 1, 4)
china_trade$HS5 <- substring(china_trade$code, 1, 5)
china_trade$HS6 <- substring(china_trade$code, 1, 6)

china_trade <- china_trade %>%
  group_by(HS2,year) %>%
  mutate(sum_imp_hs2 = sum(import))

china_trade <- china_trade %>%
  group_by(HS4,year) %>%
  mutate(sum_imp_hs4 = sum(import))

china_trade <- china_trade %>%
  group_by(HS5,year) %>%
  mutate(sum_imp_hs5 = sum(import))

china_trade <- china_trade %>%
  group_by(HS6,year) %>%
  mutate(sum_imp_hs6 = sum(import)) %>%
  arrange(HS6)

#nrow(china_trade)

ct <- china_trade[,-c(5,6)]

ct <- distinct(ct, year, code, .keep_all = TRUE)

a <- head(merge_2, 10000)
View(a)
b <- head(ct, 10000)
View(b)

merge_3 <- left_join(ct, merge_2, by = c("year" = "year", "HS2" = "HS2"))


c <- head(merge_3, 10000)
View(c)
