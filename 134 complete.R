setwd("C:/Users/emily_2vx2daj/OneDrive/Documents/Jinternship Work/R work/Practice/Social Anxiety")

#library
library(tidyverse)
library(readxl)
library(rstudioapi)
library(purrr)
library(mdatools)

#expand visual
writeRStudioPreference("data_view_max_columns", 1000L)

#read file
ex2 <- read_csv("134_practice.csv")


#renaming vars
ex2 <-  ex2 %>% 
  rename("fair" = "175") %>% 
  rename("medium" = "176") %>% 
  rename("unfair" = "177") %>% 
  rename("rej0_acc1" = "178") %>% 
  rename("reac_time" = "185")

#change observations into numeric forms
ex3<- ex2 %>% 
  mutate(fair = replace(fair, !is.na(fair), 1)) %>% 
  mutate(medium = replace(medium, !is.na(medium), 2)) %>% 
  mutate(unfair = replace(unfair, !is.na(unfair), 3))

ex3 <- ex3[-c(1),]

#percent accepted + fair
acc_fair <- data.frame(na.omit(ex3$rej0_acc1 == 1 & ex3$fair == 1))

per_acc_fair <- sum(acc_fair)/sum(na.omit(as.numeric(ex3$fair))) * 100

#percent accepted + medium
acc_med <- data.frame(na.omit(ex3$rej0_acc1 == 1 & ex3$medium == 1))
sum(acc_med)

per_acc_med <- sum(acc_med)/sum(na.omit(as.numeric(ex3$medium))) * 100

#percent accepted unfair
acc_unfair <- data.frame(na.omit(ex3$rej0_acc1 == 1 & ex3$unfair == 1))

per_acc_unfair <- sum(acc_unfair)/sum(na.omit(as.numeric(ex3$unfair))) * 100

#create zoomed in df
crop_ex <- ex3[,c(175:185)]
crop_ex <- crop_ex[-c(1:6, 43:61),]

#avg reaction time for fair
reac_fair <- crop_ex %>% filter(fair == "1")

nrow(reac_fair)
sum(as.numeric(reac_fair$reac_time))

avg_reac_fair <- sum(as.numeric(reac_fair$reac_time))/nrow(reac_fair)

#avg reaction time for medium
reac_medium <- crop_ex %>% filter(medium == "2")

nrow(reac_medium)
sum(as.numeric(reac_medium$reac_time))

avg_reac_medium <- sum(as.numeric(reac_medium$reac_time))/nrow(reac_medium)

#avg reaction time for unfair 
reac_unfair <- reac_medium <- crop_ex %>% filter(unfair == "3")

nrow(reac_unfair)
sum(as.numeric(reac_unfair$reac_time))

avg_reac_unfair <- sum(as.numeric(reac_unfair$reac_time))/nrow(reac_unfair)

#avg rt fair accepted
rt_fair_acc <- crop_ex %>% filter(fair == "1", rej0_acc1 == "1")

nrow(rt_fair_acc)
sum(as.numeric(rt_fair_acc$reac_time))

avg_rt_fair_acc <- sum(as.numeric(rt_fair_acc$reac_time))/nrow(rt_fair_acc)

#avg rt fair rejected 
rt_fair_rej <- crop_ex %>% filter(fair == "1", rej0_acc1 == "0")

nrow(rt_fair_rej)
sum(as.numeric(rt_fair_rej$reac_time))

avg_rt_fair_rej <- sum(as.numeric(rt_fair_rej$reac_time))/nrow(rt_fair_rej)

#avg rt med accepted
rt_med_acc <- crop_ex %>% filter(medium == "2", rej0_acc1 == "1")

nrow(rt_med_acc)
sum(as.numeric(rt_med_acc$reac_time))

avg_rt_med_acc <- sum(as.numeric(rt_med_acc$reac_time))/nrow(rt_med_acc)

#avg rt med rejected
rt_med_rej <- crop_ex %>% filter(medium == "2", rej0_acc1 == "0")

nrow(rt_med_rej)
sum(as.numeric(rt_med_rej$reac_time))

avg_rt_med_rej <- sum(as.numeric(rt_med_rej$reac_time))/nrow(rt_med_rej)

#avg rt unfair accepted 
rt_unfair_acc <- crop_ex %>% filter(unfair == "3", rej0_acc1 == "1")

nrow(rt_unfair_acc)
sum(as.numeric(rt_unfair_acc$reac_time))

avg_rt_unfair_acc <- sum(as.numeric(rt_unfair_acc$reac_time))/nrow(rt_unfair_acc)

#avg rt unfair rejected
rt_unfair_rej <- crop_ex %>% filter(unfair == "3", rej0_acc1 == "0")

nrow(rt_unfair_rej)
sum(as.numeric(rt_unfair_rej$reac_time))

avg_rt_unfair_rej <- sum(as.numeric(rt_unfair_rej$reac_time))/nrow(rt_unfair_rej)

#avg rt accepted
ave_rt_accepted <- crop_ex %>% filter(rej0_acc1 == "1")

nrow(ave_rt_accepted)

avg_rt_accepted <- sum(as.numeric(ave_rt_accepted$reac_time))/nrow(ave_rt_accepted)

#avg rt rejected
ave_rt_rejected <- crop_ex %>% filter(rej0_acc1 == "0")

nrow(ave_rt_rejected)
sum(as.numeric(ave_rt_rejected$reac_time))

avg_rt_rejected <- sum(as.numeric(ave_rt_rejected$reac_time))/nrow(ave_rt_rejected)

#sum gain fair
ex3 <- ex3 %>% rename("yours" = "163")

sum_g_fair <- ex3 %>% filter(fair == "1", rej0_acc1 == "1")

sum_gain_fair <- sum(as.numeric(sum_g_fair$yours))

#sum gain medium
sum_g_medium <- ex3 %>% filter(medium == "2", rej0_acc1 == "1")

sum_gain_medium <- sum(as.numeric(sum_g_medium$yours))

#sum gain unfair
sum_g_unfair <- ex3 %>% filter(unfair == "3", rej0_acc1 == "1")

sum_gain_unfair <- sum(as.numeric(sum_g_unfair$yours))

#sum gain total
sum_gain_total <- sum(as.numeric(sum_gain_fair,sum_gain_medium,sum_gain_unfair))

#rank money motivation
ex3 <- ex3 %>% rename("rm_motivation" = "90")

ex3$rm_motivation <- gsub("\\{ENTER\\}", "", ex3$rm_motivation)
rank_money_motivation <- ex3$rm_motivation[1]


#rank money subjective
rank_money_sub_val <- ex3$rm_motivation[2]

#anger before 
ex3 <- ex3 %>% rename("angr_bfr" = "46")
angr_bfr <- na.omit(ex3$angr_bfr)

#fear before
ex3 <- ex3 %>% rename("fear_bfr" = "79")
fear_bfr <- na.omit(ex3$fear_bfr)

#happiness before
ex3 <- ex3 %>% rename("happiness_bfr" = "81")
happiness_bfr <- na.omit(ex3$happiness_bfr)

#sadness before
ex3 <- ex3 %>% rename("sadness_bfr" = "99")
sadness_bfr <- na.omit(ex3$sadness_bfr)

#anger after 
ex3 <- ex3 %>% rename("angr_afr" = "45")
anger_after <- na.omit(ex3$angr_afr)

#fear after
ex3 <- ex3 %>% rename("fear_afr" = "78")
fear_after <- na.omit(ex3$fear_afr)

#happiness after
ex3 <- ex3 %>% rename("happiness_afr" = "80")
happiness_after <- na.omit(ex3$happiness_afr)

#sadness after
ex3 <- ex3 %>% rename("sadness_afr" = "98")
sadness_after <- na.omit(ex3$sadness_afr)

#anger fair
ex3 <- ex3 %>% rename("angr_fair" = "105")
anger_fair <- na.omit(ex3$angr_fair)

#fear fair
ex3 <- ex3 %>% rename("fear_fair" = "135")
fear_fair <- na.omit(ex3$fear_fair)

#happiness fair
ex3 <- ex3 %>% rename("happiness_fair" = "148")
happiness_fair <- na.omit(ex3$happiness_fair)

#sadness fair
ex3 <- ex3 %>% rename("sadness_fair" = "197")
sadness_fair <- na.omit(ex3$sadness_fair)

#fairness fair
ex3 <- ex3 %>% rename("fairness_fair" = "123")
fairness_fair <- na.omit(ex3$fairness_fair)

#anger medium
ex3 <- ex3 %>% rename("anger_medium" = "106")
anger_medium <- na.omit(ex3$anger_medium)

#Fear_medium
ex3 <- ex3 %>% rename("fear_medium" = "136")
fear_medium <- na.omit(ex3$fear_medium)

#Happiness_medium
ex3 <- ex3 %>% rename("happiness_medium" = "149")
happiness_medium <- na.omit(ex3$happiness_medium)

#Sadness_medium
ex3 <- ex3 %>% rename("sadness_medium" = "198")
sadness_medium <- na.omit(ex3$sadness_medium)

#Fairness_medium
ex3 <- ex3 %>% rename("fairness_medium" = "124")
fairness_medium <- na.omit(ex3$fairness_medium)

#Anger_unfair
ex3 <- ex3 %>% rename("anger_unfair" = "107")
anger_unfair <- na.omit(ex3$anger_unfair)

#Fear_unfair
ex3 <- ex3 %>% rename("fear_unfair" = "137")
fear_unfair <- na.omit(ex3$fear_unfair)

#Happiness_unfair
ex3 <- ex3 %>% rename("happiness_unfair" = "150")
happiness_unfair <- na.omit(ex3$happiness_unfair)

#Sadness_unfair
ex3 <- ex3 %>% rename("sadness_unfair" = "199")
sadness_unfair <- na.omit(ex3$sadness_unfair)

#Fairness_unfair
ex3 <- ex3 %>% rename("fairness_unfair" = "125")
fairness_unfair <- na.omit(ex3$fairness_unfair)

#offer differences
ex3 <- ex3 %>% rename("player" = "162")

#total of all %s
running_total <- data.frame(rbind(per_acc_fair, 
                                  per_acc_med, 
                                  per_acc_unfair, 
                                  avg_reac_fair, 
                                  avg_reac_medium, 
                                  avg_reac_unfair, 
                                  avg_rt_fair_acc, 
                                  avg_rt_fair_rej, 
                                  avg_rt_med_acc, 
                                  avg_rt_med_rej, 
                                  avg_rt_unfair_acc,
                                  avg_rt_unfair_rej, 
                                  avg_rt_accepted,
                                  avg_rt_rejected,
                                  sum_gain_fair,
                                  sum_gain_medium,
                                  sum_gain_unfair,
                                  sum_gain_total,
                                  rank_money_motivation,
                                  rank_money_sub_val,
                                  angr_bfr,
                                  fear_bfr,
                                  happiness_bfr,
                                  sadness_bfr,
                                  anger_after,
                                  fear_after,
                                  happiness_after,
                                  sadness_after,
                                  anger_fair,
                                  fear_fair,
                                  happiness_fair,
                                  sadness_fair,
                                  fairness_fair,
                                  anger_medium,
                                  fear_medium,
                                  happiness_medium,
                                  sadness_medium,
                                  fairness_medium,
                                  anger_unfair,
                                  fear_unfair,
                                  happiness_unfair,
                                  sadness_unfair,
                                  fairness_unfair))

#offer differences

test <- data.frame(na.omit(as.numeric(ex3$player)) - na.omit(as.numeric(ex3$yours)))

#attaching offer diff to running total
row.names(test)
row.names(running_total)

colnames(running_total)
colnames(test)

#duplicate column name to commence rbind
running_total <- running_total %>% rename("running_total" = "rbind.per_acc_fair..per_acc_med..per_acc_unfair..avg_reac_fair..")


#rbind 
test <- test %>% rename("running_total" = "na.omit.as.numeric.ex3.player.....na.omit.as.numeric.ex3.yours..")

official_running_total <- rbind(running_total, test)


#offer diff title
row.names(official_running_total)[44] <-  "Offer Differences"

#df for offers sequence
combo <- data.frame(coalesce(ex3$fair, ex3$medium, ex3$unfair))

combo <- combo %>%
  mutate(coalesce.ex3.fair..ex3.medium..ex3.unfair. =
           str_replace(coalesce.ex3.fair..ex3.medium..ex3.unfair., "1", "fair")) %>%
  mutate(coalesce.ex3.fair..ex3.medium..ex3.unfair. =
           str_replace(coalesce.ex3.fair..ex3.medium..ex3.unfair., "2", "medium")) %>%
  mutate(coalesce.ex3.fair..ex3.medium..ex3.unfair. =
           str_replace(coalesce.ex3.fair..ex3.medium..ex3.unfair., "3", "unfair"))

combo <- combo[-c(1:6, 43:61),]
combo <- data.frame(combo)

combo <- combo %>% rename("running_total" = "combo")

official_running_total <- rbind(official_running_total, combo)

row.names(official_running_total)[80] <- "Offers Sequence"

view(official_running_total)
