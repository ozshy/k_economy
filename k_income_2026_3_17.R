# Code for actual_v?.tex titled: "Stated Preference and Actual Choice"
# This code uses the 2024 translation-level and individual public data

# Packages used
library(ggplot2); theme_set(theme_bw())# for graphics
#library(mfx)# binomial logit marginal effects
#library(stargazer) # for displaying multinomial coefficients. Does not work with mfx
#library(texreg) # for displaying multinomial coefficients. Works with mfx (unlike stargazer). Also displayes multiple regression.
#library(huxtable)#displays multiple regressions as table => advantage, since the table can be edited in R => Problem: built-in to_latex output does not run in LaTeX in my experience. 
#library(nnet)# multinomial regressions
library(haven)# may be needed to handle haven labelled when data is converted from other software e.g. Stata
library("xtable") #exporting to LaTeX
library(dplyr)# for sample_n
library(ineq)# GINI coefficient and others not weighted
library(wINEQ)# weighted GINI and other ineq measures
#library(gtools)# for stars.pval function

#2025 begins####
setwd("~/SDCPC/2025_SDCPC")# work machine
dir()
# Read transaction dataset
trans2025_1.df = readRDS("dcpc-2025-tranlevel.rds")
dim(trans2025_1.df)
names(trans2025_1.df)

# Read individual dataset
indiv2025_1.df = readRDS("dcpc-2025-indlevel.rds")
dim(indiv2025_1.df)
names(indiv2025_1.df)

# select needed variables from the indiv dataset
indiv2025_2.df = subset(indiv2025_1.df, select = c(uasid, ind_weight, ind_weight_all,  income_hh))
# num of resp with missing a variable
sum(is.na(indiv2025_2.df$income_hh))
sum(is.na(indiv2025_2.df$ind_weight))
sum(is.na(indiv2025_2.df$ind_weight_all))
# => the ALL weights should be used if using the entire sample
# remove ind_weight
names(indiv2025_2.df)
dim(indiv2025_2.df)
indiv2025_3.df = subset(indiv2025_2.df, select = -ind_weight)
names(indiv2025_3.df)
dim(indiv2025_3.df)

# Remove NAs 
dim(indiv2025_3.df)
indiv2025_4.df = indiv2025_3.df[complete.cases(indiv2025_3.df), ]
dim(indiv2025_4.df)

# merge the indiv with the trans datasets by id
m1.df = left_join(trans2025_1.df, indiv2025_4.df, by = "uasid")
dim(m1.df)# num of payments (transactions)

# select only the needed variables
m2.df = subset(m1.df, select = c(uasid, ind_weight_all, amnt, income_hh, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$ind_weight_all))
sum(is.na(m2.df$uasid))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))

# delete obs with NAs (missing HH income & merch)
dim(m2.df)# num payments before
m3.df = m2.df[complete.cases(m2.df), ]
dim(m3.df)# num payments after

# adjusting the weights (new weights variable: w)
sum(m3.df$ind_weight_all)
m3.df$w = nrow(m3.df)*m3.df$ind_weight_all/sum(m3.df$ind_weight_all)
sum(m3.df$w)# should sum up to nrow

# Calculate Gini coefficient
(gini_2025_w = Gini(m3.df$income_hh, m3.df$w))# weighted
(gini_2025 = ineq(m3.df$income_hh, type = "Gini"))# unweighted

#(theil_2025_w = Theil_T(m3.df$income_hh, m3.df$w))# weighted
#(theil_2025_ = ineq(m3.df$income_hh, type = "Theil"))# weighted

## For the table of merchant type calculations 2025
table(m3.df$merch)# number of transactions by merch type
(merch_obs_2025 = m3.df %>% group_by(merch) %>% summarise(n()))
#
(merch_obs_2025.vec = as.numeric((merch_obs_2025$`n()`)))
#
# compute avg spending by merch type
(merch_total_spend_2025 = m3.df %>% group_by(merch) %>% summarise(sum(amnt)))
(merch_total_spend_2025.vec = as.numeric(merch_total_spend_2025$`sum(amnt)`))
#
(merch_avg_spend_2025.vec = merch_total_spend_2025.vec/merch_obs_2025.vec)

#2025 ends####

#+++++++++++++++++++

#2024 begins####
setwd("~/SDCPC/2024_SDCPC")# work machine
dir()
# Read transaction dataset
trans2024_1.df = readRDS("dcpc-2024-tranlevel-public.rds")
dim(trans2024_1.df)
names(trans2024_1.df)

# Read individual dataset
indiv2024_1.df = readRDS("dcpc-2024-indlevel-public.rds")
dim(indiv2024_1.df)
names(indiv2024_1.df)

# select needed variables from the indiv dataset
indiv2024_2.df = subset(indiv2024_1.df, select = c(id, ind_weight, ind_weight_all,  income_hh))
# num of resp with missing a variable
sum(is.na(indiv2024_2.df$income_hh))
sum(is.na(indiv2024_2.df$ind_weight))
sum(is.na(indiv2024_2.df$ind_weight_all))
# => the ALL weights should be used if using the entire sample
# remove ind_weight
names(indiv2024_2.df)
dim(indiv2024_2.df)
indiv2024_3.df = subset(indiv2024_2.df, select = -ind_weight)
names(indiv2024_3.df)
dim(indiv2024_3.df)

# Remove NAs 
dim(indiv2024_3.df)
indiv2024_4.df = indiv2024_3.df[complete.cases(indiv2024_3.df), ]
dim(indiv2024_4.df)

# merge the indiv with the trans datasets by id
m1.df = left_join(trans2024_1.df, indiv2024_4.df, by = "id")
dim(m1.df)# num of payments (transactions)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, ind_weight_all, amnt, income_hh, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$ind_weight_all))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))

# delete obs with NAs (missing HH income & merch)
dim(m2.df)# num payments before
m3.df = m2.df[complete.cases(m2.df), ]
dim(m3.df)# num payments after

# adjusting the weights (new weights variable: w)
sum(m3.df$ind_weight_all)
m3.df$w = nrow(m3.df)*m3.df$ind_weight_all/sum(m3.df$ind_weight_all)
sum(m3.df$w)# should sum up to nrow

# Calculate Gini coefficient
(gini_2024_w = Gini(m3.df$income_hh, m3.df$w))# weighted
(gini_2024 = ineq(m3.df$income_hh, type = "Gini"))# unweighted

#(theil_2024_w = Theil_T(m3.df$income_hh, m3.df$w))# weighted
#(theil_2024_ = ineq(m3.df$income_hh, type = "Theil"))# weighted

## For the table of merchant type calculations 2024
table(m3.df$merch)# number of transactions by merch type
(merch_obs_2024 = m3.df %>% group_by(merch) %>% summarise(n()))
#
(merch_obs_2024.vec = as.numeric((merch_obs_2024$`n()`)))
#
# compute avg spending by merch type
(merch_total_spend_2024 = m3.df %>% group_by(merch) %>% summarise(sum(amnt)))
(merch_total_spend_2024.vec = as.numeric(merch_total_spend_2024$`sum(amnt)`))
#
(merch_avg_spend_2024.vec = merch_total_spend_2024.vec/merch_obs_2024.vec)

#2024 ends####

#+++++++++++++++++++

#2023 begins####
setwd("~/SDCPC/2023_SDCPC")
dir()
# Read transaction dataset
trans2023_1.df = readRDS("dcpc-2023-tranlevel-public.rds")
dim(trans2023_1.df)
names(trans2023_1.df)

# Read individual dataset
indiv2023_1.df = readRDS("dcpc-2023-indlevel-public.rds")
dim(indiv2023_1.df)
names(indiv2023_1.df)

# select needed variables from the indiv dataset
indiv2023_2.df = subset(indiv2023_1.df, select = c(id, ind_weight, ind_weight_all,  income_hh))
# num of resp with missing a variable
sum(is.na(indiv2023_2.df$income_hh))
sum(is.na(indiv2023_2.df$ind_weight))
sum(is.na(indiv2023_2.df$ind_weight_all))
# => the ALL weights should be used if using the entire sample
# remove ind_weight
names(indiv2023_2.df)
dim(indiv2023_2.df)
indiv2023_3.df = subset(indiv2023_2.df, select = -ind_weight)
names(indiv2023_3.df)
dim(indiv2023_3.df)

# Remove NAs 
dim(indiv2023_3.df)
indiv2023_4.df = indiv2023_3.df[complete.cases(indiv2023_3.df), ]
dim(indiv2023_4.df)

# merge the indiv with the trans datasets by id
m1.df = left_join(trans2023_1.df, indiv2023_4.df, by = "id")
dim(m1.df)# num of payments (transactions)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, ind_weight_all, amnt, income_hh, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$ind_weight_all))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))

# delete obs with NAs (missing HH income & merch)
dim(m2.df)# num payments before
m3.df = m2.df[complete.cases(m2.df), ]
dim(m3.df)# num payments after

# adjusting the weights (new weights variable: w)
sum(m3.df$ind_weight_all)
m3.df$w = nrow(m3.df)*m3.df$ind_weight_all/sum(m3.df$ind_weight_all)
sum(m3.df$w)# should sum up to nrow

# Calculate Gini coefficient
(gini_2023_w = Gini(m3.df$income_hh, m3.df$w))# weighted
(gini_2023 = ineq(m3.df$income_hh, type = "Gini"))# unweighted

#(theil_2023_w = Theil_T(m3.df$income_hh, m3.df$w))# weighted
#(theil_2023_ = ineq(m3.df$income_hh, type = "Theil"))# weighted

## For the table of merchant type calculations 2023
table(m3.df$merch)# number of transactions by merch type
(merch_obs_2023 = m3.df %>% group_by(merch) %>% summarise(n()))
#
(merch_obs_2023.vec = as.numeric((merch_obs_2023$`n()`)))
#
# compute avg spending by merch type
(merch_total_spend_2023 = m3.df %>% group_by(merch) %>% summarise(sum(amnt)))
(merch_total_spend_2023.vec = as.numeric(merch_total_spend_2023$`sum(amnt)`))
#
(merch_avg_spend_2023.vec = merch_total_spend_2023.vec/merch_obs_2023.vec)

#2023 ends####

#+++++++++++++++++++

#2022 begins####
setwd("~/SDCPC/2022_DCPC")
dir()
# Read transaction dataset
trans2022_1.df = readRDS("dcpc-2022-tranlevel-public.rds")
dim(trans2022_1.df)
names(trans2022_1.df)

# Read individual dataset
indiv2022_1.df = readRDS("dcpc-2022-indlevel-public.rds")
dim(indiv2022_1.df)
names(indiv2022_1.df)

# select needed variables from the indiv dataset
indiv2022_2.df = subset(indiv2022_1.df, select = c(id, ind_weight, ind_weight_all,  income_hh))
# num of resp with missing a variable
sum(is.na(indiv2022_2.df$income_hh))
sum(is.na(indiv2022_2.df$ind_weight))
sum(is.na(indiv2022_2.df$ind_weight_all))
# => the ALL weights should be used if using the entire sample
# remove ind_weight
names(indiv2022_2.df)
dim(indiv2022_2.df)
indiv2022_3.df = subset(indiv2022_2.df, select = -ind_weight)
names(indiv2022_3.df)
dim(indiv2022_3.df)

# Remove NAs 
dim(indiv2022_3.df)
indiv2022_4.df = indiv2022_3.df[complete.cases(indiv2022_3.df), ]
dim(indiv2022_4.df)

# merge the indiv with the trans datasets by id
m1.df = left_join(trans2022_1.df, indiv2022_4.df, by = "id")
dim(m1.df)# num of payments (transactions)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, ind_weight_all, amnt, income_hh, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$ind_weight_all))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))

# delete obs with NAs (missing HH income & merch)
dim(m2.df)# num payments before
m3.df = m2.df[complete.cases(m2.df), ]
dim(m3.df)# num payments after

# adjusting the weights (new weights variable: w)
sum(m3.df$ind_weight_all)
m3.df$w = nrow(m3.df)*m3.df$ind_weight_all/sum(m3.df$ind_weight_all)
sum(m3.df$w)# should sum up to nrow

# Calculate Gini coefficient
(gini_2022_w = Gini(m3.df$income_hh, m3.df$w))# weighted
(gini_2022 = ineq(m3.df$income_hh, type = "Gini"))# unweighted

#(theil_2022_w = Theil_T(m3.df$income_hh, m3.df$w))# weighted
#(theil_2022_ = ineq(m3.df$income_hh, type = "Theil"))# weighted

## For the table of merchant type calculations 2022
table(m3.df$merch)# number of transactions by merch type
(merch_obs_2022 = m3.df %>% group_by(merch) %>% summarise(n()))
#
(merch_obs_2022.vec = as.numeric((merch_obs_2022$`n()`)))
#
# compute avg spending by merch type
(merch_total_spend_2022 = m3.df %>% group_by(merch) %>% summarise(sum(amnt)))
(merch_total_spend_2022.vec = as.numeric(merch_total_spend_2022$`sum(amnt)`))
#
(merch_avg_spend_2022.vec = merch_total_spend_2022.vec/merch_obs_2022.vec)

#2022 ends####

#+++++++++++++++++++
#
#2021 begins####
setwd("~/SDCPC/2021_DCPC")
dir()
# Read transaction dataset
trans2021_1.df = readRDS("dcpc-2021-tranlevel-public.rds")
dim(trans2021_1.df)
names(trans2021_1.df)

# Read individual dataset
indiv2021_1.df = readRDS("dcpc-2021-indlevel-public.rds")
dim(indiv2021_1.df)
names(indiv2021_1.df)

# select needed variables from the indiv dataset
indiv2021_2.df = subset(indiv2021_1.df, select = c(id, ind_weight, ind_weight_all,  income_hh))
# num of resp with missing a variable
sum(is.na(indiv2021_2.df$income_hh))
sum(is.na(indiv2021_2.df$ind_weight))
sum(is.na(indiv2021_2.df$ind_weight_all))
# => the ALL weights should be used if using the entire sample
# remove ind_weight
names(indiv2021_2.df)
dim(indiv2021_2.df)
indiv2021_3.df = subset(indiv2021_2.df, select = -ind_weight)
names(indiv2021_3.df)
dim(indiv2021_3.df)

# Remove NAs 
dim(indiv2021_3.df)
indiv2021_4.df = indiv2021_3.df[complete.cases(indiv2021_3.df), ]
dim(indiv2021_4.df)

# merge the indiv with the trans datasets by id
m1.df = left_join(trans2021_1.df, indiv2021_4.df, by = "id")
dim(m1.df)# num of payments (transactions)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, ind_weight_all, amnt, income_hh, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$ind_weight_all))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))

# delete obs with NAs (missing HH income & merch)
dim(m2.df)# num payments before
m3.df = m2.df[complete.cases(m2.df), ]
dim(m3.df)# num payments after

# adjusting the weights (new weights variable: w)
sum(m3.df$ind_weight_all)
m3.df$w = nrow(m3.df)*m3.df$ind_weight_all/sum(m3.df$ind_weight_all)
sum(m3.df$w)# should sum up to nrow

# Calculate Gini coefficient
(gini_2021_w = Gini(m3.df$income_hh, m3.df$w))# weighted
(gini_2021 = ineq(m3.df$income_hh, type = "Gini"))# unweighted

#(theil_2021_w = Theil_T(m3.df$income_hh, m3.df$w))# weighted
#(theil_2021_ = ineq(m3.df$income_hh, type = "Theil"))# weighted

## For the table of merchant type calculations 2021
table(m3.df$merch)# number of transactions by merch type
(merch_obs_2021 = m3.df %>% group_by(merch) %>% summarise(n()))
#
(merch_obs_2021.vec = as.numeric((merch_obs_2021$`n()`)))
#
# compute avg spending by merch type
(merch_total_spend_2021 = m3.df %>% group_by(merch) %>% summarise(sum(amnt)))
(merch_total_spend_2021.vec = as.numeric(merch_total_spend_2021$`sum(amnt)`))
#
(merch_avg_spend_2021.vec = merch_total_spend_2021.vec/merch_obs_2021.vec)

#2021 ends####

#+++++++++++++++++

# Comparisons of inequality Gini ####
(gini_income.vec = c(gini_2021, gini_2022, gini_2023, gini_2024, gini_2025))
(gini_income_w.vec = c(gini_2021_w, gini_2022_w, gini_2023_w, gini_2024_w, gini_2025_w))

# Put in a data frame
(ineq.df = data.frame(year.vec = 2021:2025, gini_income.vec, gini_income_w.vec))

# Plotting Gini 
# searching for lower and upper bounds
min(gini_income_w.vec, gini_income.vec)
max(gini_income_w.vec, gini_income.vec)


ggplot(ineq.df, aes(x=year.vec, y=gini_income_w.vec)) + geom_line(linewidth = 1.0) +geom_point(size=1.9) + geom_line(aes(y=gini_income.vec), linewidth = 1.0, color="red", linetype="longdash") +geom_point(aes(y=gini_income.vec), size=1.9, color="red") + scale_x_continuous(breaks = 2021:2025) +labs(x="Year", y="Gini HH income inequality index") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20))  + scale_y_continuous(breaks = seq(0.4,0.55, 0.01)) +annotate("text", x = 2023, y = 0.432, label = "Weighted", angle = 0,  color = "black", size = 6, hjust = 0) +annotate("text", x = 2023, y = 0.465, label = "Unweighted", angle = 0,  color = "red", size = 6, hjust = 0)



#+ geom_line(aes(y=gini_necessities.vec), linewidth = 1.0, color="red", linetype="longdash") +geom_point(aes(y=gini_necessaties.vec), size=1.9, color="red") +geom_line(aes(y=gini_discretionary.vec), linewidth = 1.0, color="blue", linetype="dotdash") +geom_point(aes(y=gini_discretionary.vec), size=1.9, color="blue") +annotate("text", x = 2021, y = 0.23,           label = "All spending", angle = 48,  color = "black", size = 6, hjust = 0) +annotate("text", x = 2023, y = 0.252,     label = "Necessities", angle = -25,  color = "red", size = 6, hjust = 0) +annotate("text", x = 2023, y = 0.189,     label = "Discretionary", angle = -12,  color = "blue", size = 6, hjust = 0)

### Table describing the data
#Begin: List of merchant types ####
### Abbreviating Table 1's 21 merchant description (from Brian's slides)
# to be used in tables (see below further merchX_abv_fig for figures)
(merch1_abv = "1. Grocery store (N)")
(merch2_abv = "2. Gas station (N)")
(merch3_abv = "3. Restaurant/bar (D)")
(merch4_abv = "4. Fast food/coffee shop (D)")
(merch5_abv = "5. General merchandise store (D)")
(merch6_abv = "6. General service (D)")
(merch7_abv = "7. Art/entertainment (D)")
(merch8_abv = "8. Non-government utility (N)")
(merch9_abv = "9. Taxi/airplane/delivery")
(merch10_abv = "10. Phone/internet/cable (N)")
(merch11_abv = "11. Contractor/plumb/elec")
(merch12_abv = "12. Professional service")
(merch13_abv = "13. Hotel/motel/campsite")
(merch14_abv = "14. Rent (N)")
(merch15_abv = "15. Mortgage/insurance/loans (N)")
(merch16_abv = "16. Person-to-person (D)")
(merch17_abv = "17. Charitable/religious (D)")
(merch18_abv = "18. Hospital/doctor/dentist (N)")
(merch19_abv = "19. Government taxes (N)")
(merch20_abv = "20. School/college/childcare (N)")
(merch21_abv = "21. Public transport/tolls (N)")
# Make names a vector
(merch_abv.vec = c(merch1_abv, merch2_abv, merch3_abv, merch4_abv, merch5_abv, merch6_abv, merch7_abv, merch8_abv, merch9_abv, merch10_abv, merch11_abv, merch12_abv, merch13_abv, merch14_abv, merch15_abv, merch16_abv, merch17_abv, merch18_abv, merch19_abv, merch20_abv, merch21_abv))
# Finalizing merchant name table
merch_num.vec = 1:21# not needed
(merch_abv.df = data.frame(merch_abv.vec))
dim(merch_name.df)

# Adding num obs and avg spending pairs of years by merchant type
(merch_abv2.df = merch_abv.df %>% mutate(obs21 = merch_obs_2021.vec, avg21 = round(merch_avg_spend_2021.vec, digits = 0), obs22 = merch_obs_2022.vec, avg22 = round(merch_avg_spend_2022.vec, digits = 0), obs23 = merch_obs_2023.vec, avg23 = round(merch_avg_spend_2023.vec, digits = 0), obs24 = merch_obs_2024.vec, avg24 = round(merch_avg_spend_2024.vec, digits = 0), obs25 = merch_obs_2025.vec, avg25 = round(merch_avg_spend_2025.vec, digits = 0)))
#

print(xtable(merch_abv2.df, digits = 0), include.rownames = F, hline.after = c(0))
