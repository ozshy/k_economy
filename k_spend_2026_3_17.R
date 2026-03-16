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
library(ineq)# GINI coefficient
library(Hmisc)
#library(gtools)# for stars.pval function

#2025 begins####
setwd("~/SDCPC/2025_SDCPC")# work machine
dir()
# Read transaction dataset
trans2025_1.df = readRDS("dcpc-2025-tranlevel.rds")
dim(trans2025_1.df)
names(trans2025_1.df)
(sampled_num_trans_2025 = nrow(trans2025_1.df))

# Read individual dataset
indiv2025_1.df = readRDS("dcpc-2025-indlevel.rds")
dim(indiv2025_1.df)
names(indiv2025_1.df)
(sampled_num_resp_2025 = nrow(indiv2025_1.df))

# select needed variables from the indiv dataset
indiv2025_2.df = subset(indiv2025_1.df, select = c(uasid, ind_weight_all, income_hh))

# removing missing income
sum(is.na(indiv2025_2.df$income_hh))# missing income
dim(indiv2025_2.df)
indiv2025_3.df = subset(indiv2025_2.df, !is.na(income_hh))# removing respondents with missing income
dim(indiv2025_3.df)# num of respondent

# adjusting the weights (addinig a column "w")
nrow(indiv2025_3.df)
sum(indiv2025_3.df$ind_weight_all)
indiv2025_3.df$w = nrow(indiv2025_3.df)*indiv2025_3.df$ind_weight_all/sum(indiv2025_3.df$ind_weight_all)
sum(indiv2025_3.df$w)

# adding a variable with income quantile for each respondent
(income_qt = wtd.quantile(indiv2025_3.df$income_hh, weights = indiv2025_3.df$w, probs = c(seq(0,1,0.1))))# creating 10 quantiles
#
# adding a column with the resp income quantile
indiv2025_3.df = indiv2025_3.df %>%
  mutate(income_quantiles = cut(indiv2025_3.df$income_hh, income_qt, include.lowest = T, labels = F))
names(indiv2025_3.df)
sample_n(indiv2025_3.df,10)
# num of resp in each weighted quantile bin
table(indiv2025_3.df$income_quantiles)

# merge the indiv with the trans datasets by id
m1.df = left_join(trans2025_1.df, indiv2025_3.df, by = "uasid")
dim(m1.df)# num of payments (transactions)

# select only the needed variables
m2.df = subset(m1.df, select = c(uasid, amnt, income_hh, income_quantiles, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$uasid))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))
# delete obs with NAs (missing HH income & merch)
dim(m2.df)# num payments before
m3.df = m2.df[complete.cases(m2.df), ]
dim(m3.df)# num payments after

m4.df = m3.df
m5.df = m4.df
m6.df = m5.df

# for general knowledge: Avg spending per respondent over the entire sample
names(m6.df)
head(m6.df)
nrow(m6.df[unique(m6.df$uasid), ])# num of resp
(avg_spend_entire_2025 = sum(m6.df$amnt)/nrow(m6.df[unique(m6.df$uasid), ]))

# number of respondents by income quantile. Note, this is different from the num resp by quantile when constructed due to the elimination of NAs from the merged indiv-trans datasets
names(m6.df)
m6.df %>% group_by(income_quantiles) %>% summarise(n_distinct(uasid))

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
(m7.df = m6.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(uasid)))
dim(m7.df)

# adding variable: avg spending by quantile
(m7.df$avg_spend = m7.df$spend/m7.df$num_resp)
m7.df

# Calculate Gini coefficient
(gini_2025 = ineq(m7.df$avg_spend, type = "Gini"))

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

### 2025 Focusing on selected merchant types
dim(m3.df)# number of payments
names(m3.df)
nrow(m3.df[unique(m3.df$uasid), ])# number of unique respondents 

# start merchant types "necessaries" 
(necessities.vec = as.factor(c(1,2,8,10,14,15,18,19,20, 21) ))
# focus on necessaries only
m4.df = subset(m3.df, merch %in% necessities.vec)
dim(m4.df)# number of transaction (necessities only)
nrow(m4.df[unique(m4.df$uasid), ])# num resp (necessities)
sample_n(m4.df, 5)

names(m4.df)
# num of resp in each quantile. Uneven, but not an issue because only avg spending per resp is needed for inequality
(quantile_count.df = m4.df %>% group_by(income_quantiles) %>% summarise(unique_ID_count = n_distinct(uasid)))

# adding column with aggregate spending (amnt) income quantile and the num resp by quanntile
(m5.df = m4.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(uasid)))

# Add column of average spend (spending per person)
m6.df = m5.df
m7.df = m6.df
m7.df$avg_spend = m6.df$spend/m6.df$num_resp
m7.df

# Calculate Gini coefficient for necessities
(gini_2025_necessities = ineq(m7.df$avg_spend, type = "Gini"))

# start merchant types "discretionary" 
(discretionary.vec = as.factor(c(3,4,5,6,7,16,17) ))
# focus on discretionary only
m4.df = subset(m3.df, merch %in% discretionary.vec)
dim(m4.df)# number of transaction (discretionary only)
nrow(m4.df[unique(m4.df$uasid), ])# num resp (discretionary)
sample_n(m4.df, 5)

names(m4.df)
# num of resp in each quantile. Uneven, but not an issue because only avg spending per resp is needed for inequality
(quantile_count.df = m4.df %>% group_by(income_quantiles) %>% summarise(unique_ID_count = n_distinct(uasid)))

# adding column with aggregate spending (amnt) income quantile and the num resp by quanntile
(m5.df = m4.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(uasid)))

# Add column of average spend (spending per person)
m6.df = m5.df
m7.df = m6.df
m7.df$avg_spend = m6.df$spend/m6.df$num_resp
m7.df

# Calculate Gini coefficient for discretionary
(gini_2025_discretionary = ineq(m7.df$avg_spend, type = "Gini"))

#2025 ends####

#+++++++++++++++++++

#2024 begins####
setwd("~/SDCPC/2024_SDCPC")# work machine
dir()
# Read transaction dataset
trans2024_1.df = readRDS("dcpc-2024-tranlevel-public.rds")
dim(trans2024_1.df)
names(trans2024_1.df)
(sampled_num_trans_2024 = nrow(trans2024_1.df))

# Read individual dataset
indiv2024_1.df = readRDS("dcpc-2024-indlevel-public.rds")
dim(indiv2024_1.df)
names(indiv2024_1.df)
(sampled_num_resp_2024 = nrow(indiv2024_1.df))

# select needed variables from the indiv dataset
indiv2024_2.df = subset(indiv2024_1.df, select = c(id, ind_weight_all,  income_hh))

# removing missing income
sum(is.na(indiv2024_2.df$income_hh))# missing income
dim(indiv2024_2.df)
indiv2024_3.df = subset(indiv2024_2.df, !is.na(income_hh))# removing respondents with missing income
dim(indiv2024_3.df)# num of respondent

# adjusting the weights (addinig a column "w")
nrow(indiv2024_3.df)
sum(indiv2024_3.df$ind_weight_all)
indiv2024_3.df$w = nrow(indiv2024_3.df)*indiv2024_3.df$ind_weight_all/sum(indiv2024_3.df$ind_weight_all)
sum(indiv2024_3.df$w)

# adding a variable with income quantile for each respondent
(income_qt = wtd.quantile(indiv2024_3.df$income_hh, weights = indiv2024_3.df$w, probs = c(seq(0,1,0.1))))# creating 10 quantiles
#
# adding a column with the resp income quantile
indiv2024_3.df = indiv2024_3.df %>%
  mutate(income_quantiles = cut(indiv2024_3.df$income_hh, income_qt, include.lowest = T, labels = F))
names(indiv2024_3.df)
sample_n(indiv2024_3.df,10)
# num of resp in each weighted quantile bin
table(indiv2024_3.df$income_quantiles)

# merge the indiv with the trans datasets by id
m1.df = left_join(trans2024_1.df, indiv2024_3.df, by = "id")
dim(m1.df)# num of payments (transactions)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, amnt, income_hh, income_quantiles, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))
# delete obs with NAs (missing HH income & merch)
dim(m2.df)# num payments before
m3.df = m2.df[complete.cases(m2.df), ]
dim(m3.df)# num payments after

m4.df = m3.df
m5.df = m4.df
m6.df = m5.df

# for general knowledge: Avg spending per respondent over the entire sample
names(m6.df)
head(m6.df)
nrow(m6.df[unique(m6.df$id), ])# num of resp
(avg_spend_entire_2024 = sum(m6.df$amnt)/nrow(m6.df[unique(m6.df$id), ]))

# number of respondents by income quantile
names(m6.df)
m6.df %>% group_by(income_quantiles) %>% summarise(n_distinct(id))

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
(m7.df = m6.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7.df)

# adding variable: avg spending by quantile
(m7.df$avg_spend = m7.df$spend/m7.df$num_resp)
m7.df

# Calculate Gini coefficient
(gini_2024 = ineq(m7.df$avg_spend, type = "Gini"))

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

### 2024 Focusing on selected merchant types
dim(m3.df)# number of payments
names(m3.df)
nrow(m3.df[unique(m3.df$id), ])# number of unique respondents 

# start merchant types "necessaries" 
(necessities.vec = as.factor(c(1,2,8,10,14,15,18,19,20, 21) ))
# focus on necessaries only
m4.df = subset(m3.df, merch %in% necessities.vec)
dim(m4.df)# number of transaction (necessities only)
nrow(m4.df[unique(m4.df$id), ])# num resp (necessities)
sample_n(m4.df, 5)

names(m4.df)
# num of resp in each quantile. Uneven, but not an issue because only avg spending per resp is needed for inequality
(quantile_count.df = m4.df %>% group_by(income_quantiles) %>% summarise(unique_ID_count = n_distinct(id)))

# adding column with aggregate spending (amnt) income quantile and the num resp by quanntile
(m5.df = m4.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))

# Add column of average spend (spending per person)
m6.df = m5.df
m7.df = m6.df
m7.df$avg_spend = m6.df$spend/m6.df$num_resp
m7.df

# Calculate Gini coefficient for necessities
(gini_2024_necessities = ineq(m7.df$avg_spend, type = "Gini"))

# start merchant types "discretionary" 
(discretionary.vec = as.factor(c(3,4,5,6,7,16,17) ))
# focus on discretionary only
m4.df = subset(m3.df, merch %in% discretionary.vec)
dim(m4.df)# number of transaction (discretionary only)
nrow(m4.df[unique(m4.df$id), ])# num resp (discretionary)
sample_n(m4.df, 5)

names(m4.df)
# num of resp in each quantile. Uneven, but not an issue because only avg spending per resp is needed for inequality
(quantile_count.df = m4.df %>% group_by(income_quantiles) %>% summarise(unique_ID_count = n_distinct(id)))

# adding column with aggregate spending (amnt) income quantile and the num resp by quanntile
(m5.df = m4.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))

# Add column of average spend (spending per person)
m6.df = m5.df
m7.df = m6.df
m7.df$avg_spend = m6.df$spend/m6.df$num_resp
m7.df

# Calculate Gini coefficient for discretionary
(gini_2024_discretionary = ineq(m7.df$avg_spend, type = "Gini"))

#2024 ends####

#+++++++++++++++++++

#2023 begins####
setwd("~/SDCPC/2023_SDCPC")
dir()
# Read transaction dataset
trans2023_1.df = readRDS("dcpc-2023-tranlevel-public.rds")
dim(trans2023_1.df)
names(trans2023_1.df)
(sampled_num_trans_2023 = nrow(trans2023_1.df))

# Read individual dataset
indiv2023_1.df = readRDS("dcpc-2023-indlevel-public.rds")
dim(indiv2023_1.df)
names(indiv2023_1.df)
(sampled_num_resp_2023 = nrow(indiv2023_1.df))

# select needed variables from the indiv dataset
indiv2023_2.df = subset(indiv2023_1.df, select = c(id, ind_weight_all,  income_hh))

# removing missing income
sum(is.na(indiv2023_2.df$income_hh))# missing income
dim(indiv2023_2.df)
indiv2023_3.df = subset(indiv2023_2.df, !is.na(income_hh))# removing respondents with missing income
dim(indiv2023_3.df)# num of respondent

# adjusting the weights (addinig a column "w")
nrow(indiv2023_3.df)
sum(indiv2023_3.df$ind_weight_all)
indiv2023_3.df$w = nrow(indiv2023_3.df)*indiv2023_3.df$ind_weight_all/sum(indiv2023_3.df$ind_weight_all)
sum(indiv2023_3.df$w)

# adding a variable with income quantile for each respondent
(income_qt = wtd.quantile(indiv2023_3.df$income_hh, weights = indiv2023_3.df$w, probs = c(seq(0,1,0.1))))# creating 10 quantiles
#
# adding a column with the resp income quantile
indiv2023_3.df = indiv2023_3.df %>%
  mutate(income_quantiles = cut(indiv2023_3.df$income_hh, income_qt, include.lowest = T, labels = F))
names(indiv2023_3.df)
sample_n(indiv2023_3.df,10)
# num of resp in each weighted quantile bin
table(indiv2023_3.df$income_quantiles)


# merge the indiv with the trans datasets by id
m1.df = left_join(trans2023_1.df, indiv2023_3.df, by = "id")
dim(m1.df)# num of payments (transactions)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, amnt, income_hh, income_quantiles, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))
# delete obs with NAs (missing HH income & merch)
dim(m2.df)# num payments before
m3.df = m2.df[complete.cases(m2.df), ]
dim(m3.df)# num payments after

m4.df = m3.df
m5.df = m4.df
m6.df = m5.df

# for general knowledge: Avg spending per respondent over the entire sample
names(m6.df)
head(m6.df)
nrow(m6.df[unique(m6.df$id), ])# num of resp
(avg_spend_entire_2023 = sum(m6.df$amnt)/nrow(m6.df[unique(m6.df$id), ]))

# number of respondents by income quantile
names(m6.df)
m6.df %>% group_by(income_quantiles) %>% summarise(n_distinct(id))

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
(m7.df = m6.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7.df)

# adding variable: avg spending by quantile
(m7.df$avg_spend = m7.df$spend/m7.df$num_resp)
m7.df

# Calculate Gini coefficient
(gini_2023 = ineq(m7.df$avg_spend, type = "Gini"))

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

### 2023 Focusing on selected merchant types
dim(m3.df)# number of payments
names(m3.df)
nrow(m3.df[unique(m3.df$id), ])# number of unique respondents 

# start merchant types "necessaries" 
(necessities.vec = as.factor(c(1,2,8,10,14,15,18,19,20, 21) ))
# focus on necessaries only
m4.df = subset(m3.df, merch %in% necessities.vec)
dim(m4.df)# number of transaction (necessities only)
nrow(m4.df[unique(m4.df$id), ])# num resp (necessities)
sample_n(m4.df, 5)

names(m4.df)
# num of resp in each quantile. Uneven, but not an issue because only avg spending per resp is needed for inequality
(quantile_count.df = m4.df %>% group_by(income_quantiles) %>% summarise(unique_ID_count = n_distinct(id)))

# adding column with aggregate spending (amnt) income quantile and the num resp by quanntile
(m5.df = m4.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))

# Add column of average spend (spending per person)
m6.df = m5.df
m7.df = m6.df
m7.df$avg_spend = m6.df$spend/m6.df$num_resp
m7.df

# Calculate Gini coefficient for necessities
(gini_2023_necessities = ineq(m7.df$avg_spend, type = "Gini"))

# start merchant types "discretionary" 
(discretionary.vec = as.factor(c(3,4,5,6,7,16,17) ))
# focus on discretionary only
m4.df = subset(m3.df, merch %in% discretionary.vec)
dim(m4.df)# number of transaction (discretionary only)
nrow(m4.df[unique(m4.df$id), ])# num resp (discretionary)
sample_n(m4.df, 5)

names(m4.df)
# num of resp in each quantile. Uneven, but not an issue because only avg spending per resp is needed for inequality
(quantile_count.df = m4.df %>% group_by(income_quantiles) %>% summarise(unique_ID_count = n_distinct(id)))

# adding column with aggregate spending (amnt) income quantile and the num resp by quanntile
(m5.df = m4.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))

# Add column of average spend (spending per person)
m6.df = m5.df
m7.df = m6.df
m7.df$avg_spend = m6.df$spend/m6.df$num_resp
m7.df

# Calculate Gini coefficient for discretionary
(gini_2023_discretionary = ineq(m7.df$avg_spend, type = "Gini"))

#2023 ends####

#+++++++++++++++++++

#2022 begins####
setwd("~/SDCPC/2022_DCPC")
dir()
# Read transaction dataset
trans2022_1.df = readRDS("dcpc-2022-tranlevel-public.rds")
dim(trans2022_1.df)
names(trans2022_1.df)
(sampled_num_trans_2022 = nrow(trans2022_1.df))

# Read individual dataset
indiv2022_1.df = readRDS("dcpc-2022-indlevel-public.rds")
dim(indiv2022_1.df)
names(indiv2022_1.df)
(sampled_num_resp_2022 = nrow(indiv2022_1.df))

# select needed variables from the indiv dataset
indiv2022_2.df = subset(indiv2022_1.df, select = c(id, ind_weight_all,  income_hh))

# removing missing income
sum(is.na(indiv2022_2.df$income_hh))# missing income
dim(indiv2022_2.df)
indiv2022_3.df = subset(indiv2022_2.df, !is.na(income_hh))# removing respondents with missing income
dim(indiv2022_3.df)# num of respondent

# adjusting the weights (addinig a column "w")
nrow(indiv2022_3.df)
sum(indiv2022_3.df$ind_weight_all)
indiv2022_3.df$w = nrow(indiv2022_3.df)*indiv2022_3.df$ind_weight_all/sum(indiv2022_3.df$ind_weight_all)
sum(indiv2022_3.df$w)

# adding a variable with income quantile for each respondent
(income_qt = wtd.quantile(indiv2022_3.df$income_hh, weights = indiv2022_3.df$w, probs = c(seq(0,1,0.1))))# creating 10 quantiles
#
# adding a column with the resp income quantile
indiv2022_3.df = indiv2022_3.df %>%
  mutate(income_quantiles = cut(indiv2022_3.df$income_hh, income_qt, include.lowest = T, labels = F))
names(indiv2022_3.df)
sample_n(indiv2022_3.df,10)
# num of resp in each weighted quantile bin
table(indiv2022_3.df$income_quantiles)

# merge the indiv with the trans datasets by id
m1.df = left_join(trans2022_1.df, indiv2022_3.df, by = "id")
dim(m1.df)# num of payments (transactions)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, amnt, income_hh, income_quantiles, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))
# delete obs with NAs (missing HH income & merch)
dim(m2.df)# num payments before
m3.df = m2.df[complete.cases(m2.df), ]
dim(m3.df)# num payments after

m4.df = m3.df
m5.df = m4.df
m6.df = m5.df

# for general knowledge: Avg spending per respondent over the entire sample
names(m6.df)
head(m6.df)
nrow(m6.df[unique(m6.df$id), ])# num of resp
(avg_spend_entire_2022 = sum(m6.df$amnt)/nrow(m6.df[unique(m6.df$id), ]))

# number of respondents by income quantile
names(m6.df)
m6.df %>% group_by(income_quantiles) %>% summarise(n_distinct(id))

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
(m7.df = m6.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7.df)

# adding variable: avg spending by quantile
(m7.df$avg_spend = m7.df$spend/m7.df$num_resp)
m7.df

# Calculate Gini coefficient
(gini_2022 = ineq(m7.df$avg_spend, type = "Gini"))

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

### 2022 Focusing on selected merchant types
dim(m3.df)# number of payments
names(m3.df)
nrow(m3.df[unique(m3.df$id), ])# number of unique respondents 

# start merchant types "necessaries" 
(necessities.vec = as.factor(c(1,2,8,10,14,15,18,19,20, 21) ))
# focus on necessaries only
m4.df = subset(m3.df, merch %in% necessities.vec)
dim(m4.df)# number of transaction (necessities only)
nrow(m4.df[unique(m4.df$id), ])# num resp (necessities)
sample_n(m4.df, 5)

names(m4.df)
# num of resp in each quantile. Uneven, but not an issue because only avg spending per resp is needed for inequality
(quantile_count.df = m4.df %>% group_by(income_quantiles) %>% summarise(unique_ID_count = n_distinct(id)))

# adding column with aggregate spending (amnt) income quantile and the num resp by quanntile
(m5.df = m4.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))

# Add column of average spend (spending per person)
m6.df = m5.df
m7.df = m6.df
m7.df$avg_spend = m6.df$spend/m6.df$num_resp
m7.df

# Calculate Gini coefficient for necessities
(gini_2022_necessities = ineq(m7.df$avg_spend, type = "Gini"))

# start merchant types "discretionary" 
(discretionary.vec = as.factor(c(3,4,5,6,7,16,17) ))
# focus on discretionary only
m4.df = subset(m3.df, merch %in% discretionary.vec)
dim(m4.df)# number of transaction (discretionary only)
nrow(m4.df[unique(m4.df$id), ])# num resp (discretionary)
sample_n(m4.df, 5)

names(m4.df)
# num of resp in each quantile. Uneven, but not an issue because only avg spending per resp is needed for inequality
(quantile_count.df = m4.df %>% group_by(income_quantiles) %>% summarise(unique_ID_count = n_distinct(id)))

# adding column with aggregate spending (amnt) income quantile and the num resp by quanntile
(m5.df = m4.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))

# Add column of average spend (spending per person)
m6.df = m5.df
m7.df = m6.df
m7.df$avg_spend = m6.df$spend/m6.df$num_resp
m7.df

# Calculate Gini coefficient for discretionary
(gini_2022_discretionary = ineq(m7.df$avg_spend, type = "Gini"))

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
(sampled_num_trans_2021 = nrow(trans2021_1.df))

# Read individual dataset
indiv2021_1.df = readRDS("dcpc-2021-indlevel-public.rds")
dim(indiv2021_1.df)
names(indiv2021_1.df)
(sampled_num_resp_2021 = nrow(indiv2021_1.df))

# select needed variables from the indiv dataset
indiv2021_2.df = subset(indiv2021_1.df, select = c(id, ind_weight_all,  income_hh))

# removing missing income
sum(is.na(indiv2021_2.df$income_hh))# missing income
dim(indiv2021_2.df)
indiv2021_3.df = subset(indiv2021_2.df, !is.na(income_hh))# removing respondents with missing income
dim(indiv2021_3.df)# num of respondent

# adjusting the weights (addinig a column "w")
nrow(indiv2021_3.df)
sum(indiv2021_3.df$ind_weight_all)
indiv2021_3.df$w = nrow(indiv2021_3.df)*indiv2021_3.df$ind_weight_all/sum(indiv2021_3.df$ind_weight_all)
sum(indiv2021_3.df$w)

# adding a variable with income quantile for each respondent
(income_qt = wtd.quantile(indiv2021_3.df$income_hh, weights = indiv2021_3.df$w, probs = c(seq(0,1,0.1))))# creating 10 quantiles
#
# adding a column with the resp income quantile
indiv2021_3.df = indiv2021_3.df %>%
  mutate(income_quantiles = cut(indiv2021_3.df$income_hh, income_qt, include.lowest = T, labels = F))
names(indiv2021_3.df)
sample_n(indiv2021_3.df,10)
# num of resp in each weighted quantile bin
table(indiv2021_3.df$income_quantiles)


# merge the indiv with the trans datasets by id
m1.df = left_join(trans2021_1.df, indiv2021_3.df, by = "id")
dim(m1.df)# num of payments (transactions)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, amnt, income_hh, income_quantiles, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))
# delete obs with NAs (missing HH income & merch)
dim(m2.df)# num payments before
m3.df = m2.df[complete.cases(m2.df), ]
dim(m3.df)# num payments after

m4.df = m3.df
m5.df = m4.df
m6.df = m5.df

# for general knowledge: Avg spending per respondent over the entire sample
names(m6.df)
head(m6.df)
nrow(m6.df[unique(m6.df$id), ])# num of resp
(avg_spend_entire_2021 = sum(m6.df$amnt)/nrow(m6.df[unique(m6.df$id), ]))

# number of respondents by income quantile
names(m6.df)
m6.df %>% group_by(income_quantiles) %>% summarise(n_distinct(id))

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
(m7.df = m6.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7.df)

# adding variable: avg spending by quantile
(m7.df$avg_spend = m7.df$spend/m7.df$num_resp)
m7.df

# Calculate Gini coefficient
(gini_2021 = ineq(m7.df$avg_spend, type = "Gini"))

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

### 2021 Focusing on selected merchant types
dim(m3.df)# number of payments
names(m3.df)
nrow(m3.df[unique(m3.df$id), ])# number of unique respondents 

# start merchant types "necessaries" 
(necessities.vec = as.factor(c(1,2,8,10,14,15,18,19,20, 21) ))
# focus on necessaries only
m4.df = subset(m3.df, merch %in% necessities.vec)
dim(m4.df)# number of transaction (necessities only)
nrow(m4.df[unique(m4.df$id), ])# num resp (necessities)
sample_n(m4.df, 5)

names(m4.df)
# num of resp in each quantile. Uneven, but not an issue because only avg spending per resp is needed for inequality
(quantile_count.df = m4.df %>% group_by(income_quantiles) %>% summarise(unique_ID_count = n_distinct(id)))

# adding column with aggregate spending (amnt) income quantile and the num resp by quanntile
(m5.df = m4.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))

# Add column of average spend (spending per person)
m6.df = m5.df
m7.df = m6.df
m7.df$avg_spend = m6.df$spend/m6.df$num_resp
m7.df

# Calculate Gini coefficient for necessities
(gini_2021_necessities = ineq(m7.df$avg_spend, type = "Gini"))

# start merchant types "discretionary" 
(discretionary.vec = as.factor(c(3,4,5,6,7,16,17) ))
# focus on discretionary only
m4.df = subset(m3.df, merch %in% discretionary.vec)
dim(m4.df)# number of transaction (discretionary only)
nrow(m4.df[unique(m4.df$id), ])# num resp (discretionary)
sample_n(m4.df, 5)

names(m4.df)
# num of resp in each quantile. Uneven, but not an issue because only avg spending per resp is needed for inequality
(quantile_count.df = m4.df %>% group_by(income_quantiles) %>% summarise(unique_ID_count = n_distinct(id)))

# adding column with aggregate spending (amnt) income quantile and the num resp by quanntile
(m5.df = m4.df %>% group_by(income_quantiles) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))

# Add column of average spend (spending per person)
m6.df = m5.df
m7.df = m6.df
m7.df$avg_spend = m6.df$spend/m6.df$num_resp
m7.df

# Calculate Gini coefficient for discretionary
(gini_2021_discretionary = ineq(m7.df$avg_spend, type = "Gini"))

#2021 ends####

#+++++++++++++++++

# Comparisons of inequality Gini ####
(gini_all.vec = c(gini_2021, gini_2022, gini_2023, gini_2024, gini_2025))
#
(gini_necessities.vec = c(gini_2021_necessities, gini_2022_necessities, gini_2023_necessities, gini_2024_necessities, gini_2025_necessities))
#
(gini_discretionary.vec = c(gini_2021_discretionary, gini_2022_discretionary, gini_2023_discretionary, gini_2024_discretionary, gini_2025_discretionary))
#
# Put all Gini in a data frame
(ineq.df = data.frame(year.vec = 2021:2025, gini_all.vec, gini_necessities.vec, gini_discretionary.vec))

# Plotting Gini 
# searching for lower and upper bounds
min(gini_all.vec, gini_necessities.vec, gini_discretionary.vec)
max(gini_all.vec, gini_necessities.vec, gini_discretionary.vec)


ggplot(ineq.df, aes(x=year.vec, y=gini_all.vec)) + geom_line(linewidth = 1.0) +geom_point(size=1.9) + scale_x_continuous(breaks = 2021:2025) +labs(x="Year", y="Gini spending inequality index") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20))  + scale_y_continuous(breaks = seq(0.17,0.27, 0.01)) + geom_line(aes(y=gini_necessities.vec), linewidth = 1.0, color="red", linetype="longdash") +geom_point(aes(y=gini_necessities.vec), size=1.9, color="red") +geom_line(aes(y=gini_discretionary.vec), linewidth = 1.0, color="blue", linetype="dotdash") +geom_point(aes(y=gini_discretionary.vec), size=1.9, color="blue") +annotate("text", x = 2021, y = 0.223,           label = "All spending", angle = 52,  color = "black", size = 6, hjust = 0) +annotate("text", x = 2023, y = 0.253,     label = "Necessities", angle = -20,  color = "red", size = 6, hjust = 0) +annotate("text", x = 2023, y = 0.183,     label = "Discretionary", angle = -7,  color = "blue", size = 6, hjust = 0)

# showing (not in the paper) that nominal spending increased from year to year
avg_spend_entire_2021
avg_spend_entire_2022
avg_spend_entire_2023
avg_spend_entire_2024
avg_spend_entire_2025


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

#Summary of diary samples####
sampled_num_resp_2021
sampled_num_resp_2022
sampled_num_resp_2023
sampled_num_resp_2024
sampled_num_resp_2025
#
sampled_num_trans_2021
sampled_num_trans_2022
sampled_num_trans_2023
sampled_num_trans_2024
sampled_num_trans_2025
