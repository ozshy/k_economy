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
library(scales)# for commas , in data frame numbers
#library(ineq)# GINI coefficient
library(Hmisc)# for cutting data into bins
#library(gtools)# for stars.pval function

# merchant classifications:
necessities.vec = c(1,2,8,10,14,15,18,19,20,21) 
length(necessities.vec)
#
mixed_situational.vec = c(9,11,12,13) 
#
discretionary.vec = c(3,4,5,6,7,16,17) 
length(discretionary.vec)

#Begin: Table 1: HH income by year & quintile from Census####
(q2021.vec = c(0, 28261, 54440, 87037, 140307, Inf))
#
(q2022.vec = c(0, 30623, 58741, 93431, 150597, Inf))
#
q2023.vec = c(0, 31942, 61190, 97458, 155515, Inf)
#
q2024.vec = c(0, 33775, 64384, 101835, 163696, Inf)
#
# Estimating q2025 using the CAGR
(q1_cagr = (q2024.vec[1+1]/q2021.vec[1+1])^(1/4)-1)
#
(q2_cagr = (q2024.vec[1+2]/q2021.vec[1+2])^(1/4)-1)
#
(q3_cagr = (q2024.vec[1+3]/q2021.vec[1+3])^(1/4)-1)
#
(q4_cagr = (q2024.vec[1+4]/q2021.vec[1+4])^(1/4)-1)
#
# put all CAGR into a vecor
(qcagr.vec = c(0, q1_cagr, q2_cagr, q3_cagr, q4_cagr, 0))
(qcagr_percent.vec = 100*c(0, q1_cagr, q2_cagr, q3_cagr, q4_cagr, 0))
#
# generating q2025.vec from the above CAGR and 2024
(q2025.vec = round(q2024.vec*(1+qcagr.vec)))
#
# Make it a data frame and a LaTeX table
(quintile = c("0", "1st quintile", "2nd quintile", "3rd quintile", "4th quintile", "5th quintile"))
#
(q.df = data.frame("Upper limit of" = quintile, "Y2021" = q2021.vec, "Y2022" = q2022.vec, "Y2023" = q2023.vec, "Y2024" = q2024.vec, "Y2025" = q2025.vec, "CAGR" = round(qcagr_percent.vec, 1)))
str(q.df)

# make it a LaTeX table
(q2.df = q.df[-c(1), ])# delete 0 and Infinity rows
names(q2.df)
#
# add % sign to the CAGR
(q3.df = q2.df %>% mutate(CAGR = paste0(CAGR, "%")))
#
# adding commas using scales package
(q4.df = q3.df %>% mutate(Y2021 = comma(Y2021), Y2022 = comma(Y2022), Y2023 = comma(Y2023), Y2024 = comma(Y2024), Y2025 = comma(Y2025)))
#
# modify bottom 5th quintile row
q5.df = q4.df
q5.df[5, 2:6] = "Highest"
q5.df[5, 7] = NA
q5.df
# 
print(xtable(q5.df), include.rownames = F, hline.after = c(0))

# Export to CSV
#write.csv(q5.df, "table_1.csv", row.names = FALSE)

#End: Table 1: HH income by year & quintile from Census####

#++++++++++++++ 

#2025 begins####

#setwd("~/SDCPC/2025_SDCPC")
setwd("~/Research/SCPC_DCPC/2025_SDCPC")
dir()
# Read transaction dataset
trans2025_1.df = readRDS("dcpc-2025-tranlevel-public.rds")
dim(trans2025_1.df)
names(trans2025_1.df)
(sampled_num_trans_2025 = nrow(trans2025_1.df))

# Read individual dataset
indiv2025_1.df = readRDS("dcpc-2025-indlevel-public.rds")
dim(indiv2025_1.df)
names(indiv2025_1.df)
(sampled_num_resp_2025 = nrow(indiv2025_1.df))

# select needed variables from the indiv dataset
indiv2025_2.df = subset(indiv2025_1.df, select = c(id, ind_weight_all, income_hh))

# removing missing income
sum(is.na(indiv2025_2.df$income_hh))# missing income
dim(indiv2025_2.df)
indiv2025_3.df = subset(indiv2025_2.df, !is.na(income_hh))# removing respondents with missing income
dim(indiv2025_3.df)# num of respondent
sum(is.na(indiv2025_3.df$income_hh))# missing income
# are there income < or = 0?
nrow(subset(indiv2025_3.df, income_hh <  0))
nrow(subset(indiv2025_3.df, income_hh == 0))#zero income!

# labeling each resp with income bin (quintile)
q2025.vec# quintile thresholds
q2025_labels.vec = c("q1", "q2", "q3", "q4", "q5")
bin_cut_2025.vec = cut(indiv2025_3.df$income_hh, breaks = q2025.vec, labels = q2025_labels.vec, include.lowest = T)
head(bin_cut_2025.vec)
table(bin_cut_2025.vec, useNA = "always")

# adding a column with the resp income quantile bin
indiv2025_3.df = indiv2025_3.df %>%
  mutate(income_quantile = bin_cut_2025.vec)
names(indiv2025_3.df)
sample_n(indiv2025_3.df,10)
# num of resp in each weighted quantile bin
table(indiv2025_3.df$income_quantile, useNA = "always")

# merge the indiv with the trans datasets by id
# delete transactions not made in October
table(trans2025_1.df$date)
trans2025_2.df = subset(trans2025_1.df, date > "2025-09-30" & date < "2025-11-01")
table(trans2025_2.df$date)
#
m1.df = left_join(trans2025_2.df, indiv2025_3.df, by = "id")
dim(m1.df)# num of payments (transactions)
table(m1.df$date)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, amnt, income_hh, income_quantile, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))

# delete obs with NAs (missing HH income)
dim(m2.df)# num payments before
m3.df = subset(m2.df, !is.na(income_hh))
dim(m3.df)# num payments after

# are there amnt < or = 0?
summary(m3.df$amnt)
nrow(subset(m3.df, amnt <= 0))
# delete amnt < or = 0
m4.df = subset(m3.df, amnt > 0)
dim(m4.df)

# delete obs with NAs (missing merchant category)
nrow(subset(m4.df, is.na(merch)))# num trans with missing merchant
dim(m4.df)# num payments before
m5.df = subset(m4.df, !is.na(merch))
dim(m5.df)# num payments after
#
m6.df = m5.df

# for general knowledge: Avg spending per respondent over the entire sample (included in the merchants table as the bottom row)
names(m6.df)
head(m6.df)
nrow(m6.df[unique(m6.df$id), ])# num of resp
(avg_spend_entire_2025 = sum(m6.df$amnt)/nrow(m6.df[unique(m6.df$id), ]))

# number of respondents by income quantile. Note, this is different from the num resp by quantile when constructed due to the elimination of NAs from the merged indiv-trans datasets
names(m6.df)
m6.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7.df = m6.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7.df)

# adding variable: avg spending by quantile
(m7.df$avg_spend = m7.df$spend/m7.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2025.vec = m7.df$avg_spend)# 
m7.df

## For the table of merchant type calculations 2025 (go back to m6.df)
table(m6.df$merch, useNA = "always")# number of transactions by merch type
(merch_obs_2025 = m6.df %>% group_by(merch) %>% summarise(n()))
#
(merch_obs_2025.vec = as.numeric((merch_obs_2025$`n()`)))
length(merch_obs_2025.vec)# verify 21 merch categories
#
(total_payments_entire_2025 = nrow(m6.df))# bottom of merchants table
#verify by summing up trans by merch
sum(merch_obs_2025.vec)
#
(total_resp_2025 = nrow(m6.df[unique(m6.df$id), ]))# num of resp
#
# compute avg spending by merch type
(merch_total_spend_2025 = m6.df %>% group_by(merch) %>% summarise(sum(amnt)))
(merch_total_spend_2025.vec = as.numeric(merch_total_spend_2025$`sum(amnt)`))
#
(merch_avg_spend_2025.vec = merch_total_spend_2025.vec/merch_obs_2025.vec)

## Groceries only (merch == 1)
names(m6.df)
dim(m6.df)
m6g.df = subset(m6.df, merch == 1)# merch 1 only
dim(m6g.df)
# how many resp in each quintile?
m6g.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))
# how many trans in each quintile?
m6g.df %>% group_by(income_quantile) %>% summarise(n())

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7g.df = m6g.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7g.df)

# adding variable: avg spending by quantile
(m7g.df$avg_spend = m7g.df$spend/m7g.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2025g.vec = m7g.df$avg_spend)# 
m7g.df

## Necessaries only (merch = )
necessities.vec# recall list of merch
#
dim(m6.df)# all merch
m6n.df = subset(m6.df, merch %in% necessities.vec)# merch necessities only
dim(m6n.df)
# how many resp in each quintile?
m6n.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))
# how many trans in each quintile?
m6n.df %>% group_by(income_quantile) %>% summarise(n())

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7n.df = m6n.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7n.df)

# adding variable: avg spending by quantile
(m7n.df$avg_spend = m7n.df$spend/m7n.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2025n.vec = m7n.df$avg_spend)# 
m7n.df

#2025 ends####

#+++++++++++++++++++

#2024 begins####

#setwd("~/SDCPC/2024_SDCPC")# 
setwd("~/Research/SCPC_DCPC/2024_SDCPC")
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
indiv2024_2.df = subset(indiv2024_1.df, select = c(id, ind_weight_all, income_hh))

# removing missing income
sum(is.na(indiv2024_2.df$income_hh))# missing income
dim(indiv2024_2.df)
indiv2024_3.df = subset(indiv2024_2.df, !is.na(income_hh))# removing respondents with missing income
dim(indiv2024_3.df)# num of respondent
sum(is.na(indiv2024_3.df$income_hh))# missing income
# are there income < or = 0?
nrow(subset(indiv2024_3.df, income_hh <  0))
nrow(subset(indiv2024_3.df, income_hh == 0))#zero income!

# labeling each resp with income bin
q2024.vec
q2024_labels.vec = c("q1", "q2", "q3", "q4", "q5")
bin_cut_2024.vec = cut(indiv2024_3.df$income_hh, breaks = q2024.vec, labels = q2024_labels.vec, include.lowest = T)
head(bin_cut_2024.vec)
table(bin_cut_2024.vec, useNA = "always")

# adding a column with the resp income quantile bin
indiv2024_3.df = indiv2024_3.df %>%
  mutate(income_quantile = bin_cut_2024.vec)
names(indiv2024_3.df)
sample_n(indiv2024_3.df,10)
# num of resp in each weighted quantile bin
table(indiv2024_3.df$income_quantile, useNA = "always")

# merge the indiv with the trans datasets by id
# delete transactions not made in October
table(trans2024_1.df$date)
trans2024_2.df = subset(trans2024_1.df, date > "2024-09-30" & date < "2024-11-01")
table(trans2024_2.df$date)
#
m1.df = left_join(trans2024_2.df, indiv2024_3.df, by = "id")
dim(m1.df)# num of payments (transactions)
table(m1.df$date)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, amnt, income_hh, income_quantile, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))

# delete obs with NAs (missing HH income, not merch. )
dim(m2.df)# num payments before
m3.df = subset(m2.df, !is.na(income_hh))
dim(m3.df)# num payments after


# are there amnt < or = 0?
summary(m3.df$amnt)
nrow(subset(m3.df, amnt <= 0))
# delete amnt < or = 0
m4.df = subset(m3.df, amnt > 0)
dim(m4.df)

# delete obs with NAs (missing merchant category)
nrow(subset(m4.df, is.na(merch)))# num trans with missing merchant
dim(m4.df)# num payments before
m5.df = subset(m4.df, !is.na(merch))
dim(m5.df)# num payments after
#
m6.df = m5.df

# bottom of merchants table: Avg spending per respondent over the entire sample
names(m6.df)
head(m6.df)
nrow(m6.df[unique(m6.df$id), ])# num of resp
(avg_spend_entire_2024 = sum(m6.df$amnt)/nrow(m6.df[unique(m6.df$id), ]))

# number of respondents by income quantile. Note, this is different from the num resp by quantile when constructed due to the elimination of NAs from the merged indiv-trans datasets
names(m6.df)
m6.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7.df = m6.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7.df)

# adding variable: avg spending by quantile
(m7.df$avg_spend = m7.df$spend/m7.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2024.vec = m7.df$avg_spend)# 
m7.df

## For the table of merchant type calculations 2024 (go back to m6.df)
table(m6.df$merch, useNA = "always")# number of transactions by merch type
(merch_obs_2024 = m6.df %>% group_by(merch) %>% summarise(n()))
#
(merch_obs_2024.vec = as.numeric((merch_obs_2024$`n()`)))
length(merch_obs_2024.vec)# 21. 
(total_payments_entire_2024 = nrow(m6.df))# bottom of merchants table
#verify by summing up trans by merch
sum(merch_obs_2024.vec)
#
(total_resp_2024 = nrow(m6.df[unique(m6.df$id), ]))# num of resp

#
# compute avg spending by merch type
(merch_total_spend_2024 = m6.df %>% group_by(merch) %>% summarise(sum(amnt)))
(merch_total_spend_2024.vec = as.numeric(merch_total_spend_2024$`sum(amnt)`))
#
(merch_avg_spend_2024.vec = merch_total_spend_2024.vec/merch_obs_2024.vec)

## Groceries only (merch == 1)
names(m6.df)
dim(m6.df)
m6g.df = subset(m6.df, merch == 1)# merch 1 only
dim(m6g.df)
# how many resp in each quintile?
m6g.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))
# how many trans in each quintile?
m6g.df %>% group_by(income_quantile) %>% summarise(n())

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7g.df = m6g.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7g.df)

# adding variable: avg spending by quantile
(m7g.df$avg_spend = m7g.df$spend/m7g.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2024g.vec = m7g.df$avg_spend)# 
m7g.df

## Necessaries only (merch = )
necessities.vec# recall list of merch
#
dim(m6.df)# all merch
m6n.df = subset(m6.df, merch %in% necessities.vec)# merch neccessities only
dim(m6n.df)
# how many resp in each quintile?
m6n.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))
# how many trans in each quintile?
m6n.df %>% group_by(income_quantile) %>% summarise(n())

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7n.df = m6n.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7n.df)

# adding variable: avg spending by quantile
(m7n.df$avg_spend = m7n.df$spend/m7n.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2024n.vec = m7n.df$avg_spend)# 
m7n.df


#2024 ends####

#+++++++++++++++++++

#2023 begins####
#setwd("~/SDCPC/2023_SDCPC")
setwd("~/Research/SCPC_DCPC/2023_SDCPC")
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
indiv2023_2.df = subset(indiv2023_1.df, select = c(id, ind_weight_all, income_hh))

# removing missing income
sum(is.na(indiv2023_2.df$income_hh))# missing income
dim(indiv2023_2.df)
indiv2023_3.df = subset(indiv2023_2.df, !is.na(income_hh))# removing respondents with missing income
dim(indiv2023_3.df)# num of respondent
sum(is.na(indiv2023_3.df$income_hh))# missing income
# are there income < or = 0?
nrow(subset(indiv2023_3.df, income_hh <  0))
nrow(subset(indiv2023_3.df, income_hh == 0))#zero income!

# labeling each resp with income bin
q2023.vec
q2023_labels.vec = c("q1", "q2", "q3", "q4", "q5")
bin_cut_2023.vec = cut(indiv2023_3.df$income_hh, breaks = q2023.vec, labels = q2023_labels.vec, include.lowest = T)
head(bin_cut_2023.vec)
table(bin_cut_2023.vec, useNA = "always")

# adding a column with the resp income quantile bin
indiv2023_3.df = indiv2023_3.df %>%
  mutate(income_quantile = bin_cut_2023.vec)
names(indiv2023_3.df)
sample_n(indiv2023_3.df,10)
# num of resp in each weighted quantile bin
table(indiv2023_3.df$income_quantile, useNA = "always")

# merge the indiv with the trans datasets by id
# delete transactions not made in October
table(trans2023_1.df$date)
trans2023_2.df = subset(trans2023_1.df, date > "2023-09-30" & date < "2023-11-01")
table(trans2023_2.df$date)
#
m1.df = left_join(trans2023_2.df, indiv2023_3.df, by = "id")
dim(m1.df)# num of payments (transactions)
table(m1.df$date)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, amnt, income_hh, income_quantile, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))

# delete obs with NAs (missing HH income, not merch. )
dim(m2.df)# num payments before
m3.df = subset(m2.df, !is.na(income_hh))
dim(m3.df)# num payments after

# are there amnt < or = 0?
summary(m3.df$amnt)
nrow(subset(m3.df, amnt <= 0))
# delete amnt < or = 0
m4.df = subset(m3.df, amnt > 0)
dim(m4.df)

# delete obs with NAs (missing merchant category)
nrow(subset(m4.df, is.na(merch)))# num trans with missing merchant
dim(m4.df)# num payments before
m5.df = subset(m4.df, !is.na(merch))
dim(m5.df)# num payments after
#
m6.df = m5.df

# for general knowledge: Avg spending per respondent over the entire sample
names(m6.df)
head(m6.df)
nrow(m6.df[unique(m6.df$id), ])# num of resp
(avg_spend_entire_2023 = sum(m6.df$amnt)/nrow(m6.df[unique(m6.df$id), ]))

# number of respondents by income quantile. Note, this is different from the num resp by quantile when constructed due to the elimination of NAs from the merged indiv-trans datasets
names(m6.df)
m6.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7.df = m6.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7.df)

# adding variable: avg spending by quantile
(m7.df$avg_spend = m7.df$spend/m7.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2023.vec = m7.df$avg_spend)# 
m7.df

## For the table of merchant type calculations 2023 (go back to m6.df)
table(m6.df$merch, useNA = "always")# number of transactions by merch type
(merch_obs_2023.vec = m6.df %>% group_by(merch) %>% summarise(n()))
(total_payments_entire_2023 = nrow(m6.df))# bottom of merchants table
#verify by summing up trans by merch
sum(merch_obs_2023.vec)

#
(merch_obs_2023.vec = as.numeric((merch_obs_2023.vec$`n()`)))
length(merch_obs_2023.vec)# 21. 
#
(total_resp_2023 = nrow(m6.df[unique(m6.df$id), ]))# num of resp
#
# compute avg spending by merch type
(merch_total_spend_2023 = m6.df %>% group_by(merch) %>% summarise(sum(amnt)))
(merch_total_spend_2023.vec = as.numeric(merch_total_spend_2023$`sum(amnt)`))
#
(merch_avg_spend_2023.vec = merch_total_spend_2023.vec/merch_obs_2023.vec)

## Groceries only (merch == 1)
names(m6.df)
dim(m6.df)
m6g.df = subset(m6.df, merch == 1)# merch 1 only
dim(m6g.df)
# how many resp in each quintile?
m6g.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))
# how many trans in each quintile?
m6g.df %>% group_by(income_quantile) %>% summarise(n())

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7g.df = m6g.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7g.df)

# adding variable: avg spending by quantile
(m7g.df$avg_spend = m7g.df$spend/m7g.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2023g.vec = m7g.df$avg_spend)# 
m7g.df

## Necessaries only (merch = )
necessities.vec# recall list of merch
#
dim(m6.df)# all merch
m6n.df = subset(m6.df, merch %in% necessities.vec)# merch neccessities only
dim(m6n.df)
# how many resp in each quintile?
m6n.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))
# how many trans in each quintile?
m6n.df %>% group_by(income_quantile) %>% summarise(n())

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7n.df = m6n.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7n.df)

# adding variable: avg spending by quantile
(m7n.df$avg_spend = m7n.df$spend/m7n.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2023n.vec = m7n.df$avg_spend)# 
m7n.df


#2023 ends####

#+++++++++++++++++++

#2022 begins####

#setwd("~/SDCPC/2022_DCPC")
setwd("~/Research/SCPC_DCPC/2022_SDCPC")
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
indiv2022_2.df = subset(indiv2022_1.df, select = c(id, ind_weight_all, income_hh))

# removing missing income
sum(is.na(indiv2022_2.df$income_hh))# missing income
dim(indiv2022_2.df)
indiv2022_3.df = subset(indiv2022_2.df, !is.na(income_hh))# removing respondents with missing income
dim(indiv2022_3.df)# num of respondent
sum(is.na(indiv2022_3.df$income_hh))# missing income
# are there income < or = 0?
nrow(subset(indiv2022_3.df, income_hh <  0))
nrow(subset(indiv2022_3.df, income_hh == 0))#zero income!

# labeling each resp with income bin
q2022.vec
q2022_labels.vec = c("q1", "q2", "q3", "q4", "q5")
bin_cut_2022.vec = cut(indiv2022_3.df$income_hh, breaks = q2022.vec, labels = q2022_labels.vec, include.lowest = T)
head(bin_cut_2022.vec)
table(bin_cut_2022.vec, useNA = "always")

# adding a column with the resp income quantile bin
indiv2022_3.df = indiv2022_3.df %>%
  mutate(income_quantile = bin_cut_2022.vec)
names(indiv2022_3.df)
sample_n(indiv2022_3.df,10)
# num of resp in each weighted quantile bin
table(indiv2022_3.df$income_quantile, useNA = "always")

# merge the indiv with the trans datasets by id
# delete transactions not made in October
table(trans2022_1.df$date)
trans2022_2.df = subset(trans2022_1.df, date > "2022-09-30" & date < "2022-11-01")
table(trans2022_2.df$date)
#
m1.df = left_join(trans2022_2.df, indiv2022_3.df, by = "id")
dim(m1.df)# num of payments (transactions)
table(m1.df$date)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, amnt, income_hh, income_quantile, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))

# delete obs with NAs (missing HH income, not merch. )
dim(m2.df)# num payments before
m3.df = subset(m2.df, !is.na(income_hh))
dim(m3.df)# num payments after


# are there amnt < or = 0?
summary(m3.df$amnt)
nrow(subset(m3.df, amnt <= 0))
# delete amnt < or = 0
m4.df = subset(m3.df, amnt > 0)
dim(m4.df)

# delete obs with NAs (missing merchant category)
nrow(subset(m4.df, is.na(merch)))# num trans with missing merchant
dim(m4.df)# num payments before
m5.df = subset(m4.df, !is.na(merch))
dim(m5.df)# num payments after
#
m6.df = m5.df
# for general knowledge: Avg spending per respondent over the entire sample
names(m6.df)
head(m6.df)
nrow(m6.df[unique(m6.df$id), ])# num of resp
(avg_spend_entire_2022 = sum(m6.df$amnt)/nrow(m6.df[unique(m6.df$id), ]))

# number of respondents by income quantile. Note, this is different from the num resp by quantile when constructed due to the elimination of NAs from the merged indiv-trans datasets
names(m6.df)
m6.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7.df = m6.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7.df)

# adding variable: avg spending by quantile
(m7.df$avg_spend = m7.df$spend/m7.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2022.vec = m7.df$avg_spend)# 
m7.df

## For the table of merchant type calculations 2022 (go back to m6.df)
table(m6.df$merch, useNA = "always")# number of transactions by merch type
(merch_obs_2022 = m6.df %>% group_by(merch) %>% summarise(n()))
#
(merch_obs_2022.vec = as.numeric((merch_obs_2022$`n()`)))
length(merch_obs_2022.vec)# 21 categories
(total_payments_entire_2022 = nrow(m6.df))# bottom of merchants table
#verify by summing up trans by merch
sum(merch_obs_2022.vec)
#
(total_resp_2022 = nrow(m6.df[unique(m6.df$id), ]))# num of resp
#
# compute avg spending by merch type
(merch_total_spend_2022 = m6.df %>% group_by(merch) %>% summarise(sum(amnt)))
(merch_total_spend_2022.vec = as.numeric(merch_total_spend_2022$`sum(amnt)`))
#
(merch_avg_spend_2022.vec = merch_total_spend_2022.vec/merch_obs_2022.vec)

## Groceries only (merch == 1)
names(m6.df)
dim(m6.df)
m6g.df = subset(m6.df, merch == 1)# merch 1 only
dim(m6g.df)
# how many resp in each quintile?
m6g.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))
# how many trans in each quintile?
m6g.df %>% group_by(income_quantile) %>% summarise(n())

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7g.df = m6g.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7g.df)

# adding variable: avg spending by quantile
(m7g.df$avg_spend = m7g.df$spend/m7g.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2022g.vec = m7g.df$avg_spend)# 
m7g.df

## Necessaries only (merch = )
necessities.vec# recall list of merch
#
dim(m6.df)# all merch
m6n.df = subset(m6.df, merch %in% necessities.vec)# merch neccessities only
dim(m6n.df)
# how many resp in each quintile?
m6n.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))
# how many trans in each quintile?
m6n.df %>% group_by(income_quantile) %>% summarise(n())

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7n.df = m6n.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7n.df)

# adding variable: avg spending by quantile
(m7n.df$avg_spend = m7n.df$spend/m7n.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2022n.vec = m7n.df$avg_spend)# 
m7n.df

#2022 ends####

#+++++++++++++++++++
#
#2021 begins####

#setwd("~/SDCPC/2021_DCPC")
setwd("~/Research/SCPC_DCPC/2021_DCPC")
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
indiv2021_2.df = subset(indiv2021_1.df, select = c(id, ind_weight_all, income_hh))

# removing missing income
sum(is.na(indiv2021_2.df$income_hh))# missing income
dim(indiv2021_2.df)
indiv2021_3.df = subset(indiv2021_2.df, !is.na(income_hh))# removing respondents with missing income
dim(indiv2021_3.df)# num of respondent
sum(is.na(indiv2021_3.df$income_hh))# missing income
# are there income < or = 0?
nrow(subset(indiv2021_3.df, income_hh <  0))
nrow(subset(indiv2021_3.df, income_hh == 0))#zero income!

# labeling each resp with income bin
q2021.vec
q2021_labels.vec = c("q1", "q2", "q3", "q4", "q5")
bin_cut_2021.vec = cut(indiv2021_3.df$income_hh, breaks = q2021.vec, labels = q2021_labels.vec, include.lowest = T)
head(bin_cut_2021.vec)
table(bin_cut_2021.vec, useNA = "always")

# adding a column with the resp income quantile bin
indiv2021_3.df = indiv2021_3.df %>%
  mutate(income_quantile = bin_cut_2021.vec)
names(indiv2021_3.df)
sample_n(indiv2021_3.df,10)
# num of resp in each weighted quantile bin
table(indiv2021_3.df$income_quantile, useNA = "always")

# merge the indiv with the trans datasets by id
# delete transactions not made in October
table(trans2021_1.df$date)
trans2021_2.df = subset(trans2021_1.df, date > "2021-09-30" & date < "2021-11-01")
table(trans2021_2.df$date)
#
m1.df = left_join(trans2021_2.df, indiv2021_3.df, by = "id")
dim(m1.df)# num of payments (transactions)
table(m1.df$date)

# select only the needed variables
m2.df = subset(m1.df, select = c(id, amnt, income_hh, income_quantile, merch))
sum(is.na(m2.df))# number of trans with missing obs
sum(is.na(m2.df$amnt))
sum(is.na(m2.df$id))
sum(is.na(m2.df$income_hh))
sum(is.na(m2.df$merch))

# delete obs with NAs (missing HH income, not merch. )
dim(m2.df)# num payments before
m3.df = subset(m2.df, !is.na(income_hh))
dim(m3.df)# num payments after


# are there amnt < or = 0?
summary(m3.df$amnt)
nrow(subset(m3.df, amnt <= 0))
# delete amnt < or = 0
m4.df = subset(m3.df, amnt > 0)
dim(m4.df)

# delete obs with NAs (missing merchant category)
nrow(subset(m4.df, is.na(merch)))# num trans with missing merchant
dim(m4.df)# num payments before
m5.df = subset(m4.df, !is.na(merch))
dim(m5.df)# num payments after
#
# delete a respondent who reported purchasing groceries for $2000 six times
dim(m5.df)
subset(m5.df, merch==1 & amnt > 1000)# num trans with groceries > $1000
m6.df = subset(m5.df, !(merch==1 & amnt > 1000))
dim(m6.df)

# for general knowledge: Avg spending per respondent over the entire sample
names(m6.df)
head(m6.df)
nrow(m6.df[unique(m6.df$id), ])# num of resp
(avg_spend_entire_2021 = sum(m6.df$amnt)/nrow(m6.df[unique(m6.df$id), ]))

# number of respondents by income quantile. Note, this is different from the num resp by quantile when constructed due to the elimination of NAs from the merged indiv-trans datasets
names(m6.df)
m6.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7.df = m6.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7.df)

# adding variable: avg spending by quantile
(m7.df$avg_spend = m7.df$spend/m7.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2021.vec = m7.df$avg_spend)# 
m7.df

## For the table of merchant type calculations 2021 (go back to m6.df)
table(m6.df$merch, useNA = "always")# number of transactions by merch type
(merch_obs_2021 = m6.df %>% group_by(merch) %>% summarise(n()))
#
(merch_obs_2021.vec = as.numeric((merch_obs_2021$`n()`)))
length(merch_obs_2021.vec)# 21 categories
(total_payments_entire_2021 = nrow(m6.df))# bottom of merchants table
#verify by summing up trans by merch
sum(merch_obs_2021.vec)
#
(total_resp_2021 = nrow(m6.df[unique(m6.df$id), ]))# num of resp
#
# compute avg spending by merch type
(merch_total_spend_2021 = m6.df %>% group_by(merch) %>% summarise(sum(amnt)))
(merch_total_spend_2021.vec = as.numeric(merch_total_spend_2021$`sum(amnt)`))
#
(merch_avg_spend_2021.vec = merch_total_spend_2021.vec/merch_obs_2021.vec)

## Groceries only (merch == 1)
names(m6.df)
dim(m6.df)
m6g.df = subset(m6.df, merch == 1)# merch 1 only
dim(m6g.df)
# how many resp in each quintile?
m6g.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))
# how many trans in each quintile?
m6g.df %>% group_by(income_quantile) %>% summarise(n())

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7g.df = m6g.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7g.df)

# adding variable: avg spending by quantile
(m7g.df$avg_spend = m7g.df$spend/m7g.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2021g.vec = m7g.df$avg_spend)# 
m7g.df

## Necessaries only (merch = )
necessities.vec# recall list of merch
#
dim(m6.df)# all merch
m6n.df = subset(m6.df, merch %in% necessities.vec)# merch neccessities only
dim(m6n.df)
# how many resp in each quintile?
m6n.df %>% group_by(income_quantile) %>% summarise(n_distinct(id))
# how many trans in each quintile?
m6n.df %>% group_by(income_quantile) %>% summarise(n())

# adding 2 variables: Aggregate spending (new variable: spend) of each quantile and number of respondents in each quantile
# Below, m7 uses selected variables for the chart. Go back to m6.df for merchant specific analysis
(m7n.df = m6n.df %>% group_by(income_quantile) %>% summarise(spend = sum(amnt), num_resp = n_distinct(id)))
dim(m7n.df)

# adding variable: avg spending by quantile
(m7n.df$avg_spend = m7n.df$spend/m7n.df$num_resp)
#save it for a chart (avg spending during 3 days)
(avg_spend_2021n.vec = m7n.df$avg_spend)# 
m7n.df

#2021 ends####

#+++++++++++++++++

# showing (not in the paper) that nominal spending increased from year to year
avg_spend_entire_2021
avg_spend_entire_2022
avg_spend_entire_2023
avg_spend_entire_2024
avg_spend_entire_2025

#Begin: Figure 1: Avg spending by quantile 2021:2025####

#1st quantile
(quant_1.vec = c(avg_spend_2021.vec[1], avg_spend_2022.vec[1], avg_spend_2023.vec[1], avg_spend_2024.vec[1], avg_spend_2025.vec[1]))
#2nd quantile
(quant_2.vec = c(avg_spend_2021.vec[2], avg_spend_2022.vec[2], avg_spend_2023.vec[2], avg_spend_2024.vec[2], avg_spend_2025.vec[2]))
#3rd quantile
(quant_3.vec = c(avg_spend_2021.vec[3], avg_spend_2022.vec[3], avg_spend_2023.vec[3], avg_spend_2024.vec[3], avg_spend_2025.vec[3]))
#4th quantile
(quant_4.vec = c(avg_spend_2021.vec[4], avg_spend_2022.vec[4], avg_spend_2023.vec[4], avg_spend_2024.vec[4], avg_spend_2025.vec[4]))
#5th quantile
(quant_5.vec = c(avg_spend_2021.vec[5], avg_spend_2022.vec[5], avg_spend_2023.vec[5], avg_spend_2024.vec[5], avg_spend_2025.vec[5]))

# make it a data frame (3 day spending)
(quant.df = data.frame(year.vec = 2021:2025, quant1 = quant_1.vec, quant2 = quant_2.vec, quant3 = quant_3.vec, quant4 = quant_4.vec, quant5 = quant_5.vec))
#
# make it monthly spending (instead of 3 days)
(quant_month.df = data.frame(year.vec = 2021:2025, quant1 = 31*quant_1.vec/3, quant2 = 31*quant_2.vec/3, quant3 = 31*quant_3.vec/3, quant4 = 31*quant_4.vec/3, quant5 = 31*quant_5.vec/3))

#Compute CAGR (%) (reported inside Figure 1)
quant_1.vec
length(quant_1.vec)# 5 years
(cagr1 = 100*((tail(quant_1.vec, 1) / quant_1.vec[1])^(1 / (length(quant_1.vec) - 1)) - 1))
#
(cagr2 = 100*((tail(quant_2.vec, 1) / quant_2.vec[1])^(1 / (length(quant_2.vec) - 1)) - 1))
#
(cagr3 = 100*((tail(quant_3.vec, 1) / quant_3.vec[1])^(1 / (length(quant_3.vec) - 1)) - 1))
#
(cagr4 = 100*((tail(quant_4.vec, 1) / quant_4.vec[1])^(1 / (length(quant_4.vec) - 1)) - 1))
#
(cagr5 = 100*((tail(quant_5.vec, 1) / quant_5.vec[1])^(1 / (length(quant_5.vec) - 1)) - 1))
# put them into a data frame (not used)
(cagr.df = data.frame(Quantile =1:5, cagr = round(c(cagr1, cagr2, cagr3, cagr4, cagr5), 2)))

#plot 5 quintiles
ggplot(quant_month.df, aes(x=year.vec, y=quant1)) + geom_line(linewidth = 1.0) +geom_point(size=1.9) + geom_line(aes(y=quant2), linewidth = 1.0, color="red") +geom_point(aes(y=quant2), size=1.9, color="red") +geom_line(aes(y=quant3), linewidth = 1.0, color="blue") +geom_point(aes(y=quant3), size=1.9, color="blue") +geom_line(aes(y=quant4), linewidth = 1.0, color="magenta") +geom_point(aes(y=quant4), size=1.9, color="magenta") +geom_line(aes(y=quant5), linewidth = 1.0, color="darkgreen") +geom_point(aes(y=quant5), size=1.9, color="darkgreen")+ scale_x_continuous(breaks = 2021:2025) + scale_y_continuous(breaks = seq(3000, 17000, 1000)) +labs(x="Year", y="Average (per-person) October spending") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 2024, y = 5000, label = "1st quintile", angle = 0,  color = "black", size = 6, hjust = 0) +annotate("text", x = 2024, y = 6300, label = "2nd quintile", angle = 0,  color = "red", size = 6, hjust = 0)  +annotate("text", x = 2024, y = 9200, label = "3rd quintile", angle = -10,  color = "blue", size = 6, hjust = 0)  +annotate("text", x = 2024, y = 10500, label = "4th quintile", angle = 27,  color = "magenta", size = 6, hjust = 0)  +annotate("text", x = 2024, y = 15000, label = "5th quintile", angle = 23,  color = "darkgreen", size = 6, hjust = 0) +annotate("text", x = 2022, y = 13000, label = paste("5th CAGR =", round(cagr5,1),"%"), angle = 0,  color = "darkgreen", size = 6, hjust = 0) +annotate("text", x = 2022, y = 12000, label = paste("4th CAGR =", round(cagr4,1),"%"), angle = 0,  color = "magenta", size = 6, hjust = 0) +annotate("text", x = 2022, y = 11000, label = paste("3rd CAGR =", round(cagr3,1),"%"), angle = 0,  color = "blue", size = 6, hjust = 0) +annotate("text", x = 2022, y = 10000, label = paste("2nd CAGR =", round(cagr2,1),"%"), angle = 0,  color = "red", size = 6, hjust = 0) +annotate("text", x = 2022, y = 9000, label = paste("1st CAGR =", round(cagr1,1),"%"), angle = 0,  color = "black", size = 6, hjust = 0)

# Export to CSV
#write.csv(quant_month.df, "figure_1_main.csv", row.names = FALSE)
#
#write.csv(cagr.df, "figure_1_cagr.csv", row.names = FALSE)

#End: Figure 1: Avg spending by quantile 2021:2025####

#+++++++++++++++++

#Begin: Table 2: Ratios of Avg spending by quantile 2021:2025####

# ratio of 5th to 4th
(ratio_5_4.vec = round(quant_5.vec/quant_4.vec, 1))
# ratio of 5th to 3rd
(ratio_5_3.vec = round(quant_5.vec/quant_3.vec, 1))
# ratio of 5th to 2nd
(ratio_5_2.vec = round(quant_5.vec/quant_2.vec, 1))
# ratio of 5th to 4th
(ratio_5_1.vec = round(quant_5.vec/quant_1.vec, 1))
# ratio of 4th to 3rd
(ratio_4_3.vec = round(quant_4.vec/quant_3.vec, 1))
# ratio of 4th to 2nd
(ratio_4_2.vec = round(quant_4.vec/quant_2.vec, 1))
# ratio of 4th to 1st
(ratio_4_1.vec = round(quant_4.vec/quant_1.vec, 1))
# ratio of 3rd to 2nd
(ratio_3_2.vec = round(quant_3.vec/quant_2.vec, 1))
# ratio of 3rd to 1st
(ratio_3_1.vec = round(quant_3.vec/quant_1.vec, 1))
# ratio of 2nd to 1st
(ratio_2_1.vec = round(quant_2.vec/quant_1.vec, 1))

# make a data frame
(ratio_year.vec = c("Y2021", "Y2022", "Y2023", "Y2024", "Y2025"))
(ratio.df = data.frame(year = ratio_year.vec, ratio54 = ratio_5_4.vec, ratio53 = ratio_5_3.vec, ratio52 = ratio_5_2.vec, ratio51 = ratio_5_1.vec, ratio43 = ratio_4_3.vec, ratio42 = ratio_4_2.vec, ratio41 = ratio_4_1.vec, ratio32 = ratio_3_2.vec, ratio31 = ratio_3_1.vec, ratio21 = ratio_2_1.vec))

print(xtable(ratio.df, digits=1), include.rownames = F, hline.after = c(0))

# Export to CSV
#write.csv(ratio.df, "table_2.csv", row.names = FALSE)

#End: Table 2: Ratios of Avg spending by quantile 2021:2025####

#+++++++++++++++++

#Begin: Table A.1: List of merchant types ####
### Table describing the data
### Abbreviating Table 1's 21 merchant description (from Brian's slides)
# to be used in tables (see below further merchX_abv_fig for figures)
(merch1_abv = "1. Grocery store (N)")
(merch2_abv = "2. Gas station (N)")
(merch3_abv = "3. Restaurant/bar")
(merch4_abv = "4. Fast food/coffee shop")
(merch5_abv = "5. General merchandise store")
(merch6_abv = "6. General service")
(merch7_abv = "7. Art/entertainment")
(merch8_abv = "8. Non-government utility (N)")
(merch9_abv = "9. Taxi/airplane/delivery")
(merch10_abv = "10. Phone/internet/cable (N)")
(merch11_abv = "11. Contractor/plumb/elec")
(merch12_abv = "12. Professional service")
(merch13_abv = "13. Hotel/motel/campsite")
(merch14_abv = "14. Rent (N)")
(merch15_abv = "15. Mortgage/insur/loans (N)")
(merch16_abv = "16. Person-to-person")
(merch17_abv = "17. Charitable/religious")
(merch18_abv = "18. Hospital/doctor/dentist (N)")
(merch19_abv = "19. Government taxes (N)")
(merch20_abv = "20. School/college/childcare (N)")
(merch21_abv = "21. Public transport/tolls (N)")
# Make names a vector
(merch_abv.vec = c(merch1_abv, merch2_abv, merch3_abv, merch4_abv, merch5_abv, merch6_abv, merch7_abv, merch8_abv, merch9_abv, merch10_abv, merch11_abv, merch12_abv, merch13_abv, merch14_abv, merch15_abv, merch16_abv, merch17_abv, merch18_abv, merch19_abv, merch20_abv, merch21_abv))
# Finalizing merchant name table
(merch_abv.df = data.frame(merch_abv.vec))
dim(merch_abv.df)

# Adding num obs and avg spending pairs of years by merchant type
(merch_abv2.df = merch_abv.df %>% mutate(obs21 = merch_obs_2021.vec, avg21 = round(merch_avg_spend_2021.vec, digits = 0), obs22 = merch_obs_2022.vec, avg22 = round(merch_avg_spend_2022.vec, digits = 0), obs23 = merch_obs_2023.vec, avg23 = round(merch_avg_spend_2023.vec, digits = 0), obs24 = merch_obs_2024.vec, avg24 = round(merch_avg_spend_2024.vec, digits = 0), obs25 = merch_obs_2025.vec, avg25 = round(merch_avg_spend_2025.vec, digits = 0)))
#
#Row of all spending categories num obs and amount 
(all_spending.vec = c("All spending categories", total_payments_entire_2021, round(avg_spend_entire_2021, digits = 0), total_payments_entire_2022, round(avg_spend_entire_2022, digits = 0), total_payments_entire_2023, round(avg_spend_entire_2023, digits = 0), total_payments_entire_2024, round(avg_spend_entire_2024, digits = 0), total_payments_entire_2025, round(avg_spend_entire_2025, digits = 0)))
# append it at the bottom of the merchants table
(merch_abv3.df = rbind(merch_abv2.df, all_spending.vec))
dim(merch_abv3.df)

# also adding number of respondents (individuals) at the bottom row
(total_resp.vec = c("No.\ respondents (consumers)",  total_resp_2021, NA, total_resp_2022, NA, total_resp_2023, NA, total_resp_2024, NA, total_resp_2025, NA))
# Append it to the bottom
(merch_abv4.df = rbind(merch_abv3.df, total_resp.vec))
dim(merch_abv4.df)

print(xtable(merch_abv4.df, digits = 0), include.rownames = F, hline.after = c(0, 21))

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

# Export to CSV
write.csv(merch_abv4.df, "table_A1.csv", row.names = FALSE)

#End: Table A.1: List of merchant types ####

#+++++++++++++++++++

#Begin: Figure 2: grocery stores only####
avg_spend_2021g.vec
avg_spend_2022g.vec
avg_spend_2023g.vec
avg_spend_2024g.vec
avg_spend_2025g.vec

#1st quantile
(quant_1g.vec = c(avg_spend_2021g.vec[1], avg_spend_2022g.vec[1], avg_spend_2023g.vec[1], avg_spend_2024g.vec[1], avg_spend_2025g.vec[1]))
#2nd quantile
(quant_2g.vec = c(avg_spend_2021g.vec[2], avg_spend_2022g.vec[2], avg_spend_2023g.vec[2], avg_spend_2024g.vec[2], avg_spend_2025g.vec[2]))
#3rd quantile
(quant_3g.vec = c(avg_spend_2021g.vec[3], avg_spend_2022g.vec[3], avg_spend_2023g.vec[3], avg_spend_2024g.vec[3], avg_spend_2025g.vec[3]))
#4th quantile
(quant_4g.vec = c(avg_spend_2021g.vec[4], avg_spend_2022g.vec[4], avg_spend_2023g.vec[4], avg_spend_2024g.vec[4], avg_spend_2025g.vec[4]))
#5th quantile
(quant_5g.vec = c(avg_spend_2021g.vec[5], avg_spend_2022g.vec[5], avg_spend_2023g.vec[5], avg_spend_2024g.vec[5], avg_spend_2025g.vec[5]))

# make it a data frame (3 day spending)
(quantg.df = data.frame(year.vec = 2021:2025, quant1 = quant_1g.vec, quant2 = quant_2g.vec, quant3 = quant_3g.vec, quant4 = quant_4g.vec, quant5 = quant_5g.vec))
#
# make it monthly spending (instead of 3 days)
(quant_monthg.df = data.frame(year.vec = 2021:2025, quant1g = 31*quant_1g.vec/3, quant2g = 31*quant_2g.vec/3, quant3g = 31*quant_3g.vec/3, quant4g = 31*quant_4g.vec/3, quant5g = 31*quant_5g.vec/3))

#Compute CAGR (%) (reported inside Figure 1)
quant_1g.vec
length(quant_1g.vec)# 5 years
(cagr1g = 100*((tail(quant_1g.vec, 1) / quant_1g.vec[1])^(1 / (length(quant_1g.vec) - 1)) - 1))
#
(cagr2g = 100*((tail(quant_2g.vec, 1) / quant_2g.vec[1])^(1 / (length(quant_2g.vec) - 1)) - 1))
#
(cagr3g = 100*((tail(quant_3g.vec, 1) / quant_3g.vec[1])^(1 / (length(quant_3g.vec) - 1)) - 1))
#
(cagr4g = 100*((tail(quant_4g.vec, 1) / quant_4g.vec[1])^(1 / (length(quant_4g.vec) - 1)) - 1))
#
(cagr5g = 100*((tail(quant_5g.vec, 1) / quant_5g.vec[1])^(1 / (length(quant_5g.vec) - 1)) - 1))
# put them into a data frame (not used)
(cagrg.df = data.frame(Quantile =1:5, cagrg = round(c(cagr1g, cagr2g, cagr3g, cagr4g, cagr5g), 2)))

#plot 5 quintiles
ggplot(quant_monthg.df, aes(x=year.vec, y=quant1g)) + geom_line(linewidth = 1.0) +geom_point(size=1.9) + geom_line(aes(y=quant2g), linewidth = 1.0, color="red") +geom_point(aes(y=quant2g), size=1.9, color="red") +geom_line(aes(y=quant3g), linewidth = 1.0, color="blue") +geom_point(aes(y=quant3g), size=1.9, color="blue") +geom_line(aes(y=quant4g), linewidth = 1.0, color="magenta") +geom_point(aes(y=quant4g), size=1.9, color="magenta") +geom_line(aes(y=quant5g), linewidth = 1.0, color="darkgreen") +geom_point(aes(y=quant5g), size=1.9, color="darkgreen")+ scale_x_continuous(breaks = 2021:2025) + scale_y_continuous(breaks = seq(0, 1200, 50)) +labs(x="Year", y="Average October spending on groceries") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 2022, y = 700, label = "1st quintile", angle = 42,  color = "black", size = 6, hjust = 0) +annotate("text", x = 2021, y = 725, label = "2nd quintile", angle = 18,  color = "red", size = 6, hjust = 0) +annotate("text", x = 2022, y = 830, label = "3rd quintile", angle = 10,  color = "blue", size = 6, hjust = 0)  +annotate("text", x = 2023, y = 1020, label = "4th quintile", angle = 0,  color = "magenta", size = 6, hjust = 0) +annotate("text", x = 2024, y = 1025, label = "5th quintile", angle = 27,  color = "darkgreen", size = 6, hjust = 0) +annotate("text", x = 2023, y = 800, label = paste("5th CAGR =", round(cagr5g,1),"%"), angle = 0,  color = "darkgreen", size = 6, hjust = 0) +annotate("text", x = 2023, y = 750, label = paste("4th CAGR =", round(cagr4g,1),"%"), angle = 0,  color = "magenta", size = 6, hjust = 0) +annotate("text", x = 2023, y = 700, label = paste("3rd CAGR =", round(cagr3g,1),"%"), angle = 0,  color = "blue", size = 6, hjust = 0) +annotate("text", x = 2023, y = 650, label = paste("2nd CAGR =", round(cagr2g,1),"%"), angle = 0,  color = "red", size = 6, hjust = 0) +annotate("text", x = 2023, y = 600, label = paste("1st CAGR =", round(cagr1g,1),"%"), angle = 0,  color = "black", size = 6, hjust = 0)

# Export to CSV
#write.csv(quant_monthg.df, "figure_2_main.csv", row.names = FALSE)
#
#write.csv(cagrg.df, "figure_2_cagr.csv", row.names = FALSE)


#End: Figure 2: grocery stores only####

#+++++++++++++++++++

#Begin: Figure 3: necessities only####
avg_spend_2021n.vec
avg_spend_2022n.vec
avg_spend_2023n.vec
avg_spend_2024n.vec
avg_spend_2025n.vec

#1st quantile
(quant_1n.vec = c(avg_spend_2021n.vec[1], avg_spend_2022n.vec[1], avg_spend_2023n.vec[1], avg_spend_2024n.vec[1], avg_spend_2025n.vec[1]))
#2nd quantile
(quant_2n.vec = c(avg_spend_2021n.vec[2], avg_spend_2022n.vec[2], avg_spend_2023n.vec[2], avg_spend_2024n.vec[2], avg_spend_2025n.vec[2]))
#3rd quantile
(quant_3n.vec = c(avg_spend_2021n.vec[3], avg_spend_2022n.vec[3], avg_spend_2023n.vec[3], avg_spend_2024n.vec[3], avg_spend_2025n.vec[3]))
#4th quantile
(quant_4n.vec = c(avg_spend_2021n.vec[4], avg_spend_2022n.vec[4], avg_spend_2023n.vec[4], avg_spend_2024n.vec[4], avg_spend_2025n.vec[4]))
#5th quantile
(quant_5n.vec = c(avg_spend_2021n.vec[5], avg_spend_2022n.vec[5], avg_spend_2023n.vec[5], avg_spend_2024n.vec[5], avg_spend_2025n.vec[5]))

# make it a data frame (3 day spending)
(quantg.df = data.frame(year.vec = 2021:2025, quant1 = quant_1n.vec, quant2 = quant_2n.vec, quant3 = quant_3n.vec, quant4 = quant_4n.vec, quant5 = quant_5n.vec))
#
# make it monthly spending (instead of 3 days)
(quant_monthn.df = data.frame(year.vec = 2021:2025, quant1n = 31*quant_1n.vec/3, quant2n = 31*quant_2n.vec/3, quant3n = 31*quant_3n.vec/3, quant4n = 31*quant_4n.vec/3, quant5n = 31*quant_5n.vec/3))

#Compute CAGR (%) (reported inside Figure 1)
quant_1n.vec
length(quant_1n.vec)# 5 years
(cagr1n = 100*((tail(quant_1n.vec, 1) / quant_1n.vec[1])^(1 / (length(quant_1n.vec) - 1)) - 1))
#
(cagr2n = 100*((tail(quant_2n.vec, 1) / quant_2n.vec[1])^(1 / (length(quant_2n.vec) - 1)) - 1))
#
(cagr3n = 100*((tail(quant_3n.vec, 1) / quant_3n.vec[1])^(1 / (length(quant_3n.vec) - 1)) - 1))
#
(cagr4n = 100*((tail(quant_4n.vec, 1) / quant_4n.vec[1])^(1 / (length(quant_4n.vec) - 1)) - 1))
#
(cagr5n = 100*((tail(quant_5n.vec, 1) / quant_5n.vec[1])^(1 / (length(quant_5n.vec) - 1)) - 1))
# put them into a data frame (not used)
(cagrn.df = data.frame(Quantile =1:5, cagrn = round(c(cagr1n, cagr2n, cagr3n, cagr4n, cagr5n), 2)))

#plot 5 quintiles
ggplot(quant_monthn.df, aes(x=year.vec, y=quant1n)) + geom_line(linewidth = 1.0) +geom_point(size=1.9) + geom_line(aes(y=quant2n), linewidth = 1.0, color="red") +geom_point(aes(y=quant2n), size=1.9, color="red") +geom_line(aes(y=quant3n), linewidth = 1.0, color="blue") +geom_point(aes(y=quant3n), size=1.9, color="blue") +geom_line(aes(y=quant4n), linewidth = 1.0, color="magenta") +geom_point(aes(y=quant4n), size=1.9, color="magenta") +geom_line(aes(y=quant5n), linewidth = 1.0, color="darkgreen") +geom_point(aes(y=quant5n), size=1.9, color="darkgreen")+ scale_x_continuous(breaks = 2021:2025) + scale_y_continuous(breaks = seq(3000, 14000, 1000)) +labs(x="Year", y="Average October spending on necessities") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 2023, y = 3200, label = "1st quintile", angle = 2,  color = "black", size = 6, hjust = 0) +annotate("text", x = 2023, y = 4150, label = "2nd quintile", angle = 8,  color = "red", size = 6, hjust = 0) +annotate("text", x = 2023, y = 5100, label = "3rd quintile", angle = 25,  color = "blue", size = 6, hjust = 0)  +annotate("text", x = 2023, y = 6500, label = "4th quintile", angle = 25,  color = "magenta", size = 6, hjust = 0) +annotate("text", x = 2023, y = 12500, label = "5th quintile", angle = 3,  color = "darkgreen", size = 6, hjust = 0) +annotate("text", x = 2022, y = 11000, label = paste("5th CAGR =", round(cagr5n,1),"%"), angle = 0,  color = "darkgreen", size = 6, hjust = 0) +annotate("text", x = 2022, y = 10000, label = paste("4th CAGR =", round(cagr4n,1),"%"), angle = 0,  color = "magenta", size = 6, hjust = 0) +annotate("text", x = 2022, y = 9000, label = paste("3rd CAGR =", round(cagr3n,1),"%"), angle = 0,  color = "blue", size = 6, hjust = 0) +annotate("text", x = 2022, y = 8000, label = paste("2nd CAGR =", round(cagr2n,1),"%"), angle = 0,  color = "red", size = 6, hjust = 0) +annotate("text", x = 2022, y = 7000, label = paste("1st CAGR =", round(cagr1n,1),"%"), angle = 0,  color = "black", size = 6, hjust = 0)

# Export to CSV
#write.csv(quant_monthn.df, "figure_3_main.csv", row.names = FALSE)
#
#write.csv(cagrn.df, "figure_3_cagrn.csv", row.names = FALSE)

#End: Figure 3: necessities only####
