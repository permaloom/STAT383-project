graphics.off() # clear all previous plots
rm(list = ls()) # clear the environment from previous codes
cat("\014") # clear the console

library(ggplot2)
library(readxl)

df <- read_excel("consolidated_data.xlsx")
View(df)
head(df)

# change a column name

colnames(df)[3] <- "different"
sum(df$vote == "Donald Trump")
sum(df$vote == "Kamala Harris")
sum(df$vote == "Other")
nrow(df)

# proportion of Donald Trump voters
print(sum(df$vote == "Donald Trump") / nrow(df))

# proportion of Kamala Harris voters
print(sum(df$vote == "Kamala Harris") / nrow(df))

# proportion of Other voters
print(sum(df$vote == "Other") / nrow(df))

# Subsetting a dataset
df_males <- df[df$gender == 1, ]
df_females <- df[df$gender == 0, ]

n_males <- nrow(df_males)
n_females <- nrow(df_females)

male_votes_trump <- sum(df_males$vote == "Donald Trump")
female_votes_trump <- sum(df_females$vote == "Donald Trump")

#1 claim: male population from the survey is a good representation of the 18 to 29 year old male population of the likely voters in the U.S.

# H_0: p <= 0.49 
# H_A: p != 0.49

p_1 <- 0.49
n_1 <- n_males

p_hat_1 <- male_votes_trump / n_1

# ts = (p_hat - p)/sqrt((p * (1 - p)/n))
# ts ~ N(0,1)

# using sample proportion
ts_obs_1 <- (p_hat_1 - p_1) / sqrt((p_1 * (1 - p_1) / n_1))

# alpha = 0.05
# RR: (-inf, -1.96] ∪ [1.96, inf)

c_1 <- 1.96
decision_1 <- (abs(ts_obs_1) > abs(c_1))

if (decision_1 == TRUE) {
  print("Since TS_obs is in the rejection region, we reject H_0.")
} else {
  print("Since TS_obs is not in the rejection region, we fail to reject H_0.")
}

p_value_1 <- 2 * (1 - pnorm(abs(ts_obs_1)))
print(p_value_1)


#2 claim: the sample population is a good representation of young voters in the U.S.

# TODO adjust proportion_young_females_us
proportion_young_females_us <- 0.5

# find the derivation for this formula in claim 4
n_young_females <- ceiling(((proportion_young_females_us * n_males) /
                              (1 - proportion_young_females_us)))
adjusting_factor_young_females <- round(n_young_females / n_females, 2)

# TODO adjust p_2
p_2 <- 0.5
n_2 <- ceiling(n_males + adjusting_factor_young_females * n_females)

p_hat_2 <- (male_votes_trump + adjusting_factor_young_females *
              female_votes_trump) / n_2

ts_obs_2 <- (p_hat_2 - p_2) / sqrt((p_2 * (1 - p_2) / n_2))

# alpha = 0.05
# RR: (-inf, -1.96] ∪ [1.96, inf)

c_2 <- 1.96
decision_2 <- (abs(ts_obs_2) > abs(c_2))

if (decision_2 == TRUE) {
  print("Since TS_obs is in the rejection region, we reject H_0.")
} else {
  print("Since TS_obs is not in the rejection region, we fail to reject H_0.")
}

p_value_2 <- 2 * (1 - pnorm(abs(ts_obs_2)))
print(p_value_2)


# ----- North Country - New York State -----

females_st_lawrence <- 0.506
females_franklin <- 0.475
females_clinton <- 0.491
females_essex <- 0.487
females_hamilton <- 0.493
females_jefferson <- 0.473

females_north_country <- mean(females_st_lawrence,
                              females_franklin,
                              females_clinton,
                              females_essex,
                              females_hamilton,
                              females_jefferson)

# source https://spectrumlocalnews.com/nys/central-ny/politics/2024/11/07/how-new-york-voted-in-the-2024-presidential-election
result_st_lawrence <- 0.59
result_franklin <- 0.546
result_clinton <- 0.511
result_essex <- 0.502
result_hamilton <- 0.654
result_jefferson <- 0.62

result_north_country <- mean(result_st_lawrence,
                             result_franklin,
                             result_clinton,
                             result_essex,
                             result_hamilton,
                             result_jefferson)


# derivation for adjusted number of females and corresponding adjusting factor

# get number of how many females should be in the sample data to match the actual numbers of the north country
# n_females / (n_males + n_females) = females_north_country
# n_females = females_north_country * (n_males + n_females) 
# n_females = females_north_country * n_males + females_north_country * n_females
# n_females - females_north_country * n_females = females_north_country * n_males
# n_females * (1 - females_north_country) = females_north_country * n_males
# n_females = (females_north_country * n_males) / (1 - females_north_country)

# get the adjusting factor
# n_females_nc = proportion_females * n_females 
# proportion_females = n_females_nc/n_females


#3 claim: the adapted sample data is a good representation of voters in the north country

n_females_nc <- ceiling(((females_north_country * n_males) /
                           (1 - females_north_country)))

adjusting_factor_females_nc <- round(n_females_nc / n_females, 2)

p_3 <- result_north_country
n_3 <- ceiling(n_males + adjusting_factor_females_nc * n_females)

p_hat_3 <- (male_votes_trump + adjusting_factor_females_nc *
              female_votes_trump) / n_3

ts_obs_3 <- (p_hat_3 - p_3) / sqrt((p_3 * (1 - p_3) / n_3))

# alpha = 0.05
# RR: (-inf, -1.96] union [1.96, inf)

c_3 <- 1.96
decision_3 <- (abs(ts_obs_3) > abs(c_3))

if (decision_3 == TRUE) {
  print("Since TS_obs is in the rejection region, we reject H_0.")
} else {
  print("Since TS_obs is not in the rejection region, we fail to reject H_0.")
}

p_value_3 <- 2 * (1 - pnorm(abs(ts_obs_3)))
print(p_value_3)



#4 claim: taking into account when parents vote the same as their children the sample data is a good representation of voters in the north country

n_parents_data <- sum(df$different == 0)

n_males_including_parents <- n_males + n_parents_data
n_females_including_parents <- n_females + n_parents_data

parents_votes_trump <- sum(df$vote == "Donald Trump" & df$different == 0)

male_votes_trump_including_parents <- male_votes_trump + parents_votes_trump
female_votes_trump_including_parents <- female_votes_trump + parents_votes_trump

n_females_nc_including_parents <- ceiling(((females_north_country * n_males_including_parents) /
                           (1 - females_north_country)))

adjusting_factor_females_nc_including_parents <- round(n_females_nc_including_parents / n_females_including_parents, 2)

p_4 <- result_north_country
n_4 <- ceiling(n_males_including_parents + adjusting_factor_females_nc_including_parents * n_females_including_parents)

p_hat_4 <- (male_votes_trump_including_parents + adjusting_factor_females_nc_including_parents *
              female_votes_trump_including_parents) / n_4

ts_obs_4 <- (p_hat_4 - p_4) / sqrt((p_4 * (1 - p_4) / n_4))

# alpha = 0.05
# RR: (-inf, -1.96] union [1.96, inf)

c_4 <- 1.96
decision_4 <- (abs(ts_obs_4) > abs(c_4))

if (decision_4 == TRUE) {
  print("Since TS_obs is in the rejection region, we reject H_0.")
} else {
  print("Since TS_obs is not in the rejection region, we fail to reject H_0.")
}

p_value_4 <- 2 * (1 - pnorm(abs(ts_obs_4)))
print(p_value_4)
