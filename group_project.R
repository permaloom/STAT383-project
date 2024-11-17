graphics.off() # clear all previous plots
rm(list=ls()) # clear the environment from previous codes
cat("\014") # clear the consol

library(ggplot2)
library(readxl)
df <- read_excel("consolidated_data.xlsx") 
View(df)
head(df)

# change a column name

colnames(df)[3] <- "different"
sum(df$vote=="Donald Trump")
sum(df$vote=="Kamala Harris")
sum(df$vote=="Other")
nrow(df)

# proportion of Donald Trump voters
print(sum(df$vote=="Donald Trump")/nrow(df))

# proportion of Kamala Harris voters
print(sum(df$vote=="Kamala Harris")/nrow(df))

# proportion of Other voters
print(sum(df$vote=="Other")/nrow(df))

# Subsetting a dataset

df_males <- df[df$gender==1,]

df_females <- df[df$gender==0,]

# proportion of Donald Trump voters amongst male students
print(sum(df_males$vote=="Donald Trump")/nrow(df_males))

# proportion of Donald Trump voters amongst female students
print(sum(df_females$vote=="Donald Trump")/nrow(df_females))


# St. Lawrence County 
females_st_lawrence <- 0.506
females_franklin <- 0.475
females_clinton <- 0.491
females_essex <- 0.487
females_hamilton <- 0.493
females_jefferson <- 0.473

females_north_country <- mean(females_st_lawrence, females_franklin, females_clinton, females_essex, females_hamilton, females_jefferson)

total_count <- nrow(df)

# target number of females and males based on the state's gender ratio
target_females <- round(total_count * 0.488) 
target_males <- total_count - target_females

# Resample females and males to match the target counts
set.seed(123)
df_females_adjusted <- df_females[sample(1:nrow(df_females), target_females, replace = TRUE), ]
df_males_adjusted <- df_males[sample(1:nrow(df_males), target_males, replace = TRUE), ]

# combine adjusted female and male datasets
df_adjusted <- rbind(df_females_adjusted, df_males_adjusted)

# female adjusted
sum(df_adjusted$gender == 0)
# proportion female adjusted
sum(df_adjusted$gender == 0) / nrow(df_adjusted)

# male adjusted
sum(df_adjusted$gender == 1)
# propotion male adjusted
sum(df_adjusted$gender == 1) / nrow(df_adjusted)


proportions_overall <- data.frame(
  Candidate = c("Donald Trump", "Kamala Harris", "Other"),
  Proportion = c(
    sum(df$vote == "Donald Trump") / nrow(df),
    sum(df$vote == "Kamala Harris") / nrow(df),
    sum(df$vote == "Other") / nrow(df)
  )
)

proportions_by_gender <- data.frame(
  Gender = rep(c("Male", "Female"), each = 3),
  Candidate = rep(c("Donald Trump", "Kamala Harris", "Other"), times = 2),
  Proportion = c(
    sum(df_males$vote == "Donald Trump") / nrow(df_males),
    sum(df_males$vote == "Kamala Harris") / nrow(df_males),
    sum(df_males$vote == "Other") / nrow(df_males),
    sum(df_females$vote == "Donald Trump") / nrow(df_females),
    sum(df_females$vote == "Kamala Harris") / nrow(df_females),
    sum(df_females$vote == "Other") / nrow(df_females)
  )
)

# Plot overall proportions
ggplot(proportions_overall, aes(x = Candidate, y = Proportion, fill = Candidate)) +
  geom_bar(stat = "identity") +
  labs(title = "Overall Voting Proportions", x = "Candidate", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()()

ggsave("overall_voting_proportions.jpg", width = 7, height = 7, dpi = 300)

# Plot gender-based proportions
ggplot(proportions_by_gender, aes(x = Candidate, y = Proportion, fill = Candidate)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Gender) +
  labs(title = "Voting Proportions by Gender", x = "Candidate", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()()

ggsave("overall_voting_proportions_by_gender.jpg", width = 7, height = 7, dpi = 300)
