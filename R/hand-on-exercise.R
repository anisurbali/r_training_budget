##################################################################################
#########       Script for Preparing Working Paper Graphs         ###############
#########       Finance Division, Ministry of Finance             ##############
#########       Prepared by:- Md. Anisur Rahman Bali              ##############
################################################################################


#Exercise 1

2 + 3
10 * 5
100 / 4
2^5
sqrt(25)


#Exercise 2

# Calculate the average expenditure

x <- c(100, 120, 150, 180)

mean(x)

#Exercise 3
#Create some variables and assign values to them

x <- 100
name <- "Bangladesh"
rate <- 0.075


#Exercise 4
# Create a dataframe

df <- data.frame(
  year = c("2023-24", "2024-25", "2025-26"),
  budget = c(100, 120, 150),
  actual = c(95, 110, 130)
)



# Exercise 5
# import an excel file

library(readxl)

df <- read_xlsx("data/raw/exercise.xlsx")

# here() package is more useful for opening a file

library(here)
df <- read_xlsx(here("data", "raw", "exercise.xlsx"))

# Explore the data


head(df)
tail(df)

str(df)
summary(df)

nrow(df)
ncol(df)

names(df)
