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



library(dplyr)

df_summary <- df %>%
  summarise(
    total_budget = sum(budget),
    total_actual = sum(actual),
    average_actual = mean(actual)
  )

df_summary2 <- df %>%
  group_by(type)%>%
  summarise(
    total_budget = sum(budget),
    total_actual = sum(actual),
    average_actual = mean(actual)
  )

df_summary3 <-	df %>%
  group_by(type) %>%
  summarise(
    across(c(budget, revised, actual), ~mean(.x, na.rm = TRUE))
  )


## long format
library(tidyr)

df_long <- df %>%
  pivot_longer(
    cols = c(budget, revised, actual),
    names_to = "category",
    values_to = "value"
  )

# to order the column bars
df_long$category <- factor(
  df_long$category,
  levels = c("budget", "revised", "actual")
)

## ggplot

# for 

library(ggplot2)

ggplot(
  df_long,
  aes(
    x = year,
    y = value,
    fill = category
  )
) +
   
  # Bars
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  # Edit the titles of the plot, x and y axis
  labs(
    title = "Budget, Actual and Revised Expenditure",
    x = "Fiscal Year",
    y = "Expenditure",
    fill = NULL
  ) +
  
  # edit the bar color and legend label
  scale_fill_manual(
    values = c(
      budget = "#4472C4",
      actual = "#70AD47",
      revised = "#ED7D31"
    ),
    labels = c(
      budget = "Budget",
      actual = "Actual",
      revised = "Revised"
    )
  ) +
  
  # edit the y axis values
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # x axis to show all the years
  scale_x_continuous(
    breaks = 2015:2025,
    labels = 2015:2025
  ) +
  
  # a theme to use
  theme_minimal(base_size = 14) +
  
  # editing 
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold"
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

p
