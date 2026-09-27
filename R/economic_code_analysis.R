##################################################################################
#           Script for Preparing multiple codewise graphs     
#           Finance Division, Ministry of Finance             
#           Prepared by:- Md. Anisur Rahman Bali              
################################################################################

rm(list = ls())
options(scipen = 999)
##################
# load necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(ragg)
library(ggplot2)
library(here)






df <- read_xlsx(here("data", "raw", "126_Science_detail_Total.xlsx"))

# here df specific columns are being divided by 10000


# Columns to be summed
value_cols <- c(
  "budget_2020_21", "budget_2021_22", "budget_2022_23", "budget_2023_24", "budget_2024_25", "budget_2025_26",
  "actual_2020_21", "actual_2021_22", "actual_2022_23", "actual_2023_24", "actual_2024_25", "actual_2025_26",
  "revised_2020_21", "revised_2021_22", "revised_2022_23", "revised_2023_24", "revised_2024_25", "revised_2025_26",
  "budget_2026_27", "budget_2027_28", "budget_2028_29"
)



df <- df %>%
  mutate(
    across(all_of(value_cols), ~ .x / 10000)
  )


#################################################################
#           Office Wise Economic Code Analysis for Operating Budget
################################################################


df_op <- df %>%
  filter(is.na(activity_code) | substr(as.character(activity_code), 1, 1) == "1" )



## summarize econnomic group over office groups



# make group office and generel / activity wise economic code values

df_op_group <- df_op %>% 
  group_by(group_code, group_name, activity_code, eco_code, eco_name) %>% 
  summarise(across(all_of(value_cols), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")



## for every office group and activity type generate a graph for each
# economic code under general activity or special activity

for (i in 1:nrow(df_op_group)){  #nrow(df_op_group)
  
  # df_op_group has 1596 rows it will generate all of them
  
  row <- df_op_group[i, ]  # every time taking only one row
  
  if (row$budget_2026_27 == 0){
    next
  }
  
  if (row$revised_2025_26 != 0 & row$budget_2026_27/row$revised_2025_26<1.1){
    next
  }
  
  row <- row %>% pivot_longer(
    cols = c(6:last_col()),
    names_to = "year",
    values_to = "amount"
  )
  
  row <- row %>%
    separate(
      year,
      into = c("type", "year"),
      sep = "_",
      extra = "merge"
    )
  
  econcode <- row$eco_code[1]
  codename <- row$eco_name[1]
  office_group <- row$group_code[1]
  office_name <- row$group_name[1]
  activity <- row$activity_code[1]
  
  # draw the image 
  agg_png(here("output", "office_group", office_group, paste0(econcode, ".png")), width = 8, height = 6, units = "in", res = 300)
  p <-ggplot(row, 
           aes(x = year, 
               y = amount, 
               group = type,
               color = type)) +
    geom_line(linewidth = 1) +
    
    geom_text(aes(label = sprintf("%.2f", amount)),
              vjust = -0.7,
              size = 3,
              show.legend = FALSE) +
    
    labs(
      title = paste(econcode, "-", codename, "    ", office_name, activity),
      color = "Legend",
      y = "টাকা (কোটি)"
      
    ) +
    scale_x_discrete(labels = c(
      "2020_21" = "2020-21",
      "2021_22" = "2021-22",
      "2022_23" = "2022-23",
      "2023_24" = "2023-24",
      "2024_25" = "2024-25",
      "2025_26" = "2025-26",
      "2026_27" = "2026-27"
    )) +
    theme_minimal(base_family = "NikoshBAN")
  
  print(p)
  dev.off()
  
  if (i %% 10 == 0){
  cat(round(i/nrow(df_op_group) * 100, 0),"%", "Complete\n" )
  }
}



########################################################################
#           Office and activity Wise total budget Analysis for Operating
#########################################################################



df_office <- df_op %>% 
  group_by(office_code, office_name, activity_code) %>% 
  summarise(across(all_of(value_cols), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")





## for every office group and activity type generate a graph for each
# economic code under general activity or special activity

# nrow(df_office) has 657 rows. running the following loop will generate 657 graphs

# we can shorten to specific criteria. For example, generate graphs only if the present year
# budget exceeds 5% of previous year's

for (i in 1:nrow(df_office)){  #nrow(df_office)
  
  # Condition to filter the graphs
  
  row <- df_office[i, ]
  # 
  # increase <- row$budget_2026_27 / row$revised_2025_26
  # 
  # if (is.nan(increase) | increase  < 1.05){
  #   next
  # }
  
  row <- row %>% pivot_longer(
    cols = c(4:last_col()),
    names_to = "year",
    values_to = "amount"
  )
  
  row <- row %>%
    separate(
      year,
      into = c("type", "year"),
      sep = "_",
      extra = "merge"
    )
  
  # econcode <- row$eco_code[1]
  # codename <- row$eco_name[1]
  office_code <- row$office_code[1]
  office_name <- row$office_name[1]
  
  # draw the image 
  agg_png(here("output", "office", paste0(office_code, ".png")), width = 8, height = 6, units = "in", res = 300)
  p <-ggplot(row, 
             aes(x = year, 
                 y = amount, 
                 group = type,
                 color = type)) +
    geom_line(linewidth = 1) +
    
    geom_text(aes(label = sprintf("%.2f", amount)),
              vjust = -0.7,
              size = 3) +
    
    
    labs(
      title = paste(office_group, "-", office_name),
      color = "Legend",
      y = "টাকা (কোটি)"
      
    ) +
    scale_x_discrete(labels = c(
      "2020_21" = "2020-21",
      "2021_22" = "2021-22",
      "2022_23" = "2022-23",
      "2023_24" = "2023-24",
      "2024_25" = "2024-25",
      "2025_26" = "2025-26",
      "2026_27" = "2026-27"
    )) +
    theme_minimal(base_family = "NikoshBAN")
  
  print(p)
  dev.off()
  
  
  if (i %% 10 == 0){
    print(cat(round(i/nrow(df_op_group) * 100, 0),"%", "Complete"))
  }
}

