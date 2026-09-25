##################################################################################
#########       Script for Training of Budget Officers            ###############
#########       Finance Division, Ministry of Finance             ##############
#########       Prepared by:- Md. Anisur Rahman Bali              ##############
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




if (Sys.getenv("USERNAME") == "bmani")  {
  drive <- "C:/Users/bmani/OneDrive/Finance Division/"
  
  
} else if (Sys.getenv("USERNAME") == "Shihab") {
  
  drive <- "C:/Users/User/OneDrive/Finance Division/"
  output <- "C:/github/office_task/output"
  
} else {
  "Device Not available"
}




file_path <- paste0(drive, "Trainings/R Training Materials")


df <- read_xlsx(paste0(file_path,"/116_detail_budget_Total.xlsx"))


#################################################################
#           Office Wise Economic Code Analysis for Operating
################################################################


## give the directory for output, I will save to the filepath


df_op <- df %>%
  filter(is.na(activity_code) | substr(as.character(activity_code), 1, 1) == "1" )


## get a list of offices

office_list <- df_op %>%
  distinct(office_code) 

## get a list of group offices

office_group <- df_op %>%
  distinct(group_code)

## summarize econnomic group over office groups

# Columns to be summed
value_cols <- c(
  "budget_2021_22", "budget_2022_23", "budget_2023_24", "budget_2024_25", "budget_2025_26",
  "actual_2021_22", "actual_2022_23", "actual_2023_24", "actual_2024_25", "actual_2025_26",
  "revised_2021_22", "revised_2022_23", "revised_2023_24", "revised_2024_25", "revised_2025_26",
  "budget_2026_27", "budget_2027_28", "budget_2028_29"
)


df_op_group <- df_op %>% 
  group_by(group_code, group_name, activity_code, eco_code, eco_name) %>% 
  summarise(across(all_of(value_cols), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")



## for every office group and activity type generate a graph for each
# economic code under general activity or special activity

for (i in 1:20){  #nrow(df_op_group)
  row <- df_op_group[i, ]
  
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
  
  # draw the image 
  agg_png(paste0(output,"/", office_group, "/", econcode, ".png"), width = 8, height = 6, units = "in", res = 300)
  p <-ggplot(row, 
           aes(x = year, 
               y = amount, 
               group = type,
               color = type)) +
    geom_line(linewidth = 1) +
    
    labs(
      title = paste(econcode, "-", codename, "    ", office_name),
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
  
  print(paste(office_group, "printing", econcode ))
  print(cat(i, "out of", nrow(df_op_group)))
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

for (i in 1:nrow(df_office)){  #nrow(df_office)
  row <- df_office[i, ]
  
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
  
  # econcode <- row$eco_code[1]
  # codename <- row$eco_name[1]
  office_group <- row$office_code[1]
  office_name <- row$office_name[1]
  
  # draw the image 
  agg_png(paste0(output,"/office_wise/", office_group, ".png"), width = 8, height = 6, units = "in", res = 300)
  p <-ggplot(row, 
             aes(x = year, 
                 y = amount, 
                 group = type,
                 color = type)) +
    geom_line(linewidth = 1) +
    
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
  
  # print(paste(office_group, "printing", econcode ))
  print(cat(i, "out of", nrow(df_office)))
}

