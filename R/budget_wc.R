##################################################################################
#########       Script for Preparing Working Paper Graphs         ###############
#########       Finance Division, Ministry of Finance             ##############
#########       Prepared by:- Md. Anisur Rahman Bali              ##############
################################################################################

rm(list = ls())

##################
# load necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(ragg)
library(ggplot2)


## load an excel file, it is stored in data folder inside raw, check it

df <- read_xlsx("data/raw/116_detail_budget_Total.xlsx")



# Columns haveing budget, actual and revised values. These columns will be summed
#  to get the total
# conditional sum of these columns also give operating budget, development budget



value_cols <- c(
  "budget_2020_21", "budget_2021_22", "budget_2022_23", "budget_2023_24", "budget_2024_25", "budget_2025_26",
  "actual_2020_21", "actual_2021_22", "actual_2022_23", "actual_2023_24", "actual_2024_25", "actual_2025_26",
  "revised_2020_21", "revised_2021_22", "revised_2022_23", "revised_2023_24", "revised_2024_25", "revised_2025_26",
  "budget_2026_27", "budget_2027_28", "budget_2028_29"
)

## convert all the value columns to crore by dividing 10000
# ibas gives the excel data in thousand
# here df specific columns are being divided by 10000

df <- df %>%
  mutate(
    across(all_of(value_cols), ~ .x / 10000)
  )




############################################
#------------------------------------------------------------------------------
#                 1-5 years total budget graph Graph 1
#-------------------------------------------------------------------------------

# make a dataframe with operating, development and total rows for graph




## get only one row for total of all columns



df_total <- bind_rows(
  
  df %>%
    filter(is.na(activity_code) |
             substr(as.character(activity_code), 1, 1) == "1") %>%
    summarise(across(all_of(value_cols), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(category = "Operating"),
  
  df %>%
    filter(substr(as.character(activity_code), 1, 1) == "2") %>%
    summarise(across(all_of(value_cols), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(category = "Development"),
  
  df %>%
    summarise(across(all_of(value_cols), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(category = "Total")
) %>%
  select(category, all_of(value_cols))




## convert the data to long format for plotting

df_long <- df_total %>%
  pivot_longer(cols = c(2:last_col()),
               names_to = "year",
               values_to = "amount")


# creating a seperate type columns based on the value in year column

df_long <- df_long %>%
  separate(
    year,
    into = c("type", "year"),
    sep = "_",
    extra = "merge"
  ) 

# keeping necessary rows for plotting on a bar

df_plot <- df_long %>%
  filter((year %in% c("2021_22", "2022_23", "2023_24", "2024_25") & type == "actual") |
          (year == "2025_26" & type == "revised") |
           (year %in% c("2026_27", "2027_28", "2028_29") & type == "budget"))


#####################################################################
#     Plotting the 5 year Data
#####################################################################




## color code for three bars

op_color    <- "#2E7D5B"   # Deep muted green
dev_color   <- "#3F6FA3"   # Professional blue
total_color <- "#B07A2A"   # Muted ochre/gold

agg_png("output/working paper/graph_1.png", width = 12,
        height = 8, units = "in", res = 300)

p <- ggplot(df_plot,
            aes(x = year, y = amount, fill = category)) +

  geom_col(position = position_dodge(width = 0.8),
         width = 0.7,
         color = NA) +
  
  
  scale_fill_manual(values = c(
    "Operating" = op_color,       # green
    "Development" = dev_color,
    "Total" = total_color),
  
    labels = c(
      "Operating" = "পরিচালন",
      "Development" = "উন্নয়ন",
      "Total" = "মোট"
    )) +
  
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = "টাকা (কোটি)",
    fill = NULL
  ) +
  
  # Add some space above labels
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12)),
    labels = scales::comma
  )  +

  scale_x_discrete(labels = c(
    "2021_22" = "2021-22 \n প্রকৃত",
    "2022_23" = "2022-23 \n প্রকৃত",
    "2023_24" = "2023-24 \n প্রকৃত",
    "2024_25" = "2024-25 \n প্রকৃত",
    "2025_26" = "2025-26  \n সংশোধিত",
    "2026_27" = "2026-27 \n প্রাক্কলন",
    "2027_28" = "2027-28 \n প্রক্ষেপন",
    "2028_29" = "2028-29 \n প্রক্ষেপন"
  )) +
  theme_minimal(base_family = "NikoshBAN", base_size = 24) +
  
  theme(
    # Background
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.key.size = unit(.5, "cm"),
    plot.title = element_text(face = "bold", size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E6E6E6", linewidth = 0.8),
    axis.text.x = element_text(family = "NikoshBAN", size = 15, colour = "black"),
    
    
    # Plot margins
    plot.margin = margin(
      t = 15,
      r = 20,
      b = 15,
      l = 15
    )
   ) +
  
  geom_text(aes(label = scales::comma(round(amount, 1))),
            position = position_dodge(width = 0.8),
            family = "NikoshBAN", size = 3, color = "black",
            vjust = -.5,
            angle = 0)

print(p)
dev.off()

##############################################################################
##############################################################################
#             Operating & Development Ratio
############################################################################



df_pie <- df_plot %>% 
  filter(category != "Total") %>%
  group_by(year) %>%
  mutate(
    pct = amount / sum(amount) * 100
  ) %>%
  ungroup()



agg_png("output/working paper/graph_2.png", width = 12,
        height = 8, units = "in", res = 300)


p <- ggplot(df_pie, aes(x = "", y = pct, fill = category)) +
  
  geom_col(
    width = 1,
    color = "white",
    linewidth = 0.8
  ) +
  
  geom_text(
    aes(
      label = paste0(round(pct, 1), "%")
    ),
    position = position_stack(vjust = 0.5),
    color = "white",
    family = "NikoshBAN",
    size = 6
  ) +
  
  coord_polar(theta = "y") +
  
  facet_wrap(~ year, ncol = 4,
             labeller=as_labeller(c(
               "2021_22" = "2021-22 \n প্রকৃত",
               "2022_23" = "2022-23 \n প্রকৃত",
               "2023_24" = "2023-24 \n প্রকৃত",
               "2024_25" = "2024-25 \n প্রকৃত",
               "2025_26" = "2025-26  \n সংশোধিত",
               "2026_27" = "2026-27 \n প্রাক্কলন",
               "2027_28" = "2027-28 \n প্রক্ষেপন",
               "2028_29" = "2028-29 \n প্রক্ষেপ"
             ))
             ) +
  
  scale_fill_manual(
    values = c(
      "Operating" = "#FA8072",
      "Development" = "#05DF72"
    ),
    labels = c(
      "Operating" = "পরিচালন",
      "Development" = "উন্নয়ন"
    )
  ) +
  
  theme_void(base_family = "NikoshBAN") +
  
  
  theme(
    
    text = element_text(
      family = "NikoshBAN"
    ),
    
    strip.text = element_text(
      family = "NikoshBAN",
      size = 14
    ),
    
    legend.text = element_text(
      family = "NikoshBAN",
      size = 15
    ),
    
    legend.position = "bottom",
    panel.spacing = unit(0.8, "cm")
  ) +
  
  labs(fill = NULL)

print(p)
dev.off()


############################################################################
############################################################################
#         Total budget & spending capacity 5 years
###########################################################################


df_total <- df %>% 
  summarise(across(all_of(value_cols), ~ sum(.x, na.rm = TRUE)))


df_long <- df_total %>%
  pivot_longer(cols = everything(),
               values_to = "amount",
               names_to = "year")

df_long <- df_long %>%
  separate(
    year,
    into = c("type", "year"),
    sep = "_",
    extra = "merge"
  ) 

df_long <- df_long %>%
  filter(year %in% c("2020_21", "2021_22", "2022_23", "2023_24", "2024_25")) %>% 
  group_by(type, year)%>%    ## make total of all categories
  summarise(amount = sum(amount, na.rm = TRUE),
            .groups = "drop")

saveRDS(df_long, "data/processed/table1.rds")


df_ratio <- df_long %>%
  group_by(year) %>%   ## make ratios
  summarise(
    ratio = amount[type == "actual"] / amount[type == "revised"],
    .groups = "drop"
  )

# Scaling factor for secondary axis
scale_factor <- max(df_long$amount, na.rm = TRUE) /
  max(df_ratio$ratio, na.rm = TRUE)



agg_png("output/working paper/graph_3.png", width = 12,
        height = 8, units = "in", res = 300)


p <- ggplot(df_long, aes(x = year, y = amount, fill = factor(type, levels = c("budget", "revised", "actual")))) +
  
  geom_col(position = position_dodge(width = 0.68),
           width = 0.64,
           color = NA) +
  
  
  # ----------------Bar labels--------------
  geom_text(
    aes(label = round(amount,2)),
    position = position_dodge(width = 0.68),
    hjust = .4,
    vjust = -.5,
    size = 3.5,
    angle = 0) +
  
  # ---------------- Line ----------------
geom_line(
  data = df_ratio,
  aes(x = year, 
      y = ratio * scale_factor, 
      group = 1,
      color = "ratio"),
  inherit.aes = FALSE,
  linewidth = 1.2
) +
  
  #--------- Point ----------------
  
  geom_point(
    data = df_ratio,
    aes(x = year, 
        y = ratio * scale_factor, 
        color = "ratio"),
    inherit.aes = FALSE,
    size = 2.5
  ) +
  
  #--------- Point label -------------
geom_text(
  data = df_ratio,
  aes(x = year, 
      y = ratio * scale_factor, 
      label = sprintf("%.2f%%", ratio*100),
      color = "ratio"),
  vjust = -0.5,
  inherit.aes = FALSE,
  size = 5.5,
  show.legend = FALSE
) +
  
  
  scale_y_continuous(
    name = "টাকা (কোটি)",
    sec.axis = sec_axis(~ . / scale_factor *100,
                        name = "শতকরা হার (%)")
  ) +

scale_fill_manual(values = c(
  "actual" = "#31D492",       # green
  "budget" = "#A684FF",
  "revised" = "#024A70"),
  labels = c(
    "actual" = "প্রকৃত",
    "budget" = "বাজেট",
    "revised" = "সংশোধিত"
  )) +
  
    scale_color_manual(values = c(
    "ratio" = "#3D45AA"),
    labels = c("ratio" = "প্রকৃত ব্যয় এর হার \n (সংশোধিত বাজেটের তুলনায়)")
  ) +
  
  labs(
    title = "বিভাগের গত ৫ অর্থবছরের বাজেট বরাদ্দ ও ব্যয়ের সক্ষমতা (সংশোধিত বাজেটের তুলনায়)",
    subtitle = "",
    x = NULL,
    fill = NULL
  )  +
  
  scale_x_discrete(labels = c(
    "2020_21" = "2020-21",
    "2021_22" = "2021-22",
    "2022_23" = "2022-23",
    "2023_24" = "2023-24",
    "2024_25" = "2024-25",
    "2025_26" = "2025-26"
  )) +
  
  theme_minimal(base_family = "NikoshBAN", base_size = 24) +
  
  theme(
    # Background
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E6E6E6", linewidth = 0.8),
    axis.text.x = element_text(family = "NikoshBAN", size = 15, colour = "black"),
    
    
    # Plot margins
    plot.margin = margin(
      t = 15,
      r = 20,
      b = 15,
      l = 15
    )
  )

print(p)
dev.off()



############################################################################
############################################################################
#         Operating budget & spending capacity 5 years
###########################################################################


df_total <- df %>% 
  filter(is.na(activity_code) | substr(as.character(activity_code),0,1) == "1")%>%
  summarise(across(all_of(value_cols), ~ sum(.x, na.rm = TRUE)))


df_long <- df_total %>%
  pivot_longer(cols = everything(),
               values_to = "amount",
               names_to = "year")

df_long <- df_long %>%
  separate(
    year,
    into = c("type", "year"),
    sep = "_",
    extra = "merge"
  ) 

df_long <- df_long %>%
  filter(year %in% c("2020_21", "2021_22", "2022_23", "2023_24", "2024_25")) %>% 
  group_by(type, year)%>%    ## make total of all categories
  summarise(amount = sum(amount, na.rm = TRUE),
            .groups = "drop")

saveRDS(df_long, "data/processed/table2.rds")



df_ratio <- df_long %>%
  group_by(year) %>%   ## make ratios
  summarise(
    ratio = amount[type == "actual"] / amount[type == "revised"],
    .groups = "drop"
  )

# Scaling factor for secondary axis
scale_factor <- max(df_long$amount, na.rm = TRUE) /
  max(df_ratio$ratio, na.rm = TRUE)



agg_png("output/working paper/graph_4.png", width = 12,
        height = 8, units = "in", res = 300)


p <- ggplot(df_long, aes(x = year, y = amount, fill = factor(type, levels = c("budget", "revised", "actual")))) +
  
  geom_col(position = position_dodge(width = 0.68),
           width = 0.64,
           color = NA) +
  
  
  # ----------------Bar labels--------------
geom_text(
  aes(label = round(amount,2)),
  position = position_dodge(width = 0.68),
  hjust = .4,
  vjust = -.5,
  size = 3.5,
  angle = 0) +
  
  # ---------------- Line ----------------
geom_line(
  data = df_ratio,
  aes(x = year, 
      y = ratio * scale_factor, 
      group = 1,
      color = "ratio"),
  inherit.aes = FALSE,
  linewidth = 1.2
) +
  
  #--------- Point ----------------

geom_point(
  data = df_ratio,
  aes(x = year, 
      y = ratio * scale_factor, 
      color = "ratio"),
  inherit.aes = FALSE,
  size = 2.5
) +
  
  #--------- Point label -------------
geom_text(
  data = df_ratio,
  aes(x = year, 
      y = ratio * scale_factor, 
      label = sprintf("%.2f%%", ratio*100),
      color = "ratio"),
  vjust = -0.5,
  inherit.aes = FALSE,
  size = 5.5,
  show.legend = FALSE
) +
  
  
  scale_y_continuous(
    name = "টাকা (কোটি)",
    sec.axis = sec_axis(~ . / scale_factor *100,
                        name = "শতকরা হার (%)")
  ) +
  
  scale_fill_manual(values = c(
    "actual" = "#31D492",       # green
    "budget" = "#A684FF",
    "revised" = "#024A70"),
    labels = c(
      "actual" = "প্রকৃত",
      "budget" = "বাজেট",
      "revised" = "সংশোধিত"
    )) +
  
  scale_color_manual(values = c(
    "ratio" = "#3D45AA"),
    labels = c("ratio" = "প্রকৃত ব্যয় এর হার \n (সংশোধিত বাজেটের তুলনায়)")
  ) +
  
  labs(
    title = "বিভাগের গত ৫ অর্থবছরের পরিচালন ব্যয়ের সক্ষমতা (সংশোধিত বাজেটের তুলনায়)",
    subtitle = "",
    x = NULL,
    fill = NULL
  )  +
  
  scale_x_discrete(labels = c(
    "2020_21" = "2020-21",
    "2021_22" = "2021-22",
    "2022_23" = "2022-23",
    "2023_24" = "2023-24",
    "2024_25" = "2024-25",
    "2025_26" = "2025-26"
  )) +
  
  theme_minimal(base_family = "NikoshBAN", base_size = 24) +
  
  theme(
    # Background
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E6E6E6", linewidth = 0.8),
    axis.text.x = element_text(family = "NikoshBAN", size = 15, colour = "black"),
    
    
    # Plot margins
    plot.margin = margin(
      t = 15,
      r = 20,
      b = 15,
      l = 15
    )
  )

print(p)
dev.off()


############################################################################
############################################################################
#         Development budget & spending capacity 5 years
###########################################################################


df_total <- df %>% 
  filter(substr(as.character(activity_code),0,1)  %in% c("2", "3") )%>%
  summarise(across(all_of(value_cols), ~ sum(.x, na.rm = TRUE)))


df_long <- df_total %>%
  pivot_longer(cols = everything(),
               values_to = "amount",
               names_to = "year")

df_long <- df_long %>%
  separate(
    year,
    into = c("type", "year"),
    sep = "_",
    extra = "merge"
  ) 

df_long <- df_long %>%
  filter(year %in% c("2020_21", "2021_22", "2022_23", "2023_24", "2024_25")) %>% 
  group_by(type, year)%>%    ## make total of all categories
  summarise(amount = sum(amount, na.rm = TRUE),
            .groups = "drop")


saveRDS(df_long, "data/processed/table3.rds")

df_ratio <- df_long %>%
  group_by(year) %>%   ## make ratios
  summarise(
    ratio = amount[type == "actual"] / amount[type == "revised"],
    .groups = "drop"
  )

# Scaling factor for secondary axis
scale_factor <- max(df_long$amount, na.rm = TRUE) /
  max(df_ratio$ratio, na.rm = TRUE)



agg_png("output/working paper/graph_5.png", width = 12,
        height = 8, units = "in", res = 300)


p <- ggplot(df_long, aes(x = year, y = amount, fill = factor(type, levels = c("budget", "revised", "actual")))) +
  
  geom_col(position = position_dodge(width = 0.68),
           width = 0.64,
           color = NA) +
  
  
  # ----------------Bar labels--------------
geom_text(
  aes(label = round(amount,2)),
  position = position_dodge(width = 0.68),
  hjust = .4,
  vjust = -.5,
  size = 3.5,
  angle = 0) +
  
  # ---------------- Line ----------------
geom_line(
  data = df_ratio,
  aes(x = year, 
      y = ratio * scale_factor, 
      group = 1,
      color = "ratio"),
  inherit.aes = FALSE,
  linewidth = 1.2
) +
  
  #--------- Point ----------------

geom_point(
  data = df_ratio,
  aes(x = year, 
      y = ratio * scale_factor, 
      color = "ratio"),
  inherit.aes = FALSE,
  size = 2.5
) +
  
  #--------- Point label -------------
geom_text(
  data = df_ratio,
  aes(x = year, 
      y = ratio * scale_factor, 
      label = sprintf("%.2f%%", ratio*100),
      color = "ratio"),
  vjust = -0.5,
  inherit.aes = FALSE,
  size = 5.5,
  show.legend = FALSE
) +
  
  
  scale_y_continuous(
    name = "টাকা (কোটি)",
    sec.axis = sec_axis(~ . / scale_factor *100,
                        name = "শতকরা হার (%)")
  ) +
  
  scale_fill_manual(values = c(
    "actual" = "#31D492",       # green
    "budget" = "#A684FF",
    "revised" = "#024A70"),
    labels = c(
      "actual" = "প্রকৃত",
      "budget" = "বাজেট",
      "revised" = "সংশোধিত"
    )) +
  
  scale_color_manual(values = c(
    "ratio" = "#3D45AA"),
    labels = c("ratio" = "প্রকৃত ব্যয় এর হার \n (সংশোধিত বাজেটের তুলনায়)")
  ) +
  
  labs(
    title = "বিভাগের বিগত ৫ অর্থবছরের উন্নয়ন ব্যয়ের সক্ষমতা (সংশোধিত বাজেটের তুলনায়)",
    subtitle = "",
    x = NULL,
    fill = NULL
  )  +
  
  scale_x_discrete(labels = c(
    "2020_21" = "2020-21",
    "2021_22" = "2021-22",
    "2022_23" = "2022-23",
    "2023_24" = "2023-24",
    "2024_25" = "2024-25",
    "2025_26" = "2025-26"
  )) +
  
  theme_minimal(base_family = "NikoshBAN", base_size = 24) +
  
  theme(
    # Background
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E6E6E6", linewidth = 0.8),
    axis.text.x = element_text(family = "NikoshBAN", size = 15, colour = "black"),
    
    
    # Plot margins
    plot.margin = margin(
      t = 15,
      r = 20,
      b = 15,
      l = 15
    )
  )

print(p)
dev.off()


