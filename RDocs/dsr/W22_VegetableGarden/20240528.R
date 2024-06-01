#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork,               
  showtext, extrafont, ggtext, ggtext, glue ,
  highcharter
)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-05-28")

planting_2020 <- tuesdata$planting_2020
planting_2021 <- tuesdata$planting_2021

harvest_2020 <- tuesdata$harvest_2020
harvest_2021 <- tuesdata$harvest_2021

spending_2020 <- tuesdata$spending_2020
spending_2021 <- tuesdata$spending_2021

planting_2020 |> count(vegetable, sort = TRUE)
planting_2021 |> count(vegetable, sort = TRUE)
harvest_2020 |> count(vegetable, sort = TRUE)
harvest_2021 |> count(vegetable, sort = TRUE)
spending_2020 |> count(vegetable, sort = TRUE)
spending_2021 |> count(vegetable, sort = TRUE)
#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

## Overview: -----
# check that all similar data sets have same columns names across years
# remove unwanted columns: notes - eggplant_item_number - units

planting_2020 |> nrow()
planting_2020 |> head()
planting_2021 |> head()

harvest_2020 |> nrow()
harvest_2020 |> head()
harvest_2021 |> head()

spending_2020 |> nrow()
spending_2020 |> head()
spending_2021 |> head()

planting_2020 <- planting_2020 |> select(-notes)
planting_2021 <- planting_2021 |> select(-notes)

harvest_2020 <- harvest_2020 |> select(-units)
harvest_2021 <- harvest_2021 |> select(-units)

spending_2020 <- spending_2020 |> select(-eggplant_item_number)

# combining 2020 and 2021

pdf <- planting_2020 |> 
  mutate(year = 2020) |> 
  bind_rows(planting_2021 |> mutate(year = 2021))
pdf
hdf <- harvest_2020 |> 
  mutate(year = 2020) |> 
  bind_rows(harvest_2021 |> mutate(year = 2021)) |> 
  mutate(
    vegetable = case_when(
      vegetable == 'pumpkin' ~ 'pumpkins', 
      TRUE ~ vegetable))
hdf

sdf <- spending_2020 |> 
  mutate(year = 2020) |> 
  bind_rows(spending_2021 |> mutate(year = 2021))
sdf

## summary tables ----

hdf |> 
  group_by(year, vegetable) |> 
  summarise(total = sum(weight)) |> 
  arrange(desc(total)) |> 
  filter(year == 2021) |> 
  head(20) |> 
  pull(vegetable)

selected_veggies <- c("tomatoes", "pumpkins", "zucchini", "squash", "cucumbers", "rutabaga", 
                      "beans", "potatoes", "peas", "carrots", "beets", "corn", "lettuce", 
                      "peppers", "edamame", "kale")

## summarize by total seed, total weight, average price across varieties

sdf |> 
  filter(vegetable %in% selected_veggies) |> 
  #group_by(year, vegetable) |> 
  #summarise(avg_price = sum(price_with_tax), .groups = 'drop') |> 
  #filter(is.na(avg_price)) |> 
  filter(year == 2021)

yearly_veg_smry <- function(data, col, name, average = FALSE){
  
  if(average){
    data |> 
      filter(vegetable %in% selected_veggies) |> 
      group_by(year, vegetable) |> 
      summarise({{name}} := mean({{col}}, na.rm = TRUE), .groups = 'drop')
  } else {
    data |> 
      filter(vegetable %in% selected_veggies) |> 
      group_by(year, vegetable) |> 
      summarise({{name}} := sum({{col}}, na.rm = TRUE), .groups = 'drop')
  }
}

df <- 
  pdf |> 
  yearly_veg_smry(col = number_seeds_planted, name = total_seeds) |> 
  full_join(
    hdf |> yearly_veg_smry(col = weight, name = total_weight),
    by = join_by(year, vegetable)) |> 
  full_join(
    sdf |> 
      yearly_veg_smry(col = price_with_tax, name = avg_price, 
                      average = TRUE),
    by = join_by(year, vegetable)
  ) |> 
  mutate(
    total_seeds = replace_na(total_seeds, 0),
    avg_price = replace_na(avg_price, 0),
    vegetable = str_to_upper(vegetable))

write_csv(df, 'RDrafts/datasets/veggie_data1.csv')
  

df_price <- df |> 
  group_by(vegetable) |> 
  summarise(avg_price = mean(avg_price)) |> 
  filter(avg_price > 0)

write_csv(df_price, 'RDrafts/datasets/veggie_data2.csv')

unique(df$year)


pdf |> count(plot) |> n_distinct()

sdf |> summarise(total = sum(price))
sdf |> group_by(year) |> 
  summarise(total = sum(price)) |> 
  mutate(pct = total / sum(total))
sdf |> summarise(total = sum(price_with_tax))
hdf |> summarise(total = sum(weight))
#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")

font_families()
showtext_auto()


#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#

bg_color    <- "white"
text_light  <- ""
text_dark   <- ""
highlight   <- ""
title_color <- ""
sub_color   <- ""
cap_color   <- ""




#==========================================================================#
# Define texts and annotations ---------------------------------------------
#==========================================================================#
tsize = unit(30, units = "cm")

sysfonts::font_add(
  family  = "fab6_reg",
  regular = "Font Awesome 6 Brands-Regular-400.otf") #/Library/Fonts/

github          <- "&#xf09b"
github_username <- "birusod"

xtwitter          <- "&#xe61b"
xtwitter_username <- "@DSbyKnight"

social_caption <- glue::glue(
  "<span style='font-family:fab6_reg;'>{github};</span> 
  <span style='color: {cap_color}'>{github_username}  </span> 
  <span style='font-family:fab6_reg;'>{xtwitter};</span> 
  <span style='color: {cap_color}'>{xtwitter_username}</span>")
plot_caption <- paste0(
  "**Data**: TidyTuesday Week ##<br>", 
  "**Graphics:** ", 
  social_caption)


title    <- ""
subtitle <- ""


#==========================================================================#
# Custom Theme------------------------------------------------
#==========================================================================#

costum_theme <- function(){ 
  cfont <- "roboto"   
  theme_minimal(
    #base_family = cfont,
    #base_size = 40
  ) %+replace%    
    theme()
}


# highcharter theme ----

thm <- hc_theme_merge(
  hc_theme_darkunica(),
  hc_theme(
    title = list(style = list(color = "white",
                              fontFamily = "Bebas neue",
                              fontSize = "25px")),
    subtitle = list(style = list(
      color = "#17becf",
      fontFamily = "iosevka", #Shadows Into Light
      fontSize = "20px")),
    xAxis = list(lineColor = "grey20", tickColor = "grey20",
                 lineWidth = 1, tickWidth = 1,
                 gridLineColor = "transparent",
                 labels = list(enabled = TRUE, 
                               style = list( color = "white",
                                             fontSize = "20px")),
                 title = list(enabled = TRUE,
                              style = list(color = "white",
                                           fontSize = "20px"))),
    yAxis = list(lineColor = "grey20", tickColor = "grey20",
                 lineWidth = 1, tickWidth = 1,
                 gridLineColor = "transparent",
                 labels = list(enabled = TRUE, 
                               style = list( color = "white",
                                             fontSize = "20px")),
                 title = list(enabled = TRUE,
                              style = list(color = "white",
                                           fontSize = "20px"))),
    tooltip = list(
      backgroundColor = "#C9C8C3",
      style = list(color = "#000000", 
                   fontSize = "20px",
                   padding = "10px"))
    )
)



#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

## By total seeds ------
df |> 
  ggplot(
    aes(
      total_seeds, 
      fct_reorder(vegetable, total_seeds), 
      fill = factor(year))) +
  geom_col(position = 'dodge', show.legend = FALSE) +
  facet_wrap(~ year) +
  labs(
    x = NULL, y = NULL,
    title = 'TITLE',
    subtitle = 'Total seeds by veggie type'
  )

## By total weight -------
df |> 
  ggplot(
    aes(
      total_weight, 
      fct_reorder(vegetable, total_weight), 
      fill = factor(year))) +
  geom_col(position = 'dodge', show.legend = FALSE) +
  facet_wrap(~ year) +
  labs(
    x = NULL, y = NULL,
    title = 'TITLE',
    subtitle = 'Total weight by veggie type'
  )


## By average price -----
df_price |> 
  ggplot(
    aes(
      avg_price, 
      fct_reorder(vegetable, avg_price))) +
  geom_col(position = 'dodge', show.legend = FALSE) +
  labs(
    x = NULL, y = NULL,
    title = 'TITLE',
    subtitle = 'Average  price by veggie type'
  )

df_sorted <- df |> 
  filter(year == 2020, avg_price > 0) |> 
  arrange(desc(avg_price))
hchart(
  df_sorted,
  type = "bar",
  hcaes(x = vegetable, y = avg_price)
)

df_sorted |> 
  hchart(
    type = "bar",
    hcaes(x = vegetable, y = avg_price), 
    #color = "#800000", 
    name = "Average price") |>
  hc_title(text = "Vegetable Price") |>
  hc_subtitle(text = "Average Price in 2020") |>
  hc_yAxis(title = list(text = "Price (USD)")) |> 
  hc_xAxis(title = list(text = "")) |> 
  hc_tooltip(
    #crosshairs = TRUE, 
    borderColor = "black",
    borderWidth = 1,
    table = TRUE,
    pointFormat = ": ${point.avg_price:.2f}<br>"
             ) |> 
  hc_add_theme(thm)
  #hc_add_theme(hc_theme_darkunica())

#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W22_VegetableGarden",
                       "plots_w22", 
                       paste0("hc_plot_w22.jpeg"))
  )

ggsave(
  filename = here::here(),
  #plot    = plot,
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_col
)
