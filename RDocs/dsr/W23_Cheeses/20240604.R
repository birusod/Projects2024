#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork,               
  showtext, extrafont, ggtext, ggtext, glue  
)
source('RDrafts/myfunc.R')

#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-06-04")
cheese_data <- tuesdata$cheeses
cheese_data |> sample_n(100) |> view()
#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

cheese_data |> missing_details() 

## By country: -----
# which countries are producing vegan vs vegetarian cheese?

country_df <- cheese_data |> 
  select(cheese, country, vegetarian, vegan) |> 
  mutate(country = case_when(
    str_detect(country, 'United Kingdom') ~ 'United Kingdom',
    TRUE ~ country)) |> 
  drop_na() |> 
  pivot_longer(c(3,4), names_to = 'diet', values_to = 'vals') |> 
  separate_rows(country, sep = ', ') |> 
  separate_rows(country, sep = ' and ') |> 
  mutate(country = trim(country),
         vals = as.numeric(vals)) 

# Vegetarian cheese: Top countries by frequency 

country_df |>
  filter(diet == 'vegetarian', 
         vals > 0) |> 
  group_by(country) |> 
  summarise(total = sum(vals)) |> 
  arrange(desc(total))
  
# Vegetarian cheese: Top 10 non USA countries by frequency 
country_top10 <- 
  country_df |>
  filter(diet == 'vegetarian', 
         vals > 0,
         country != 'United States') |> 
  group_by(country) |> 
  summarise(total = sum(vals)) |> 
  arrange(desc(total)) |> 
  head(10) |> 
  mutate(country  = str_to_upper(country))


## By  state -----

states_10 <- cheese_data |> 
  filter(str_detect(country, 'United States')) |> 
  separate_rows(country, sep = ', ') |> 
  filter(country == 'United States') |> 
  filter(!is.na(region)) |> 
  separate_rows(region, sep = ', ') |> 
  mutate(region = case_when(
    str_detect(region, 'NY') ~ 'New York',
    str_detect(region, 'VT') ~ 'Vermont',
    str_detect(region, 'MI') ~ 'Michigan',
    str_detect(region, 'MN') ~ 'Minnesota',
    str_detect(region, 'Mankato') ~ 'Minnesota',
    str_detect(region, 'Ann Arbor') ~ 'Michigan',
    str_detect(region, 'Oregon') ~ 'Oregon',
    str_detect(region, 'Sonoma') ~ 'California',
    str_detect(region, 'Port Townsend') ~ 'Washington',
    str_detect(region, 'Fairview') ~ 'Washington',
    str_detect(region, 'Tieton') ~ 'Washington',
    str_detect(region, 'Seattle') ~ 'Washington',
    str_detect(region, 'Greensboro') ~ 'North Carolina',
    TRUE ~ region)) |> 
  count(region, sort = TRUE) |>
  head(10) |> 
  mutate(region = str_to_upper(region)) |> 
  rename_with(~c('state', 'total'))



## By type -----

by_type <- cheese_data |> 
  select(cheese, type) |> #filter(is.na(type))
  separate_rows(type, sep = ', ') |> 
  mutate(type = replace_na(type, 'Unknown')) |> 
  count(type, sort = TRUE, name = 'total') |> 
  mutate(type = str_to_upper(type))


## By flavor --------
cheese_data |> filter(is.na(flavor)) #98, 8%

flavor_df <- cheese_data |> 
  select(flavor) |> 
  drop_na() |> 
  separate_rows(flavor, sep = ', ') |> 
  count(flavor, sort = TRUE, name = 'total')

flavor_df

## By Texture -----
texture_df <- cheese_data |> 
  select(texture) |> 
  drop_na() |> 
  separate_rows(texture, sep = ', ') |> 
  count(texture, sort = TRUE, name = 'total')


## By Color -----
colors_df <- cheese_data |> 
  select(color) |> 
  drop_na() |> 
  count(color, sort = TRUE, name = 'total')
colors_df

## By Milk used -----
milk_df <- cheese_data |> 
  select(milk) |> 
  drop_na() |> 
  separate_rows(milk, sep = ', ') |> 
  count(milk, sort = TRUE, name = 'total')
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



#==========================================================================#
# Custom Theme------------------------------------------------
#==========================================================================#

costum_theme <- function(){ 
  cfont <- "roboto"   
  theme_minimal(
    #base_family = cfont,
    #base_size = 40
  ) %+replace%    
    theme(
      plot.title = element_text(
        family = 'roboto', face = 'bold', size = 20),
      plot.subtitle = element_text(
        family = 'roboto', face = 'italic', size = 16, color = 'grey50',
        margin = margin(t = .1, b = .5, unit = 'cm')),
      plot.caption = element_text(
        family = 'roboto', color = 'grey70', size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(face = 'bold', size = 12),
      plot.margin = margin(.5, 1, .5, 0, unit = 'cm')
    )
}




#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

barplot <- function(data, colname, stitle){
  data |> 
    ggplot(aes(total, fct_reorder({{colname}}, total))) + 
    geom_col(alpha = .7) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = NULL, y = NULL,
         title = 'Cheese production',
         subtitle = stitle,
         caption = 'Source: TidyTuesday Week 23') +
    theme_light()
}


# By country ------

# top 10 nun-US countries:
country_top10 |> 
  barplot(
    country, 
    'Top 10 non-US countries for Vegetarian cheese') +
  costum_theme()

# By state ----
states_10 |> 
  barplot(state, 'Total Number of Cheese Type: Top 10 US States') +
  costum_theme()

# By type -----
by_type |> 
  barplot(type, 'Top 14 types') + costum_theme()

# By Falvor ----
flavor_df |> 
  #mutate(flavor = fct_lump_n(flavor, n = 40, w = total)) |> 
  arrange(desc(total)) |> 
  head(20) |> 
  barplot(flavor, 'Flavor Type Frequency: top 20') + costum_theme()

# By Texture -----
texture_df |> 
  barplot(texture, 'Texture Type Frequency') + costum_theme()

# By Texture -----
colors_df |> barplot(color, 'Cheese Colors Frequency') + costum_theme()

# By Milk used -----
milk_df |> 
  mutate(milk = fct_lump_n(milk, n = 3, w = total)) |> 
  barplot(milk, 'Frequency Of Milk Type Used') + costum_theme()
#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W23_Cheeses",
                       "plots_w23", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here(),
  #plot    = plot,
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_col
)
