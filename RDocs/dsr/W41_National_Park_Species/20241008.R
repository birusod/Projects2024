#=============================================================#
# Load packages ----------------------------------------------
#=============================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes,              
  showtext, extrafont, ggtext, ggtext, glue,
  patchwork, ggview, gt, gtUtils
)
source('RDrafts/myfunc.R')
#df |> missing_details() 
#=============================================================#
# Loading data ------------------------------------------------
#=============================================================#

tuesdata <- tidytuesdayR::tt_load("2024-10-08")

df <- tuesdata$most_visited_nps_species_data |> 
  clean_names()

#=============================================================#
# Wrangling Data ---------------------------------------------
#=============================================================#
df |> count(park_name)

df |> count(category_name, sort = TRUE)
df |> count(order, sort = TRUE)
df |> count(family, sort = TRUE)

df |> 
  select(sensitive, park_accepted) |> 
  group_by(park_accepted) |> 
  count(sensitive)


df |> 
  filter(category_name == 'Amphibian', !is.na(family)) |> 
  mutate(family = fct_lump_n(family, n = 10)
           |> fct_infreq() |> fct_rev()) |> 
  count(family, sort = TRUE, name = 'total') |> 
  ggplot(aes(total, family)) +
  geom_col()

## Plotting function -----
plot_freq_family <- function(data, category, title = NULL, subtitle = NULL){
  data |> 
    filter(
      category_name == category, 
      !is.na(family)) |> 
    mutate(family = fct_lump_n(family, n = 10)
         |> fct_infreq() |> fct_rev()) |> 
    count(family, sort = TRUE, name = 'total') |> 
    ggplot(aes(total, family)) +
    geom_col() +
    labs(title = title,
         subtitle = subtitle,
         x = 'Total Number', 
         y = NULL)
}

df |> plot_freq_family('Reptile',
                       title = 'National Park Species',
                       subtitle = 'Reptiles: Most frequent families')

df |> plot_freq_family('Bird',
                       title = 'National Park Species',
                       subtitle = 'Birds: Most frequent families')

df |> plot_freq_family('Insect',
                       title = 'National Park Species',
                       subtitle = 'Insects: Most frequent families')

df |> plot_freq_family('Fish',
                       title = 'National Park Species',
                       subtitle = 'Fish: Most frequent families')



#=============================================================#
# Loading fonts ----------------------------------------------
#=============================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")

font_families()
showtext_auto()


#=============================================================#
# Defining colors --------------------------------------------
#=============================================================#

bg_color    <- "white"
text_light  <- ""
text_dark   <- ""
highlight   <- ""
title_color <- ""
sub_color   <- ""
cap_color   <- ""




#=============================================================#
# Define texts and annotations -------------------------------
#=============================================================#
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


#=============================================================#
# Custom Theme------------------------------------------------
#=============================================================#

costum_theme <- function(){ 
  cfont <- "roboto"   
  theme_minimal(
    #base_family = cfont,
    #base_size = 40
  ) %+replace%    
    theme()
}




#===========================================================#
# Data Viz -------------------------------------------------
#===========================================================#

ggview(units = 'cm', width = 20, height = 18)



#==========================================================#
# Saving Plots and Gifs -----------------------------------
#==========================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W41_National_Park_Species",
                       "plots_w41", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W41_National_Park_Species",
                        "plots_w41", 
                        paste0("final_plot", ".png")),
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_color
)
