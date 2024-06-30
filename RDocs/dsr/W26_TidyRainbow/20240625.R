#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork,               
  showtext, extrafont, ggtext, ggtext, glue, ggview,
  gglgbtq
)
source('RDrafts/myfunc.R')
#df |> missing_details() 
#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-06-25")
dfr <- tuesdata$lgbtq_movies 
dfr |> head()

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

## Overview  -----
dfr |> nrow()
dfr |> missing_details()
dfr |> count()

dfr |> filter(year(release_date) <= 1900) |> pull(title)

## Cleaning ------
# Variables selection 
# filtering missing values 

df <- 
  dfr |> 
  select(id, date = release_date, title, popularity, vote_average, vote_count,
         language = original_language, adult, video) |> 
  filter(!is.na(date)) |> 
  mutate(month = month(date, abbr = TRUE, label = TRUE),
         year = year(date))

df

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


main_title <- "TIDYRAINBOW DATA PROJECT"
subtitle_year <- "Numner Of Movies Release By Year Since 1960"


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




#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

### By year (after 1960)-----
df |> 
  filter(year >= 1960) |> 
  ggplot(aes(x = year)) +
  geom_bar() +
  labs(title = main_title, subtitle = subtitle_year)

df |> 
  filter(year >= 1960) |> 
  ggplot(aes(x = year, fill = adult)) +
  geom_bar() +
  scale_fill_manual(values = palette_lgbtq("pansexual")) +
  scale_x_continuous(
    expand = c(0,0),
    breaks = c(seq(1970, 2020, 10))) +
  labs(title = main_title, 
       subtitle = subtitle_year,
       fill = 'Adult genre') +
  theme_lgbtq("pansexual") +
  theme(
    legend.position = c(.1, .85),
    legend.background = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = .2)
    )


df |> 
  filter(language != 'en') |> 
  mutate(language = fct_lump_n(language, n = 10) |> 
           fct_infreq() |> 
           fct_rev()) |> 
  count(language, sort = TRUE) |> 
  ggplot(aes(n, language)) +
  geom_col()


df |> 
  filter(popularity > 0) |> 
  ggplot(aes(popularity, vote_average)) +
  geom_point() +
  scale_x_log10()
  


ggview(units = 'cm', width = 16, height = 9)

#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W26_TidyRainbow",
                       "plots_w26", 
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
