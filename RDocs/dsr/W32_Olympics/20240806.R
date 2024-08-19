#=============================================================#
# Load packages ----------------------------------------------
#=============================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes,              
  showtext, extrafont, ggtext, ggtext, glue,
  patchwork, ggview
)
source('RDrafts/myfunc.R')
#df |> missing_details() 
#=============================================================#
# Loading data ------------------------------------------------
#=============================================================#

tuesdata <- tidytuesdayR::tt_load("2024-08-06")
dfr <- tuesdata$olympics
dfr |> sample_n(20) |> view()
#=============================================================#
# Wrangling Data ---------------------------------------------
#=============================================================#

## post 1960 -----
df <- dfr |> filter(year >= 1960)

## medals over the years -----

df |> 
  filter(!is.na(medal)) |> 
  count(medal)

df |> 
  filter(!is.na(medal)) |> 
  count(year, medal) |> 
  ggplot(aes(year, n, fill =  medal)) +
  geom_col() 

df |> 
  filter(!is.na(medal)) |> 
  count(year, season, medal) |> 
  ggplot(aes(year, n, fill =  medal)) +
  geom_col(position = 'dodge') +
  facet_wrap(~season, ncol = 1, scales = 'free_y')


## participation -----

# by year and season
df |> 
  count(year, season) |> 
  ggplot(aes(year, n, fill = season)) +
  geom_col() 


## by year and country -------
# summer
df |> 
  filter(season == 'Summer') |> 
  count(noc, sort = TRUE) |> 
  head(20) |> 
  ggplot(aes(n, fct_reorder(noc, n))) +
  geom_col()

df |> 
  filter(season == 'Summer') |> 
  count(noc, sort = TRUE) |> 
  tail(20) |> 
  ggplot(aes(n, fct_reorder(noc, n))) +
  geom_col()

## by year and country -------
# winter
df |> 
  filter(season == 'Winter') |> 
  count(noc, sort = TRUE) |> 
  head(20) |> 
  ggplot(aes(n, fct_reorder(noc, n))) +
  geom_col()

df |> 
  filter(season == 'Winter') |> 
  count(noc, sort = TRUE) |> 
  tail(20) |> 
  ggplot(aes(n, fct_reorder(noc, n))) +
  geom_col()

## by sport -----
# summer
df |> 
  filter(season == 'Summer') |> 
  count(sport) |> 
  ggplot(aes(n, fct_reorder(sport, n))) +
  geom_col()

# winter
df |> 
  filter(season == 'Winter') |> 
  count(sport) |> 
  ggplot(aes(n, fct_reorder(sport, n))) +
  geom_col()


## By athlete 
df |> 
  filter(season == 'Summer') |> 
  count(year, name) |> 
  count(name, sort = TRUE, name = 'participation') |> 
  head(30) |> 
  ggplot(aes(participation, fct_reorder(name, participation))) +
  geom_col() +
  scale_x_continuous(breaks = breaks_pretty())

df |> 
  filter(season == 'Winter') |> 
  count(year, name) |> 
  count(name, sort = TRUE, name = 'participation') |> 
  head(30) |> 
  ggplot(aes(participation, fct_reorder(name, participation))) +
  geom_col() +
  scale_x_continuous(breaks = breaks_pretty())



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
  filename = file.path("W32_Olympics",
                       "plots_w32", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W32_Olympics",
                        "plots_w32", 
                        paste0("final_plot", ".png")),
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_color
)
