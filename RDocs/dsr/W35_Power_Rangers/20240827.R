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

tuesdata <- tidytuesdayR::tt_load("2024-08-27")
eps <- tuesdata$power_rangers_episodes
sss <- tuesdata$power_rangers_seasons

#=============================================================#
# Wrangling Data ---------------------------------------------
#=============================================================#

eps |> head(20) |> view()
sss |> head(20) |> view()


## average rating by year
sss |> 
  mutate(year = year(air_date_first_ep)) |> 
  summarise(avg = mean(IMDB_rating), .by = year) |> 
  ggplot(aes(year, avg)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 8)) +
  labs(title = 'Episodes Average Rating (IMDB) by Year',
       x = NULL, y = 'yearly average') +
  theme_light()


## average rating by season
sss |> 
  summarise(avg = mean(IMDB_rating), .by = season_num) |> 
  ggplot(aes(factor(season_num), avg)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 7.5)) +
  labs(title = 'Episodes Average Rating (IMDB) by Season',
       x = 'Seasons', y = 'Average') +
  theme_light()



## IMDB Rating vs 
eps |> 
  ggplot(aes(IMDB_rating, total_votes)) +
  geom_point()

## average votes by year
eps |> 
  mutate(year = year(air_date)) |> 
  summarise(avg = mean(total_votes), .by = year) |> 
  ggplot(aes(year, avg)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 510)) +
  labs(title = 'Average Votes by Year',
       x = NULL, y = 'yearly average') +
  theme_light()


## average rating by season
eps |> 
  mutate(season = str_extract(season_title, '(?<=\\()\\w+(?=\\)')) |> 
  select(season_title, season)
  summarise(avg = mean(total_votes), .by = season_num) |> 
  ggplot(aes(factor(season_num), avg)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 7.5)) +
  labs(title = 'Average Rating (IMDB) by Season',
       x = 'Seasons', y = 'Average') +
  theme_light()

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
  filename = file.path("W34_Power_Rangers",
                       "plots_w35", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W34_Power_Rangers",
                        "plots_w35", 
                        paste0("final_plot", ".png")),
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_color
)
