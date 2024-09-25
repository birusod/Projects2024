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

tuesdata <- tidytuesdayR::tt_load("2024-09-17")

roju <- tuesdata$romeo_juliet

#=============================================================#
# Wrangling Data ---------------------------------------------
#=============================================================#

roju |> 
  count(character, name = 'total') |> 
  ggplot(aes(total, fct_reorder(character, total))) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = 'Shakespeare Dialogue: R & J',
    subtitle = 'Characters speaking and stage directions',
    caption = 'TidyTuesday | week 38',
    x = NULL, y = NULL) +
  theme_light() +
  theme(
    axis.ticks.y = element_blank()
  )
 
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
  filename = file.path("W38_Shakespeare",
                       "plots_w38", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W38_Shakespeare",
                        "plots_w38", 
                        paste0("final_plot", ".png")),
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_color
)
