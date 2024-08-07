#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork,               
  showtext, extrafont, ggtext, ggtext, glue, ggview
)
source('RDrafts/myfunc.R')
#df |> missing_details() 
#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-07-02")

dfr <- tuesdata$tt_urls
dfr |> head(20) |> view()

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

dfr |> colnames()
  
dfr |> count(year)

dfr |> 
  ggplot(aes(factor(year))) +
  geom_bar() +
  labs(x = NULL, y = NULL,
       title = 'Total Datasets Released By Year')


dfr |> 
  mutate(domain = fct_lump_n(domain, n = 20) |> fct_infreq() |> fct_rev()) |> 
  count(domain, sort = TRUE) |> 
  filter(domain != 'Other') |> 
  ggplot(aes(n, domain)) +
  geom_col() +
  labs(x = NULL, y = NULL,
       title = 'Total Datasets Released By Domain',
       subtitle = 'Top 20')


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




#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

ggview(units = 'cm', width = 20, height = 18)



#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W27_tidytuesdayR",
                       "plots_w27", 
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
