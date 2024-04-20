#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork,               
  showtext, extrafont, ggtext, ggtext, glue,
  shiny, shinyWidgets, shinythemes
)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-04-16")


#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#
shiny_revdeps <- tuesdata$shiny_revdeps
pkg_details <- tuesdata$package_details |> clean_names()

shiny_revdeps |> count(parent, sort = TRUE)


shiny_details |> head(20) |> view()

## Package  authors  -----
authors_top_50 <- 
  pkg_details |>
  select(package, author) |> 
  mutate(author  = str_remove_all(author, "(\\[.+\\])")) |> 
  mutate(author  = str_remove_all(author, "\\([^()]*\\)")) |> 
  mutate(author  = str_remove_all(author, "\n ")) |> 
  separate_rows(author, sep = ', ') |> 
  mutate(author = str_trim(author)) |> 
  filter(!author %in% c(
    'PBC', 'Posit Software', 'RStudio', 'Inc.', 'Windsor.ai',
    'F. Hoffmann-La Roche AG', 'Posit', 'R Core Team',
    'Robin K. S. Hankin')) |> 
  count(author, sort = TRUE, name = 'total')  |> 
  mutate(author = fct_reorder(author, total)) |> 
  head(50)
#top_50_ind_author |> view()


## Imports ------
pkg_details |> 
  filter(!is.na(imports)) |> 
  select(package, imports) |> 
  separate_rows(imports, sep = ', ') |> 
  count(package, name = 'total', sort = TRUE) |> 
  head(20)
  



#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#
font_import()
loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto") # text
font_add_google("Press Start 2P", "PressS2P") #title
font_add_google("Orbitron", "Orbitron")       # caption
font_add('Jersey', 'Jersey10-Regular.ttf') # subtitle


font_families()
showtext_auto()


#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#

bg_color    <- "black"
title_color <- "white"
sub_color   <- "#bfbd38"
cap_color   <- "grey80"
tsize       <-  unit(30, units = "cm")



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
  "**Data**: TidyTuesday Week 16<br>", 
  "**Graphics:** ", 
  social_caption)


plot_title    <- "R PACKAGES AUTHORS"
plot_subtitle <- "Top 20 authors as of April 2024"


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
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill =  'black'),
      plot.background = element_rect(fill =  'black'),
      plot.title = element_text(
        color = 'white',
        size = 60,
        family = 'PressS2P',
        margin = margin(0,0,.5,0, unit = "cm")),
      plot.subtitle = element_text(
        color = '#bfbd38',
        size = 55,
        family = 'Jersey',
        margin = margin(0,0,1,0, unit = "cm")),
      plot.caption =  element_textbox(
        #family = caption_font, 
        hjust = 1, vjust = 1,
        color = 'grey70', size = 20,
        lineheight = 0.35,
        margin = margin(0,0,1.5,0, unit = "cm")),
      plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm")
    )
}




#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

## authors  -----
gg <- authors_top_50 |> 
  head(20) |> 
  ggplot(aes(total, author)) +
  geom_col(width = .1, fill = '#24aafe') +
  geom_point(size = 9, color = '#0c527f') +
  geom_text(
    aes(label = total), 
    color = 'white', size = 10, 
    fontface = 'bold') +
  geom_text(
    aes(
      x = 0,
      label = author), 
    color = '#24aafe', size = 12,
    hjust  = 0, vjust = -.5, 
    fontface = 'bold', family = 'Orbitron') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 160)) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  theme_light() +
  costum_theme()
gg




#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#


ggsave(
  filename = here::here("W16_ShinyPackages", "plots_w16", "final_plot_w16.png"),
  #plot    = plot,
  width    = 20, 
  height   = 20, 
  units    = "cm"
)

