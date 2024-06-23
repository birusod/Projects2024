#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork, gt, gtExtras,             
  showtext, extrafont, ggtext, ggtext, glue, ggview
)
source('RDrafts/myfunc.R')
#df |> missing_details() 
#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-06-18")

fh <- tuesdata$federal_holidays
fh |> missing_details() 
#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#
fhd <- fh |> 
  mutate(fixed = case_when(
    str_detect(date_definition, 'fixed') ~ 'Yes',
    TRUE ~ 'No')) |> 
  select(name = official_name,
         date,
         fixed, 
         established = year_established,
         details
         )


#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")
font_add_google("Pacifico", "pacifico")

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


fhd |> 
  rename_with(~c('HOLIDAY', 'DATE', 'FIXED', 'ESTABLISHED', 'DETAILS')) |> 
  gt() |> 
  tab_style(
    style = cell_text(
      color = 'grey', 
      weight = 'normal', 
      style = 'italic',
      size = '.6em'),
    locations = cells_body(
      columns = DETAILS
    )) |> 
  tab_style(
    style = cell_text(
      align = 'center'),
    locations = cells_body(
      columns = c(DATE, FIXED, ESTABLISHED))) |> 
  tab_style(
    style = list(
      cell_fill(color = "crimson"),
      cell_text(weight = "bold", color = 'white')),
    locations = cells_body(columns = c(HOLIDAY))) |> 
  tab_header(
    title = md("**US Federal Holidays**"),
    subtitle = html(
      glue('<span style="color:grey; font-weight:bold;font-family:pacifico">
      Which days of the week do</span>', 
      '<span style="color:crimson; font-weight:bold;font-family:ruluko">
      federal holidays</span>',
      '<span style="color:grey; font-weight:bold;font-family:pacifico">
      fall on this year?</span>'))) |> 
  tab_style(
    locations = cells_column_labels(),
    style = list(
      cell_fill(color = 'firebrick4'),
      cell_text(color = 'white', size = 14, weight = 'bold'),
      cell_borders(
        sides = c('left', 'right', 'top', 'bottom'), 
        color = 'white', 
        weight = px(3)))) |>
  tab_source_note(
    source_note = md(
      html('<p style="color:grey;font-size:.8em">TidyTuesday | week-25</p>'))) 


#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

ggview(units = 'cm', width = 20, height = 18)



#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W25_Federal_holidays",
                       "plots_w25", 
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
