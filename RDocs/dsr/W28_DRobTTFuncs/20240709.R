#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork,               
  showtext, extrafont, ggtext, ggtext, glue, ggview,
  packcircles
)
source('RDrafts/myfunc.R')
#df |> missing_details() 
#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-07-09")
dfr <- tuesdata$drob_funs
dfr |> missing_details()
#dfr |> view()
#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

dfr |> count(funs, sort = TRUE)
dfr |> count(funs, pkgs, sort = TRUE)


dfr |> 
  filter(pkgs != '(unknown)') |> 
  mutate(pkgs = fct_lump_n(pkgs, n = 50)) |> 
  count(pkgs, sort = TRUE) 

# top 10 most loaded packages:

pkgs10 <- dfr |> 
  filter(pkgs != '(unknown)', !in_multiple_pkgs) |> 
  count(pkgs, sort = TRUE) |> 
  head(10) |> pull(pkgs)

# funs
# top 100 most used functions from the 10 packages:

funs100 <- dfr |> 
  filter(!in_multiple_pkgs, pkgs %in% pkgs10) |> 
  count(funs, pkgs, sort = TRUE) |> 
  rename_with(~c('group', 'category', 'value')) |> 
  head(100)


# custom function to prep data and plot circles:
make_circle_plot <- function(dat, type = 'area', n = 50, title = 'mytitle'){
  rows    <- nrow(dat)
  val     <- dat |> pull(value)
  cat     <- dat |> pull(category)
  cols    <- n_distinct(cat)
  packing <- circleProgressiveLayout(val, sizetype = type)
  dgg     <- circleLayoutVertices(packing, npoints = n) |> 
    mutate(category = rep(cat, each = n + 1))
  
  dff <- cbind(dat, packing)
  
  dgg |> 
    ggplot() + 
    geom_polygon(                     
      aes(x, y,  group = id, fill = as.factor(category)), 
      colour = "black", 
      alpha = 0.6) +
    scale_fill_manual(values = tableau_color_pal('Tableau 10')(cols)) +
    geom_text(                        
      data = dff, 
      aes(x, y, 
          size = value, 
          label = group),
      color = 'white', fontface = 'bold') +
    scale_size_continuous(range = c(1,4)) +
    guides(size = 'none',
           fill = guide_legend(
             title = '',
             theme = theme(
               legend.key = element_rect(color = 'white'),
               legend.key.spacing.y = unit(4, "pt")
               )
           )) +
    
    ggtitle(title) +
    theme_void() + 
    coord_equal() 
  
}

#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")
font_add_google("Nova Mono", "nova") 
font_add_google("Saira Extra Condensed", "saira") 
font_add_google("Wellfleet", "wellfleet") 

showtext_auto()
#font_families()



#==========================================================================#
# Defining colors ----------------------------------------------------------
#==========================================================================#

bg_color    <- "white"
text_light  <- ""
text_dark   <- ""
highlight   <- ""
title_color <- ""
sub_color   <- ""
cap_color   <- "grey30"




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
  "**Data**: TidyTuesday Week 28 | ", 
  "**source**: @drob screencast | ",
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
    theme()
}




#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

funs100 |> ggplot(aes(category)) + geom_bar()

funs100 |> 
  make_circle_plot(
    n = 50, 
    title = 'DAVID ROBINSON SCREENCAST:\nTOP 10 PACKAGES & MOST USED FUNCTIONS') +
  labs(caption = plot_caption) +
  theme(
    panel.background = element_rect(fill = '#303236'),
    plot.margin = margin(1, 1, 1, 1, unit = 'cm'),
    text = element_text(family = 'wellfleet'),
    plot.title = element_text(family = 'nova', size = 1 * tsize, 
                              face = 'bold', color = 'black',
                              hjust = .5,
                              lineheight = .3),
    plot.caption = element_textbox_simple(
      family = 'saira',
      halign = 0.5, 
      size = .5 * tsize,
      margin = margin(t = 5, unit = 'pt')),
    legend.key.size = unit(.4, 'cm'),
    legend.text = element_text(size = .6 * tsize)
    
  )

ggview(units = 'cm', width = 14, height = 12)



#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W28_DRobTTFuncs",
                       "plots_w28", 
                       paste0("w28_finalPlot.jpeg"))
  )

ggsave(
  filename = here::here(),
  #plot    = plot,
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_col
)
