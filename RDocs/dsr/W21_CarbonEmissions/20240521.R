#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork,               
  showtext, extrafont, ggtext, ggtext, glue  
)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-05-21")

dfr <- tuesdata$emissions |> 
  select(year, entity = parent_entity, type = parent_type,
         commodity, quantity  = production_value,
         unit = production_unit, emission = total_emissions_MtCO2e)

dfr |> glimpse()

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

## Total emission by year ----
# million tonnes of carbon dioxide equivalent (MtCO2e))
yearly_emission <- dfr |> 
  group_by(year) |> 
  summarise(total = sum(emission))

## total emission by commodity -----
commodity_emission <- dfr |> 
  group_by(commodity) |> 
  summarise(emission = sum(emission)) |> 
  arrange(desc(emission))


## total emission by nation states / state owned / Companies-----

by_state <- dfr |> 
  filter(type == 'Nation State') |> 
  mutate(entity = case_when(
    str_detect(entity, 'China') ~ 'China',
    TRUE ~ entity)) |> 
  group_by(entity) |> 
  summarise(emission = sum(emission)) |> 
  arrange(desc(emission))

by_entity <- dfr |> 
  filter(type == 'State-owned Entity') |> 
  group_by(entity) |> 
  summarise(emission = sum(emission)) |> 
  arrange(desc(emission)) |> 
  slice_head(n = 10)

by_company <- dfr |> 
  filter(type == 'Investor-owned Company') |> 
  group_by(entity) |> 
  summarise(emission = sum(emission)) |> 
  arrange(desc(emission)) |> 
  slice_head(n = 10)


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
  "**Data**: TidyTuesday Week 21<br>", 
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




## Total emission by year ----
# million tonnes of carbon dioxide equivalent (MtCO2e))
yearly_emission |> 
  ggplot(aes(year, total)) +
  geom_line() 

yearly_emission |> 
  ggplot(aes(year, total))  +
  geom_area(fill =  'darkorange', alpha = .5) +
  geom_line(color = 'brown', linewidth = 1) +
  theme_light() +
  theme(
    panel.grid = element_blank()
  )

## total emission commodity -----
commodity_emission |> 
  ggplot(aes(emission, fct_reorder(commodity, emission))) +
  geom_col() +
  geom_text(
    aes(label = round(emission, 0)),
    hjust = -.2) +
  scale_x_continuous(
    limits = c(0, 650000),
    expand = c(0, 0),
    breaks = c(100000, 300000, 500000),
    labels = c('100K', '300K', '500K')) +
  labs(x = NULL, y = NULL,
       title = 'Total Emission By Commodities') +
  theme_light() +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(family = 'roboto',
                              size = 20,
                              face = 'bold')
  )


## quick custom function ------
geom_bar_labelled <- function(
    data, x, y, 
    sub = NULL, breaks = NULL, labels = NULL, 
    lim = NULL, ysize = NULL, fillcol = 'grey', ...) {
  data |> 
    mutate(grp := fct_reorder({{x}}, {{y}})) |> 
    ggplot(aes({{y}}, grp)) +
    geom_col(fill = fillcol, alpha = .7) +
    geom_text(aes(label = round({{y}}, 0)),
              hjust = -.1,
              fontface = 'bold',
              size = 5) +
    scale_y_discrete(labels = label_wrap(ysize)) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = breaks,
                       labels = labels,
                       limits = lim) +
    labs(x = NULL, y = NULL,
         title = 'CARBON MAJORS EMISSIONS',
         subtitle = sub) +
    #theme_light() +
    theme_minimal() +
    theme(
      axis.ticks.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(face = 'bold', 
                               size = 12,
                               family = 'roboto',
                               color = 'grey40'),
      plot.title = element_text(family = 'roboto',
                                size = 20,
                                face = 'bold'),
      plot.subtitle = element_textbox(family = 'roboto',
                                      size = 16,
                                      face = 'bold.italic',
                                      color = 'grey80'),
      margin(.5, 1, 1, 0, unit = 'cm')
    )
}

cm_fillcol <-  "#ff7f0e"
commodity_emission |> 
  geom_bar_labelled(
    commodity, emission,
    sub = glue::glue(
      "Total emission (MtCO2e) by ",
      "<span style='color: {cm_fillcol}'>commodities</span>"),
    breaks = seq(1, 5, 2) * 1e5,
    labels = c('100K', '300K', '500K'),
    lim = c(0, 650000),
    ysize = 12,
    fillcol = glue::glue(cm_fillcol))


## total emission by nation states / state owned / Companies-----

st_fillcol <-  "#17becf"
by_state |> 
  geom_bar_labelled(entity, emission,
                    sub = glue::glue(
                      "Total emission (MtCO2e) by ",
                      "<span style='color: {st_fillcol}'>country</span>"),
                    breaks = seq(.5, 2.5, 1) * 1e5,
                    labels = c('50K', '150K', '250K'),
                    lim = c(0, 370000),
                    ysize = 12,
                    fillcol = glue::glue(st_fillcol))


et_fillcol <-  "#17becf"
by_entity  |> 
  geom_bar_labelled(entity, emission,
                    sub = glue::glue(
                      "Total emission (MtCO2e) by ",
                      "<span style='color: {et_fillcol}'>entity</span>"),
                    breaks = seq(2, 6, 2) * 1e4,
                    labels = c('20K', '40K', '60K'),
                    lim = c(0, 85000),
                    fillcol = glue::glue(et_fillcol)
                    )
cy_fillcol <-  "dodgerblue"
by_company |> 
  geom_bar_labelled(entity, emission,
                    sub = glue::glue(
                      "Total emission (MtCO2e) by ",
                      "<span style='color: {cy_fillcol}'>company</span>"),
                    breaks = seq(2, 6, 2) * 1e4,
                    labels = c('20K', '40K', '60K'),
                    lim = c(0, 70000),
                    fillcol = glue::glue(cy_fillcol))



#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W21_CarbonEmissions",
                       "plots_w21", 
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
