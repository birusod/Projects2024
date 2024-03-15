#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-03-05")
dfr <- tuesdata$trashwheel |> clean_names()

#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#

font_add_google("Roboto", "roboto")
font_add_google("Abril Fatface", "fatface")
font_add_google("Fira sans", "firas")
font_add_google("Alfa Slab One", "alfaslab")          
font_add_google("Saira Extra Condensed", "sairacond") 
font_add_google("Righteous", family = "righteous")  
font_add_google('Pacifico', 'pacifico')
showtext_auto()

#==========================================================================#
# Define texts and annotations ---------------------------------------------
#==========================================================================#
tsize = unit(30, units = "cm")

sysfonts::font_add(
  family = "fab6_reg",
  regular = "Font Awesome 6 Brands-Regular-400.otf") #/Library/Fonts/

github <- "&#xf09b"
github_username <- "birusod"

xtwitter <- "&#xe61b"
xtwitter_username <- "@DSbyKnight"

social_caption <- glue::glue(
  "<span style='font-family:fab6_reg;'>{github};</span> 
  <span style='color: {text_light}'>{github_username}  </span> 
  <span style='font-family:fab6_reg;'>{xtwitter};</span> 
  <span style='color: {text_light}'>{xtwitter_username}</span>")
plot_caption <- paste0(
  "**Data**: TidyTuesday Week 10<br>", 
  "**Graphics:** ", 
  social_caption)


title <- "BALTIMORE HEALTHY HARBOR INITIATIVE"
subtitle <- paste0(
    "Trash Wheel Collection: Total items collected by Trash Wheel in 2023", 
    "<br/>",
    "The most collected trash categories are: ", 
    "<span style='color: #8c564b'>cigarrette butts</span>, ",
    "<span style='color: #17becf'>plastic bottles</span>, and ", 
    "<span style='color: #9467bd'>wrappers</span>"
    )



#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#

bg_col <- "white"
text_light <- "grey70"
text_dark <- ""
highlight_col <- ""
title_col <- '#1E51A5'
sub_color <- "grey70"
tsize = unit(30, units = "cm")

costum_theme <- function(){ 
  theme_minimal() %+replace%    
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(
        color = 'black', hjust = .01, 
        size = tsize),
      plot.title = element_textbox(
        family = 'alfaslab', color = title_col, size = 2.5 * tsize),
      plot.subtitle = element_textbox(
        family = 'fatface', hjust = .5, color = sub_color, size = 2 * tsize,
        margin = margin(t = .5, b = 1, unit = 'cm'), lineheight = .3),
      plot.caption = element_textbox_simple(
        color = 'grey80', lineheight = .5, 
        halign = 1, size = tsize * 1.5,
        margin = margin(t = .5, unit = 'cm')),
      strip.text.x = element_textbox(
        family = 'pacifico', 
        color = 'navy', size = 2 * tsize),
      axis.text.y = element_text(
        family = 'roboto', color = 'black', 
        hjust = 1, size = 1.5 * tsize),
      axis.text.x = element_text(
        family = 'roboto', color = 'grey50', 
        size = 1.5 * tsize),
      plot.margin = margin(1, 1, 1, 1, unit = 'cm'),
      panel.spacing = unit(1, 'cm'))
}

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

## Overview: EDA
dfr |> head(30) |> view()
dfr |> colnames()
dfr |> dim()
dfr |> is.na() |> colMeans() |> round(3)
dfr |> count(id)
dfr |> count(dumpster, id)
dfr |> count(id, dumpster)
dfr |> group_by(id) |> count(dumpster, sort = TRUE)
dfr |> filter(dumpster == 21) |> view()

dfr |> count(dumpster)
dfr |> count(year)

dfr |> 
  select(weight, volume, homes_powered) |> 
  pivot_longer(everything(), 
               names_to = 'mvar', 
               values_to = 'vvals') |> 
  ggplot(aes(vvals))  +
  geom_histogram() +
  facet_wrap(~mvar, scales = 'free')
  

dfr |> filter(is.na(sports_balls)) |> head(20) |> view()

## cleaning: 
# replace NAs with zeros for some columns
dff <- dfr |> 
  replace_na(
    list(plastic_bottles = 0,
         polystyrene     = 0,
         cigarette_butts = 0,
         glass_bottles   = 0,
         plastic_bags    = 0,
         wrappers        = 0,
         sports_balls    = 0)) |> 
  mutate(date = mdy(date))

## Data analysis -----------

### By type: -------
# What type of trash is collected the most? 
bytype <- dff |>
  select(plastic_bottles:sports_balls) |> 
  pivot_longer(everything(), 
               names_to = 'type', 
               values_to = 'count') |> 
  group_by(type) |> 
  summarise(total = sum(count)) |> 
  mutate(type = fct_reorder(
    str_replace_all(type, '_', ' ') |> str_to_sentence(),
    total))
bytype |> arrange(desc(total))

### By trash  wheel: ------
# Do the different Trash Wheels collect different sets of trash? 
by_wheel <- dff |> 
  select(id, plastic_bottles:sports_balls) |> 
  pivot_longer(-id, 
               names_to = 'type', 
               values_to = 'count') |> 
  group_by(id, type) |> 
  summarise(total = sum(count), .groups = 'drop') |> 
  mutate(type = fct_reorder(
    str_replace_all(type, '_', ' ') |> str_to_sentence(),
    total))

### By year:--------
# Are there times of the year when more or less trash is collected?

by_year_1 <- dff |> 
  select(year, id, plastic_bottles:sports_balls) |> 
  pivot_longer(-c(id, year), 
               names_to = 'type', 
               values_to = 'count') |> 
  group_by(id, year, type) |> 
  summarise(total = sum(count), .groups = 'drop') |> 
  mutate(type = fct_reorder(
    str_replace_all(type, '_', ' ') |> str_to_sentence(),
    total))

### By volume / weight  -----

by_volume_wgt <- dff |> 
  select(id, year, volume, weight) |> 
  pivot_longer(-c(id, year), 
               names_to = 'measure', 
               values_to = 'value') |> 
  group_by(id, year, measure) |> 
  summarise(total = sum(value), .groups = 'drop')



### recent data -------
# gwynnda was intalled in june 2021. filtering data for only recent
# collection bewteen 1/1/22 to 12/31/23

rdf <- dff |> 
  filter(date >= ymd('2022-01-01'))

rdf_type <- rdf |> 
  select(plastic_bottles:sports_balls) |> 
  pivot_longer(everything(), 
               names_to = 'type', 
               values_to = 'count') |> 
  group_by(type) |> 
  summarise(total = sum(count)) |> 
  mutate(type = fct_reorder(
    str_replace_all(type, '_', ' ') |> str_to_sentence(),
    total))

### Focus on Mister: 
# the oldest trash wheel

mdf <- dff |> filter(id == 'mister') |> 
  mutate(month  = month(date, abbr = FALSE, label = TRUE))

mdf_type <- mdf |> 
  select(plastic_bottles:sports_balls) |> 
  pivot_longer(everything(), 
               names_to = 'type', 
               values_to = 'count') |> 
  group_by(type) |> 
  summarise(total = sum(count)) |> 
  mutate(type = fct_reorder(
    str_replace_all(type, '_', ' ') |> str_to_sentence(),
    total)) 

mdf_year_month <- mdf |> 
  select(year, month, plastic_bottles:sports_balls) |> 
  pivot_longer(-c(month, year), 
               names_to = 'type', 
               values_to = 'count') |> 
  group_by(year, month, type) |> 
  summarise(total = sum(count), .groups = 'drop') |> 
  mutate(type = fct_reorder(
    str_replace_all(type, '_', ' ') |> str_to_sentence(),
    total))

### Powering Homes:

hm_dff <- dff |> 
  select(id, year, volume, weight, homes_powered) |> 
  group_by(id, year) |> 
  summarise(
    across(
      .cols = c(volume, weight, homes_powered),
      .fns = mean),
    .groups = 'drop')

## Year 2023 -------
df23 <- dff |> 
  filter(year == 2023) |> 
  select(id, year, month, plastic_bottles:sports_balls) |> 
  group_by(id, year, month) |> 
  summarise(
    across(
      .cols = plastic_bottles:sports_balls,
      .fns = mean),
  .groups = 'drop') |> 
  pivot_longer(
    -c(id, year, month), 
    names_to = 'type',
    values_to = 'total') |> 
  mutate(
    across(
      .cols = c(id, type),
      .fns = str_to_upper))

type_ordered <- df23 |> 
  group_by(type) |> 
  summarise(total = sum(total)) |> 
  mutate(type = str_replace_all(type, '_', ' ')) |> 
  arrange(total) |> 
  pull(type)
df23_data <- df23  |> 
  mutate(
    type = str_replace_all(type, '_', ' '),
    type = factor(type, levels = type_ordered),
    id = factor(
      id, 
      levels = c('PROFESSOR', 'CAPTAIN', 'MISTER', 'GWYNNDA'))) 



#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

# By type
bytype |> 
  ggplot(aes(total, type)) +
  geom_col()

rdf_type |> 
  ggplot(aes(total, type)) +
  geom_col()

mdf_type |> 
  ggplot(aes(total, type)) +
  geom_col()



by_wheel |> 
  ggplot(aes(type, total)) +
  geom_col() +
  facet_wrap(~id)

by_wheel |> 
  ggplot(aes(id, total, fill = type)) +
  geom_col()

by_wheel |> 
  ggplot(aes(type, total, fill = id)) +
  geom_col()

by_year_1 |> 
  ggplot(aes(year, total, color = type)) +
  geom_line(linewidth = 2) +
  facet_wrap(~id, scales = 'free_y')

mdf_year_month |> 
  ggplot(aes(month, total, fill = type)) +
  geom_col() +
  facet_wrap(~year, scales = 'free_y')

mdf_year_month |> 
  filter(type == 'Cigarette butts') |> 
  group_by(year) |> 
  summarise(total = sum(total)) |> 
  ggplot(aes(year, total)) +
  geom_col()

by_volume_wgt |> 
  ggplot(aes(year, total)) +
  geom_col() +
  facet_grid(id~measure, scales = 'free_y')


mdf_year_month |> 
  filter(type != 'Cigarette butts') |> 
  group_by(year, type) |> 
  summarise(total = sum(total), .groups = 'drop') |> 
  ggplot(aes(factor(year), total, fill = type)) +
  geom_col(show.legend = FALSE)  +
  scale_fill_few() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = unit_format(
      unit = "K", 
      scale = 1/1000, 
      sep = " ")
    ) +
  facet_wrap(~type, scales = 'free_y') +
  labs(x = NULL, y = NULL, title  = 'TITLE', subtitle  = 'SUBTITLE') +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', hjust = .01),
        plot.title = element_textbox(family = 'chivo', color = 'dodgerblue'))


hm_dff |> 
  ggplot(aes(year, homes_powered, color = id))  +
  geom_line()

dff |> 
  group_by(id) |> 
  summarise(avg = mean(homes_powered)) |> 
  mutate(id = fct_reorder(str_to_upper(id), -avg)) |> 
  ggplot(aes(id, avg)) +
  geom_col()

dff |> 
  group_by(id) |> 
  summarise(avg = mean(homes_powered)) |> 
  mutate(id = fct_reorder(str_to_upper(id), -avg)) |> 
  ggplot(aes(id, avg)) +
  geom_col()

dff |> 
  group_by(id) |> 
  summarise(avg = sum(homes_powered)) 

dff |> filter(homes_powered > 0, year >= 2017) |> 
  group_by(year, id) |> 
  summarise(total = sum(homes_powered)) |> 
  ggplot(aes(year, total, fill = id)) +
  geom_col()

dff |> 
  filter(id == 'mister', year >= 2015, homes_powered > 0) |> 
  group_by(year) |> 
  summarise(total = sum(homes_powered)) |> 
  ggplot(aes(year, total)) + 
  geom_col()

dff |> 
  filter(id == 'mister', year >= 2015, homes_powered > 0) |> 
  group_by(year) |> 
  summarise(total = sum(weight)) |> 
  ggplot(aes(year, total)) + 
  geom_col()

dff |> 
  filter(id == 'mister', year >= 2015, homes_powered > 0) |> 
  group_by(year) |> 
  summarise(total = sum(volume)) |> 
  ggplot(aes(year, total)) + 
  geom_col()



### Final plot  -----

df23_data |> 
  group_by(id, type) |> 
  summarise(total = sum(total), .groups = 'drop') |> 
  ggplot(aes(total, type, fill = type))  +
  geom_col(show.legend = FALSE, alpha = .8, color = NA) +
  facet_wrap(~id, scales = 'free_x') +
  scale_fill_manual(values = c(
    '#1f77b4', '#ff7f0e', '#bcbd22', '#bb3e03', '#9467bd',
    '#17becf' , '#8c564b')) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = unit_format(
      unit = "K", 
      scale = 1/1000, 
      sep = "")) +
  labs(x = NULL, y = NULL, 
       title  = title, subtitle  = subtitle,
       caption = plot_caption) +
  costum_theme()

#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'


ggsave(
  filename = here::here("W10_TrashWheel", "plots_w10", "final_plot_trash.png"),
  width = 40, 
  height = 30, 
  units = "cm",
  bg = bg_col
)
