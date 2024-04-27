#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-02-13")

by_year <- tuesdata$historical_spending |> 
  pivot_longer(-c(1:3), names_to = 'Type', values_to = 'Avg') |> 
  clean_names() |> 
  rename_with(~c('year', 'percent', 'amount', 'type', 'avg')) |> 
  mutate(type = case_when(
    type ==  'EveningOut' ~ 'Evening Out', 
    type ==  'GiftCards' ~ 'Gift Cards', 
    type ==  'GreetingCards' ~ 'Greeting Cards', 
    TRUE ~ type
  ))

by_gender <- tuesdata$gifts_gender |> 
  pivot_longer(-c(1:2), names_to = 'Type', values_to = 'Avg') |> 
  clean_names() |> 
  rename_with(~c('gender', 'percent', 'type', 'avg')) |> 
  mutate(type = case_when(
    type ==  'EveningOut' ~ 'Evening Out', 
    type ==  'GiftCards' ~ 'Gift Cards', 
    type ==  'GreetingCards' ~ 'Greeting Cards', 
    TRUE ~ type
  ))

by_age <- tuesdata$gifts_age |> 
  pivot_longer(-c(1:2), names_to = 'Type', values_to = 'Avg') |> 
  clean_names() |> 
  rename_with(~c('agegrp', 'percent', 'type', 'avg')) |> 
  mutate(type = case_when(
    type ==  'EveningOut' ~ 'Evening Out', 
    type ==  'GiftCards' ~ 'Gift Cards', 
    type ==  'GreetingCards' ~ 'Greeting Cards', 
    TRUE ~ type
  ))

#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#

font_add_google("Roboto", "roboto")
font_add_google("Fira code", "firac")
font_add_google("Fira sans", "firas")
font_add_google("Alfa Slab One", "alfaslab")          
font_add_google("Saira Extra Condensed", "sairacond") 
font_add_google("Righteous", family = "righteous")  
showtext_auto()

title_font <- "alfaslab"
sub_font <- "sairacond"
text_font <- 'roboto'
cap_font <- "righteous"

#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#

bg_col <- "white"
text_light <- "grey70"
text_dark <- "grey50"
highlight_col <- "#69BABB"
title_col <- "#945B88"
sub_color <- "#C8839F"
tsize = unit(30, units = "cm")


costum_theme <- function(...){ 
  cfont <- "roboto"   
  theme_minimal(base_family = text_font,
                base_size = 70) %+replace%    
    theme(
  plot.title = element_textbox_simple(
    family = title_font, color = title_col,
    hjust = 0, size = 2 * tsize,
    margin = margin(b  = .2, unit = 'cm')
  ),
  plot.subtitle = element_textbox(
    family = sub_font, color = sub_color,
    hjust = 0, size = 3 * tsize,
    lineheight = .3,
    margin = margin(b  = .5, unit = 'cm')
  ),
  plot.caption = element_textbox(
    family = cap_font, color = text_light,
    margin = margin(t  = .5, unit = 'cm'),
    lineheight = .1, hjust = 1, size = 40
    )
  )
}

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#



#==========================================================================#
# Define texts and annotations ---------------------------------------------
#==========================================================================#

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
  "**Data**: TidyTuesday Week 7<br>", 
  "**Graphics:** ", 
  social_caption)


ptitle <- "VALENTINE DAY SPENDING 2010-2022"
psubtitle_year <- "Individual Average Spending"
psubtitle_year_type <- paste0(
  "<b>Individual Average Spending By Type:</b>", "<br>",
  "<span style='color: darkgreen'> => Jewelery, Evening Out, Clothings are the top 3</span><br>",
  "<span style='color: firebrick'> => Greeting Cards, Candy, Flowers the bottom 3</span></b>")



#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

# Yearly Average Spending ----------
by_year |> 
  group_by(year) |> 
  summarise(avg = mean(avg)) |> 
  ggplot(aes(factor(year), avg, group = 1)) +
  geom_line(linewidth = 3, color = 'grey') +
  geom_point(size = 2) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 25),
    labels = dollar) +
  labs(title = ptitle,
       subtitle = psubtitle_year,
       caption = plot_caption,
       y = NULL, x = NULL) +
  theme_light() +
  costum_theme() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = .5),
    axis.line = element_line(color = 'black', linewidth = 1)
    )

# Yearly Average Spending By Type ----------
# Legend Not Ordered !!!
by_year |> 
  group_by(year, type) |> 
  summarise(avg = mean(avg), .groups = 'drop') |> 
  ggplot(aes(factor(year), avg, color = type, group = type)) +
  geom_line(linewidth = 3)

# ordered
by_year |> 
  group_by(year, type) |> 
  summarise(avg = mean(avg), .groups = 'drop') |> 
  mutate(type = fct_reorder(type, avg, tail, n = 1, .desc = TRUE)) |> 
  ggplot(aes(factor(year), avg, color = type, group = type)) +
  geom_line(linewidth = 3)

by_year |> 
  group_by(year, type) |> 
  summarise(avg = mean(avg), .groups = 'drop') |> 
  ggplot(aes(factor(year), avg, group = type,
             color = fct_reorder2(type, year, avg))) +
  geom_line(linewidth = 3, color =  'grey80') +
  geom_point(size = 4) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), labels = dollar) +
  guides(
    color = guide_legend(
      title = 'TYPE',
      label.position  = 'right',
      keyheight = 2,
      label.hjust = 0))  +
  labs(title = ptitle,
       subtitle = psubtitle_year_type,
       caption = plot_caption,
       y = NULL, x = NULL, color = 'TYPE') +
  theme_light() +
  costum_theme() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = .5),
    axis.line = element_line(color = 'black', linewidth = 1)
  )

# Average Spending By Age group and Type ----------
by_age |> 
  group_by(agegrp, type) |> 
  summarise(
    percent  = mean(percent),
    avg = mean(avg)) |> 
  pivot_longer(-c(1:2), names_to = 'cat', values_to = 'vals') |> 
  ggplot(aes(agegrp, vals, fill = type)) +
  geom_col() +
  facet_grid(type~cat, scales = 'free_y')

# Yearly Average Celebration ----------
by_year |> 
  group_by(year) |> 
  summarise(percent = mean(percent) / 100) |> 
  ggplot(aes(factor(year), percent, group = 1)) +
  geom_col() +
  geom_label(
    aes(label = paste0(percent * 100, '%')),
    vjust = ) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, .65),
                     labels = percent)  +
  theme(
    axis.ticks = element_blank()
  )




#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path(
    "W7_Valentine_2024",
    "plots_w7", 
    paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W7_Valentine_2024", "plots_w7", "plot_type.png"),
  #plot = plot,
  width = 40, 
  height = 30, 
  units = "cm",
  bg = bg_col
)
