#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue, 
  echarts4r, data.tree)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-02-27")

events <- tuesdata$events
births <- tuesdata$births
deaths <- tuesdata$deaths

raw <- read_csv('RDrafts/leap_day_births.csv')
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


#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#

bg_col <- ""
text_light <- ""
text_dark <- ""
highlight_col <- ""
title_col <- ""
sub_color <- ""
tsize = unit(30, units = "cm")

costum_theme <- function(){ 
  cfont <- "roboto"   
  theme_minimal(
    #base_family = cfont,
    #base_size = 40
    ) %+replace%    
    theme()
}

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#
df <- raw |> 
  select(birth = year_birth, death = year_death, category, status, country) |> 
  mutate(
    year = case_when(status == 'Dead' ~ death, TRUE ~ 2024),
    age = year - birth) |> 
  select(-year)

## by_country -----
by_country <- df |> 
  mutate(country = fct_lump_n(country, n = 2)) |> 
  count(country, sort = TRUE, name = 'total')

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
  <span style='color: {text_col}'>{github_username}  </span> 
  <span style='font-family:fab6_reg;'>{xtwitter};</span> 
  <span style='color: {text_col}'>{xtwitter_username}</span>")
plot_caption <- paste0(
  "**Data**: TidyTuesday Week 6<br>", 
  "**Graphics:** ", 
  social_caption)


ptitle <- "Leap Day"
psub_bar <- "Distribution by country of famous people born on Feb 29th"
psub_dnt <- "Distribution by status of famous people born on Feb 29th"
psub_sun <- "Distribution by status and work sector of famous people born on Feb 29th"


#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

# Bar: by country -----
by_country |> 
  e_chart(country) |> 
  e_bar(total, legend = FALSE) |> 
  e_title(ptitle, psub_bar) |> 
  e_theme("dark") |> 
  e_tooltip()

# Donut: by status --------
df |> 
  count(status, name = 'total') |> 
  e_chart(status) |> 
  e_pie(total, 
        radius = c("40%", "70%"),
        legend = FALSE) |> 
  e_title(ptitle, psub_dnt)


# Sunburst: by country  / category -----
# data.tree
sb_data <- df |> 
  count(category, country) |> 
  rename_with(~ c('parents', 'labels', 'value'))

sb_data$pathString <- paste("world", 
                            sb_data$parents, 
                            sb_data$labels, 
                            sb_data$value,
                            sep = "/")
sbd <- as.Node(sb_data)
print(sbd, "value", limit = 5)

sbd |>
  e_charts() |> 
  e_sunburst()

df |> count(category, sort = TRUE)
ddf1 <- df |> count(status, sort = TRUE)
ddf_alive <- df |> 
  mutate(category = fct_lump(category, n = 4)) |> 
  count(status, category) |> 
  filter(status == 'Alive')
ddf_dead <- df |> 
  mutate(category = fct_lump(category, n = 4)) |> 
  count(status, category) |> 
  filter(status == 'Dead')

sun <- tibble(
  name = ddf1 |> pull(status),
  value = ddf1 |> pull(n),
  children = list(
    tibble(
      name = ddf_alive |> pull(category),
      value = ddf_alive |> pull(n)
    ),
    tibble(
      name = ddf_dead |> pull(category),
      value = ddf_dead |> pull(n)
    )
  )
) |>
  e_charts() |>
  e_sunburst() |> 
  e_title(ptitle, psub_sun) |> 
  e_theme("westoros")

### penguins data ----
pp <- palmerpenguins::penguins |> drop_na()
pp1 <- pp |> count(sex)
pp2 <- pp |> count(sex, island)

tibble(
  name  =  pp1 |> pull(sex),
  value = pp1 |> pull(n),
  #itemStyle = tibble(color = c("red", "blue")),
  children = list(
    tibble(
      name = pp2 |> filter(sex == 'female') |> pull(island),
      value  = pp2 |> filter(sex == 'female') |> pull(n),
      #itemStyle = tibble(color = c("grey", "brown", "cyan"))
    ),
    tibble(
      name = pp2 |> filter(sex == 'male') |> pull(island),
      value  = pp2 |> filter(sex == 'male') |> pull(n),
      #itemStyle = tibble(color = c("grey", "brown", "cyan"))
    )
  )
) |>
  e_charts() |>
  e_sunburst() |> 
  e_theme("dark-fresh-cut")



bar <- df |> 
  count(category) |> arrange(n) |> 
  e_charts(category) |> 
  e_bar(n, legend = FALSE) |> 
  e_flip_coords() |> 
  e_theme("weforum") |> #fresh-cut, sakura, weforum, wonderland, eduardo, walden
  e_title(ptitle, "Distribution by work sector of famous people born on Feb 29th")

# e_x_axis(
#   inverse = TRUE,
#   axisLabel = list(inside = TRUE),
#   axisTick = list(show = FALSE),
#   axisLine = list(show = FALSE)) |> 
#   e_legend(show = FALSE) 


df |> 
  count(category) |> arrange(n) |> 
  e_charts(category) |> 
  e_polar() |> 
  e_angle_axis() |> 
  e_radius_axis(category) |> 
  e_bar(n, coord_system = "polar") |> 
  e_scatter(n, coord_system = "polar") |> 
  e_legend(show = FALSE)

#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#




# save the final plot as 'final_plot'
ggsave(
  filename = file.path(
    "W9_LeapDay",
    "plots_w9", 
    paste0("final_plot", ".jpeg"))
  )

ggsave(
  filename = here::here(),
  plot = plot,
  width = 40, 
  height = 30, 
  units = "cm",
  bg = bg_col
)
