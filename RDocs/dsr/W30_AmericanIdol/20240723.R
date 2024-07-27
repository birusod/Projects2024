#============================================================#
# Load packages ---------------------------------------------
#============================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes,              
  showtext, extrafont, ggtext, ggtext, glue,
  patchwork, ggview
)
source('RDrafts/myfunc.R')
#df |> missing_details() 
#============================================================#
# Loading data -----------------------------------------------
#============================================================#

tuesdata <- tidytuesdayR::tt_load("2024-07-23")


#============================================================#
# Wrangling Data ---------------------------------------------
#============================================================#

season_year <- tuesdata$auditions |>
  select(season, audition_date_end) |> 
  mutate(end_year = year(ymd(audition_date_end))) |> 
  select(season, end_year)



viewers <- tuesdata$ratings |> 
  select(season, show_number, viewers_in_millions)



fdf <- tuesdata$finalists |> 
  clean_names() |> 
  mutate(dob = dmy(birthday),
         birth_year = year(dob)) |>
  left_join(season_year, 
            by = join_by(season), 
            relationship = "many-to-many") |> #season 18 missing in finalists
  left_join(viewers,
            by = join_by(season), 
            relationship = "many-to-many") |> 
  mutate(contestant_age = end_year - birth_year,
         season = factor(
           paste0('Season_', season),
           levels = c(
             "Season_1", "Season_2", "Season_3",  "Season_4", 
             "Season_5", "Season_6",  "Season_7", "Season_8",  
             "Season_9",  "Season_10", "Season_11", "Season_12", 
             "Season_13", "Season_14", "Season_15", "Season_16", 
             "Season_17")),
         contestant = case_when(
           contestant == 'Jos\x8e \"Sway\" Penala' ~ 'Jose Sway Penala',
           TRUE ~ contestant
         ))
  



## Age distribution ----
fdf |> 
  ggplot(aes(contestant_age)) +
  geom_histogram(bins = 15, color = 'dodgerblue', fill = 'wheat') +
  theme_light() +
  labs(title = 'Contestant Age Distribution', y = NULL, x = NULL)


fdf |> 
  summarise(avg = mean(contestant_age, na.rm = TRUE), .by = season) |> 
  ggplot(aes(season, avg)) +
  geom_col() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 25)) +
  labs(title = 'Contestant Age: Season Average', y = NULL, x = NULL) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 


fdf |> 
  ggplot(aes(season, contestant_age)) +
  geom_boxplot() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(14, 30)) +
  labs(title = 'Contestant Age Distribution', y = NULL, x = NULL) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 


fdf |> 
  summarise(
    avg = mean(viewers_in_millions, na.rm = TRUE),  
    .by = c(season, end_year)) |> 
  #summarise(mean = mean(avg, na.rm = TRUE)): season_14 value= overall mean
  mutate(avg = replace_na(avg, mean(avg, na.rm = TRUE))) |> 
  unite(seasson_year, c(season, end_year), sep = '_') |> 
  mutate(order = row_number(),
         seasson_year = fct_reorder(seasson_year, order)) |> 
  ggplot(aes(seasson_year, avg)) +
  geom_col() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 30)) +
  labs(title = 'Viewers in Millions: Average by season', 
       y = NULL, x = NULL) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.margin = margin(l = 1, r = 1, unit = 'cm'),
        panel.grid.major.x = element_blank()) 


## Contestant by state ----
fdf |> filter(is.na(birthplace)) |>  count(contestant) 
fdf |> filter(str_detect(birthplace, 'Karen')) |> select(birthplace)
# five contestants are missing birthplace
by_city_state <- fdf |> 
  mutate(birthplace = case_when(
    contestant == 'Amber Holcomb' ~ 'Shepherd, Texas',
    contestant == 'Burnell Taylor' ~ 'New Orleans, Louisiana',
    contestant == 'Curtis Finch, Jr.' ~ 'St. Louis, Missouri',
    contestant == 'Devin Velez' ~ 'Chicago, Illinois',
    contestant == 'Jennifer Hudson' ~ 'Chicago, Illinois',
    contestant == 'Stevie Scott' ~ 'Fair Oaks, California',
    contestant == 'Jose Sway Penala' ~ 'South San Francisco, California',
    TRUE ~ birthplace)) |> 
  distinct(contestant, birthplace) |> 
  separate(birthplace, c('city', 'state'), sep = ', ') |> 
  mutate(state = fct_infreq(state) |> fct_rev()) 

fdf |> filter(str_detect(birthplace, 'Canada'))
by_city_state |> 
  filter(!state %in% c('Canada', 'Alberta', 'Ireland', 
                       'Philippines', 'Western Australia')) |> 
  count(state, name = 'total') |> 
  ggplot(aes(total, state)) +
  geom_col() +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 25)) +
  labs(title = 'Number of contestant by US state', 
       y = NULL, x = NULL) +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) 


# Nicola Rennie github
# https://github.com/nrennie/tidytuesday/blob/main/2024/2024-07-23/20240723.R
#devtools::install_github("nrennie/ggtextcircle")
pacman::p_load(showtext, ggtextcircle, ggtext, ggimage)
font_add_google("Ubuntu")
font_add_google("Passion One")
showtext_auto()

body_font <- "Ubuntu"
title_font <- "Passion One"
bg_col <- "#00186d"
text_col <- "#fafafa"
highlight_col <- "red"

plot_data <- tuesdata$eliminations |>
  #select(place) |> 
  mutate(
    place2 = case_when(
      str_detect(place, "–") ~ str_extract(place, "(?<=–).*"),
      str_detect(place, "-") ~ str_extract(place, "(?<=-).*"),
      TRUE ~ place
    ),
    place2 = as.numeric(place2)
  ) |> 
  filter(place2 <= 10, season <= 10) |> 
  select(contestant, place2) |>
  mutate(
    place2 = if_else(place2 == "1", 'red', 'dodgerblue')
  )

st <- glue::glue(
  "All American Idol <b><span style='color: {highlight_col};'>winners</span></b>
  and contestants who made the top 10 in each of the first 10 seasons."
)
cap <- paste0(
  "**Data**: TidyTuesday<br>**Code source:** @nrennie"
)

ggplot() +
  geom_textcircle(
    data = plot_data, #|> #head(45),
    mapping = aes(label = contestant, colour = place2),
    family = body_font,
    size = 6,
    r = 4) +
  geom_image(
    data = slice_head(plot_data, n = 1),
    aes(
      x = 0,
      y = 0.5,
      image = "W30_AmericanIdol/logo.png"),
    size = 0.3) +
  geom_textbox(
    data = data.frame(x = 0, y = -1, label = st),
    mapping = aes(x = x, y = y, label = label),
    hjust = 0.5,
    halign = 0.5,
    colour = text_col,
    family = body_font,
    lineheight = 0.5,
    fill = "transparent",
    box.colour = "transparent",
    size = 7,
    minwidth = 0.45
  ) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(-5, 5)) +
  scale_y_continuous(limits = c(-5, 5)) +
  labs(caption = cap) +
  coord_fixed() +
  theme_void(base_size = 20) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(t = 1, b = 2, l = 5),
      lineheight = 0.5,
      family = body_font
    )
  )

#============================================================#
# Loading fonts ---------------------------------------------
#============================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")

font_families()
showtext_auto()


#============================================================#
# Defining colors -------------------------------------------
#============================================================#

bg_color    <- "white"
text_light  <- ""
text_dark   <- ""
highlight   <- ""
title_color <- ""
sub_color   <- ""
cap_color   <- ""




#============================================================#
# Define texts and annotations -------------------------------
#============================================================#
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




#=============================================================#
# Data Viz ----------------------------------------------------
#=============================================================#

ggview(units = 'cm', width = 15, height = 15)



#==============================================================#
# Saving Plots and Gifs ---------------------------------------
#==============================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W30_AmericanIdol",
                       "plots_w30", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W30_AmericanIdol",
                        "plots_w30", 
                        paste0("final_plot2", ".png")),
  width = 7,
  height = 7,
  units = "in",
  dpi = 300,
  device = "png"
  
)
