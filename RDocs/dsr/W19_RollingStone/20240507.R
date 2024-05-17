#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork, hrbrthemes,              
  showtext, extrafont, ggtext, ggtext, glue,
  ggalt
)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-05-07")


#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#
#tuesdata$rolling_stone |> head(20) |> view()
tuesdata$rolling_stone |> count(type)
df <- tuesdata$rolling_stone |> 
  drop_na() |> 
  select(
    genre, type, album, differential, 
    name = clean_name, 
    starts_with('rank'), 
    release =  release_year, 
    spotify = spotify_popularity)
tuesdata$rolling_stone |> select(1:3, differential) |> arrange(differential)

## Average popularity  on Spotify ----
ave_pop <- df |> 
  group_by(name) |> 
  summarise(spotify = mean(spotify)) |> 
  arrange(desc(spotify)) |> 
  slice_head(n = 20)



## differential -------
df_dif <- df |> 
  select(name, album, release, differential) |> 
  mutate(pos = differential > 0, abs = abs(differential)) |> 
  group_by(pos) |> 
  arrange(desc(abs)) |> 
  slice_head(n = 10) |> view()



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
cap_color   <- "grey90"
tsize       <-  unit(30, units = "cm")



#==========================================================================#
# Define texts and annotations ---------------------------------------------
#==========================================================================#


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
  "**Data**: TidyTuesday Week 19<br>", 
  "**Graphics:** ", 
  social_caption)


ptitle    <- "ROLLING STONE ALBUM RANKINGS"
subtitle_pop <- "Average Score By Popularity on Spotify"
subtitle_dif <- paste0(
  "Top 10 Albums: Raking ",
  "<span style='color: darkgreen'>Gain</span>",
  " & ",
  "<span style='color: darkred'>Loss</span>",
  " From ",
  "<span style='color: gold'>2003</span>",
  " To ",
  "<span style='color: gold'>2020</span>")


#==========================================================================#
# Custom Theme------------------------------------------------
#==========================================================================#

costum_theme_pop <- function(){ 
  cfont <- "roboto"   
  theme_minimal(base_family = cfont) %+replace%    
    theme(
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text.y = element_text(
        color = 'cyan3', hjust = 1, size = 2 *  tsize),
      axis.text.x = element_text(color = 'white', size = 2 * tsize),
      axis.line.y = element_line(color = 'white'),
      panel.background = element_rect(fill  = 'black'),
      plot.background = element_rect(fill  = 'black'),
      plot.title = element_text(
        color = 'white', size = 3 * tsize,
        face = 'bold',
        hjust = .5,
      ),
      plot.subtitle = element_text(
        color = 'grey70', size = 2 * tsize,
        face = 'bold', 
        hjust = .5,
        margin = margin(.2, 0, 1, 0, unit = 'cm')),
      plot.caption = element_textbox(
        colour = 'grey80',
        size = tsize,
        lineheight = .5,
        hjust = 1)
    )
}

costum_theme_dif <- function(){ 
  cfont <- "roboto"   
  theme_minimal() %+replace%    
    theme(
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(
        linetype = 'dashed',
        linewidth = .2,
        color = 'grey70'),
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(
        color = 'white', face = 'bold', size = 2 *  tsize),
      panel.background = element_rect(fill  = 'black'),
      plot.background = element_rect(fill  = 'black'),
      plot.title = element_text(
        color = 'white', size = 3 *  tsize,
        face = 'bold',
        hjust = .5,
        margin = margin(.2, 0, 0, 0, unit = 'cm')),
      plot.subtitle = element_textbox(
        color = 'white', hjust = .5, size = 2 * tsize, face = 'bold',
        margin = margin(.25, 0, 1, 0, unit = 'cm')),
      plot.caption = element_textbox(
        colour = 'grey80',
        size = tsize,
        lineheight = .5,
        hjust = 1)
    )
}




#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#


## Popularity -------
ave_pop |> 
  mutate(name = fct_reorder(name, spotify)) |> 
  ggplot(aes(spotify, name)) +
  geom_lollipop(
    point.colour = 'cyan3',
    point.size = 5,
    color = 'grey90',
    horizontal = TRUE
  ) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 95)) +
  labs(
    y = '', x = 'Average popularity on Spotify',
    title = ptitle,
    subtitle = subtitle_pop,
    caption = plot_caption
  ) +
  costum_theme_pop()



## Differential ------

df_dif |>
  mutate(name = fct_reorder(name, differential)) |> 
  ggplot() +
  geom_col(
    aes(differential, name, fill = pos),
    show.legend = FALSE,
    width = .7) +
  geom_text(
    aes(differential, name, label = album),
    hjust = ifelse(df_dif$differential < 0, 1.05, -.05),
    x = 0,
    size = .6 * tsize,
    vjust = 0) +
  geom_text(
    aes(differential, name, label = paste0(name, ' , ', release)),
    hjust = ifelse(df_dif$differential < 0, 1.05, -.05),
    x = 0, color = 'white',
    size = .4 * tsize,
    vjust = 1) +
  geom_vline(xintercept = 0, color = 'white', linewidth = .3) +
  scale_x_continuous(
    expand = c(0, 0), 
    limits = c(min(df_dif$differential) - 50,
                 max(df_dif$differential) + 50),
    breaks = seq(-400, 400, by = 200),
    labels = c('-400', '-200', '0', '+200', '+400')
    ) +
  labs(
    x = '', y = '',
    title = ptitle,
    subtitle = subtitle_dif,
    caption = plot_caption
  ) +
  costum_theme_dif()


#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# final_plot_pop.png
# final_plot_dif.png
ggsave(
  filename = here::here("W19_RollingStone",
                        "plots_w19", 
                        paste0("final_plot_dif", ".png")),
  #plot    = plot,
  width    = 40, 
  height   = 30, 
  units    = "cm"
)

