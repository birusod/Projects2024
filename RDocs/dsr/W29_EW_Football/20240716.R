#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, hrbrthemes,             
  showtext, extrafont, ggtext, ggtext, glue,
  patchwork, ggview,
  ggstream, waffle, ggwaffle, emojifont
)
#devtools::install_github("GuangchuangYu/emojifont")
source('RDrafts/myfunc.R')
#df |> missing_details() 
#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-07-16")

ewf_appearances <- tuesdata$ewf_appearances
ewf_matches <- tuesdata$ewf_matches
ewf_standings <- tuesdata$ewf_standings

dfr <- ewf_matches |>
  mutate(year = year(date))
#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

## year / division -----

dfr |> 
  count(year) |> 
  ggplot(aes(year, n)) +
  geom_col()

dyd <- dfr |> 
  count(year, division, name = 'total') |> 
  mutate(len = nchar(division),
         division_ordered = fct_reorder(division, len))


## season / division -----
dsd <- dfr |> 
  count(season, division, name = 'total') |> 
  mutate(division_ordered = fct_inorder(division)) 
  #separate(season, c('season_start', 'season_end'))




dfr |> colnames()


#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")
font_add_google("Racing Sans One", "racing") 
font_add_google("Saira Extra Condensed", "saira") 
font_add_google("Jockey One", "jockey") 
font_add_google("Pompiere", "pompiere")

font_families()
showtext_auto()


#==========================================================================#
# Defining colors ---------------------------------------------------------
#==========================================================================#

bg_color    <- "white"
text_light  <- ""
text_dark   <- ""
highlight   <- ""
title_color <- ""
sub_color   <- ""
cap_color   <- "grey70"




#==========================================================================#
# Define texts and annotations ---------------------------------------------
#==========================================================================#
tsize = unit(30, units = "cm")

sysfonts::font_add(
  family  = "fab6_reg",
  regular = "Font Awesome 6 Brands-Regular-400.otf") #/Library/Fonts/

sysfonts::font_add(
  family  = "fab6_solid",
  regular = "Font Awesome 6 Free-Solid-900.otf")

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
  "**Data**: TidyTuesday Week 29<br>", 
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

## By year / division -----
cols <- c("#1d9642", "#FFB400", "#FFC740", "#C20008", "#FF020D", "#13AFEF", "#e377c2", "#7f7f7f")

dyd |> 
  ggplot(aes(year, total, fill  = division)) +
  geom_col()

### ggstream ------
# Stream

dyd |> 
  ggplot(aes(year, total, 
             fill = division_ordered  # for labels reordering
             )) +
  geom_stream(
    bw = .7,
    extra_span = 0.2,
    n_grid = 100,
    color = NA
  ) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = seq(2011, 2024, by = 3)) +
  #scale_y_continuous(limits = c(-700, 600)) +
  guides(
    fill = guide_legend(
      ncol = 1,
      title = NULL,
      keywidth = 4,
      position = 'inside',
      theme = theme(legend.text = element_text(
        size = 20,
        colour = 'white'))
    )) +
  labs(x = NULL, y = NULL,
       title = 'TITLE',
       subtitle = 'Subtitle',
       caption = 'Tidytuesday week-29') +
  theme_minimal(base_size = 20) +
  theme(
    legend.position.inside = c(0.25, 0.15),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      size = 40, color = 'grey', 
      face = 'bold'),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(
      size = 40, color = 'grey30', 
      face = 'bold'),
    plot.subtitle = element_text(
      size = 30, color = 'grey50', 
      face = 'bold', margin = margin(b = 1, unit = 'cm')),
    plot.caption = element_text(
      size = 22, color = 'grey70', 
      face = 'bold'),
  )



ggview(units = 'cm', 
       width    = 40, 
       height   = 30,
       bg = 'black')

### waffle --------
# ggtext :
wtitle <- glue::glue("<b><span>ENGLISH WOMEN FOOTBALL</span></b>")
wsub <- glue::glue(
  "<b><span style='color:darkgrey; font-size:80px'>Each square represents 5 matches, colored according to divison naming history:</span></b><br>
  The <b><span style='color:#5A4FCF;'>FA Women Super League</span></b> was established in <b>2010</b>, and is the highest league of Women Football in England. It was split into <b><span style='color:#570D32;'>WSL 1</span></b> & <b><span style='color:#0BB19F;'>WSL 2</span></b> from <b>2014</b> season to <b>2017\\-2018</b> season.<br>",
  
  "The <b><span style='color:#D5006A;'>FA Women Championship</span></b> was established in <b>2014</b> as the FA Women Super League 2 (WSL 2). It is the second-highest division and became known under that name in <b>2018</b>.<br>",
  "After <b>2018</b>, the WSL 1 was renamed back to FA Women Super League. However, ahead of the <b>2022–2023</b> season, the top two tiers unveiled a new visual identity, dropping<br> the <b>FA</b> from the league names as just: <b><span style='color:#D590DA;'>Women Super League (WSL)</span></b> & <b><span style='color:#FF4500;'>Women Championship</span></b>.")
  # "<b><span style='color:#FFC43D;'>Women's Super League</span></b>"<b><span style='color:#F0426B;'>Asia</span></b>, <b><span style='color:#5A4FCF;'>Europe</span></b>, <b><span style='color:#059E75;'>Central and North America</span></b>, <b><span style='color:#06D6A0;'>South America</span></b> or <b><span style='color:#F68EA6;'>Oceania</span>. "
wcap = "<i>TidyTuesday | week 29</i>"
pals <- c(
  "#5A4FCF", "#570D32", "#0BB19F", "#a87c9f",  
  "#9CCADE", "#D5006A", "#FF4500", "#D590DA")

dsd |> 
  mutate(total = round(total/5, 0)) |> 
  ggplot(aes(fill = division_ordered, values = total)) +
  waffle::geom_waffle(
  n_rows = 4,
  colour = "white",
  size = 1.5,
  flip = TRUE) +
  scale_fill_manual(values = pals) +
  facet_grid(~season) +
  guides(
    fill = guide_legend(
      ncol = 2,
      title = NULL,
      keywidth = 1,
      keyheight = .5,
      position = 'inside',
      theme = theme(
        legend.text = element_text(size = 1.5 * tsize),
        legend.key.spacing.x = unit(1, 'cm')))) +
  labs( title = wtitle,
        subtitle = wsub,
        caption = plot_caption) +
  coord_equal() +
  theme_enhance_waffle() +
  theme(
    strip.text = element_text(
      size = 1.5 * tsize, face = 'bold', 
      color = 'grey', angle = 30),
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.background.x = element_rect(fill = "white"),
    strip.background.y = element_rect(fill = "dimgrey"),
    strip.text.y = element_text(color = "white"),
    legend.position.inside = c(0.5, -.3),
    plot.title = element_textbox(
      size = 4 * tsize, family = 'racing',
      margin = margin(b = .2, unit = 'cm')),
    plot.subtitle = element_textbox(
      size = 1.8 * tsize, family = 'saira',
      lineheight = .5,
      margin = margin(b = 2, unit = 'cm')),
    plot.caption = element_textbox(
      size = tsize,
      lineheight = .5,
      margin = margin(t = 5, unit = 'cm'))
  )

ggview(units = 'cm', 
       width    = 35, 
       height   = 25)


dfr

#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W29_EW_Football",
                       "plots_w29", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here(
    "W29_EW_Football",
    "plots_w29", 
    paste0("final_plot2", ".png")
  ),
  #plot    = plot,
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_color
)



# test
install.packages(c("waffle", "emojifont", "showtext"))

library(tidyverse)
library(waffle)
library(emojifont)
library(showtext)

# checking
extrafont::font_import(pattern = "fa-", prompt =  FALSE)
extrafont::loadfonts()
extrafont::fonttable() %>% 
  dplyr::as_tibble() %>% 
  dplyr::filter(grepl("Awesom", FamilyName)) %>% 
  select(FamilyName, FontName, fontfile)


sysfonts::font_add(family = "FontAwesome5Free-Solid", 
                   regular = "fa-solid-900.ttf")
font_add(family = "FontAwesome5Free-Regular", 
         regular = "fa-regular-400.ttf")
font_add(family = "FontAwesome5Brands-Regular", 
         regular = "fa-brands-400.ttf")
showtext_auto()

waffle::fa_grep("-ball")

waffle(
  c(`Poor=10` =10, `Average=18` = 18, `Excellent=7` =7), rows = 5, colors = c("#FD6F6F", "#93FB98", "#D5D9DD"),
  use_glyph = "soccer-ball", glyph_size = 12 ,title = 'Girls Performance', legend_pos="bottom"
)
