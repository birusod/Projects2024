#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,           # data import-wrangling-viz
  scales, ggthemes, patchwork,                # theme-layout
  showtext, extrafont, ggtext, ggtext, glue   # text rendering
  )


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-04-02")
dbd <- 
  tuesdata$dubois_week10 |> 
  clean_names() |> 
  rename_with(~c('job', 'pct'))

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#




#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#

# sysfonts [google fonts]
font_add_google("Roboto", "roboto")            # caption
font_add_google("JetBrains Mono", "jbmono")    # x axis
font_add_google("DM Serif Display", "dmserif") # title
font_add_google("Tajawal", "tajawal")          # subtitle

# showtext
showtext_auto()

#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#

bg_color    <- 'black'
bar_color   <- 'seagreen'
hl_color    <- "gold"  # highlight
title_color <- "#bf2c23"
sub_color   <- "white"
cap_color   <- 'grey90'

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
  "**Data**: TidyTuesday Week 14<br>", 
  "**Graphics:** ", 
  social_caption)


title        <- "Du BOIS VIZ CHALLENGE 2024"
title_txt    <- "Du BOIS VIZ CHALLENGE 2024"
subtitle     <- "Percentage of graduates of Atlanta University as of 1900 by occupation"
subtitle_txt <- "Percentage of graduates by occupation<br>Atlanta University (1900)"


#==========================================================================#
# Custom Theme------------------------------------------------
#==========================================================================#

costum_theme_radial <- function(){ 
  theme_void() %+replace% 
    theme(
      plot.background = element_rect(fill = bg_color),
      plot.caption    =  element_textbox(family     = "roboto",
                                         hjust      = 1,
                                         color      = cap_color,
                                         size       = 10,
                                         lineheight = 0.3,
                                         margin     = margin(.5,0,0,0, 
                                                             unit = 'cm')),
      plot.margin    = margin(1,1,1,1, 
                              unit = 'cm')
    )
}

  
#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#
## barplot

dbd |> 
  mutate(job = fct_reorder(job, pct)) |> 
  ggplot(aes(pct/100, job)) +
  geom_col(fill = bar_color) +
  scale_x_continuous(labels = percent,
                     expand = c(0, 0),
                     breaks = c(.15, .30, .45)) +
  labs(x        = NULL, 
       y        = NULL, 
       title    = title, 
       subtitle = subtitle) +
  theme_light() +
  ggthemes::theme_tufte() +
  theme(axis.text.x   = element_text(hjust  = 1, 
                                     family = 'jbmono'),
        axis.text.y   = element_text(family = 'jbmono'),
        plot.title    = element_text(family = 'dmserif'),
        plot.subtitle = element_text(family = 'tajawal')
  ) 

## radial char
dbd |> 
  arrange(pct) |> 
  mutate(
    pos     = c(1:6),
    greens  = c("#B5D4C2", "#88BA9C", "#439364", "#2D8651", "#16793E", "#006D2C") ,
    pct_fmt = sprintf("%05.2f", pct)) |> 
  
  ggplot(aes(x = 0, xend = pct, y = pos, yend = pos, color = greens)) +
  #geom_hline(yintercept = c(1:6), linewidth = 10, color = "#CCE1D4") +
  #geom_hline(yintercept = c(1:6), linewidth = 10, color = "#CCE1D4") +
  geom_segment(color = "cyan4", linewidth = 11, lineend = 'round') +
  geom_segment(linewidth = 9, lineend = 'round') +
  
  geom_text(
    aes(label = paste0(job, ', ', pct_fmt, '% '),
        x     = (0.01 * pos)), 
    hjust    = 1, 
    nudge_x  = 0,
    size     = 8, 
    color    = hl_color,
    fontface = 'bold', 
    family   = 'jbmono') +
  scale_color_identity() +
  geom_point(aes(x = pct - 0.03 * pos), 
             size  = 5, 
             color = "#bf2c23",
             shape = 21, 
             fill  = 'white') +
  geom_point(aes(x = pct - 0.03 * pos), 
             size  = 2, 
             color = "#bf2c23",
             shape = 21, 
             fill  = 'white') +
  scale_x_continuous(limits = c(0, 90)) +
  scale_y_continuous(limits = c(-3, 6)) +
  coord_polar() +
  theme_void() -> radial_plot

#radial_plot_titled <- radial_plot + labs(title = title, subtitle = subtitle)

radial_plot + 
  labs(caption = plot_caption) +
  costum_theme_radial() +
  annotate(
    "richtext", 
    x           = 0, 
    y           = -1, 
    label       = title_txt, 
    family      = "dmserif", 
    fontface    = "bold", 
    size        = 12, 
    hjust       = 0.5, 
    vjust       = 1,
    color       = title_color, 
    fill        = NA, 
    label.color = NA
  )  +
  annotate(
    "richtext", 
    x           = 0, 
    y           = -3, 
    label       = subtitle_txt, 
    family      = "tajawal", 
    size        = 10, 
    hjust       = 0.5, 
    vjust       = 1,
    color       = sub_color, 
    fill        = NA, 
    label.color = NA
  ) -> p1
#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#


ggsave(
  plot = p1,
  filename = here::here("W14_DuBois", "plots_w14", "final_pdf_1.pdf"), 
  device = cairo_pdf,
  width = 12, height = 12, units = "in" # dpi = 600
)

library(magick)
tmp = image_read_pdf(
  here::here("W14_DuBois", "plots_w14", "final_pdf_1.pdf"), 
  density = 600)

image_write(tmp, here::here("W14_DuBois", "plots_w14", "plot_img_1.png"))
