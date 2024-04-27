#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-02-06")

raw <- tuesdata$heritage
raw |> glimpse()


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

title_font <- 'alfaslab'
subt_font <- 'righteous'
caption_font <- 'sairacond'

#==========================================================================#
# Defining colors and Theme ------------------------------------------------
#==========================================================================#

bg_col <- "#ebfaff"
text_col <- "#4f2b00"
text_hil <- "#c46c00"


norway <- c("#323232", "#666666", "#999999")
denmark <- c("#0073C2", "#66ABDA", "#99C7E6")
sweden <- c("#990000", "#DC143C", "#FF6666")
df
cpal <- c("Norway" = "#323232", "Denmark" = "#1e90ff", "Sweden" = "#E16565")
monochromeR::view_palette(cpal)

# alpha scale
amax <- 1
amin <- 0.7
avals <- seq(amax, amin, length.out = 2)

tsize = unit(10, units = "cm")

costum_theme <- function(){ 
  cfont <- "roboto"   
  theme_minimal(base_family = cfont
    #base_size = 40
  ) %+replace%    
    theme(
      panel.grid.major = element_blank(),    
      panel.grid.minor = element_blank(),    
      axis.ticks = element_blank(), 
      
      plot.title = element_textbox_simple(  
        family = title_font,           
        size = 3 * tsize,               
        face = 'bold',            
        hjust = 0.5,
        margin = margin(0,0,1,0, unit = 'pt')),               
      
      plot.subtitle = element_textbox_simple(         
        family = subt_font,           
        size = 3 * tsize, 
        color = 'grey60',
        hjust = 0.5,
        margin = margin(0,0,1,0, unit = 'cm')),               
      
      plot.caption =  element_textbox(
        family = caption_font, 
        hjust = 1, 
        color = 'grey60', 
        size = 1.5 * tsize,
        lineheight = 0.3,
        margin = margin(.5,0,0,0, unit = 'cm')),               
      
      axis.title = element_text(            
        family = cfont,            
        size = 3 * tsize),              
      
      axis.text = element_text(             
        family = cfont,           
        size = 3 * tsize),               
      plot.margin = margin(1,1,1,1, unit = 'cm')
    )
}
#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

df <- raw |> 
  pivot_longer(-country, names_to = 'year', values_to = 'total') |>
  mutate(country = fct_relevel(country, 'Norway', 'Denmark', 'Sweden'))


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
  <span style='color: {text_col}'>{xtwitter_username}</span>"
)
plot_caption <- paste0("**Data**: TidyTuesday Week 6  | ", "**Graphics:** ", social_caption)

ptitle <- "WORLD HERITAGE"
ps_year <- "Number of Sites by Year"
ps_country <- glue(
  "Number of Sites by Country: ",
  "<span style = 'color:{country_pal['Denmark']};'>Denmark</span>, ",
  "<span style = 'color:{country_pal['Norway']};'>Norway</span>, ",
  "<span style = 'color:{country_pal['Sweden']};'>Sweden</span>.")
ps_country2 <- paste0(
  "Number of Sites by Country: ",
  "<span style = 'color:", country_pal['Denmark'], ";'>Denmark</span>, ",
  "<span style = 'color:", country_pal['Norway'], ";'>Norway</span>, and ",
  "<span style = 'color:", country_pal['Sweden'], ";'>Sweden</span>.")
ps_country_year <- "Number of Sites by Country and Year"

#ps_country2 <- paste(strwrap(ps_country, 40), collapse = "\n")


#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

## Bar plot by  country/year -----
df |> 
  ggplot(aes(year, total, fill = country)) +
  geom_col(position = 'dodge', show.legend = FALSE) +
  scale_fill_manual(values = cpal) +
  labs(
    x = NULL, y = NULL,
    title = ptitle,
    subtitle = ps_country,
    caption = plot_caption) +
  costum_theme()

## Bar plot by  country with year shaded -----

df |> 
  ggplot(aes(total, country, alpha = year, fill  = country)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(values = cpal)  +
  scale_alpha_manual(values = avals)  +
  guides(
    fill = guide_none(),
    alpha = guide_legend(
      override.aes = aes(label = ""),
      title = 'YEAR',
      title.position = 'left',
      label.position  = 'top',
      direction = 'horizontal',
      keyheight = .5,
      label.vjust = -1)) +
  labs(
    x = NULL, y = NULL,
    title = ptitle,
    subtitle = ps_country,
    caption = plot_caption) +
  costum_theme() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = 'top',
    legend.title = element_text(family = "roboto", size = 60, face = 'bold'),
    legend.text = element_text(family = "roboto", size = 60, hjust = -1),
    legend.spacing.x = unit(1,"cm"),
    legend.spacing.y = unit(.4,"cm")
    )
  

## Donut


df_2022 <- df |> 
  filter(year == 2022) 

labs_2022 <- paste0(df_2022$country, "\n", df_2022$total)
labs_all <- paste0(df$country, ", ", df$total, " (", df$year, ")")
dd_size = 10


df_2022 |> 
  ggpubr::ggdonutchart("total", label = labs_2022,
             fill = "country", color = "white",
             lab.pos = "in", lab.font = "white",
             palette = c("#E16565", "#1e90ff", "#323232")) +
  labs(
    x = NULL, y = NULL,
    title = ptitle,
    subtitle = ps_country2,
    caption = plot_caption) +
  theme( 
    axis.text.y = element_blank(),
    legend.position = 'none',
    plot.title = element_textbox_simple(
        family = title_font,
        size = 14, face = 'bold', hjust = 0,
        margin = margin(0,0,1,0, unit = 'pt')),
    plot.subtitle = element_textbox_simple(
        family = subt_font,
        size = 12,  width =  unit(2, "npc"),
        color = 'grey60', hjust = 0,
        margin = margin(0.1,0,0.1,0, unit = 'cm')),
      plot.caption =  element_textbox(
        family = caption_font, color = 'grey60',
        margin = margin(0,0,0,0, unit = 'cm')),
    )


df_abbr <- df |> 
  mutate(abbr = case_when(
    country == 'Norway' ~ 'NOR',
    country == 'Denmark' ~ 'DNK',
    country == 'Sweden' ~ 'SWE')) 
labs_abbr <- paste0(df_abbr$abbr, ", ", df$total, " (", df$year, ")")
df_abbr |> 
  ggpubr::ggdonutchart("total", label = labs_abbr,
                       fill = "country", color = "white",
                       lab.pos = "in", lab.font = c(2 * dd_size, "bold", "white"),
                       palette = c("#E16565", "#1e90ff", "#323232")) +
  labs(
    x = NULL, y = NULL,
    title = ptitle,
    subtitle = ps_country2,
    caption = plot_caption) +
  theme( 
    axis.text.y = element_blank(),
    legend.position = 'none',
    plot.title = element_textbox_simple(
      family = title_font,
      size = 8 * dd_size, face = 'bold', hjust = 0,
      margin = margin(0,0,1,0, unit = 'pt')),
    plot.subtitle = element_textbox_simple(
      family = subt_font,
      size = 8 * dd_size,  width =  unit(2, "npc"),
      color = 'grey60', hjust = 0,
      margin = margin(0.1,0,0.1,0, unit = 'cm')),
    plot.caption =  element_textbox(
      family = caption_font, color = 'grey60',
      size = 6 * dd_size,
      margin = margin(0,0,0,0, unit = 'cm')),
  )

## Donut webr ----

webr::PieDonut(
  df, 
  aes(country, year, count = total, groupR), 
  title = paste0(ptitle, ":\n", ps_country_year))


## Bar plot by  country with year shaded + geom_textbox-----

df |> 
  ggplot(aes(total, country, fill  = country)) +
  geom_col(aes(alpha = year), position = 'dodge') +
  ggtext::geom_textbox(aes(label = total, group = year),
                       size = 60, color = 'white', 
                       family = 'roboto', fontface = 'bold',
                       halign = 1, hjust = 1,
                       fill = NA, box.colour = NA,
                       position = position_dodge(width = .9)) +
  scale_fill_manual(values = cpal)  +
  scale_alpha_manual(values = avals)  +
  guides(
    fill = guide_none(),
    alpha = guide_legend(
      override.aes = aes(label = "", size = 1),
      title = 'YEAR',
      title.position = 'left',
      label.position  = 'top',
      direction = 'horizontal',
      keyheight = .5,
      label.vjust = -1)) +
  labs(
    x = NULL, y = NULL,
    title = ptitle,
    subtitle = ps_country,
    caption = plot_caption) +
  costum_theme() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = 'top',
    legend.title = element_text(family = "roboto", size = 60, face = 'bold'),
    legend.text = element_text(family = "roboto", size = 60, hjust = -1),
    legend.spacing.x = unit(1,"cm"),
    legend.spacing.y = unit(.4,"cm")
  )

#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path(
    "W6_HeritageSites",
    "plots_w6", 
    paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W6_HeritageSites", "plots_w6", "plot_scale_nolab.png"),
  #plot = plot, # last plot
  width = 40, 
  height = 30, 
  units = "cm",
  bg = bg_col
)
