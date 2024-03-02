#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue,
  ggpath, ggalt)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-02-20")

raw <- tuesdata$isc_grants
#raw |> glimpse()

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
subt_font <- 'sairacond'

#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#
rcols <- c("#bb3e03","#ee9b00","#e9d8a6","#94d2bd","#0a9396","#005f73",
           '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
           '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
bg_col <- "grey90"
text_light <- '#f6d619'  #"grey70"
text_dark <- "#7f7f7f"
nbr_col <- '#f20759'
highlight_col <- "#7bf2da"
title_col <- "#3e84d4"
sub_color <- "#0a9396"
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
  select(year, group, amount = funded, name =  proposed_by, title) |> 
  mutate(cycle = case_when(group == 1 ~ 'Spring', TRUE ~ 'Fall'))

df |> count(year, sort = TRUE)
df |> count(name, sort = TRUE)
df |> count(cycle, sort = TRUE)

avg_func <- function(df, vals, ...){
  df |> 
    group_by(!!! ensyms(...)) |> 
    summarise(avg = mean({{vals}}), .groups = "drop")
}

avg_year <- df |> avg_func(amount, year)
avg_cycle <- df |> avg_func(amount, cycle)
avg_year_cycle <- df |> avg_func(amount, year, cycle)

df |> 
  arrange(desc(amount)) |> 
  group_by(name) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(desc(amount)) |> 
  head(10)

img_url <- "https://pbs.twimg.com/profile_images/1580520486899011585/RQyQKD_t_400x400.jpg"
avg10 <- df |> 
  group_by(name) |> 
  summarise(amount = mean(amount), .groups = 'drop') |> 
  arrange(desc(amount)) |> 
  top_n(10, amount) |> 
  mutate(
    #url_image = img_url,
    name = fct_reorder(name, amount)
    )
  
raw
ttl1 <- 'Building the “Spatial Data Science With R” Educational Materials and Pedagogical Infrastructure'
tts1 <- 'Building the “Spatial Data Science With R”...'
ttl2 <- 'The RECON COVID-19 challenge: leveraging the R community to improve COVID-19 analytics resources'
tts2 <- 'The RECON COVID-19 challenge: improving COVID-19 analytics'
ttl3 <- 'Strengthening of R in support of spatial data infrastructures management : geometa and ows4R R packages'
tts3 <- 'Strengthening R in spatial data infrastructures management: geometa-ows4R'
top20 <- df |> 
  arrange(desc(amount)) |> 
  select(name, amount, year, title) |> 
  head(20) |> 
  unite('name_year', c(name, year), remove = FALSE) |> 
  mutate(
    name_year = fct_reorder(name_year, amount),
    title = case_when(
      title == ttl1 ~ tts1,
      title == ttl2 ~ tts2,
      title == ttl3 ~ tts3,
      TRUE ~ title))

top20 |> select(title)

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
  "**Data**: TidyTuesday Week 8<br>", 
  "**Graphics:** ", 
  social_caption)


plot_title <- "R Consortium ISC Grants"
plot_subtitle_grant <- "Top 20 requested grants by amount in USD, including year requested, project title and, name of the person who requested the grant."


#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

df |> 
  ggplot(aes(factor(year))) +
  geom_bar() +
  labs(x = '', y = 'Frequency',
       title = 'R Consortium ISC Grants',
       subtitle = 'Distribution by year') +
  theme_light()



df |> 
  ggplot(aes(amount)) +
  geom_histogram(bins = 40) +
  labs(title = 'R Consortium ISC Grants',
       subtitle = 'Distribution by Grant Amount') +
  theme_light()


#geom_from_path(aes(path = url_image),
#               width = 0.05, #width relative to total
#               hjust = 0 #side alignment
#)

avg10 |> 
  ggplot(aes(amount, name)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = name), 
            x = 1, size = 3, color = 'gold',
            hjust = 0,  nudge_y = 0.45, 
            fontface = "bold") +
  scale_x_continuous(
    expand = expansion(mult = c(0,.1)),
    labels = dollar,
    breaks = c(seq(1:4) * 1e4)) +
  labs(
    x = "Grants Amount (USD)",
    title = 'My Title',
    subtitle = 'My  Subtitle',
    caption = "mycap") +
  theme_classic() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10, unit = 'pt')),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid.major.x = element_line(),
    plot.subtitle = element_text(margin = margin(b = 10, unit = 'pt')),
    plot.margin = margin(2,10,2,10,"pt"),
    plot.background = element_rect(fill = "#f1f1f1", color = NA),
    panel.background = element_rect(fill = "#f1f1f1", color = NA)
    )


top20 |> 
  ggplot(aes(amount,name_year)) +
  geom_lollipop(
    horizontal = TRUE,
    point.colour = nbr_col, color = nbr_col, size = 2,
    point.size = 0) +
  geom_text(aes(label = paste0(name, " [", year, "]")),  
            x = 1, size = tsize/1.5, color = text_light,
            hjust = 0,  nudge_y = 0.25,  fontface = "bold") +
  geom_text(
    aes(label = title, x = paste0(amount, "\n")), #str_wrap(title, width = 60) 
    x = 1, size = tsize/2, color = highlight_col, lineheight = 1,
    hjust = 0,  nudge_y = -0.3,  fontface = "italic") +
  geom_text(aes(label = paste('$', amount), x = amount), 
            size = tsize/1.5, color = nbr_col,
            hjust = 1,  nudge_x = 1, nudge_y = 0.3, fontface = "bold.italic") +
  labs(
    x = "Grants Amount (USD)",
    title = plot_title,
    subtitle = str_wrap(plot_subtitle_grant),
    caption = plot_caption) +
  theme_classic()  +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid.major.x = element_line(linewidth = .01, color = 'grey20'),
    plot.margin = margin(2,10,2,10,"pt"),
    plot.background = element_rect(fill = 'black', color = NA),
    panel.background = element_rect(fill = 'black', color = NA),
    plot.caption = element_textbox_simple(
      color = 'grey80', lineheight = .5, 
      halign = 1, size = tsize * 1.5),
    plot.title = element_textbox_simple(
      family = title_font, color = title_col,
      size = 3 * tsize, halign = .5),
    plot.subtitle = element_text(
      family = subt_font, color = sub_color, 
      size = 3 * tsize, lineheight = .4, hjust = .5,
      margin = margin(b = 10, t = 10, unit = 'pt'))
  )
#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path(
    "W8_ISC_Grants",
    "plots_w8", 
    paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W8_ISC_Grants", "plots_w8", "final_plot_grants.png"),
  width = 40, height = 40, units = "cm", bg = bg_col
)
