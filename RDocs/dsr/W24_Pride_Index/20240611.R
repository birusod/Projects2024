#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork, ggrepel,             
  showtext, extrafont, ggtext, ggtext, glue, ggview
)
source('RDrafts/myfunc.R')
#df |> missing_details() 
#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-06-11")

p_idx <- tuesdata$pride_index
p_tag <- tuesdata$pride_index_tags

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

p_idx |> missing_details() 
p_tag |> missing_details() 

p_tag |> select(hbcu, military, other_minority_serving, technical)

p_tag |> count(campus_location) |> view()

p_tag |> 
  slice(41, 117) |> 
  select(campus_location)

dfr <-  
  p_idx |> left_join(p_tag, by = join_by(campus_name, campus_location)) |> 
  separate(campus_location, c('city', 'state'), sep = ', ')

dfr_long <- dfr |> 
  pivot_longer(-c(1:6), names_to = 'tag', values_to = 'tagged') |> 
  mutate(tagged = case_when(is.na(tagged) ~ FALSE, TRUE ~ tagged)) 

  

## Rating by state -----
pol_states <- read_csv('RDrafts/datasets/us_states_politics.csv')

dfr |> count(state)
by_state <- 
  dfr |> 
  summarise(rating_avg = mean(rating),
            students_avg = mean(students),
            total = sum(students),
            .by = state) |> 
  left_join(pol_states, by = join_by(state))


# Rating by campus -----
dfr |> 
  ggplot(aes(students, rating)) +
  geom_point()

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

bg_color    <- "#617a89"
text_light  <- "white"
text_dark   <- "grey80"
highlight   <- "cyan"
title_color <- ""
sub_color   <- ""
cap_color   <- "grey90"





#==========================================================================#
# Define texts and annotations ---------------------------------------------
#==========================================================================#
tsize = unit(20, units = "cm")

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
  "**Data**: TidyTuesday Week 24<br>", 
  "**Graphics:** ", 
  social_caption)


ptitle    <- "CAMPUS PRIDE INDEX"
notes1 <- str_wrap("Overall states where colleges and universities have large students population size on average tend to have a higher Campus Pride Index. This is often the case when those colleges and universities are located in states that lean toward liberal policies (aka 'Blues States'). Some outliers do exist: Rhode Island (with low CPI), Alabama and West Virginia (with high CPI).", 100)
subtitle1 <- glue::glue(
  "Relationship between rating and students population\n\n",
  notes1)


#==========================================================================#
# Custom Theme------------------------------------------------
#==========================================================================#

costum_theme <- function(){ 
  cfont <- "roboto"   
  theme_minimal(
    #base_family = cfont,
    #base_size = 40
  ) %+replace%    
    theme(
      legend.position = c(1, 0.05),
      legend.justification.inside = c(1.05, 0),
      plot.background = element_rect(fill = bg_color),
      panel.background = element_rect(fill = 'black'),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = .1, color = 'grey30'),
      panel.border = element_rect(color = 'grey25', fill = NA, size = 1),
      axis.text = element_text(size = 2 * tsize, color = text_dark),
      axis.ticks = element_blank(),
      axis.title = element_text(
        face = 'bold', size = 2.5 * tsize, color = text_light),
      axis.title.x = element_text(margin = margin(t = .5, unit = 'cm')),
      plot.title = element_text(
        face = 'bold', size = 4 * tsize, 
        color = text_light, hjust = .5,
        margin = margin(b = .5, unit = 'cm')),
      plot.subtitle = element_text(
        size = tsize * 2 , color = 'white',
        lineheight = .3,
        margin = margin(b = .5, unit = 'cm')),
      plot.caption =  element_textbox(
        #family = caption_font,
        hjust = 1,
        color = 'grey80',
        size = 1.5 * tsize,
        lineheight = 0.3,
        margin = margin(.5,0,0,0, unit = 'cm')),
      plot.margin = margin(.2, .2, .2, .2, unit = 'cm')
    )
}




#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#
# dfr |> 
#   ggplot(aes(community_type, students)) +
#   geom_boxplot()
# 
# 
# dfr |> 
#   ggplot(aes(rating, students)) +
#   geom_point()
# 
# dfr |> 
#   count(state) |> 
#   ggplot(aes(n, fct_reorder(state, n))) +
#   geom_col()


## Rating by state ------

by_state |> 
  ggplot(aes(students_avg, rating_avg, label = state, color = leaning)) +
  geom_point(aes(size = total, fill = rating_avg), shape = 21) +
  geom_text_repel(min.segment.length = 2, 
                  size = tsize/2, 
                  fontface = 'bold',
                  #color = highlight,
                  nudge_x = 2) +
  scale_size_area(max_size = 15, 
                  breaks = c(50000, 150000, 250000),
                  labels = c('50K', '150K', '250K')) +
  scale_fill_gradient2(low = 'red', 
                        mid = 'orange', midpoint = 3.5, 
                        high = 'darkgreen') +
  scale_color_manual(values = c('dodgerblue', 'wheat', 'tomato')) +
  scale_x_continuous(labels = comma, expand = c(.05, 0)) +
  coord_cartesian(ylim = c(1.8, 5.2), clip = 'off') +
  guides(
    color = 'none',
    size = guide_legend(
      override.aes = list(key = 3, color = 'grey'), 
      title = 'Population',
      direction = 'horizontal',
      position = 'inside',
      theme(
        legend.title.position = 'top',
        legend.text.position = 'top',
        legend.text = element_text(
          hjust = 0.5, vjust = -1, 
          color = text_light,
          size = 2 * tsize),
        legend.title = element_text(
          hjust = 0.5, vjust = -1,
          color = text_dark, face = 'bold',
          size = 3 * tsize),
        legend.key.spacing.x = unit(1, 'pt'),
        legend.background = element_rect(fill = NA),
        legend.key = element_blank())),
    fill = guide_legend(
      override.aes = list(size = 5), 
      title = 'Rating',
      direction = 'horizontal',
      position = 'inside',
      theme(
        legend.title.position = 'top',
        legend.text.position = 'top',
        legend.text = element_text(
          hjust = 0.5, vjust = -1, 
          color = text_light,
          size = 2 * tsize),
        legend.title = element_text(
          hjust = .5, vjust = -1,
          color = text_dark, face = 'bold',
          size = 3 * tsize),
        legend.key.spacing.x = unit(10, 'pt'),
        legend.background = element_rect(fill = NA)))) +
  labs(
    x = 'Student population (full-time / Average)',
    y = 'Average Star Rating (1-5 points)',
    title = ptitle,
    subtitle = subtitle1,
    caption = plot_caption) +
  # annotate('text', x = 20000, y = 6.2, 
  #          label = subtitle1, 
  #          color = 'white', size = tsize * 1.5 ,
  #          hjust = 0.5, vjust = 1,
  #          lineheight = .5) +
  costum_theme()
ggview(units = 'cm', width = 25, height = 22)
#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

ggview(p, units = 'px', height = 800, width = 1000)
# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W24_Pride_Index",
                       "plots_w24", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W24_Pride_Index", "plots_w24", "final2_w24.png"),
  #plot    = plot,
  width = 25, height = 22,
  units    = "cm"
)
