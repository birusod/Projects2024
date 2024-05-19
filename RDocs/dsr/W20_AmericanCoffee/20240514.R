#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork,               
  showtext, extrafont, ggtext, ggtext, glue  
)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load('2024-05-14')
tidytuesdayR::tt_load(2024, week = 20)
coffee_survey1 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

coffee_survey1 |> glimpse()
coffee_survey1 |> count(cups)

coffee_survey <- coffee_survey1 |> 
  filter(!is.na(age)) |> 
  mutate(
    age = str_remove_all(age, ' years old') |> 
      fct_relevel("<18", "18-24", "25-34",
                  "35-44", "45-54", "55-64",
                  ">65")) |> 
  filter(political_affiliation %in% c('Democrat', 'Independent', 'Republican'),
         !is.na(strength),
         cups %in% c('1', '2', '3', '4')) |> 
  mutate(cups = parse_number(cups))

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#
coffee_survey |> head(3)

# by political affiliation ----
coffee_survey |> count(political_affiliation)
coffee_survey |> 
  #filter(!str_detect(brew, 'Bean-to-cup')) |>
  #filter(!str_detect(brew, 'Coffee brewing machine')) |>
  count(strength)
paf_df <- coffee_survey  |> 
  select(political_affiliation, strength, cups) 

paf_df |> 
  count(political_affiliation, strength) |> 
  mutate(strength = fct_relevel(
   strength,
   'Weak', 'Somewhat light', 'Medium', 'Somewhat strong', 'Very strong')) |> 
  ggplot(aes(n, political_affiliation, fill = strength)) +
  geom_col(position = 'fill')


paf_df |> 
  group_by(political_affiliation) |> 
  summarise(cups = mean(cups)) |> 
  ggplot(aes(political_affiliation, cups, fill = political_affiliation)) +
  geom_col(show.legend = FALSE)


# By age groups ----

coffee_survey |> 
  count(age) |> 
  ggplot(aes(age, n)) +
  geom_col()


# favorite ----
favorite_df <- coffee_survey |> 
  mutate(favorite = str_remove_all(favorite, ' \\(e.g. Frappuccino\\)')) |> 
  mutate(favorite = case_when(
    favorite == 'Blended drink'  ~ 'Other', 
    TRUE ~ favorite)) |> 
  count(favorite, sort = TRUE)

coffee_survey |> colnames()


#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")               
font_add_google("Niconne", "niconne")             #title 
font_add_google("Stint Ultra Condensed", "stint") #caption
font_add_google("KoHo", "koho")                   #subtitle

showtext_auto()
font_families()
showtext_auto()


#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#
coffee_pal <- c("#623C2B", "#AF524E", "#736849", "#846c5b", "#EBD188", 
                "grey90", "#846c5b", '#3C2F2F', '#FDFDFB')

bg_color    <- coffee_pal[4]
text_light  <- coffee_pal[9]
text_dark   <- coffee_pal[1]
highlight   <- coffee_pal[5]
title_color <- coffee_pal[5]
sub_color   <- coffee_pal[9]
cap_color   <- coffee_pal[6]




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
  "**Data**: TidyTuesday Week 20<br>", 
  "**Graphics:** ", 
  social_caption)


ptitle    <- "The Great American Coffee Taste Test"
psubtitle1 <- 'bold("Participants favorite coffee drinks:")'
psubtitle2 <- "Survey by World champion barista James Hoffmann and coffee company Cometeer.\nDuring the survey, viewers were asked to fill out a survey about 4 coffees from Cometeer.\nData blogger Robert McKeon Aloe analyzed the data the following month (October 2023)."

gs1 <- ggplot() +
  annotate(
    geom = "text",
    x = 0.1, y = 0.5,
    hjust = 1,
    vjust = 0,
    label = psubtitle1, parse = TRUE,
    family = "koho",
    colour = sub_color,
    lineheight = 0.4,
    size = 1.8 * tsize) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "transparent",
      color = "transparent"
    )
  )
gs2 <- ggplot() +
  annotate(
    geom = "text",
    x = 0.1, y = 0.5,
    hjust = 1,
    vjust = 0,
    label = psubtitle2,
    family = "koho",
    fontface = 'italic',
    colour = sub_color,
    lineheight = 0.4,
    size = .8 * tsize) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "transparent",
      color = "transparent"
    )
  )
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
      panel.grid = element_blank(),
      plot.background = element_rect(fill = bg_color),
      plot.margin = margin(0.5, 0, .5, 0, unit = 'cm'),
      plot.title = element_textbox(
        size = 7  * tsize,
        hjust = 0.5,
        color = title_color,
        face = 'bold',
        family = 'niconne',
        margin = margin(0,0,1,0, "cm")),
      plot.caption = element_textbox(
        size = 2 * tsize,
        hjust = .95,
        color = cap_color,
        family = 'stint',
        margin = margin(.3, unit = 'cm'),
        lineheight = .3),
      axis.text.y = element_text(
        family = 'stint',
        face = 'bold',
        color = text_light,
        hjust = 1,
        size = 4 * tsize),
      axis.text.x = element_blank())
}


#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

p <- favorite_df |> 
  ggplot(aes(n, fct_reorder(favorite, n))) +
  geom_col(width = .5) +
  geom_text(
    aes(label = n), 
    hjust = -.5, 
    color = highlight,
    fontface = 'bold.italic',
    size = tsize) +
  scale_x_continuous(limits = c(0, 700),
                     expand = c(0, 0),
                     breaks = c(200, 400, 600)) +
  labs(
    x = NULL, y = NULL,
    title = ptitle, 
    caption = plot_caption) +
  costum_theme()

p +
  inset_element(
    p = gs1,
    on_top = TRUE,
    ignore_tag = TRUE,
    clip = FALSE,
    left = .1,
    bottom = -.1,
    right = 1.8,
    top = .5) +
  inset_element(
    p = gs2,
    on_top = TRUE,
    ignore_tag = TRUE,
    clip = FALSE,
    left = .1,
    bottom = -.2,
    right = 1.8,
    top = .3
  )

  
 


#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
#ggsave(
#  filename = file.path("W20_AmericanCoffee",
#                       "plots_w20", 
#                       paste0("final_plot", ".png"))
#  )

ggsave(
  filename = here::here("W20_AmericanCoffee", "plots_w20", "final_plot_w20.png"),
  #plot    = plot,
  width    = 30, 
  height   = 25, 
  units    = "cm",
  #bg       = bg_col
)

