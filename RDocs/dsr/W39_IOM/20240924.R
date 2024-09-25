#=============================================================#
# Load packages ----------------------------------------------
#=============================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes,              
  showtext, extrafont, ggtext, ggtext, glue,
  patchwork, ggview, GWalkR, summarytools
)

#df |> missing_details() 
#=============================================================#
# Loading data ------------------------------------------------
#=============================================================#

tuesdata <- tidytuesdayR::tt_load("2024-09-24")

country_results_df <- tuesdata$country_results_df
individual_results_df <- tuesdata$individual_results_df
timeline_df <- tuesdata$timeline_df

timeline_df |> 
  dfSummary() |> 
  view()

# ctable(x = df$col1, y = df$col2, prop = "r")   # Show row proportions

freq(individual_results_df$award)
freq(individual_results_df$award, 
     order    = "freq", 
     rows     = 1:3,
     headings = FALSE,
     plain.ascii = TRUE, style = "rmarkdown") |> 
  view()

#=============================================================#
# Wrangling Data ---------------------------------------------
#=============================================================#

# Participating countries by year

timeline_df |> 
  ggplot(aes(year, countries)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 120)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = 'International Mathematical Olympiad (IMO)',
    subtitle = 'Number of participating countries by year',
    x = NULL,
    y = 'Total') +
  theme_light()


# Participation over years

timeline_df |> 
  ggplot(aes(year, all_contestant)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 700)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = 'International Mathematical Olympiad (IMO)',
    subtitle = 'Number of participatants by year',
    x = NULL,
    y = 'Total') +
  theme_light()


# Participation by gender over years

timeline_df |> 
  select(edition,  year, country, city, countries) |> 
  filter(year < 1985, year > 1970)

timeline_df |> 
  select(year, male_contestant, female_contestant) |> 
  pivot_longer(-year, names_to = 'gender', values_to = 'total') |> 
  mutate(gender = str_remove_all(gender, '_contestant')) |> 
  #filter(!is.na(total), year != 1980) |> 
  ggplot(aes(year, total, fill = gender)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 700)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = 'International Mathematical Olympiad (IMO)',
    subtitle = 'Number of participatants by year',
    x = NULL,
    y = 'Total') +
  theme_light()


# individual results by  country
individual_results_df |> 
  arrange(year, total) |> 
  slice_max(total, n = 2)


individual_results_df |> 
  summarise(avg = mean(total), .by = country) |> 
  arrange(desc(avg)) |> 
  head(n = 30)


#=============================================================#
# Loading fonts ----------------------------------------------
#=============================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")

font_families()
showtext_auto()


#=============================================================#
# Defining colors --------------------------------------------
#=============================================================#

bg_color    <- "white"
text_light  <- ""
text_dark   <- ""
highlight   <- ""
title_color <- ""
sub_color   <- ""
cap_color   <- ""




#=============================================================#
# Define texts and annotations -------------------------------
#=============================================================#
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




#===========================================================#
# Data Viz -------------------------------------------------
#===========================================================#

ggview(units = 'cm', width = 20, height = 18)



#==========================================================#
# Saving Plots and Gifs -----------------------------------
#==========================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W39_IOM",
                       "plots_w39", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W39_IOM",
                        "plots_w39", 
                        paste0("final_plot", ".png")),
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_color
)
