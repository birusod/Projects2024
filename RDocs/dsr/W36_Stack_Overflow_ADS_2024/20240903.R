#=============================================================#
# Load packages ----------------------------------------------
#=============================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes,              
  showtext, extrafont, ggtext, ggtext, glue,
  patchwork, ggview
)
source('RDrafts/myfunc.R')
#df |> missing_details() 
#=============================================================#
# Loading data ------------------------------------------------
#=============================================================#

tuesdata <- tidytuesdayR::tt_load("2024-09-03")
svy <- tuesdata$stackoverflow_survey_single_response

questions <- tuesdata$stackoverflow_survey_questions
responses <- tuesdata$qname_levels_single_response_crosswalk

#responses |> view()
#questions |> view()

#=============================================================#
# Wrangling Data ---------------------------------------------
#=============================================================#

svy |> missing_details() 

# Participants description -----

desc_df <- responses |> 
  filter(qname == 'main_branch') |> 
  mutate(desc = str_remove(label, 'I am a |I am |I ') |> str_to_upper())

svy |> 
  count(main_branch) |> 
  left_join(desc_df, by = join_by(main_branch == level)) |> 
  mutate(desc = fct_reorder(desc, n)) |> 
  ggplot(aes(n, desc)) +
  geom_col() +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 55000),
                     labels = comma) +
  labs(title = 'Stack Overflow Annual Developer Survey 2024',
       subtitle = 'Which of the following options best describes you today? For the purpose of this survey, a developer is "someone who writes code"',
       x = 'Total participants', 
       y = NULL) +
  theme_light() +
  theme(plot.subtitle = element_textbox_simple(),
        plot.title.position = 'plot')
  

# participants age ----
age_lbl <- 
  responses |> 
  filter(qname == 'age') |> 
  mutate(age = str_remove_all(label, ' years old')) |> 
  mutate(age = case_when(
    age == '65 years or older' ~ '65+',
    age == 'Prefer not to say' ~ 'Unknown',
    age == 'Under 18' ~ '<18',
    TRUE ~ age)) |> 
  mutate(age = factor(age, 
                      levels = c("<18", "18-24", "25-34", "35-44",
                                 "45-54", "55-64", "65+", "Unknown"))) |> 
  select(age_level = level , age_cat = age)

svy |> 
  count(age) |> 
  left_join(age_lbl, by = join_by(age == age_level)) |> 
  mutate(col = if_else(age_cat == 'Unknown', 'grey', 'blue')) |> 
  ggplot(aes(age_cat, n, 
             fill = col)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c('skyblue', 'grey')) +
  labs(title = 'Stack Overflow Annual Developer Survey 2024',
       subtitle = 'Participants Age Distribution') +
  theme_light()


# participants current job ----

job_df <- 
  responses |> 
  filter(qname == 'dev_type')


jobs <- svy |> 
  count(dev_type, name = 'total') |> 
  left_join(job_df, by = join_by(dev_type == level)) |> 
  filter(!is.na(label)) |> 
  mutate(job = fct_lump_n(label, n = 20, w = total)) 

jobs |> 
  summarise(total = sum(total), .by = job) |> 
  mutate(job = fct_reorder(job, total)) |> 
  ggplot(aes(total, job)) +
  geom_col() +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20))


x <- factor(rep(LETTERS[1:9], times = c(40, 10, 5, 27, 1, 1, 1, 1, 1)))
x %>% table()
x %>%
  fct_lump_n(4) %>%
  table()

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
  filename = file.path("W36_Stack_Overflow_ADS_2024",
                       "plots_w36", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W36_Stack_Overflow_ADS_2024",
                        "plots_w36", 
                        paste0("final_plot", ".png")),
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_color
)
