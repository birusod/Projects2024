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

tuesdata <- tidytuesdayR::tt_load("2024-08-20")
df <- tuesdata$english_monarchs_marriages_df
df
#=============================================================#
# Wrangling Data ---------------------------------------------
#=============================================================#

df <- 
  df |> 
  mutate(k_age = parse_number(king_age),
         c_age = parse_number(consort_age),
         m_year = parse_number(year_of_marriage),
         age_diff = k_age - c_age)

# Age distribution  ----
df |> 
  filter(!is.na(k_age)) |> 
  ggplot(aes(k_age)) +
  geom_density() +
  labs(title = 'Age Distribution of Kings')

df |> 
  filter(!is.na(c_age)) |> 
  ggplot(aes(c_age)) +
  geom_density() +
  labs(title = 'Age Distribution of Consorts')

age_data <- 
  df |>
  filter(!is.na(k_age), !is.na(c_age)) |> 
  select(k_age, c_age) |> 
  pivot_longer(everything(), names_to = 'title', values_to = 'age')

age_data |> 
  ggplot(aes(age, color = title, fill = title)) +
  geom_density(alpha = .4) +
  labs(title = 'Age Distribution of Kings and Consorts') 



## Age evolution

df |> 
  drop_na() |> 
  mutate(m_year = fct_inorder(as.character(m_year))) |> 
  ggplot(aes(m_year, k_age)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'DATE (YEAR)', y = NULL, 
       title = 'Marriage Age Of Kings',
       caption = 'TidyTuesday | Week 33') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks.x = element_blank())
  
df |> 
  drop_na() |> 
  mutate(m_year = fct_inorder(as.character(m_year))) |> 
  ggplot(aes(m_year, c_age)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'DATE (YEAR)', y = NULL, 
       title = 'Marriage Age Of Consorts',
       caption = 'TidyTuesday | Week 33') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks.x = element_blank())

age_gap <- df |> 
  drop_na() |> 
  mutate(m_year = fct_inorder(as.character(m_year))) |> 
  select(m_year, k_age, c_age) 
age_gap |> 
  ggplot(aes(x = k_age, xend = c_age,
             y = m_year, yend = m_year)) +
  geom_segment()

age_gap |> 
  pivot_longer(-m_year, names_to = 'title', values_to = 'age') |> 
  ggplot(aes(age, m_year, color = title)) +
  geom_point() +
  scale_color_manual(values = c('firebrick', 'navy')) +
  theme_light()


## Age difference ----
df |> count(m_year, sort = TRUE)
df |> 
  filter(!is.na(age_diff), age_diff > 0) |> 
  mutate(m_year = fct_inorder(as.character(m_year))) |> 
  ggplot(aes(m_year, age_diff)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = 'DATE (YEAR)', y = NULL, 
       title = 'Marriage Age Gap Between Kings and Consorts',
       caption = 'TidyTuesday | Week 33') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks.x = element_blank())
  
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
  filename = file.path("W34_Monarchs_and_Marriages",
                       "plots_w34", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W34_Monarchs_and_Marriages",
                        "plots_w34", 
                        paste0("final_plot", ".png")),
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_color
)
