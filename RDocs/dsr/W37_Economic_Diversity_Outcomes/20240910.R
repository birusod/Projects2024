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
df |> missing_details() 
#=============================================================#
# Loading data ------------------------------------------------
#=============================================================#

tuesdata <- tidytuesdayR::tt_load("2024-09-10")
df <- tuesdata$college_admissions

  
#=============================================================#
# Wrangling Data ---------------------------------------------
#=============================================================#

df |> colnames()

df |> 
  filter(!is.na(attend_instate)) |> 
  missing_details() 

# attend: fraction of students attending within a parent income bin.
# rel_apply: Calculated using adjusted score-sending rates, the relative fraction of all standardized test takers who send test scores to a given college

dff <- df |> 
  select(
    name, par_income_bin, par_income_lab, attend,  
    rel_apply, public:test_band_tier)


dff |> view()

dff |> select(par_income_lab) |> distinct()

dd <- dff |>
  filter(par_income_lab == 'Top 0.1') |> 
  summarise(val  = sum(attend, na.rm = TRUE), .by = tier) |> 
  mutate(income = 'Top 0.1%') |> 
  bind_rows(
    dff |> 
      filter(par_income_lab == 'Top 1') |> 
      summarise(val  = sum(attend, na.rm = TRUE), .by = tier) |> 
      mutate(income = 'Top 1%')
  ) |> 
  bind_rows(
    dff |> 
      filter(par_income_lab %in% c(
        '95-96', '96-97', '97-98', 
        '98-99', '99-99.9', 'Top 0.1')) |> 
      summarise(val  = sum(attend, na.rm = TRUE), .by = tier) |> 
      mutate(income = 'Top 5%')
  ) |> 
  bind_rows(
    dff |> 
      filter(par_income_lab %in% c(
        '90-95', '95-96', '96-97', '97-98', 
        '98-99', '99-99.9', 'Top 0.1')) |> 
      summarise(val  = sum(attend, na.rm = TRUE), .by = tier) |> 
      mutate(income = 'Top 10%')
  ) |> 
  bind_rows(
    dff |> 
      filter(par_income_lab %in% c(
        '80-90', '90-95', '95-96', '96-97', '97-98', 
        '98-99', '99-99.9', 'Top 0.1')) |> 
      summarise(val  = sum(attend, na.rm = TRUE), .by = tier) |> 
      mutate(income = 'Top 20%')
  ) |> 
  bind_rows(
    dff |> 
      filter(par_income_lab %in% c('60-70', '70-80')) |> 
      summarise(val  = sum(attend, na.rm = TRUE), .by = tier) |> 
      mutate(income = '4th quintile')
  ) |> 
  bind_rows(
    dff |> 
      filter(par_income_lab %in% c('40-60')) |> 
      summarise(val  = sum(attend, na.rm = TRUE), .by = tier) |> 
      mutate(income = '3rd quintile')
  ) |> 
  bind_rows(
    dff |> 
      filter(par_income_lab %in% c('20-40')) |> 
      summarise(val  = sum(attend, na.rm = TRUE), .by = tier) |> 
      mutate(income = '2nd quintile')
  ) |> 
  bind_rows(
    dff |> 
      filter(par_income_lab == '0-20') |> 
      summarise(val  = sum(attend), .by = tier) |> 
      mutate(income = 'Bottom 20%')
  ) |> 
  mutate(
    income = fct_inorder(income) |> fct_rev(),
    tier = factor(tier, levels = c(
      'Ivy Plus', 'Other elite schools (public and private)',
      'Highly selective private', 'Highly selective public', 
      'Selective private', 'Selective public')) |> 
      fct_rev()) 

dd |> select(tier) |> distinct()

dd |> 
  filter(income == 'Bottom 20%') |> 
  ggplot(aes(val, tier)) +
  geom_col()

dd |> 
  ggplot(aes(val, income, fill = tier)) +
  geom_col(position = 'fill') +
  theme(
    legend.position = 'top'
  )
  


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
  filename = file.path("W37_Economic_Diversity_Outcomes",
                       "plots_w37", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W37_Economic_Diversity_Outcomes",
                        "plots_w37", 
                        paste0("final_plot", ".png")),
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_color
)
