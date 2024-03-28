#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#
ls()
pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-03-12")


## Overview -----
dfr <- tuesdata$fiscal_sponsor_directory 
dfr |> tail(30) |> view()
dfr |> dim()
dfr |> colnames()

dfs <- dfr |> 
  select(
    name, 
    created = year_501c3, 
    acted = year_fiscal_sponsor,
    total = n_sponsored,
    fee = fiscal_sponsorship_fee_description,
    eligibility = eligibility_criteria,
    project = project_types,
    services,
    model = fiscal_sponsorship_model) 


#==========================================================================#
# EDA + Wrangling Data -----------------------------------------------------
#==========================================================================#

## Missing values: -----
# replace missing model with 'Unknown'
# remove all rows with missing created & acted year, fee, project, services...

dfs |> is.na() |> colMeans() |> round(2)
dfs |> filter(is.na(project)) |> view()



## Any duplicate name? ----

dfs |> select(name) |> duplicated() |> sum()
dfs |> count(name, sort = TRUE)
dfs |> 
  filter(name %in% c('Peace Development Fund', 'Side Project Inc.')) |> 
  view()
# removing dup rows:
# distinct(col, .keep_all = TRUE)
# df |> unique(col)
# df |> group_by(col) |> filter(row_number() == 1)
# df |> group_by(col) |> slice(1)
# df |> group_by(x) |> filter (! duplicated(y))
# df |> arrange(desc(val)) |> group_by(id) |> slice(1)


dff <- dfs |> 
  mutate(model = replace_na(model, 'Unknown')) |> 
  drop_na() |> 
  group_by(name) |> filter(row_number() == 1)


## Extracting percentage from the fee column: -----

dff |> 
  select(name, fee) |> slice_sample(n = 5) |> 
  mutate(pct = str_extract_all(fee, "[0-9]+[.]?[0-9]*(?=%)")) |> 
  unnest(pct) |> 
  view()

# all digits: "\\d+\\.?\\d?"
# digits before %: "[0-9]+[.]?[0-9]*(?=%)"
get_percent <- function(data, id_column, value_column){
  data |> 
  select({{id_column}}, {{value_column}}) |> 
  mutate(percent := str_extract_all(
    {{value_column}}, 
    "[0-9]+[.]?[0-9]*(?=%)")
    ) |> 
  unnest(percent) |> 
  mutate(percent = parse_number(percent)) |> 
  group_by(name) |> 
  summarise(percent =  mean(percent))
}


dff2 <- dff |> 
  left_join(dff |> get_percent(name, fee)) |> 
  ungroup()

dff2 |> 
  select(name, eligibility) |> 
  mutate(
    eligibility = str_remove_all(
      eligibility, 
      paste(
        c('Type of service: ', 'Aligned mission/values: ',
          'Aligned mission values: '), 
        collapse = "|"
      )))

## Counting projects, services  and models ----
project_df <- dff2 |> 
  select(name, project) |> 
  separate_longer_delim(project, delim = '|') |> 
  mutate(project = case_when(
    str_detect(project, 'Arts and culture') ~ 'Arts and culture',
    str_detect(project, 'Mental health') ~ 'Health',
    str_detect(project, 'Health/nutrition') ~ 'Health', 
    str_detect(project, 'Mental Health') ~ 'Health',
    str_detect(project, 'Health/Nutrition') ~ 'Health', 
    str_detect(project, 'Education:') ~ 'Education',
    str_detect(project, 'Children, youth') ~ 'Children, youth and families',
    str_detect(project, 'Disaster relief') ~ 'Disaster relief',
    str_detect(project, 'Youth Development') ~ 'Youth development',
    str_detect(project, 'Youth') ~ 'Youth development',
    str_detect(project, 'Women') ~ 'Women',
    str_detect(project, 'Economic development') ~ 'Economic development',
    str_detect(project, 'Housing') ~ 'Housing',
    str_detect(project, 'Social services') ~ 'Social services',
    str_detect(project, ' Social services') ~ 'Social services',
    str_detect(project, 'Homelessness') ~ 'Social services',
    str_detect(project, 'Festivals and events') ~ 'Festivals and events',
    str_detect(
      project, paste(c('communities of color', 'LGBTQ', 'GBTQ'),
                     collapse = "|")) ~ 'People or communities of color/minorities',
    TRUE ~ project)) |> 
  mutate(project = fct_lump_min(project, 25))
  
project_df |> count(project, sort = TRUE)



services_df <- dff2 |> 
  select(name, services) |> 
  separate_longer_delim(services, delim = '|') |> 
  mutate(services = case_when(
    str_detect(services, 'Bill paying') ~ 'Bill paying',
    str_detect(services, 'Bill Paying') ~ 'Bill paying',
    str_detect(services, 'Bookkeeping') ~ 'Bookkeeping',
    str_detect(services, 'Computer IT') ~ 'Computer IT',
    str_detect(services, 'IT') ~ 'Computer IT', 
    str_detect(services, 'Auditing') ~ 'Auditing', 
    str_detect(services, 'Payroll') ~ 'Payroll Services',
    str_detect(services, 'Tax ') ~ 'Tax reporting', 
    str_detect(services, 'Insurance') ~ 'Insurance',
    str_detect(services, 'Donation') ~ 'Donations',
    str_detect(services, 'donation') ~ 'Donations',
    str_detect(services, 'Human resource') ~ 'HR management',
    str_detect(services, 'development') ~ 'Org/Prog Development',
    TRUE ~ services)) |> 
  mutate(services = fct_lump_min(services, 44))

services_df |> count(services, sort = TRUE)

model_df <- dff2 |> 
  select(name, model) |> 
  separate_longer_delim(model, delim = '|') |> 
  mutate(model = case_when(
    str_detect(model, 'Model A') ~ 'Model A, Direct Project',
    str_detect(model, 'Model B') ~ 'Model B, Independent Contractor Project',
    str_detect(model, 'Model C') ~ 'Model C, Preapproved Grant Relationship',
    str_detect(model, 'Model D') ~ 'Model D, Group Exemption',
    str_detect(model, 'Model E') ~ 'Model E, Supporting Organization',
    str_detect(model, 'Model F') ~ 'Model F, Technical Assistance',
    str_detect(model, 'Model L') ~ 'Model L, Single Member LLC',
    str_detect(model, 'Unknown') ~ 'Unknown, Unknown',
    TRUE ~ model)) |> 
  mutate(model = fct_lump_min(model, 8, other_level = 'Other, Other')) |> 
  separate(model, c('model', 'defintion'), sep = ', ')

model_df |> count(model, sort = TRUE)


## joining projects, services  and models ----
df_joined <- dff2 |> 
  select(name, created, acted, total, fee, percent) |> 
  left_join(project_df, by = join_by(name)) |> 
  left_join(services_df, by = join_by(name), relationship = 'many-to-many') |> 
  left_join(model_df, by = join_by(name), relationship = 'many-to-many')


df_joined |> 
  select(fee, percent, model) |> 
  filter(!is.na(percent)) |> 
  view()

df_joined |> 
  group_by(model) |> 
  summarise(percent = mean(percent, na.rm = TRUE))

df_joined |> 
  group_by(name, project)

write_csv(df_joined, 'RDrafts/datasets/fiscal_data_w10.csv')

## Yearly created: decade -----
dff2 |> count(created) 
by_decade <- dff2 |> 
  mutate(decade_created = created %/% 10 * 10,
         decade_acted = acted %/% 10 * 10) |> 
  count(decade_acted, decade_created)

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
caption_font <- 'roboto'
axis_font <- 'roboto'

#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#

bg_col <- "white"
text_light <- "grey70"
text_dark <- "grey30"
text_col <- 'grey'
highlight_col <- ""
title_col <- "#bb3e03"
sub_color <- "white"



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
  <span style='color: {text_col}'>{xtwitter_username}</span>")

plot_caption <- paste0(
  "**Data**: TidyTuesday Week 11<br>", 
  "**Graphics:** ", 
  social_caption)


ptitle2 <- "FISCAL SPONSOR FACTS & STATS"
ptitle <- glue("<span style = 'color:", title_col, "'>FISCAL SPONSOR FACTS & STATS</span>")
psubtitle <- "Total projects sponsored by service type between 1903 and 2022"



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
      panel.grid.major.y = element_blank(),    
      panel.grid.minor = element_blank(),  
      panel.grid.major.x = element_line(color = 'grey20'),  
      
      plot.title = element_textbox_simple(
        family = title_font, 
        size = 3 * tsize,
        face = 'bold',
        hjust = 0.5,
        margin = margin(0,0,1,0, unit = 'pt')),

      plot.subtitle = element_textbox_simple(
        family = subt_font,
        size = 2.5 * tsize,
        color = sub_color,
        hjust = 0.5,
        margin = margin(0,0,1,0, unit = 'cm')),

      plot.caption =  element_textbox(
        family = caption_font,
        hjust = 1,
        color = 'grey60',
        size = 1.5 * tsize,
        lineheight = 0.3,
        margin = margin(.5,0,0,0, unit = 'cm')
        ),
      
      axis.text.y = element_text(
        family = axis_font, 
        face = 'bold',
        color = 'white',
        hjust = 1,
        size = 2 * tsize),
      axis.text.x = element_text(
        family = axis_font, 
        color = 'white',
        face = 'bold',
        size = 2 * tsize),
      
      plot.margin = margin(1,1,1,1, unit = 'cm')
    )
}

#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

## By  dacade ----
by_decade |> 
  pivot_longer(-n, names_to = 'event', values_to = 'decade') |> 
  mutate(event = str_remove_all(event, 'decade_') |> str_to_upper()) |> 
  ggplot(aes(as.character(decade), n)) +
  geom_col() +
  facet_wrap(~event, ncol = 1)


## Services ----
services_df |> 
  count(services) |> 
  ggplot(aes(n, fct_reorder(services, n))) +
  geom_col() +
  labs(x = NULL, y = NULL)
## by services joined -----
df_joined |> 
  group_by(name, services) |> 
  summarise(total = mean(total), .groups = 'drop') |> 
  group_by(services) |> 
  summarise(total = sum(total)) |> 
  mutate(clr = case_when(
    services %in% c(
      'Org/Prog Development', 'Donations', 'Bookkeeping') ~ 'catA',
    services == 'Other' ~ 'catB',
    TRUE ~ 'catC')) |> 
  ggplot(aes(total, fct_reorder(services, total), fill = clr)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c('#c4920d', 'grey', '#ccc241')) +
  labs(x = NULL, y = NULL,
       title = ptitle, subtitle = psubtitle,
       caption = plot_caption) +
  costum_theme()

#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

## save the final plot as 'final_plot' ----
ggsave(
  filename = file.path(
    "W11_FiscalSponsors",
    "plots_w11", 
    paste0("final_plot1", ".png"))
  )

## adjust size, background -----
ggsave(
  filename = here::here(
    "W11_FiscalSponsors", "plots_w11", "final_plot_w11.png"),
  width = 40, 
  height = 30, 
  units = "cm",
  bg = 'black'   #bg_col
)
