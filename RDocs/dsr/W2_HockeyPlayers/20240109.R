
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue, eeptools, gg.layers)


# Loading data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-01-09")
df <- 
  tuesdata$nhl_rosters |> 
  select(id        = player_id, 
         code      = team_code, 
         season, 
         position  = position_type,
         first     = first_name, 
         last      = last_name, 
         catch     = shoots_catches,
         height    = height_in_centimeters, weight = weight_in_kilograms,
         dob       = birth_date, 
         city      = birth_city,
         country   = birth_country,
         province  = birth_state_province) |> 
  left_join(
    tuesdata$nhl_teams |> 
      select(
        code       = team_code,
        name       = full_name
      ),
    by = join_by(code)
  )


# Loading fonts ------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Fira code", "firac")
font_add_google("Fira sans", "firas")
showtext_auto()


# Defining colors ---------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


# Wrangling Data ----------------------------------------------------------

## glimpse -----
df |> nrow()
df |> colnames()
df |> glimpse()
df |> head(20) |> view()

## check  for  NA by columns ------
colSums(is.na(df))
colMeans(is.na(df))

df |> summarise_all(~ sum(is.na(.)))
df |> summarise(across(everything(), ~ sum(is.na(.))))
df |> summarise(across(everything(), ~ mean(is.na(.))))
df |> summarise(across(everything(), \(x) mean(is.na(x))))


# code snippet from Statistics Globe [Joachim Schork]
# https://www.youtube.com/watch?v=wMF6OJZB9Rg
func_insert <- function(x, pos, char){
  gsub(
    paste0("^(.{", pos, "})(.*)$"),
    paste0("\\1", char, "\\2"),
    x)
}

func_update_season <- function(x){
  gsub(
    paste0("^(.{4})(.*)$"),
    paste0("\\1", "-", "\\2"),
    x)
}
df$season[1]
func_insert(df$season[1], 4, '-')
func_update_season(df$season[1])

df |> count(season)
substr(df$season[1], start = 1, stop = 4)

df |> 
  select(season) |> 
  mutate(start  = substr(season, start = 1, stop = 4),
         end  = substr(season, start = 5, stop = 8))

eu_countries <- c(
  'AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 
  'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
  'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
  'SVN', 'ESP', 'SWE', 'GBR')

## Final df -----
dff <- df |> 
  mutate(
    start  = substr(season, start = 1, stop = 4) |> parse_integer(),
    end  = substr(season, start = 5, stop = 8) |> parse_integer(),
    year = year(dob)) |> 
  mutate(group = case_when(
    country %in% eu_countries ~ 'EUROPE',
    country == 'CAN' ~ 'CANADA',
    country == 'USA' ~ 'UNITED STATES',
    TRUE ~ 'OTHER'
  ))


## new data to retain only player at their first season ----
# and remove duplicate rows
#age = round(eeptools::age_calc(dob, today(), units =  'years')),
#age2 = parse_integer(start) - year(dob))

dfn <- 
  dff |>
  arrange(id, first) |> 
  group_by(id) |>
  mutate(
    start = min(start),
    end = max(end),
    yearspan = end - start) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  mutate(age = start - year(dob)) |> 
  drop_na(age, height, weight)
  
dfn |> 
  filter(first == 'Bryan', last == 'Hextall') |> 
  view() # sanity check!

## Country ------
dff |> count(country)
top10_countries_not_CAN <- dfn |> 
  filter(country != 'CAN') |> 
  mutate(country  = fct_lump_n(
    country,  n = 9, #ties.method = 'first',
    other_level = 'OTHER') |> fct_infreq() |> fct_rev()) |> 
  count(country, sort = TRUE, name = 'total')


group_freq <- dfn |> 
  count(group, sort = TRUE, name = 'total')


## City and Province ------

dfn |> count(city, sort = TRUE)
dfn |> count(province, sort = TRUE)

## age, height and weight ------
dfn |> 
  select(height, weight, age) |> 
  psych::describe()



### By group ------
avg_by_group <- dfn |> 
  group_by(group) |> 
  summarise(
    across(
      .cols = c(age, height, weight),
      .fns = mean
    ))



### By position -----

dfn |> 
  count(position)

avg_by_pos <- dfn |> 
  group_by(position) |> 
  summarise(
    across(
      .cols = c(age, height, weight),
      .fns = mean
    ))


# Define texts and annotations --------------------------------------------

# social <- nrBrand::social_caption(
#   bg_colour = bg_col,
#   icon_colour = highlight_col,
#   font_colour = text_col,
#   font_family = "roboto"
# )
# title <- ""
# st <- ""
# cap <- paste0(
#   "**Data**: <br>", #social
# )


# Data Viz -------------------------------------------------------------------

## Country ------

top10_countries_not_CAN |> 
  ggplot(aes(total, country)) +
  geom_col() +
  labs(x = NULL, y = NULL,
       title = 'Players By Country',
       subtitle = 'Top 10 countries by total')

group_freq |>
  ggplot(aes(total, fct_reorder(group, total), fill = group)) +
  geom_col() +
  labs(x = NULL, y = NULL,
       title = 'Players By Group',
       subtitle = 'Comparing Canada, USA, EU and Rest of the world')


## age, height and weight ------
dfn |> 
  select(height, weight, age, yearspan) |> 
  pivot_longer(
    everything(), 
    names_to = 'measure', 
    values_to = 'values') |> 
  ggplot(aes(values)) +
  geom_histogram() +
  facet_wrap(~measure, scales = 'free') +
  labs(
    x = NULL, y = NULL,
    title = 'Players Attributes',
    subtitle = 'Distribution of Weight, Height, Age and Carrier span')

dfn |> 
  select(height, weight) |> 
  ggplot(aes(height, weight)) +
  geom_point() +
  geom_smooth() +
  labs(x = NULL, y = NULL,
       title = 'Players Attributes',
       subtitle = 'Relation between Height and Weight')

dfn |> 
  select(age, weight) |> 
  ggplot(aes(age, weight)) +
  geom_point() +
  geom_smooth() +
  labs(x = NULL, y = NULL,
       title = 'Players Attributes',
       subtitle = 'Relation between Age and Weight')


## By group ------

viz_by_cat <- function(df, cat, value){
  df |> 
    filter(!is.na({{value}})) |> 
    ggplot(aes({{cat}}, {{value}})) +
    geom_boxplot() +
    geom_violin(aes(fill = {{cat}}), alpha = .3, color = NA) +
    theme_light()
}

dfn |> viz_by_cat(group, age) + 
  labs(x = NULL, y = NULL, title = 'Players By Group',
       subtitle = 'Age distribution')
dff |> viz_by_cat(group, height) + 
  labs(x = NULL, y = NULL, title = 'Players By Group',
       subtitle = 'Height distribution')
dff |> viz_by_cat(group, weight) + 
  labs(x = NULL, y = NULL, title = 'Players By Group',
       subtitle = 'Weight distribution')


## By position ------
dfn |> viz_by_cat(position, age) + 
  labs(x = NULL, y = NULL, title = 'Players By Position',
       subtitle = 'Age distribution')
dfn |> viz_by_cat(position, height) + 
  labs(x = NULL, y = NULL, title = 'Players By Position',
       subtitle = 'Height distribution')
dfn |> viz_by_cat(position, weight) + 
  labs(x = NULL, y = NULL, title = 'Players By Position',
       subtitle = 'Weight distribution')


dfn |> 
  ggplot(aes(position, age)) +
  geom_boxplot2(width.errorbar = 0.3) +
  geom_violin(aes(fill = position), alpha = .3, color = NA) +
  theme_light()


# Saving Plots and Gifs ------------------------------------------------------

# save the final plot as 'final_plot'
ggsave(
  filename = file.path(
    "W2_HockeyPlayers",
    "plots_w2", 
    paste0("final_plot", ".png"))
  )
