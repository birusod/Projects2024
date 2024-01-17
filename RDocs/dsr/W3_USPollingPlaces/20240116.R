
# Load packages -----------------------------------------------------------
#remotes::install_github("vincentarelbundock/tinytable")
pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue
  )

pacman::p_load(g2r, reactable, tinytable)
# https://g2r.opifex.org/articles/docs
# https://github.com/RProDigest/Tables/blob/main/The_Power_Of_Reactable.R
# https://vincentarelbundock.github.io/tinytable/vignettes/tutorial.html

# Loading data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-01-16")
dfr <- tuesdata$polling_places
dfr |> count(state, sort = TRUE) |> tail(10)
dfr |> filter(state == 'WA') |> view()

dfs <- dfr |> 
  select(
    name, 
    precinct = precinct_name,
    date = election_date,
    state, jurisdiction, 
    jtype = jurisdiction_type,
    county = county_name,
    type = location_type) |> 
  mutate(
    name = str_to_upper(name),
    jurisdiction = str_to_upper(jurisdiction),
    year = year(date))
  
dfs |> count(year)
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

## By year ----
by_year <- 
  dfs |> 
  count(year, name = 'total')


## state and year----

by_year_state <- dfs |> 
  mutate(state = fct_infreq(state) |> fct_rev()) |> 
  group_by(state, year) |> 
  tally(name  = 'total')  |> 
  ungroup()

##  By jurisdiction type -----
jtype_df <- dfs |> 
  filter(!is.na(jtype)) |> 
  mutate(jtype = fct_infreq(jtype)) |> 
  count(jtype, name = 'total') |> 
  mutate(jtype = case_when(
    jtype == 'county_municipality'~ 'cm',
    TRUE ~ jtype
  ))


## Early vote ------
dfs |> 
  filter(str_detect(type, 'early_vote')) |> 
  count(year, state)
  

## By type -------
type_df <- dfs |> 
  filter(!is.na(type)) |> 
  mutate(type = case_when(
    type == 'early_vote_site' ~ 'Early vote',
    type == 'early_vote' ~ 'Early vote',
    TRUE ~ type)) |> 
  mutate(type = str_replace_all(type, '_', ' ') |> str_to_title()) |> 
  count(type, name = 'total')
# Define texts and annotations --------------------------------------------

# social <- nrBrand::social_caption(
#   bg_colour = bg_col,
#   icon_colour = highlight_col,
#   font_colour = text_col,
#   font_family = "roboto"
# )
title <- ""
st <- ""
cap <- paste0(
  "**Data**: <br>", #social
)


# Data Viz -------------------------------------------------------------------

# year by state
by_year |> 
  ggplot(aes(factor(year), total)) +
  geom_col(fill = 'grey50')

# g2r
by_year |> 
  mutate(year = factor(year)) |> 
  g2(asp(year, total)) %>% 
  fig_interval()



by_year_state |> 
  ggplot(aes(total, state, fill = factor(year))) +
  geom_col()
# g2r
by_year_state |> 
  mutate(year = factor(year)) |> 
  g2(asp(state, total, color = year)) %>% 
  fig_interval()


## By jtype ----
jtype_df |> 
  mutate(jtype = fct_reorder(str_to_upper(jtype), total)) |> 
  ggplot(aes(total, jtype)) +
  geom_col() +
  scale_x_continuous(labels = comma) +
  labs(x = NULL, y = NULL)

jtype_df |> 
  mutate(jtype = fct_reorder(str_to_upper(jtype), total)) |> 
  g2(asp(jtype, total, color = jtype)) %>% 
  fig_interval()

jtype_df |> 
  g2(asp(y = total, color = jtype, label = jtype)) %>% 
  fig_pie()

## By type:

type_df |> 
  ggplot(aes(type, total)) +
  geom_col()

type_df |>
  g2(asp(type, total)) %>% 
  fig_interval(asp(label = total), fill = "#8adb9a") |> 
  info_text(
    asp(x = 0, y = 110000),
    content = "TITLE")

type_df |> 
  g2(asp(y = total, color = type)) %>% 
  fig_pie(
    asp(label = total),
    lineWidth = 1, stroke = "#fff") %>% 
  legend_color(position = "right") %>% 
  tooltip(showMarkers = FALSE)


type_df |> 
  g2(asp(type, total, color = type)) %>% 
  fig_interval(lineWidth = 1, stroke = "#fff") %>% 
  coord_type("polar", innerRadius = .1) %>% 
  axis_hide() %>% 
  interplay("element", "highlight") %>% 
  legend_color(position = "right") %>% 
  tooltip(showMarkers = FALSE) |> 
  info_text(
    asp(x = 1, y = 110000),
    content = "Distribution of\nPolling Location Types")

## Radial Bar
type_df %>% 
  g2(asp(type, total, color = total)) %>% 
  fig_interval() %>% 
  coord_type("polar", innerRadius = .2) %>% 
  coord_transpose() %>% 
  gauge_color(c("#BAE7FF", "#1890FF", "#0050B3")) %>% 
  gauge_y_linear(max = 20) %>% 
  axis_x(
    tickLine = NULL,
    grid = NULL,
    line = NULL,
    label = list(offset = 20)
  ) %>% 
  legend_color(FALSE)

type_df2 <- type_df |> 
  arrange(total) |> 
  mutate(id = row_number(),
         type2 = paste0('G', id, ' ', type))

type_df2 %>% 
  g2(asp(id, total, color = type2)) %>% 
  fig_interval() %>% 
  coord_type("polar", innerRadius = .2) %>% 
  coord_transpose() %>% 
  axis_x(
    tickLine = NULL,
    grid = NULL,
    line = NULL) |> 
  legend_color(position = "left")



# Saving Plots and Gifs ------------------------------------------------------

# save the final plot as 'final_plot'
ggsave(
  filename = file.path(
    "W3_USPollingPlaces",
    "plots_w3", 
    paste0("final_plot", ".png"))
  )
