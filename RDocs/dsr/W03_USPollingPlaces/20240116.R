
# Load packages -----------------------------------------------------------
#remotes::install_github("vincentarelbundock/tinytable")
pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue
  )

pacman::p_load(g2r, reactable, tinytable, htmltools)
# https://g2r.opifex.org/articles/docs
# https://github.com/RProDigest/Tables/blob/main/The_Power_Of_Reactable.R
# https://vincentarelbundock.github.io/tinytable/vignettes/tutorial.html

# Loading data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-01-16")
dfr <- tuesdata$polling_places
dfr |> count(state, sort = TRUE) |> tail(10)
dfr |> filter(state == 'WA') |> view()
dfr |> glimpse()
dfs <- dfr |> 
  select(
    state, 
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
  legend_color(position = "right")


type_df |> 
  g2(asp(type, total, color = type)) %>% 
  fig_interval(lineWidth = 1, stroke = "#fff") %>% 
  coord_type("polar", innerRadius = .1) %>% 
  axis_hide() %>% 
  interplay("element", "highlight") %>% 
  legend_color(position = "right") %>% 
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

## Tables -----------

usa <- tibble(name = state.name, abbr = state.abb)

tab_pct <- dfs |> 
  count(state, name = 'total') |> 
  mutate(percent  =  round(total / sum(total)*100, 3)) |> 
  left_join(usa, by = join_by(state == abbr)) |> 
  select(name, everything()) |> 
  rename_all(toupper)
tab <- dfs |> 
  count(state, name = 'total') |> 
  mutate(percent  =  total / sum(total)) |> 
  left_join(usa, by = join_by(state == abbr)) |> 
  select(name, everything()) |> 
  rename_all(toupper) |> 
  mutate(BAR = round(PERCENT * 100, 1),
         NAME = str_to_upper(NAME))

###   basic reactable ----
tab_pct |> 
  reactable(
    theme = superhero(),
    columns = list(
      PERCENT = colDef(
        format = colFormat(suffix = "%", 
                           separators = TRUE, 
                           digits = 2))
    ))


tab_pct |> 
  select(NAME, PERCENT) |> 
  reactable(
    defaultColDef = colDef(
      cell = data_bars(tab_pct,
                       text_position = "outside-end")))
tab_pct |> 
  select(NAME, PERCENT) |> 
  mutate(PERCENT = round(PERCENT, 4)) |> 
  reactable(
    defaultColDef = colDef(
      cell = data_bars(
        tab_pct,
        fill_color = c("lightblue","royalblue","navy"),
        fill_gradient = TRUE,
        text_position = "outside-end")))

### Table with bars ------
# library(htmltools)

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
  bar <- div(
    style = list(background = fill, width = width, height = height))
  chart <- div(
    style = list(
      flexGrow = 1, 
      marginLeft = "0.5rem", 
      background = background), 
    bar)
  div(
    style = list(display = "flex", alignItems = "center"), 
    label, 
    chart)
}

bar_style <- function(width = 1, fill = "#e6e6e6", height = "75%", align = c("left", "right"), color = NULL) {
  align <- match.arg(align)
  if (align == "left") {
    position <- paste0(width * 100, "%")
    image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
  } else {
    position <- paste0(100 - width * 100, "%")
    image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
  }
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = color
  )
}

tab |> 
  reactable(
    theme = nytimes(header_font_size = 20),
    searchable = TRUE,
    defaultPageSize = 15,
    sortable = TRUE,
    compact = TRUE,
    columns = (
      list(
        NAME = colDef(
          width = 150),
        PERCENT = colDef(
          align = 'right',
          format = colFormat(percent = TRUE, digits = 2)
        ),
        TOTAL = colDef(
          width = 150,
          style = function(value) {
            bar_style(
              width = value / max(tab$TOTAL), 
              fill = "hsl(208, 70%, 90%)")
          }),
        BAR = colDef(
          name = '',
          align = "right",
          cell = function(value) {
            bar_chart(
              label = '',
              width = value / max(tab$BAR) * 100, 
              fill = "#fc5185",
              background = "#e1e1e1"
              )})
      ))
  ) |> 
  add_title(title = 'US Polling Places') |>
  add_subtitle(
    subtitle = '2012-2020',
    font_size = 20,
    font_color = '#1666BC',
    margin = margin(t = 10,r = 0,b = 15,l = 0)) |>  
  add_source(
    source = 'TidyTuesday 2024-W3 | The Center for Public Integrity',
    font_style = 'italic',
    font_weight = 'bold',
    font_color = '#666666'
  ) |> 
  google_font("Roboto Mono", font_weight = 500, font_style = "italic")


# Saving Plots and Gifs ------------------------------------------------------

# save the final plot as 'final_plot'
ggsave(
  filename = file.path(
    "W3_USPollingPlaces",
    "plots_w3", 
    paste0("final_plot", ".png"))
  )
