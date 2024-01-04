
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue)


# Loading data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-14")
df <- tuesdata$diwali_sales_data

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

df |> 
  count(Zone, sort = TRUE)


by_zone <- df |> 
  mutate(Zone = fct_infreq(Zone)) |>
  count(Zone, name = 'Total')
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

by_zone |> 
  ggplot(aes(Zone, Total, fill = Zone)) +
  geom_col() +
  theme_wsj()


# Saving Plots and Gifs ------------------------------------------------------

# save the final plot as 'final_plot'
ggsave(
  filename = file.path(
    "W46_DiwaliSales",
    "plots_w46", 
    paste0("final_plot", ".png"))
  )
