
#=========================================================================#
# Load packages -----------------------------------------------------------
#=========================================================================#
pacman::p_load(
  here, tidytuesdayR, tidyverse, janitor, scales, fontawesome,
  showtext, patchwork, ggtext, glue, ggthemes, magick, 
  leaflet, sf,
  usmap, usmapdata, 
  rnaturalearth, rnaturalearthdata)

#=========================================================================#
# Loading data -------------------------------------------------------------
#=========================================================================#
tuesdata <- tidytuesdayR::tt_load("2024-01-30")
raw <- tuesdata$groundhogs
raw |> head(10) |> view()
#summarytools::dfSummary(raw) |> view()

pred2023 <- tuesdata$predictions |> 
  filter(year == 2023) 


#=========================================================================#
# Loading fonts ------------------------------------------------------------
#=========================================================================#
font_add_google("Roboto", "roboto")
font_add_google("Fira code", "firac")
font_add_google("Fira sans", "firas")
font_add_google("Alfa Slab One", family = "alfaslab")          # title_font 
font_add_google("Saira Extra Condensed", family = "sairacond") # caption_font
font_add_google("Righteous", family = "righteous")             # body_font
showtext_auto()

#=========================================================================#
# Defining colors ---------------------------------------------------------
#=========================================================================#
bg_col <- "#ebfaff"
text_col <- "#4f2b00"
text_hil <- "#c46c00"

gpal <- c("#0ab6f0", "#00990a")


#=========================================================================#
# Wrangling Data ----------------------------------------------------------
#=========================================================================#

duplicated(raw$name) |> sum()

raw |> glimpse()
raw |> count(city)
df <- raw |> 
  select(name, city, region, country, latitude, longitude, is_groundhog) |> 
  mutate(cnt = 1) |> 
  group_by(region) |> 
  mutate(total = sum(cnt)) |> 
  ungroup() |> 
  select(-cnt)

# credit: @AdityaDahiyaIAS
# https://github.com/Aditya-Dahiya/projects_presentations/blob/main/projects/tidy_groundhog.R
pred2023_ids <- pred2023 |> 
  pull(id) |> 
  unique()
df2 <- raw |> 
  filter(id %in% pred2023_ids) |> 
  select(id, name, latitude, longitude, image, region) |> 
  left_join(pred2023) |> 
  mutate(predict = if_else(shadow,
                           "Groundhog saw its shadow: Extended Winters",
                           "No shadow: An Early Spring!")) |> 
  select(-c(shadow, details, year)) |> 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) |> 
  st_transform(crs = 5070) |> 
  drop_na()
image1 <- image_read("https://img.freepik.com/free-vector/adorable-groundhog-cartoon-with-groundhog-day-banner_1308-153480.jpg")


# regions  -----
regions <- df |> 
  mutate(region =  fct_lump_min(region, 3)) |> 
  count(region, sort = TRUE, name = 'total')


# US Canada map
# Getting a map of USA and Canada
northamerica <- 
  ne_countries(scale = "large", 
               returnclass = "sf",
               continent = "North America") |> 
  filter(sovereignt %in% c("Canada", "United States of America")) |> 
  filter(gu_a3 %in% c("USA", "CAN")) |> 
  st_transform(crs = 5070)

ggplot() +
  geom_sf(data = northamerica, fill = "white")

# africa
africa <- 
  ne_countries(scale = "large", 
               returnclass = "sf",
               continent = "Africa") |> 
  st_transform()
africa |> 
  ggplot() +
  geom_sf(fill = "white") +
  ggthemes::theme_map()
df2

#=========================================================================#
# Define texts and annotations --------------------------------------------
#=========================================================================#

tsize = unit(30, units = "cm")    # text  size

# social <- nrBrand::social_caption(
#   bg_colour = bg_col,
#   icon_colour = highlight_col,
#   font_colour = text_col,
#   font_family = "roboto"
# )

# Caption stuff
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = here::here(
    "RDrafts", "fontBrands", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "birusod"
xtwitter <- "&#xe61b"
xtwitter_username <- "@DSbyKnight"
social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> 
  <span style='color: {text_col}'>{github_username}  </span> 
  <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> 
  <span style='color: {text_col}'>{xtwitter_username}</span>"
  )
plot_caption <- paste0("**Data**: TidyTuesday | Week5<br>", "**Graphics:** ", social_caption)
ptitle <- "Groundhog Day: 2023"
st_text <- "Predictions of an early Spring! A map of North America showing the locations of Groundhogs and their predictions in 2023."
plot_subtitle <- paste(strwrap(st_text, 80), collapse = "\n")

gtitle <- "Groundhog Day: 2024"
gst_text <- paste0(
  "Distribution Of Groudhog Event Locations In ",
  "<b><span style='color:#d62728;'>The United States</span></b>",
  " and ",
  "<b><span style='color:#005f73;'>Canada</span></b>.")
gplot_subtitle <- paste(strwrap(gst_text, 80), collapse = "\n")


#=========================================================================#
# Data Viz ----------------------------------------------------------------
#=========================================================================#
df |> 
  ggplot(aes(longitude, latitude)) +
  geom_point()

# maps  ----
# usa
#library(mapdata)
usa <- map_data('usa')
state <- map_data("state")

leaflet(df) %>% 
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
             radius = ~total
  )


usa |> 
  ggplot() + 
  geom_polygon(aes(long, lat, group = group),
               fill = 'lightblue', color = 'black') + 
  geom_point(data = df, 
             aes(x = longitude, y = latitude, size = total),
             alpha = .4) +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank()) + 
  ggtitle('Groundhog Day 2024: Count By State') + 
  coord_fixed(1.3) +
  theme_map()

state |> 
  ggplot() + 
  geom_polygon(aes(long, lat, group = group),
               fill = 'lightblue', color = 'black') + 
  geom_point(data = df, 
             aes(x = longitude, y = latitude, size = total),
             alpha = .4) +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank()) + 
  ggtitle('U.S. Map') + 
  coord_fixed(1.3) +
  theme_map()

regions |> 
  mutate(region = fct_reorder(region, total)) |> 
  ggplot(aes(total,region)) +
  geom_col(fill = text_hil, alpha = .8) +
  labs(
    x = 'Total', y = NULL,
    title = gtitle,
    subtitle = gplot_subtitle,
    caption = plot_caption) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 40))  +
  theme_light() +
  theme(
    plot.title = element_text(
      family = title_font, hjust = 0.5,
      color = text_hil, size = 4 * tsize,
      face = "bold", margin = margin(0,0,0.2,0, unit = "cm")),
    plot.subtitle    = element_textbox(
      family = subt_font, 
      hjust = 0.5,
      color = text_col, size = 3 * tsize,
      margin = margin(0,0,1,0, unit = "cm"),
      lineheight = 0.8),
    axis.text = element_text(
      family = 'righteous', size = 2 * tsize, color = 'grey30'),
    plot.caption = element_textbox(
      family = "roboto", hjust = 0.5, 
      color = 'grey70', size = 1 *  tsize,
      lineheight = 0.35),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(1, 1, 1, 1, unit = 'cm')
  )

title_font <- 'alfaslab'
subt_font <- 'sairacond'
caption_font <- 'righteous'

g1 <- ggplot() +
  geom_sf(data = northamerica, fill = "white") +
  geom_sf(data = df2,
          aes(color = predict),
          size = 5, alpha = 0.5) +
  labs(x = NULL, y = NULL, size = NULL, color = NULL,
    title = ptitle,
    subtitle = plot_subtitle,
    caption = plot_caption) +
  scale_colour_manual(values = gpal) +
  guides(
    colour = guide_legend(
      override.aes = list(size = 8))) +
  ggthemes::theme_map() +
  theme(
    legend.position = c(0.1, .6),
    plot.caption =  element_textbox(
      family = caption_font, hjust = 0.5, 
      color = 'grey70', size = 1.5 * tsize,
      lineheight = 0.35),
    plot.title = element_text(
      family = title_font, hjust = 0.5,
      color = text_hil, size = 5 * tsize,
      face = "bold", margin = margin(0,0,0.5,0, unit = "cm")),
    plot.subtitle    = element_text(
      family = subt_font, hjust = 0.5,
      color = text_col, size = 3 * tsize,
      margin = margin(0,0,0,0, unit = "cm"),
      lineheight = 0.35),
    plot.background =  element_rect(fill = bg_col, colour = bg_col, linewidth = 0),
    plot.title.position = "plot",
    legend.text = element_text(
      family = subt_font, hjust = 0, face = 'bold',
      colour = text_col, size = 1.5 * tsize,
      margin = margin(0,0,0,0)),
    legend.key = element_rect(fill = bg_col),
    legend.background = element_rect(fill = bg_col),
    legend.box = "horizontal"
  )

g1

#=========================================================================#
# Saving Plots and Gifs ---------------------------------------------------
#=========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path(
    "W5_groundhogs",
    "plots_w5", 
    paste0("final2_plot_w5", ".jpeg")),
  width = 36, 
  height = 26, 
  units = "cm"
  )

ggsave(
  filename = here::here("W5_groundhogs", "plots_w5", "tidy_groundhog2.png"),
  plot = g1,
  width = 40, 
  height = 30, 
  units = "cm",
  bg = bg_col
)
