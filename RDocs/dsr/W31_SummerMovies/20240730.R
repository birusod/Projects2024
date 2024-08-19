#=============================================================#
# Load packages ----------------------------------------------
#=============================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes,              
  showtext, extrafont, ggtext, ggtext, glue, gridtext,
  patchwork, ggview,
  ggstream, seecolor, paletteer
)
source('RDrafts/myfunc.R')
movies |> missing_details() 
movies |> filter(is.na(genres))
#=============================================================#
# Loading data ------------------------------------------------
#=============================================================#

tuesdata <- tidytuesdayR::tt_load("2024-07-30")
#tuesdata
genres <- tuesdata$summer_movie_genres
movies <- tuesdata$summer_movies

genres |> nrow()
movies |> head(10) |> view()
  select(genres) |> 
  separate_rows(genres, sep = ',') |> 
  nrow()


#=============================================================#
# Wrangling Data ---------------------------------------------
#=============================================================#

df <- 
  movies |> 
  separate_rows(genres, sep = ',') |> 
  select(-c(original_title, simple_title)) |> 
  mutate(decade = as.character(year %/% 10 * 10))



## Rating and votes -----
df |> 
  ggplot(aes(average_rating)) +
  geom_histogram(bins = 20, color = 'grey') +
  theme_light()

df |> 
  ggplot(aes(num_votes)) +
  geom_histogram(bins = 20, color = 'grey') +
  theme_light()

df |> 
  ggplot(aes(average_rating, num_votes)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10(breaks = c(100, 10000, 1000000),
                labels = c('0.1K', '10K', '1000K')) +
  labs(title = 'Summer Movies',
       subtitle = 'Relationship between rating and number of votes',
       x = 'Total votes',
       y = 'Average rating') +
  theme_light() 


## By year -----
df |> 
  filter(!is.na(year)) |> 
  count(year) |> 
  ggplot(aes(year, n)) +
  geom_col()

df |> 
  filter(!is.na(year),
         year >= 1950) |> 
  count(year, title_type) |> 
  ggplot(aes(year, n, fill = title_type)) +
  geom_col() +
  theme_light()

df |> 
  filter(!is.na(year)) |> 
  count(decade, title_type) |> 
  ggplot(aes(decade, n, fill = title_type)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  guides(
    fill  = guide_legend(position = "inside")) +
  labs(title = 'Summer Movies',
       subtitle = 'Distribution of movies types by decade',
       x = NULL,
       fill = 'Types',
       y = 'Frequency') +
  theme_light() +
  theme(legend.position.inside = c(.2, .8),
        legend.background = element_blank())


## runtime -----
df |> 
  ggplot(aes(runtime_minutes)) +
  geom_histogram()


df |> 
  drop_na() |> 
  filter(title_type == 'movie') |> 
  ggplot(aes(runtime_minutes, average_rating)) +
  geom_point()


## Genres -----
movies |> filter(is.na(genres))
df |> filter(is.na(genres))

df |> 
  ggplot(aes(genres)) +
  geom_bar()

genres <- df |> 
  filter(!is.na(genres)) |> 
  mutate(genres = fct_infreq(genres)) |> 
  count(genres, name = 'total') |> 
  mutate(genres = fct_lump_n(genres, w = total, n = 21)) |> 
  mutate(genres = fct_reorder(genres, total)) 

genres |> tail()

genres  |> 
  ggplot(aes(total, genres)) +
  geom_col()

df |> 
  drop_na() |> 
  mutate(genres = fct_infreq(genres)) |> 
  count(year, genres, name = 'total') |> 
  ggplot(aes(year, total, fill = genres, group = genres)) +
  geom_area()

df |> 
  drop_na() |> 
  mutate(genres = fct_infreq(genres)) |> 
  count(decade, genres, name = 'total') |> 
  ggplot(aes(decade, total, color = genres, group = genres)) +
  geom_line()

df |> 
  drop_na() |> 
  mutate(genres = fct_infreq(genres)) |> 
  count(decade, genres, name = 'total') |> 
  ggplot(aes(decade, total, fill = genres, group = genres)) +
  geom_area()


df |> 
  drop_na() |> 
  mutate(genres = fct_infreq(genres)) |> 
  count(decade, genres, name = 'total') |> 
  mutate(genres = fct_lump_n(genres, w = total, n = 5)) |> 
  ggplot(aes(decade, total, fill = genres, group = genres)) +
  geom_area()

df |> 
  drop_na() |> 
  mutate(genres = fct_infreq(genres)) |> 
  count(decade, genres, name = 'total') |> 
  mutate(genres = fct_lump_n(genres, w = total, n = 5)) |> 
  ggplot(aes(decade, total, fill = genres, group = genres)) +
  geom_col()


## genres by decades ------

gdf <- df |> 
  filter(year >= 1940) |> 
  drop_na() |> 
  mutate(genres = fct_infreq(genres)) |> 
  mutate(genres = fct_lump(genres, n = 6)) |> 
  count(decade, genres, name = 'total')


#=============================================================#
# Loading fonts ----------------------------------------------
#=============================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")
font_add_google("Bangers", "bangers")  # title
font_add_google("Saira Extra Condensed", "saira") # caption

font_add_google("Martel Sans",
                family = "martel",
                regular.wt = 300,
                bold.wt = 900)  #  text

#f1b <- "Publico Headline Bold"


#font_families()
showtext_auto()


#=============================================================#
# Defining colors --------------------------------------------
#=============================================================#
# Credits for coffeee palette & aditya dahiya
mypal <- c(paletteer::paletteer_d("MoMAColors::Panton"), "grey70")
mypal |> seecolor::print_color()
bg_color    <- "white"
text_light  <- ""
text_dark   <- ""
highlight   <- ""
title_color <- ""
sub_color   <- ""
cap_color   <- "grey95"




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
  "**Data**:TidyTuesday Week 31<br>", 
  social_caption)


ptitle    <- "SUMMER MOVIES"
psubtitle <- "Clear Dominance Of Drama, Comedy & Romance"


#=============================================================#
# Custom Theme------------------------------------------------
#=============================================================#

costum_theme <- function(){ 
  cfont <- "roboto"   
  theme_void() %+replace%    
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "#555665", color = NA),
      panel.grid.major.x = element_line(linewidth = 0.1, color = 'grey90'),
      axis.text.x = element_text(
        family = 'roboto', color = 'grey90', 
        margin = margin(10, 0, 0, 0), 
        size = 30),
      axis.text.y = element_text(
        family = 'roboto', color = 'grey90', 
        size = 20),
      plot.margin = margin(20, 20, 20, 20),
      plot.caption = element_textbox_simple(
        color = cap_color,
        size = 12, family = 'martel',
        halign = 1,
        lineheight = .5,
        margin = margin(t = 20)),
      plot.title = element_text(size = 40, family = 'bangers', color = '#FFFDD0'),
      plot.subtitle = element_text(
        size = 40, family = 'saira', color = '#ededdf',
        margin = margin(t = 5, b = 10))
    )
}




#===========================================================#
# Data Viz -------------------------------------------------
#===========================================================#

g <- gdf |> 
  mutate(decade = parse_integer(decade)) |> 
  ggplot(aes(x = decade, y = total, fill = genres)) +
  geom_stream(lwd = 0.25, bw = .85) +  # n_grid = 180
  geom_stream_label(aes(label = genres, ), color = 'white', 
                    size = 8, family = 'bangers' ) +
  scale_fill_manual(values = mypal) +
  labs(title = ptitle,
       subtitle = psubtitle,
       caption = plot_caption) +
  costum_theme()

g


ggview(units = 'cm', width = 10, height = 10)



#==========================================================#
# Saving Plots and Gifs -----------------------------------
#==========================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W31_SummerMovies",
                       "plots_w31", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W31_SummerMovies",
                        "plots_w31", 
                        paste0("final_plot2", ".png")),
  width    = 10, 
  height   = 10, 
  units    = "cm",
  dpi = 300
)

getwd()
ragg::agg_png(
  filename = here::here("W31_SummerMovies",
                        "plots_w31", 
                        paste0("final_plot", ".png")), 
  width = 5, 
  height = 5, 
  units = "in", 
  res = 300,
  scaling = 2)
g
dev.off()
