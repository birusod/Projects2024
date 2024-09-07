#=============================================================#
# Load packages ----------------------------------------------
#=============================================================#
devtools::install_github('davidgohel/ggiraph')
pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes,              
  showtext, extrafont, ggtext, ggtext, glue,
  patchwork, ggview
)

pacman::p_load(ggflags, ggiraph, htmlwidgets, htmltools)

source('RDrafts/myfunc.R')
#dfr |> missing_details() 
#=============================================================#
# Loading data ------------------------------------------------
#=============================================================#

tuesdata <- tidytuesdayR::tt_load("2024-08-13")
dfr <- tuesdata$worlds_fairs |> 
  mutate(
    start_date = dmy(paste(1, start_month, start_year, sep = '/')),
    end_date = dmy(paste(1, end_month, end_year, sep = '/'))) |> 
  drop_na()
  
dfr


# country codes
# https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv
cc <- read_csv('https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv') |> clean_names()
cc |> head() 
ccc <- 
  cc |> 
  select(name, alpha_2, alpha_3, country_code, region, sub_region) 
#=============================================================#
# Wrangling Data ---------------------------------------------
#=============================================================#
dfr |> sample_n(20)
dfr |> colnames()

dfr |> count(start_year)
dfr |> count(country)
dfr |> count(category)
dfr |> count(city)

dfr |> filter(start_year >= 1937, start_year <= 1957)
dfr |> 
  mutate(start_year = as.character(start_year)) |> 
  ggplot(aes(start_year, attending_countries, group = 1)) +
  geom_line()

dfr |> 
  ggplot(aes(start_date, cost)) +
  geom_line() +
  scale_y_log10() +
  labs(y = 'costs in Millions USD')

dfr |> 
  ggplot(aes(start_date, visitors)) +
  geom_line() +
  labs(y = 'visitors in Millions')

dfr |> 
  summarise(avg = mean(cost), .by = city) |> 
  mutate(city = fct_reorder(city, avg)) |> 
  ggplot(aes(avg, city)) +
  geom_col()

plot_average_by_category <- function(data, value, category, 
                                     xlabel = NULL, subtitle = NULL) {
  data |> 
    # select({{value}}, {{category}}) |> 
    # rename_with(~c('val', 'cat')) |> 
    # summarise(avg = mean(val), .by = cat) |> 
    # mutate(cat = fct_reorder(cat, avg)) |>
    group_by({{category}}) |> 
    summarise(avg := mean({{value}})) |> 
    mutate(cat := fct_reorder({{category}}, avg)) |> 
    ggplot(aes(avg, cat)) +
    geom_col() +
    labs(
      x = xlabel, y = NULL,
      title = 'WORLD FAIRS',
      subtitle = subtitle
    ) +
    theme_light()
}
dfr |> plot_average_by_category(cost, country, 
                                subtitle = 'Average costs (USD) by country',
                                xlabel = 'Costs (USD)')
dfr |> plot_average_by_category(cost, city, 
                                subtitle = 'Average costs (USD) by city',
                                xlabel = 'Costs (USD)')

dfr |> plot_average_by_category(cost, category, 
                                subtitle = 'Average costs (USD) by category',
                                xlabel = 'Costs (USD)')


dfr |> 
  ggplot(aes(area, cost)) +
  geom_point() +
  geom_text(aes(label = city))


dfr |> 
  mutate(year = as.character(start_year)) |> 
  ggplot(aes(year, visitors)) +
  geom_point(aes(size = visitors)) +
  geom_text(aes(label = city))

##***************************** ##
dfc <- dfr |> 
  select(start_year, country, city, category,visitors, name_of_exposition)

df <- dfc |> 
  mutate(country = case_when(
    country == 'United Kingdom' ~ 'United Kingdom of Great Britain and Northern Ireland',
    country == 'Colony of Victoria' ~ 'Australia',
    country == "People's Republic of China" ~ 'China',
    country == 'Austria-Hungary' ~ 'Austria', # Vienna
    country == 'United States' ~ 'United States of America',
    country == 'South Korea' ~ 'Korea, Republic of',
    TRUE ~ country)) |> 
  left_join(ccc, by = join_by(country == name)) |> 
  mutate(country = case_when(
    country == 'United Kingdom of Great Britain and Northern Ireland' ~ 'United Kingdom',
    country == 'United States of America' ~ 'United States',
    country == 'Korea, Republic of' ~ 'South Korea',
    TRUE ~ country)) |> 
  mutate(year = as.character(start_year),
         code = str_to_lower(alpha_2),
         city_year = paste0(city, year),
         tooltip = paste0( 
           "Country: ", country, "<br/>",
           "City: ", city, "<br/>", 
           "Year: ", year),
         label = glue(
           "{name_of_exposition}\nRegion: {region}\nSubregion: {sub_region}"))

#df |> head() |> view()


# https://www.ardata.fr/ggiraph-book/examples.html
# https://uncharteddata.netlify.app/posts/2022-09-30-interactive-tooltip-tables/
# https://alhdzsz.net/posts/mexico_subnational.html
# https://aditya-dahiya.github.io/projects_presentations/data_vizs/tidy_world_fairs.html
#=============================================================#
# Loading fonts ----------------------------------------------
#=============================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")
# Font for titles
font_add_google("Amita", "amita") 

# Font for the caption
font_add_google("Saira Extra Condensed", "sec") 

# Font for plot text
font_add_google("Ubuntu Condensed", "ubucond") 

font_add_google(
  name = "Send Flowers",
  family = "flor",
  db_cache = FALSE
)
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
cap_color   <- "grey70"




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
  "**Data**: TidyTuesday Week 33<br>", 
  "**Graphics:** ", 
  social_caption)


title    <- "WORLD'S FAIRS" 
subtitle <- "Total visitors throughout years of universal exhibitions"


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

#ggview(units = 'cm', width = 20, height = 18)
df |> 
  ggplot(aes(start_year, visitors)) +
  geom_flag(aes(country = code), size = 5) + 
  geom_point_interactive(
    aes(
      size = visitors,
      #fill = region,
      tooltip = tooltip, 
      data_id = start_year),
    alpha = 0.6,
    shape = 21) + 
  geom_text_interactive(
    data = df, 
    aes(x = 1860, 
        y = 60, 
        label = label,
        data_id = start_year),
    alpha = 0, 
    size = 2, 
    hjust = "left") -> g1

girafe(ggobj = g1,
       options = list(opts_sizing(rescale = FALSE))) |>  
  girafe_options(
    opts_tooltip(
      opacity = .9, 
      use_fill = TRUE,
      use_stroke = TRUE, 
      css = "padding:5pt;
            background-color:#333333;
            font-family: Open Sans;
            font-size:1rem;
            color:red;"),
    opts_hover_inv(css = "opacity:0.8"), 
    opts_toolbar(saveaspng = TRUE),
    opts_zoom(max = 1),
    opts_hover(
      css = girafe_css(
        css = glue("fill:orange;fill-opacity:1"),
        text = glue("stroke:none;fill:blue;fill-opacity:1;")
      ))
    )

df |> 
  ggplot(aes(start_year, visitors)) +
  geom_flag(aes(country = code), size = 5) + 
  geom_point_interactive(
    aes(
      size = visitors,
      #fill = region,
      tooltip = tooltip, 
      data_id = city_year),
    alpha = 0.6,
    shape = 21) + 
  geom_text_interactive(
    data = df, 
    aes(x = 1860, 
        y = 70, 
        label = label,
        data_id = city_year),
    alpha = 0, 
    size = 2, 
    hjust = "left") +
  scale_size(
    breaks = c(20, 40, 60),
    range = c(6, 14)) +
  scale_y_continuous(limits = c(0, 80)) +
  theme_minimal() +
  labs(#title = title, 
       #subtitle = subtitle,
       caption = 'caption', 
       x = NULL, y = NULL, 
       size = 'Visitors\n(Millions)'
  ) +
  theme(
    #text = element_text(color = "#1f3225", family = "Corbel"),
    # plot.background = element_rect(fill = 'black', color = 'black'),
    # panel.background = element_rect(fill = 'black', color = 'black'),
    # plot.title = element_text(size = 20, face = "bold"),
    # plot.subtitle = element_text(size = 10),
    #plot.caption = element_text(size = 7),
    #axis.text.x = element_text(face = 'bold'),
    #plot.title = element_text(face = 'bold'),
    #plot.subtitle = element_text(face = 'bold'),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) -> gg

girafe(ggobj = gg,
       options = list(opts_sizing(rescale = FALSE))) |>  
  girafe_options(
    opts_tooltip(
      opacity = .9, 
      use_fill = TRUE,
      use_stroke = TRUE, 
      css = "padding:5pt;
            background-color:#333333;
            font-family: Open Sans;
            font-size:1rem;
            color:red;"),
    opts_hover_inv(css = "opacity:0.8"), 
    opts_toolbar(saveaspng = TRUE),
    opts_zoom(max = 1),
    opts_hover(
      css = girafe_css(
        css = glue("fill:orange;fill-opacity:1"),
        text = glue("stroke:none;fill:blue;fill-opacity:1;")
      ))
  ) -> d1

d1




#==========================================================#
# Saving Plots and Gifs -----------------------------------
#==========================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W33_WorldFairs",
                       "plots_w33", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W33_WorldFairs",
                        "plots_w33", 
                        paste0("final_plot", ".png")),
  width    = 40, 
  height   = 30, 
  units    = "cm",
  bg       = bg_color
)
