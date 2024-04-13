#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, ggeasy,         
  scales, ggthemes, patchwork, ggnewscale, PNWColors,         
  showtext, extrafont, ggtext, glue ,
  summarytools, usmap, sf
)



#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-04-09")

a2023 <- tuesdata$eclipse_annular_2023 
t2024 <- tuesdata$eclipse_total_2024 
p2023 <- tuesdata$eclipse_partial_2023 
p2024 <- tuesdata$eclipse_partial_2024 



#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#
#a2023 |> head(100) |> view()
#a2023 |> summarytools::dfSummary() |> view()

a2023 |> distinct(state) |> 
  mutate(year = 2023,
         type = "Annular")


count_by_state <- function(data, col, year = year, type=type){
  data |> 
    count({{col}}) |> 
    mutate(year := {{year}},
           type := {{type}})
}

df <- 
  bind_rows(
    a2023 |> count_by_state(state, year = 2023, type = 'Annular'),
    t2024 |> count_by_state(state, year = 2024, type = 'Total'),
    p2023 |> count_by_state(state, year = 2023, type = 'Partial'),
    p2024 |> count_by_state(state, year = 2024, type = 'Partial')
  )

#write_csv(df, 'RDrafts/datasets/eclipses_by_state.csv')


# Week 19 - 2023 data: Childcare Costs
cccdata <- tidytuesdayR::tt_load('2023-05-09')

#childcare costs data
ccc <- cccdata$childcare_costs 
#counties data
cts <- cccdata$counties
ccc |> 
  summarytools::dfSummary() |> view()


ccc |> count(study_year)
unemploy_16_older_2018 <- 
  ccc |> 
  filter(study_year == 2018) |> 
  select(fips       = county_fips_code,
         unemploy   = unr_16)

county_2018 <- 
  cts |> 
  select(fips   = county_fips_code, 
         county = county_name, 
         state  = state_name,
         abbr    = state_abbreviation)

unemploy_16_older_2018 |> nrow()

unemp_data <-  
  unemploy_16_older_2018 |> 
  left_join(county_2018, by = join_by(fips))
unemp_data_avg_by_state <- 
  unemp_data |> 
  group_by(state, abbr) |> 
  summarise(unemploy_avg = mean(unemploy), .groups = 'drop')

  
unemp_data_avg_by_state


us <- tigris::states(resolution = "20m")
us_state_geo <- us |> select(state = NAME) 
us_state_geo |> head()
counties_unemp <- maps::county.fips |> 
  separate(polyname, c('state', 'county_name'), sep = ',') |> 
  left_join(unemp_data, by = join_by(fips))



unemp_data_avg_by_state |> head() 

unemp_data_geo <-  
  us_state_geo |> 
  left_join(unemp_data_avg_by_state, by = join_by(state))

counties_unemp |> head()

unemp_data_avg_by_state |> head()

us_state_geo |> head()
unemp_data_geo <- 
  us_state_geo |> 
  left_join(unemp_data_avg_by_state, by = join_by(state))

unemp_data_geo |> head()
#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")
font_add_google("Racing Sans One", "rso")       
font_add_google("Bowlby One SC", "bosc")     
font_add_google("Saira Extra Condensed",  "second")     
font_add_google("Changa", family = "changa")  

font_families()
showtext_auto()


#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#

bg_color    <- "white"
text_light  <- ""
text_dark   <- ""
highlight   <- ""
title_color <- ""
sub_color   <- ""
cap_color   <- "grey"
tsize       <-  unit(30, units = "cm")



pal1 <- pnw_palette("Bay", n = 100, type = "continuous")
pal2 <- pnw_palette("Anemone", 100, type = "continuous")
pal3 <- MetBrewer::met.brewer("Tam")
#==========================================================================#
# Define texts and annotations ---------------------------------------------
#==========================================================================#
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
  "**Data**: TidyTuesday Week 15<br>", 
  "**Graphics:** ", 
  social_caption)


plot_title    <- "UNEMPLOYMENT IN USA"
plot_subtitle <- str_wrap("Unemployment rate of the population aged 16 years old or older in US Counties in 2018", 20)

cap_font <- 'second'
title_font <- 'rso'
subtitle_font <- 'changa'


#==========================================================================#
# Custom Theme------------------------------------------------
#==========================================================================#

costum_theme <- function(){ 
  cfont <- "roboto"   
  theme_minimal(
    #base_family = cfont,
    base_size = 40
  ) %+replace%    
    theme(
      # plot.title = element_textbox(
      #   family = "roboto", 
      #   color = "black",
      #   hjust = 0.5, size = 4 * tsize
      #   #margin = margin(b  = .2, unit = 'cm')
      # ),
      # plot.subtitle = element_text(
      #   family = "roboto", 
      #   color = "black",
      #   hjust = 0.5, size = 3 * tsize,
      #   lineheight = .3
      #   #margin = margin(b  = .5, unit = 'cm')
      # ),
      
      
      
      #plot.margin = margin(1, 5, 1, 5, unit = 'cm')
    )
}




#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#
unemp_data$unemploy |> hist()

## ggplot + sf + tigris + maps ------


unemp_data_geo |> filter(str_detect(state, 'Puerto'))
 
map_data <- unemp_data_geo |> 
  filter(!state %in% c('Alaska', 'Hawaii', 'Puerto Rico'))
         
ggplot() +
  geom_sf(data = map_data, fill = "#4B423D", color = "grey70") +
  geom_sf(data = unemp_data_geo, 
          aes(fill = unemploy_avg)) +
  scale_x_continuous(limits = c(-123, -68)) +
  scale_y_continuous(limits = c(22, 50)) +
  theme_void(base_family = 'roboto') +
  scale_fill_stepsn(colors = pal3) +
  labs(title = 'UNEMPLOYMENT IN CONTINENTAL US IN 2018',
       fill = 'Unemployment percent for\n19 years old or more') -> ggmap 

ggmap +
  theme(
    legend.position = "top",
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, size = 12),
    legend.text = element_text(size = 13),
    plot.background = element_rect(fill = "#c1ced6", color = NA),
    plot.title = element_text(
      size = 16, hjust = 0.5, face = "bold", 
      margin = margin(0, 0, 10, 0)),
    plot.margin = margin(20, 0, 10, 0)
  )


## usmap------
plot_usmap(
  data = unemp_data, 
  values = "unemploy", 
  color = "blue"
  ) + 
scale_fill_gradient2(
  midpoint = 5,
  name = "Unemployment rate", 
  label = scales::comma) + 
labs(
  title = "UNEMPLOYMENT IN USA", 
  subtitle = "Unemployment rate of the population aged 16 years old or older in US Counties in 2018", 50) +
#theme(legend.position = "right")
easy_move_legend(to = c("right"))


unemp_data_avg_by_state$unemploy_avg |> hist()

unemp_data_avg_by_state |>  
  plot_usmap(
  data = _, 
  values = "unemploy_avg", 
  labels = TRUE, label_color = 'navy',
  color = NA,
  size = .1) +
  scale_fill_gradientn(colours = pal2, 
                       limits = c(0,10))  +
  guides(
    fill = guide_legend(
      title = 'Rates',
      title.position = "top", 
      title.hjust = 0.5,
      label.position = "bottom")) +
  labs(
    title = plot_title, 
    subtitle = plot_subtitle,
    caption = plot_caption) +
  theme(
    plot.margin = margin(10, 0, 0, 0),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    #legend.position.inside = c(.5, 1),
    legend.title = element_text(size = .6 * tsize, face = 'bold'),
    legend.text = element_text(size = .5 * tsize),
    #legend.margin = margin(t = 1, unit = 'cm'),
    legend.key.width = unit(.5, 'cm'),
    legend.key.height = unit(.2, 'cm'),
    
    plot.title = element_textbox(
      family = title_font,
      color = "black",
      hjust = 0.5, size = 1 * tsize
      
    ),
    plot.subtitle = element_textbox_simple(
      family = title_font,
      face = 'italic',
      color = "grey70",
      halign = 0.5, 
      size = .8 * tsize,
      lineheight = .1
      #margin = margin(b  = .2, unit = 'cm')
    ),
    
    plot.caption = element_textbox(
      family = cap_font,
      color = "grey",
      lineheight = .1, hjust = 1, size = .5 * tsize
    )
  )

#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
# ggsave(
#   filename = file.path("W15_USSolarEclipses",
#                        "plots_w15", 
#                        paste0("final_plot", ".png"))
#   )

ggsave(
  filename = here::here("W15_USSolarEclipses", "plots_w15", "final_plot_w15.png"),
  width    = 11,  height   = 9,  units    = "cm",
  bg       = bg_color
)
