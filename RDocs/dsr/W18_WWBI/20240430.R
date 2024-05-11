#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, here,         
  scales, ggthemes, patchwork,               
  showtext, extrafont, ggtext, ggtext, glue,
  ggalt, ggflags
)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-04-30")

wdata <- tuesdata$wwbi_data 
wseries <- tuesdata$wwbi_series
wcountry <- tuesdata$wwbi_country

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#
#|> rename_with(~c('country', 'indicator', 'year',  'value'))

# Merging data sets
wwbi <- 
  wdata |> 
  left_join(wcountry, by = join_by(country_code == country_code)) |> 
  left_join(wseries, by = join_by(indicator_code == indicator_code)) 

wwbi |> head(10) |> view()
wwbi |> colnames()

# final df : selected variables/columns -----

df <- 
  wwbi |> 
  select(
    country = country_code, code = indicator_code, 
    year, value, region, short_name, table_name, long_name,
    acode = x2_alpha_code, wbcode = wb_2_code, income_group, lending_category,
    other_groups, system_of_trade, government_accounting_concept,
    imf_data_dissemination_standard, indicator_name
  )
df |> count(region)

## Health workers as share of public formal employees ----
df |> filter(code == 'BI.EMP.FRML.HE.PB.ZS') |> select(indicator_name)
hw_pct <- df |> 
  filter(
    code == 'BI.EMP.FRML.HE.PB.ZS', 
    region == 'Sub-Saharan Africa') |> 
  group_by(short_name, acode) |> 
  summarise(avg = mean(value), .groups = 'drop') |> 
  mutate(short_name  = fct_reorder(short_name, avg),
         acode  = fct_reorder(acode, avg) |> str_to_lower(),
         pct = round(avg *  100, 0))
  


## Edu 3 in Public sector ------
# Proportion of total employees with tertiary Education working in public sector
df |> filter(str_detect(
  indicator_name, 
  'Proportion of total employees with tertiary Education working in public sector')) |> 
  select(code)
df |> 
  #filter(code == 'BI.EMP.FRML.HE.PB.ZS') |> 
  count(indicator_name) |>
  view()

edu3 <- df |> 
  filter(
    code == 'BI.EMP.TOTL.PB.TT.ZS', 
    region == 'Sub-Saharan Africa') |> 
  select(short_name, acode, year, value)
 
#edu3 |> select(year) |> summary()
cnts <- edu3 |> select(short_name) |> distinct() |> pull()
edu_year <- tibble(
  year = rep(c(2000:2020), 40),
  short_name = rep(cnts, each = 21))

edu3 |> 
  filter(short_name == 'Ethiopia') |> select(acode)


edu_full <- edu_year |> 
  full_join(edu3) |> 
  arrange(short_name, acode) |> 
  fill(acode) |> 
  arrange(year, short_name) |> 
  group_by(short_name) |> 
  fill(value, .direction = "downup")

anim_df <- 
  edu_full |> 
  arrange(year, -value) |> 
  group_by(year) |> 
  mutate(rank = 1:n())
#mutate(rank = row_number(desc(avg))) |> 
#arrange(year, rank)

anim_df |> 
  filter(short_name == 'Ethiopia') 


## Education and Employment in Sub Saharan africa -----

edu_code <- df |> 
  filter(str_detect(indicator_name, 'Individuals with')) |> 
  count(code) |> 
  pull(code)

df |> 
  filter(code %in% edu_code, 
         region == 'Sub-Saharan Africa')  |> 
  select(year, a3 = country, a2 = acode, 
         name = short_name, indicator = indicator_name, 
         value) |> 
  view()

edu_job <- df |> 
  filter(code %in% edu_code, 
         region == 'Sub-Saharan Africa') |> 
  select(year, a3 = country, a2 = acode, 
         name = short_name, indicator = indicator_name, 
         value) |> 
  mutate(indicator = str_remove(indicator, 'Individuals with ')) |> 
  separate(indicator, c('edu_level', 'category'), sep = ' as a share of ') |> 
  mutate(edu_level = str_to_title(edu_level)) |> 
  mutate(class = case_when(
    str_detect(category, 'private') ~ 'Private',
    str_detect(category, 'public') ~ 'Public',
    TRUE ~ 'All')) |> 
  separate(category, c('category', 'sector'), sep = ', by ') |>
  mutate(sector = replace_na(sector, 'Overall') ) |> 
  select(-category) |> 
  separate(sector, c('category', 'group'), sep = ': ') |>
  mutate(group = replace_na(group, 'Overall')) |> 
  mutate(a2 = str_to_lower(a2))

edu_job |> 
  filter(
    #year == 2000,
    edu_level == 'Primary Education',
    #category != 'Overall',
    group != 'Overall',
    value > 0
    ) |> count(category)
  ggplot(aes(value, fct_reorder(name, value))) +
  geom_col()

getwd()
write_csv(edu_job, 'W18_WWBI/wwbiApp/edu_job.csv') 
  
edu_job |> count(group, sort = TRUE)

edu_job |> filter(value >= 1) |> select(name) |> n_distinct()
edu_job |> 
  filter(edu_level  == 'Tertiary Education',
         group == 'Health',
         category != 'Overall',) |> 
  count(year, name)
hist(edu_job$value)

edu_job |> 
  select(-a3) |> 
  mutate(
    a2 = replace_na(a2, 'na'),
    decade = year %/% 10 * 10) |> 
  count(year, a2, name, edu_level, group, class) |> 
  group_by(name, edu_level) |> 
  summarise(n = sum(n)) |> 
  arrange(desc(n))

edu_job |> 
  mutate(edu  = case_when(
           edu_level == 'No Education' ~ 'No Education',
           TRUE ~ 'With Education')) |> 
  group_by(year, a2, name, edu, class, group) |> 
  summarise(value = mean(value), .groups = 'drop') |> 
  mutate(pct = round(value * 100)) |> 
  filter(
    #category == 'Overall',
    #group == 'Overall',
    value > 0) |> 
  count(year, group)

edu_job |> 
  filter(class == 'Public', 
         edu_level == 'Tertiary Education',
         group != 'Overall') |> 
  select(year, a2, name, group, value) |> 
  group_by(group, name) |> 
  
  filter(value == max(value)) |> 
  ungroup() |> 
  mutate(pct = round(value * 100, 0)) |> 
  filter(
    group == 'Education',
    class == 'Public') |> 
  ggplot(aes(value, name, fill  = class)) +
  geom_col(position = 'dodge') +
  facet_wrap(~edu_level)

#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#
#font_import()
#loadfonts()
#font_files() |> tibble() |> view()
font_add_google("Roboto", "roboto")

font_families()
showtext_auto()


#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#

bg_color    <- "grey20"
text_light  <- ""
text_dark   <- ""
highlight   <- ""
title_color <- ""
sub_color   <- ""
cap_color   <- "white"
tsize       <-  unit(30, units = "cm")



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
  "**Data**: TidyTuesday Week 18<br>", 
  "**Graphics:** ", 
  social_caption)


hw_title    <- "WOLRDWIDE BUREAUCRACY INDICATORS"
hw_subtitle <- "Health workers as a share (%) of public formal employees"


#==========================================================================#
# Custom Theme------------------------------------------------
#==========================================================================#

costum_theme <- function(){ 
  cfont <- "roboto"   
  theme_minimal(
    #base_family = cfont,
    #base_size = 40
  ) %+replace%    
    theme()
}




#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

## Health workers as share of public formal employees ----
hw_pct |> 
  arrange(desc(avg)) |> 
  slice_head(n = 10) |> 
  ggplot(aes(avg, short_name)) +
  geom_col(width  = .05, fill = 'grey99', alpha = .7) +
  geom_point(size = 12, alpha = 1, color = 'dodgerblue') +
  geom_text(aes(label = short_name), 
            #x = c(.005, .005, .015, rep(.005, 9)),
            x = .005,
            hjust = 0, vjust = -.5, 
            color = 'white',
            fontface = 'bold.italic') +
  geom_text(aes(label = paste0(pct, '%'), x = avg), 
            hjust = .5, vjust = .5, 
            color = 'white',
            size = 4, fontface = 'bold') +
  geom_flag(aes(country = acode),
            x = 0, size = 10) +
  labs(x = NULL, y = NULL,
       title = hw_title, 
       subtitle = hw_subtitle,
       caption = plot_caption) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = 'black'),
    plot.background = element_rect(fill = 'black'),
    plot.title = element_text(color = 'white', face = 'bold'),
    plot.subtitle = element_text(color = 'white', face = 'bold'),
    plot.caption =  element_textbox(
      #family     = caption_font,
      hjust      = 1,
      color      = 'grey60',
      #size       = 1.5 * tsize,
      lineheight = 0.3,
      margin     = margin(.5,0,0,0, unit = 'cm')),
  )



## Edu 3 in Public sector ----
anim_df
gg_edu <- anim_df |> 
  filter(rank <= 10, year %in% c(2010, 2015)) |> 
  ggplot() +
  geom_col(aes(x = rank, y = value,
               fill = short_name
               )
           ) +
  geom_text(
    aes(x = rank, y = -.1, label = short_name), 
    hjust = 1, color = 'white') +
  geom_text(
    aes(x = rank,
        y = value + .01, label = paste0(round(value * 100, 1), '%')),
    hjust = 0, color = 'white') +
  coord_flip(clip = "off", expand = FALSE) +
  labs(
    title = "WOLRDWIDE BUREAUCRACY INDICATORS",
    subtitle = 'Proportion of total employees with tertiary Education\n working in public sector by year: {closest_state}',
    x = "", 
    y = "total launched",
    caption = "tidytuesday - Week 18") +
  scale_y_continuous(
    labels = scales::comma,
    breaks = seq(0, 1, .1),
    limits = c(-.3, 1.2)) +
  scale_x_reverse()
p_edu <- gg_edu  +
  transition_states(
    year, transition_length = 4, 
    state_length = 1, wrap = FALSE) +
  enter_fade() + 
  exit_fade() +
  theme_void() +
  theme(
    text = element_text(size = 14, lineheight = 0.1, color = 'white'),
    plot.background = element_rect(fill = bg_color, color = bg_color),
    plot.title = element_textbox_simple(
      color = txt, hjust = 0.05, margin = margin(t = 10, b = 10),
      size = 18, face = "bold"),
    plot.subtitle = element_text(
      color = 'white', hjust = 0.05, 
      #margin = margin(t = 10, b = 20)
      ),
    plot.caption = element_textbox_simple(
      color = txt, hjust = 0.5, 
      #margin = margin(t = 10)
      ),
    #plot.margin = margin(b = 10, t = 10, r = 50, l = 10),
    axis.text = element_text(color = 'white', size = 10),
    axis.text.y = element_blank(),
    legend.position = "none"
  )
p_edu
animate(p_edu, fps = 30, duration = 30, width = 800, height = 600)


## Jobs -----
jobs <- df |> 
  filter(code %in% edu_code, 
         region == 'Sub-Saharan Africa') |> 
  select(year, a3 = country, a2 = acode, 
         name = short_name, indicator = indicator_name, 
         value)

jobs |> 
  filter(
    year == 2000,
    name == 'Angola') |> 
  ggplot(aes(value, indicator)) +
  geom_col()

 #===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path("W18_WWBI",
                       "plots_w18", 
                       paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W18_WWBI", "plots_w18", "final_plot_hw.png"),
  #plot    = plot,
  width    = 20, 
  height   = 15, 
  units    = "cm",
  #bg       = bg_col
)
