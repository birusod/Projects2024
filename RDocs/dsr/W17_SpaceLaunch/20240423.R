#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor,          
  scales, ggthemes, patchwork,               
  showtext, extrafont, ggtext, ggtext, glue,
  gganimate, hrbrthemes, MetBrewer,
  gifski, av
)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-04-23")
oso <- tuesdata$outer_space_objects |> clean_names()

#==========================================================================#
# Wrangling Data + Viz -----------------------------------------------------
#==========================================================================#
oso |> 
  #head(20) |> 
  view()



## Private companies -----
pv <- oso |> filter(is.na(code))
pv_total <- pv |>
  mutate(entity = str_to_upper(entity)) |> 
  group_by(entity) |> 
  summarise(total = sum(num_objects)) |> 
  mutate(
    entity = fct_lump_n(entity, 7, w = total, other_level = 'OTHER') |> 
      fct_reorder(total)
    )

pv_total |> 
  ggplot(aes(total, entity)) +
  geom_col()






## Countries  ------

by_country_top10 <- oso |> 
  filter(!is.na(code), code != 'OWID_WRL') |> 
  group_by(code) |> 
  summarise(total = sum(num_objects)) |> 
  arrange(desc(total)) |> 
  head(10)
by_country_top10 |> 
  mutate(code  = fct_reorder(str_to_upper(code), total)) |> 
  ggplot(aes(total, code, fill = code))  +
  geom_col() +
  scale_fill_manual(values = met.brewer("Redon", n = 10))



oso |> 
  filter(!is.na(code), code != 'OWID_WRL') |> 
  


## World -----
oso |> 
  filter(code == 'OWID_WRL') |> 
  summarise(total = sum(num_objects))

pv |> 
  summarise(total = sum(num_objects))

oso |> 
  filter(is.na(code)) |> 
  mutate(len = nchar(code)) |> 
  filter(!len %in% c(3, 8))

df_check <- oso |> 
  mutate(group = case_when(
    code == 'OWID_WRL' ~ 'World',
    TRUE ~ 'Other'
  )) |> 
  group_by(year, group) |> 
  summarise(total = sum(num_objects))

df_check |> 
  ggplot(aes(year, total, fill = group)) +
  geom_col(position = 'dodge')

df_check |> 
  pivot_wider(id_cols = year, names_from = group, values_from = total) |> 
  mutate(check = abs(Other - World)) |> 
  arrange(desc(check))
 

## By year
by_year_type <- oso |> 
  filter(code == 'OWID_WRL' | is.na(code)) |> 
  mutate(type = case_when(
    is.na(code) ~ 'PRIVATE',
    TRUE ~ 'PUBLIC'
  )) |> 
  group_by(year, type) |> 
  summarise(total = sum(num_objects))

by_year_type |> 
  ggplot(aes(year, total, fill = type)) +
  geom_col() +
  facet_wrap(~ type, scales = 'free')


## Animation --------
# tutorial 1
# https://www.youtube.com/watch?v=XhTGEfnlGVw&t=37s


anim_df <- oso |> 
  filter(code != 'OWID_WRL' , !is.na(code)) |> 
  group_by(code, year) |> 
  summarise(total = sum(num_objects)) |> 
  mutate(cumtot = cumsum(total)) 

anim_df2 <- anim_df |> 
  group_by(year) |> 
  arrange(year, -cumtot) |> 
  mutate(rank = 1:n())

anim_df2 |> 
  #filter(year == 2023) |> 
  filter(rank <= 10) |> 
  ggplot() +
  aes(xmin = 0, xmax = cumtot) +
  aes(y = rank, ymin = rank - .45, ymax = rank + .45) +
  facet_wrap(~year) +
  geom_rect(alpha = .7) +
  scale_x_continuous(
    limits = c(-500, 12000),
    breaks = seq(2500, 10000, 2500)) +
  geom_text(
    col = 'darkblue',
    hjust = 'right',
    aes(label = code),
    x = -100) +
  geom_text(
    col = 'darkblue',
    hjust = 'right',
    aes(label = paste(cumtot)),
    x = 12000) +
  scale_y_reverse() +
  theme_classic() -> pp
  
pp +
  facet_null() +
  geom_text(
    x = 8000,
    y = -10,
    aes(label = paste0('Year: ', as.character(year))),
    size = 16,
    color = 'red') +
  aes(group = code) +
  transition_time(year) -> anim

animate(anim, nframes = 100, end_pause = 10)
animate(
  anim,
  nframes = 20,
  fps = 1, 
  #duration = 50
)

## By decade and country

anim_decade <- oso |> 
  filter(code != 'OWID_WRL' , !is.na(code)) |> 
  mutate(decade = year %/% 10 * 10) |> 
  group_by(code, decade) |> 
  summarise(total = sum(num_objects)) |> 
  mutate(cumtot = cumsum(total)) |> 
  ungroup()

anim_decade |> 
  group_by(decade) |> 
  arrange(decade, -cumtot) |> 
  mutate(rank = 1:n()) |> 
  filter(rank <= 10) |> 
  ggplot(
    aes(xmin = 0, xmax = cumtot,
        y = rank, ymin = rank - .45, ymax = rank + .45,
        fill = code)) +
  facet_wrap(~decade) +
  geom_rect(
    alpha = .7, 
    show.legend = FALSE,
    color = NA) +
  scale_x_continuous(
    limits = c(-500, 12000),
    breaks = seq(2500, 10000, 2500)) +
  geom_text(
    aes(label = code, color = code),
    hjust = 'right', fontface = 'bold',
    x = -100,
    show.legend = FALSE) +
  geom_text(
    col = 'gold',
    hjust = 'right',
    aes(label = paste(cumtot)),
    x = 12000,
    show.legend = FALSE) +
  scale_y_reverse() +
  scale_color_brewer(palette = 'Paired') +
  scale_fill_brewer(palette = 'Paired') +
  labs(
    y = NULL,
    title = 'Objects Launched into Space:\nTop 10 countries',
    subtitle = 'By decade: {closest_state}',
    caption = 'Data: TidyTuesday | Week 17') +
  theme_ft_rc() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_blank(),
    plot.subtitle = element_text(face = 'bold', color = 'gold')
  ) -> ppp

ppp2 <- ppp +
  facet_null() +
  geom_text(
    x = 5000,
    y = -10,
    aes(label = as.character(decade)),
    size = 20,
    color = 'grey80') +
  aes(group = code) 

ppp2 +
  transition_states(decade) +
  enter_fade() +
  exit_fade()  -> anim2
#anim2
anim_g <- animate(
  anim2,
  nframes = 10,
  fps = 1,
  duration = 10
)


#  tutorial 2
# https://www.youtube.com/watch?v=FOEoKbRUsT8&t=2353s

dat <- oso |> 
  filter(code != 'OWID_WRL' , !is.na(code)) |> 
  mutate(decade = year %/% 10 * 10) |> 
  arrange(decade) 
dat1 <- dat |> 
  group_by(code) |> 
  mutate(cumtot = cumsum(num_objects))
dat2 <- dat1 |> 
  group_by(decade, code) |> 
  summarise(total = sum(cumtot)) |> 
  ungroup()

dat_wide <- dat2 |> 
  pivot_wider(
    names_from = code,
    values_from = total
  )
dat_rank <- dat2 |> 
  group_by(decade) |> 
  arrange(decade, -total) |> 
  mutate(rank = 1:n()) |> 
  filter(rank <= 10)

dat_rank |> 
  ggplot(aes(rank, total)) +
  geom_bar(stat = 'identity', aes(fill = code), color = NA) +
  scale_fill_manual(values = met.brewer("Redon", n = 13)) +
  geom_text(aes(9, 30000, label = sprintf("%1.0f", decade)), 
            color = 'grey', size = 20) +
  coord_flip(clip = 'off', expand = FALSE) +
  scale_y_continuous(limits = c(0, 35000)) +
  scale_x_reverse() +
  geom_text(aes(rank, y = 0, label = code),
            hjust = 1.1, fontface = 'bold', size = 8) +
  geom_text(aes(label = sprintf("%1.0f", total)), 
            hjust = 0, fontface = 'bold', size = 8)  +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = 'none',
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(1, 2, 1, 2, 'cm'),
    plot.background = element_rect(fill = '#fff9f3', color = NA),
    plot.title = element_text(color = 'brown', face = 'bold', size = 20),
    plot.subtitle = element_text(color = 'firebrick', face = 'bold.italic', size = 14),
    plot.caption = element_text(color = 'grey', face = 'italic', size = 10)
  ) -> dat_chart

dat_anim <- 
  dat_chart +
  transition_states(decade, 
                    transition_length = 4, 
                    state_length = 1) +
  #view_follow(fixed_x = TRUE) +
  ease_aes('quadratic-in-out') +
  labs(
    title = 'Objects Launched into Space:\nTop 10 countries',
    subtitle = 'By decade: {closest_state}',
    caption = 'Data: TidyTuesday | Week 17')

dat_anim |> animate(height = 450, width = 500)


#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# making a gif: gifski

# save the final gif 
anim_save(
  anim_g,
  filename = here::here("W17_SpaceLaunch", "plots_w17", "gif_01.gif"))


gif1 <- dat_anim |> animate(height = 450, width = 500)
anim_save(
  gif1, 
  filename = here::here("W17_SpaceLaunch", "plots_w17", "gif_02.gif"))

# Exporting as mp4
vid_anim <- 
  dat_anim |> 
  animate(
    fps = 24,
    duration = 55,
    #width = 1080,
    #height = 1020,
    renderer = av_renderer()
    )

anim_save(
  vid_anim, 
  filename = here::here("W17_SpaceLaunch", "plots_w17", "vid_01.mp4"))
