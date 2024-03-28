#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue,
  ggstream, cowplot, paletteer, colorspace)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-03-19")

xmm <- tuesdata$mutant_moneyball |> clean_names()

xmm |> view()
xmm |> glimpse()
xmm |> select(Member) |> n_distinct() ==  xmm |> nrow()

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

## Custom function: -------
# 1 => input data: selected columns
# 2 => pivot longer
# 3 => extract year , change to `yyyy` and rename to `decade`
# 4 => extract values and parse as double and rename to predefined column name
data_transform <- function(
    data, begin = begin, end = end, value_col = value_col){
  dat <- data |> select(member, begin:end)
  if (!is.character(dat[, 2] |> pull())) {
     dat |> 
      pivot_longer(-member, names_to = 'decade', values_to = value_col) |> 
      mutate(decade = 1900 + parse_number(decade))
  } else {
    dat |> 
      select(member, begin:end) |> 
      pivot_longer(-member, names_to = 'decade', values_to = value_col) |> 
      mutate(decade = 1900 + parse_number(decade),
             {{value_col}} := parse_number(get(!!value_col)))
  }
}

## Appearance ------
# The total number of issues each X-Men member appeared 
# in between 1963 and 1992
xmm_appearance <- xmm |> 
  data_transform(
    begin = 'total_issues60s', 
    end = 'total_issues90s', 
    value_col = 'appearance')

## Heritage highest sale  ----
# Total value of each X-Men team member's total number of issues 
# as reflected by Heritage highest sale of comics released btw 1963 and 1992.
xmm_hhs <- xmm |> 
  data_transform(
    begin = 'total_value60s_heritage', 
    end = 'total_value90s_heritage', 
    value_col = 'hh_value')

## Ebay sales -----
# Total value of each X-Men team member's total number of issues 
# as reflected by ebay sales in 2022 in which sellers tagged the 
# issue as VG (Very Good) Condition.

xmm_ebay_val <- xmm |> 
  data_transform(
    begin = 'total_value60s_ebay', 
    end = 'total_value90s_ebay', 
    value_col = 'ebay_value')


## Appearance percent -----
# The percentage each X-Men member appeared in an issue published 
# between 1963 and 1992

xmm_appear_percent <- xmm |> 
  data_transform(
    begin = 'x60s_appearance_percent', 
    end = 'x90s_appearance_percent', 
    value_col = 'appear_pct')

## Heritage Average price per issue  ----
# Average price per issue for each X-Men member based on highest sales 
# on Heritage for issues published between 1963 and 1992.

xmm_h_ppi <- xmm |> 
  data_transform(
    begin = 'ppi60s_heritage', 
    end = 'ppi90s_heritage', 
    value_col = 'h_ppi')

## Ebay average price per issue ----
# Average price per issue for each X-Men member based on VG sales 
# on eBay for issues published between 1963 and 1969.
xmm_ebay_ppi <- xmm |> 
  data_transform(
    begin = 'ppi60s_ebay', 
    end = 'ppi90s_ebay', 
    value_col = 'ebay_ppi')


## Value in Wizard Price Guide -----
# Total value of each X-Men team member's total number of issues released between 1963 and 1969 as they were valued in April 1993's Wizard Price Guide
xmm_wiz_val <- xmm |> 
  data_transform(
    begin = 'total_value60s_wiz', 
    end = 'total_value90s_wiz', 
    value_col = 'wiz_value')

## Value in Overstreet -------
# Total value of each X-Men team member's total number of issues released 
# between 1963 and 1969 as they were valued in 2015's Overstreet Price Guide.

xmm_os_val <- xmm |> 
  data_transform(
    begin = 'total_value60s_o_street', 
    end = 'total_value90s_o_street', 
    value_col = 'street_value')


## Wiz average price per issue ----
# Average price per issue for each X-Men member based on April 1993 
# Wizard Price Guide for issues published between 1963 and 1992.

xmm_wiz_ppi <- xmm |> 
  data_transform(
    begin = 'ppi60s_wiz', 
    end = 'ppi90s_wiz', 
    value_col = 'wiz_ppi')

## Overstreet average price per issue ----
# Average price per issue for each X-Men member based on 2015 
# Overstreet Price Guide for issues published between 1963 and 19

xmm_os_ppi <- xmm |> 
  data_transform(
    begin = 'ppi60s_o_street', 
    end = 'ppi90s_o_street', 
    value_col = 'os_ppi')


## Data merge -----
df_list <- list(xmm_appearance, xmm_hhs, xmm_appear_percent,  xmm_h_ppi,
                xmm_ebay_val, xmm_ebay_ppi, 
                xmm_wiz_val, xmm_wiz_ppi, 
                xmm_os_val, xmm_os_ppi)
### Base R 
base_merge_func <- function(data_list) {
  Reduce(
    function(...) merge(..., all.x = TRUE, by = c('member', 'decade')), 
    data_list
    )
}

df_list |> base_merge_func() |> as_tibble()


### dplyr 
#join_all(list(x,y,z), by='Flag', type='left')
df_list |> 
  plyr::join_all(
    by = c('member', 'decade'), 
    type = 'left') |> 
  as_tibble() -> final_xmenmutal_df


### Saving final fdf after  cleaning member names 
name_clean_func <- function(df, mcol){
  df |> 
    mutate(
      {{mcol}} := str_replace(
        {{mcol}}, 
        pattern = "([[:upper:]])",
        replacement = " \\1")) |> 
    separate({{mcol}}, into = c('fn', 'ln'), sep = ' ') |> 
    mutate(fn = str_to_title(fn)) |> 
    unite(name, c(fn, ln), sep = ' ') |> 
    select(name, everything())
}

fdf <-  
  final_xmenmutal_df |> 
  name_clean_func(member) |> 
  mutate(decade = factor(
    decade, 
    levels = c('1960', '1970', '1980', '1990')))
#write_csv(fdf, 'RDrafts/datasets/final_xmenmutal_df.csv')

#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

fdf |> glimpse()

# The percentage each X-Men member appeared in an issue
appearance_percent <- fdf |> 
  select(name, decade, appear_pct) |> 
  mutate(appear_pct = appear_pct / 100) |> 
  group_by(name) |> 
  summarise(pct = mean(appear_pct)) |> 
  mutate(name = fct_reorder(name, pct)) |> 
  ggplot(aes(pct, name)) +
  geom_col() +
  scale_x_continuous(labels = percent, expand = c(0, 0))


# Average price per issue for each X-Men member
avg_price <- fdf |> 
  select(decade, contains('ppi')) |> 
  rename_with(~c('decade', 'Heritage', 'VG eBay', 
                 'Wizard PG', 'Overstreet PG')) |> 
  pivot_longer(-decade, names_to = 'listing', values_to = 'avg') |> 
  group_by(decade, listing) |> 
  summarise(avg = mean(avg), .groups = 'drop')

avg_price |> 
  ggplot(aes(decade, avg, fill = listing )) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~listing, scales = 'free') +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = dollar
      #unit_format(unit = "K", scale = 1/1000, sep = "")
      ) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_minimal()
xmm |> 
  filter()
  
# Total value of each X-Men team member's total number of issues

total_values_df <- fdf |> 
  select(decade, contains('value')) |> 
  rename_with(~c('decade', 'Heritage', 'VG eBay', 
                 'Wizard PG', 'Overstreet PG')) |> 
  pivot_longer(-decade, names_to = 'listing', values_to = 'total')

total_values_df |> 
  ggplot(aes(decade, total, fill = listing)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~listing, scales = 'free') +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = dollar) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_minimal()

  
#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#

font_add_google("Roboto", "roboto")
font_add_google(name = "Protest Riot", family = "protest", db_cache = FALSE)
font_add_google("Anton", regular.wt = 400, family = "anton")  
font_add_google("Abel", family = "abel")  
font_add_google("Barlow Condensed", family = "barlow")
font_add_google("Pacifico", family = "pacifico")

showtext_auto()


#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#

bg_col <- "white"
text_light <- "grey70"
text_dark <- ""
highlight_col <- ""
title_col <- "#6D4843FF"
title_font <- 'protest'
sub_color <- ""
sub_font <- 'barlow'
caption_font <- 'roboto'
tsize = unit(30, units = "cm")



#==========================================================================#
# Define texts and annotations ---------------------------------------------
#==========================================================================#
tsize = unit(30, units = "cm")

sysfonts::font_add(
  family = "fab6_reg",
  regular = "Font Awesome 6 Brands-Regular-400.otf") #/Library/Fonts/

github <- "&#xf09b"
github_username <- "birusod"

xtwitter <- "&#xe61b"
xtwitter_username <- "@DSbyKnight"

text_col <- "grey70"
social_caption <- glue::glue(
  "<span style='font-family:fab6_reg;'>{github};</span> 
  <span style='color: {text_col}'>{github_username}  </span> 
  <span style='font-family:fab6_reg;'>{xtwitter};</span> 
  <span style='color: {text_col}'>{xtwitter_username}</span>")
plot_caption <- paste0(
  "**Data**: TidyTuesday Week 12<br>", 
  "**Graphics:** ", 
  social_caption)


ptitle <- "X-MEN MUTANT MONEYBALL"
psubtitle <- paste0(
  "Average price per issue for each X-Men member by listing", 
  "for issues published between 60s and 90s")



#==========================================================================#
# Custom Theme------------------------------------------------
#==========================================================================#

costum_theme <- function(){ 
  cfont <- "roboto"   
  theme_minimal(
    #base_family = cfont,
    #base_size = 40
  ) %+replace%    
    theme(
      
      plot.title = element_text(
        size        = 4 * tsize, 
        family      = title_font,
        face        = 'bold',
        color       = title_col,
        margin      = margin(t = 5, b = 5)), 
      
      plot.subtitle = element_textbox_simple(
        size        = 3 * tsize,
        family      = sub_font,
        color       = title_col,
        lineheight  = .8, 
        halign      = .5,
        margin      = margin(t = .5, b = 2, unit = 'cm')),
      
      plot.caption =  element_textbox(
        family     = caption_font,
        hjust      = 1,
        color      = 'grey60',
        size       = 1.5 * tsize,
        lineheight = 0.3,
        margin     = margin(.5,0,0,0, unit = 'cm')),
      
      strip.clip = "off",
      strip.text = element_text(
        size       = 3  *  tsize,
        color      = "#6D4843FF",
        family     = 'pacifico',
        margin     = margin(b = 2, unit = 'pt')),
      axis.text.y = element_text(
        size       = 2.5 * tsize,
        family     = 'anton',
        color      = "#01192FFF"),
      axis.text.x = element_text(
        size       = 3 * tsize,
        family     = 'anton',
        color      = "grey70"),
      
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = .5),
      panel.spacing      = unit(4, "lines")
      
    )
}

avg_price |> 
  ggplot(aes(decade, avg, fill = listing )) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~listing, scales = 'free_y') +
  #scale_y_continuous(
  #  expand = expansion(mult = c(0.01, 0.03)),
  #  labels = dollar) +
  ggh4x::facetted_pos_scales(
    y = list(
      listing == 'Heritage' ~ scale_y_continuous(breaks = c(2, 4, 6)  * 1000),
      listing == 'Overstreet PG' ~ scale_y_continuous(breaks = c(100, 250, 400)),
      listing == 'VG eBay' ~ scale_y_continuous(breaks = c(50, 100, 150)),
      listing == 'Wizard PG' ~ scale_y_continuous(breaks = c(10, 30, 50)))) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(
    values = paletteer_d("ggsci::planetexpress_futurama") |> 
      darken(.15)) +
  labs(x = NULL, y = NULL, 
       title = ptitle, subtitle = psubtitle, caption = plot_caption)  +
  costum_theme()



#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

# save the final plot as 'final_plot'
ggsave(
  filename = file.path(
    "W12_XMen",
    "plots_w12", 
    paste0("final_plot", ".png"))
  )

ggsave(
  filename = here::here("W12_XMen", "plots_w12", "final_plot_w12.png"),
  width = 40, 
  height = 30, 
  units = "cm",
  bg = bg_col
)





