#==========================================================================#
# Load packages ------------------------------------------------------------
#==========================================================================#

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue)


#==========================================================================#
# Loading data -------------------------------------------------------------
#==========================================================================#

tuesdata <- tidytuesdayR::tt_load("2024-03-26")

results <- tuesdata$`team-results` |> clean_names()
pubpick <- tuesdata$`public-picks` |> clean_names()

#==========================================================================#
# Wrangling Data -----------------------------------------------------------
#==========================================================================#

results |> colnames()
results |> glimpse()
pubpick |> colnames()
pubpick |> glimpse()

results |> count(team)
pubpick |> count(team)


wins <- results |> 
  select(team, champ) |> 
  filter(champ > 0) |> 
  arrange(desc(champ)) |> 
  mutate(team = str_to_upper(team))

wpercent20 <- results |> 
  select(team, winpercent) |> 
  mutate(winpercent = round(winpercent, 2),
         wpct_fmt = format(round(winpercent, 2), 2)) |> 
  filter(winpercent > 0) |> 
  arrange(desc(winpercent)) |>
  head(20) |> 
  mutate(team = str_to_upper(team))

picks <- pubpick |> 
  select(team, finals) |> 
  mutate(finals = parse_number(finals)) |> 
  filter(finals > 0) |> 
  mutate(team = str_to_upper(team))
  

wins |> 
  left_join(picks) |> 
  mutate(finals = replace_na(finals, 0))


#===========================================================================#
# Data Viz ------------------------------------------------------------------
#===========================================================================#

wins |> 
  ggplot(aes(champ, fct_reorder(team, champ))) +
  geom_col()

wins |> 
  ggplot(aes(champ, fct_reorder(team, champ))) +
  geom_col(width = .1) +
  geom_text(
    aes(label = team), x = 0, hjust = 0, vjust = -1) +
  geom_text(aes(label = champ, x = champ), hjust = 1, vjust = -1) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())


wpercent20 |> 
  ggplot(aes(winpercent, fct_reorder(team, winpercent) )) +
  geom_col()

wpercent20 |> 
  ggplot(aes(winpercent, fct_reorder(team, winpercent) )) +
  geom_col(width = .1) +
  geom_text(
    aes(label = team), x = 0, hjust = 0, vjust = -1) +
  geom_text(aes(label = paste0(wpct_fmt, "%"), x = winpercent), 
            hjust = 1, vjust = -1) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())


#==========================================================================#
# Loading fonts ------------------------------------------------------------
#==========================================================================#

font_add_google("Roboto", "roboto")
font_add_google("Racing Sans One", "rso")       
font_add_google("Bowlby One SC", "bosc")     
font_add_google("Saira Extra Condensed",  "second")     
font_add_google("Changa", family = "changa")        
showtext_auto()



#==========================================================================#
# Defining colors and Theme------------------------------------------------
#==========================================================================#
paletteer::paletteer_d("futurevisions::atomic_blue")
paletteer::paletteer_d("ltc::trio3")
mcols <- c("#0c2d56", "#005eb9","#717b85", "#3CC8C0FF", "#F2EBBBFF", 
           "#0E7175FF", "#C6AA76FF", "#56B4E9FF", "#a99a86", "#D9B523FF")
bg_color <- "white"
text_light <- ""
text_dark <- ""
highlight <- ""
title_color <- mcols[1]
title_color2 <- mcols[6]
sub_color <- mcols[4]
sub_color2 <- mcols[10]
cap_color <- mcols[3]
cap_color2 <- 'grey90'
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

social_caption <- glue::glue(
  "<span style='font-family:fab6_reg;'>{github};</span> 
  <span style='color: {cap_color2}'>{github_username}  </span> 
  <span style='font-family:fab6_reg;'>{xtwitter};</span> 
  <span style='color: {cap_color2}'>{xtwitter_username}</span>")
plot_caption <- paste0(
  "**Data**: TidyTuesday Week 13<br>", 
  "**Graphics:** ", 
  social_caption)

cap_font <- 'second'

ptitle <- "NCAA MEN'S MARCH MADNESS"
title_font <- 'rso'

subtitle_win <- "Total Champioship Wins"

subtitle_pct <- str_wrap("Top 20 teams with highest winning percentage based on results from previous years", 50)

subtitle_font <- 'changa'

#==========================================================================#
# Custom Theme------------------------------------------------
#==========================================================================#

costum_theme1 <- function(){ 
  cfont <- "roboto"   
  theme_minimal(
    #base_family = cfont,
    #base_size = 40
  ) %+replace%    
    theme(
      plot.title = element_textbox(
        family = title_font, 
        color = title_color,
        hjust = 0.5, size = 4 * tsize,
        margin = margin(b  = .2, unit = 'cm')
      ),
      plot.subtitle = element_textbox(
        family = subtitle_font, 
        color = sub_color,
        hjust = 0.5, size = 3 * tsize,
        lineheight = .3,
        margin = margin(b  = .5, unit = 'cm')
      ),
      plot.caption = element_textbox(
        family = cap_font, 
        color = cap_color,
        margin = margin(t  = .5, unit = 'cm'),
        lineheight = .1, hjust = 1, size = 40
      ),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}

costum_theme2 <- function(){
  #cfont <- "roboto"
  theme_minimal(
    #base_family = cfont,
    #base_size = 40
  ) %+replace%
    theme(
        plot.title = element_textbox(
          family = title_font, 
          color = title_color2,
          hjust = 0.5, size = 4 * tsize,
          margin = margin(b  = .2, unit = 'cm')
        ),
        plot.subtitle = element_text(
          family = subtitle_font, 
          color = sub_color2,
          hjust = 0.5, size = 3 * tsize,
          lineheight = .3,
          margin = margin(b  = .5, unit = 'cm')
        ),
        plot.caption = element_textbox(
          family = cap_font, 
          color = cap_color2,
          margin = margin(t  = .5, unit = 'cm'),
          lineheight = .1, hjust = .5, size = 40
        ),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        
        plot.margin = margin(1, 5, 1, 5, unit = 'cm')
    )
}




#===========================================================================#
# Saving Plots and Gifs -----------------------------------------------------
#===========================================================================#

wins |> 
  ggplot(aes(champ, fct_reorder(team, champ))) +
  geom_col(width = .1) +
  geom_text(
    aes(label = team), x = 0, hjust = 0, vjust = -1,
    color = mcols[2], 
    size = .8 * tsize,
    family = 'bosc'
  ) +
  geom_text(aes(label = champ, x = champ), hjust = 1, vjust = -1,
            color = mcols[2], 
            size = .8 * tsize,
            family = 'bosc') +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL,
       title  = ptitle, subtitle = subtitle_win, caption = plot_caption) +
  costum_theme1()


wpercent20 |> 
  ggplot(aes(winpercent, fct_reorder(team, winpercent) )) +
  geom_col(width = .3, fill = mcols[5], alpha = .7) +
  geom_text(
    aes(label = team), x = 0, hjust = 0, vjust = -.5,
    color = mcols[10], 
    size = .6 * tsize,
    family = 'roboto') +
  geom_text(aes(label = paste0(wpct_fmt, "%"), x = winpercent), 
            hjust = 1, vjust = -.5,
            color = mcols[10], 
            size = .6 * tsize,
            family = 'roboto') +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL,
       title  = ptitle, subtitle = subtitle_pct, caption = plot_caption) +
  costum_theme2()



ggsave(
  filename = here::here('W13_MarchMadness', 'plots_w13', 'final_plot_w13.png'),
  width = 40, 
  height = 30, 
  units = "cm",
  bg = 'black'
)



