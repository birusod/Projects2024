
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue,
  monochromeR)


# Loading data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-01-23")
tuesdata

dfr <- tuesdata$english_education
colSums(is.na(dfr))
dfr |> summarise_all(~ sum(is.na(.))) |> t()
dfr |> summarise(across(everything(), \(x) sum(is.na(x))))
dfr %>% summarise(across(everything(), ~sum(is.na(.))))

dfr |> 
  map_df(function(x) sum(is.na(x))) |> 
  #gather(colname, num_nulls) |> 
  pivot_longer(everything(), names_to = 'colname', values_to = 'num_nulls') |> 
  arrange(desc(num_nulls))


# Loading fonts ------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Fira code", "firac")
font_add_google("Fira sans", "firas")
showtext_auto()


# Defining colors ---------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""

jobcat_pal <- c(Mixed = "#798E87", 
                Residential = "#8347C5", 
                Working = "#a45e41")

text_pat <- c(light_text = "#323A30",
              dark_text =  "#0C1509")

monochromeR::view_palette(jobcat_pal)
text_pat |> view_palette()

# Wrangling Data ----------------------------------------------------------
df <-  dfr |> 
  select(
    code = town11nm,
    pop = population_2011, 
    size = size_flag,
    region = rgn11nm,
    coastal,
    jobcat = job_density_flag,
    income = income_flag,
    resid_3564_q4 = level4qual_residents35_64_2011,
    score = education_score,
    earning = activity_at_age_19_employment_with_earnings_above_10_000) |> 
  drop_na()
df |> glimpse()
df |> group_by(size) |> summarise(avg = mean(score))
df |> group_by(region) |> summarise(avg = mean(score))
df |> group_by(coastal) |> summarise(avg = mean(score))
df |> group_by(jobcat) |> summarise(avg = mean(score))
df |> group_by(income) |> summarise(avg = mean(score))



# Define texts and annotations --------------------------------------------

# social <- nrBrand::social_caption(
#   bg_colour = bg_col,
#   icon_colour = highlight_col,
#   font_colour = text_col,
#   font_family = "roboto"
# )
ptitle <- "Educational Attainment of Young People in English Towns" 
psubt <- paste0(
  "Proportion of 2012-2013 key stage 4 cohort in sustained employment earning",
  expression(">=£10,000."), "<br>",
  " Comapring Towns described as ",
  "<span style='color:", jobcat_pal['Mixed'], "'>**Mixed**</span>, ", 
  "<span style='color:", jobcat_pal['Residential'], "'>**Residential**</span> ",
  "<span style='color:", jobcat_pal['Working'], "'>and **Working**</span>.")
pcap <- "TidyTuesday Data | Week4"

# Data Viz -------------------------------------------------------------------

df |> 
  mutate(region = fct_infreq(region)) |> 
  group(region, jobcat, name = 'total') |> 
  summarise(avg = mean(score)) |> 
  
  ggplot(aes(total, region, fill = jobcat)) +
  geom_col(position = 'dodge') +
  #facet_wrap(~jobcat) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(
    breaks = c(0, 30, 60, 90),
    expand = c(0,0)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 5))  +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

df_stats <- df |> 
  mutate(resid_3564_q4 = fct_relevel(resid_3564_q4, 'Low', 'Medium', 'High')) |> 
  group_by(resid_3564_q4, jobcat) |> 
  summarise(avg = round(mean(earning),1), .groups = 'drop') 

df_stats |> 
  ggplot(aes(avg, resid_3564_q4, fill = jobcat)) +
  geom_col(show.legend = FALSE) +
  geom_label(
    aes(label = avg), 
    fill  = NA, label.size = NA, hjust = 1, 
    color = 'white', fontface = 'bold') +
  labs(x = NULL, y = NULL) +
  facet_wrap(~jobcat, ncol = 1)

p1 <- df_stats |> 
  ggplot(aes(avg, resid_3564_q4, fill = jobcat)) +
  geom_col(show.legend = FALSE) +
  geom_textbox(
    aes(label = paste0("<span style=font-size:8pt>Type: ", 
                       resid_3564_q4, "</span> | ", 
                       "<span style=font-size:8pt>Score: ",
                       avg, "</span>")), 
    fill  = NA, box.color = NA, 
    hjust = 1, halign = 1,
    color = 'white', fontface = 'bold'
    ) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(
    expand = c(0, 0)) +
  facet_wrap(~jobcat, ncol = 1) 
p1


p1 +
  scale_fill_manual(values = jobcat_pal) +
  labs(title = ptitle, subtitle = psubt, caption = pcap) +
  theme_minimal(
    base_size = 10,
    base_family = 'roboto'
  ) +
  theme(
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = .01, face = 'bold', size = rel(1)),
    plot.margin = margin(t = .5, r = 1, l = 1, unit = 'lines'),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.title = element_textbox_simple(
      color = text_pat['dark_text'],
      size = rel(1.2), face = 'bold', family = 'firac',
      margin = margin(b = .5, unit = 'lines')),
    plot.subtitle = element_textbox_simple(
      color = text_pat['light_text'],
      size = rel(1), face = 'italic', family = 'firas',
      margin = margin(b = .5, unit = 'lines')),
    plot.caption = element_text(face = 'italic', color = 'grey70')
  )


# geaom_text example with  symbols
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  facet_wrap(~Species) +
  scale_color_manual(
    name = NULL,
    values = c(setosa = "#0072B2", virginica = "#009E73", versicolor = "#D55E00"),
    labels = c(
      setosa = "<i style='color:#0072B2'>I. setosa  &mu; </i>",
      virginica = "<i style='color:#009E73'>I. virginica  &mu; </i>",
      versicolor = "<i style='color:#D55E00'>I. versicolor  &mu; </i>")
  ) +
  labs(
    title = "**Fisher's *Iris* dataset  (test unicode symbol: &mu;)**  
    <span style='size:8'>Sepal width vs. sepal length for three *Iris*
    species  &mu; </span>",
    x = "Sepal length (cm)<br>(test unicode symbol: &mu;)", 
    y = "Sepal width (cm)<br> (test unicode symbol: &mu;)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
    legend.text = element_markdown(size = 11),
    axis.title.x = element_markdown(hjust = 0.5),
    axis.title.y = element_markdown(vjust = 0.5)
  )
# Saving Plots and Gifs ------------------------------------------------------

# save the final plot as 'final_plot'
ggsave(
  filename = file.path(
    "W4_EnglishEdu",
    "plots_w4", 
    paste0("final_plot", ".png"))
  )
