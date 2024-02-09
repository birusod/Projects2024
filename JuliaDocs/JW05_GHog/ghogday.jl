# Groundho Day 2024 -  TidyTuesday Week 05
#******************************************************
##  Packages:  *
# ******************************************************
using CSV, DataFrames, Chain, Pipe
using Statistics, Plotly, PlotlyJS


##  Data:
# ******************************************************
data_url =  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv"
drw = CSV.File(download(data_url)) |> DataFrame;
drw  |> size
drw  |> names
first(drw, 4)
drw |> describe
## EDA: 
# ******************************************************

# Selecting and renaming coliumns
@pipe drw |>
    select(_, :region, :country, :type, :predictions_count)  |>
    rename(_, :predictions_count => :pred)

df = @pipe drw |>
    select(_, :region, :country, :type, :predictions_count  => :pred)

# Regions 
regions1 = @pipe df |>
    groupby(_ , :region) |>
    combine(_ , nrow => :total)  |>
    filter(x  -> x.total >= 3, _ )

regions2 = @pipe df |>
    groupby(_ , :region) |>
    combine(_ , nrow => :total)  |>
    filter(x  -> x.total < 3, _ ) |>
    combine(_, :total => sum)

regions3 = vcat(
    regions1,
    DataFrame(region= "Other", total=35)) 

regions4 = @pipe regions3 |> sort(_, :total, rev = true)

plot(regions4, x=:region, y=:total, kind="bar")

plot(
    regions4, 
    x=:region, y=:total, kind="bar",
    color=:region, 
    config=PlotConfig(responsive=false),
    Layout(
        yaxis_visible=true,
        xaxis_visible=true,  
        showlegend=false))

plot(
    regions4, 
    x=:region, y=:total, 
    kind="bar", 
    text = :total, textposition="outside",
    marker=attr(
        color="rgb(158,202,225)", 
        line_color="rgb(8,48,107)", 
        line_width=1.5, 
        opacity=0.6), 
    Layout(
        yaxis_visible=false,
        title_text="Groundhog Day 2024")
)



plot(
    bar(
        regions4, 
        x=:region, y=:total,  textposition="outside",
        marker=attr(
            color=:orange, 
            line_color=:crimson, 
            line_width=2, 
            opacity=0.7)
        ),
        Layout(
            font_family="Roboto",
            font_color="grey",
            title=attr(
                text="<b>Groundhog Day 2024:</b><i> Distribution By State</i>",
                font_family="Times New Roman", # Courier New
                font_color="RebeccaPurple",
                font_size = 25)
                )
        )