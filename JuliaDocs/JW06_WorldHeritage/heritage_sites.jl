#                                       World Heritage Sites                               #       
#                                     TidyTuesday  -  Week 6                            #
# ***************************************************************#


# Import packages ---------------------------------------------

using CSV, DataFrames, Pipe
using CairoMakie, Colors    # Colots installed as dep with   Makie
using AlgebraOfGraphics, DataFramesMeta


# Load Data ----------------------------------------------------

url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv"
raw = CSV.File(download(url)) |> DataFrame;


# EDA ----------------------------------------------------------
raw

df = stack(
    raw, 
    Not(:country), 
    variable_name = :year,  value_name = :total);

df

# VIZ ----------------------------------------------------------

Figure(
    backgroundcolor = :grey, size = (300, 300)
) # resultion deprecated => size = (300, 300)

Figure(
    backgroundcolor=colorant"#adc2eb",
    size = (600, 450)
)

ax = Axis(Figure()[1, 1])

fig = Figure(backgroundcolor=colorant"#adc2eb", size = (600, 450))
ax = Axis(fig[1, 1])
fig

fig = Figure(backgroundcolor=colorant"#adc2eb", size = (600, 450))
ax = Axis(fig[2, 2])
fig

fig = Figure(backgroundcolor=colorant"#adc2eb", size = (600, 450))
ax1 = Axis(fig[1, 1:2])
ax2 = Axis(fig[2, 1])
ax3 = Axis(fig[2, 2])
fig

# Labels
fig = Figure(backgroundcolor=colorant"#adc2eb", size = (600, 450))
ax = Axis(
    fig[1, 1],
    xlabel = "Xlabel", ylabel = "Ylabel", 
    title = "TITLE"
    )
fig



## basic plot
x = -10:.1:10 
y = sin.(x)

fig = Figure(backgroundcolor=colorant"#adc2eb", size = (600, 450))
ax = Axis(fig[1, 1], xlabel = "X", ylabel = "Sin(x)",  title = "SCATTER")
scatter!(ax, x, y)
fig


fig = Figure(backgroundcolor=colorant"#adc2eb", size = (600, 450))
ax = Axis(fig[1, 1], xlabel = "X", ylabel = "Sin(x)",  title = "LINE PLOT")
lines!(ax, x, y, color = :green, linewidth = 5)
fig

xs = 1:0.2:10
ys = 0.5 .* sin.(xs)
fig = Figure(backgroundcolor=colorant"#adc2eb", size = (600, 450))
ax = Axis(fig[1, 1], xlabel = "X", ylabel = "Sin(x)",  title = "LINE PLOT")
barplot(xs, ys, gap = 0, color = :gray55, strokecolor = :black, strokewidth = 1)



age_groups = ["0-12", "13-24", "25-36", "37-48", "49-60"]
num_individuals = [35, 23, 17, 13, 12]

f, ax, bp = barplot(
    num_individuals,
    color=:black,
    strokecolor=:white,
    strokewidth=1,
    width=1,
    gap=0.2,
    axis = (
            xlabel="Age Groups (in African Elephant years)",
            ylabel="Relative Abundance (%)",
            title="Type A Profile",
            limits=(0.5, 5.5, 0, 35),
            yticks=(0:5:35),
            xticks=(1:5, age_groups),
            xticklabelsize = 14,
            xminorticks = 0.5:1:5.5,
            xminorticksvisible = true,
            xminorticksize = 8,
            topspinevisible=false,
            rightspinevisible=false,
            xgridvisible=false,
            ygridvisible=false,
            xticksvisible=false
    ),
    figure = (font = "Arial", resolution = (600, 450))
)


f, ax, bp = barplot(
    df.total,
    axis = (
           title="Barplot",
            limits=(0.5, 6.5, 0, 18),
            yticks=(0:5:20),
            xticks=(1:6, df.country),
    ),
    figure = (font = "Arial", resolution = (600, 450))
)


tbl = (x = [1, 1, 2, 2, 3, 3],
       height = [5, 4, 13, 8, 10, 15],
       year = [1,2, 1,2, 1,2],
       grp = [1,2,1,2,1,2]
       )

barplot(tbl.x, tbl.height,
        stack = tbl.grp,
        color = tbl.grp,
        axis = (xticks = (1:3, ["Norway", "Denmark", "Sweden"]),
                title = "Stacked bars"),
        )

barplot(tbl.x, tbl.height,
    dodge = tbl.year,
    color = tbl.year,
    axis = (
        xticks = (1:3, ["Norway", "Denmark", "Sweden"]),
        title = "Stacked bars"),
        )

barplot(
    df.total,
    color = [1,2,3, 1,2,3],
    axis = (
        xticks = (1:6, df.country),
        title = "Stacked bars"),
        )

#

## Algebra of graphics =========================================
# https://tutorials.pumas.ai/html/PlottingInJulia/01-AoG-intro.html
axis = (width = 225, height = 225)
dfb =  data(df) * 
    mapping( :country,  :total, color = :year, dodge = :year) * 
    visual(BarPlot)
draw(dfb; axis = axis)

data(@by df :country :total :year) *
    mapping(:country, :total, color = :year, dodge = :year) *
    visual(BarPlot) |> draw

data(df ) * 
    mapping(:country, :total, color = :year, stack = :year) *
    visual(BarPlot) |> draw

plt = data(df ) * 
    mapping(:country, :total, color = :country, col = :year) *
    visual(BarPlot)
draw(plt;)

plt = data(df ) * 
    mapping(:year, :total, color = :country, dodge = :country) *
    visual(BarPlot)

pp  =  draw(plt;
    axis = (;
        title = "UNESCO WORLD HERITAGE",
        aspect = 4 / 3,
        ylabel = "Total Sites",
        xticksvisible = false,
        xgridvisible = false,
        xlabel = "",),
    figure = (;
        size = (600, 600),
        figure_padding = 6,
        backgroundcolor = :white,
        fontsize = 16,),
    palettes = (; color = [:tomato, :black, :dodgerblue])
)

pp
save("my_image.png", pp; px_per_unit = 3)
 