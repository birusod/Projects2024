# Canadian Hockey Players Data  Analysis

## packages ----------------------------------------------------
using CSV
using DataFrames
using CategoricalArrays
using StatsPlots
using Pipe
using Downloads, Statistics, Dates

## Data----------------------------------------------------------
url_data = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv"
df_raw = CSV.File(Downloads.download(url_data)) |> DataFrame;

## Data Overview------------------------------------------------
df_raw |> nrow
df_raw |> ncol
df_raw |> names
df_raw |> describe
describe(df_raw, :nmissing)
describe(df_raw, :eltype)
eltype.(eachcol(df_raw))
Dict(names(df_raw) .=> eltype.(eachcol(df_raw)))

first(df_raw, 4)
first(df_raw[!, 15:18], 4)

## Selecting Columns of interest ---------------------------------
dfs = @pipe df_raw |> select(_, 
    :player_id, :season,
    :position_type => :position,
    :team_code => :team,
    :shoots_catches => :catches,
    :height_in_centimeters => :height,
    :weight_in_kilograms => :weight,
    :birth_date => :dob,
    :birth_city => :city,
    :birth_country => :country,
    :birth_state_province => :province,
)

## Coverting Columns Types -------------------------------------
describe(dfs)

dfs1 = @pipe dfs |>
    transform(_, 
        :player_id => ByRow(string) => :player_id,
        :season => ByRow(string) => :season) |>
    filter([:height , :weight] => (height, weight)  -> height !=  "NA"  && weight !=  "NA", _) |>
    transform(_,  
        :height => ByRow(x -> parse(Int64, x)) => :height,
        :weight => ByRow(x -> parse(Int64, x)) => :weight
    )

## Filtering Unique Row for each  player --------------------------------------
dfs1 |> nrow
dfc = unique(dfs1, :player_id)


## Adding new Columns ----------------------------------------------------

top9 = @pipe dfc  |>
    groupby(_, :country) |>
    combine(_,  nrow => :total) |>
    sort(_, :total, rev  = true) |>
    first(_, 9)

@pipe dfc |> transform(_, [:height, :weight] =>  ByRow((hgt, wgt) ->  hgt > 190 || wgt > 90 ? "large" : "other") =>  :type)
@pipe dfc |> transform(_, :country =>  ByRow(x ->  x == "CAN" ? "Canada" :  x) =>  :type)

dff = @pipe dfc |> 
    transform(_, 
        :country => ByRow(x ->   x == "CAN"  ? x :  x == "USA"  ? x : "WORLD" ) =>  :group,
        :country => ByRow(x -> in(x, top9.country) ? x : "Other") => :top10,
        :dob => ByRow(x -> (year(x), monthname(x), dayname(x))) => [:year, :month, :day],
        :season => ByRow(x -> parse(Int64, x[1:4])) => :startyear) |>
    transform(_,
        [:startyear, :year] => ByRow((x, y) -> x-y) => :age
    )



## Counting --------------------------------------------------------
dff |> describe

top10c1 = @pipe dff |>
    groupby(_, :top10) |>
    combine(_, nrow => :total) |>
    rename(_, :top10 => :country) |>
    sort(_, :total) |>
    transform(_, :country => ByRow(x -> x == "Other" ? "grey" : "cyan") => :clr)

bar(top10c1.country, top10c1.total, label = false)
bar(
    top10c1.country, 
    top10c1.total, label = false, permute = (:x, :y),
    color = :cyan)

cls = [:orange, :grey]
@df top10c1 bar(
    :country, :total,
    label = false, permute = (:x, :y),
    title  = "Players Country Of Birth: Top 10  Country",
    color  = :clr
    )

by_group = @pipe dff |>
    groupby(_, :group) |>
    combine(_, nrow => :total)

@df by_group bar(
    :group, :total,
    label = false,
    title  = "Players Group of Country",
    color  = [:orange, :grey, :cyan]
    )


by_position = @pipe dff |>
    groupby(_, :position) |>
    combine(_, nrow => :total)

@df by_position bar(
    :position, :total,
    label = false,
    title  = "Players Position of Play",
    color  = [:magenta, :grey, :crimson]
    )

by_position_group = @pipe dff |>
    groupby(_, [:position, :group]) |>
    combine(_, nrow => :total)

@df by_position_group groupedbar(
    :position, :total, group = :group, color  = [:magenta :grey :crimson],
    bar_width = 0.9, lw = 0, framestyle = :box,
    title  = "Players Position of Play By Group",
    )
@df by_position_group groupedbar(
    :group, :total, group = :position, color  = [:orange :grey :dodgerblue],
    bar_position = :stack,
    lw = 0, framestyle = :box,
    title  = "Players Position of Play By Group",
    ylims= [0, 6000]
    )

# chaining with plot
@pipe dff |>
    groupby(_, :group) |>
    combine(_, nrow => :total) |>
    @df bar(:group, :total)

## Calculating averages for height, weight, age -------------------------------------

@pipe dff |>
    groupby(_, :group) |>
    combine(_, [:height] => mean)

@pipe dff |>
    groupby(_, :group) |>
    combine(_, [:height] => round ∘ mean => :avgh)

@pipe dff |>
    groupby(_, :group) |>
    combine(_, 
        [:height, :weight, :age] .=> round ∘ mean .=> [:avg_hgt, :avg_wgt, :avg_age])


roundedMean(data_col) = round(Int, mean(data_col))
@pipe dff |>
    groupby(_, :group) |>
    combine(_, 
    :height => roundedMean => :avg_hgt)

roundedMean2(data_col) = round(mean(data_col), digits = 2)
@pipe dff |>
    groupby(_, :group) |>
    combine(_, 
    :height => roundedMean2 => :avg_hgt)

@pipe dff |>
    groupby(_, :group) |>
    combine(_, 
        [:height, :weight, :age] .=> roundedMean2 .=> [:avg_hgt, :avg_wgt, :avg_age])

round(3.1456, digits = 3)

## Age-Weight-Height: Box, Violin and dotplots -----------------------------------------

@df dff violin(
    string.(:group), :age, lw = 0, label = false,
    title = "Age Distribution By Group")
@df dff boxplot!(
    string.(:group), :age, label = false,
    inewidth= 2, fillalpha=0.7, color = :orange)
@df dff dotplot(
    string.(:group), :age, label = false, title = "Age Distribution By Group",
    marker = (:red, stroke(0), size(1)))



## Pivoting (stack / unstack) : wide  to long ---------------------------------------
dff |> describe
@pipe dff |>
    select(_, :player_id, :height, :weight, :age) |>
    stack(_, Not(:player_id), variable_name = :measure, value_name = :vals) |>
    @df density(
        :vals, group = :measure, layout = 3,
        plot_title = "Age-Weight-Height Of Players")


histogram(dff.age)
@pipe dff |>
    select(_, :catches, :player_id, r"ght") |>
    select(_, :player_id, :) |>
    stack(_, r"ght") |>
    @df groupedhist(:value, group = :variable, bar_position = :stack)

@pipe dff |>
    select(_, :catches, :player_id, r"ght") |>
    select(_, :player_id, :) |>
    stack(_, r"ght") |>
    @df groupedhist(:value, group = :variable, layout=2)

@pipe dff |>
    filter(x -> x.catches != "NA", _) |>
    @df groupedhist(:age, group = :catches, bar_width = 1)

@pipe dff |>
    filter(x -> x.catches != "NA", _) |>
    @df groupedhist(:age, group = :catches, bar_width = 1, bar_position = :stack) # :overlay

@pipe dff |>
    filter(x -> x.catches != "NA", _) |>
    @df histogram(:age)