#
# Educational attainment of young people in English towns
#----------------------------------------------------------#

# Loading Packages and Data -----------------------------#

using CSV, DataFrames, Plots, StatsPlots
using Downloads, Statistics, Dates


url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv"
raw =  CSV.File(Downloads.download(url)) |> DataFrame;
first(raw, 3)
names(raw)

ds  = select(raw, 
:town11nm => :code, :population_2011 => :pop2011, :size_flag => :size, :rgn11nm => :region, 
:coastal  => :coastal, :job_density_flag => :jobcat, :income_flag => :income,
:level4qual_residents35_64_2011 => :resid_3564_q4, :education_score => :score, 
:activity_at_age_19_employment_with_earnings_above_10_000 => :earning
)


# EDA  ----------------------------------------------------------#

ds |> names
ds |> describe

df = @pipe ds |> 
    dropmissing  |>
    filter(x -> x.pop2011 !=  "NA", _) |> 
    filter(x -> x.earning !=  "NA", _) |>
    transform(_, 
        :pop2011 => ByRow(x -> parse(Int64, x)) => :pop2011,
        :earning => ByRow(x -> parse(Float64, x)) => :earning)

    df |> describe


#  Pop and earning

boxplot(df.earning)

histogram(
    df.earning, color =  "wheat", 
    label = false, title = "Earning Distribution For Adults 34-65 With Level 4+"
)


boxplot(
    df.jobcat, df.earning, group= df.jobcat,
    label = false,
    color = [:cyan :orange :brown],
    title = "Earning Distribution By Type Of Towns",
    )

    @df df violin(
        :jobcat, :earning, group = :jobcat,
        label =  false,
        palette= :Dark2_3, #ColorBrewer.palette("Set1", 9)),
        title = "Earning Distribution By Type Of Towns"
    )

# jobcat

job_df = @pipe df |> 
    filter(x -> x.jobcat !=  "NA", _) |>
    groupby(_, :jobcat) |>
    combine(_,  nrow => :total)

bar(
    job_df.jobcat, job_df.total, 
    label = false,
    color = [:cyan, :orange, :brown],
    title = "Towns Distribution By Type",
    ylabel = "Frequency")




# jobcat

job_df = @pipe df |> 
    filter(x -> x.jobcat !=  "NA", _) |>
    groupby(_, :jobcat) |>
    combine(_,  nrow => :total)

bar(
    job_df.jobcat, job_df.total, 
    label = false,
    color = [:cyan, :orange, :brown],
    title = "Towns Distribution By Type",
    ylabel = "Frequency")
@pipe ds |>
    groupby(_, :jobcat) |>
    combine(_, :jobcat  => nrow)

combine(groupby(df, :jobcat), :jobcat  => nrow)