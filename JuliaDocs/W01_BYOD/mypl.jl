
# Premier League Table

using CSV, DataFrames, Plots

df = CSV.File("pl2023_2024.csv") |> DataFrame;

first(df, 3)

