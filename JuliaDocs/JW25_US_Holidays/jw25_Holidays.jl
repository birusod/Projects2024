#++++++++++++++++++++++++++++++++++++++++++#
#              US HOLIDAYS                 #
#++++++  TidyTuesday | Week 25  +++++++++++#

# Setup ####################################

# Libraries
using CSV, DataFrames, Pipe
using SummaryTables
using Statistics


# Data
main_url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/"
dat_url = "data/2024/2024-06-18/federal_holidays.csv"
main_url * dat_url
dfr = CSV.File(download(main_url * dat_url)) |> DataFrame;


# EDA ####################################

# Overview

dfr |> names

dfr |> size

first(dfr, 3)

dfr |> describe

# Regex and string review
name = "Birusod13"
replace(name, r"[0-9]" => "")
replace("Python is a programming language.", "Python" => "Julia")

occursin(r"^\s*(?:#|$)", "# a comment")
occursin(r"^\s*(?:#|$)", "a comment #")
occursin(r"^\s*(?:#|$)", "a comment")

replace("a-bcd", r"(a|\-|b)" => "H" )

# Create and Select columns

dff = @pipe dfr |>
    select(_, 
        :official_name => :holiday, 
        :date_definition => :definition,
        :year_established => :year,
        :date,
        :details) |>
    transform(_,
        :date => ByRow(x -> replace(x, r"([0-9]|\s|\–)" => "")) => :date,
        :definition => ByRow(x -> occursin("fixed", x) ? "Yes" : "No") => :fixed) |>
    rename(_, :date => :month) |>
    select(_, 1, 3:4, 6, 5)


# EDA ####################################

# example
data = DataFrame(
    value = 1:3, 
    group1 = repeat(["A", "B", "C"]))



df_stacked = stack(dff, [:year, :month, :fixed, :details], :holiday)

listingtable(
    df_stacked, 
    :value, 
    rows = :holiday, 
    cols = :variable => "US Holidays",
    variable_header = false,
    sort = false
    )