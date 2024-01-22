# US Polling Places 2012-2020

# load packages:
using CSV, Gadfly
using Tidier, TidierStrings
using Chain
using Statistics, Downloads

# load data
urld = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-16/polling_places.csv"
dfr = CSV.File(
    Downloads.download(urld); 
    types=Dict(:jurisdiction_type => String, :location_type => String)
    ) |> DataFrame;

first(dfr, 5)
dfr |> describe
describe(dfr, :eltype)
Dict(names(dfr) .=> eltype.(eachcol(dfr)))

using DataFrames
mapcols(eltype, dfr) # DataFrames
dfr |> names

# Wrangling

# selecting columns
@chain dfr begin
    select(
        :election_date => :date,
        :state, 
        :jurisdiction_type => :type,
        :location_type => :location
        )
    first
end

df = @chain dfr begin
    @select(
        date = election_date,
        state, 
        type =  jurisdiction_type,
        location = location_type
        )
end;

# dropping missing
df |> describe

# counting by type
df |>
    groupby(_, :type)
    combine(_, nrow => :total)

bytype = @chain df begin
    @group_by(type)
    @summarise(total = nrow())
    @filter(type != "NA")
    #@mutate(type2 = convert(String, type))
    #@mutate(type2 = str_replace_all(type, "county_", " "))
    @mutate(type = case_when(
        type == "county_municipality" => "countymunic", 
        true => type))
    @arrange(total)
    @mutate(type = str_to_upper(type))
end


byloc = @chain df begin
    @group_by(location)
    @summarise(total = nrow())
    @filter(location != "NA")
    #@mutate(location = str_replace(location, "bo", "ville"))
    @arrange(total)
    @mutate(
        location = case_when(
            location == "early_vote" => "Early vote", 
            location == "early_vote_site" => "Early vote", 
            location == "vote_center" => "Vote center", 
            location == "election_day" => "Election day", 
            location == "polling_location" => "Polling location", 
            location == "polling_place" => "Polling place"))
end




##  TidierStrings
1  + 1
dd = DataFrame(
  Names = ["Alice", "Bob", "Charlie", "Dave", "Eve", "Frank", "Grace"],
  City = ["New York        2019-20", "Los    \n\n\n\n\n\n    Angeles 2007-12 2020-21", "San Antonio 1234567890         ", "       New York City", "LA         2022-23", "Philadelphia            2023-24", "San Jose               9876543210"],
  Occupation = ["Doctor", "Engineer", "Final Artist", "Scientist", "Physician", "Lawyer", "Teacher"],
  Description = ["Alice is a doctor in New York",
                 "Bob is is is an engineer in Los Angeles",
                 "Charlie is an artist in Chicago",
                 "Dave is a scientist in Houston",
                 "Eve is a physician  in Phoenix",
                 "Frank is a lawyer in Philadelphia",
                 "Grace is a teacher in San Antonio"]
)
@chain dd begin
    @mutate(split = str_remove_all(Description, "is"))
end


## Plotting with Gadfly

Gadfly.with_theme(:dark) do
    plot(bytype, x=:type, y=:total, color=:type, Geom.bar())
end




Gadfly.with_theme(:dark) do
    plot(
        byloc, x=:location, y=:total, color = :location,
        Geom.bar,
	    Guide.xlabel("Polling Type"),
	    Guide.ylabel("Frequency"),
	    Guide.title("Polling Types Disctribution"),
	    Scale.y_continuous(format = :plain),
	    Theme(background_color = "ghostwhite", bar_spacing = 1mm))
end


plot(
    byloc, x=:location, y=:total, color = :location,
    Geom.bar,
	Guide.xlabel("Polling Locations"),
	Guide.ylabel("Frequency"),
	Guide.title("Polling Locations Disctribution"),
	Scale.y_continuous(format = :plain),
	Theme(bar_spacing = 2mm)
    )