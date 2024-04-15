#                                       US Maps - Eclipses / Unemployment                              #       
#                                     TidyTuesday  -  Week 15                            #
# ***************************************************************#


# Import packages ---------------------------------------------

using CSV, DataFrames
using VegaLite, VegaDatasets
using Chain
using Statistics


# Load Data ----------------------------------------------------

main_page = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/"
ccc = "data/2023/2023-05-09/childcare_costs.csv"
counties = "data/2023/2023-05-09/counties.csv"

df1 = CSV.File(download(main_page * ccc)) |> DataFrame;
df2 = CSV.File(download(main_page * counties)) |> DataFrame;

# EDA ----------------------------------------------------------

## Subsetting: Unemployment  and county data ----------------
df1 |> names
vscodedisplay(df1)

unemp_data = @chain df1  begin
    select(
        :county_fips_code => :fips,
        :study_year => :year,
        :unr_16 => :rate,
        :funr_16 => :female,
        :munr_16 => :male
    )
end;
first(unemp_data, 3)
county_data = @chain  df2   begin
    select(
        :county_fips_code => :fips,
        :county_name => :county,
        :state_name => :state
    )
end;
first(county_data, 3)

## Filter 2018 data  and merging with  county_data ---------

ud_2018 = @chain unemp_data begin
    filter(x -> x.year == 2018, _)
end;

ud_joined_by_fips = leftjoin(ud_2018, county_data, on = :fips);
first(ud_joined_by_fips, 3)


## Average  rate by  state:

avg_df = @chain ud_joined_by_fips begin
    groupby(_, :state)
    combine(_,
        :rate  => mean => :avg_rate,
        :female   => mean => :avg_female,
        :male  => mean => :avg_male   
    )
end;


##  Basic US map (doggo dot jl tutorial) ------
### https://www.youtube.com/watch?v=mptWWrScdS4&t=429s
us10m = dataset("us-10m")

usmap = @vlplot(
    width = 640,
    height = 360,
    title = "US Map by State",
    projection = {type = :albersUsa},
    data = {
        values = us10m,
        format = {
            type = :topojson,
            feature = :states
        }
    },
    mark = {
        type = :geoshape,
        fill = :lightgray,
        stroke = :white
    }
)

# how to save plot#
#save("p_us_states.svg", p)


vega_u = dataset("unemployment")
vega_h = dataset("population_engineers_hurricanes")
income = dataset("income")


# using VegaDatasets
unemployment = dataset("unemployment")

canvas_1 = @vlplot(
    width = 640,
    height = 360,
    title = "US Unemployment Rate by County (year unknown)",
    projection = {type = :albersUsa}
);

usmap_vl = @vlplot(
    data = {
        values = us10m,
        format = {
            type = :topojson,
            feature = :counties
        }
    },
    transform = [{
        lookup = :id,
        from = {
            data = unemployment,
            key = :id,
            fields = ["rate"]
        }
    }],
    mark = :geoshape,
    color = "rate:q"
);

p = canvas_1 + usmap_vl



# using by  state data set


# using ud_joined_by_fips data set

# county
canvas_2 = @vlplot(
    width = 640,
    height = 360,
    title = "US Unemployment Rate by County (2018)",
    projection = {type = :albersUsa}
);

usmap_fips = @vlplot(
    data = {
        values = us10m,
        format = {
            type = :topojson,
            feature = :counties
        }
    },
    transform = [{
        lookup = :id,
        from = {
            data = ud_joined_by_fips,
            key = :fips,
            fields = ["rate"]
        }
    }],
    mark = :geoshape,
    color = "rate:q"
);
p = canvas_2 + usmap_fips

#  state
state_code = DataFrame(CSV.File("usa_code.csv"))
first(state_code, 6)
state_code2 = @chain state_code begin
    dropmissing
    select(_, :state, :code)
end
first(state_code2, 6)

avg_joined = leftjoin(avg_df, state_code2, on=:state)

#### all gender
canvas_3 = @vlplot(
    width = 640,
    height = 360,
    title = {
        text = "US Unemployment Rate by State",
        color = :grey, fontSize = 30,  font = "Roboto", fontWeight = "bold",
        subtitle =  "Unemployment rate of the population aged 16 years old or older in 2018",
        subtitleColor = :brown, subtitleFont = "JuliaMono", 
        subtitleFontWeight = "bold", subtitleFontSize = 14,
    },
    projection = {type = :albersUsa}
);

usmap_fips_3 = @vlplot(
    data = {
        values = us10m,
        format = {
            type = :topojson,
            feature = :states
        }
    },
    transform = [{
        lookup = :id,
        from = {
            data = avg_joined,
            key = :code,
            fields = ["avg_rate"]
        }
    }],
    mark = :geoshape,
    color = {
        "avg_rate:q",
        legend={title="Rates"},
        scale = {
            scheme = "greys"
        }
     }
);
p = canvas_3 + usmap_fips_3

#### Males
canvas_m = @vlplot(
    width = 640,
    height = 360,
    title = {
        text = "US Unemployment Rate by State",
        color = :grey, fontSize = 30,  font = "Boska", fontWeight = "bold",
        subtitle =  "Unemployment rate of the males aged 16 years old or older in 2018",
        subtitleColor = :steelblue, subtitleFont = "losevka", 
        subtitleFontWeight = "bold", subtitleFontSize = 16,
    },
    projection = {type = :albersUsa}
);
usmap_fips_m = @vlplot(
    data = {
        values = us10m,
        format = {
            type = :topojson,
            feature = :states
        }
    },
    transform = [{
        lookup = :id,
        from = {
            data = avg_joined,
            key = :code,
            fields = ["avg_male"]
        }
    }],
    mark = :geoshape,
    color =  {
       "avg_male:q",
        legend={title="Rates"},
        scale =  {
            scheme =  "redblue" 
        }
    },

);
p = canvas_m + usmap_fips_m

#### Fenales
canvas_f = @vlplot(
    width = 640,
    height = 360,
    title = {
        text = "US Unemployment Rate by State",
        color = :dodgerblue, 
        fontSize = 26,  
        font = "Roboto", 
        fontWeight = "bold",
        subtitle =  "Unemployment rate of the females aged 16 years old or older in 2018",
        subtitleColor = :crimson, subtitleFont = "Helvetica Neue", 
        subtitleFontWeight = "bold", subtitleFontSize = 16,
    },
    projection = {type = :albersUsa}
);
usmap_fips_f = @vlplot(
    data = {
        values = us10m,
        format = {
            type = :topojson,
            feature = :states
        }
    },
    transform = [{
        lookup = :id,
        from = {
            data = avg_joined,
            key = :code,
            fields = ["avg_male"]
        }
    }],
    mark = :geoshape,
    color =  {
        "avg_male:q",
        legend={
            title="Rates", 
            titleColor = :navy
            },
        scale =  {
            scheme =  "darkred" 
        }
    }
);
p = canvas_f + usmap_fips_f

# World population
world110m = dataset("world-110m")
df_country = DataFrame(CSV.File("country_code.csv"));
west = [35, -15, -2.5]
east = [-70, -12.5, -7]

# canvas
canvas_world_ortho = @vlplot(
    width = 640,
    height = 360,
    title = "2019 Population by Country",
    projection = {
        type = :orthographic,
        rotate = east
    }
);
canvas_world_topo = @vlplot(
    width = 640,
    height = 360,
    title = "2019 Population by Country",
    #projection = {type = :topojson}
);

# world map
worldmap = @vlplot(
    data = {
        values = world110m,
        format = {
            type = :topojson,
            feature = :countries
        }
    },
    transform = [{
        lookup = :id,
        from = {
            data = df_country,
            key = :id,
            fields = ["my19"]
        }
    }],
    mark = :geoshape,
    color = "my19:q"
);

# combine layers
p = canvas_world_ortho + worldmap
p = canvas_world_topo + worldmap