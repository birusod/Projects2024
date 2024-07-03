### A Pluto.jl notebook ###
# v0.19.43

using Markdown
using InteractiveUtils

# This Pluto notebook uses @bind for interactivity. When running this notebook outside of Pluto, the following 'mock version' of @bind gives bound variables a default value (instead of an error).
macro bind(def, element)
    quote
        local iv = try Base.loaded_modules[Base.PkgId(Base.UUID("6e696c72-6542-2067-7265-42206c756150"), "AbstractPlutoDingetjes")].Bonds.initial_value catch; b -> missing; end
        local el = $(esc(element))
        global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : iv(el)
        el
    end
end

# ╔═╡ 1d6087d8-2a51-4244-a4d6-1f45b4789cec
begin using PlutoUI; TableOfContents() end

# ╔═╡ 285ebfba-78c7-4409-b930-943899531262
using CSV, Pipe, DataFrames

# ╔═╡ da9f16ee-621d-4c03-b92d-96543f9ddf4b
using VegaLite, VegaDatasets

# ╔═╡ a4be5a19-1491-48e1-a3f0-d4a3d420ff73
using Statistics

# ╔═╡ 673545b9-cda8-4ace-b75d-3d5425967665
using FreqTables

# ╔═╡ c85a6852-2e99-11ef-0c75-b7a7215f6d0a
html"""
<div style="position: absolute; width: calc(100% - 30px); border: 50vw solid purple;
border-top: 500px solid purple; border-bottom: none; box-sizing: content-box;
left: calc(-50vw + 15px); top: -500px; height: 160px; pointer-events: none;">
</div>

<div style=" height: 160px; width: 300%; background: purple; color: purple;
padding-top: 5px; padding-left: 5px;
">

<span style="font-family:Fira;font-weight:200;font-feature-settings:'lnum','pnum';"> 

<p style=" font-family: Roboto; font-size: 2rem; font-weight: 700; opacity: 1.0;
color: #ffa600;
	">CAMPUS PRIDE INDEX
</p>

<p style="text-align: left; font-size: 1.8rem; color: gold">
	TidyTuesday Data: Week-24
</p>
<p style=" font-family: 'Julia mono'; font-size: 1rem; font-weight: 200; color: crimson;">
	&copy birusod
</p>
"""

# ╔═╡ b7f38e1d-b51f-409a-879f-a0a5084fec9f
md"---"

# ╔═╡ f5b95c86-5ab9-49ac-8121-328bb9e641d0
md"# Setup"

# ╔═╡ 749d1f97-c8fa-494d-bdf7-8bfbf761d2bf
html"""
<h2 style="
	text-align:left;
	font-size: 1.6rem;
	font-family:roboto;
	font-weight:bold;
	color: olive">

		Packages
</h2>
"""

# ╔═╡ b547889a-bcb1-4e2c-a30c-d60b168755b0
html"""
<h2 style="text-align:left;font-size: 1.6rem;color: olive;font-family:roboto;font-weight:bold">
		Data
</h2>
"""

# ╔═╡ 08193b54-cc76-460f-a312-494f4d68701d
begin
	main_url="https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/"
	idf_url="data/2024/2024-06-11/pride_index.csv"
	tags_url="data/2024/2024-06-11/pride_index_tags.csv"

	idf = CSV.File(download(main_url * idf_url)) |> DataFrame
	tags = CSV.File(download(main_url * tags_url)) |> DataFrame
end;

# ╔═╡ b8bf1cf7-f4a2-44e4-ba97-fc6d4aafc032
idf |> names

# ╔═╡ 17683b64-ae4c-4ead-abdd-fdd60ed19939
tags |> names

# ╔═╡ eeffe16e-101f-4c18-882e-f901315bf958
df = leftjoin(idf, tags, on = [:campus_name, :campus_location]);

# ╔═╡ 3b74c5d7-8a67-4037-8b14-6c9ed084af3f
md"**TidyTuesday descriotion**
 - campus_name: Name of the college or university
 - campus_location: Location of the college or university
 - rating: Campus Pride Index star rating from 1 to 5
 - students: Full-time equivalent student population
 - community_type: Locale where the campus is located: large urban city, medium city (100k to 500k), small city (25k to 100k), small town (10k to 25k),  very small town (5k to 10k), rural community (< 5k)

"

# ╔═╡ ab9e4ee4-d0d0-4bb1-8dfd-e8bcb6971862
md"# EDA"

# ╔═╡ e0ed56d8-df47-4c15-80cd-12f27d28457f
html"""
<h2 style="text-align:left;font-size: 1.6rem;color: olive;font-family:roboto;font-weight:bold">
		Overview
</h2>
"""

# ╔═╡ e70513da-aac5-4b8e-a310-9c4002611b08
df |> names

# ╔═╡ 4abd899b-ab67-4b9d-9408-3089d35e1c91
df |> describe

# ╔═╡ f09d27d0-07f4-40c5-b32f-74246bb1cc80
html"""
<h2 style="text-align:left;font-size: 1.6rem;color: olive;font-family:roboto;font-weight:bold">
		Data wrangling and exploration
</h3>
"""

# ╔═╡ 80d8d490-3b0a-4e33-89e8-cf2a327113ae
md"### Spliting campus location column into (city and state)"

# ╔═╡ 7ed020d8-149c-45a1-9dd8-a37cf77f8aca
@pipe df |>
	select(_, contains.(names(df), "campus")) |>
	first(_, 3)

# ╔═╡ 63892c57-0603-4825-a2bb-9f2ad0e2cfb8
md"Code below works only of number of splits is know and equal for all rows"

# ╔═╡ 179763d8-9ffb-4428-a019-c312bfc9af5f
# df2 = transform!(df, :campus_location => ByRow(x -> split(x, ",")) => [:city, :state])

# ╔═╡ 43b67881-cdfb-440f-9219-51949cfa8189
md"Solution for 3 splits in some rows:"

# ╔═╡ 63b826c8-1d71-4b2f-86f7-527d32c79610
@pipe df |>
	select(_, contains.(names(df), "campus")) |>
	transform(_, :campus_location => 
		ByRow(x -> get.(Ref(split(x, ", ")), 1:3, missing)) 
		=> [:city, :state, :to_ignore]) |>
	filter(!x -> ismissing(x.to_ignore), _)

# ╔═╡ 4a7d6d17-aec4-4cec-94ff-eb1ef13d2175
md"Using a generic function to handle unknown number of split: see [Julia discourse](https://discourse.julialang.org/t/separating-a-column-into-a-variable-number-of-possible-columns/69035)"

# ╔═╡ 3e0cfbde-cacd-410d-94c5-643c9c658405
function split_uniformly(col)
    s = split.(col, ", ")
    n = maximum(length.(s))
    [NamedTuple(Symbol.("split", 1:n) .=> get.(Ref(split), 1:n, missing))
     for split in s]
end

# ╔═╡ 48245210-5bd6-4e2e-868f-c2e803ee2957
@pipe df |>
	select(_, contains.(names(df), "campus")) |>
	transform(_, :campus_location => split_uniformly => [:city, :state, :ignore]) |>
	filter(!x -> ismissing(x.ignore), _)

# ╔═╡ 45861f9d-10b2-45da-9cd1-5c267dfe9a34
md"
**Final data set: fdf **
 - Drop columns *ignore*, *campus_name*, *campus_location*
"

# ╔═╡ dd1661fe-d1f6-45d2-b192-0a32c412263a
fdf = @pipe df |>
	transform(_, :campus_location => split_uniformly => [:city, :state, :ignore]) |>
	select(_, 
		:campus_name => :campus, :city, :state, 
		Not([:ignore, :campus_name, :campus_location]), 
	);

# ╔═╡ 68d79e15-f86a-44c3-826e-96452134a89a
md"### Rating average by state"

# ╔═╡ 312b6d00-10e5-4238-920d-222b585f8cbc
rating_df = @pipe fdf |>
	groupby(_, :state) |>
	combine(_, :rating => round ∘ mean => :average) |>
	sort(_, :average, rev = true)

# ╔═╡ 130fd0d4-d8a6-4d8c-8645-d6f4f9197c04
md"### Student pop total by state"

# ╔═╡ 142cb788-9e70-4aea-ad4f-bfdf79fd437a
pop_df = @pipe fdf |>
	groupby(_, :state) |>
	combine(_, :students => sum => :total) |>
	sort(_, :total, rev = true)

# ╔═╡ 87fac164-589e-43a1-acc6-1a47dd2caa6b
md"### Total population of Top 10 rated campuses"

# ╔═╡ 2c8282bb-c773-4f5a-b356-af16a5edbdc4
top20_pop = @pipe fdf |>
	groupby(_, :state) |>
	combine(_, 
		:students => sum => :total,
		:rating => round ∘ mean => :average) |>
	sort(_, :average, rev = true) |>
	first(_, 20) |>
	sort(_, :total, rev = true)

# ╔═╡ 2d83977b-5077-4eff-8369-6a64685846a8
md"### Community types"

# ╔═╡ 2fb8af36-f1de-47cb-85de-e28608d9e24e
com_types = @pipe fdf |>
	groupby(_, :community_type) |>
	combine(_, nrow => :total) |>
	sort(_, :total, rev = true)

# ╔═╡ aa0babef-e476-495d-afd1-b819cddae44c
md"### Average rating by category"

# ╔═╡ 705bfb97-ae83-4b8e-a8ba-64154df2999f
by_category = @pipe fdf |>
	select(_, :campus, :rating, Between(:public, :other_minority_serving)) |>
	stack(_, Between(:public, :other_minority_serving)) |>
	transform(_, :value  => ByRow(x -> x == "TRUE" ? "Yes" : "No") => :value) |>
	groupby(_, [:variable, :value]) |>
	combine(_, :rating => mean => :average) |>
	rename(_, 1 => :category, 2 => :class) |>
	transform(_, 
		:category => ByRow(x -> replace(titlecase(x), "_" => " ")) => :category)

# ╔═╡ de9c8ad9-58ea-4886-926a-99eea1265c79
replace("a_new_string", "_" => " ")

# ╔═╡ 16719450-365e-4c38-bb88-2ccfd2b54006
names(fdf)

# ╔═╡ 33c1768e-34b3-4227-bb57-a7929ee905fc
dd1 = DataFrame(a=["x", "y"], b=[1.0, 2.0], c=["true", "NA"], d=["true", "NA"])

# ╔═╡ e556427c-7656-49ae-8243-72e47cad96c0
stack(dd1, Not(:a))

# ╔═╡ ce72e4d9-0e0a-4025-b7d6-c6e26887f163
@pipe fdf |>
	groupby(_, :doctoral) |>
	combine(_, :rating => mean) 

# ╔═╡ 9fb017ca-b6bf-49a4-9d10-04979a613d41
@pipe fdf |>
	groupby(_, :community_type) |>
	combine(_, :rating => round ∘ mean => :average)

# ╔═╡ 18f9aa4c-3fab-4e17-be79-0fac03495a97
fdf

# ╔═╡ 96d2c5ee-13f3-4f03-9407-c9c301314c9a
html"""
<h2 style="
	color: olive;
	text-align:left;
	font-size: 1.6rem;
	font-family: roboto;
	font-weight:bold">

	Data Viz
</h2>
"""

# ╔═╡ 0fc3b978-1eca-4138-9b60-ba4fc8bc1cb5
md"### Barplot: total students population by state"

# ╔═╡ 824d1004-78d2-4508-9e3a-584dd3f40304
pop_df |> 
	@vlplot(
		width= 600, height=300,
		title = "Campus Pride Index: Student Population By State",
		:bar, 
		x={"state:o", sort="total", axis={title=""}}, 
		y={:total, axis={title=""}}
	)

# ╔═╡ 95d8e0fb-3a3f-4944-9ada-bf925ad08a20
md"### Top 20 most rated campuses sorted by pop size"

# ╔═╡ a09bc1a7-5709-40af-adb2-5801cd02d7e5
top20_pop |> 
	@vlplot(
		width=600, height=300,
		title = "Campus Pride Index: Comparing population of top 20 rated campuses",
		:bar, 
		x={:total, axis={title=""}},
		y={"state:o", sort="average", axis={title=""}},
		color={"state:n", legend = false}
	)

# ╔═╡ ff884d21-3b0d-4f97-86f8-e024125ca575
md"### Community Types"

# ╔═╡ 93c88135-b0a3-4822-b0ab-1bd22f25e8a2
com_types |>
	@vlplot(
		width=500, height=300,
		title = "Campus Pride Index: Community type distributiomn of campuses",
		:bar, 
		x={:total, axis={title=""}},
		y={
			"community_type:o", 
			axis={title="", grid=false, labels=false, ticks=false},
			sort="total"},
		color={
			"community_type:n",
			scale={
				range=["#e7ba52","#c7c7c7","#aec7e8","#1f77b4","#9467bd", "#675193"]
        		},
			legend={title="Types"}
    		},
		opacity={value=0.9}
		
	)

# ╔═╡ 1ae08ab1-fadd-4239-ab11-b7a39b1f758c
md"### By category"			

# ╔═╡ 6d1a0bc0-c51f-4092-81d8-5965d1ed6d3c
by_category |>
	@vlplot(
		width=550,
		title = "Campus Pride Index: Campuses by category and class",
		:bar,
		x = {:category, axis={labelAngle = -20}},
		y = :average,
		color = {:class, scale={range=["crimson", "seagreen"]}},
		spacing=10,
	)

# ╔═╡ be63cc7d-9bfc-4dfd-b22f-6a1b0bd3e752
by_category |>
	@vlplot(
		width = 500,
		title = "Campus Pride Index: Campuses by category and class",
		:bar,
		x = :average,
		y = {
			"category:n", 
			axis={title="", ticks=false}},
		color = {:class, legend={title="Class"}},
		spacing=0,
		config={
			view={stroke=:transparent},
			axis={domainWidth=1}
    	}
	)

# ╔═╡ c822de52-59b3-4ccf-87fd-fd36c782790a
by_category |>
	@vlplot(
		width = 35,
		title = "Campus Pride Index: Campuses by category and class",
		:bar,
		column = {:category, axis={title=""}},
		x = {:class, axis={title="", ticks=false, labels=false, grid=false}},
		y = {:average, axis={grid=false}},
		color = {:class, 
			scale={range=["crimson", "seagreen"]}},
		spacing=1,
		config={
			view={stroke=:transparent},
			axis={domainWidth=1}
    	}
	)

# ╔═╡ 50c95188-fcb1-4306-b099-90ccb3fc78e6
by_category |>
	@vlplot(
		width = 600,
		title = "Campus Pride Index: Campuses by category and class",
		:bar,
		row = {"category:n", axis={title="", labelAngle = 30}},
		x = {:average, axis={grid=false}},
		y = {:class, axis={title="", ticks=false, labels=false, grid=false}},
		color = {:class, 
			scale={range=["crimson", "seagreen"]}},
		spacing=3,
		config={
			view={stroke=:transparent},
			axis={domainWidth=1}
    	}
	)

# ╔═╡ efe95437-eccf-4110-8e2b-da1a4ff688a3
md"### Maps with VegaLite"

# ╔═╡ cbdad6e1-7320-41b0-8b15-2ef5bdd6dca2
md"**Basic US map**
 - [Vega link](https://www.queryverse.org/VegaLite.jl/stable/examples/examples_maps/#)
 - [Doggo Dot Tut](https://www.youtube.com/watch?v=mptWWrScdS4&t=1958s)
"

# ╔═╡ 9991edd9-6cb0-4623-b641-5484825233ef
md"Resolution with GIS: 1mm = 10m the higher the number, the lower the rsolution" 

# ╔═╡ b93f7520-7de2-49c9-8070-fdee2126d648
us10m = dataset("us-10m")

# ╔═╡ 73fb5a24-9164-4847-ab7a-84377529cf95
rows = dataset("population_engineers_hurricanes");

# ╔═╡ af874b7d-892d-4296-8e5c-f5a9a6d5848a
unemployment = dataset("unemployment");

# ╔═╡ 96ba6f69-7451-4e84-89e8-3b08bda1cace
typeof(unemployment)

# ╔═╡ 7146d77e-ee44-4a2d-b1a4-31afe03d1c6f
typeof(DataFrame(unemployment))

# ╔═╡ f7fcb31e-d33a-4b71-b932-3c3d583dc2c9
md"Vega plot examples"

# ╔═╡ 4e636dc2-0155-4716-9488-8de6514d3a7c
@vlplot(
    title="The population per state, engineers per state, and hurricanes per state",
    repeat={
        row=["population", "engineers", "hurricanes"]
    },
    resolve={
        scale={
            color=:independent
        }
    }
) +
@vlplot(
    width=500,
    height=300,
    data=rows,
    transform=[{
        lookup=:id,
        from={
            data={
                values=us10m,
                format={
                    type=:topojson,
                    feature=:states
                }
            },
            key=:id
        },
        as=:geo
    }],
    projection={type=:albersUsa},
    mark=:geoshape,
    shape={
        field=:geo,
        type=:geojson
    },
    color={
        field={repeat=:row},
        type=:quantitative
    }
);

# ╔═╡ 54cae628-0e3a-4099-b29b-5a3af09b86fe
@vlplot(
    width=500,
    height=300,
	title = "Basic US States Map",
    projection={type=:albersUsa},
    data={
		values=us10m,
		format={
			type=:topojson,
			feature=:states
			}
		},
    mark={
		type = :geoshape,
		fill = :lightgrey,
		stroke = :white
	}
)

# ╔═╡ f0233e6e-3613-4443-9bc4-0519cad49bbf
md"**Total population by state**"

# ╔═╡ 37dbdd6e-8323-405c-a679-c0ca1ccb49ca
first(pop_df, 2)

# ╔═╡ 07522ca3-612a-4252-a3d3-7de0c6dc21cf
us_names = CSV.File("us_states_politics.csv") |> DataFrame;

# ╔═╡ 41089a3e-80a0-48c5-a666-f0e11e02e826
first(us_names, 2)

# ╔═╡ b6a27afe-0e40-4c02-bb68-33d39f5b90a8
states_data = leftjoin(us_names, pop_df, on= :state);

# ╔═╡ 38f6cf10-47f4-4926-8052-dce99b160a6c
states_data2 = select(states_data, :id, :);

# ╔═╡ e94b730a-1240-429f-9683-9e3150c56031
first(states_data2, 2)

# ╔═╡ 77af257b-6661-48f3-b88e-13579ddf8a1d
# to review...

# ╔═╡ 8643d012-018b-47b2-89f5-62a065a5f82d
@vlplot(
	width = 640,
	height = 360,
	title = "Campus Pride Index: Student population by state",
	projection = {type = :albersUsa},

	data = {
		values = us10m,
		format = {
			type = :topojson,
			feature = :state
		}
	},

	transform = [{
		lookup = :id,
		from = {
			data = states_data2,
			key = :id,
			fields = ["total"]
		}
			
	}],

	mark = :geoshape,
	color = "total:q"
)

# ╔═╡ 643ac8d5-049f-4cd7-a643-ca909b87f28f
# to review...

# ╔═╡ 685bf7a9-ec39-4f78-b9d0-5663379d7e47
@vlplot(
    width=500,
    height=300,
	title = "Basic US States Map",
    projection={type=:albersUsa},
    data={
		values=us10m,
		format={
			type=:topojson,
			feature=:states
		}
	},
	transform = [{
		lookup = :id,
		from = {
			data = states_data2,
			key = :id,
			fields = ["total"]
		}
	}],
	projection={
        type=:albersUsa
    },
    color = "total:q"
)

# ╔═╡ 188bc781-b12c-45d9-86a0-ccda2c328c44
@vlplot(
    :geoshape,
    width=500, height=300,
    data={
        values=us10m,
        format={
            type=:topojson,
            feature=:counties
        }
    },
    transform=[{
        lookup=:id,
        from={
            data=unemployment,
            key=:id,
            fields=["rate"]
        }
    }],
    projection={
        type=:albersUsa
    },
    color="rate:q"
)

# ╔═╡ 5dbff5b9-e75d-4f69-bae0-86ae95e72ce7
first(states_data, 2)

# ╔═╡ 2cf4a38b-1030-44d2-b3f4-ecdfa5b250b0
md"### World map"

# ╔═╡ 0f6f1f8b-f97a-4edf-ba8b-41da4e6f7ff6
w110 = dataset("world-110m")

# ╔═╡ fe6a521b-966e-41d6-a8ae-47f2c8dd3e80
gdp = DataFrame(CSV.File("df_countrycode.csv"))

# ╔═╡ 0a4c1c70-8858-47b3-a249-b5cdf15df4bd
@vlplot(
    width=500, height=300,
	
    data={
        values=w110,
        format={
            type=:topojson,
            feature=:countries
        }
    },
   
    mark= {
		type = :geoshape,
		fill = :wheat,
		stroke = :grey
	}
)

# ╔═╡ 2ccac70d-53c7-48d1-adaa-4ffe5b858a35
md"**Rotation**"

# ╔═╡ b9a4d03a-2864-4163-88cc-819713639238
md"
East-West: $(@bind x1 Slider(-180:180, default=0, show_value=true))
Up-Down: $(@bind x2 Slider(-180:180, default=0, show_value=true))
Face-rotate: $(@bind x3 Slider(-180:180, default=0, show_value=true))
"

# ╔═╡ 35b18e7d-61e2-492d-998a-5ec30d9f0299
rot = [x1, x2, x3]

# ╔═╡ 9e9f3a42-8d05-4a75-93f3-4ec1a2d34db2
@vlplot(
    width=500, height=300,
	projection = {
		type = :orthographic,
		rotate  = rot
		},
	data={
		values=w110,
		format={
			type=:topojson,
			feature=:countries
			}
		},
    mark={
		type = :geoshape,
		fill = :lightgrey,
		stroke = :white
	}
)

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
CSV = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
FreqTables = "da1fdf0e-e0ff-5433-a45f-9bb5ff651cb1"
Pipe = "b98c9c47-44ae-5843-9183-064241ee97a0"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
Statistics = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"
VegaDatasets = "0ae4a718-28b7-58ec-9efb-cded64d6d5b4"
VegaLite = "112f6efa-9a02-5b7d-90c0-432ed331239a"

[compat]
CSV = "~0.10.14"
DataFrames = "~1.6.1"
FreqTables = "~0.4.6"
Pipe = "~1.3.0"
PlutoUI = "~0.7.52"
VegaDatasets = "~2.1.1"
VegaLite = "~3.3.0"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.10.4"
manifest_format = "2.0"
project_hash = "46162764e070b57791f0340c6630abe6ad3fa7d6"

[[deps.AbstractPlutoDingetjes]]
deps = ["Pkg"]
git-tree-sha1 = "91bd53c39b9cbfb5ef4b015e8b582d344532bd0a"
uuid = "6e696c72-6542-2067-7265-42206c756150"
version = "1.2.0"

[[deps.ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"
version = "1.1.1"

[[deps.Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"

[[deps.Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"

[[deps.BufferedStreams]]
git-tree-sha1 = "4ae47f9a4b1dc19897d3743ff13685925c5202ec"
uuid = "e1450e63-4bb3-523b-b2a4-4ffa8c0fd77d"
version = "1.2.1"

[[deps.CSV]]
deps = ["CodecZlib", "Dates", "FilePathsBase", "InlineStrings", "Mmap", "Parsers", "PooledArrays", "PrecompileTools", "SentinelArrays", "Tables", "Unicode", "WeakRefStrings", "WorkerUtilities"]
git-tree-sha1 = "6c834533dc1fabd820c1db03c839bf97e45a3fab"
uuid = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
version = "0.10.14"

[[deps.CategoricalArrays]]
deps = ["DataAPI", "Future", "Missings", "Printf", "Requires", "Statistics", "Unicode"]
git-tree-sha1 = "1568b28f91293458345dabba6a5ea3f183250a61"
uuid = "324d7699-5711-5eae-9e2f-1d82baa6b597"
version = "0.10.8"
weakdeps = ["JSON", "RecipesBase", "SentinelArrays", "StructTypes"]

    [deps.CategoricalArrays.extensions]
    CategoricalArraysJSONExt = "JSON"
    CategoricalArraysRecipesBaseExt = "RecipesBase"
    CategoricalArraysSentinelArraysExt = "SentinelArrays"
    CategoricalArraysStructTypesExt = "StructTypes"

[[deps.CodecZlib]]
deps = ["TranscodingStreams", "Zlib_jll"]
git-tree-sha1 = "59939d8a997469ee05c4b4944560a820f9ba0d73"
uuid = "944b1d66-785c-5afd-91f1-9de20f533193"
version = "0.7.4"

[[deps.ColorTypes]]
deps = ["FixedPointNumbers", "Random"]
git-tree-sha1 = "b10d0b65641d57b8b4d5e234446582de5047050d"
uuid = "3da002f7-5984-5a60-b8a6-cbb66c0b333f"
version = "0.11.5"

[[deps.Combinatorics]]
git-tree-sha1 = "08c8b6831dc00bfea825826be0bc8336fc369860"
uuid = "861a8166-3701-5b0c-9a16-15d98fcdc6aa"
version = "1.0.2"

[[deps.Compat]]
deps = ["TOML", "UUIDs"]
git-tree-sha1 = "b1c55339b7c6c350ee89f2c1604299660525b248"
uuid = "34da2185-b29b-5c13-b0c7-acf172513d20"
version = "4.15.0"
weakdeps = ["Dates", "LinearAlgebra"]

    [deps.Compat.extensions]
    CompatLinearAlgebraExt = "LinearAlgebra"

[[deps.CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"
version = "1.1.1+0"

[[deps.ConstructionBase]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "260fd2400ed2dab602a7c15cf10c1933c59930a2"
uuid = "187b0558-2788-49d3-abe0-74a17ed4e7c9"
version = "1.5.5"

    [deps.ConstructionBase.extensions]
    ConstructionBaseIntervalSetsExt = "IntervalSets"
    ConstructionBaseStaticArraysExt = "StaticArrays"

    [deps.ConstructionBase.weakdeps]
    IntervalSets = "8197267c-284f-5f27-9208-e0e47529a953"
    StaticArrays = "90137ffa-7385-5640-81b9-e52037218182"

[[deps.Crayons]]
git-tree-sha1 = "249fe38abf76d48563e2f4556bebd215aa317e15"
uuid = "a8cc5b0e-0ffa-5ad4-8c14-923d3ee1735f"
version = "4.1.1"

[[deps.DataAPI]]
git-tree-sha1 = "abe83f3a2f1b857aac70ef8b269080af17764bbe"
uuid = "9a962f9c-6df0-11e9-0e5d-c546b8b5ee8a"
version = "1.16.0"

[[deps.DataFrames]]
deps = ["Compat", "DataAPI", "DataStructures", "Future", "InlineStrings", "InvertedIndices", "IteratorInterfaceExtensions", "LinearAlgebra", "Markdown", "Missings", "PooledArrays", "PrecompileTools", "PrettyTables", "Printf", "REPL", "Random", "Reexport", "SentinelArrays", "SortingAlgorithms", "Statistics", "TableTraits", "Tables", "Unicode"]
git-tree-sha1 = "04c738083f29f86e62c8afc341f0967d8717bdb8"
uuid = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
version = "1.6.1"

[[deps.DataStructures]]
deps = ["Compat", "InteractiveUtils", "OrderedCollections"]
git-tree-sha1 = "1d0a14036acb104d9e89698bd408f63ab58cdc82"
uuid = "864edb3b-99cc-5e75-8d2d-829cb0a9cfe8"
version = "0.18.20"

[[deps.DataValueInterfaces]]
git-tree-sha1 = "bfc1187b79289637fa0ef6d4436ebdfe6905cbd6"
uuid = "e2d170a0-9d28-54be-80f0-106bbe20a464"
version = "1.0.0"

[[deps.DataValues]]
deps = ["DataValueInterfaces", "Dates"]
git-tree-sha1 = "d88a19299eba280a6d062e135a43f00323ae70bf"
uuid = "e7dc6d0d-1eca-5fa6-8ad6-5aecde8b7ea5"
version = "0.4.13"

[[deps.Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"

[[deps.DelimitedFiles]]
deps = ["Mmap"]
git-tree-sha1 = "9e2f36d3c96a820c678f2f1f1782582fcf685bae"
uuid = "8bb1440f-4735-579b-a4ab-409b98df4dab"
version = "1.9.1"

[[deps.DocStringExtensions]]
deps = ["LibGit2"]
git-tree-sha1 = "2fb1e02f2b635d0845df5d7c167fec4dd739b00d"
uuid = "ffbed154-4ef7-542d-bbb7-c09d3a79fcae"
version = "0.9.3"

[[deps.DoubleFloats]]
deps = ["GenericLinearAlgebra", "LinearAlgebra", "Polynomials", "Printf", "Quadmath", "Random", "Requires", "SpecialFunctions"]
git-tree-sha1 = "b14dd11945504e0fd81b4f92dc610a80168bca66"
uuid = "497a8b3b-efae-58df-a0af-a86822472b78"
version = "1.3.8"

[[deps.Downloads]]
deps = ["ArgTools", "FileWatching", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"
version = "1.6.0"

[[deps.FileIO]]
deps = ["Pkg", "Requires", "UUIDs"]
git-tree-sha1 = "82d8afa92ecf4b52d78d869f038ebfb881267322"
uuid = "5789e2e9-d7fb-5bc7-8068-2c6fae9b9549"
version = "1.16.3"

[[deps.FilePaths]]
deps = ["FilePathsBase", "MacroTools", "Reexport", "Requires"]
git-tree-sha1 = "919d9412dbf53a2e6fe74af62a73ceed0bce0629"
uuid = "8fc22ac5-c921-52a6-82fd-178b2807b824"
version = "0.8.3"

[[deps.FilePathsBase]]
deps = ["Compat", "Dates", "Mmap", "Printf", "Test", "UUIDs"]
git-tree-sha1 = "9f00e42f8d99fdde64d40c8ea5d14269a2e2c1aa"
uuid = "48062228-2e41-5def-b9a4-89aafe57970f"
version = "0.9.21"

[[deps.FileWatching]]
uuid = "7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"

[[deps.FixedPointNumbers]]
deps = ["Statistics"]
git-tree-sha1 = "05882d6995ae5c12bb5f36dd2ed3f61c98cbb172"
uuid = "53c48c17-4a7d-5ca2-90c5-79b7896eea93"
version = "0.8.5"

[[deps.FreqTables]]
deps = ["CategoricalArrays", "Missings", "NamedArrays", "Tables"]
git-tree-sha1 = "4693424929b4ec7ad703d68912a6ad6eff103cfe"
uuid = "da1fdf0e-e0ff-5433-a45f-9bb5ff651cb1"
version = "0.4.6"

[[deps.Future]]
deps = ["Random"]
uuid = "9fa8497b-333b-5362-9e8d-4d0656e87820"

[[deps.GenericLinearAlgebra]]
deps = ["LinearAlgebra", "Printf", "Random", "libblastrampoline_jll"]
git-tree-sha1 = "02be7066f936af6b04669f7c370a31af9036c440"
uuid = "14197337-ba66-59df-a3e3-ca00e7dcff7a"
version = "0.3.11"

[[deps.Hyperscript]]
deps = ["Test"]
git-tree-sha1 = "8d511d5b81240fc8e6802386302675bdf47737b9"
uuid = "47d2ed2b-36de-50cf-bf87-49c2cf4b8b91"
version = "0.0.4"

[[deps.HypertextLiteral]]
deps = ["Tricks"]
git-tree-sha1 = "7134810b1afce04bbc1045ca1985fbe81ce17653"
uuid = "ac1192a8-f4b3-4bfe-ba22-af5b92cd3ab2"
version = "0.9.5"

[[deps.IOCapture]]
deps = ["Logging", "Random"]
git-tree-sha1 = "8b72179abc660bfab5e28472e019392b97d0985c"
uuid = "b5f81e59-6552-4d32-b1f0-c071b021bf89"
version = "0.2.4"

[[deps.InlineStrings]]
deps = ["Parsers"]
git-tree-sha1 = "9cc2baf75c6d09f9da536ddf58eb2f29dedaf461"
uuid = "842dd82b-1e85-43dc-bf29-5d0ee9dffc48"
version = "1.4.0"

[[deps.InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"

[[deps.InvertedIndices]]
git-tree-sha1 = "0dc7b50b8d436461be01300fd8cd45aa0274b038"
uuid = "41ab1584-1d38-5bbf-9106-f11c6c58b48f"
version = "1.3.0"

[[deps.IrrationalConstants]]
git-tree-sha1 = "630b497eafcc20001bba38a4651b327dcfc491d2"
uuid = "92d709cd-6900-40b7-9082-c6be49f344b6"
version = "0.2.2"

[[deps.IterableTables]]
deps = ["DataValues", "IteratorInterfaceExtensions", "Requires", "TableTraits", "TableTraitsUtils"]
git-tree-sha1 = "70300b876b2cebde43ebc0df42bc8c94a144e1b4"
uuid = "1c8ee90f-4401-5389-894e-7a04a3dc0f4d"
version = "1.0.0"

[[deps.IteratorInterfaceExtensions]]
git-tree-sha1 = "a3f24677c21f5bbe9d2a714f95dcd58337fb2856"
uuid = "82899510-4779-5014-852e-03e436cf321d"
version = "1.0.0"

[[deps.JLLWrappers]]
deps = ["Artifacts", "Preferences"]
git-tree-sha1 = "7e5d6779a1e09a36db2a7b6cff50942a0a7d0fca"
uuid = "692b3bcd-3c85-4b1f-b108-f13ce0eb3210"
version = "1.5.0"

[[deps.JSON]]
deps = ["Dates", "Mmap", "Parsers", "Unicode"]
git-tree-sha1 = "31e996f0a15c7b280ba9f76636b3ff9e2ae58c9a"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.4"

[[deps.JSON3]]
deps = ["Dates", "Mmap", "Parsers", "PrecompileTools", "StructTypes", "UUIDs"]
git-tree-sha1 = "eb3edce0ed4fa32f75a0a11217433c31d56bd48b"
uuid = "0f8b85d8-7281-11e9-16c2-39a750bddbf1"
version = "1.14.0"

    [deps.JSON3.extensions]
    JSON3ArrowExt = ["ArrowTypes"]

    [deps.JSON3.weakdeps]
    ArrowTypes = "31f734f8-188a-4ce0-8406-c8a06bd891cd"

[[deps.JSONSchema]]
deps = ["Downloads", "JSON", "JSON3", "URIs"]
git-tree-sha1 = "5f0bd0cd69df978fa64ccdcb5c152fbc705455a1"
uuid = "7d188eb4-7ad8-530c-ae41-71a32a6d4692"
version = "1.3.0"

[[deps.LaTeXStrings]]
git-tree-sha1 = "50901ebc375ed41dbf8058da26f9de442febbbec"
uuid = "b964fa9f-0449-5b57-a5c2-d3ea65f4040f"
version = "1.3.1"

[[deps.LibCURL]]
deps = ["LibCURL_jll", "MozillaCACerts_jll"]
uuid = "b27032c2-a3e7-50c8-80cd-2d36dbcbfd21"
version = "0.6.4"

[[deps.LibCURL_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll", "Zlib_jll", "nghttp2_jll"]
uuid = "deac9b47-8bc7-5906-a0fe-35ac56dc84c0"
version = "8.4.0+0"

[[deps.LibGit2]]
deps = ["Base64", "LibGit2_jll", "NetworkOptions", "Printf", "SHA"]
uuid = "76f85450-5226-5b5a-8eaa-529ad045b433"

[[deps.LibGit2_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll"]
uuid = "e37daf67-58a4-590a-8e99-b0245dd2ffc5"
version = "1.6.4+0"

[[deps.LibSSH2_jll]]
deps = ["Artifacts", "Libdl", "MbedTLS_jll"]
uuid = "29816b5a-b9ab-546f-933c-edad1886dfa8"
version = "1.11.0+1"

[[deps.Libdl]]
uuid = "8f399da3-3557-5675-b5ff-fb832c97cbdb"

[[deps.LinearAlgebra]]
deps = ["Libdl", "OpenBLAS_jll", "libblastrampoline_jll"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"

[[deps.LogExpFunctions]]
deps = ["DocStringExtensions", "IrrationalConstants", "LinearAlgebra"]
git-tree-sha1 = "a2d09619db4e765091ee5c6ffe8872849de0feea"
uuid = "2ab3a3ac-af41-5b50-aa03-7779005ae688"
version = "0.3.28"

    [deps.LogExpFunctions.extensions]
    LogExpFunctionsChainRulesCoreExt = "ChainRulesCore"
    LogExpFunctionsChangesOfVariablesExt = "ChangesOfVariables"
    LogExpFunctionsInverseFunctionsExt = "InverseFunctions"

    [deps.LogExpFunctions.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    ChangesOfVariables = "9e997f8a-9a97-42d5-a9f1-ce6bfc15e2c0"
    InverseFunctions = "3587e190-3f89-42d0-90ee-14403ec27112"

[[deps.Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"

[[deps.MIMEs]]
git-tree-sha1 = "65f28ad4b594aebe22157d6fac869786a255b7eb"
uuid = "6c6e2e6c-3030-632d-7369-2d6c69616d65"
version = "0.1.4"

[[deps.MacroTools]]
deps = ["Markdown", "Random"]
git-tree-sha1 = "2fa9ee3e63fd3a4f7a9a4f4744a52f4856de82df"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.13"

[[deps.Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"

[[deps.MbedTLS_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "c8ffd9c3-330d-5841-b78e-0817d7145fa1"
version = "2.28.2+1"

[[deps.Missings]]
deps = ["DataAPI"]
git-tree-sha1 = "ec4f7fbeab05d7747bdf98eb74d130a2a2ed298d"
uuid = "e1d29d7a-bbdc-5cf2-9ac0-f12de2c33e28"
version = "1.2.0"

[[deps.Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"

[[deps.MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"
version = "2023.1.10"

[[deps.NamedArrays]]
deps = ["Combinatorics", "DataStructures", "DelimitedFiles", "InvertedIndices", "LinearAlgebra", "Random", "Requires", "SparseArrays", "Statistics"]
git-tree-sha1 = "c7aab3836df3f31591a2b4167fcd87b741dacfc9"
uuid = "86f7a689-2022-50b4-a561-43c23ac3c673"
version = "0.10.2"

[[deps.NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"
version = "1.2.0"

[[deps.NodeJS]]
deps = ["Pkg"]
git-tree-sha1 = "bf1f49fd62754064bc42490a8ddc2aa3694a8e7a"
uuid = "2bd173c7-0d6d-553b-b6af-13a54713934c"
version = "2.0.0"

[[deps.Nullables]]
git-tree-sha1 = "8f87854cc8f3685a60689d8edecaa29d2251979b"
uuid = "4d1e1d77-625e-5b40-9113-a560ec7a8ecd"
version = "1.0.0"

[[deps.OpenBLAS_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Libdl"]
uuid = "4536629a-c528-5b80-bd46-f80d51c5b363"
version = "0.3.23+4"

[[deps.OpenLibm_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "05823500-19ac-5b8b-9628-191a04bc5112"
version = "0.8.1+2"

[[deps.OpenSpecFun_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "13652491f6856acfd2db29360e1bbcd4565d04f1"
uuid = "efe28fd5-8261-553b-a9e1-b2916fc3738e"
version = "0.5.5+0"

[[deps.OrderedCollections]]
git-tree-sha1 = "dfdf5519f235516220579f949664f1bf44e741c5"
uuid = "bac558e1-5e72-5ebc-8fee-abe8a469f55d"
version = "1.6.3"

[[deps.Parsers]]
deps = ["Dates", "PrecompileTools", "UUIDs"]
git-tree-sha1 = "8489905bcdbcfac64d1daa51ca07c0d8f0283821"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "2.8.1"

[[deps.Pipe]]
git-tree-sha1 = "6842804e7867b115ca9de748a0cf6b364523c16d"
uuid = "b98c9c47-44ae-5843-9183-064241ee97a0"
version = "1.3.0"

[[deps.Pkg]]
deps = ["Artifacts", "Dates", "Downloads", "FileWatching", "LibGit2", "Libdl", "Logging", "Markdown", "Printf", "REPL", "Random", "SHA", "Serialization", "TOML", "Tar", "UUIDs", "p7zip_jll"]
uuid = "44cfe95a-1eb2-52ea-b672-e2afdf69b78f"
version = "1.10.0"

[[deps.PlutoUI]]
deps = ["AbstractPlutoDingetjes", "Base64", "ColorTypes", "Dates", "FixedPointNumbers", "Hyperscript", "HypertextLiteral", "IOCapture", "InteractiveUtils", "JSON", "Logging", "MIMEs", "Markdown", "Random", "Reexport", "URIs", "UUIDs"]
git-tree-sha1 = "e47cd150dbe0443c3a3651bc5b9cbd5576ab75b7"
uuid = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
version = "0.7.52"

[[deps.Polynomials]]
deps = ["LinearAlgebra", "RecipesBase", "Setfield", "SparseArrays"]
git-tree-sha1 = "a9c7a523d5ed375be3983db190f6a5874ae9286d"
uuid = "f27b6e38-b328-58d1-80ce-0feddd5e7a45"
version = "4.0.6"

    [deps.Polynomials.extensions]
    PolynomialsChainRulesCoreExt = "ChainRulesCore"
    PolynomialsFFTWExt = "FFTW"
    PolynomialsMakieCoreExt = "MakieCore"
    PolynomialsMutableArithmeticsExt = "MutableArithmetics"

    [deps.Polynomials.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    FFTW = "7a1cc6ca-52ef-59f5-83cd-3a7055c09341"
    MakieCore = "20f20a25-4f0e-4fdf-b5d1-57303727442b"
    MutableArithmetics = "d8a4904e-b15c-11e9-3269-09a3773c0cb0"

[[deps.PooledArrays]]
deps = ["DataAPI", "Future"]
git-tree-sha1 = "36d8b4b899628fb92c2749eb488d884a926614d3"
uuid = "2dfb63ee-cc39-5dd5-95bd-886bf059d720"
version = "1.4.3"

[[deps.PrecompileTools]]
deps = ["Preferences"]
git-tree-sha1 = "5aa36f7049a63a1528fe8f7c3f2113413ffd4e1f"
uuid = "aea7be01-6a6a-4083-8856-8a6e6704d82a"
version = "1.2.1"

[[deps.Preferences]]
deps = ["TOML"]
git-tree-sha1 = "9306f6085165d270f7e3db02af26a400d580f5c6"
uuid = "21216c6a-2e73-6563-6e65-726566657250"
version = "1.4.3"

[[deps.PrettyTables]]
deps = ["Crayons", "LaTeXStrings", "Markdown", "PrecompileTools", "Printf", "Reexport", "StringManipulation", "Tables"]
git-tree-sha1 = "66b20dd35966a748321d3b2537c4584cf40387c7"
uuid = "08abe8d2-0d0c-5749-adfa-8a2ac140af0d"
version = "2.3.2"

[[deps.Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"

[[deps.Quadmath]]
deps = ["Compat", "Printf", "Random", "Requires"]
git-tree-sha1 = "67fe599f02c3f7be5d97310674cd05429d6f1b42"
uuid = "be4d8f0f-7fa4-5f49-b795-2f01399ab2dd"
version = "0.5.10"

[[deps.REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"

[[deps.Random]]
deps = ["SHA"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"

[[deps.RecipesBase]]
deps = ["PrecompileTools"]
git-tree-sha1 = "5c3d09cc4f31f5fc6af001c250bf1278733100ff"
uuid = "3cdcf5f2-1ef4-517c-9805-6587b60abb01"
version = "1.3.4"

[[deps.Reexport]]
git-tree-sha1 = "45e428421666073eab6f2da5c9d310d99bb12f9b"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.2.2"

[[deps.Requires]]
deps = ["UUIDs"]
git-tree-sha1 = "838a3a4188e2ded87a4f9f184b4b0d78a1e91cb7"
uuid = "ae029012-a4dd-5104-9daa-d747884805df"
version = "1.3.0"

[[deps.SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"
version = "0.7.0"

[[deps.SentinelArrays]]
deps = ["Dates", "Random"]
git-tree-sha1 = "90b4f68892337554d31cdcdbe19e48989f26c7e6"
uuid = "91c51154-3ec4-41a3-a24f-3f23e20d615c"
version = "1.4.3"

[[deps.Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"

[[deps.Setfield]]
deps = ["ConstructionBase", "Future", "MacroTools", "StaticArraysCore"]
git-tree-sha1 = "e2cc6d8c88613c05e1defb55170bf5ff211fbeac"
uuid = "efcf1570-3423-57d1-acb7-fd33fddbac46"
version = "1.1.1"

[[deps.Sockets]]
uuid = "6462fe0b-24de-5631-8697-dd941f90decc"

[[deps.SortingAlgorithms]]
deps = ["DataStructures"]
git-tree-sha1 = "66e0a8e672a0bdfca2c3f5937efb8538b9ddc085"
uuid = "a2af1166-a08f-5f64-846c-94a0d3cef48c"
version = "1.2.1"

[[deps.SparseArrays]]
deps = ["Libdl", "LinearAlgebra", "Random", "Serialization", "SuiteSparse_jll"]
uuid = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"
version = "1.10.0"

[[deps.SpecialFunctions]]
deps = ["IrrationalConstants", "LogExpFunctions", "OpenLibm_jll", "OpenSpecFun_jll"]
git-tree-sha1 = "2f5d4697f21388cbe1ff299430dd169ef97d7e14"
uuid = "276daf66-3868-5448-9aa4-cd146d93841b"
version = "2.4.0"

    [deps.SpecialFunctions.extensions]
    SpecialFunctionsChainRulesCoreExt = "ChainRulesCore"

    [deps.SpecialFunctions.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"

[[deps.StaticArraysCore]]
git-tree-sha1 = "36b3d696ce6366023a0ea192b4cd442268995a0d"
uuid = "1e83bf80-4336-4d27-bf5d-d5a4f845583c"
version = "1.4.2"

[[deps.Statistics]]
deps = ["LinearAlgebra", "SparseArrays"]
uuid = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"
version = "1.10.0"

[[deps.StringManipulation]]
deps = ["PrecompileTools"]
git-tree-sha1 = "a04cabe79c5f01f4d723cc6704070ada0b9d46d5"
uuid = "892a3eda-7b42-436c-8928-eab12a02cf0e"
version = "0.3.4"

[[deps.StructTypes]]
deps = ["Dates", "UUIDs"]
git-tree-sha1 = "ca4bccb03acf9faaf4137a9abc1881ed1841aa70"
uuid = "856f2bd8-1eba-4b0a-8007-ebc267875bd4"
version = "1.10.0"

[[deps.SuiteSparse_jll]]
deps = ["Artifacts", "Libdl", "libblastrampoline_jll"]
uuid = "bea87d4a-7f5b-5778-9afe-8cc45184846c"
version = "7.2.1+1"

[[deps.TOML]]
deps = ["Dates"]
uuid = "fa267f1f-6049-4f14-aa54-33bafae1ed76"
version = "1.0.3"

[[deps.TableShowUtils]]
deps = ["DataValues", "Dates", "JSON", "Markdown", "Unicode"]
git-tree-sha1 = "2a41a3dedda21ed1184a47caab56ed9304e9a038"
uuid = "5e66a065-1f0a-5976-b372-e0b8c017ca10"
version = "0.2.6"

[[deps.TableTraits]]
deps = ["IteratorInterfaceExtensions"]
git-tree-sha1 = "c06b2f539df1c6efa794486abfb6ed2022561a39"
uuid = "3783bdb8-4a98-5b6b-af9a-565f29a5fe9c"
version = "1.0.1"

[[deps.TableTraitsUtils]]
deps = ["DataValues", "IteratorInterfaceExtensions", "Missings", "TableTraits"]
git-tree-sha1 = "78fecfe140d7abb480b53a44f3f85b6aa373c293"
uuid = "382cd787-c1b6-5bf2-a167-d5b971a19bda"
version = "1.0.2"

[[deps.Tables]]
deps = ["DataAPI", "DataValueInterfaces", "IteratorInterfaceExtensions", "LinearAlgebra", "OrderedCollections", "TableTraits"]
git-tree-sha1 = "cb76cf677714c095e535e3501ac7954732aeea2d"
uuid = "bd369af6-aec1-5ad0-b16a-f7cc5008161c"
version = "1.11.1"

[[deps.Tar]]
deps = ["ArgTools", "SHA"]
uuid = "a4e569a6-e804-4fa4-b0f3-eef7a1d5b13e"
version = "1.10.0"

[[deps.Test]]
deps = ["InteractiveUtils", "Logging", "Random", "Serialization"]
uuid = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

[[deps.TextParse]]
deps = ["CodecZlib", "DataStructures", "Dates", "DoubleFloats", "Mmap", "Nullables", "WeakRefStrings"]
git-tree-sha1 = "eb1f4fb185c8644faa2d18d14c72f2c24412415f"
uuid = "e0df1984-e451-5cb5-8b61-797a481e67e3"
version = "1.0.2"

[[deps.TranscodingStreams]]
git-tree-sha1 = "a947ea21087caba0a798c5e494d0bb78e3a1a3a0"
uuid = "3bb67fe8-82b1-5028-8e26-92a6c54297fa"
version = "0.10.9"
weakdeps = ["Random", "Test"]

    [deps.TranscodingStreams.extensions]
    TestExt = ["Test", "Random"]

[[deps.Tricks]]
git-tree-sha1 = "eae1bb484cd63b36999ee58be2de6c178105112f"
uuid = "410a4b4d-49e4-4fbc-ab6d-cb71b17b3775"
version = "0.1.8"

[[deps.URIParser]]
deps = ["Unicode"]
git-tree-sha1 = "53a9f49546b8d2dd2e688d216421d050c9a31d0d"
uuid = "30578b45-9adc-5946-b283-645ec420af67"
version = "0.4.1"

[[deps.URIs]]
git-tree-sha1 = "67db6cc7b3821e19ebe75791a9dd19c9b1188f2b"
uuid = "5c2747f8-b7ea-4ff2-ba2e-563bfd36b1d4"
version = "1.5.1"

[[deps.UUIDs]]
deps = ["Random", "SHA"]
uuid = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

[[deps.Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"

[[deps.Vega]]
deps = ["BufferedStreams", "DataStructures", "DataValues", "Dates", "FileIO", "FilePaths", "IteratorInterfaceExtensions", "JSON", "JSONSchema", "MacroTools", "NodeJS", "Pkg", "REPL", "Random", "Setfield", "TableTraits", "TableTraitsUtils", "URIParser"]
git-tree-sha1 = "0efd71a3df864e86d24236c99aaae3970e6f0ed0"
uuid = "239c3e63-733f-47ad-beb7-a12fde22c578"
version = "2.7.0"

[[deps.VegaDatasets]]
deps = ["DataStructures", "DataValues", "FilePaths", "IterableTables", "IteratorInterfaceExtensions", "JSON", "TableShowUtils", "TableTraits", "TableTraitsUtils", "TextParse"]
git-tree-sha1 = "c997c7217f37205c5795de8c797f8f8531890f1d"
uuid = "0ae4a718-28b7-58ec-9efb-cded64d6d5b4"
version = "2.1.1"

[[deps.VegaLite]]
deps = ["Base64", "BufferedStreams", "DataStructures", "DataValues", "Dates", "FileIO", "FilePaths", "IteratorInterfaceExtensions", "JSON", "MacroTools", "NodeJS", "Pkg", "REPL", "Random", "TableTraits", "TableTraitsUtils", "URIParser", "Vega"]
git-tree-sha1 = "79115432a0a40955f071bccc6e5574ae5567a7b0"
uuid = "112f6efa-9a02-5b7d-90c0-432ed331239a"
version = "3.3.0"

[[deps.WeakRefStrings]]
deps = ["DataAPI", "InlineStrings", "Parsers"]
git-tree-sha1 = "b1be2855ed9ed8eac54e5caff2afcdb442d52c23"
uuid = "ea10d353-3f73-51f8-a26c-33c1cb351aa5"
version = "1.4.2"

[[deps.WorkerUtilities]]
git-tree-sha1 = "cd1659ba0d57b71a464a29e64dbc67cfe83d54e7"
uuid = "76eceee3-57b5-4d4a-8e66-0e911cebbf60"
version = "1.6.1"

[[deps.Zlib_jll]]
deps = ["Libdl"]
uuid = "83775a58-1f1d-513f-b197-d71354ab007a"
version = "1.2.13+1"

[[deps.libblastrampoline_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850b90-86db-534c-a0d3-1478176c7d93"
version = "5.8.0+1"

[[deps.nghttp2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850ede-7688-5339-a07c-302acd2aaf8d"
version = "1.52.0+1"

[[deps.p7zip_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "3f19e933-33d8-53b3-aaab-bd5110c3b7a0"
version = "17.4.0+2"
"""

# ╔═╡ Cell order:
# ╠═c85a6852-2e99-11ef-0c75-b7a7215f6d0a
# ╟─b7f38e1d-b51f-409a-879f-a0a5084fec9f
# ╠═1d6087d8-2a51-4244-a4d6-1f45b4789cec
# ╟─f5b95c86-5ab9-49ac-8121-328bb9e641d0
# ╠═749d1f97-c8fa-494d-bdf7-8bfbf761d2bf
# ╠═285ebfba-78c7-4409-b930-943899531262
# ╠═da9f16ee-621d-4c03-b92d-96543f9ddf4b
# ╠═a4be5a19-1491-48e1-a3f0-d4a3d420ff73
# ╠═673545b9-cda8-4ace-b75d-3d5425967665
# ╠═b547889a-bcb1-4e2c-a30c-d60b168755b0
# ╠═08193b54-cc76-460f-a312-494f4d68701d
# ╠═b8bf1cf7-f4a2-44e4-ba97-fc6d4aafc032
# ╠═17683b64-ae4c-4ead-abdd-fdd60ed19939
# ╠═eeffe16e-101f-4c18-882e-f901315bf958
# ╠═3b74c5d7-8a67-4037-8b14-6c9ed084af3f
# ╟─ab9e4ee4-d0d0-4bb1-8dfd-e8bcb6971862
# ╠═e0ed56d8-df47-4c15-80cd-12f27d28457f
# ╠═e70513da-aac5-4b8e-a310-9c4002611b08
# ╠═4abd899b-ab67-4b9d-9408-3089d35e1c91
# ╟─f09d27d0-07f4-40c5-b32f-74246bb1cc80
# ╟─80d8d490-3b0a-4e33-89e8-cf2a327113ae
# ╠═7ed020d8-149c-45a1-9dd8-a37cf77f8aca
# ╠═63892c57-0603-4825-a2bb-9f2ad0e2cfb8
# ╠═179763d8-9ffb-4428-a019-c312bfc9af5f
# ╟─43b67881-cdfb-440f-9219-51949cfa8189
# ╠═63b826c8-1d71-4b2f-86f7-527d32c79610
# ╟─4a7d6d17-aec4-4cec-94ff-eb1ef13d2175
# ╠═3e0cfbde-cacd-410d-94c5-643c9c658405
# ╠═48245210-5bd6-4e2e-868f-c2e803ee2957
# ╠═45861f9d-10b2-45da-9cd1-5c267dfe9a34
# ╠═dd1661fe-d1f6-45d2-b192-0a32c412263a
# ╟─68d79e15-f86a-44c3-826e-96452134a89a
# ╠═312b6d00-10e5-4238-920d-222b585f8cbc
# ╟─130fd0d4-d8a6-4d8c-8645-d6f4f9197c04
# ╠═142cb788-9e70-4aea-ad4f-bfdf79fd437a
# ╟─87fac164-589e-43a1-acc6-1a47dd2caa6b
# ╠═2c8282bb-c773-4f5a-b356-af16a5edbdc4
# ╟─2d83977b-5077-4eff-8369-6a64685846a8
# ╠═2fb8af36-f1de-47cb-85de-e28608d9e24e
# ╟─aa0babef-e476-495d-afd1-b819cddae44c
# ╠═705bfb97-ae83-4b8e-a8ba-64154df2999f
# ╠═de9c8ad9-58ea-4886-926a-99eea1265c79
# ╠═16719450-365e-4c38-bb88-2ccfd2b54006
# ╠═33c1768e-34b3-4227-bb57-a7929ee905fc
# ╠═e556427c-7656-49ae-8243-72e47cad96c0
# ╠═ce72e4d9-0e0a-4025-b7d6-c6e26887f163
# ╠═9fb017ca-b6bf-49a4-9d10-04979a613d41
# ╠═18f9aa4c-3fab-4e17-be79-0fac03495a97
# ╟─96d2c5ee-13f3-4f03-9407-c9c301314c9a
# ╟─0fc3b978-1eca-4138-9b60-ba4fc8bc1cb5
# ╠═824d1004-78d2-4508-9e3a-584dd3f40304
# ╟─95d8e0fb-3a3f-4944-9ada-bf925ad08a20
# ╠═a09bc1a7-5709-40af-adb2-5801cd02d7e5
# ╟─ff884d21-3b0d-4f97-86f8-e024125ca575
# ╠═93c88135-b0a3-4822-b0ab-1bd22f25e8a2
# ╟─1ae08ab1-fadd-4239-ab11-b7a39b1f758c
# ╠═6d1a0bc0-c51f-4092-81d8-5965d1ed6d3c
# ╠═be63cc7d-9bfc-4dfd-b22f-6a1b0bd3e752
# ╠═c822de52-59b3-4ccf-87fd-fd36c782790a
# ╠═50c95188-fcb1-4306-b099-90ccb3fc78e6
# ╟─efe95437-eccf-4110-8e2b-da1a4ff688a3
# ╟─cbdad6e1-7320-41b0-8b15-2ef5bdd6dca2
# ╟─9991edd9-6cb0-4623-b641-5484825233ef
# ╠═b93f7520-7de2-49c9-8070-fdee2126d648
# ╠═73fb5a24-9164-4847-ab7a-84377529cf95
# ╠═af874b7d-892d-4296-8e5c-f5a9a6d5848a
# ╠═96ba6f69-7451-4e84-89e8-3b08bda1cace
# ╠═7146d77e-ee44-4a2d-b1a4-31afe03d1c6f
# ╟─f7fcb31e-d33a-4b71-b932-3c3d583dc2c9
# ╠═4e636dc2-0155-4716-9488-8de6514d3a7c
# ╠═54cae628-0e3a-4099-b29b-5a3af09b86fe
# ╟─f0233e6e-3613-4443-9bc4-0519cad49bbf
# ╠═37dbdd6e-8323-405c-a679-c0ca1ccb49ca
# ╠═07522ca3-612a-4252-a3d3-7de0c6dc21cf
# ╠═41089a3e-80a0-48c5-a666-f0e11e02e826
# ╠═b6a27afe-0e40-4c02-bb68-33d39f5b90a8
# ╠═38f6cf10-47f4-4926-8052-dce99b160a6c
# ╠═e94b730a-1240-429f-9683-9e3150c56031
# ╠═77af257b-6661-48f3-b88e-13579ddf8a1d
# ╠═8643d012-018b-47b2-89f5-62a065a5f82d
# ╠═643ac8d5-049f-4cd7-a643-ca909b87f28f
# ╠═685bf7a9-ec39-4f78-b9d0-5663379d7e47
# ╠═188bc781-b12c-45d9-86a0-ccda2c328c44
# ╠═5dbff5b9-e75d-4f69-bae0-86ae95e72ce7
# ╟─2cf4a38b-1030-44d2-b3f4-ecdfa5b250b0
# ╠═0f6f1f8b-f97a-4edf-ba8b-41da4e6f7ff6
# ╠═fe6a521b-966e-41d6-a8ae-47f2c8dd3e80
# ╠═0a4c1c70-8858-47b3-a249-b5cdf15df4bd
# ╟─2ccac70d-53c7-48d1-adaa-4ffe5b858a35
# ╟─b9a4d03a-2864-4163-88cc-819713639238
# ╟─35b18e7d-61e2-492d-998a-5ec30d9f0299
# ╠═9e9f3a42-8d05-4a75-93f3-4ec1a2d34db2
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
