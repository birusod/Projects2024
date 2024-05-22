### A Pluto.jl notebook ###
# v0.19.42

using Markdown
using InteractiveUtils

# ╔═╡ 244fd8e8-0e4f-4baa-a523-3d0c9acf7b3e
begin
	using PlutoUI
	TableOfContents()
end

# ╔═╡ 216da246-13e2-4222-b218-4587f77f6db4
using CSV, DataFrames, PlotlyLight

# ╔═╡ cef7f42f-02d0-4e6e-bff4-145b828d3753
using Pipe

# ╔═╡ 7f197b67-d5ab-47d0-a331-f7b166d327ca
using CategoricalArrays

# ╔═╡ 106a6bbb-becf-48d4-8eb0-31d80de2ff48
using Statistics

# ╔═╡ dea52539-5dc1-4ae2-bd36-3d44c80251b3
md"# The Great American Coffee Taste Test
TidyTuesday Week-20
"

# ╔═╡ d254f8d3-d4e5-4c2b-9ef5-6033725b8a3b
md"## Setup"

# ╔═╡ c1d64bec-1722-11ef-2f03-b71298e95ce9
pwd()

# ╔═╡ 36ce391f-9ab7-4b82-8808-0b250e81225c
readdir()

# ╔═╡ 6e4015d0-5b4c-4c2a-b8ac-f32f209c10df
md"### Libraries"

# ╔═╡ 963510bf-1b9a-4ee8-9cc4-56b3a2f6c065
#varinfo(PlutoUI)

# ╔═╡ 4abb7bac-52ba-4b25-8455-7d8010242498
md"### Data"

# ╔═╡ fc191507-1abc-48b4-8eb6-9a15c240a968
begin
	tt_url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/"
	dt_url = "data/2024/2024-05-14/coffee_survey.csv"
	dfr = CSV.File(download(tt_url * dt_url)) |> DataFrame;
	first(dfr, 3)
end

# ╔═╡ a1f180ee-735d-4f0a-a4ea-9e921421408c
md"## EDA"

# ╔═╡ 67306106-1bca-4ed3-a513-ee67de611ff6
md"### Overview"

# ╔═╡ 6525c227-7278-4693-8481-0890e0964ea2
dfr |> size

# ╔═╡ 4945fa74-1d1c-4069-80f6-f86862c2595f
dfr |> names

# ╔═╡ ed1c04b5-c538-4e89-bd30-fc68ec335d67
dfr |> describe

# ╔═╡ 20c673b5-706e-4e93-90b5-72a0bcf6bb72
describe(dfr, :eltype)

# ╔═╡ 79123708-9e33-456b-9de9-839b2452957f
md"### Data Analysis"

# ╔═╡ 9daec5c3-883f-4446-a725-220ae249ed83
md"#### Age groups"

# ╔═╡ b8709fed-7421-4048-b1cd-92fbdc2cd845
age_df = @pipe dfr |> 
	filter(x -> x.age  != "NA", _) |>
	groupby(_, :age) |>
	combine(_, nrow => :total) |>
	transform(_, 
		:age => col -> replace.(col, " years old" => "")) |>
	rename(_, :age_function => :age_cat)

# ╔═╡ 1a3c7a56-c981-488c-bae9-be6d3f912f37
age_df.age_cat = categorical(age_df.age_cat, ordered=true)

# ╔═╡ f7040a76-f7b4-4a69-b6ff-d513944f1ffb
ordered!(age_df.age_cat, true)

# ╔═╡ a4725bba-d24a-4c49-97e6-e61c0bcd7d4f
levels!(age_df.age_cat, ["<18", "18-24", "25-34", "35-44", "45-54", "55-64", ">65"])

# ╔═╡ f9c05f90-20df-409d-93f1-e25714f2e056
age_df_ordered = sort(age_df, :age_cat)

# ╔═╡ c6828111-8f06-4f10-858c-2a470f80e8a5
md"### Favorite coffee"

# ╔═╡ c61cb2a4-5131-445f-b737-009b651cd7ef
fav_coffee  = 
	@pipe dfr |>
	filter(x -> x.favorite  != "NA", _) |>
	groupby(_, :favorite) |>
	combine(_, nrow => :total) |>
	transform(_, 
		:favorite => fv -> replace.(fv, " (e.g. Frappuccino)" => "")) |>
	select(_, :total, :favorite_function => :favorite);

# ╔═╡ b6d11fac-8d67-4356-8ea2-3f54a5d1699e
fav_df_1 = @pipe fav_coffee |>
	sort(_, :total, rev = true);

# ╔═╡ 4d799a72-b0e0-495f-a7e7-eb3a6fa1b1b0
fav_df_2 = @pipe fav_coffee |>
	sort(_, :total);
	

# ╔═╡ 561c8578-d962-41f5-ad61-953984c17b89
md"### Gender - Education - Race"

# ╔═╡ e3b63160-d2c4-4c18-b73b-da167c06afc7
@pipe dfr |> 
	transform(
		_, 
		:gender => ByRow(
			g -> g ∈ ["Non-binary", "Prefer not to say", "Other (please specify)"] ? "Other" : g == "NA" ? "Unknown" : g ) => :gender,
		:ethnicity_race => ByRow(r -> 
			r == "White/Caucasian" ? "white" : 
			r == "Asian/Pacific Islander" ? "Asian/PI" : 
			r == "Black/African American" ? "Black/AA" : 
			r == "Hispanic/Latino" ? "Hispanic" : 
			r == "Native American/Alaska Native" ? "AIAN" : 
			r == "Other (please specify)" ? "Other" : "Unknown") => :race,
		:education_level => ByRow(e -> e == "NA" ? "Unknown" : e) => :education) |>
	filter(
		row -> 
			row.gender != "Unknown" && 
			row.race != "Unknown"   &&
			row.education != "Unknown", _) |>
	groupby(_, [:gender, :race, :education]) |>
	combine(_, 
		nrow => :count)

# ╔═╡ acb92fae-91a9-4c63-a296-f4a356a6aa80
gender_df = @pipe dfr |> 
	transform(
		_, 
		:gender => ByRow(
			g -> g ∈ ["Non-binary", "Prefer not to say", "Other (please specify)"] ? "Other" : g == "NA" ? "Unknown" : g ) => :gender) |>
	groupby(_, :gender) |>
	combine(_,  nrow => :count) |>
	sort(_, :count)

# ╔═╡ fec44ac1-a368-4d05-858d-c2416beadc38
race_df = @pipe dfr |> 
	transform(
		_, 
		:ethnicity_race => ByRow(r -> 
			r == "White/Caucasian" ? "white" : 
			r == "Asian/Pacific Islander" ? "Asian/PI" : 
			r == "Black/African American" ? "Black/AA" : 
			r == "Hispanic/Latino" ? "Hispanic" : 
			r == "Native American/Alaska Native" ? "AIAN" : 
			r == "Other (please specify)" ? "Other" : "Unknown") => :race) |>
	groupby(_, :race) |>
	combine(_,  nrow => :count) |>
	sort(_, :count)

# ╔═╡ da16708e-acb8-48b3-8b9f-01df0d9941c9


# ╔═╡ abfe3c52-76bd-4015-9c17-20b39cafeae4
edu_df = @pipe dfr |> 
	transform(
		_, 
		:education_level => ByRow(e -> e == "NA" ? "Unknown" : e) => :education) |>
	groupby(_, :education) |>
	combine(_, nrow => :count) |>
	transform(_, :education => categorical, renamecols = false)

# ╔═╡ 7deb6457-326f-4305-b68b-ca08629827bf
levels!(edu_df.education,
	
	[
		"Unknown", "Less than high school", "High school graduate", 
		"Bachelor's degree", "Some college or associate's degree", 
		"Master's degree", "Doctorate or professional degree"
	]
)

# ╔═╡ 4f1fc3e2-2c8c-46d0-b331-d6227e29c5be
edu_df_ordered = @pipe edu_df |> sort(_, :education)

# ╔═╡ d5b3f438-0c46-4662-973c-62b3c59cdc29
tryparse(Int, String("123"))

# ╔═╡ afff5be1-53c3-4d3e-b121-1d31ab40748b
parse.(Int, ["123", "456"])

# ╔═╡ 01a870e5-96ae-4801-be56-2c5a9e0d05b9
md"## Viz"

# ╔═╡ ce53979d-163e-4758-aa31-c0f869eae978
md"### PlotlyLight Intro"

# ╔═╡ 9573a9eb-c609-4442-beca-fc579fb70f03
md"**Basic line chart example**"

# ╔═╡ 958c7daa-5f6c-4668-8446-dfb650a6e350
md"
*Change template:*
 - ggplot2 - seaborn - simple_white
 - plotly - plotly\_white - plotly\_dark
 - presentation - xgridoff - ygridoff - gridon - none
"

# ╔═╡ cf9e913f-3f44-40b8-a430-9d68d6efd425
PlotlyLight.template!("plotly_dark")

# ╔═╡ 827299a3-e4c0-42fc-97ad-87d2c78ed771
begin
	p = Plot(x = 1:20, y = cumsum(randn(20)), type="scatter", mode="lines+markers")
	p.layout.title.text = "My Title!"
	p
end

# ╔═╡ 945088d3-aef8-4624-94e2-b8e1b6668b17
md"**Basic Bar chart exmaple**"

# ╔═╡ 601f93e6-b057-4273-ab67-fe8455755151
begin
	cats = ["Cat A", "Cat B", "Cat C", "Cat D"]
	vals = [15, 10, 7, 12]

	fig_barv = Plot(
		type="bar", 
		x = cats, 
		y = vals)
	fig_barv.layout.title.text = "Participants Age groups"
	fig_barv
end

# ╔═╡ 523a1c0b-b008-4bd3-8a46-6c4c92dd67ab


# ╔═╡ f82264d0-4043-4ea6-b57c-f4b0048926ad
md"### Age groups"

# ╔═╡ 58bac58c-35cd-4f01-8b29-83cd2ab68955
levels(age_df.age_cat)

# ╔═╡ db406c7d-8f51-4d6f-adfc-ba083784c6f9
begin
	ag = Plot(
		type="bar", 
		x = age_df.age, 
		y = age_df.total)
	ag.layout.title.text = "Participants Age groups"
	ag
end

# ╔═╡ 6f26039c-0ae6-41f3-bd14-6a06d062052b
begin
	agc = Plot(
		type="bar", 
		x = age_df_ordered.age_cat, 
		y = age_df_ordered.total)
	agc.layout.title.text = "Participants Age groups"
	agc
end

# ╔═╡ 5fcda9f3-dca5-4172-9cc3-b998ada2a51e
md"### Favorite coffee drinks"

# ╔═╡ d5560636-1c72-4bc7-97a9-66ea67d7da04
begin
	fav = Plot(
		type="bar", 
		x = fav_df_1.favorite, 
		y = fav_df_1.total)
	fav.layout.title.text = "Favorite coffee drinks"
	fav
end

# ╔═╡ 8c2e7abb-b381-4ada-9106-1cb4a02df7df
begin
	fav2 = Plot(
		type="bar", 
		y = fav_df_2.favorite, 
		x = fav_df_2.total,
		orientation="h")
	fav2.layout.title.text = "Favorite coffee drinks"
	fav2
end

# ╔═╡ 26f3fa35-7498-4b51-b92a-369849a3bf9d
md"### Gender - Educ - Race"

# ╔═╡ 546f9b63-ed23-4fca-b67c-a8c677628e36
begin
	gp = Plot(
		type="bar", 
		y = gender_df.gender, 
		x = gender_df.count,
		orientation="h"
	)
	gp.layout.title.text = "Survey participants by gender"
	gp
end

# ╔═╡ 4ff76867-8c3e-4321-891a-74e47287f1ad
begin
	rp = Plot(
		type="bar", 
		y = race_df.race, 
		x = race_df.count,
		orientation="h"
	)
	rp.layout.title.text = "Survey participants by race"
	rp
end

# ╔═╡ 41dbd597-6fe2-4b5e-bca4-dbd6ef14110a
begin
	ep = Plot(
		type="bar", 
		x = edu_df_ordered.education, 
		y = edu_df_ordered.count,
		#orientation="h"
	)
	ep.layout.title.text = "Survey participants by education level"
	ep
end

# ╔═╡ 1e7e90cd-93e4-453f-8605-16f129688dc3
begin
	epo = Plot(
		type="bar", 
		x = edu_df_ordered.education, 
		y = edu_df_ordered.count
	)
	epo.layout.title.text = "Survey participants by education level"
	epo
end

# ╔═╡ 87554384-fae7-4604-9d36-4c4c558f74bf
varinfo(PlutoUI)

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
CSV = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
CategoricalArrays = "324d7699-5711-5eae-9e2f-1d82baa6b597"
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
Pipe = "b98c9c47-44ae-5843-9183-064241ee97a0"
PlotlyLight = "ca7969ec-10b3-423e-8d99-40f33abb42bf"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
Statistics = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[compat]
CSV = "~0.10.14"
CategoricalArrays = "~0.10.8"
DataFrames = "~1.6.1"
Pipe = "~1.3.0"
PlotlyLight = "~0.9.1"
PlutoUI = "~0.7.59"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.10.3"
manifest_format = "2.0"
project_hash = "58e0274b1cdcfa6cb89afd92f6d95da110d6dce2"

[[deps.AbstractPlutoDingetjes]]
deps = ["Pkg"]
git-tree-sha1 = "6e1d2a35f2f90a4bc7c2ed98079b2ba09c35b83a"
uuid = "6e696c72-6542-2067-7265-42206c756150"
version = "1.3.2"

[[deps.ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"
version = "1.1.1"

[[deps.Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"

[[deps.Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"

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

    [deps.CategoricalArrays.extensions]
    CategoricalArraysJSONExt = "JSON"
    CategoricalArraysRecipesBaseExt = "RecipesBase"
    CategoricalArraysSentinelArraysExt = "SentinelArrays"
    CategoricalArraysStructTypesExt = "StructTypes"

    [deps.CategoricalArrays.weakdeps]
    JSON = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
    RecipesBase = "3cdcf5f2-1ef4-517c-9805-6587b60abb01"
    SentinelArrays = "91c51154-3ec4-41a3-a24f-3f23e20d615c"
    StructTypes = "856f2bd8-1eba-4b0a-8007-ebc267875bd4"

[[deps.Cobweb]]
deps = ["DefaultApplication", "OrderedCollections", "Scratch"]
git-tree-sha1 = "1d38f9c609b1d8b33319911b4f016da29e33a776"
uuid = "ec354790-cf28-43e8-bb59-b484409b7bad"
version = "0.6.1"

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

[[deps.Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"

[[deps.DefaultApplication]]
deps = ["InteractiveUtils"]
git-tree-sha1 = "c0dfa5a35710a193d83f03124356eef3386688fc"
uuid = "3f0dd361-4fe0-5fc6-8523-80b14ec94d85"
version = "1.1.0"

[[deps.Downloads]]
deps = ["ArgTools", "FileWatching", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"
version = "1.6.0"

[[deps.EasyConfig]]
deps = ["JSON3", "OrderedCollections", "StructTypes"]
git-tree-sha1 = "11fa8ecd53631b01a2af60e16795f8b4731eb391"
uuid = "acab07b0-f158-46d4-8913-50acef6d41fe"
version = "0.1.16"

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

[[deps.Future]]
deps = ["Random"]
uuid = "9fa8497b-333b-5362-9e8d-4d0656e87820"

[[deps.Hyperscript]]
deps = ["Test"]
git-tree-sha1 = "179267cfa5e712760cd43dcae385d7ea90cc25a4"
uuid = "47d2ed2b-36de-50cf-bf87-49c2cf4b8b91"
version = "0.0.5"

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

[[deps.IteratorInterfaceExtensions]]
git-tree-sha1 = "a3f24677c21f5bbe9d2a714f95dcd58337fb2856"
uuid = "82899510-4779-5014-852e-03e436cf321d"
version = "1.0.0"

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

[[deps.Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"

[[deps.MIMEs]]
git-tree-sha1 = "65f28ad4b594aebe22157d6fac869786a255b7eb"
uuid = "6c6e2e6c-3030-632d-7369-2d6c69616d65"
version = "0.1.4"

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

[[deps.NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"
version = "1.2.0"

[[deps.OpenBLAS_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Libdl"]
uuid = "4536629a-c528-5b80-bd46-f80d51c5b363"
version = "0.3.23+4"

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

[[deps.PlotlyLight]]
deps = ["Artifacts", "Cobweb", "Downloads", "EasyConfig", "JSON3", "REPL", "Random", "StructTypes"]
git-tree-sha1 = "5684d34d28ca87ef546a6cb9646d53c13f93a8ea"
uuid = "ca7969ec-10b3-423e-8d99-40f33abb42bf"
version = "0.9.1"

[[deps.PlutoUI]]
deps = ["AbstractPlutoDingetjes", "Base64", "ColorTypes", "Dates", "FixedPointNumbers", "Hyperscript", "HypertextLiteral", "IOCapture", "InteractiveUtils", "JSON", "Logging", "MIMEs", "Markdown", "Random", "Reexport", "URIs", "UUIDs"]
git-tree-sha1 = "ab55ee1510ad2af0ff674dbcced5e94921f867a9"
uuid = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
version = "0.7.59"

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
git-tree-sha1 = "88b895d13d53b5577fd53379d913b9ab9ac82660"
uuid = "08abe8d2-0d0c-5749-adfa-8a2ac140af0d"
version = "2.3.1"

[[deps.Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"

[[deps.REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"

[[deps.Random]]
deps = ["SHA"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"

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

[[deps.Scratch]]
deps = ["Dates"]
git-tree-sha1 = "3bac05bc7e74a75fd9cba4295cde4045d9fe2386"
uuid = "6c6a2e73-6563-6170-7368-637461726353"
version = "1.2.1"

[[deps.SentinelArrays]]
deps = ["Dates", "Random"]
git-tree-sha1 = "363c4e82b66be7b9f7c7c7da7478fdae07de44b9"
uuid = "91c51154-3ec4-41a3-a24f-3f23e20d615c"
version = "1.4.2"

[[deps.Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"

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

[[deps.TableTraits]]
deps = ["IteratorInterfaceExtensions"]
git-tree-sha1 = "c06b2f539df1c6efa794486abfb6ed2022561a39"
uuid = "3783bdb8-4a98-5b6b-af9a-565f29a5fe9c"
version = "1.0.1"

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

[[deps.TranscodingStreams]]
git-tree-sha1 = "5d54d076465da49d6746c647022f3b3674e64156"
uuid = "3bb67fe8-82b1-5028-8e26-92a6c54297fa"
version = "0.10.8"
weakdeps = ["Random", "Test"]

    [deps.TranscodingStreams.extensions]
    TestExt = ["Test", "Random"]

[[deps.Tricks]]
git-tree-sha1 = "eae1bb484cd63b36999ee58be2de6c178105112f"
uuid = "410a4b4d-49e4-4fbc-ab6d-cb71b17b3775"
version = "0.1.8"

[[deps.URIs]]
git-tree-sha1 = "67db6cc7b3821e19ebe75791a9dd19c9b1188f2b"
uuid = "5c2747f8-b7ea-4ff2-ba2e-563bfd36b1d4"
version = "1.5.1"

[[deps.UUIDs]]
deps = ["Random", "SHA"]
uuid = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

[[deps.Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"

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
# ╟─dea52539-5dc1-4ae2-bd36-3d44c80251b3
# ╟─d254f8d3-d4e5-4c2b-9ef5-6033725b8a3b
# ╟─c1d64bec-1722-11ef-2f03-b71298e95ce9
# ╟─36ce391f-9ab7-4b82-8808-0b250e81225c
# ╠═244fd8e8-0e4f-4baa-a523-3d0c9acf7b3e
# ╟─6e4015d0-5b4c-4c2a-b8ac-f32f209c10df
# ╠═216da246-13e2-4222-b218-4587f77f6db4
# ╠═cef7f42f-02d0-4e6e-bff4-145b828d3753
# ╠═7f197b67-d5ab-47d0-a331-f7b166d327ca
# ╠═106a6bbb-becf-48d4-8eb0-31d80de2ff48
# ╠═963510bf-1b9a-4ee8-9cc4-56b3a2f6c065
# ╟─4abb7bac-52ba-4b25-8455-7d8010242498
# ╠═fc191507-1abc-48b4-8eb6-9a15c240a968
# ╟─a1f180ee-735d-4f0a-a4ea-9e921421408c
# ╟─67306106-1bca-4ed3-a513-ee67de611ff6
# ╠═6525c227-7278-4693-8481-0890e0964ea2
# ╠═4945fa74-1d1c-4069-80f6-f86862c2595f
# ╠═ed1c04b5-c538-4e89-bd30-fc68ec335d67
# ╠═20c673b5-706e-4e93-90b5-72a0bcf6bb72
# ╟─79123708-9e33-456b-9de9-839b2452957f
# ╟─9daec5c3-883f-4446-a725-220ae249ed83
# ╠═b8709fed-7421-4048-b1cd-92fbdc2cd845
# ╠═1a3c7a56-c981-488c-bae9-be6d3f912f37
# ╠═f7040a76-f7b4-4a69-b6ff-d513944f1ffb
# ╠═a4725bba-d24a-4c49-97e6-e61c0bcd7d4f
# ╠═f9c05f90-20df-409d-93f1-e25714f2e056
# ╠═c6828111-8f06-4f10-858c-2a470f80e8a5
# ╠═c61cb2a4-5131-445f-b737-009b651cd7ef
# ╠═b6d11fac-8d67-4356-8ea2-3f54a5d1699e
# ╠═4d799a72-b0e0-495f-a7e7-eb3a6fa1b1b0
# ╟─561c8578-d962-41f5-ad61-953984c17b89
# ╠═e3b63160-d2c4-4c18-b73b-da167c06afc7
# ╠═acb92fae-91a9-4c63-a296-f4a356a6aa80
# ╠═fec44ac1-a368-4d05-858d-c2416beadc38
# ╠═da16708e-acb8-48b3-8b9f-01df0d9941c9
# ╠═abfe3c52-76bd-4015-9c17-20b39cafeae4
# ╠═7deb6457-326f-4305-b68b-ca08629827bf
# ╠═4f1fc3e2-2c8c-46d0-b331-d6227e29c5be
# ╠═d5b3f438-0c46-4662-973c-62b3c59cdc29
# ╠═afff5be1-53c3-4d3e-b121-1d31ab40748b
# ╟─01a870e5-96ae-4801-be56-2c5a9e0d05b9
# ╟─ce53979d-163e-4758-aa31-c0f869eae978
# ╟─9573a9eb-c609-4442-beca-fc579fb70f03
# ╟─958c7daa-5f6c-4668-8446-dfb650a6e350
# ╠═cf9e913f-3f44-40b8-a430-9d68d6efd425
# ╠═827299a3-e4c0-42fc-97ad-87d2c78ed771
# ╟─945088d3-aef8-4624-94e2-b8e1b6668b17
# ╠═601f93e6-b057-4273-ab67-fe8455755151
# ╠═523a1c0b-b008-4bd3-8a46-6c4c92dd67ab
# ╟─f82264d0-4043-4ea6-b57c-f4b0048926ad
# ╠═58bac58c-35cd-4f01-8b29-83cd2ab68955
# ╠═db406c7d-8f51-4d6f-adfc-ba083784c6f9
# ╠═6f26039c-0ae6-41f3-bd14-6a06d062052b
# ╟─5fcda9f3-dca5-4172-9cc3-b998ada2a51e
# ╠═d5560636-1c72-4bc7-97a9-66ea67d7da04
# ╠═8c2e7abb-b381-4ada-9106-1cb4a02df7df
# ╠═26f3fa35-7498-4b51-b92a-369849a3bf9d
# ╠═546f9b63-ed23-4fca-b67c-a8c677628e36
# ╠═4ff76867-8c3e-4321-891a-74e47287f1ad
# ╠═41dbd597-6fe2-4b5e-bca4-dbd6ef14110a
# ╠═1e7e90cd-93e4-453f-8605-16f129688dc3
# ╠═87554384-fae7-4604-9d36-4c4c558f74bf
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
