### A Pluto.jl notebook ###
# v0.19.43

using Markdown
using InteractiveUtils

# ╔═╡ a48f5366-fd84-48ac-86ca-5a5f4538f1de
begin
	using PlutoUI
	TableOfContents()
end

# ╔═╡ c6f6c677-9975-4334-8f85-81a0f6472cf2
using Makie

# ╔═╡ ad834892-03cb-4534-81e7-42d8645bbb61
using WGLMakie

# ╔═╡ e4408004-1ed1-4d6f-b380-de4daa339dfa
using Plots

# ╔═╡ 2257f548-47c4-11ef-3309-c7cbe98d7a5b
html"""
<div style="position: absolute; width: calc(100% - 30px); border: 50vw solid white;
border-top: 500px solid white; border-bottom: none; box-sizing: content-box;
left: calc(-50vw + 15px); top: -500px; height: 160px; pointer-events: none;">
</div>

<div style=" height: 160px; width: 300%; background: white; color: white;
padding-top: 5px; padding-left: 5px;
">

<span style="font-family:Fira;font-weight:200;font-feature-settings:'lnum','pnum';"> 

<p style=" font-family: Roboto; font-size: 2rem; font-weight: 700; opacity: 1.0;
color: crimson;
	">ENGLISH WOMEN FOOTBALL
</p>

<p style="text-align: left; font-size: 1.8rem; color: firebrick">
	TidyTuesday Data: Week-29
</p>
<p style=" font-family: 'Julia mono'; font-size: 1rem; font-weight: 200; color: darkgrey;">
	&copy birusod
</p>
"""

# ╔═╡ 43e4a376-225c-4f67-8f67-d263ba23393b
md"---"

# ╔═╡ 07bcd7b7-d375-4739-9b2b-bf74c87be777
md"# Setup"

# ╔═╡ 27db505f-52bb-43b1-817e-fd855ae623ff
html"""
<h2 style="
	text-align:left;
	font-size: 1.6rem;
	font-family:roboto;
	font-weight:bold;
	color: darkred">
		Packages
</h2>
"""

# ╔═╡ 3f3c6d4b-d7a8-4418-912c-57e6a0d28ae3
begin
	f = Figure()
	Axis(f[1, 1])

	xs = 1:0.2:10
	ys = 0.5 .* sin.(xs)

	p = barplot!(xs, ys, color = :red, strokecolor = :black, strokewidth = 1)
end

# ╔═╡ ac53cd6e-bb10-4b76-aa39-e816c43c6f5a
plot(p)

# ╔═╡ 988e4249-3a6a-4e81-a726-3a377761d22c
# Define your data (replace with your actual data)
data = [5, 8, 2, 10, 7]

# ╔═╡ 1c6b63f7-35cb-4c25-920f-2e5d7f4d5bea
# Create a scene
f2 = scene(width = 400, height = 400);

# ╔═╡ c289f4c2-a4a0-4bd4-9e8a-a1913897a814


# ╔═╡ 94ceb4c3-956f-4d26-8c45-e7a65b76e624
# Grid dimensions (adjust based on data length)
rows = 2

# ╔═╡ 40b77dc7-4049-4a66-b417-65ff736e0a84
cols = 3

# ╔═╡ 43f440a9-da1b-4c3e-8009-caaf97abdfe6
for i in 1:length(data)
  row = (i - 1) // cols + 1
  col = mod(i - 1, cols) + 1
  
  # Adjust square size and position based on grid layout
  square = rect(left = col * 100, bottom = row * 100, width = 80, height = 80)
  
  # Color the square based on data value (replace with your logic)
  if data[i] > 7
      f << square;
      square.fillcolor = :darkgray
  else
      f << square;
      square.fillcolor = :lightgray
  end
end

# ╔═╡ 6282c92f-61b5-4955-b7c8-5a314ca237ed
# Display the plot
display(f)

# ╔═╡ Cell order:
# ╟─2257f548-47c4-11ef-3309-c7cbe98d7a5b
# ╟─43e4a376-225c-4f67-8f67-d263ba23393b
# ╟─a48f5366-fd84-48ac-86ca-5a5f4538f1de
# ╠═07bcd7b7-d375-4739-9b2b-bf74c87be777
# ╠═27db505f-52bb-43b1-817e-fd855ae623ff
# ╠═c6f6c677-9975-4334-8f85-81a0f6472cf2
# ╠═ad834892-03cb-4534-81e7-42d8645bbb61
# ╠═e4408004-1ed1-4d6f-b380-de4daa339dfa
# ╠═3f3c6d4b-d7a8-4418-912c-57e6a0d28ae3
# ╠═ac53cd6e-bb10-4b76-aa39-e816c43c6f5a
# ╠═988e4249-3a6a-4e81-a726-3a377761d22c
# ╠═1c6b63f7-35cb-4c25-920f-2e5d7f4d5bea
# ╠═c289f4c2-a4a0-4bd4-9e8a-a1913897a814
# ╠═94ceb4c3-956f-4d26-8c45-e7a65b76e624
# ╠═40b77dc7-4049-4a66-b417-65ff736e0a84
# ╠═43f440a9-da1b-4c3e-8009-caaf97abdfe6
# ╠═6282c92f-61b5-4955-b7c8-5a314ca237ed
