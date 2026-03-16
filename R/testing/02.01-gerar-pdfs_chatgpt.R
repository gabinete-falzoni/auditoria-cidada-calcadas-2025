---
  output:
  pdf_document:
  latex_engine: xelatex
keep_tex: false
geometry: margin = 1in
params:
  title: ""
text_blocks: NULL
link_blocks: NULL
image_path: ""
image_caption: ""
lon: NA
lat: NA
map_caption: ""
basemap:
  provider: "carto"     # osm | carto | mapbox
mapbox_style: NULL
mapbox_token: NULL
---

library(sf)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(knitr)
library(grid)
library(png)