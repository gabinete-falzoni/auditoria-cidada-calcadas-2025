library(sf)
library(ggplot2)
library(ggmap)
library(rmarkdown)
library(magick)

# Replace with your shapefile path
shapefile_path <- "path/to/your/shapefile.shp"
points_data <- st_read(shapefile_path)

generate_pdf_report <- function(record) {
  # Extract information from record
  title <- record$col1
  description <- record$col2
  reference <- record$col3
  image_path <- record$imagepath

  # Get coordinates
  coords <- st_coordinates(record)
  lon <- coords[1]
  lat <- coords[2]

  # Get the base map from OSM
  map <- get_map(location = c(lon, lat), zoom = 15, maptype = "roadmap")

  # Create a filename for the PDF
  pdf_file <- paste0("report_", gsub(" ", "_", title), ".pdf")

  # Create the PDF document
  rmarkdown::render("report_template.Rmd", output_file = pdf_file, params = list(
    title = title,
    description = description,
    reference = reference,
    image_path = image_path,
    map = map
  ))
}

for (i in 1:nrow(points_data)) {
  generate_pdf_report(points_data[i, ])
}
