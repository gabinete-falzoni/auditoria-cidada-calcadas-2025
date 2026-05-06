# Baixar imagens do Mapillary

library('tidyverse')
library('tidylog')
library('sf')
library('httr')
library('jsonlite')
library('future.apply')

pasta_base <- '/mnt/fern/Dados/gitlab'
pasta_fotos <- '/mnt/fern/Dados/projetos/2025_Auditoria_Calcadas/01_dados_processados/06_publicacao/qgis2web/fotos_mapillary'
dir.create(pasta_fotos, showWarnings = FALSE)

# ------------------------------
# User parameters
# ------------------------------
# [1] just in case there are extra empty lines
access_token <- readLines(file.path(pasta_base, 'z_txts/api_mapillary.txt'), warn = FALSE)[1]
# user_id      <- "9033162670054571"
creator_name   <- "gabinete_falzoni"
target_user_id <- "982503570396388"
# Optional date range
start_date <- "2025-11-08T00:00:00Z"  # or NULL
end_date   <- "2025-11-17T23:59:59Z"  # or NULL

# Bounding box - Brás
# https://bboxfinder.com
bbox <- c(-46.628036, -23.549392, -46.605377, -23.529917)


# ------------------------------
# Helper function: split bbox into grid tiles
# ------------------------------
make_grid <- function(bbox, nx = 4, ny = 4) {
  xmin <- bbox[1]; ymin <- bbox[2]; xmax <- bbox[3]; ymax <- bbox[4]

  xs <- seq(xmin, xmax, length.out = nx + 1)
  ys <- seq(ymin, ymax, length.out = ny + 1)

  grid <- list()
  k <- 1
  for (i in 1:nx) {
    for (j in 1:ny) {
      grid[[k]] <- c(xs[i], ys[j], xs[i + 1], ys[j + 1])
      k <- k + 1
    }
  }
  return(grid)
}

# ------------------------------
# Generate tiles
# ------------------------------
tiles <- make_grid(bbox, nx = 4, ny = 4)

# ------------------------------
# Function to build URL for a tile
# ------------------------------
build_url <- function(tile, start_date=NULL, end_date=NULL) {
  url <- paste0(
    "https://graph.mapillary.com/images?",
    "fields=id,computed_geometry,thumb_original_url,captured_at,creator",
    "&bbox=", paste(tile, collapse = ","),
    "&creator_username=", creator_name,
    "&limit=1000",
    "&access_token=", access_token
  )

  if (!is.null(start_date)) url <- paste0(url, "&start_captured_at=", start_date)
  if (!is.null(end_date))   url <- paste0(url, "&end_captured_at=", end_date)

  return(url)
}

# ------------------------------
# Fetch images for all tiles with pagination
# ------------------------------
all_images <- list()

for (tile in tiles) {
  cat("Processing tile:", paste(tile, collapse = ","), "\n")

  next_url <- build_url(tile, start_date, end_date)

  while (!is.null(next_url)) {
    res <- GET(next_url)
    json <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

    if (!"data" %in% names(json)) break

    all_images <- append(all_images, list(json$data))

    next_url <- if (!is.null(json$paging[["next"]])) json$paging[["next"]] else NULL
    Sys.sleep(0.2)  # avoid rate limiting
  }
}

# ------------------------------
# Combine results and remove duplicates
# ------------------------------
images_df <- bind_rows(all_images) %>% distinct(id, .keep_all = TRUE)

# images_df$captured_datetime <- as.POSIXct(
#   images_df$captured_at / 1000,
#   origin = "1970-01-01",
#   tz = "UTC"
# )

# as.POSIXct(1745406731000 / 1000, origin = "1970-01-01", tz = "UTC")

# ------------------------------
# Filter by user
# ------------------------------
user_images <- images_df %>% filter(creator.id == target_user_id)
user_images <- user_images %>% rename(url = thumb_1024_url) %>% filter(!is.na(url))

# ------------------------------
# Extract coordinates and convert timestamps
# ------------------------------
user_images <- user_images %>%
  mutate(
    lon = sapply(computed_geometry.coordinates, `[`, 1),
    lat = sapply(computed_geometry.coordinates, `[`, 2),
    captured_datetime = as.POSIXct(captured_at / 1000, origin = "1970-01-01", tz = "UTC")
  )


# Baixar fotos do Mapillary
plan(multisession)

future_lapply(seq_len(nrow(user_images)), function(i) {
  url <- user_images$url[i]
  file <- file.path(pasta_fotos, paste0(user_images$id[i], ".jpg"))

  tryCatch({
    if (!file.exists(file)) {
      download.file(url, file, mode = "wb", quiet = TRUE)
      Sys.sleep(0.1)
    }
  }, error = function(e) {
    NULL
  })
})


# ------------------------------
# Optional: save as GeoJSON
# ------------------------------
sf_points <- st_as_sf(user_images, coords = c("lon","lat"), crs = 4326)
sf_points <- sf_points %>% select(id, captured_datetime)
st_write(sf_points, "user_images.geojson", delete_dsn = TRUE)

# ------------------------------
# Final output
# ------------------------------
print(paste("Total images retrieved for user:", nrow(user_images)))
head(user_images)