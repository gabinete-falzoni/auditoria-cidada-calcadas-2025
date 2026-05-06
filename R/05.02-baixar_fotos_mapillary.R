# Baixa as fotos do Mapillary. A lógica é traçar um bounding box delimitando a
# área de busca das fotos e filtrá-las tanto por data quanto por usuário (esse
# filtro de usuário não está na documentação, mas funciona). No caso do Brás,
# dividindo as áreas de busca por 23x22 metros aproximadamente, quase todas as
# fotos foram baixadas - faltaram umas 5.

library('tidyverse')
library('tidylog')
library('sf')
library('httr')
library('lubridate')
library('geosphere')
library('furrr')
library('future.apply')
# library('jsonlite')

pasta_base <- '/mnt/fern/Dados/gitlab'
pasta_publi <- '/mnt/fern/Dados/projetos/2025_Auditoria_Calcadas/01_dados_processados/06_publicacao'
# pasta_publi <- '/media/livre/Expansion/projetos/2025_Auditoria_Calcadas/01_dados_processados/06_publicacao'
pasta_resul <- sprintf('%s/resultados', pasta_publi)
pasta_fotos <- sprintf('%s/qgis2web/fotos_mapillary', pasta_publi)
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

# Bounding box - Brás  - a área tem cerca de 2.300 m de largura por 2.150 de altura)
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
# Diminuir grid caso as imagens não baixem como um todo. Neste caso, cada seção
# vai ter aproximadamente 23x22 metros.
tiles <- make_grid(bbox, nx = 100, ny = 100)

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
# all_images <- list()
#
# for (tile in tiles) {
#   cat("Processing tile:", paste(tile, collapse = ","), "\n")
#
#   next_url <- build_url(tile, start_date, end_date)
#
#   while (!is.null(next_url)) {
#     res <- GET(next_url)
#     json <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
#
#     if (!"data" %in% names(json)) break
#
#     all_images <- append(all_images, list(json$data))
#
#     next_url <- if (!is.null(json$paging[["next"]])) json$paging[["next"]] else NULL
#     Sys.sleep(0.2)  # avoid rate limiting
#   }
# }



fetch_tile <- function(tile) {
  cat("Processing tile:", paste(tile, collapse = ","), "\n")

  all_images <- list()
  next_url <- build_url(tile, start_date, end_date)

  repeat {
    res <- GET(next_url)
    json <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

    if (!"data" %in% names(json)) break

    all_images <- append(all_images, list(json$data))

    if (!is.null(json$paging[["next"]])) {
      next_url <- json$paging[["next"]]
      Sys.sleep(runif(1, 0.2, 0.5))  # jitter helps avoid throttling
    } else {
      break
    }
  }

  if (length(all_images) == 0) return(NULL)

  bind_rows(all_images)
}

plan(multisession, workers = 4)
all_images <- future_map(
  tiles,
  fetch_tile,
  .progress = TRUE
)


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
user_images <- user_images %>% rename(url = thumb_original_url) %>% filter(!is.na(url))

# ------------------------------
# Extract coordinates and convert timestamps
# ------------------------------
user_images <- user_images %>%
  mutate(
    lon = sapply(computed_geometry.coordinates, `[`, 1),
    lat = sapply(computed_geometry.coordinates, `[`, 2),
    captured_datetime = as.POSIXct(captured_at / 1000, origin = "1970-01-01", tz = "UTC")
  )


# Gravar arquivo com fotos e coordenadas
write_delim(user_images, file.path(pasta_fotos, '../fotos_mapillary.csv'), delim = ';')


# ------------------------------
# Baixar fotos do Mapillary
# ------------------------------

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
# Transformar em shapefile
# ------------------------------

# Resultados da auditoria - vamos puxar o latlon original
result <- sprintf('%s/auditoria_calcadas_bras.gpkg', pasta_resul)
result <- read_sf(result) %>% filter(kf_flag)
result <- result %>%
  select(timestamp_ms, geom) %>%
  arrange(timestamp_ms) %>%
  mutate(timestamp_ms = as.character(timestamp_ms + hours(3)),
         lon = st_coordinates(geom)[, 1],
         lat = st_coordinates(geom)[, 2]) %>%
  st_drop_geometry()


# Fotos baixadas do Mapillary - latlon pode ter sofrido um pouco no processo
fotos <- file.path(pasta_fotos, '../fotos_mapillary.csv')
fotos <- read_delim(fotos, delim = ';', col_types = 'cccccccddT')
fotos <- fotos %>%
  select(captured_datetime, id, lon, lat) %>%
  arrange(captured_datetime) %>%
  # De alguma forma, as fotos estão com 3 horas a mais do que deveriam - corrigir
  mutate(captured_datetime = as.character(captured_datetime - hours(3)))


# Reconstituir latlon original das fotos
fotos <- fotos %>%
  # Como algumas fotos possuem o mesmo momento de captura em HHMMSS, haverá duplicatas
  left_join(result, by = c('captured_datetime' = 'timestamp_ms')) %>%
  # Vamos calcular a distância entre os pontos (original vs Mapillay)...
  mutate(dist_m = distVincentyEllipsoid(cbind(lon.x, lat.x), cbind(lon.y, lat.y))) %>%
  #... para deixar somente o que tiver menor distância entre eles
  group_by(id) %>%
  filter(dist_m == min(dist_m)) %>%
  # tally() %>% ungroup() %>% filter(n > 1)
  ungroup() %>%
  # ... e para finalmente puxar o lat lon original em vez do alterado pelo Mapillary
  select(captured_datetime, id, lon = lon.y, lat = lat.y)


# Criar endereço para ver fotos no servidor e no Mapillaty
# https://www.mapillary.com/app/user?lat=-23.50242890063116&lng=-46.61111296893637&z=19.9&menu=false&panos=true&all_coverage=false&dateFrom=2025-04-09&dateTo=2025-06-30&username%5B%5D=gabinete_falzoni&pKey=1277960299983573
fotos <- fotos %>%
  # URL para fotos no servidor: coluna foto_url, <img src='../fotos/2022-05-10_13-23-52.jpg'>
  # https://falzoni.com.br/auditoria_calcadas/fotos/964044312767569.jpg
  # Para visualização local
  # fotos <- fotos %>% mutate(foto_url = str_c("<img src='", pasta_fotos_mapillary_crop, "/", X1, "'>", sep = ''), .after = X1)
  mutate(foto_url = str_c("<img src='https://falzoni.com.br/auditoria_calcadas/fotos/", id, ".jpg'>", sep = ''),
         # mapillary_url = str_c(
         #   '<h3><a href="https://www.mapillary.com/app/user?lat=', lat,
         #   '&lng=', lon,
         #   # '&z=18&menu=false&panos=true&all_coverage=false&dateFrom=2025-04-09&dateTo=2025-06-30&username%5B%5D=gabinete_falzoni&mapStyle=Esri+navigation" target="_blank">',
         #   '&z=18&menu=false&panos=true&all_coverage=false&dateFrom=2025-11-08&dateTo=2025-11-17&username%5B%5D=gabinete_falzoni" target="_blank">',
         #   'Clique aqui para navegar nas fotos (Mapillary)',
         #   '</a></h3>',
         #   sep = '')
         mapillary_url = str_c(
           'https://www.mapillary.com/app/user?lat=', lat,
           '&lng=', lon,
           # '&z=18&menu=false&panos=true&all_coverage=false&dateFrom=2025-04-09&dateTo=2025-06-30&username%5B%5D=gabinete_falzoni&mapStyle=Esri+navigation" target="_blank">',
           '&z=18&menu=false&dateFrom=2025-11-08&dateTo=2025-11-17&username%5B%5D=gabinete_falzoni&&pKey=', id,
           sep = '')
  )

# Transformar em shapefile
fotos <- fotos %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326, remove = TRUE)

out_file <- file.path(pasta_publi, 'shapes_base/fotos_auditoria.gpkg')
st_write(fotos, out_file, driver = 'GPKG', append = FALSE, delete_layer = TRUE)
