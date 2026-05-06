# Insere coordenadas latlon e infos de timestamp nas fotos que serão publicadas
# no Mapillary. Daqui, a ideia é subir as fotos no Mapillary, esperar até que
# sejam processadas na plataforma (pode levar uns 3 dias) e baixá-las com o
# script seguinte

library('tidyverse')
library('tidylog')
library('sf')
library('lubridate')
# library('mapview')

pasta_base  <- "/mnt/fern/Dados"
pasta_audi  <- sprintf("%s/projetos/2025_Auditoria_Calcadas", pasta_base)
pasta_proc  <- sprintf("%s/01_dados_processados", pasta_audi)
pasta_publi <- sprintf("%s/06_publicacao", pasta_proc)
pasta_resul <- sprintf('%s/resultados', pasta_publi)
pasta_web   <- sprintf("%s/qgis2web/keyframes", pasta_publi)


# ------------------------------------------------------------------------------
# Copiar imagens usadas como keyframes para pasta de publicação
# ------------------------------------------------------------------------------

# result <- sprintf('%s/auditoria_calcadas_bras.gpkg', pasta_resul)
# result <- read_sf(result) %>% st_drop_geometry() %>% filter(kf_flag)
#
# result <- result %>% select(imagepath)
#   mutate(imagepath = str_c(pasta_proc, imagepath, sep = '/'))
#
#
# for (img in result$imagepath) {
#   # img <- result$imagepath[1]
#   # print(img)
#
#   img_in  <- str_c(pasta_proc, img, sep = '/')
#   img_out <- str_c(pasta_web, basename(img), sep = '/')
#   file.copy(from = img_in, to = img_out)
# }
#
# rm(result, img, img_in, img_out)


# ------------------------------------------------------------------------------
# Puxar keyframes da auditoria
# ------------------------------------------------------------------------------

result <- sprintf('%s/auditoria_calcadas_bras.gpkg', pasta_resul)
result <- read_sf(result) %>% filter(kf_flag)
result <- result %>%
  select(timestamp_ms, imagepath) %>%
  mutate(imagepath = str_c(pasta_web, basename(imagepath), sep = '/'),
         timestamp_ms = timestamp_ms + hours(3),
         timestamp_ms = format(timestamp_ms, '%Y-%m-%dT%H:%M:%SZ'),
         lon = st_coordinates(geom)[, 1],
         lat = st_coordinates(geom)[, 2]) %>%
  distinct(imagepath, .keep_all = TRUE) %>%
  st_drop_geometry() %>%
  arrange(imagepath)



# ------------------------------------------------------------------------------
# Inserir timestamps e coordenadas GPS nas fotos
# ------------------------------------------------------------------------------

exiftool_path <- "/usr/bin/exiftool"

# Inserir timestamp nas fotos - -overwrite_original é opcional
# exiftool -XMP:DateTimeOriginal='2025-01-28T10:53:43Z' .
# for (i in seq_len(nrow(result))) {
#   args <- c(
#     sprintf('-XMP:DateTimeOriginal=%s', result$timestamp_ms[i]),
#     "-overwrite_original",
#     result$imagepath[i]
#   )
#
#   system2(exiftool_path, args)
#
# }


# Inserir coordenadas e infos de timestamp nas fotos
for (i in seq_len(nrow(result))) {

  lat <- result$lat[i]
  lon <- result$lon[i]
  ts  <- result$timestamp_ms[i]

  args <- c(
    sprintf('-XMP:DateTimeOriginal=%s', ts),
    sprintf('-EXIF:DateTimeOriginal=%s', ts),

    sprintf('-GPSLatitude=%f', abs(lat)),
    sprintf('-GPSLatitudeRef=%s', ifelse(lat >= 0, "N", "S")),

    sprintf('-GPSLongitude=%f', abs(lon)),
    sprintf('-GPSLongitudeRef=%s', ifelse(lon >= 0, "E", "W")),

    "-overwrite_original",
    result$imagepath[i]
  )

  system2(exiftool_path, args)
}


# Limpar ambiente
rm(args, lat, lon, ts, i)


# ------------------------------------------------------------------------------
# Redistribuir imagens em pastas para Mapillary não confundir ordenação
# ------------------------------------------------------------------------------

# Listar pastas
folders <- result %>%
  select(imagepath) %>%
  mutate(imagepath = str_remove(imagepath, pasta_web)) %>%
  mutate(folder = str_sub(imagepath, 2, 24)) %>%
  select(folder) %>%
  distinct()

# Para cada pasta listada, criar pasta e mover imagens para ela
for (folder in folders$folder) {
  # folder <- folders$folder[1]
  out_folder <- file.path(pasta_web, folder)
  dir.create(out_folder, recursive = TRUE, showWarnings = FALSE)

  mv_imgs <- list.files(pasta_web, pattern = sprintf('%s_[0-9]{5}_ms.jpg$', folder))
  for (img in mv_imgs) {
    # img <- mv_imgs[1]
    file.rename(from = file.path(pasta_web, img),
                to = file.path(out_folder, img))
  }
}
