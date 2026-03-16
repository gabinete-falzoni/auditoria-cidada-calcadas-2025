library('tidyverse')
library('tidylog')
# library('gpx')
library('sf')
library('mapview')
# library('measurements')
library('data.table')
library('lubridate')


# Pastas para abrigar sequência de imagens e vídeos reduzidos
gpkg_folder          <- '00_gpkg_shapefiles'
img_sequences_folder <- '01_image_sequences'
video_reduced_folder <- '02_video_red_files'

# Estrutura de pastas
pasta_base  <- '/mnt/fern/Dados/2025_Auditoria_Calcadas/pinheiros'
# pasta_base  <- '/home/livre/Downloads/auditoria_calcadas_testes_sinc'
pasta_dados <- sprintf('%s/00_dados_originais', pasta_base)
pasta_msens <- sprintf('%s/MultiSensor', pasta_dados)
pasta_proc  <- sprintf('%s/01_dados_processados', pasta_base)
# pasta_msens <- sprintf('%s/usp', pasta_base)
pasta_gpkg_shps <- sprintf('%s/%s', pasta_proc, gpkg_folder)
pasta_gpkg_bkps <- sprintf('%s/BKP', pasta_gpkg_shps)
pasta_qgis_imgs <- sprintf('%s/%s', pasta_proc, img_sequences_folder)
pasta_qgis_vids <- sprintf('%s/%s', pasta_proc, video_reduced_folder)
dir.create(pasta_gpkg_bkps, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_qgis_imgs, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_qgis_vids, recursive = TRUE, showWarnings = FALSE)


# Executáveis externos
# ffprobe_path  <- sprintf('/usr/bin/ffprobe')
ffmpeg_path   <- sprintf('/usr/bin/ffmpeg')
exiftool_path <- sprintf("/usr/bin/exiftool")


# ------------------------------------------------------------------------------
# Associar vídeos 360 às pastas do MultiSensor conforme o timestamp mais próximo
# ------------------------------------------------------------------------------

# Pastas MultiSensor - padrão de nomenclatura: 2025-08-27-07-29-48-127
pastas_ms <-
  tibble(pasta_msensor = list.files(pasta_msens, pattern = '^[0-9-]{23}$', full.names = TRUE)) %>%
  mutate(timestamp = ymd_hms(str_sub(basename(pasta_msensor), 1, 19)), .before = 1)

# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------

# Gerar frames de vídeos MultiSensor (1 frame a cada segundo de vídeo)
gerar_frames_multisensor <- function(base_file_name, pasta_ms, pasta_frames_ms) {
  # Extrair frames
  # ffmpeg -i video.mp4 -vf fps=1 -q:v 2 -y lala_%05d_ms.jpg
  message('\nExtraindo frames do vídeo do MuliSensor com o ffmpeg.\n')
  cmd <- sprintf(
    "%s -i %s/video.mp4 -vf fps=1 -q:v 2 -y %s/%s_%%05d_ms.jpg",
    # "%s -i %s/video.mp4 -vf fps=1/2 -q:v 2 -y %s/%s_%%05d_ms.jpg",
    ffmpeg_path, pasta_ms, pasta_frames_ms, base_file_name
  )
  system(cmd)
}


# Reduzir vídeo para resolução SD: 854x480
reduzir_video <- function(out_video_basename, pasta_ms, pasta_qgis_vids) {
  # Extrair frames
  # ffmpeg -i video.mp4 -vf scale=854:480 -preset veryfast teste_veryfast.mp4
  message('\nReduzindo vídeo para 854x480 com o ffmpeg.\n')
  cmd <- sprintf(
    "%s -i %s/video.mp4 -vf scale=854:480 -preset veryfast -y %s/%s.mp4",
    # "%s -i %s/video.mp4 -vf fps=1/2 -q:v 2 -y %s/%s_%%05d_ms.jpg",
    ffmpeg_path, pasta_ms, pasta_qgis_vids, out_video_basename
  )
  system(cmd)
}


# library('reticulate')
# this <- pastas_ms %>% slice(1)
# use_virtualenv(sprintf("%s/whisper-env", this$pasta_msensor), required = TRUE)
# py_config()
# whisper <- import("whisper")
# model <- whisper$load_model("tiny")
# result <- model$transcribe(sprintf("%s/audio.mp3", this$pasta_msensor), language = "pt")
# result$text
# tsv_lines <- lapply(result$segments, function(seg) {
#   sprintf("%.2f\t%.2f\t%s", seg$start, seg$end, seg$text)
# })
# tibble(tsv_lines) %>% unlist()
# writeLines(tsv_lines, sprintf("%s/audio_R.tsv", this$pasta_msensor))
#
# tibble(result) %>% unlist()


for (i in seq(1, nrow(pastas_ms))) {
  # this <- pastas_ms %>% slice(1)
  this <- pastas_ms %>% slice(i)

  pasta_ms <- this$pasta_msensor


  # "2025-09-12-09-32-52-497"
  base_file_name <- basename(pasta_ms)
  # Timestamp inicial MultiSensor - vem do nome da pasta - "20250912-093252"
  ts_inicial_ms <- str_replace_all(base_file_name, '-', '')
  ts_inicial_ms <- str_c(str_sub(ts_inicial_ms, 1, 8), str_sub(ts_inicial_ms, 9, 17), sep = '-')

  # Gerar frames a partir do vídeo do MultiSensor
  pasta_frames_ms <- sprintf('%s/%s', pasta_qgis_imgs, ts_inicial_ms)
  dir.create(pasta_frames_ms, recursive = TRUE, showWarnings = FALSE)
  gerar_frames_multisensor(base_file_name, pasta_ms, pasta_frames_ms)

  # Gerar vídeo em tamanho menor
  reduzir_video(ts_inicial_ms, pasta_ms, pasta_qgis_vids)


  # Puxar GPS gravado pelo MultiSensor
  gps <- sprintf('%s/gps.csv', pasta_ms)
  gps <- read_delim(gps, delim = ',', col_types = "Tiddd")

  gps <- gps %>%
    # head(20) %>%
    # Descartar primeiros 5 pontos (5 segundos) %>%
    # slice(6:nrow(.)) %>%
    # Ajustar fuso horário (-3 horas)
    mutate(datetime = datetime_utc - hours(3),
           # Diferença de tempos entre cada GPS
           # time_diff = as.numeric(datetime_utc - lag(datetime_utc), units = "secs"),
           point_id = row_number()) %>%
    # select(point_id, datetime, time_diff, latitude, longitude) %>%
    select(point_id, datetime, latitude, longitude) %>%
    setDT()

  # Qual o timestamp do primeiro GPS?
  gps_inicio <- gps$datetime[1]
  # Qual a diferença de tempo entre o primeiro GPS e o primeiro frame de vídeo?
  # dif_gps_ms <- round(as.double(gps_inicio - ymd_hms(str_sub(ts_inicial_ms, 1, 15))))

  # Associar frames MultiSensor às coordenadas GPS
  base_img_folder <- sprintf('%s/%s/', img_sequences_folder, basename(pasta_frames_ms))
  frames_ms_times <-
    tibble(imagepath = paste0(base_img_folder, list.files(pasta_frames_ms, pattern = '\\.jpg$', full.names = FALSE))) %>%
    # mutate(datetime  = seq(from = ymd_hms(base_file_name), by = 4, length.out = nrow(.))) %>%
    mutate(datetime  = seq(from = ymd_hms(str_sub(ts_inicial_ms, 1, 15)), by = 1, length.out = nrow(.))) %>%
    setDT()

  # frames_gps <- gps[frames_ms_times, roll = "nearest", on = 'datetime']
  frames_gps <- frames_ms_times[gps, roll = "nearest", on = 'datetime',
                                # df1[df2, roll = "nearest", on = .(timestamp = timestamp_insta),
                                # i. refers to the right table (df2)
                                .(point_id,
                                  timestamp_gps = i.datetime,
                                  # x. refers to the left table in the join (df1)
                                  timestamp_ms = x.datetime,
                                  time_diff = abs(x.datetime - i.datetime),
                                  latitude,
                                  longitude,
                                  imagepath)]


  # Transformar em dataframe, reordenar colunas e remover fotos que estão com
  # a mesma coordenada geográfica de outras (pegar só a última)
  frames_gps <-
    frames_gps %>%
    tibble() %>%
    # Filtrar primeiro item de id em dataframe com repetições do id nas linhas
    # group_by(point_id) %>%
    group_by(imagepath) %>%
    # Se houver mais de uma associação por ponto GPS, pegar a última
    # filter(row_number() == n()) %>%
    # Se houver mais de uma associação por ponto GPS, pegar a com menor time_diff
    filter(time_diff == min(time_diff)) %>%
    ungroup()


  # Criar colunas com link para vídeo e segundo de início
  frames_gps <- frames_gps %>% mutate(video_path = sprintf('%s/%s.mp4', video_reduced_folder, ts_inicial_ms),
                                      # Vamos adicionar 3 segundos a mais para
                                      # iniciar vídeo, para ajudar na sinc
                                      # start_time = row_number() + dif_gps_ms + 3,
                                      start_time = as.integer(str_sub(basename(imagepath), -12, -8)),
                                      time_diff = round(as.double(time_diff), 3))

  # this <- read_sf(g)
  # this <- this %>%
  #   # st_drop_geometry() %>%
  #   # select(imagepath) %>%
  #   mutate(imagepath_win = str_replace_all(imagepath, '/', '\\\\')) %>%
  #   # select(imagepath_win) %>%
  #   mutate(imagepath_win = str_c(img_claros, imagepath_win))
  #
  # this <- this %>%
  #   # st_drop_geometry() %>%
  #   # select(video_path) %>%
  #   mutate(videopath_win = str_replace_all(video_path, '/', '\\\\')) %>%
  #   # select(videopath_win) %>%
  #   mutate(videopath_win = str_c(img_claros, videopath_win))

  # Criar shapefile
  frames_gps <- frames_gps %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  # mapview(frames_gps, cex = 1, legend = FALSE)


  # Exportar
  out_gpkg <- sprintf('%s/%s.gpkg', pasta_gpkg_shps, base_file_name)
  st_write(frames_gps, out_gpkg, driver = 'GPKG', append = FALSE)

  # Exportar o mesmo shapefile para pasta de backup
  out_gpkg_bkp <- sprintf('%s/%s.gpkg', pasta_gpkg_bkps, base_file_name)
  st_write(frames_gps, out_gpkg_bkp, driver = 'GPKG', append = FALSE)

}

