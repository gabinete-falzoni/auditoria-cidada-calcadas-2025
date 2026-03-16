# Processa as pastas originais de gravação (MultiSensor), extraindo os frames,
# vídeos em resolução mais baixa e arquivos gps a serem usados nas análises

library('tidyverse')
library('tidylog')
library('sf')
library('mapview')
library('data.table')
library('lubridate')


# Pastas para abrigar sequência de imagens e vídeos reduzidos
img_sequences_folder <- '01_image_sequences'
video_reduced_folder <- '02_videos_low_res'
gpkg_folder          <- '03_gpkg_shapefiles'

# Estrutura de pastas
# pasta_base  <- '/mnt/fern/Dados/2025_Auditoria_Calcadas/pinheiros'
pasta_base  <- '/media/livre/Expansion/projetos/2025_Auditoria_Calcadas'
pasta_dados <- sprintf('%s/00_dados_originais', pasta_base)
pasta_msens <- sprintf('%s/MultiSensor', pasta_dados)
pasta_proc  <- sprintf('%s/01_dados_processados', pasta_base)
# pasta_msens <- sprintf('%s/usp', pasta_base)
pasta_gpkg_shps <- sprintf('%s/%s', pasta_proc, gpkg_folder)
pasta_csvs_bkps <- sprintf('%s/BKP_CSVs', pasta_gpkg_shps)
pasta_gpkg_bkps <- sprintf('%s/BKP_GPKG', pasta_gpkg_shps)
pasta_qgis_imgs <- sprintf('%s/%s', pasta_proc, img_sequences_folder)
pasta_qgis_vids <- sprintf('%s/%s', pasta_proc, video_reduced_folder)
dir.create(pasta_csvs_bkps, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_gpkg_bkps, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_qgis_imgs, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_qgis_vids, recursive = TRUE, showWarnings = FALSE)


# Executáveis externos
ffmpeg_path   <- sprintf('/usr/bin/ffmpeg')
exiftool_path <- sprintf("/usr/bin/exiftool")


# ------------------------------------------------------------------------------
# Funções
# ------------------------------------------------------------------------------

# Gerar frames de vídeos MultiSensor (1 frame a cada segundo de vídeo)
gerar_frames_multisensor <- function(base_file_name, pasta_ms, pasta_frames_ms, force = FALSE) {

  # Rodar somente se pasta não existir ou não estiver vazia, exceto com force == TRUE
  if (dir.exists(pasta_frames_ms) & length(list.files(pasta_frames_ms)) > 0 & force == FALSE) {
    warning(sprintf(
      'Pasta %s existe e não está vazia. Rode com force = TRUE para sobrescrevê-la.',
      pasta_frames_ms))
  } else {
    # Extrair frames: ffmpeg -i video.mp4 -vf fps=1 -q:v 2 -y lala_%05d_ms.jpg
    message(sprintf(
      '\nExtraindo frames do vídeo %s do MuliSensor com o ffmpeg.\n',
      base_file_name))
    cmd <- sprintf(
      "%s -i %s/video.mp4 -vf fps=1 -q:v 2 -y %s/%s_%%05d_ms.jpg",
      # "%s -i %s/video.mp4 -vf fps=1/2 -q:v 2 -y %s/%s_%%05d_ms.jpg",
      ffmpeg_path, pasta_ms, pasta_frames_ms, base_file_name
    )
    system(cmd)
  }
}


# Reduzir vídeo para resolução SD: 854x480
reduzir_video <- function(out_video_basename, pasta_ms, pasta_qgis_vids, force = FALSE) {
  # Rodar somente se pasta não existir ou não estiver vazia, exceto com force == TRUE
  out_video_name <- sprintf('%s/%s.mp4', pasta_qgis_vids, out_video_basename)
  if (file.exists(out_video_name) & force == FALSE) {
    warning(sprintf(
      'Vídeo de baixa resolução %s já existe. Rode com force = TRUE para sobrescrevê-lo.',
      out_video_name))
  } else {
    # ffmpeg -i video.mp4 -vf scale=854:480 -preset veryfast teste_veryfast.mp4
    message(sprintf(
      '\nReduzindo vídeo %s para 854x480 com o ffmpeg.\n',
      out_video_basename))
    cmd <- sprintf(
      "%s -i %s/video.mp4 -vf scale=854:480 -preset veryfast -g 1 -c:v libx264 -crf 23 -y %s/%s.mp4",
      ffmpeg_path, pasta_ms, pasta_qgis_vids, out_video_basename
    )
    system(cmd)
  }
}


# ------------------------------------------------------------------------------
# Listar pastas com gravações MultiSensor
# ------------------------------------------------------------------------------

# Pastas MultiSensor - padrão de nomenclatura: 2025-08-27-07-29-48-127
pastas_ms <-
  tibble(pasta_msens = list.files(pasta_msens,
                                  pattern = '^[0-9-]{23}$',
                                  include.dirs = TRUE,
                                  full.names = TRUE,
                                  recursive = TRUE)) %>%
  # Extrair datetime dos nomes das pastas
  mutate(timestamp = ymd_hms(str_sub(basename(pasta_msens), 1, 19)),
         # Extrair milissegundos dos nomes das pastas
         ms = as.numeric(str_sub(basename(pasta_msens), 21, 23)),
         .before = 1) %>%
  # Adicionar milissegundos aos timestamps
  mutate(timestamp = timestamp + seconds(ms / 1000)) %>%
  select(-ms)



for (i in seq(1, nrow(pastas_ms))) {
  # this <- pastas_ms %>% slice(1)
  this <- pastas_ms %>% slice(i)

  pasta_ms <- this$pasta_msens

  # "2025-09-12-09-32-52-497"
  base_file_name <- basename(pasta_ms)
  # Manter registro do dia/pessoa que fez o registro em campo
  dados_campo <- str_extract(pasta_ms, 'D[0-9]P[0-9]')
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
  gps_file <- sprintf('%s/gps.csv', pasta_ms)
  gps <- read_delim(gps_file, delim = ',', col_types = "Tiddd")

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

  # Inserir registro dos dados de campo
  frames_gps <- frames_gps %>% mutate(campo = dados_campo, .before = 'imagepath')

  # Criar shapefile
  frames_gps <- frames_gps %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  # mapview(frames_gps, cex = 1, legend = FALSE)


  # Exportar somente se arquivo ainda não existir
  out_gpkg <- sprintf('%s/%s.gpkg', pasta_gpkg_shps, base_file_name)
  if (!file.exists(out_gpkg)) {
    st_write(frames_gps, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)
  }

  # Exportar um backup dos arquivos .gpkg
  out_gpkg2 <- sprintf('%s/%s.gpkg', pasta_gpkg_bkps, base_file_name)
  if (!file.exists(out_gpkg2)) {
    st_write(frames_gps, out_gpkg2, driver = 'GPKG', append = FALSE, delete_layer = TRUE)
  }

  # Fazer backup do arquivo original .csv
  bkp_csv <- sprintf('%s/%s.csv', pasta_csvs_bkps, base_file_name)
  if (!file.exists(bkp_csv)) {
    file.copy(from = gps_file, to = bkp_csv)
  }


}

