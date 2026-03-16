library('tidyverse')
library('tidylog')
library('gpx')
library('sf')
library('mapview')
# library('measurements')
library('data.table')
library('lubridate')


# Estrutura de pastas
pasta_base  <- '/mnt/fern/Dados/2025_Auditoria_Calcadas'
# pasta_base  <- '/home/livre/Downloads/auditoria_calcadas_testes_sinc'
pasta_dados <- sprintf('%s/00_dados_originais', pasta_base)
pasta_insta <- sprintf('%s/Insta360', pasta_dados)
pasta_msens <- sprintf('%s/MultiSensor', pasta_dados)
pasta_proc  <- sprintf('%s/01_dados_processados', pasta_base)
pasta_mapillary <- sprintf('%s/01_mapillary_sequences', pasta_proc)
pasta_qgis_imgs <- sprintf('%s/02_image_sequences', pasta_proc)
dir.create(pasta_proc, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_mapillary, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_qgis_imgs, recursive = TRUE, showWarnings = FALSE)
# pasta_dados <- sprintf('%s/00_pasta_de_trabalho', pasta_base)
# pasta_fila  <- sprintf('%s/fila', pasta_dados)
# pasta_frames_ret <- sprintf('%s/03_image_sequences', pasta_base)


# Executáveis externos
ffmpeg_path   <- sprintf('/usr/bin/ffmpeg')
ffprobe_path  <- sprintf('/usr/bin/ffprobe')
exiftool_path <- sprintf("/usr/bin/exiftool")


# ------------------------------------------------------------------------------
# Associar vídeos 360 às pastas do MultiSensor conforme o timestamp mais próximo
# ------------------------------------------------------------------------------

# Pastas MultiSensor - padrão de nomenclatura: 2025-08-27-07-29-48-127
pastas_ms <-
  tibble(pasta_msensor = list.files(pasta_msens, pattern = '^[0-9-]{23}$', full.names = TRUE)) %>%
  mutate(timestamp = ymd_hms(str_sub(basename(pasta_msensor), 1, 19)), .before = 1)

# Vídeos Insta360 - VID_20250912_093249_00_742.insv; VID_20250912_093249_10_742.insv
videos_insta <-
  # Pegar somente vídeo 00
  tibble(video_insta1 = list.files(pasta_insta, pattern = '^VID_[0-9_]{15}_00_[0-9]{3}.insv$', full.names = TRUE),
         video_insta2 = list.files(pasta_insta, pattern = '^VID_[0-9_]{15}_10_[0-9]{3}.insv$', full.names = TRUE)) %>%
  mutate(timestamp_insta = str_sub(basename(video_insta1), 5, 19), .before = 1) %>%
  mutate(timestamp_insta = ymd_hms(timestamp_insta))

# Converter para data.table
df1 <- setDT(pastas_ms)
df2 <- setDT(videos_insta)

# Definir coluna para join
setkey(df2, timestamp_insta)

# Associar videos insta360 à pasta MultiSensor de acordo com o timestamp mais
# próximo via rolling join nearest
# arquivos <- tibble(df1[df2, roll = "nearest", on = "timestamp"])
# arquivos <- df1[df2, roll = "nearest", on = .(timestamp = timestamp_insta)]
arquivos <- df1[df2, roll = "nearest", on = .(timestamp = timestamp_insta),
                  # i. refers to the right table (df2)
                .(timestamp_insta = i.timestamp_insta,
                  # x. refers to the left table in the join (df1)
                  timestamp_ms = x.timestamp,
                  time_diff = abs(x.timestamp - i.timestamp_insta),
                  video_insta1,
                  video_insta2,
                  pasta_msensor)]
arquivos <- arquivos %>% tibble()

# # Adicionar timestamp
# arquivos <- df1[arquivos, on = 'pasta_msensor']
#
# arquivos <- arquivos %>% tibble() %>%  select(timestamp_insta = i.timestamp,
#                                               timestamp_ms = timestamp,
#                                               video_insta1,
#                                               video_insta2,
#                                               pasta_msensor)


# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------

# # Puxar a data de criação do vídeo com ffprobe
# puxar_data_criacao_video <- function(video_file) {
#   arg_o1 <- sprintf('-v quiet -select_streams v:0 -show_entries stream_tags=creation_time -of default=noprint_wrappers=1:nokey=1 "%s"', video_file)
#   system2(command = ffprobe_path, args = c(arg_o1))
#
# cmd <- sprintf(
#   "%s -v quiet -select_streams v:0 -show_entries stream_tags=creation_time \\
#     -of default=noprint_wrappers=1:nokey=1 %s/video.mp4",
#   ffprobe_path, pasta_ms
# )
# system(cmd)
# }

# Gera vídeo .mp4 dual fisheye a partir de dois vídeos (frente/verso) .insv
gerar_video_dual_fisheye <- function(video1, video2, out_mp4) {
  # Converter arquivos .insv para .mp4 (dual fisheye)
  # ffmpeg -i VID_20250409_140416_00_003.insv -i VID_20250409_140416_10_003.insv -filter_complex "[0:v][1:v]hstack=inputs=2" -c:v libx264 -crf 23 -preset ultrafast boo.mp4
  message('\nConvertendo arquivos .insv em .mp4 com o ffmpeg.\n')
  arg_o1 <- sprintf('-i "%s"', video1)
  arg_o2 <- sprintf('-i "%s"', video2)
  # arg_o3 <- sprintf('-filter_complex "[0:v][1:v]hstack=inputs=2" -c:v libx264 -crf 23 -preset ultrafast -y %s', out_mp4)
  arg_o3 <- sprintf('-filter_complex "[0:v][1:v]hstack=inputs=2" -c:v libx264 -crf 0 -y %s', out_mp4)
  # Caso o arquivo original tenha sido gravado como vídeo (arquivo maior que 500 MB),
  # extrair como 1 FPS em vez de 29.97 (-r 1)
  if (file.size(video1) > 500000000) {
    arg_o3 <- sprintf('-r 0.25 -filter_complex "[0:v][1:v]hstack=inputs=2" -c:v libx264 -crf 0 -y %s', out_mp4)
  }
  system2(command = ffmpeg_path, args = c(arg_o1, arg_o2, arg_o3))
}


# Gera frames 360° para Mapillary a partir de vídeo dual fish eye
gerar_frames_mapillary <- function(base_file_name, out_mp4, pasta_frames) {
  # Definir valores para corrigir distorção no encontro entre as duas imagens 180°
  fov <- 195

  # Converter arquivo .mp4 em sequência de JPGs
  # ffmpeg -i tmp_video.mp4 -vf v360=dfisheye:e:yaw=0:ih_fov=195:iv_fov=195 output_%05d.jpg
  message('\nConvertendo arquivo .mp4 em sequência JPG com o ffmpeg.\n')
  arg_o1 <- sprintf('-i "%s"', out_mp4)
  arg_o2 <- sprintf('-vf v360=dfisheye:e:yaw=0:ih_fov=%s:iv_fov=%s -qmin 1 -q:v 1', fov, fov)
  arg_o3 <- sprintf('%s/%s_%%05d.jpg', pasta_frames, base_file_name)
  system2(command = ffmpeg_path, args = c(arg_o1, arg_o2, arg_o3))
}


# Insere timestamp de criação no primeiro frame; atualiza os demais a cada n segundos
inserir_timestamps_frames <- function(ts_inicial, intervalo_seg = 4, pasta_frames) {

  # Inserir timestamp inicial nas fotos - -overwrite_original é opcional
  # exiftool -XMP:DateTimeOriginal='2025-01-28T10:53:43Z' .
  message('\nInserindo timestamp inicial nas fotos.\n')
  cmd <- sprintf(
    "%s -XMP:DateTimeOriginal=%s -overwrite_original %s",
    exiftool_path, ts_inicial, pasta_frames
  )
  system(cmd)

  # Atualizar timestamp a cada x segundos - -overwrite_original é opcional
  # exiftool '-XMP:DateTimeOriginal+<0:0:${filesequence;$_*=4}' $(ls -1v *.jpg)
  message('\nInserir valores do timestamp a cada X segundos.\n')
  cmd <- sprintf(
    "%s '-XMP:DateTimeOriginal+<0:0:${filesequence;$_*=%s}' -overwrite_original $(ls -1v %s/*.jpg)",
    exiftool_path, intervalo_seg, pasta_frames
  )
  system(cmd)

}


# Gerar frames retilineares a partir dos frames 360°, para serem vistos no QGIS
gerar_frames_retilineares <- function(base_file_name, pasta_frames, pasta_frames_ret) {

  # Campo de visão - se câmera tiver gravado muito pra cima, aumentar para ~135
  fov2 <- 90

  # Extrair as porções frontais dos frames
  # ffmpeg -framerate 1 -start_number 1 -i 20250409_143230_%05d.jpg -vf "v360=e:rectilinear:yaw=0:h_fov=90:v_fov=90" -y 00_lala_%05d.jpg
  message('\nExtraindo frames retilineares das imagens 360° com o ffmpeg.\n')
  cmd <- sprintf(
    "%s -framerate 1 -start_number 1 -i %s/%s_%%05d.jpg \\
    -vf 'v360=e:rectilinear:yaw=0:h_fov=%s:v_fov=%s,scale=1920:1080' \\
    -q:v 2 -y %s/%s_%%05d_ret.jpg",
    ffmpeg_path, pasta_frames, base_file_name, fov2, fov2, pasta_frames_ret, base_file_name
  )
  system(cmd)

}


# Gerar frames de vídeos MultiSensor (1 frame a cada segundo de vídeo)
gerar_frames_multisensor <- function(base_file_name, pasta_ms, pasta_frames_ms) {
  # Extrair frames
  # ffmpeg -framerate 1 -start_number 1 -i 20250409_143230_%05d.jpg -vf "v360=e:rectilinear:yaw=0:h_fov=90:v_fov=90" -y 00_lala_%05d.jpg
  message('\nExtraindo frames do vídeo do MuliSensor com o ffmpeg.\n')
  cmd <- sprintf(
    "%s -i %s/video.mp4 -vf fps=1 -q:v 2 -y %s/%s_%%05d_ms.jpg",
    # "%s -i %s/video.mp4 -vf fps=1/2 -q:v 2 -y %s/%s_%%05d_ms.jpg",
    ffmpeg_path, pasta_ms, pasta_frames_ms, base_file_name
  )
  system(cmd)
}




for (i in seq(1, nrow(arquivos))) {
  # this <- arquivos %>% slice(2)
  this <- arquivos %>% slice(i)

  video1 <- this$video_insta1
  video2 <- this$video_insta2
  pasta_ms <- this$pasta_msensor


  # VID_20250409_140416_00_003.insv -> 20250409_140416
  base_file_name <- basename(video1) %>% str_sub(5, 19)
  # Timestamp vindo do nome do vídeo da Insta360 - 2025-09-12T09:32:42.000000Z
  ts_inicial <- format(ymd_hms(base_file_name), '%Y-%m-%dT%H:%M:%SZ')
  # Timestamp inicial MultiSensor - vem do nome da pasta - "20250912-093252"
  ts_inicial_ms <- str_replace_all(basename(this$pasta_msensor), '-', '')
  ts_inicial_ms <- str_c(str_sub(ts_inicial_ms, 1, 8), str_sub(ts_inicial_ms, 9, 14), sep = '-')

  # Gerar vídeo dual fish-eye
  out_mp4 <- sprintf('%s/%s.mp4', pasta_proc, base_file_name)
  # gerar_video_dual_fisheye(video1, video2, out_mp4)

  # Gera frames 360° para Mapillary (ffmpeg, sem metadados ou coord. geográficas)
  pasta_frames <- sprintf('%s/%s', pasta_mapillary, base_file_name)
  dir.create(pasta_frames, recursive = TRUE, showWarnings = FALSE)
  # gerar_frames_mapillary(base_file_name, out_mp4, pasta_frames)

  # Não precisamos mais do arquivo .mp4
  # file.remove(out_mp4)

  # Inserir metadados de DateTimeOriginal nos frames a cada X segundos
  # inserir_timestamps_frames(ts_inicial, intervalo_seg = 4, pasta_frames)

  # Gerar frames retilineares para uso no QGIS (ffmpeg, sem metadados)
  pasta_frames_ret <- sprintf('%s/%s', pasta_qgis_imgs, base_file_name)
  dir.create(pasta_frames_ret, recursive = TRUE, showWarnings = FALSE)
  # gerar_frames_retilineares(base_file_name, pasta_frames, pasta_frames_ret)


  # Gerar frames a partir do vídeo do MultiSensor
  pasta_frames_ms <- sprintf('%s/tmp_frames_ms_%s', pasta_proc, base_file_name)
  dir.create(pasta_frames_ms, recursive = TRUE, showWarnings = FALSE)
  # gerar_frames_multisensor(base_file_name, pasta_ms, pasta_frames_ms)


  # Puxar GPS gravado pelo MultiSensor
  gps <- sprintf('%s/gps.csv', pasta_ms)
  gps <- read_delim(gps, delim = ',', col_types = "Tiddd")
  gps <- gps %>%
    # Ajustar fuso horário (-3 horas)
    mutate(datetime = datetime_utc - hours(3),
           point_id = row_number()) %>%
    select(point_id, datetime, latitude, longitude) %>%
    setDT()

  # Associar frames retilineares às coordenadas GPS
  frames_ret_times <-
    tibble(imagepath = list.files(pasta_frames_ret, pattern = '\\.jpg$', full.names = TRUE)) %>%
    mutate(datetime  = seq(from = ymd_hms(base_file_name), by = 4, length.out = nrow(.))) %>%
    setDT()

  lala <- gps[frames_ret_times, roll = "nearest", on = 'datetime']


  # Associar frames MultiSensor às coordenadas GPS
  frames_ms_times <-
    tibble(imagepath = list.files(pasta_frames_ms, pattern = '\\.jpg$', full.names = TRUE)) %>%
    # mutate(datetime  = seq(from = ymd_hms(base_file_name), by = 4, length.out = nrow(.))) %>%
    mutate(datetime  = seq(from = ymd_hms(ts_inicial_ms), by = 1, length.out = nrow(.))) %>%
    setDT()

  frames_gps <- frames_ms_times[lala, roll = "nearest", on = 'datetime']
  rm(lala)

  # Transformar em dataframe, reordenar colunas e remover fotos que estão com
  # a mesma coordenada geográfica de outras (pegar só a última)
  frames_gps <-
    frames_gps %>%
    tibble() %>%
    select(point_id,
           datetime,
           imagepath_ret = i.imagepath,
           imagepath_ms = imagepath,
           lat = latitude,
           lon = longitude) %>%
    # Filtrar primeiro item de id em dataframe com repetições do id nas linhas
    group_by(point_id) %>%
    filter(row_number() == n()) %>%
    ungroup()

  # Criar shapefile
  frames_gps <- frames_gps %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
  mapview(frames_gps, cex = 1, legend = FALSE)

  # Exportar
  out_gpkg <- sprintf('%s/teste_multisensor.gpkg', pasta_proc)
  st_write(frames_gps, out_gpkg, driver = 'GPKG', append = FALSE)

}





# ------------------------------------------------------------------------------
# Inserir timestamps nas fotos - Parte 1: Inserir e replicar primeiro timestamp
# ------------------------------------------------------------------------------

# Precisamos do valor do primeiro timestamp do GPX revisado
gpx <- sprintf('%s/gpx_revisto.gpx', pasta_dados)
min_time <- read_gpx(gpx)
min_time <- format(min(min_time$waypoints$Time), '%Y-%m-%dT%H:%M:%SZ')





# ------------------------------------------------------------------------------
# Gerar gpx para revisão
# ------------------------------------------------------------------------------

# Temos um arquivo .gpx gerado pelo osmtracker. O problema: ele deveria estar
# gravando os pontos a cada 4 segundos, mas está gerando a intervalos completamente
# aleatórios.Vamos precisar abrir esse .gpx, reescrever as infos de tempo e
# revisá-lo no QGIS para ver se está tudo ok.

# Abrir arquivo GPX
gpx <- list.files(pasta_fila, pattern = '.*\\.gpx$', full.names = TRUE)
gpx <- data.frame(read_gpx(gpx)$tracks) %>% select(Elevation = 1,
                                                   Time = 2,
                                                   Latitude = 3,
                                                   Longitude = 4,
                                                   speed = 5,
)

# Puxar endereços das imagens - vamos querer ver também se há mais imagens do que
# pontos no .gox
images <- data.frame(X1 = list.files(pasta_frames, pattern = '.*\\.jpg$', full.names = FALSE))
images <- images %>% mutate(X1 = str_replace(X1, '\\.jpg', '_ret.jpg'),
                            imagepath = str_c(pasta_frames_ret, basename(X1), sep = '/'))

qtd_ptos <- nrow(gpx); qtd_fotos <- nrow(images)
print(sprintf('Linhas GPX: %s - Qtd fotos: %s', qtd_ptos, qtd_fotos))


# Se quantidade de fotos for maior do que os pontos registrados no arquivo .gpx,
# vamos fazer a associação a partir do último ponto e repetir os primeiros. Isso
# porque a gravação em campo está sendo feita:
# 1. Rec na câmera, que demora vários segundos para começar a gravar;
# 2. Uma vez gravando, iniciar a gravação no osm_tracker;
# 3. Percorrer o trecho;
# 4. Parar a gravação na câmera;
# 5. Parar a gravação no osm_tracker.
# Por isso, sincronizar de trás para frente deve gerar menos imagens deslocadas
# no que se refere ao geoposicionamento
if (qtd_fotos > qtd_ptos) {
  # Inverter a ordem as imagens e dos pontos gravados
  images <- images %>% arrange(desc(X1))
  gpx <- gpx %>% arrange(desc(Time))

  # Adicionar linhas ao gpx, copiando os valores do primeiro registro (agora, na
  # última linha)
  ultima_linha <- gpx %>% tail(1)

  # Linhas a adicionar
  novas_linhas <- qtd_fotos - qtd_ptos

  # Adicionar novas linhas, repetindo valor da última
  gpx <- gpx %>% bind_rows(replicate(novas_linhas, ultima_linha, simplify = FALSE))

  # Juntar dataframes e voltar à ordem original de gravação
  images <- cbind(images, gpx) %>% arrange(X1) %>% select(-Time)

  # Precisamos redistribuir proporcionalmente os pontos no espaço, para facilitar
  # a revisão da posição deles no QGIS

  # 1. Converter os pontos em sf LINESTRING
  linha <- images %>%
    select(lon = Longitude, lat = Latitude) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%  # WGS84
    # Transformar para SIRGAS 23S para considerar distâncias corretamente
    st_transform(31983) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("LINESTRING")

  mapview(linha)

} else if (qtd_fotos <= qtd_ptos) {
  # 1. Converter os pontos em sf LINESTRING
  linha <- gpx %>%
    select(lon = Longitude, lat = Latitude) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%  # WGS84
    # Transformar para SIRGAS 23S para considerar distâncias corretamente
    st_transform(31983) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("LINESTRING")

  mapview(linha)
}

# 2. Transformar linha em uma sequência de pontos, na qual a quantidade desses
# pontos está definida por qtd_fotos
pontos_equidistantes <- linha %>%
  st_line_sample(n = qtd_fotos, type = "regular") %>%
  st_cast("POINT") %>%
  st_sf() %>%
  # Transformar de volta em WGS84 para obter os latlon
  st_transform(4326)

mapview(pontos_equidistantes, cex = 2)


# Conferir a partir do original
# gpx %>% select(lon = Longitude, lat = Latitude) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(col.regions = 'red', cex = 3)

# Juntar novas coordenadas geográficas em um sf
images <- cbind(pontos_equidistantes, images)

# Converter em dataframe com colunas de latlon
# pontos_equidistantes <-
#   pontos_equidistantes %>%
#   mutate(id = row_number()) %>%
#   mutate(
#     lon = st_coordinates(.)[,1],
#     lat = st_coordinates(.)[,2]
#   ) %>%
#   select(id, lat, lon)

# rm(ultima_linha, qtd_fotos, qtd_ptos, linha, pontos_equidistantes)


# O GPX não está gravando os pontos no intervalo de tempo correto, que seria de
# 4 segundos. Vamos pegar a primeira ocorrência de tempo e forçar um registro a
# cada 4s, substituindo os tempos originais
min_time <- as.POSIXct(min(gpx$Time), origin = "1970-01-01")

# Criar série a cada 4s
time_series <- min_time + seq(0, by = 4, length.out = nrow(images))
time_series <- data.frame(Time = time_series)

# Substituir tempos no gpx
images <- images %>% cbind(time_series)


# Gerar shapefile a partir do gpx
# images <- images %>% st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = TRUE)

out_file <- sprintf('%s/gpx_para_revisao.gpkg', pasta_fila)
st_write(images, out_file, driver = 'GPKG', append = FALSE, delete_layer = TRUE)


# Limpar ambiente
print(sprintf('Linhas GPX: %s - Qtd fotos: %s', qtd_ptos, qtd_fotos))
rm(list = ls())
gc(T)

# ------------------------------------------------------------------------------
# Revisar arquivo .GPX no QGIS e salvar arquivo revisado como novo .gpx
# ------------------------------------------------------------------------------

# Ao abrir o arquivo no QGIS:
# 1. Checar como as fotos estão posicionadas no mapa e ajustar onde necessário;
#
# 2. Colar os pontos nas estruturas cicloviárias, utilizando os scripts PyQGIS:
# - A. Gravar latlon originais no shape (01_latlon_to_columns.py);
# - B. Colar pontos nas estruturas cicloviárias (02_move_selected_points.py);
# - C. Desfazer B (acima) caso preciso (03_latlon_cols_to_geometry.py);
# - D. Gerar colunas de lat lon com novas coordenadas (04_latlon_rev_to_cols.py)
#
# 3. Uma vez feito isso, exportar como .GPX, habilitando as opções:
# GPX_USE_EXTENSIONS = TRUE e
# FORCE_GPX_TRACK = TRUE
#
# Salvar arquivo como "gpx_revisto.gpx" na mesma pasta_dados


# Se precisar, usar Processing Toolbox > Points along geometry para criar pontos em
# trechos do OSM (camada sao_paulo_osm_filtrado) que não estão com infra cicloviária ainda
# Conversão: 2 metros = 0.000018 em graus