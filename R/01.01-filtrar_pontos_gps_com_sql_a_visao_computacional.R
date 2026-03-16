# Uma vez que os pontos GPS foram revisados no QGIS e as camadas foram unificadas
# por script de PyQGIS em uma camada única e associados aos lotes via nearest
# neighbor (também por PyQGIS), este script usa essa camada para pegar como o
# ponto "do meio", resultando em um ponto único para cada lote. As marcações dos
# algoritmos do IME, como a de problemas de superfície, são somadas e calculadas
# de forma proporcional, de forma a gerar uma visualização

library('tidyverse')
library('tidylog')
library('sf')
library('mapview')
library('janitor')


# Estrutura de pastas
pasta_base <- '/media/livre/Expansion'
# pasta_audi  <- '/mnt/fern/Dados/2025_Auditoria_Calcadas/pinheiros'
pasta_iptu <- sprintf('%s/dados/Geosampa/CADASTRO/IPTU/IPTU_2025/', pasta_base)
pasta_audi <- sprintf('%s/projetos/2025_Auditoria_Calcadas', pasta_base)
pasta_proc <- sprintf('%s/01_dados_processados', pasta_audi)
pasta_analises <- sprintf('%s/04_analises', pasta_proc)
# dir.create(pasta_analises, showWarnings = FALSE)


# Base de pontos GPS revisada, já associada aos lotes
gps_rev <- sprintf('%s/merged_pontos_gps_revisados_sql_lotes.gpkg', pasta_analises)
gps_rev <- read_sf(gps_rev)
# Este sql está duplicado na base, sobreponto a uma divisão de lotes no mesmo local
gps_rev <- gps_rev %>% filter(sql != '0180820016')


# Análises Visão Computacional
vc <- sprintf('%s/predictions.csv', pasta_analises)
vc <- read_delim(vc, delim = ',', col_types = "ciiii")
vc <- vc %>%
  mutate(imagepath = str_c('01_image_sequences', image, sep = '/'), .before = 1) %>%
  select(-image)


# ------------------------------------------------------------------------------
# Resumir dados por lote, tendo o GPS "do meio" como referência
# ------------------------------------------------------------------------------

sql_summary <-
  gps_rev %>%
  # Puxar lat long para trabalhar como dataframe
  mutate(lon = st_coordinates(geom)[, 1],
         lat = st_coordinates(geom)[, 2]) %>%
  st_drop_geometry() %>%
  # Unir dados vindos de visão computacional
  left_join(vc, by = 'imagepath') %>%
  # select(sql, point_id, imagepath) %>%
  # Reconhecer mudança de grupos - cada vez que um sql muda, o grupo muda
  mutate(
    # Criar coluna temporária com NAs como string para serem comparáveis na criação de grupos
    sql_tmp = coalesce(sql, "__NA__"),
    # Quando valor na linha seguinte muda, is_change é TRUE
    is_change = sql_tmp != lag(sql_tmp) | is.na(sql_tmp) != is.na(lag(sql_tmp)),
    # Marcar a primeira linha de cada grupo
    is_change = replace_na(is_change, TRUE),
    # Numeração cumulativa dos grupos (cumsum)
    group_id  = cumsum(is_change)
  ) %>%
  group_by(group_id) %>%
  mutate(
    # Somar quantas linhas há em cada grupo
    group_n = n(),
    # Linhas a serem mantidas são as que 'descartar' não está marcada como 'sim'
    keep = is.na(descartar) | descartar == FALSE,
    # Tamanho do grupo, ignorando descartar == TRUE
    group_n_eff = sum(keep),
    # Identificar linha do meio, dentre as linhas que serão mantidas
    mid_pos = ceiling(group_n_eff / 2),
    # Número de linha, somente entre linhas que serão mantidas
    # row_pos = if_else(keep, cumsum(keep), NA_integer_)
    row_pos = if_else(keep, cumsum(keep), 0)
  ) %>%
  ungroup() %>%
  # Remover pontos marcados para serem descartados
  filter(is.na(descartar)) %>%
  # Remover colunas temporárias
  select(-c(is_change, sql_tmp, group_n_eff, keep)) %>%
  # head(20) %>%
  group_by(group_id) %>%
  # Todos os valores vão se referir aos valores do meio, pois queremos meio que
  # um "centróide" de cada lote (sql ou, em outras palavras, group_id)
  summarise(point_id = point_id[row_pos == mid_pos],
            sql = sql[row_pos == mid_pos],
            campo = campo[row_pos == mid_pos],
            imagepath = imagepath[row_pos == mid_pos],
            video_path = video_path[row_pos == mid_pos],
            lon = lon[row_pos == mid_pos],
            lat = lat[row_pos == mid_pos],
            # Somente para o início do vídeo, que vamos querer no começo do lote
            start_time = first(start_time),
            # Duração de play do vídeo é a quantidade de linhas (segundos) por lote
            video_duration = group_n[row_pos == mid_pos],
            # Calcular a quantidade de marcações por grupo para cada item
            crosswalk = sum(crosswalk, na.rm = TRUE),
            curbramp = sum(curbramp, na.rm = TRUE),
            surfaceproblem = sum(surfaceproblem, na.rm = TRUE),
            obstacle = sum(obstacle, na.rm = TRUE),
            # Marcações proporcionais de cada item, por grupo
            crosswalk_prop = round(sum(crosswalk, na.rm = TRUE) / n(), 2),
            curbramp_prop = round(sum(curbramp, na.rm = TRUE) / n(), 2),
            surfaceproblem_prop = round(sum(surfaceproblem, na.rm = TRUE) / n(), 2),
            obstacle_prop = round(sum(obstacle, na.rm = TRUE) / n(), 2),
            ) %>%
  ungroup() %>%
  # filter(sql == '0020030022') %>%
  # filter(group_id %in% c(1230, 1279)) %>%
  mutate(
    # Reduzir 1 segundo do início do vídeo, para garantir reprodução
    # start_time = ifelse(is.na(start_time) | start_time <= 1, 1, start_time - 1),
    # Duração de play do vídeo deve ser de pelo menos 5 segundos
    video_duration = ifelse(video_duration < 5, 5, video_duration)
    )

# sql_summary %>% group_by(imagepath) %>% tally() %>% filter(n > 2)
# sql_summary %>% group_by(group_id) %>% tally() %>% filter(n > 1)
# sql_summary %>% filter(group_id == 3651)
# gps_rev %>% filter(sql == '0250050155' & campo == 'D3P1')
# sql_summary %>% filter(is.na(lat) | is.na(lon))
sql_summary <- sql_summary %>% filter(!is.na(lat) & !is.na(lon))


# ------------------------------------------------------------------------------
# IPTU
# ------------------------------------------------------------------------------

# Base de dados do IPTU - contém dados de logradouro e número da rua
iptu <- list.files(pasta_iptu, pattern = '.csv$', full.names = TRUE)[1]
iptu <- read_delim(iptu, delim = ';', col_types = cols(.default = "c"))
iptu <- iptu %>% clean_names()

# sql vai ser a chave de conexão entre as bases
iptu <- iptu %>% mutate(sql = str_sub(numero_do_contribuinte, 1, 10), .before = 1)

# Simplificar dataframe de IPTU
iptu <- iptu %>% select(sql,
                        n_contrib = numero_do_contribuinte,
                        n_cond    = numero_do_condominio,
                        codlog    = codlog_do_imovel,
                        logradouro = nome_de_logradouro_do_imovel,
                        numero    = numero_do_imovel,
                        testada_m = testada_para_calculo,
                        esquinas  = quantidade_de_esquinas_frentes,
                        andares   = quantidade_de_pavimentos,
                        # tipo_uso  = tipo_de_uso_do_imovel,
                        # complem   = complemento_do_imovel,
                        # bairro    = bairro_do_imovel,
                        cep       = cep_do_imovel,
                        # tipo_const = tipo_de_padrao_da_construcao,
                        # tipo_terreno = tipo_de_terreno
)

# iptu %>% filter(sql == '0250500047')
# sql_summary %>% group_by(sql) %>% tally() %>% filter(n > 1)


# ------------------------------------------------------------------------------
# Junção das bases
# ------------------------------------------------------------------------------

sql_summary


gps_out <- sql_summary %>% left_join(iptu, by = 'sql')
gps_out <- gps_out %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)
# gps_out %>% filter(group_id %in% c(1230, 1279))

out_gpkg <- sprintf('%s/resumo_pontos_por_lote.gpkg', pasta_analises)
st_write(gps_out, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)

# sql_summary <- gps_rev %>%
#   mutate(lon = st_coordinates(geom)[, 1],
#          lat = st_coordinates(geom)[, 2]) %>%
#   st_drop_geometry() %>%
#   left_join(vc, by = 'imagepath') %>%
#   filter(is.na(descartar)) %>%
#   group_by(sql, video_path) %>%
#   summarize(
#     point_id = first(point_id),
#     campo = first(campo),
#     imagepath = first(imagepath),
#     video_path = first(video_path),
#     start_time = first(start_time),
#     crosswalk = sum(crosswalk, na.rm = TRUE),
#     curbramp = sum(curbramp, na.rm = TRUE),
#     surfaceproblem = sum(surfaceproblem, na.rm = TRUE),
#     obstacle = sum(obstacle, na.rm = TRUE),
#     crosswalk_prop = round(sum(crosswalk, na.rm = TRUE) / n(), 2),
#     curbramp_prop = round(sum(curbramp, na.rm = TRUE) / n(), 2),
#     surfaceproblem_prop = round(sum(surfaceproblem, na.rm = TRUE) / n(), 2),
#     obstacle_prop = round(sum(obstacle, na.rm = TRUE) / n(), 2),
#     row_count = n(),  # Count the number of rows in each interval
#     lon = mean(lon, na.rm = TRUE),
#     lat = mean(lat, na.rm = TRUE),
#   ) %>%
#   ungroup()



# # Get the first row and every 10th row from each group
# gps_filtered <- gps_rev %>%
#   filter(is.na(descartar)) %>%
#   group_by(sql) %>%
#   mutate(row_num = row_number(), .after = 1) %>%  # Create row index within each group
#   filter(row_num == 1 | (row_num - 1) %% 10 == 0) %>%  # Keep first row and every 10th row
#   ungroup()  # Ungroup if no further grouping is required
#
#
# gps_grouped <- gps_rev %>%
#   st_drop_geometry() %>%
#   filter(is.na(descartar)) %>%
#   left_join(vc, by = 'imagepath') %>%
#   group_by(sql) %>%
#   mutate(row_num = row_number()) %>%  # Create row index within each group
#   mutate(
#     interval = (row_num - 1) %/% 10  # Create intervals (0-9, 10-19, 20-29, ...)
#   ) %>%
#   group_by(sql, interval) %>%
#   summarize(
#     imagepath = first(imagepath),
#     crosswalk = sum(crosswalk),
#     curbramp = sum(curbramp),
#     surfaceproblem = sum(surfaceproblem),
#     obstacle = sum(obstacle),
#     row_count = n()  # Count the number of rows in each interval
#   ) %>%
#   ungroup() %>%
#   select(-c(sql, interval))
#
# head(gps_grouped, 20)
#
# gps_grouped <- gps_grouped %>%
#   mutate(
#     crosswalk = round(crosswalk / row_count, 2),
#     curbramp = round(curbramp / row_count, 2),
#     surfaceproblem = round(surfaceproblem / row_count, 2),
#     obstacle = round(obstacle / row_count, 2)
#   )
#
#
# gps_out <- gps_filtered %>% left_join(gps_grouped, by = 'imagepath')
#
# out_gpkg <- sprintf('%s/merged_pontos_gps_revistos_sql_lotes_filtrados.gpkg', pasta_proc)
# st_write(gps_out, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)
