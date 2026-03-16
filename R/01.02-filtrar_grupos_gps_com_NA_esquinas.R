#

library('tidyverse')
library('tidylog')
library('sf')
library('mapview')
library('janitor')


# Estrutura de pastas
# pasta_base  <- '/mnt/fern/Dados/2025_Auditoria_Calcadas/pinheiros'
pasta_base  <- '/media/livre/Expansion/projetos/2025_Auditoria_Calcadas'
pasta_proc  <- sprintf('%s/01_dados_processados', pasta_base)
pasta_analises <- sprintf('%s/04_analises', pasta_proc)

gps_rev <- sprintf('%s/merged_pontos_gps_revisados_sql_lotes.gpkg', pasta_analises)
gps_rev <- read_sf(gps_rev)
# Este sql está duplicado na base, sobreponto a uma divisão de lotes no mesmo local
# gps_rev <- gps_rev %>% filter(sql != '0180820016')

# Análises Visão Computacional
vc <- sprintf('%s/predictions.csv', pasta_analises)
vc <- read_delim(vc, delim = ',', col_types = "ciiii")
vc <- vc %>%
  mutate(imagepath = str_c('01_image_sequences', image, sep = '/'), .before = 1) %>%
  select(-image)

# Esquinas são os grupos de sequências de NAs + sql anterior (para poder ter
# imagem e vídeo antes do cruzamento)
esquinas <- gps_rev %>%
  # Puxar lat long para trabalhar como dataframe
  mutate(lon = st_coordinates(geom)[, 1],
         lat = st_coordinates(geom)[, 2]) %>%
  st_drop_geometry() %>%
  # Unir dados vindos de visão computacional
  left_join(vc, by = 'imagepath') %>%
  # select(sql, point_id, descartar) %>%
  # Reconhecer mudança de grupos - cada vez que um sql muda, o grupo muda
  mutate(
    # Criar coluna temporária com NAs como string para serem comparáveis na criação de grupos
    sql_tmp = coalesce(sql, "__NA__"),
    # Quando valor na linha seguinte muda, is_change é TRUE
    is_change = sql_tmp != lag(sql_tmp) | is.na(sql_tmp) != is.na(lag(sql_tmp)),
    # Marcar a primeira linha de cada grupo
    is_change = replace_na(is_change, TRUE),
    # Numeração cumulativa dos grupos (cumsum)
    group_id  = cumsum(is_change),
    # Criar grupo de NAs
    is_na_group = is.na(sql),
    # Marcar linhas antes de ocorrências NA
    next_is_na = lead(is_na_group, default = FALSE),
    prev_is_na = lag(is_na_group, default = FALSE),
    # Marcação é NA ou adjacente a NA
    # na_group = is_na_group | next_is_na
  ) %>%
  # select(-c(next_is_na)) %>%
  # slice(78:97) %>%
  group_by(group_id) %>%
  mutate(
    # Somar quantas linhas há em cada grupo
    group_n = n(),
    # Linhas a serem mantidas são as que 'descartar' não está marcada como 'sim'
    keep = is.na(descartar) | descartar == FALSE,
    # Tamanho do grupo, ignorando descartar == TRUE
    group_n_eff = sum(keep),
    # # Identificar linha do meio, dentre as linhas que serão mantidas
    # mid_pos = ceiling(group_n_eff / 2),
    # # Número de linha, somente entre linhas que serão mantidas
    # # row_pos = if_else(keep, cumsum(keep), NA_integer_)
    # row_pos = if_else(keep, cumsum(keep), 0)
  ) %>%
  ungroup() %>%
  # Remover pontos marcados para serem descartados
  filter(is.na(descartar)) %>%
  # Remover colunas temporárias
  select(-c(is_change, sql_tmp, group_n_eff, keep)) %>%
  rename(video_duration = group_n) %>%
  mutate(
    # Reduzir 1 segundo do início do vídeo, para garantir reprodução
    # start_time = ifelse(is.na(start_time) | start_time <= 1, 1, start_time - 1),
    # Duração de play do vídeo deve ser de pelo menos 5 segundos
    video_duration = ifelse(video_duration < 5, 5, video_duration),
    # Guardar valores de duração dos vídeos no grupo de NAs seguintes aos não NA
    next_vd_duration = lead(video_duration)
  )

esquinas <- esquinas %>%
  select(point_id,
         group_id,
         is_na_group,
         sql,
         campo,
         imagepath,
         video_path,
         lon,
         lat,
         start_time,
         video_duration,
         crosswalk,
         curbramp,
         surfaceproblem,
         obstacle
         )

# Quais são os grupos somente com NA?
# esquinas %>% filter(is_na_group) %>% group_by(group_id) %>% tally()
# Queremos somente os grupos com menos de 50 NA - corte arbitrário para
# desconsiderar trechos longos que não foram associados a lotes
grupos_na_validos <-
  esquinas %>%
  filter(is_na_group) %>%
  group_by(group_id) %>%
  tally() %>%
  ungroup() %>%
  filter(n < 50) %>%
  mutate(new_group_id = row_number(),
         grupos_antes  = group_id - 1,
         grupos_depois = group_id + 1)

# Grupos somente com NA
grupos_na <-
  esquinas %>%
  filter(group_id %in% grupos_na_validos$group_id) %>%
  left_join(subset(grupos_na_validos, select = c(group_id, new_group_id)), by = 'group_id')

grupos_antes <-
  esquinas %>%
  filter(group_id %in% grupos_na_validos$grupos_antes) %>%
  group_by(group_id) %>%
  slice_tail(n = 5) %>%
  ungroup() %>%
  left_join(subset(grupos_na_validos, select = c(grupos_antes, new_group_id)), by = c('group_id' = 'grupos_antes'))

grupos_depois <-
  esquinas %>%
  filter(group_id %in% grupos_na_validos$grupos_depois) %>%
  group_by(group_id) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  left_join(subset(grupos_na_validos, select = c(grupos_depois, new_group_id)), by = c('group_id' = 'grupos_depois'))

grupos_todos <-
  rbind(grupos_na, grupos_antes, grupos_depois) %>%
  arrange(group_id, point_id) %>%
  group_by(new_group_id) %>%
  mutate(new_group_size = n(),
         video_duration = n(),
         crosswalk = sum(crosswalk, na.rm = TRUE),
         curbramp = sum(curbramp, na.rm = TRUE),
         surfaceproblem = sum(surfaceproblem, na.rm = TRUE),
         obstacle = sum(obstacle, na.rm = TRUE),
         crosswalk_prop = crosswalk / n(),
         curbramp_prop = curbramp / n(),
         surfaceproblem_prop = surfaceproblem / n(),
         obstacle_prop = obstacle / n()) %>%
  ungroup()


grupos_todos <- grupos_todos %>% filter(!is.na(lat) & !is.na(lon))
gps_out <- grupos_todos %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)


# Esquinas Claros
claros <- sprintf('%s/esquinas_sem_dia_4.gpkg', pasta_analises)
claros <- read_sf(claros) %>% rename(geometry = geom) %>% clean_names()

# Adicionar novas colunas ao df original
new_cols <- names(claros)[20:26]
gps_out[new_cols] <- NA

# Remover linhas já avaliadas
gps_out <- gps_out %>%
  filter(!imagepath %in% claros$imagepath) %>%
  rbind(claros) %>%
  relocate(geometry, .after = last_col()) %>%
  arrange(group_id, point_id)

out_gpkg <- sprintf('%s/esquinas_com_dia_4.gpkg', pasta_analises)
st_write(gps_out, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)

#
#
# # ------------------------------------------------------------------------------
# # Com 5 frames de vídeo / pontos GPS antes
# # ------------------------------------------------------------------------------
#
# # # Separar: grupos de NA e SQLs próximos a esquinas
# # so_nas  <- esquinas %>% filter(is.na(sql))
# # sql_esq <- esquinas %>% filter(!is.na(sql) & na_group)
# #
# # # Dos grupos de SQLs próximos a esquinas, vamos manter só os últimos 5 frames
# # sql_esq <- sql_esq %>%
# #   group_by(group_id) %>%
# #   slice_tail(n = 5) %>%
# #   mutate(video_duration = row_number(desc(point_id))) %>%
# #   ungroup()
# #
# # # Juntar novamente os dataframes
# # so_nas <- so_nas %>% select(colnames(sql_esq))
# # esquinas_grouped <- rbind(so_nas, sql_esq)
# #
# # # Duração dos vídeos nos frames que contêm SQL vão ser a duração do grupo de NAs
# # # mais 5 frames (ou menos, se já for menos), para poder ver o vídeo todo desde
# # # a chegada à esquina
# # esquinas_grouped <-
# #   esquinas_grouped %>%
# #   # Atualizar a duração dos vídeos: vão ser os 1-5 frames iniciais + os frames
# #   # dos blocos de is.na(SQL) seguintes
# #   mutate(video_duration = ifelse(!is.na(sql),
# #                                  video_duration + next_vd_duration,
# #                                  video_duration)) %>%
# #   arrange(group_id, point_id) %>%
# #   select(point_id,
# #          group_id,
# #          # is_na_group,
# #          sql,
# #          campo,
# #          imagepath,
# #          video_path,
# #          lon,
# #          lat,
# #          start_time,
# #          video_duration,
# #          # next_vd_duration,
# #          crosswalk,
# #          curbramp,
# #          surfaceproblem,
# #          obstacle)
# #
# #
# # esquinas_grouped <- esquinas_grouped %>% filter(!is.na(lat) & !is.na(lon))
# # gps_out <- esquinas_grouped %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)
#
#
# # ------------------------------------------------------------------------------
# # Com 5 frames de vídeo / pontos GPS antes
# # ------------------------------------------------------------------------------
#
# # Separar: grupos de NA e SQLs próximos (antes) a esquinas
# so_nas  <- esquinas %>% filter(is.na(sql))
# sql_esq_antes <- esquinas %>% filter(!is.na(sql) & na_group)
#
# # sql_esq_antes %>% filter(group_id %in% c(11, 12, 13)) %>%
# #   select(group_id, point_id, sql, na_group, prev_is_na, next_is_na, video_duration) %>%
# #   head(20)
#
# # Linhas antes e depois do grupo de NAs
# linhas_ad <- 5
#
# # Dos grupos de SQLs próximos a esquinas, vamos manter só os últimos 5 frames
# sql_esq_antes <- sql_esq_antes %>%
#   group_by(group_id) %>%
#   slice_tail(n = 5) %>%
#   mutate(video_duration = row_number(desc(point_id) + linhas_ad)) %>%
#   # select(group_id, point_id, sql, na_group, prev_is_na, next_is_na, video_duration) %>%
#   ungroup()
#
# # Juntar novamente os dataframes
# so_nas <- so_nas %>% select(colnames(sql_esq_antes))
# esquinas_grouped <- rbind(so_nas, sql_esq_antes)
#
# # Duração dos vídeos nos frames que contêm SQL vão ser a duração do grupo de NAs
# # mais X frames (ou menos, se já for menos), para poder ver o vídeo todo desde
# # a chegada à esquina
# esquinas_grouped <-
#   esquinas_grouped %>%
#   # Atualizar a duração dos vídeos: vão ser os 1-5 frames iniciais + os frames
#   # dos blocos de is.na(SQL) seguintes
#   mutate(video_duration = ifelse(!is.na(sql),
#                                  video_duration + next_vd_duration,
#                                  video_duration)) %>%
#   arrange(group_id, point_id) %>%
#   select(point_id,
#          group_id,
#          # is_na_group,
#          sql,
#          campo,
#          imagepath,
#          video_path,
#          lon,
#          lat,
#          start_time,
#          video_duration,
#          # next_vd_duration,
#          crosswalk,
#          curbramp,
#          surfaceproblem,
#          obstacle)
#
# # ------------------------------------------------------------------------------
# # Adicionar 5 frames de vídeo / pontos GPS depois
# # ------------------------------------------------------------------------------
#
# # Precisamos marcar todos os frames com sql anteriores aos blocos de NA - por
# # enquanto, só o último deles (o logo anterior ao grupo de NAs) está marcado
# grupos_esquinas_depois <- esquinas %>% filter(!is.na(sql) & prev_is_na) %>% select(group_id) %>% distinct()
#
# # Marcar os grupos de esquinas no dataframe principal
# esquinas <- esquinas %>%
#   mutate(prev_is_na = ifelse(group_id %in% grupos_esquinas_depois$group_id, TRUE, prev_is_na))
#
# # Isolar somente grupos posteriores a esquinas
# sql_esq_depois <- esquinas %>% filter(!is.na(sql) & prev_is_na)
#
# sql_esq_depois %>% #filter(group_id %in% c(11, 12, 13)) %>%
#   filter(group_id %in% c(17, 19)) %>%
#   select(group_id, point_id, sql, na_group, prev_is_na, next_is_na, video_duration)
#
# # Dos grupos de SQLs próximos a esquinas, vamos manter só os últimos 5 frames
# sql_esq_depois <- sql_esq_depois %>%
#   group_by(group_id) %>%
#   slice_head(n = 5) %>%
#   mutate(video_duration = as.numeric(NA)) %>%
#   # select(group_id, point_id, sql, na_group, prev_is_na, next_is_na, video_duration) %>% head(20)
#   ungroup()
#
# # Juntar novamente os dataframes
# sql_esq_depois <- sql_esq_depois %>% select(colnames(esquinas_grouped))
# esquinas_grouped <- rbind(esquinas_grouped, sql_esq_depois) %>% arrange(group_id, point_id)
#
# # esquinas_grouped %>% filter(group_id %in% c(11, 12, 13)) %>%
# #   select(group_id, point_id, sql, video_duration) %>%
# #   fill(video_duration, .direction = "down")
#
# # Valores de duração dos vídeos após as esquinas seguirão os valores das esquinas
# esquinas_grouped <- esquinas_grouped %>% fill(video_duration, .direction = "down")
#
# esquinas_grouped %>% select(group_id) %>% distinct()
#
#
#
# # esquinas_grouped <- esquinas_grouped %>% filter(!is.na(lat) & !is.na(lon))
# # gps_out <- esquinas_grouped %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)
# #
# # out_gpkg <- sprintf('%s/esquinas3.gpkg', pasta_proc)
# # st_write(gps_out, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)
#
# esquinas_grouped_new %>% select(1, 2, 11:ncol(.)) %>%
#   head(20)
#
# # Marcar novos grupos, tendo como referência as sequências de linhas NA
# esquinas_grouped_new <- esquinas_grouped %>%
#   group_by(group_id) %>%
#   # Grupos somente com NA vão virar referência
#   mutate(is_na_only_group = all(is.na(sql))) %>%
#   ungroup() %>%
#   # Marcar grupos contíguos aos de referência
#   mutate(grp_index = match(group_id, unique(group_id))) %>%
#   # Identify reference (NA-only) group indices
#   mutate(new_group_id = if_else(is_na_only_group, grp_index, NA_integer_)) %>%
#   fill(new_group_id, .direction = "downup") %>%
#   select(-grp_index)
#
#
# df_expanded <- esquinas_grouped %>%
#
#   # 1. Identify NA-only groups
#   group_by(group_id) %>%
#   mutate(is_na_only_group = all(is.na(sql))) %>%
#   ungroup() %>%
#
#   # 2. Create a table of group order
#   mutate(grp_order = match(group_id, unique(group_id))) %>%
#
#   # 3. Get mapping for NA-only groups: prev + current + next
#   {
#     group_info <- .
#
#     na_only <- group_info %>%
#       distinct(group_id, grp_order, is_na_only_group) %>%
#       filter(is_na_only_group)
#
#     # create mapping:
#     # group_id (previous), group_id (self), group_id (next)
#     map <- bind_rows(
#       na_only %>% mutate(target = group_id),                                # self
#       na_only %>% mutate(group_id = lag(group_id), target = group_id),      # previous
#       na_only %>% mutate(group_id = lead(group_id), target = group_id)       # next
#     ) %>%
#       filter(!is.na(group_id)) %>%                          # remove missing prev/next
#       select(group_id, new_group_id = target) %>%
#       distinct()
#
#     # 4. Join mapping back to full df
#     left_join(group_info, map, by = "group_id")
#   }
#
#
#
#
# # Grupos com muitos NA são os que possuem mais do que X NA
# grupos_na_muito_grandes <-
#   # esquinas_grouped_new %>%
#   df_expanded %>%
#   filter(is_na_only_group) %>%
#   group_by(new_group_id) %>%
#   tally() %>%
#   filter(n > 50)
#
# # Remover esses grupos
# # esquinas_grouped_new <-
# #   esquinas_grouped_new %>%
# df_expanded <-
#   df_expanded %>%
#   filter(!new_group_id %in% grupos_na_muito_grandes$new_group_id) %>%
#   select(-is_na_only_group)
#
# # Calcular marcações de visão computacional
# # esquinas_grouped_new <-
# #   esquinas_grouped_new %>%
# df_expanded <-
#   df_expanded %>%
#   group_by(new_group_id) %>%
#   mutate(
#     # Calcular a quantidade de marcações por grupo para cada item
#     crosswalk = sum(crosswalk, na.rm = TRUE),
#     curbramp = sum(curbramp, na.rm = TRUE),
#     surfaceproblem = sum(surfaceproblem, na.rm = TRUE),
#     obstacle = sum(obstacle, na.rm = TRUE),
#     # Marcações proporcionais de cada item, por grupo
#     crosswalk_prop = round(sum(crosswalk, na.rm = TRUE) / n(), 2),
#     curbramp_prop = round(sum(curbramp, na.rm = TRUE) / n(), 2),
#     surfaceproblem_prop = round(sum(surfaceproblem, na.rm = TRUE) / n(), 2),
#     obstacle_prop = round(sum(obstacle, na.rm = TRUE) / n(), 2),
#   ) %>%
#   ungroup() %>%
#   select(1, 2, 11:ncol(.)) %>%
#   head(20)
#
# esquinas_grouped_new %>% select(1, 2, 11:ncol(.))
#
# esquinas_grouped_new <- esquinas_grouped_new %>% filter(!is.na(lat) & !is.na(lon))
# gps_out <- esquinas_grouped_new %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)
#
# out_gpkg <- sprintf('%s/esquinas2.gpkg', pasta_proc)
# st_write(gps_out, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)
#
#
#
#
