library('tidyverse')
library('tidylog')
library('sf')
library('mapview')


# Estrutura de pastas
# pasta_base  <- '/mnt/fern/Dados/2025_Auditoria_Calcadas/pinheiros'
pasta_base  <- '/media/livre/Expansion/projetos/2025_Auditoria_Calcadas'
pasta_proc  <- sprintf('%s/01_dados_processados', pasta_base)

gps_rev <- sprintf('%s/merged_pontos_gps_revistos_sql_lotes.gpkg', pasta_proc)
gps_rev <- read_sf(gps_rev)
# Este sql está duplicado na base, sobreponto a uma divisão de lotes no mesmo local
# gps_rev <- gps_rev %>% filter(sql != '0180820016')

# Análises Visão Computacional
vc <- sprintf('%s/predictions.csv', pasta_proc)
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
    na_group = is_na_group | next_is_na
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

# Precisamos marcar todos os frames com sql anteriores aos blocos de NA - por
# enquanto, só o último deles (o logo anterior ao grupo de NAs) está marcado
grupos_esquinas <- esquinas %>% filter(na_group) %>% select(group_id) %>% distinct()

# Marcar os grupos de esquinas no dataframe principal
esquinas <- esquinas %>%
  mutate(na_group = ifelse(group_id %in% grupos_esquinas$group_id, TRUE, na_group))

esquinas %>% filter(group_id %in% c(11, 12, 13)) %>%
  select(group_id, point_id, sql, na_group, prev_is_na, next_is_na, video_duration) %>%
  slice(3:22)

# ------------------------------------------------------------------------------
# Com 5 frames de vídeo / pontos GPS antes
# ------------------------------------------------------------------------------

# # Separar: grupos de NA e SQLs próximos a esquinas
# so_nas  <- esquinas %>% filter(is.na(sql))
# sql_esq <- esquinas %>% filter(!is.na(sql) & na_group)
#
# # Dos grupos de SQLs próximos a esquinas, vamos manter só os últimos 5 frames
# sql_esq <- sql_esq %>%
#   group_by(group_id) %>%
#   slice_tail(n = 5) %>%
#   mutate(video_duration = row_number(desc(point_id))) %>%
#   ungroup()
#
# # Juntar novamente os dataframes
# so_nas <- so_nas %>% select(colnames(sql_esq))
# esquinas_grouped <- rbind(so_nas, sql_esq)
#
# # Duração dos vídeos nos frames que contêm SQL vão ser a duração do grupo de NAs
# # mais 5 frames (ou menos, se já for menos), para poder ver o vídeo todo desde
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
#
# esquinas_grouped <- esquinas_grouped %>% filter(!is.na(lat) & !is.na(lon))
# gps_out <- esquinas_grouped %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)


# ------------------------------------------------------------------------------
# Com 5 frames de vídeo / pontos GPS antes
# ------------------------------------------------------------------------------

# Separar: grupos de NA e SQLs próximos (antes) a esquinas
so_nas  <- esquinas %>% filter(is.na(sql))
sql_esq_antes <- esquinas %>% filter(!is.na(sql) & na_group)

# sql_esq_antes %>% filter(group_id %in% c(11, 12, 13)) %>%
#   select(group_id, point_id, sql, na_group, prev_is_na, next_is_na, video_duration) %>%
#   head(20)

# Linhas antes e depois do grupo de NAs
linhas_ad <- 5

# Dos grupos de SQLs próximos a esquinas, vamos manter só os últimos 5 frames
sql_esq_antes <- sql_esq_antes %>%
  group_by(group_id) %>%
  slice_tail(n = 5) %>%
  mutate(video_duration = row_number(desc(point_id) + linhas_ad)) %>%
  # select(group_id, point_id, sql, na_group, prev_is_na, next_is_na, video_duration) %>%
  ungroup()

# Juntar novamente os dataframes
so_nas <- so_nas %>% select(colnames(sql_esq_antes))
esquinas_grouped <- rbind(so_nas, sql_esq_antes)

# Duração dos vídeos nos frames que contêm SQL vão ser a duração do grupo de NAs
# mais X frames (ou menos, se já for menos), para poder ver o vídeo todo desde
# a chegada à esquina
esquinas_grouped <-
  esquinas_grouped %>%
  # Atualizar a duração dos vídeos: vão ser os 1-5 frames iniciais + os frames
  # dos blocos de is.na(SQL) seguintes
  mutate(video_duration = ifelse(!is.na(sql),
                                 video_duration + next_vd_duration,
                                 video_duration)) %>%
  arrange(group_id, point_id) %>%
  select(point_id,
         group_id,
         # is_na_group,
         sql,
         campo,
         imagepath,
         video_path,
         lon,
         lat,
         start_time,
         video_duration,
         # next_vd_duration,
         crosswalk,
         curbramp,
         surfaceproblem,
         obstacle)

# ------------------------------------------------------------------------------
# Adicionar 5 frames de vídeo / pontos GPS depois
# ------------------------------------------------------------------------------

# Precisamos marcar todos os frames com sql anteriores aos blocos de NA - por
# enquanto, só o último deles (o logo anterior ao grupo de NAs) está marcado
grupos_esquinas_depois <- esquinas %>% filter(!is.na(sql) & prev_is_na) %>% select(group_id) %>% distinct()

# Marcar os grupos de esquinas no dataframe principal
esquinas <- esquinas %>%
  mutate(prev_is_na = ifelse(group_id %in% grupos_esquinas_depois$group_id, TRUE, prev_is_na))

# Isolar somente grupos posteriores a esquinas
sql_esq_depois <- esquinas %>% filter(!is.na(sql) & prev_is_na)

sql_esq_depois %>% #filter(group_id %in% c(11, 12, 13)) %>%
  filter(group_id %in% c(17, 19)) %>%
  select(group_id, point_id, sql, na_group, prev_is_na, next_is_na, video_duration)

# Dos grupos de SQLs próximos a esquinas, vamos manter só os últimos 5 frames
sql_esq_depois <- sql_esq_depois %>%
  group_by(group_id) %>%
  slice_head(n = 5) %>%
  mutate(video_duration = as.numeric(NA)) %>%
  # select(group_id, point_id, sql, na_group, prev_is_na, next_is_na, video_duration) %>% head(20)
  ungroup()

# Juntar novamente os dataframes
sql_esq_depois <- sql_esq_depois %>% select(colnames(esquinas_grouped))
esquinas_grouped <- rbind(esquinas_grouped, sql_esq_depois) %>% arrange(group_id, point_id)

# esquinas_grouped %>% filter(group_id %in% c(11, 12, 13)) %>%
#   select(group_id, point_id, sql, video_duration) %>%
#   fill(video_duration, .direction = "down")

# Valores de duração dos vídeos após as esquinas seguirão os valores das esquinas
esquinas_grouped <- esquinas_grouped %>% fill(video_duration, .direction = "down")

esquinas_grouped %>% select(group_id) %>% distinct()



# esquinas_grouped <- esquinas_grouped %>% filter(!is.na(lat) & !is.na(lon))
# gps_out <- esquinas_grouped %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)
#
# out_gpkg <- sprintf('%s/esquinas3.gpkg', pasta_proc)
# st_write(gps_out, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)

esquinas_grouped_new %>% select(1, 2, 11:ncol(.)) %>%
  head(20)

# Marcar novos grupos, tendo como referência as sequências de linhas NA
esquinas_grouped_new <- esquinas_grouped %>%
  group_by(group_id) %>%
  # Grupos somente com NA vão virar referência
  mutate(is_na_only_group = all(is.na(sql))) %>%
  ungroup() %>%
  # Marcar grupos contíguos aos de referência
  mutate(grp_index = match(group_id, unique(group_id))) %>%
  # Identify reference (NA-only) group indices
  mutate(new_group_id = if_else(is_na_only_group, grp_index, NA_integer_)) %>%
  fill(new_group_id, .direction = "downup") %>%
  select(-grp_index)


df_expanded <- esquinas_grouped %>%

  # 1. Identify NA-only groups
  group_by(group_id) %>%
  mutate(is_na_only_group = all(is.na(sql))) %>%
  ungroup() %>%

  # 2. Create a table of group order
  mutate(grp_order = match(group_id, unique(group_id))) %>%

  # 3. Get mapping for NA-only groups: prev + current + next
  {
    group_info <- .

    na_only <- group_info %>%
      distinct(group_id, grp_order, is_na_only_group) %>%
      filter(is_na_only_group)

    # create mapping:
    # group_id (previous), group_id (self), group_id (next)
    map <- bind_rows(
      na_only %>% mutate(target = group_id),                                # self
      na_only %>% mutate(group_id = lag(group_id), target = group_id),      # previous
      na_only %>% mutate(group_id = lead(group_id), target = group_id)       # next
    ) %>%
      filter(!is.na(group_id)) %>%                          # remove missing prev/next
      select(group_id, new_group_id = target) %>%
      distinct()

    # 4. Join mapping back to full df
    left_join(group_info, map, by = "group_id")
  }




# Grupos com muitos NA são os que possuem mais do que X NA
grupos_na_muito_grandes <-
  # esquinas_grouped_new %>%
  df_expanded %>%
  filter(is_na_only_group) %>%
  group_by(new_group_id) %>%
  tally() %>%
  filter(n > 50)

# Remover esses grupos
# esquinas_grouped_new <-
#   esquinas_grouped_new %>%
df_expanded <-
  df_expanded %>%
  filter(!new_group_id %in% grupos_na_muito_grandes$new_group_id) %>%
  select(-is_na_only_group)

# Calcular marcações de visão computacional
# esquinas_grouped_new <-
#   esquinas_grouped_new %>%
df_expanded <-
  df_expanded %>%
  group_by(new_group_id) %>%
  mutate(
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
  select(1, 2, 11:ncol(.)) %>%
  head(20)

esquinas_grouped_new %>% select(1, 2, 11:ncol(.))

esquinas_grouped_new <- esquinas_grouped_new %>% filter(!is.na(lat) & !is.na(lon))
gps_out <- esquinas_grouped_new %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)

out_gpkg <- sprintf('%s/esquinas2.gpkg', pasta_proc)
st_write(gps_out, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)




















# ------------------------------------------------------------------------------
# Com 5 frames de vídeo / pontos GPS antes e depois
# ------------------------------------------------------------------------------

# Separar: grupos de NA e SQLs próximos a esquinas
so_nas  <- esquinas %>% filter(is.na(sql))
sql_esq_antes  <- esquinas %>% filter(!is.na(sql) & next_is_na)
sql_esq_depois <- esquinas %>% filter(prev_is_na)

so_nas %>%
# sql_esq_antes %>%
  filter(group_id %in% c(11, 12, 13)) %>%
  select(group_id, point_id, sql, na_group, prev_is_na, next_is_na, video_duration) %>%
  head(20)

# Linhas antes e depois do grupo de NAs
linhas_ad <- 5

# Dos grupos de SQLs próximos a esquinas, vamos manter os primeiros e últimos 5 frames
sql_esq_antes <- sql_esq_antes %>%
  select(point_id, group_id, sql, na_group, prev_is_na, next_is_na, video_duration) %>%
  group_by(group_id) %>%
  slice_tail(n = 5) %>%
  # Criar ordem 1 a 5 para somar na duração dos frames de vídeo posteriormente
  mutate(video_duration = row_number(desc(point_id)) + linhas_ad) %>%
  ungroup()

# Juntar novamente os dataframes
# so_nas <- so_nas %>% select(colnames(sql_esq_antes))
esquinas_grouped <- rbind(so_nas, sql_esq_antes, sql_esq_depois)

# Duração dos vídeos nos frames que contêm SQL vão ser a duração do grupo de NAs
# mais 5 frames (ou menos, se já for menos), para poder ver o vídeo todo desde
# a chegada à esquina
esquinas_grouped <-
  esquinas_grouped %>%
  select(group_id, point_id, sql, na_group, prev_is_na, next_is_na, video_duration, next_vd_duration) %>%
  # Atualizar a duração dos vídeos: vão ser os 1-5 frames iniciais + os frames
  # dos blocos de is.na(SQL) seguintes
  mutate(video_duration = ifelse(!is.na(sql),
                                 video_duration + next_vd_duration,
                                 video_duration)) %>%
  arrange(group_id, point_id) %>%
  filter(between(group_id, 15, 17)) %>% head(20)
  select(point_id,
         group_id,
         # is_na_group,
         sql,
         campo,
         imagepath,
         video_path,
         lon,
         lat,
         start_time,
         video_duration,
         # next_vd_duration,
         crosswalk,
         curbramp,
         surfaceproblem,
         obstacle)


esquinas_grouped <- esquinas_grouped %>% filter(!is.na(lat) & !is.na(lon))
gps_out <- esquinas_grouped %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)

out_gpkg <- sprintf('%s/esquinas.gpkg', pasta_proc)
st_write(gps_out, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)



esquinas_grouped %>% filter(group_id == 109)



























# Inserir marcações de grupos que contêm somente NA
marcacoes_NA <- gps_rev %>%
  st_drop_geometry() %>%
  select(sql) %>%
  mutate(n = row_number()) %>%
  # filter(is.na(sql))
  mutate(
    # Criar coluna temporária com NAs como string para serem comparáveis na criação de grupos
    sql_tmp = coalesce(sql, "__NA__"),
    # Quando valor na linha seguinte muda, is_change é TRUE
    is_change = sql_tmp != lag(sql_tmp),
    # Marcar a primeira linha de cada grupo
    is_change = replace_na(is_change, TRUE),
    # Numeração cumulativa dos grupos (cumsum)
    group_id  = cumsum(is_change),
    # Identificar se o grupo é composto somente de valores NA
    is_na_group = is.na(sql),
    # Marcar linhas antes e depois de ocorrências NA
    prev_is_na = lag(is_na_group, default = FALSE),
    next_is_na = lead(is_na_group, default = FALSE),
    # Marcação é NA ou adjacente a NA
    na_group = is_na_group | prev_is_na | next_is_na
  )

# Quais grupos são NA?
grupos_na <- marcacoes_NA %>% filter(na_group == TRUE) %>% select(group_id) %>% distinct()

gps_rev %>%
  mutate(
    # Criar coluna temporária com NAs como string para serem comparáveis na criação de grupos
    sql_tmp = coalesce(sql, "__NA__"),
    # Quando valor na linha seguinte muda, is_change é TRUE
    is_change = sql_tmp != lag(sql_tmp),
    # Marcar a primeira linha de cada grupo
    is_change = replace_na(is_change, TRUE),
    # Numeração cumulativa dos grupos (cumsum)
    group_id  = cumsum(is_change)
  ) %>%
  filter(group_id %in% grupos_na$group_id)




















# Step 2: Identify the adjacent runs to NA groups
df_with_marks <- marcacoes_NA %>%
  # slice(78:97) %>%
  group_by(group_id) %>%
  # Create markers for the groups adjacent to NA blocks (before/after)
  summarise(is_na_group, .groups = "drop") %>%
  mutate(
    prev_is_na = lag(is_na_group, default = FALSE),
    next_is_na = lead(is_na_group, default = FALSE),
    mark_run   = prev_is_na | next_is_na
  ) %>%
  select(group_id, mark_run, is_na_group)

left_join(marcacoes_NA, df_with_marks, by = "group_id") %>%
  slice(78:97)
  select(sql, group_id, is_na_group, mark_run)

marcacoes_NA %>% slice(78:97)
df_with_marks %>% slice(78:97)

gps_rev %>%
  st_drop_geometry() %>%
  select(sql) %>%
  mutate(n = row_number()) %>%
  # filter(is.na(sql))
  slice(78:97) %>%
  # Temporary variable that treats NA as a real label
  mutate(sql_tmp = if_else(is.na(sql), "__NA__", sql)) %>%

  # Identify when a new run begins
  mutate(
    is_new = sql_tmp != lag(sql_tmp, default = "__FIRST__"),
    run_id = cumsum(is_new)
  ) %>%

  # Derive run-level summary: is this run an NA run?
  group_by(run_id) %>%
  mutate(run_is_na = all(is.na(sql))) %>%
  ungroup() %>%

  # Identify neighboring runs of NA runs
  group_by(run_id) %>%
  summarise(run_is_na, .groups = "drop") %>%
  mutate(
    prev_is_na = lag(run_is_na, default = FALSE),
    next_is_na = lead(run_is_na, default = FALSE),
    mark_run   = prev_is_na | next_is_na
  ) %>%

  # Join back
  left_join(gps_rev %>%
              mutate(sql_tmp = if_else(is.na(sql), "__NA__", sql)) %>%
              mutate(is_new = sql_tmp != lag(sql_tmp, default = "__FIRST__"),
                     run_id = cumsum(is_new)),
            by = "run_id") %>%

  mutate(
    mark_before_after_null = if_else(mark_run, TRUE, FALSE)
  ) %>%
  select(sql, run_id, run_is_na, mark_before_after_null)




gps_rev %>%
  select(sql, descartar) %>%
  # # Puxar lat long para trabalhar como dataframe
  # mutate(lon = st_coordinates(geom)[, 1],
  #        lat = st_coordinates(geom)[, 2]) %>%
  st_drop_geometry() %>%
  # # Puxar lat long para trabalhar como dataframe
  # mutate(lon = st_coordinates(geom)[, 1],
  #        lat = st_coordinates(geom)[, 2]) %>%
  # st_drop_geometry() %>%
  # Unir dados vindos de visão computacional
  # left_join(vc, by = 'imagepath') %>%
  # filter(is.na(descartar)) %>%
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
  # slice(78:97) %>%
  group_by(group_id) %>%
  # Somar quantas linhas há em cada grupo
  # mutate(group_n = n(),
  #        # Marcar a linha do meio: valores ímpares têm centro exato; para valores
  #        # pares, pegamos o primeiro valor acima do meio
  #        mid_pos = ceiling(group_n / 2),
  #        row_pos = row_number()
  # ) %>%
  mutate(
    group_n = n(),
    # rows to keep when computing the middle
    keep = is.na(descartar) | descartar == FALSE,
    # group size ignoring descartar == TRUE
    group_n_eff = sum(keep),
    # middle position among kept rows
    mid_pos = ceiling(group_n_eff / 2),
    # row number but only among kept rows
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