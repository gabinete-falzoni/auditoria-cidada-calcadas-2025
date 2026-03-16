# Juntar todas as informações das coletas e análises em um shapefile único:
# - Shape de pontos GPS revisados + associações aos lotes feita no QGIS
# - Associação ao PEC Calçadas feita no QGIS
# - Associação aos lotes públicos feitas por SQL
# - Análises Claros feitas por fotos (id é nome da foto)
# - Diferenciação de trechos usados como travessias feitas no R (scripts anteriores)

library('tidyverse')
library('tidylog')
library('sf')
# library('leaflet')

# Estrutura de pastas
# pasta_base <- '/media/livre/Expansion'
pasta_base <- '/mnt/fern/Dados'
pasta_audi <- sprintf('%s/projetos/2025_Auditoria_Calcadas', pasta_base)
pasta_proc <- sprintf('%s/01_dados_processados', pasta_audi)
pasta_analises <- sprintf('%s/04_analises', pasta_proc)
pasta_fotos <- sprintf('%s/fotos_resumo_por_lote', pasta_analises)
pasta_resultados <- sprintf('%s/05_resultados', pasta_proc)


# ------------------------------------------------------------------------------
# Reassociar gps_revisados aos lotes, removendo erros presentes no Geosampa
# ------------------------------------------------------------------------------

# Shape de pontos GPS revisados, com associação aos lotes da camada Geosampa >
# Cadastro > Lotes feita no QGIS
gps_revisados <- sprintf('%s/merged_pontos_gps_revisados_sql_lotes_pec.gpkg', pasta_analises)
gps_revisados <- read_sf(gps_revisados)
# Pontos marcados para descarte são esperas ao início, fim ou travessias
gps_revisados <- gps_revisados %>% mutate(descartar = ifelse(is.na(descartar), FALSE, descartar))
# gps_revisados %>% st_drop_geometry() %>% select(sql) %>% sample_n(20)

# Remover pontos duplicados
# gps_revisados <- gps_revisados %>% distinct(campo, video_path, imagepath, .keep_all = TRUE)
gps_revisados <- gps_revisados %>% distinct(geom, .keep_all = TRUE)
# gps_revisados %>%
#   filter(campo == 'D1P3' & video_path == '02_videos_low_res/20251109-082013291.mp4' & between(point_id, 705, 709)) %>%
#   select(point_id, descartar)

# Lotes com numeração e dados de cadastro vindos do IPTU
lotes <- sprintf('%s/00_shapes_base/lotes_perimetro_auditoria_com_numero.gpkg', pasta_proc)
lotes <- read_sf(lotes) %>% st_drop_geometry() %>% relocate(sql, .before = 1)
lotes <- lotes %>% filter(sql != '0180820016')

# this <- gps_revisados %>% st_drop_geometry() %>% group_by(sql) %>% tally()
# that <- gps_revisados %>% st_drop_geometry() %>% select(sql) %>% left_join(subset(lotes, select = c(sql)), by = 'sql') %>% group_by(sql) %>% tally()
# this %>% left_join(that, by = 'sql') %>% filter(n.x != n.y)
# lotes %>% filter(sql == '0250020002')

# Parte do Largo da Concórdia está sendo computado com um mesmo SQL de um lote
# da Rua João Teodoro
lotes_dup <- lotes %>% filter(sql == '0250020002')
lotes_dup_1 <- lotes_dup %>% head(1)
lotes_dup_2 <- lotes_dup %>% tail(1)

lotes <- lotes %>% distinct(sql, .keep_all = TRUE) %>% filter(sql != '0250020002') %>% rbind(lotes_dup_2)
rm(lotes_dup, lotes_dup_2)

gps_revisados <- gps_revisados %>% left_join(lotes, by = 'sql')


# O que fazer com o lote do Largo da Concórdia?
lotes_dup_1

# Pontos GPS que passam por este trecho vão ser reassociados na mão
largo_concordia <- gps_revisados %>%
  filter((str_detect(video_path, '20251109-072424340.mp4') & between(point_id, 186, 193)) |
           (str_detect(video_path, '20251109-072450402.mp4') & between(point_id, 158, 164)) |
           (str_detect(video_path, '20251109-072740010.mp4') & between(point_id, 7, 81)) |
           (str_detect(video_path, '20251109-072943708.mp4') & between(point_id, 40, 45)) |
           (str_detect(video_path, '20251109-073026637.mp4') & between(point_id, 1, 60)) |
           (str_detect(video_path, '20251109-092340580.mp4') & between(point_id, 5, 60)) |
           (str_detect(video_path, '20251110-093157636.mp4') & between(point_id, 255, 283))) %>%
  select(-names(lotes_dup_1)[names(lotes_dup_1) != "sql"])

# Este filtro vai descartar um ponto a mais do que o anterior, mas vamo que vamo
gps_revisados <- gps_revisados %>%
  filter(!(str_detect(video_path, '20251109-072424340.mp4') & between(point_id, 186, 193)) &
           !(str_detect(video_path, '20251109-072450402.mp4') & between(point_id, 158, 164)) &
           !(str_detect(video_path, '20251109-072740010.mp4') & between(point_id, 7, 81)) &
           !(str_detect(video_path, '20251109-072943708.mp4') & between(point_id, 40, 45)) &
           !(str_detect(video_path, '20251109-073026637.mp4') & between(point_id, 1, 60)) &
           !(str_detect(video_path, '20251109-092340580.mp4') & between(point_id, 5, 60)) &
           !(str_detect(video_path, '20251110-093157636.mp4') & between(point_id, 255, 283)))

largo_concordia <- largo_concordia %>% left_join(lotes_dup_1, by = 'sql')

gps_revisados <- bind_rows(gps_revisados, largo_concordia) %>% relocate(geom, .after = last_col())
gps_revisados <- gps_revisados %>% relocate(matches('^cc_'), .before = 'geom')
rm(largo_concordia, lotes_dup_1, lotes)


# ------------------------------------------------------------------------------
# Marcação de lotes públicos (PEC, SEFAZ, Cadastro > Lotes, Praças e Largos)
# ------------------------------------------------------------------------------

# Praças e Largos -> para o Brás, somente um dos SQL não está já na camada
# de Cadastro > Lotes como lote municipal
praca_largo <- '0030160070'

# Listagem de lotes públicos, conforme enviado pela SEFAZ: a classificação dos
# imóveis observa os códigos COB de acordo com a esfera administrativa: Municipal (20),
# Estadual (32) e Federal (42). No que tange às Sociedades de Economia Mista e
# Empresas Públicas, são utilizados os códigos 51 e 52.
lotes_publicos <- sprintf('%s/dados/Pedidos_LAI/LAI_SEFAZ/94779_ESIC 94779.CSV', pasta_base)
lotes_publicos <- read_delim(lotes_publicos, delim = ';', col_types = 'ccccc')
lotes_publicos <- lotes_publicos %>% mutate(across(where(is.character), str_trim),
                                            across(where(is.character), str_squish),
                                            # Substituir '' por NA
                                            across(where(is.character), ~ na_if(., "")))

# Adaptar SQL para merge com camada de lotes
lotes_publicos <-
  lotes_publicos %>%
  # filter(str_detect(LOGRADOURO, 'RANGEL PESTANA')) %>%
  # Deixar com 11 dígitos, adicionando 0 à frente e...
  mutate(sql = str_pad(SQL, width = 11, side = "left", pad = "0"), .before = 1) %>%
  # ... cortar para 10 dígitos
  mutate(sql = str_sub(sql, 1, 10))


# Manter somente coluna de tipo de proprietário
lotes_publicos <- lotes_publicos %>% select(sql, lote_tp_prop = TIPO_PROPRIETARIO)


lotes_publicos %>% group_by(lote_tp_prop) %>% tally()
# TIPO_PROPRIETARIO                                     n
# <chr>                                             <int>
# 1 ESTADO OU AUTARQUIAS ESTADUAIS                     3678
# 2 P.M.S.P. OU AUTARQUIAS MUNICIPAIS                  4576
# 3 SOCIEDADES DE ECONOMIA MISTA OU EMPRESAS PÚBLICAS  3296
# 4 UNIÃO OU AUTARQUIAS FEDERAIS                       1253


# Juntar tudo no mesmo shape
gps_revisados <- gps_revisados %>% left_join(lotes_publicos, by = 'sql')
gps_revisados <- gps_revisados %>% relocate(lote_tp_prop, .after = 'n_cond')

# Fazer marcacao única de lote de alçada pública
gps_revisados <- gps_revisados %>%
  mutate(lote_alcada_publica = ifelse(sql == praca_largo | lo_tp_lote == 'M' | !is.na(lote_tp_prop) | !is.na(cc_pec), TRUE, FALSE),
         .after = 'lote_tp_prop')

rm(lotes_publicos, praca_largo)



# ------------------------------------------------------------------------------
# Inserir marcações IME para todos os pontos GPS, exceto os para descartar
# ------------------------------------------------------------------------------

ime <- sprintf('%s/predictions.csv', pasta_analises)
ime <- read_delim(ime, delim = ',', col_types = 'ciiii')
ime <- ime %>% mutate(image = basename(image))

gps_revisados <- gps_revisados %>%
  mutate(image = basename(imagepath)) %>%
  left_join(ime, by = 'image') %>%
  select(-image) %>%
  relocate(geom, .after = last_col())

rm(ime)


# ------------------------------------------------------------------------------
# Inserir marcações de quais pontos representam o lote (keyframes de lotes)
# ------------------------------------------------------------------------------

# Shape de pontos agrupados por lote com marcações do IME associadas via imagepath
# e associação ao PEC Calçadas feita no QGIS
gps_agrupados <- sprintf('%s/resumo_pontos_por_lote_com_marcacao_pec_calcadas.gpkg', pasta_analises)
gps_agrupados <- read_sf(gps_agrupados)

# Remover pontos duplicados
# gps_agrupados <- gps_agrupados %>% distinct(imagepath, .keep_all = TRUE)
gps_agrupados <- gps_agrupados %>% distinct(geom, .keep_all = TRUE)

# Remover associações aos lotes (faltam infos sobre lotes públicos) - ficam só
# dados relacionados às marcações do IME e do PEC Calçadas
gps_agrupados <-
  gps_agrupados %>%
  st_drop_geometry() %>%
  select(-any_of(setdiff(intersect(names(gps_revisados),
                                   names(gps_agrupados)),
                         "imagepath"))) %>%
  relocate(imagepath, .before = 1)

# Marcar que são keyframes dos lotes
gps_agrupados <- gps_agrupados %>% mutate(flag = TRUE, .after = 'imagepath')

# Adicionar marcação de keyframe nos nomes das colunas
gps_agrupados <- gps_agrupados %>% rename_with(~ paste0("kf_", .x), -imagepath)

# Adicionar dados dos keyframes ao dataframe de todos os pontos
gps_revisados <- gps_revisados %>% left_join(gps_agrupados, by = 'imagepath')
gps_revisados <- gps_revisados %>% relocate(geom, .after = last_col())
rm(gps_agrupados)


# ------------------------------------------------------------------------------
# Inserir análises Claros (referentes às imagens de keyframes de lotes)
# ------------------------------------------------------------------------------

# Análises Claros - Buracos
buraco_1 <- list.files(sprintf('%s/Buraco_01', pasta_fotos), recursive = FALSE)
buraco_2 <- list.files(sprintf('%s/Buraco_02', pasta_fotos), recursive = FALSE)
# Análises Claros - Superfície
superficie_1 <- list.files(sprintf('%s/Superficie_01', pasta_fotos), recursive = FALSE)
superficie_2 <- list.files(sprintf('%s/Superficie_02', pasta_fotos), recursive = FALSE)

# '2025-11-10-09-15-47-799_00331_ms.jpg' %in% superficie_2

gps_revisados <- gps_revisados %>%
  mutate(img = basename(imagepath),
         quadra_buraco_1 = ifelse(img %in% buraco_1, TRUE, FALSE),
         quadra_buraco_2 = ifelse(img %in% buraco_2, TRUE, FALSE),
         quadra_superf_1 = ifelse(img %in% superficie_1, TRUE, FALSE),
         quadra_superf_2 = ifelse(img %in% superficie_2, TRUE, FALSE),
         quadra_flag_buraco = ifelse(quadra_buraco_1 | quadra_buraco_2, TRUE, FALSE),
         quadra_flag_superf = ifelse(quadra_superf_1 | quadra_superf_2, TRUE, FALSE)
         ) %>%
  mutate(quadra_flag_bur_sup = ifelse(quadra_flag_buraco | quadra_flag_superf, TRUE, FALSE)) %>%
  select(-img) %>%
  relocate(geom, .after = last_col())

rm(buraco_1, buraco_2, superficie_1, superficie_2)


# ------------------------------------------------------------------------------
# Inserir marcações de travessias
# ------------------------------------------------------------------------------

# Shape de travessias é o com os grupos de travessias revisados (group_id_rev)
trav <- sprintf('%s/travessias_resultados.gpkg', pasta_analises)
trav <- read_sf(trav)

# Remover pontos duplicados
# trav <- trav %>% distinct(imagepath, .keep_all = TRUE)
trav <- trav %>% distinct(geom, .keep_all = TRUE)

# Remover colunas que já existem no dataframe principal
trav <- trav %>%
  st_drop_geometry() %>%
  # Grupo 974 não é uma travessia, remover
  filter(group_id_rev != 974) %>%
  mutate(trav_flag = TRUE) %>%
  select(imagepath,
         trav_flag,
         trav_group_id = group_id_rev,
         trav_rampa = rampa,
         trav_horizontal = horizontal,
         trav_inadequada = inadequada,
         trav_repintar_horiz = repintar_horizontal,
         trav_reparar_pavim = reparar_pavimento,
         trav_reparar_rampa = reparar_rampa,
         # trav_avaliado = avaliado, # todos os demais grupos foram avaliados
         trav_categoria = travessia_ok
  )

# Valores NA são FALSE - substituir
trav <- trav %>% mutate(across(where(is.logical), ~replace_na(.x, FALSE)))


# Revisar padronização análises Claros das travessias - antes, estávamos considerando
# que se houvesse rampa, a categoria poderia ser parcialmente adequada. Agora, o
# mínimo denominador comum é a faixa de travessia - é a partir da existência dela
# que devem ser analisadas as rampas
trav <- trav %>%
  mutate(trav_categoria = case_when(trav_horizontal == TRUE & trav_rampa == TRUE ~ '1 - adequada',
                                    trav_horizontal == TRUE & (trav_rampa == FALSE | is.na(trav_rampa)) ~ '2 - parcial',
                                    # (trav_horizontal == FALSE | is.na(trav_horizontal)) & trav_rampa == TRUE ~ '2 - parcial',
                                    TRUE ~ '3 - inadequada'))

# Consertar trav_group_id == 233, que deveria ser TRUE para trav_rampa,
# trav_horizontal, trav_inadequada e trav_repintar_horiz
# trav %>% filter(trav_group_id == 233) %>%
trav <- trav %>%
  mutate(trav_rampa = ifelse(trav_group_id == 233, TRUE, trav_rampa),
         trav_horizontal = ifelse(trav_group_id == 233, TRUE, trav_horizontal),
         trav_inadequada = ifelse(trav_group_id == 233, TRUE, trav_inadequada),
         trav_repintar_horiz = ifelse(trav_group_id == 233, TRUE, trav_repintar_horiz),
         trav_categoria = ifelse(trav_group_id == 233, '1 - adequada', trav_categoria)
         )


# Juntar ao dataframe principal
gps_revisados <- gps_revisados %>% left_join(trav, by = 'imagepath') %>% relocate(geom, .after = last_col())
rm(trav)


# ------------------------------------------------------------------------------
# Exportar resultados - pontos
# ------------------------------------------------------------------------------

gps_revisados <- gps_revisados %>% arrange(campo, imagepath)

out_gpkg <- sprintf('%s/auditoria_calcadas_bras.gpkg', pasta_resultados)
st_write(gps_revisados, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)


# ------------------------------------------------------------------------------
# Exportar resultados - linhas
# ------------------------------------------------------------------------------

# Linhas de meio de quadra
gps_revisados_linhas <-
  gps_revisados %>%
  filter(descartar == FALSE) %>%
  arrange(campo, imagepath) %>%
  # select(campo, point_id, trav_group_id, sql) %>%
  mutate(
    sql = ifelse(is.na(sql), NA_character_, as.character(sql)),
    segment_id = cumsum(
      dplyr::coalesce(sql != lag(sql), TRUE)
    )
  ) %>%
  group_by(segment_id, sql) %>%
  summarise(
    campo = first(campo),
    imagepath = imagepath[ceiling(n() / 2)],
    video_path = first(video_path),
    start_time = first(start_time),
    video_duration = n(),
    segment_size = n(),
    surfaceproblem = sum(surfaceproblem),
    surface_problem_prop = surfaceproblem / segment_size,
    n_contrib = first(n_contrib),
    n_cond = first(n_cond),
    codlog = first(codlog),
    logradouro = first(logradouro),
    numero = first(numero),
    testada_m = first(testada_m),
    esquinas = first(esquinas),
    andares = first(andares),
    cep = first(cep),
    lo_tp_lote = first(lo_tp_lote),
    lote_tp_prop = first(lote_tp_prop),
    cc_pec = first(cc_pec),
    cc_situac = first(cc_situac),
    # Pegar primeiro valor não NA do grupo
    quadra_superf_1 = any(quadra_superf_1 %in% TRUE, na.rm = TRUE),
    quadra_superf_2 = any(quadra_superf_2 %in% TRUE, na.rm = TRUE),
    quadra_flag_buraco = any(quadra_flag_buraco %in% TRUE, na.rm = TRUE),
    quadra_flag_superf = any(quadra_flag_superf %in% TRUE, na.rm = TRUE),
    quadra_flag_bur_sup = any(quadra_flag_bur_sup %in% TRUE, na.rm = TRUE),
    geom = st_cast(st_combine(geom), "LINESTRING"),
    .groups = "drop"
  ) %>%
  filter(segment_size >= 2)


# O agrupamento está introduzindo um erro em 2025-11-10-09-15-47-799_00331_ms.jpg
# gps_revisados_linhas %>% filter(quadra_superf_2 & quadra_flag_buraco) %>% select(segment_size, matches('^quadra_'))
# gps_revisados_linhas %>% filter(quadra_superf_2 & quadra_flag_buraco) %>% st_drop_geometry() %>% select(imagepath) %>% pull()
gps_revisados_linhas <- gps_revisados_linhas %>%
  mutate(quadra_flag_buraco = ifelse(str_detect(imagepath, '20251110-091547799/2025-11-10-09-15-47-799_00331_ms.jpg'), FALSE, quadra_flag_buraco))

gps_revisados_linhas <- gps_revisados_linhas %>% st_transform(31983) %>% mutate(length_m = as.numeric(st_length(geom)), .before = 'geom')

out_gpkg_2 <- sprintf('%s/auditoria_calcadas_bras_linhas_quadras.gpkg', pasta_resultados)
st_write(gps_revisados_linhas, out_gpkg_2, driver = 'GPKG', append = FALSE, delete_layer = TRUE)


# Linhas travessia
gps_revisados_linhas_trav <-
  gps_revisados %>%
  filter(descartar == FALSE & trav_flag == TRUE) %>%
  arrange(campo, imagepath) %>%
  group_by(trav_group_id) %>%
  summarise(
    trav_horizontal = first(trav_horizontal),
    trav_categoria = first(trav_categoria),
    imagepath = imagepath[ceiling(n() / 2)],
    video_path = first(video_path),
    start_time = first(start_time),
    video_duration = n(),
    segment_size = n(),
    geom = st_cast(st_combine(geom), "LINESTRING"),
    .groups = "drop"
  ) %>%
  filter(segment_size >= 2)

gps_revisados_linhas_trav <- gps_revisados_linhas_trav %>% st_transform(31983) %>% mutate(length_m = as.numeric(st_length(geom)), .before = 'geom')

out_gpkg_3 <- sprintf('%s/auditoria_calcadas_bras_linhas_travessias.gpkg', pasta_resultados)
st_write(gps_revisados_linhas_trav, out_gpkg_3, driver = 'GPKG', append = FALSE, delete_layer = TRUE)
