library('tidyverse')
library('tidylog')
library('sf')
# library('mapview')
library('leaflet')
library('janitor')


# Estrutura de pastas
# pasta_base <- '/media/livre/Expansion'
pasta_base <- '/mnt/fern/Dados'
pasta_audi <- sprintf('%s/projetos/2025_Auditoria_Calcadas', pasta_base)
pasta_proc <- sprintf('%s/01_dados_processados', pasta_audi)
pasta_analises <- sprintf('%s/04_analises', pasta_proc)
pasta_resultados <- sprintf('%s/05_resultados', pasta_proc)
dir.create(pasta_resultados, recursive = TRUE, showWarnings = FALSE)


ver_leaflet <- function(df) {
  leaflet(df) %>%
    addProviderTiles("CartoDB.Positron", group = "Positron") %>%
    addProviderTiles("OpenStreetMap", group = "OSM") %>%
    addCircleMarkers(radius = 3,
                     color = "#2b8cbe",
                     fillOpacity = 0.8,
                     stroke = FALSE) %>%
    addLayersControl(
      baseGroups = c("Positron", "OSM"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

# Análises IME
ime <- sprintf('%s/predictions.csv', pasta_analises)
ime <- read_delim(ime, delim = ',', col_types = 'ciiii') %>% mutate(image = basename(image))

# esq <- sprintf('%s/esquinas_sem_dia_4.gpkg', pasta_analises)
esq <- sprintf('%s/esquinas_com_dia_4_claros.gpkg', pasta_analises)
esq <- read_sf(esq) %>% clean_names()
# Dados das análises do IME estão agrupados por group_id - descartar neste
# momento para puxar de novo a classificação por imagem ao final do script
esq <- esq %>% select(-c(crosswalk, curbramp, surfaceproblem, obstacle, matches('_prop')))

# eval_cols <- c('rampa', 'horizontal', 'inadequada', 'repintar_horizontal', 'reparar_pavimento', 'reparar_rampa', 'manter')
eval_cols <- c('rampa', 'horizontal', 'inadequada', 'repintar_horizontal', 'reparar_pavimento', 'reparar_rampa')

# Guardar análises Claros para checagem
analises_claros <-
  esq %>%
  st_drop_geometry() %>%
  arrange(imagepath) %>%
  mutate(image = basename(imagepath)) %>%
  filter(!is.na(rampa)) %>%
  select(point_id, group_id, campo, image, all_of(eval_cols))


# ------------------------------------------------------------------------------
# Revisar números de group_id
# ------------------------------------------------------------------------------

# Atualizar valores do group_id, pois alguns grupos estão se repetindo no mapa,
# em locais diferentes (ex. 2600, 45, 1430)
# esq %>% filter(group_id == '2600') %>% select(group_id) %>% ver_leaflet()
esq <- esq %>%
  # filter(between(group_id, 2435, 2438)) %>%
  # filter(between(group_id, 691, 693)) %>%
  # head(20) %>%
  # select(point_id, group_id) %>%
  arrange(campo, group_id, point_id) %>%  # make sure it’s ordered
  # st_drop_geometry() %>%
  # ver_leaflet()
  mutate(
    diff_point = abs(point_id - lag(point_id)),
    diff_group = group_id - lag(group_id),
    diff_point = ifelse(is.na(diff_point), 1, diff_point),
    diff_group = ifelse(is.na(diff_group), 0, diff_group),
    # suspicious = diff_group == 1 & diff_point == 1
    suspicious = diff_point > 5
  ) %>%
  mutate(
    # assign block numbers to suspicious sequences
    group_id_rev = cumsum(replace_na(suspicious, FALSE)) + 1
  )

# esq %>% filter(group_id == '2600') %>% select(group_id, group_id_rev) %>% head(20)
# esq %>% filter(between(group_id, 2435, 2438)) %>% select(group_id, group_id_rev) %>% head(20)


# # Atualizar valores do group_id, pois alguns grupos estão se repetindo no mapa,
# # em locais diferentes (ex. 2600, 45, 1430)
# # esq %>% filter(group_id == '2600') %>% select(group_id) %>% ver_leaflet()
# esq <-
#   esq %>%
#   group_by(group_id) %>%
#   mutate(frame = as.numeric(str_extract(basename(imagepath), '\\d{5}')),
#          diff_group = group_id != lag(group_id),
#          diff_frame = frame - lag(frame),
#          # Detectar quebras e reiniciar grupo (segment) sempre que elas
#          # ocorrerem. O 0L e 1L garantem que o resultado será um integer
#          break_flag = coalesce(diff_group, FALSE) | coalesce(diff_frame > 1, FALSE),
#          segment = cumsum(break_flag),
#          # Gerar novo group_id: inserir letra conforme o valor de segment
#          group_id_rev = paste0(group_id, LETTERS[segment + 1]),
#          .after = 'group_id') %>%
#   ungroup() %>%
#   # filter(group_id == '2600') %>% head(20) %>% select(1:8) %>% st_drop_geometry()
#   select(-c(frame, diff_group, diff_frame, break_flag, segment))
#
# # esq %>% filter(group_id == '2600') %>% select(group_id, group_id_rev) %>% head(20)


# ------------------------------------------------------------------------------
# Consertar exceções que não deram certo
# ------------------------------------------------------------------------------

grupo_maximo <- max(esq$group_id_rev)

# esq %>%
#   filter(
#     (between(point_id, 74, 76) & group_id_rev == 221 & group_id == 2591) |
#       (between(point_id, 77, 87) & group_id_rev == 221)
#     ) %>%
#   ver_leaflet()

# esq <- esq %>%
#   mutate(group_id_rev = ifelse(
#     (between(point_id, 74, 76) & group_id_rev == 10 & group_id == 2591) | (between(point_id, 77, 87) & group_id_rev == 10),
#     grupo_maximo + 1,
#     group_id_rev))

# Ok - valor revisto está correto frente ao anterior
esq %>% filter(group_id_rev == 127) %>% ver_leaflet()

esq %>% filter(group_id_rev == 10) %>% ver_leaflet()
esq <- esq %>%
  mutate(
    group_id_rev = ifelse(between(point_id, 188, 220) & group_id_rev == 10, grupo_maximo + 1, group_id_rev),
    group_id_rev = ifelse(between(point_id, 222, 239) & group_id_rev == 10, grupo_maximo + 2, group_id_rev),
    )

esq %>% filter(group_id_rev == 603) %>% ver_leaflet()
esq <- esq %>%
  mutate(
    group_id_rev = ifelse(between(point_id, 23, 26) & between(group_id, 6400, 6401) & group_id_rev == 603, grupo_maximo + 3, group_id_rev),
    group_id_rev = ifelse(between(point_id, 27, 37) & between(group_id, 6401, 6402) & group_id_rev == 603, grupo_maximo + 4, group_id_rev),
  )

esq %>% filter(group_id_rev == 656) %>% ver_leaflet()
esq <- esq %>%
  mutate(
    group_id_rev = ifelse(between(point_id, 25, 40) & between(group_id, 6247, 6249) & group_id_rev == 656, grupo_maximo + 5, group_id_rev),
  )

esq %>% filter(group_id_rev == 497) %>% ver_leaflet()
esq <- esq %>%
  mutate(
    group_id_rev = ifelse(between(point_id, 81, 100) & between(group_id, 6394, 6396) & group_id_rev == 497, grupo_maximo + 6, group_id_rev),
  )



# ------------------------------------------------------------------------------
# Consertar pontos que estão ficando no mesmo grupo, mas são diferentes
# ------------------------------------------------------------------------------

esq %>% filter(group_id_rev == 404) %>% ver_leaflet()
esq <- esq %>%
  mutate(
    group_id_rev = ifelse(between(point_id, 123, 138) & between(group_id, 3981, 3983) & group_id_rev == 404, grupo_maximo + 7, group_id_rev),
  )

esq %>% filter(group_id_rev == 475) %>% ver_leaflet()
esq <- esq %>%
  mutate(
    group_id_rev = ifelse(between(point_id, 18, 38) & between(group_id, 5424, 5426) & group_id_rev == 475, grupo_maximo + 8, group_id_rev),
  )

esq %>% filter(group_id_rev == 492) %>% ver_leaflet()
esq <- esq %>%
  mutate(
    group_id_rev = ifelse(between(point_id, 20, 38) & between(group_id, 6177, 6179) & group_id_rev == 492, grupo_maximo + 9, group_id_rev),
  )

esq %>% filter(group_id_rev == 98) %>% ver_leaflet()
esq <- esq %>%
  mutate(
    group_id_rev = ifelse(between(point_id, 100, 113) & group_id_rev == 98, grupo_maximo + 10, group_id_rev),
  )

esq %>% filter(group_id_rev == 211) %>% ver_leaflet()
esq <- esq %>%
  mutate(
    group_id_rev = ifelse(between(point_id, 58, 67) & group_id_rev == 211, grupo_maximo + 11, group_id_rev),
  )


# ------------------------------------------------------------------------------
# Consertar pontos que ficaram em grupos diferentes, mas são do mesmo grupo
# ------------------------------------------------------------------------------

esq %>% filter(group_id_rev %in% c(313, 314)) %>% ver_leaflet()
esq <- esq %>%
  mutate(
    group_id_rev = ifelse(group_id_rev %in% c(6, 7), 6, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(84, 85), 84, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(94, 95), 94, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(96, 97), 96, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(116, 117), 116, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(130, 131), 130, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(133, 134), 133, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(169, 170), 169, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(176, 177), 176, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(178, 179), 178, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(219, 220), 219, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(250, 251), 250, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(298, 299), 298, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(303, 304), 303, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(313, 314), 313, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(328, 329), 329, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(334, 335), 304, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(336, 337), 336, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(342, 343), 342, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(358, 359), 358, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(362, 363), 362, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(364, 365), 364, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(387, 388), 387, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(398, 399), 398, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(404, 405), 404, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(407, 408), 407, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(414, 415), 414, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(417, 418), 417, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(421, 422), 421, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(423, 424), 423, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(443, 444), 443, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(445, 446), 445, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(449, 450), 449, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(472, 473), 472, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(482, 483), 482, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(493, 494), 493, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(495, 496), 495, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(518, 519), 518, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(532, 533), 532, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(546, 547), 546, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(564, 565), 564, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(607, 608), 607, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(616, 617), 616, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(633, 634), 633, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(639, 640), 639, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(657, 658), 657, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(660, 661), 660, group_id_rev),
    group_id_rev = ifelse(group_id_rev %in% c(666, 667), 666, group_id_rev),
  )
esq %>% filter(group_id_rev == 313)
esq %>% filter(group_id_rev == 314)




# ------------------------------------------------------------------------------
# Remover excesso de pontos em alguns grupos
# ------------------------------------------------------------------------------

# esq %>% filter(group_id_rev == 196) %>% ver_leaflet()
esq %>% filter(group_id_rev == 123 & between(point_id, 273, 288)) %>% ver_leaflet()
esq <- esq %>%
  mutate(manter = case_when(group_id_rev == 196 & between(point_id, 17, 49) ~ FALSE,
                            group_id_rev == 155 & between(point_id, 51, 78) ~ FALSE,
                            group_id_rev == 123 & between(point_id, 273, 288) ~ FALSE,
                            group_id_rev == 209 & between(point_id, 21, 27) ~ FALSE,
                            group_id_rev == 253 & between(point_id, 23, 50) ~ FALSE,
                            group_id_rev == 254 & between(point_id, 78, 145) ~ FALSE,
                            group_id_rev == 258 & between(point_id, 237, 249) ~ FALSE,
                            group_id_rev == 342 & between(point_id, 627, 628) ~ FALSE,
                            group_id_rev == 356 & (between(point_id, 144, 148) | between(point_id, 156, 164)) ~ FALSE,
                            group_id_rev == 525 & between(point_id, 187, 201) ~ FALSE,
                            group_id_rev == 545 & between(point_id, 221, 235) ~ FALSE,
                            group_id_rev == 664 & (between(point_id, 188, 192) | between(point_id, 207, 220)) ~ FALSE,
                            TRUE ~ TRUE),
         )




esq <- esq %>% filter(manter == TRUE)

# Marcar grupos que não fazem parte da análise para excluir lá na frente (após propagação)
esq %>% filter(group_id_rev == 205) %>% ver_leaflet()
excluir <- c(11, 37, 39, 43, 45,
             55, 57, 80, 81, 82, 88, 91,
             101, 105, 114, 122, 126, 135, 142, 149,
             154, 161, 172, 173, 183, 184, 185, 186, 192, 194, 195,
             205, 210, 215, 217, 227, 229, 236, 247,
             252, 269, 277, 292,
             300, 306, 307, 315, 321, 322, 324, 340, 342, 344,
             369, 373, 374, 376, 381, 390, 394,
             416, 420, 428, 432, 441,
             489,
             540, 541,
             559, 599,
             637, 650, 668
             )


# ------------------------------------------------------------------------------
# Propagar valores
# ------------------------------------------------------------------------------

# Propagar valores da primeira linha de cada grupo para as demais
esq <- esq %>%
  # st_drop_geometry() %>%
  # filter(group_id == 2600) %>%
  group_by(group_id_rev) %>%
  mutate(
    # Criar marcação de manter - se algum dos valores das colunas de avaliação
    # for TRUE, manter será TRUE
    manter = as.logical(NA),
    manter = if_any(eval_cols, ~ . == TRUE),
    # Replica o valor da primeira linha nas demais, dentro do mesmo grupo
    across(eval_cols, ~ if_else(is.na(.), first(.), .))
  ) %>%
  ungroup()
# select(1, 2, 10, 11, 16, 17, 20:ncol(.)) %>%
# select(7:12)




# ------------------------------------------------------------------------------
# Checagem - onde valores propagados não batem?
# ------------------------------------------------------------------------------

# Checar: análises propagadas da primeira linha para as demais e a revisão dos
# grupos funcionaram?
teste <-
  esq %>%
  st_drop_geometry() %>%
  arrange(imagepath) %>%
  mutate(image = basename(imagepath)) %>%
  filter(!is.na(rampa)) %>%
  select(point_id, group_id, campo, image, eval_cols) %>%
  filter(image %in% analises_claros$image)

# Todos os erros se referem aos dias 1 e 2, o que sugere
all.equal(teste, analises_claros)
# [1] "Component “horizontal”: 'is.NA' value mismatch: 27 in current 26 in target"
# [2] "Component “inadequada”: 'is.NA' value mismatch: 190 in current 187 in target"
# [3] "Component “reparar_pavimento”: 'is.NA' value mismatch: 264 in current 263 in target"
# [4] "Component “reparar_rampa”: 'is.NA' value mismatch: 183 in current 181 in target"


# ------------------------------------------------------------------------------
# Checagem - Horizontal - Linha do grupo revisado bate com revisão no QGIS
# ------------------------------------------------------------------------------

# Qual linha não estã batendo nos dataframes? -> horizontal
this <- which(is.na(teste$horizontal) != is.na(analises_claros$horizontal))
analises_claros %>% slice(this)
(lala <- teste %>% slice(this))

# Isolar grupo defeituoso (group_id_rev)
boo <- esq %>%
  st_drop_geometry() %>%
  mutate(image = basename(imagepath)) %>%
  filter(image %in% lala$image) %>%
  select(group_id_rev)

esq %>%
  st_drop_geometry() %>%
  mutate(image = basename(imagepath),
         marcacao = ifelse(image %in% lala$image, TRUE, FALSE)) %>%
  filter(group_id_rev %in% boo$group_id_rev) %>%
  select(marcacao, point_id, group_id, group_id_rev, campo, eval_cols)



# ------------------------------------------------------------------------------
# Checagem - Inadequada
# ------------------------------------------------------------------------------

# Qual linha não estã batendo nos dataframes? -> inadequada
this <- which(is.na(teste$inadequada) != is.na(analises_claros$inadequada))
analises_claros %>% slice(this)
(lala <- teste %>% slice(this))

# Isolar grupo defeituoso (group_id_rev)
boo <- esq %>%
  st_drop_geometry() %>%
  mutate(image = basename(imagepath)) %>%
  filter(image %in% lala$image) %>%
  select(group_id_rev)

esq %>%
  st_drop_geometry() %>%
  mutate(image = basename(imagepath),
         marcacao = ifelse(image %in% lala$image, TRUE, FALSE)) %>%
  filter(group_id_rev %in% boo$group_id_rev) %>%
  select(marcacao, point_id, group_id, group_id_rev, campo, eval_cols)


# ------------------------------------------------------------------------------
# Checagem - Reparar pavimento - mesma linha de "horizontal"
# ------------------------------------------------------------------------------

# # Qual linha não estã batendo nos dataframes? -> horizontal
# this <- which(is.na(teste$reparar_pavimento) != is.na(analises_claros$reparar_pavimento))
# analises_claros %>% slice(this)
# (lala <- teste %>% slice(this))
#
# # Isolar grupo defeituoso (group_id_rev)
# boo <- esq %>%
#   st_drop_geometry() %>%
#   mutate(image = basename(imagepath)) %>%
#   filter(image %in% lala$image) %>%
#   select(group_id_rev)
#
# esq %>%
#   st_drop_geometry() %>%
#   mutate(image = basename(imagepath),
#          marcacao = ifelse(image %in% lala$image, TRUE, FALSE)) %>%
#   filter(group_id_rev %in% boo$group_id_rev) %>%
#   select(marcacao, point_id, group_id, group_id_rev, campo, eval_cols)


# ------------------------------------------------------------------------------
# Checagem - Reparar rampa
# ------------------------------------------------------------------------------

# Qual linha não estã batendo nos dataframes? -> horizontal
this <- which(is.na(teste$reparar_rampa) != is.na(analises_claros$reparar_rampa))
analises_claros %>% slice(this)
(lala <- teste %>% slice(this))

# Isolar grupo defeituoso (group_id_rev)
boo <- esq %>%
  st_drop_geometry() %>%
  mutate(image = basename(imagepath)) %>%
  filter(image %in% lala$image) %>%
  select(group_id_rev)

esq %>%
  st_drop_geometry() %>%
  mutate(image = basename(imagepath),
         marcacao = ifelse(image %in% lala$image, TRUE, FALSE)) %>%
  filter(group_id_rev %in% boo$group_id_rev) %>%
  select(marcacao, point_id, group_id, group_id_rev, campo, eval_cols)




# ------------------------------------------------------------------------------
# Juntar travessias que haviam ficado de fora
# ------------------------------------------------------------------------------

# Juntar com travessias faltantes, revistas pelo Claros - essas travessias já
# vieram com as análises feitas no QGIS, é só juntar a esse shape
trav <- sprintf('%s/travessias_faltantes_merged.gpkg', pasta_analises)
trav <- read_sf(trav)

# Padronizar colunas dos dois datframes
trav <-
  trav %>%
  mutate(
    group_id = as.numeric(NA),
    is_na_group = as.logical(NA),
    video_duration = 14,
    new_group_id = as.integer(NA),
    new_group_size = as.integer(NA),
    diff_point = as.numeric(NA),
    diff_group = as.numeric(NA),
    suspicious = as.logical(NA),
    manter = as.logical(NA)
  ) %>%
  select(point_id,
         group_id,
         is_na_group,
         sql,
         campo,
         imagepath,
         video_path,
         start_time,
         video_duration,
         new_group_id,
         new_group_size,
         rampa,
         horizontal,
         inadequada,
         repintar_horizontal,
         reparar_pavimento,
         reparar_rampa,
         avaliado,
         geom,
         diff_point,
         diff_group,
         suspicious,
         group_id_rev,
         manter
         )

names(trav) ==  names(esq)

# Juntar as esquinas que faltavam
esq <- rbind(esq, trav) %>% arrange(group_id_rev, point_id)


# ------------------------------------------------------------------------------
# Padronizar análise para visualização
# ------------------------------------------------------------------------------

# Padronizar análises Claros das travessias
esq <- esq %>%
  mutate(travessia_ok = case_when(horizontal == TRUE & rampa == TRUE ~ '1 - adequada',
                                  horizontal == TRUE & (rampa == FALSE | is.na(rampa)) ~ '2 - parcial',
                                  (horizontal == FALSE | is.na(horizontal)) & rampa == TRUE ~ '2 - parcial',
                                  TRUE ~ '3 - inadequada'))

# esq %>% filter(group_id_rev == 412) %>% select(group_id_rev, rampa, horizontal, inadequada, repintar_horizontal, travessia_ok)

# Juntar análises IME por ponto
esq <- esq %>%
  mutate(image = basename(imagepath),
         group_id = as.integer(group_id),
         group_id_rev = as.integer(group_id_rev)) %>%
  left_join(ime, by = 'image') %>%
  select(-c(diff_point, diff_group, suspicious, manter, image, is_na_group, new_group_id, new_group_size)) %>%
  relocate(geom, .after = last_col()) %>%
  relocate(group_id_rev, .after = 'group_id')


# Excluir grupos que não fazem parte da análise
esq <- esq %>% filter(!group_id_rev %in% excluir)

# out_gpkg <- sprintf('%s/esquinas_sem_dia_4_rev.gpkg', pasta_analises)
out_gpkg <- sprintf('%s/esquinas_com_dia_4_rev.gpkg', pasta_analises)
st_write(esq, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)


# Ao final, gravar esse mesmo shape na pasta de resultados
out_gpkg <- sprintf('%s/travessias_resultados.gpkg', pasta_analises)
st_write(esq, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)

esq %>% st_drop_geometry() %>% select(group_id_rev) %>% distinct() %>% nrow()


linhas_esq <- sprintf('%s/ids_linhas_travessias.csv', pasta_resultados)
linhas_esq <- read_delim(linhas_esq, delim = ',', col_types = 'ic')


esq %>% filter(!group_id_rev %in% linhas_esq$group_id_rev) %>% ver_leaflet()