library('tidyverse')
library('tidylog')
library('sf')
library('leaflet')

# Estrutura de pastas
# pasta_base <- '/media/livre/Expansion'
pasta_base <- '/mnt/fern/Dados'
pasta_audi <- sprintf('%s/projetos/2025_Auditoria_Calcadas', pasta_base)
pasta_proc <- sprintf('%s/01_dados_processados', pasta_audi)
# pasta_analises <- sprintf('%s/04_analises', pasta_proc)
pasta_resultados <- sprintf('%s/05_resultados', pasta_proc)

eval_cols <- c('rampa', 'horizontal', 'inadequada', 'repintar_horizontal', 'reparar_pavimento', 'reparar_rampa')

trav <- sprintf('%s/travessias_resultados.gpkg', pasta_resultados)
trav <- read_sf(trav)

# Marcar os pontos com SQL nulo (que detectam a travessia) e anteriores, para
# servirem como referência às marcações de travessia e rampa do IME
trav <- trav %>%
  st_drop_geometry() %>%
  group_by(group_id_rev) %>%
  mutate(flag_inicio_travessia = row_number() <= last(which(is.na(sql))), .after = 'sql')


# ------------------------------------------------------------------------------
# Cruzamento - Resultados das análises Claros de travessias x crosswalk (IME)
# ------------------------------------------------------------------------------

# Performance algoritmo de detecção de crosswalk, considerando todos os frames
# da travessia
trav %>%
  filter(horizontal) %>%
  group_by(group_id_rev) %>%
  summarise(crosswalk = sum(crosswalk),
            n = n()) %>%
  mutate(crosswalk_p = crosswalk / n) %>%
  select(crosswalk_p) %>%
  summary()
# crosswalk_p
# Min.   :0.0000
# 1st Qu.:0.5652
# Median :0.6471
# Mean   :0.6314
# 3rd Qu.:0.7333
# Max.   :1.0000
# NA's   :6

# Performance algoritmo de detecção de crosswalk, considerando somente os frames
# de início e meio da travessia (frames cujo SQL é NA ou anteriores a eles)
trav %>%
  filter(flag_inicio_travessia & horizontal) %>%
  group_by(group_id_rev) %>%
  summarise(crosswalk = sum(crosswalk),
            n = n()) %>%
  mutate(crosswalk_p = crosswalk / n) %>%
  select(crosswalk_p) %>%
  # summary() %>%
  boxplot()
# crosswalk_p
# Min.   :0.0000
# 1st Qu.:0.7778
# Median :0.9000
# Mean   :0.8363
# 3rd Qu.:1.0000
# Max.   :1.0000
# NA's   :6


# ------------------------------------------------------------------------------
# Cruzamento - Resultados das análises Claros de rampas x curbramp (IME)
# ------------------------------------------------------------------------------

# Performance algoritmo de detecção de curbramp, considerando todos os frames
# da travessia
trav %>%
  filter(horizontal & rampa) %>%
  group_by(group_id_rev) %>%
  summarise(curbramp = sum(curbramp),
            n = n()) %>%
  mutate(curbramp_p = curbramp / n) %>%
  select(curbramp_p) %>%
  summary()
# curbramp_p
# Min.   :0.0000
# 1st Qu.:0.4615
# Median :0.6250
# Mean   :0.5879
# 3rd Qu.:0.7500
# Max.   :1.0000
# NA's   :6

# Performance algoritmo de detecção de curbramp, considerando somente os frames
# de início e meio da travessia (frames cujo SQL é NA ou anteriores a eles)
trav %>%
  filter(flag_inicio_travessia & horizontal & rampa) %>%
  group_by(group_id_rev) %>%
  summarise(curbramp = sum(curbramp),
            n = n()) %>%
  mutate(curbramp_p = curbramp / n) %>%
  select(curbramp_p) %>%
  summary()
# curbramp_p
# Min.   :0.0000
# 1st Qu.:0.5000
# Median :0.7273
# Mean   :0.6713
# 3rd Qu.:0.8750
# Max.   :1.0000
# NA's   :6


# ------------------------------------------------------------------------------
# Resultados das análises Claros
# ------------------------------------------------------------------------------

# Agrupar resultados por group_id_rev (1 resultado por grupo)
trav <- trav %>%
  st_drop_geometry() %>%
  group_by(group_id_rev) %>%
  summarise(rampa = first(rampa),
            horizontal = first(horizontal),
            inadequada = first(inadequada),
            repintar_horizontal = first(repintar_horizontal),
            reparar_pavimento = first(reparar_pavimento),
            reparar_rampa = first(reparar_rampa),
            travessia_analise = first(travessia_ok),
            crosswalk = sum(crosswalk),
            curbramp = sum(curbramp),
            surfaceproblem = sum(surfaceproblem),
            n = n()) %>%
  mutate(crosswalk_p = crosswalk / n,
         curbramp_p = curbramp / n,
         surfacep_p = surfaceproblem / n)

# trav <- trav %>%
#   st_drop_geometry() %>%
#   select(group_id_rev, all_of(eval_cols), travessia_ok) %>%
#   distinct(group_id_rev, .keep_all = TRUE)



# Considerando todas as travessias que fizemos ou analisamos
trav %>%
  group_by(travessia_analise) %>%
  tally() %>%
  mutate(perc = n / sum(n))


# Considerando somente as travessias que tinham faixa de pedestre
trav %>%
  filter(horizontal == TRUE) %>%
  group_by(travessia_analise) %>%
  tally() %>%
  mutate(perc = n / sum(n))

trav %>%
  filter(horizontal == TRUE) %>%
  mutate(repintar_horizontal = replace_na(repintar_horizontal, FALSE)) %>%
  group_by(repintar_horizontal) %>%
  tally() %>%
  mutate(perc = n / sum(n))

trav %>%
  filter(horizontal == TRUE) %>%
  mutate(reparar_pavimento = replace_na(reparar_pavimento, FALSE)) %>%
  group_by(reparar_pavimento) %>%
  tally() %>%
  mutate(perc = n / sum(n))

trav %>%
  filter(rampa == TRUE) %>%
  mutate(inadequada = replace_na(inadequada, FALSE)) %>%
  group_by(inadequada) %>%
  tally() %>%
  mutate(perc = n / sum(n))

trav %>%
  filter(rampa == TRUE) %>%
  mutate(reparar_rampa = replace_na(reparar_rampa, FALSE)) %>%
  group_by(reparar_rampa) %>%
  tally() %>%
  mutate(perc = n / sum(n))





sumario <- function(df, var_name, filter_expr = NULL, label = NA) {
  if (!is.null(filter_expr)) {
    df <- df %>% filter(!!filter_expr)
  }

  df <- df %>%
    mutate(category = replace_na(!!rlang::sym(var_name), FALSE)) %>%
    group_by(category) %>%
    tally() %>%
    mutate(perc = n / sum(n),
           n_tot = sum(n)) %>%
    filter(category == TRUE) %>%
    # select(-n) %>%
    mutate(category = ifelse(is.na(label), var_name, label))

  return(df)
}

# sumario(trav, var_name = 'inadequada', label = 'Calçada inadequada')
lala <- sumario(trav, var_name = 'travessia_analise', filter_expr = expr(!!rlang::sym("horizontal") == TRUE), label = 'Travessias inadequadas')
boo <- sumario(trav, var_name = 'repintar_horizontal', filter_expr = expr(!!rlang::sym("horizontal") == TRUE), label = 'Repintar faixa')
goo <- sumario(trav, var_name = 'reparar_pavimento', filter_expr = expr(!!rlang::sym("horizontal") == TRUE), label = 'Reparar pavimento')
this <- sumario(trav, var_name = 'inadequada', filter_expr = expr(!!rlang::sym("rampa") == TRUE), label = 'Rampa inadequada')
that <- sumario(trav, var_name = 'reparar_rampa', filter_expr = expr(!!rlang::sym("rampa") == TRUE), label = 'Reparar rampa(s)')
those <- rbind(boo, goo, that, this)


# those <- those %>% mutate(
#   category = factor(category, levels = c(
#     'Repintar faixa',
#     'Reparar pavimento',
#     'Reparar rampa(s)',
#     'Rampa inadequada'
#   ))
# )



# # Gráfico Lollipop com labels e n em cada eixo
# df_pct <- tibble(
#   category = c("A", "B", "C", "D"),
#   pct_true = c(.68, .50, .82, .30),
#   n = c(100, 90, 120, 60)
# )

ggplot(those, aes(x = perc,
                  y = fct_reorder(category, -perc),
                  color = perc
                  )) +
  geom_segment(
    aes(x = 0, xend = perc, yend = category),
    # color = "#2C7FB8",
    linewidth = 1
  ) +
  # geom_point(size = 3.5, color = "#2C7FB8") +
  geom_point(size = 3.5) +
  geom_text(
    aes(label = paste0(
      scales::percent(perc, accuracy = 1),
      " (n=", n, ")"
    )),
    hjust = -0.1,
    size = 3.5,
    color = "black"
  ) +
  scale_color_gradientn(
    colors = c("#1A9641", "#FDAE61", "#D7191C"),  # red → yellow → green
    limits = c(0, 0.5),
    # labels = scales::percent_format(accuracy = 1),
    guide = "none",
    name = NULL
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.08)
  ) +
  labs(
    title = "Travessias - Resultados principais",
    subtitle = '(percentuais calculados sempre que elemento existe na travessia)',
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # Remove y-axis numbers and ticks
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    # Remove background lines
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "Montserrat"),
    plot.subtitle = element_text(hjust = 0.5),
    # Center relative to the whole plot area, not just chart area
    # plot.title.position = "plot",
  )

#
#
# library(tidyverse)
#
#
# # Gráfico Lollipop sem labels
# df_pct <- tibble(
#   category = c("A", "B", "C", "D"),
#   pct_true = c(.68, .50, .82, .30),
#   n = c(100, 90, 120, 60)
# )
#
# ggplot(df_pct, aes(x = pct_true,
#                    y = fct_reorder(category, pct_true))) +
#   geom_segment(
#     aes(x = 0, xend = pct_true, yend = category),
#     color = "grey80",
#     linewidth = 1
#   ) +
#   geom_point(size = 3.5, color = "#2C7FB8") +
#   scale_x_continuous(
#     labels = scales::percent_format(accuracy = 1),
#     limits = c(0, 1)
#   ) +
#   labs(
#     x = "% True",
#     y = NULL
#   ) +
#   theme_minimal(base_size = 12)
