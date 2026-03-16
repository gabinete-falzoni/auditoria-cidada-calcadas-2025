# Resultados das análises, a partir dos shapes revistos e consolidados

library('tidyverse')
library('tidylog')
library('sf')
library('leaflet')

# Estrutura de pastas
# pasta_base <- '/media/livre/Expansion'
pasta_base <- '/mnt/fern/Dados'
pasta_audi <- sprintf('%s/projetos/2025_Auditoria_Calcadas', pasta_base)
pasta_proc <- sprintf('%s/01_dados_processados', pasta_audi)
pasta_resultados <- sprintf('%s/05_resultados', pasta_proc)


# ------------------------------------------------------------------------------
# Funções
# ------------------------------------------------------------------------------

# Retorna resumo da variável, com subtotais e percentuais
resumir <- function(df, var) {
  col_name = rlang::sym(as.character(var))

  if ('sf' %in% class(df)) {
    df <- df %>% st_drop_geometry()
  }
  df <-
    df %>%
    # mutate(!!col_name := factor(!!col_name, levels = c(TRUE, FALSE))) %>%
    group_by(!!col_name) %>%
    tally() %>%
    mutate(perc = n / sum(n) * 100,
           perc_2 = round(n / sum(n) * 100))
  return(df)
}


# ------------------------------------------------------------------------------
# Resultados das análises - linhas
# ------------------------------------------------------------------------------

# Shape de linhas com os resultados de travessias
result_linhas <- sprintf('%s/auditoria_calcadas_bras_linhas_travessias.gpkg', pasta_resultados)
result_linhas <- read_sf(result_linhas)


# Proporção de travessias com faixas de pedestres
result_linhas %>%
  st_drop_geometry() %>%
  group_by(trav_horizontal) %>%
  tally() %>%
  mutate(perc = n / sum(n))
# trav_horizontal     n  perc
# <lgl>           <int> <dbl>
# 1 FALSE             170 0.294
# 2 TRUE              408 0.706

trav_faixa <- result_linhas %>% st_drop_geometry() %>% filter(trav_horizontal)

resumir(result_linhas, 'trav_categoria')
# trav_categoria     n  perc perc_2
# <chr>          <int> <dbl>  <dbl>
# 1 1 - adequada     333  57.6     58
# 2 2 - parcial       75  13.0     13
# 3 3 - inadequada   170  29.4     29


resumir(trav_faixa, 'trav_categoria')
# trav_categoria     n  perc perc_2
# <chr>          <int> <dbl>  <dbl>
# 1 1 - adequada     333  81.6     82
# 2 2 - parcial       75  18.4     18


# ------------------------------------------------------------------------------
# Resultados das análises - pontos
# ------------------------------------------------------------------------------

# Shape de pontos, filtrar com os resultados de travessias
result_ptos <- sprintf('%s/auditoria_calcadas_bras.gpkg', pasta_resultados)
result_ptos <- read_sf(result_ptos) %>% st_drop_geometry() %>% filter(trav_flag)

# Marcar os pontos com SQL nulo (que detectam a travessia) e anteriores, para
# servirem como referência às marcações de travessia e rampa do IME
result_ptos <- result_ptos %>%
  group_by(trav_group_id) %>%
  mutate(flag_inicio_travessia = row_number() <= last(which(is.na(sql))), .after = 'sql') %>%
  ungroup()


# # Travessias curtas, no meio de quadra, não estão sendo marcadas como flag_inicio_travessia
# # filter(flag_inicio_travessia & trav_horizontal | trav_group_id %in% c(124, 975, 977, 984, 985, 990))
# th <- result_ptos %>%
#   filter(trav_horizontal) %>%
#   group_by(trav_group_id) %>%
#   tally()
#
#
# th_ini <- result_ptos %>%
#   filter(flag_inicio_travessia & trav_horizontal) %>%
#   group_by(trav_group_id) %>%
#   summarise(crosswalk = sum(crosswalk),
#             n = n())
#
# th %>% filter(!trav_group_id %in% th_ini$trav_group_id)
# result_ptos %>% filter(trav_group_id == 975) %>% select(sql, flag_inicio_travessia, trav_horizontal, crosswalk)

# ------------------------------------------------------------------------------
# Cruzamento - Resultados das análises Claros de travessias x crosswalk (IME)
# ------------------------------------------------------------------------------

# Performance algoritmo de detecção de crosswalk, considerando todos os frames
# da travessia
result_ptos %>%
  filter(trav_horizontal) %>%
  group_by(trav_group_id) %>%
  summarise(crosswalk = sum(crosswalk),
            n = n()) %>%
  mutate(crosswalk_p = crosswalk / n) %>%
  select(crosswalk_p) %>%
  summary()
# crosswalk_p
# Min.   :0.0000
# 1st Qu.:0.5699
# Median :0.6471
# Mean   :0.6314
# 3rd Qu.:0.7333
# Max.   :1.0000

# Performance algoritmo de detecção de crosswalk, considerando somente os frames
# de início e meio da travessia (frames cujo SQL é NA ou anteriores a eles)
result_ptos %>%
  # filter(flag_inicio_travessia & trav_horizontal) %>%
  filter(flag_inicio_travessia & trav_horizontal | trav_group_id %in% c(124, 975, 977, 984, 985, 990)) %>%
  group_by(trav_group_id) %>%
  summarise(crosswalk = sum(crosswalk),
            n = n()) %>%
  mutate(crosswalk_p = crosswalk / n) %>%
  select(crosswalk_p) %>%
  summary()
# crosswalk_p
# Min.   :0.0000
# 1st Qu.:0.7692
# Median :0.9000
# Mean   :0.8318
# 3rd Qu.:1.0000
# Max.   :1.0000


# ------------------------------------------------------------------------------
# Cruzamento - Resultados das análises Claros de rampas x curbramp (IME)
# ------------------------------------------------------------------------------

# Performance algoritmo de detecção de curbramp, considerando todos os frames
# da travessia
result_ptos %>%
  filter(trav_horizontal & trav_rampa) %>%
  group_by(trav_group_id) %>%
  summarise(curbramp = sum(curbramp),
            n = n()) %>%
  mutate(curbramp_p = curbramp / n) %>%
  select(curbramp_p) %>%
  summary()
# curbramp_p
# Min.   :0.0000
# 1st Qu.:0.4615
# Median :0.6250
# Mean   :0.5862
# 3rd Qu.:0.7500
# Max.   :1.0000

# Performance algoritmo de detecção de curbramp, considerando somente os frames
# de início e meio da travessia (frames cujo SQL é NA ou anteriores a eles)
result_ptos %>%
  filter(flag_inicio_travessia & trav_horizontal & trav_rampa) %>%
  group_by(trav_group_id) %>%
  summarise(curbramp = sum(curbramp),
            n = n()) %>%
  mutate(curbramp_p = curbramp / n) %>%
  select(curbramp_p) %>%
  summary()
# curbramp_p
# Min.   :0.0000
# 1st Qu.:0.5000
# Median :0.7273
# Mean   :0.6692
# 3rd Qu.:0.8750
# Max.   :1.0000


# ------------------------------------------------------------------------------
# Boxplot performance algoritmos crosswalk e curbramp
# ------------------------------------------------------------------------------

ime_1 <- result_ptos %>%
  # filter(flag_inicio_travessia & trav_horizontal) %>%
  filter(flag_inicio_travessia & trav_horizontal | trav_group_id %in% c(124, 975, 977, 984, 985, 990)) %>%
  group_by(trav_group_id) %>%
  summarise(crosswalk = sum(crosswalk),
            n = n()) %>%
  mutate(crosswalk_p = crosswalk / n) %>%
  select(crosswalk_p)


ime_2 <- result_ptos %>%
  filter(flag_inicio_travessia & trav_horizontal & trav_rampa) %>%
  group_by(trav_group_id) %>%
  summarise(curbramp = sum(curbramp),
            n = n()) %>%
  mutate(curbramp_p = curbramp / n) %>%
  select(curbramp_p)


df_long <- data.frame(
  value = c(ime_1$crosswalk_p, ime_2$curbramp_p),
  variable = c(
    rep("Crosswalk", length(ime_1$crosswalk_p)),
    rep("Curb Ramp", length(ime_2$curbramp_p))
  )
)

counts <- df_long %>% count(variable)
# variable   n
# 1 Crosswalk 408
# 2 Curb Ramp 329

labels_with_n <- setNames(
  paste0(counts$variable, "\n(n = ", counts$n, ")"),
  counts$variable
)

# # Violin plot - resultado estranhamente sexualizado
# ggplot(df_long, aes(variable, value, fill = variable)) +
#   geom_violin(trim = FALSE, alpha = 0.4) +
#   geom_boxplot(width = 0.2, outlier.alpha = 0.3) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1),
#                      limits = c(0, 1)) +
#   theme_minimal() +
#   theme(legend.position = "none")


# Violin sem as curvas sexualizadas
ggplot(df_long, aes(variable, value, fill = variable)) +
  geom_violin(trim = FALSE, alpha = 0.1, color = NA) +
  geom_boxplot(width = 0.2, outlier.alpha = 0.3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)
                     ) +
  labs(x = NULL, y = "Proporção (quadros detectados / total de quadros)") +
  scale_x_discrete(labels = labels_with_n) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14, family = "Montserrat"),
        panel.grid.minor = element_blank()
        )

# Boxplot
ggplot(df_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(width = 0.3, outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.06, alpha = 0.1) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(x = NULL, y = "Proporção (quadros detectados / total de quadros)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14, family = "Montserrat"),
        panel.grid.minor = element_blank()
        )

# ------------------------------------------------------------------------------
# Resultados das análises Claros
# ------------------------------------------------------------------------------

sumario_trav <- function(df, var_name, filter_expr = NULL, label = NA) {
  if (!is.null(filter_expr)) {
    df <- df %>% filter(!!filter_expr)
  }

  df <- df %>%
    mutate(category = replace_na(!!rlang::sym(var_name), FALSE)) %>%
    group_by(trav_group_id) %>%
    summarise(category = first(category)) %>%
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
boo <- sumario_trav(result_ptos, var_name = 'trav_repintar_horiz', filter_expr = expr(!!rlang::sym("trav_horizontal") == TRUE), label = 'Repintar faixa')
goo <- sumario_trav(result_ptos, var_name = 'trav_reparar_pavim', filter_expr = expr(!!rlang::sym("trav_horizontal") == TRUE), label = 'Reparar pavimento')
this <- sumario_trav(result_ptos, var_name = 'trav_inadequada', filter_expr = expr(!!rlang::sym("trav_rampa") == TRUE), label = 'Rampa inadequada')
that <- sumario_trav(result_ptos, var_name = 'trav_reparar_rampa', filter_expr = expr(!!rlang::sym("trav_rampa") == TRUE), label = 'Reparar rampa(s)')
those <- rbind(boo, goo, that, this)


ggplot(those, aes(x = perc,
                  y = fct_reorder(category, -perc),
                  color = perc)) +

  # 1️⃣ Grey remainder (perc → 100%)
  geom_segment(
    aes(x = perc, xend = 1,
        yend = fct_reorder(category, -perc)),
    color = "grey80",
    linewidth = 0.2
  ) +

  # 2️⃣ Main colored segment (0 → perc)
  geom_segment(
    aes(x = 0, xend = perc,
        yend = fct_reorder(category, -perc)),
    linewidth = 1.2
  ) +

  # 3️⃣ Lollipop point
  geom_point(size = 3.5) +

  # 4️⃣ Label ABOVE the point
  geom_text(
    aes(label = paste0(
      scales::percent(perc, accuracy = 1),
      " (n=", n_tot, ")"
    )),
    vjust = -1.3,   # move above
    hjust = 0.5,
    size = 4.0,
    color = "black"
  ) +

  scale_color_gradientn(
    colors = c("#1A9641", "#FDAE61", "#D7191C"),
    limits = c(0, 0.5),
    guide = "none"
  ) +

  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.05)
  ) +

  labs(
    #title = "Travessias - Resultados principais",
    #subtitle = "(percentuais calculados quando elemento existe)",
    x = NULL,
    y = NULL
  ) +

  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 13),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "Montserrat"),
    plot.subtitle = element_text(hjust = 0.5)
  )


# ggplot(those, aes(x = perc,
#                   y = fct_reorder(category, -perc),
#                   color = perc
# )) +
#   geom_segment(
#     aes(x = 0, xend = perc, yend = category),
#     # color = "#2C7FB8",
#     linewidth = 1
#   ) +
#   # geom_point(size = 3.5, color = "#2C7FB8") +
#   geom_point(size = 3.5) +
#   geom_text(
#     aes(label = paste0(
#       scales::percent(perc, accuracy = 1),
#       " (n=", n, ")"
#     )),
#     hjust = -0.1,
#     size = 3.5,
#     color = "black"
#   ) +
#   scale_color_gradientn(
#     colors = c("#1A9641", "#FDAE61", "#D7191C"),  # red → yellow → green
#     limits = c(0, 0.5),
#     # labels = scales::percent_format(accuracy = 1),
#     guide = "none",
#     name = NULL
#   ) +
#   scale_x_continuous(
#     labels = scales::percent_format(accuracy = 1),
#     limits = c(0, 1.08)
#   ) +
#   labs(
#     title = "Travessias - Resultados principais",
#     subtitle = '(percentuais calculados sempre que elemento existe na travessia)',
#     x = NULL,
#     y = NULL
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     # Remove y-axis numbers and ticks
#     # axis.text.y = element_blank(),
#     # axis.ticks.y = element_blank(),
#     axis.text.x = element_blank(),
#     # Remove background lines
#     panel.grid = element_blank(),
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "Montserrat"),
#     plot.subtitle = element_text(hjust = 0.5),
#     # Center relative to the whole plot area, not just chart area
#     # plot.title.position = "plot",
#   )

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
