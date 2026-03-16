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

# Shape de linhas com os resultados agregados do surface_problem e análises manuais
result <- sprintf('%s/auditoria_calcadas_bras_linhas_quadras.gpkg', pasta_resultados)
result <- read_sf(result)


# Proporção do PEC Calçadas no território
result %>%
  st_drop_geometry() %>%
  group_by(cc_pec) %>%
  summarise(length_m = sum(length_m)) %>%
  mutate(perc = length_m / sum(length_m))
# cc_pec                length_m  perc
# <chr>                    <dbl> <dbl>
# 1 DECRETO N 58.845/2019   19285. 0.338
# 2 NA                      37748. 0.662


# Praças e Largos -> para o Brás, somente um dos SQL não está já na camada
# de Cadastro > Lotes como lote municipal
praca_largo <- '0030160070'
result <- result %>%
  mutate(lote_publico = ifelse(sql == praca_largo | lo_tp_lote == 'M' | !is.na(lote_tp_prop) | !is.na(cc_pec), TRUE, FALSE),
         quadra_flag_bur_sup2 = ifelse(quadra_superf_2 | quadra_flag_buraco, TRUE, FALSE))

quadras <- result %>% st_drop_geometry() %>% select(imagepath, lote_publico, matches('^quadra_'), surface_problem_prop, length_m)
# quadras %>% group_by(imagepath) %>% tally() %>% filter(n > 1)


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
    mutate(!!col_name := factor(!!col_name, levels = c(TRUE, FALSE))) %>%
    group_by(!!col_name) %>%
    tally() %>%
    mutate(perc = round(n / sum(n) * 100))
  return(df)
}

# Retorna resumo da variável, com subtotais e percentuais
resumir_summarise <- function(df, group_var, sum_var) {
  col_name1 = rlang::sym(as.character(group_var))
  col_name2 = rlang::sym(as.character(sum_var))

  if ('sf' %in% class(df)) {
    df <- df %>% st_drop_geometry()
  }
  df <- df %>%
    mutate(!!col_name1 := factor(!!col_name1, levels = c(TRUE, FALSE))) %>%
    group_by(!!col_name1) %>%
    summarise(n = sum(!!col_name2, na.rm = TRUE), .groups = 'drop') %>%
    mutate(perc = round(n / sum(n) * 100))

  return(df)
}


# ------------------------------------------------------------------------------
# Resultados das análises
# ------------------------------------------------------------------------------

resumir(quadras, 'quadra_superf_1')
# quadra_superf_1     n  perc
# <fct>           <int> <dbl>
# 1 TRUE              759    13
# 2 FALSE            5058    87

resumir(quadras, 'quadra_superf_2')
# quadra_superf_2     n  perc
# <fct>           <int> <dbl>
# 1 TRUE              330     6
# 2 FALSE            5487    94

resumir(quadras, 'quadra_flag_buraco')
# quadra_flag_buraco     n  perc
# <fct>              <int> <dbl>
# 1 TRUE                  95     2
# 2 FALSE               5722    98


resumir(quadras, 'quadra_flag_bur_sup')
# quadra_flag_bur_sup     n  perc
# <fct>               <int> <dbl>
# 1 TRUE                 1184    20
# 2 FALSE                4633    80

resumir_summarise(quadras, 'quadra_superf_1', 'length_m')
# quadra_superf_1      n  perc
# <fct>            <dbl> <dbl>
# 1 TRUE             6402.    11
# 2 FALSE           50631.    89

resumir_summarise(quadras, 'quadra_superf_2', 'length_m')
# quadra_superf_2      n  perc
# <fct>            <dbl> <dbl>
# 1 TRUE             3570.     6
# 2 FALSE           53462.    94

resumir_summarise(quadras, 'quadra_flag_buraco', 'length_m')
# quadra_flag_buraco      n  perc
# <fct>               <dbl> <dbl>
# 1 TRUE                 878.     2
# 2 FALSE              56155.    98

resumir_summarise(quadras, 'quadra_flag_bur_sup', 'length_m')
# flag_bur_sup      n  perc
# <fct>         <dbl> <dbl>
# 1 TRUE         10652.    19
# 2 FALSE        46140.    81


quadras %>% filter(lote_publico) %>% nrow()
quadras %>% filter(lote_publico) %>% select(length_m) %>% sum()


quadras %>% filter(lote_publico) %>% resumir('quadra_superf_1')
# quadra_superf_1     n  perc
# <fct>           <int> <dbl>
# 1 TRUE              216     9
# 2 FALSE            2196    91

quadras %>% filter(lote_publico) %>% resumir('quadra_superf_2')
# quadra_superf_2     n  perc
# <fct>           <int> <dbl>
# 1 TRUE               73     3
# 2 FALSE            2339    97

quadras %>% filter(lote_publico) %>% resumir('quadra_flag_buraco')
# quadra_flag_buraco     n  perc
# <fct>              <int> <dbl>
# 1 TRUE                  42     2
# 2 FALSE               2370    98

quadras %>% filter(lote_publico) %>% resumir('quadra_flag_bur_sup')
# quadra_flag_bur_sup     n  perc
# <fct>               <int> <dbl>
# 1 TRUE                  331    14
# 2 FALSE                2081    86


quadras %>% filter(lote_publico) %>% resumir_summarise('quadra_superf_1', 'length_m')
# quadra_superf_1      n  perc
# <fct>            <dbl> <dbl>
# 1 TRUE             1579.     7
# 2 FALSE           20642.    93

quadras %>% filter(lote_publico) %>% resumir_summarise('quadra_superf_2', 'length_m')
# quadra_superf_2      n  perc
# <fct>            <dbl> <dbl>
# 1 TRUE              637.     3
# 2 FALSE           21585.    97

quadras %>% filter(lote_publico) %>% resumir_summarise('quadra_flag_buraco', 'length_m')
# quadra_flag_buraco      n  perc
# <fct>               <dbl> <dbl>
# 1 TRUE                 422.     2
# 2 FALSE              21799.    98

quadras %>% filter(lote_publico) %>% resumir_summarise('quadra_flag_bur_sup', 'length_m')
# quadra_flag_bur_sup      n  perc
# <fct>                <dbl> <dbl>
# 1 TRUE                 2638.    12
# 2 FALSE               19584.    88


# ------------------------------------------------------------------------------
# Cruzamento - Resultados das análises x surface_problem (IME)
# ------------------------------------------------------------------------------


# Resultados IME - Visão computacional surface problem
quadras %>%
  select(surface_problem_prop) %>%
  mutate(surface_problem_grades = case_when(between(surface_problem_prop, 0.000, 0.20) ~ '0.00 - 0.20',
                                            between(surface_problem_prop, 0.201, 0.40) ~ '0.21 - 0.40',
                                            between(surface_problem_prop, 0.401, 0.60) ~ '0.41 - 0.60',
                                            between(surface_problem_prop, 0.601, 0.80) ~ '0.61 - 0.80',
                                            between(surface_problem_prop, 0.801, 1.00) ~ '0.81 - 1.00'
  )) %>%
  # filter(is.na(surface_problem_grades))
  group_by(surface_problem_grades) %>%
  tally() %>%
  mutate(perc = n / sum(n),
         r_perc = round(n / sum(n) * 100))
# surface_problem_grades     n  perc r_perc
# <chr>                  <int> <dbl>  <dbl>
# 1 0.00 - 0.20             2419 0.416     42
# 2 0.21 - 0.40              913 0.157     16
# 3 0.41 - 0.60              758 0.130     13
# 4 0.61 - 0.80              708 0.122     12
# 5 0.81 - 1.00             1019 0.175     18


# Quando Claros marcou buraco ou superfície, como o surface_problem marcou?
quadras %>%
  # Flag: qualquer problema de superfície ou buraco
  filter(quadra_flag_bur_sup) %>%
  select(surface_problem_prop) %>%
  mutate(surface_problem_grades = case_when(between(surface_problem_prop, 0.000, 0.20) ~ '0.00 - 0.20',
                                            between(surface_problem_prop, 0.201, 0.40) ~ '0.21 - 0.40',
                                            between(surface_problem_prop, 0.401, 0.60) ~ '0.41 - 0.60',
                                            between(surface_problem_prop, 0.601, 0.80) ~ '0.61 - 0.80',
                                            between(surface_problem_prop, 0.801, 1.00) ~ '0.81 - 1.00'
  )) %>%
  # filter(is.na(surface_problem_grades))
  group_by(surface_problem_grades) %>%
  tally() %>%
  mutate(perc = n / sum(n),
         r_perc = round(n / sum(n) * 100))# %>% ungroup() %>% select(n) %>% sum()
# surface_problem_grades     n  perc r_perc
# <chr>                  <int> <dbl>  <dbl>
# 1 0.00 - 0.20              167 0.141     14
# 2 0.21 - 0.40              129 0.109     11
# 3 0.41 - 0.60              191 0.161     16
# 4 0.61 - 0.80              222 0.188     19
# 5 0.81 - 1.00              475 0.401     40

quadras %>%
  # Flag: problema de superfície nível 2 ou buraco
  filter(quadra_flag_bur_sup2) %>%
  select(surface_problem_prop) %>%
  mutate(surface_problem_grades = case_when(between(surface_problem_prop, 0.000, 0.20) ~ '0.00 - 0.20',
                                            between(surface_problem_prop, 0.201, 0.40) ~ '0.21 - 0.40',
                                            between(surface_problem_prop, 0.401, 0.60) ~ '0.41 - 0.60',
                                            between(surface_problem_prop, 0.601, 0.80) ~ '0.61 - 0.80',
                                            between(surface_problem_prop, 0.801, 1.00) ~ '0.81 - 1.00'
  )) %>%
  # filter(is.na(surface_problem_grades))
  group_by(surface_problem_grades) %>%
  tally() %>%
  mutate(perc = n / sum(n),
         r_perc = round(n / sum(n) * 100))# %>% ungroup() %>% select(n) %>% sum()
# surface_problem_grades     n  perc r_perc
# <chr>                  <int> <dbl>  <dbl>
# 1 0.00 - 0.20               50 0.118     12
# 2 0.21 - 0.40               43 0.101     10
# 3 0.41 - 0.60               64 0.151     15
# 4 0.61 - 0.80               69 0.162     16
# 5 0.81 - 1.00              199 0.468     47



# Quando Claros marcou buraco ou superfície, como o surface_problem marcou?
quadras %>%
  filter(quadra_flag_superf) %>%
  mutate(surface_problem_grades = case_when(between(surface_problem_prop, 0.000, 0.10) ~ '0.00 - 0.10',
                                            between(surface_problem_prop, 0.101, 0.20) ~ '0.11 - 0.20',
                                            between(surface_problem_prop, 0.201, 0.30) ~ '0.21 - 0.30',
                                            between(surface_problem_prop, 0.301, 0.40) ~ '0.31 - 0.40',
                                            between(surface_problem_prop, 0.401, 0.50) ~ '0.41 - 0.50',
                                            between(surface_problem_prop, 0.501, 0.60) ~ '0.51 - 0.60',
                                            between(surface_problem_prop, 0.601, 0.70) ~ '0.61 - 0.70',
                                            between(surface_problem_prop, 0.701, 0.80) ~ '0.71 - 0.80',
                                            between(surface_problem_prop, 0.801, 0.90) ~ '0.81 - 0.90',
                                            between(surface_problem_prop, 0.901, 1.00) ~ '0.91 - 1.00'
  )) %>%
  group_by(surface_problem_grades) %>%
  tally() %>%
  mutate(perc = n / sum(n))
# surface_problem_grades     n   perc
# <chr>                  <int>  <dbl>
# 1 0.00 - 0.10              111 0.0938
# 2 0.11 - 0.20               56 0.0473
# 3 0.21 - 0.30               68 0.0574
# 4 0.31 - 0.40               61 0.0515
# 5 0.41 - 0.50              112 0.0946
# 6 0.51 - 0.60               79 0.0667
# 7 0.61 - 0.70               80 0.0676
# 8 0.71 - 0.80              142 0.120
# 9 0.81 - 0.90               72 0.0608
# 10 0.91 - 1.00              403 0.340
# > 0.120+0.0608+0.340
# [1] 0.5208 <- 52% das vezes, a proporção ficou acima de 70%
# > 0.120+0.0608+0.340+0.0676
# [1] 0.5884 <- 59% das vezes, a proporção ficou acima de 60%
# > 0.120+0.0608+0.340+0.0676+0.0667
# [1] 0.6551 <- 65% das vezes, marcou acima de 50%
#

# # Quando Claros marcou problema de superfície, como o surface_problem marcou?
quadras %>%
  st_drop_geometry() %>%
  filter(flag_superficie) %>%
  mutate(surface_problem_grades = case_when(between(surface_problem_prop, 0.000, 0.10) ~ '0.00 - 0.10',
                                            between(surface_problem_prop, 0.101, 0.20) ~ '0.11 - 0.20',
                                            between(surface_problem_prop, 0.201, 0.30) ~ '0.21 - 0.30',
                                            between(surface_problem_prop, 0.301, 0.40) ~ '0.31 - 0.40',
                                            between(surface_problem_prop, 0.401, 0.50) ~ '0.41 - 0.50',
                                            between(surface_problem_prop, 0.501, 0.60) ~ '0.51 - 0.60',
                                            between(surface_problem_prop, 0.601, 0.70) ~ '0.61 - 0.70',
                                            between(surface_problem_prop, 0.701, 0.80) ~ '0.71 - 0.80',
                                            between(surface_problem_prop, 0.801, 0.90) ~ '0.81 - 0.90',
                                            between(surface_problem_prop, 0.901, 1.00) ~ '0.91 - 1.00'
  )) %>%
  group_by(surface_problem_grades) %>%
  tally() %>%
  mutate(perc = n / sum(n))
# surface_problem_grades     n   perc
# <chr>                  <int>  <dbl>
# 1 0.00 - 0.10               98 0.0900
# 2 0.11 - 0.20               47 0.0432
# 3 0.21 - 0.30               59 0.0542
# 4 0.31 - 0.40               49 0.0450
# 5 0.41 - 0.50              102 0.0937
# 6 0.51 - 0.60               70 0.0643
# 7 0.61 - 0.70               78 0.0716
# 8 0.71 - 0.80              133 0.122
# 9 0.81 - 0.90               69 0.0634
# 10 0.91 - 1.00              384 0.353
# > 0.353+0.0634+0.122
# [1] 0.5384 <- 54% das vezes, a proporção ficou acima de 70%
# > 0.353+0.0634+0.122 +0.0716+0.0643
# > 0.353+0.0634+0.122 +0.0716
# [1] 0.61  <- 61% das vezes, marcou acima de 60%
# [1] 0.6743 <- 65% das vezes, marcou acima de 50%
#
