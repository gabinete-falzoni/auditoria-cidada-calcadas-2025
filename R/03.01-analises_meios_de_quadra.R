# Junta as análises das fotos feitas pelo Claros ao shape de meios_de_quadra_surface_problem_linhas

library('tidyverse')
library('tidylog')
library('sf')
library('leaflet')

# Estrutura de pastas
# pasta_base <- '/media/livre/Expansion'
pasta_base <- '/mnt/fern/Dados'
pasta_audi <- sprintf('%s/projetos/2025_Auditoria_Calcadas', pasta_base)
pasta_proc <- sprintf('%s/01_dados_processados', pasta_audi)
pasta_analises <- sprintf('%s/04_analises/fotos_resumo_por_lote', pasta_proc)
pasta_resultados <- sprintf('%s/05_resultados', pasta_proc)


# ------------------------------------------------------------------------------
# Funções
# ------------------------------------------------------------------------------

# Retorna resumo da variável, com subtotais e percentuais
resumir <- function(df, var) {
  col_name = rlang::sym(as.character(var))

  if ('sf' %in% class(meios_quadra)) {
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

  if ('sf' %in% class(meios_quadra)) {
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
# Análises Claros
# ------------------------------------------------------------------------------

# Análises Claros - Buracos
buraco_1 <- list.files(sprintf('%s/Buraco_01', pasta_analises), recursive = FALSE)
buraco_2 <- list.files(sprintf('%s/Buraco_02', pasta_analises), recursive = FALSE)
# Análises Claros - Superfície
superficie_1 <- list.files(sprintf('%s/Superficie_01', pasta_analises), recursive = FALSE)
superficie_2 <- list.files(sprintf('%s/Superficie_02', pasta_analises), recursive = FALSE)


# Shape de linhas com os resultados agregados do surface_problem
meios_quadra <- sprintf('%s/meios_de_quadra_surface_problem_linhas_pec_calcadas.gpkg', pasta_resultados)
meios_quadra <- read_sf(meios_quadra)

meios_quadra <- meios_quadra %>%
  mutate(img = basename(imagepath),
         buraco_1 = ifelse(img %in% buraco_1, TRUE, FALSE),
         buraco_2 = ifelse(img %in% buraco_2, TRUE, FALSE),
         superficie_1 = ifelse(img %in% superficie_1, TRUE, FALSE),
         superficie_2 = ifelse(img %in% superficie_2, TRUE, FALSE),
         flag_buracos = ifelse(buraco_1 | buraco_2, TRUE, FALSE),
         flag_superficie = ifelse(superficie_1 | superficie_2, TRUE, FALSE),
         .before = geom) %>%
  mutate(flag_bur_sup = ifelse(flag_buracos | flag_superficie, TRUE, FALSE),
         .before = geom)


# ------------------------------------------------------------------------------
# Marcação de lotes públicos (PEC, SEFAZ, Cadastro > Lotes, Praças e Largos)
# ------------------------------------------------------------------------------

# Geosampa: Cadastro > Lotes + marcação de Praça / Largo
lotes <- sprintf('%s/00_shapes_base/lotes_perimetro_auditoria_com_numero.gpkg', pasta_proc)
lotes <- read_sf(lotes)
lotes <- lotes %>% st_drop_geometry() %>% select(sql, lo_tp_quad, lo_tp_lote)

# Praças e Largos -> para o Brás, somente um dos SQL não está já na camada
# de Cadastro > Lotes como lote municipal
praca_largo <- '0030160070'

# Listagem de lotes públicos, conforme enviado pela SEFAZ: a classificação dos
# imóveis observa os códigos COB de acordo com a esfera administrativa: Municipal (20),
# Estadual (32) e Federal (42). No que tange às Sociedades de Economia Mista e
# Empresas Públicas, são utilizados os códigos 51 e 52.
lotes_publicos <- '/mnt/fern/Dados/dados/Pedidos_LAI/LAI_SEFAZ/94779_ESIC 94779.CSV'
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
  mutate(sql = str_pad(SQL, width = 11, side = "left", pad = "0"),
         .before = 1) %>%
  # ... cortar para 10 dígitos
  mutate(sql = str_sub(sql, 1, 10))


lotes_publicos %>% filter(str_detect(LOGRADOURO, 'PASTEUR'))
lotes_publicos %>% filter(str_detect(LOGRADOURO, 'DO ESTADO'))
lotes_publicos %>% filter(str_detect(LOGRADOURO, 'DE ALENCAR'))
lotes_publicos %>% filter(sql == '0180820017')



# Manter somente coluna de tipo de proprietário
lotes_publicos <- lotes_publicos %>% select(sql, tipo_proprietario = TIPO_PROPRIETARIO)


lotes_publicos %>% group_by(tipo_proprietario) %>% tally()
# TIPO_PROPRIETARIO                                     n
# <chr>                                             <int>
# 1 ESTADO OU AUTARQUIAS ESTADUAIS                     3678
# 2 P.M.S.P. OU AUTARQUIAS MUNICIPAIS                  4576
# 3 SOCIEDADES DE ECONOMIA MISTA OU EMPRESAS PÚBLICAS  3296
# 4 UNIÃO OU AUTARQUIAS FEDERAIS                       1253



# Juntar tudo no mesmo shape
meios_quadra <-
  meios_quadra %>%
  left_join(lotes, by = 'sql') %>%
  left_join(lotes_publicos, by = 'sql') %>%
  relocate(c(lo_tp_quad, lo_tp_lote, tipo_proprietario), .after = 'testada_m_double')

# Fazer marcacao única de lote público
meios_quadra <- meios_quadra %>%
  mutate(lote_publico = ifelse(sql == praca_largo | lo_tp_lote == 'M' | !is.na(tipo_proprietario) | !is.na(cc_pec), TRUE, FALSE),
         .after = 'tipo_proprietario')




# ------------------------------------------------------------------------------
# Exportar resultados
# ------------------------------------------------------------------------------

out_gpkg <- sprintf('%s/meios_de_quadra_surface_problem_analises_manuais_pec_calcadas_linhas.gpkg', pasta_resultados)
st_write(meios_quadra, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)


# ------------------------------------------------------------------------------
# Resultados das análises
# ------------------------------------------------------------------------------

resumir(meios_quadra, 'superficie_1')
# superficie_1     n  perc
# <fct>        <int> <dbl>
# 1 TRUE           759    13
# 2 FALSE         5058    87

resumir(meios_quadra, 'superficie_2')
# superficie_2     n  perc
# <fct>        <int> <dbl>
# 1 TRUE           329     6
# 2 FALSE         5488    94

resumir(meios_quadra, 'flag_superficie')
# flag_superficie     n  perc
# <fct>           <int> <dbl>
# 1 TRUE             1088    19
# 2 FALSE            4729    81

resumir(meios_quadra, 'buraco_1')
# buraco_1     n  perc
# <fct>    <int> <dbl>
# 1 TRUE        76     1
# 2 FALSE     5741    99

resumir(meios_quadra, 'buraco_2')
# buraco_2     n  perc
# <fct>    <int> <dbl>
# 1 TRUE        19     0
# 2 FALSE     5798   100


resumir(meios_quadra, 'flag_buracos')
# flag_buracos     n  perc
# <fct>        <int> <dbl>
# 1 TRUE            95     2
# 2 FALSE         5722    98


resumir(meios_quadra, 'flag_bur_sup')
# flag_bur_sup     n  perc
# <fct>        <int> <dbl>
# 1 TRUE          1183    20
# 2 FALSE         4634    80

resumir_summarise(meios_quadra, 'superficie_1', 'length_m')
# superficie_1      n  perc
# <fct>         <dbl> <dbl>
# 1 TRUE          6395.    11
# 2 FALSE        50397.    89

resumir_summarise(meios_quadra, 'superficie_2', 'length_m')
# superficie_2      n  perc
# <fct>         <dbl> <dbl>
# 1 TRUE          3464.     6
# 2 FALSE        53327.    94

resumir_summarise(meios_quadra, 'buraco_1', 'length_m')
# buraco_1      n  perc
# <fct>     <dbl> <dbl>
# 1 TRUE       578.     1
# 2 FALSE    56214.    99

resumir_summarise(meios_quadra, 'buraco_2', 'length_m')
# buraco_2      n  perc
# <fct>     <dbl> <dbl>
# 1 TRUE       215.     0
# 2 FALSE    56577.   100

resumir_summarise(meios_quadra, 'flag_buracos', 'length_m')
# flag_buracos      n  perc
# <fct>         <dbl> <dbl>
# 1 TRUE           792.     1
# 2 FALSE        55999.    99

resumir_summarise(meios_quadra, 'flag_bur_sup', 'length_m')
# flag_bur_sup      n  perc
# <fct>         <dbl> <dbl>
# 1 TRUE         10652.    19
# 2 FALSE        46140.    81



meios_quadra %>% filter(lote_publico) %>% resumir('superficie_1')
# superficie_1     n  perc
# <fct>        <int> <dbl>
# 1 TRUE           222     9
# 2 FALSE         2327    91

meios_quadra %>% filter(lote_publico) %>% resumir('superficie_2')
# superficie_2     n  perc
# <fct>        <int> <dbl>
# 1 TRUE            80     3
# 2 FALSE         2469    97

meios_quadra %>% filter(lote_publico) %>% resumir('flag_buracos')
# flag_buracos     n  perc
# <fct>        <int> <dbl>
# 1 TRUE            43     2
# 2 FALSE         2506    98

meios_quadra %>% filter(lote_publico) %>% resumir('flag_bur_sup')
# flag_bur_sup     n  perc
# <fct>        <int> <dbl>
# 1 TRUE           345    14
# 2 FALSE         2204    86

meios_quadra %>% filter(lote_publico) %>% resumir_summarise('superficie_1', 'length_m')
# superficie_1      n  perc
# <fct>         <dbl> <dbl>
# 1 TRUE          1652.     7
# 2 FALSE        22831.    93

meios_quadra %>% filter(lote_publico) %>% resumir_summarise('superficie_2', 'length_m')
# superficie_2      n  perc
# <fct>         <dbl> <dbl>
# 1 TRUE           727.     3
# 2 FALSE        23756.    97

meios_quadra %>% filter(lote_publico) %>% resumir_summarise('flag_buracos', 'length_m')
# flag_buracos      n  perc
# <fct>         <dbl> <dbl>
# 1 TRUE           382.     2
# 2 FALSE        24102.    98

meios_quadra %>% filter(lote_publico) %>% resumir_summarise('flag_bur_sup', 'length_m')
# flag_bur_sup      n  perc
# <fct>         <dbl> <dbl>
# 1 TRUE          2762.    11
# 2 FALSE        21722.    89


# ------------------------------------------------------------------------------
# Cruzamento - Resultados das análises x surface_problem (IME)
# ------------------------------------------------------------------------------

# Quando Claros marcou buraco ou superfície, como o surface_problem marcou?
meios_quadra %>%
  st_drop_geometry() %>%
  filter(flag_bur_sup) %>%
  select(surfaceproblem, surfaceproblem_prop, flag_bur_sup) %>%
  mutate(surface_problem_grades = case_when(between(surfaceproblem_prop, 0.00, 0.10) ~ '0.00 - 0.10',
                                            between(surfaceproblem_prop, 0.11, 0.20) ~ '0.11 - 0.20',
                                            between(surfaceproblem_prop, 0.21, 0.30) ~ '0.21 - 0.30',
                                            between(surfaceproblem_prop, 0.31, 0.40) ~ '0.31 - 0.40',
                                            between(surfaceproblem_prop, 0.41, 0.50) ~ '0.41 - 0.50',
                                            between(surfaceproblem_prop, 0.51, 0.60) ~ '0.51 - 0.60',
                                            between(surfaceproblem_prop, 0.61, 0.70) ~ '0.61 - 0.70',
                                            between(surfaceproblem_prop, 0.71, 0.80) ~ '0.71 - 0.80',
                                            between(surfaceproblem_prop, 0.81, 0.90) ~ '0.81 - 0.90',
                                            between(surfaceproblem_prop, 0.91, 1.00) ~ '0.91 - 1.00',
                                            # between(surfaceproblem_prop, 0.25, 0.49) ~ '0.25 - 0.49',
                                            # between(surfaceproblem_prop, 0.50, 0.74) ~ '0.50 - 0.74',
                                            # between(surfaceproblem_prop, 0.75, 1.00) ~ '0.75 - 1.00'
                                            )) %>%
  group_by(surface_problem_grades) %>%
  tally() %>%
  mutate(perc = n / sum(n))

# surface_problem_grades     n   oerc
# <chr>                  <int>  <dbl>
# 1 0.00 - 0.10              112 0.0947
# 2 0.11 - 0.20               56 0.0473
# 3 0.21 - 0.30               68 0.0575
# 4 0.31 - 0.40               60 0.0507
# 5 0.41 - 0.50              113 0.0955
# 6 0.51 - 0.60               80 0.0676
# 7 0.61 - 0.70               79 0.0668
# 8 0.71 - 0.80              138 0.117
# 9 0.81 - 0.90               72 0.0609
# 10 0.91 - 1.00              405 0.342
# > 0.117 +0.0609+0.342
# [1] 0.5199 <- 52% das vezes, a proporção ficou acima de 70%
# > 0.117 +0.0609+0.342+0.0668+0.0676
# [1] 0.6543 <- 65% das vezes, marcou acima de 50%


# Quando Claros marcou buraco, como o surface_problem marcou?
meios_quadra %>%
  st_drop_geometry() %>%
  filter(flag_buracos) %>%
  select(surfaceproblem, surfaceproblem_prop, flag_bur_sup) %>%
  mutate(surface_problem_grades = case_when(between(surfaceproblem_prop, 0.00, 0.10) ~ '0.00 - 0.10',
                                            between(surfaceproblem_prop, 0.11, 0.20) ~ '0.11 - 0.20',
                                            between(surfaceproblem_prop, 0.21, 0.30) ~ '0.21 - 0.30',
                                            between(surfaceproblem_prop, 0.31, 0.40) ~ '0.31 - 0.40',
                                            between(surfaceproblem_prop, 0.41, 0.50) ~ '0.41 - 0.50',
                                            between(surfaceproblem_prop, 0.51, 0.60) ~ '0.51 - 0.60',
                                            between(surfaceproblem_prop, 0.61, 0.70) ~ '0.61 - 0.70',
                                            between(surfaceproblem_prop, 0.71, 0.80) ~ '0.71 - 0.80',
                                            between(surfaceproblem_prop, 0.81, 0.90) ~ '0.81 - 0.90',
                                            between(surfaceproblem_prop, 0.91, 1.00) ~ '0.91 - 1.00',
                                            # between(surfaceproblem_prop, 0.25, 0.49) ~ '0.25 - 0.49',
                                            # between(surfaceproblem_prop, 0.50, 0.74) ~ '0.50 - 0.74',
                                            # between(surfaceproblem_prop, 0.75, 1.00) ~ '0.75 - 1.00'
  )) %>%
  group_by(surface_problem_grades) %>%
  tally() %>%
  mutate(perc = n / sum(n))
# surface_problem_grades     n   perc
# <chr>                  <int>  <dbl>
# 1 0.00 - 0.10               13 0.137
# 2 0.11 - 0.20                9 0.0947
# 3 0.21 - 0.30                9 0.0947
# 4 0.31 - 0.40               11 0.116
# 5 0.41 - 0.50               10 0.105
# 6 0.51 - 0.60               10 0.105
# 7 0.61 - 0.70                2 0.0211
# 8 0.71 - 0.80                8 0.0842
# 9 0.81 - 0.90                3 0.0316
# 10 0.91 - 1.00               20 0.211
# > 0.211+0.0316+0.0842
# [1] 0.3268 <- 32,6% das vezes, proporção ficou acima de 70%
# > 0.211+0.0316+0.0842+0.0211+0.105
# [1] 0.4529 <- 45,3% das vezes, proporção ficou acima de 50%


# Quando Claros marcou problema de superfície, como o surface_problem marcou?
meios_quadra %>%
  st_drop_geometry() %>%
  filter(flag_superficie) %>%
  select(surfaceproblem, surfaceproblem_prop, flag_bur_sup) %>%
  mutate(surface_problem_grades = case_when(between(surfaceproblem_prop, 0.00, 0.10) ~ '0.00 - 0.10',
                                            between(surfaceproblem_prop, 0.11, 0.20) ~ '0.11 - 0.20',
                                            between(surfaceproblem_prop, 0.21, 0.30) ~ '0.21 - 0.30',
                                            between(surfaceproblem_prop, 0.31, 0.40) ~ '0.31 - 0.40',
                                            between(surfaceproblem_prop, 0.41, 0.50) ~ '0.41 - 0.50',
                                            between(surfaceproblem_prop, 0.51, 0.60) ~ '0.51 - 0.60',
                                            between(surfaceproblem_prop, 0.61, 0.70) ~ '0.61 - 0.70',
                                            between(surfaceproblem_prop, 0.71, 0.80) ~ '0.71 - 0.80',
                                            between(surfaceproblem_prop, 0.81, 0.90) ~ '0.81 - 0.90',
                                            between(surfaceproblem_prop, 0.91, 1.00) ~ '0.91 - 1.00',
                                            # between(surfaceproblem_prop, 0.25, 0.49) ~ '0.25 - 0.49',
                                            # between(surfaceproblem_prop, 0.50, 0.74) ~ '0.50 - 0.74',
                                            # between(surfaceproblem_prop, 0.75, 1.00) ~ '0.75 - 1.00'
  )) %>%
  group_by(surface_problem_grades) %>%
  tally() %>%
  mutate(perc = n / sum(n))
# surface_problem_grades     n   perc
# <chr>                  <int>  <dbl>
# 1 0.00 - 0.10               99 0.0910
# 2 0.11 - 0.20               47 0.0432
# 3 0.21 - 0.30               59 0.0542
# 4 0.31 - 0.40               49 0.0450
# 5 0.41 - 0.50              103 0.0947
# 6 0.51 - 0.60               70 0.0643
# 7 0.61 - 0.70               77 0.0708
# 8 0.71 - 0.80              130 0.119
# 9 0.81 - 0.90               69 0.0634
# 10 0.91 - 1.00              385 0.354
# > 0.354+0.0634+0.119
# [1] 0.5364 <- 53,6% das vezes, proporção ficou acima de 70%
# > 0.354+0.0634+0.119+0.0708+0.0643
# [1] 0.6715 <- 67% das vezes, proporção ficou acima de 70%


# Quando o algoritmo marcou acima de 0.5, como Claros marcou?
meios_quadra %>%
  st_drop_geometry() %>%
  filter(surfaceproblem_prop > 0.5) %>%
  select(surfaceproblem, surfaceproblem_prop, flag_bur_sup) %>%
  group_by(flag_bur_sup) %>%
  tally() %>%
  mutate(perc = n / sum(n))
# flag_bur_sup     n  perc
# <lgl>        <int> <dbl>
# 1 FALSE         1212 0.610
# 2 TRUE           774 0.390 <- 39% coincide com Claros


meios_quadra %>%
  st_drop_geometry() %>%
  filter(surfaceproblem_prop > 0.7) %>%
  select(surfaceproblem, surfaceproblem_prop, flag_bur_sup) %>%
  group_by(flag_bur_sup) %>%
  tally() %>%
  mutate(perc = n / sum(n))
# flag_bur_sup     n  perc
# <lgl>        <int> <dbl>
# 1 FALSE          825 0.573
# 2 TRUE           615 0.427

meios_quadra %>%
  st_drop_geometry() %>%
  filter(surfaceproblem_prop > 0.8) %>%
  select(surfaceproblem, surfaceproblem_prop, flag_bur_sup) %>%
  group_by(flag_bur_sup) %>%
  tally() %>%
  mutate(perc = n / sum(n))
# flag_bur_sup     n  perc
# <lgl>        <int> <dbl>
# 1 FALSE          544 0.533
# 2 TRUE           477 0.467

meios_quadra %>%
  st_drop_geometry() %>%
  filter(surfaceproblem_prop > 0.9) %>%
  select(surfaceproblem, surfaceproblem_prop, flag_bur_sup) %>%
  group_by(flag_bur_sup) %>%
  tally() %>%
  mutate(perc = n / sum(n))
# flag_bur_sup     n  perc
# <lgl>        <int> <dbl>
# 1 FALSE          427 0.513
# 2 TRUE           405 0.487