library('tidyverse')
library('tidylog')
library('readxl')

pasta_base  <- '/mnt/fern/Dados/projetos/2025_Auditoria_Calcadas'
pasta_proc  <- sprintf('%s/01_dados_processados', pasta_base)
pasta_analises <- sprintf('%s/04_analises/sample_pdfs', pasta_proc)

# sample_dados <- sprintf('%s/sample_dados_completo.csv', pasta_analises)
# sample_dados <- read_delim(sample_dados, delim = ';', col_types = 'iidcccc')
# sample_dados <- sample_dados %>% select(group_id, frames, sf_prop = surfaceproblem_prop)
sample_dados <- sprintf('%s/sample_dados_estrat_balanceada.csv', pasta_analises)
sample_dados <- read_delim(sample_dados, delim = ';', col_types = 'iiddcccccccccci')
sample_dados <- sample_dados %>% select(group_id, frames, sf_prop, cat_frames, nota_ime, imagepath)
sample_dados <- sample_dados %>% arrange(group_id)
# sample_dados %>% group_by(nota_ime) %>% tally()

analise <- sprintf('%s/Auditoria Calçadas - Teste acurácia surface problem.xlsx', pasta_analises)
analise <- read_excel(analise, sheet = 'sample_dados_estrat_balanceada_')
# names(analise) <- c('group_id', 'nota_claros', 'nada', 'obs')
names(analise) <- c('group_id', 'nota_claros', 'nada', 'obs_1', 'obs_2', 'obs_3')
analise <- analise %>% select(-nada) %>% filter(!is.na(group_id))


# Claros deu mais estrelas conforme as calçadas estavam melhores - é o oposto
# do algoritmo, que detectou mais frames caso as calçadas estavam piores. Vamos
# ajustar isso revisando a escala do algoritmo do IME
sample_dados <- sample_dados %>% mutate(nota_ime = case_when(sf_prop >= 0.0 & sf_prop <= 0.2 ~ 5,
                                                             sf_prop >  0.2 & sf_prop <= 0.4 ~ 4,
                                                             sf_prop >  0.4 & sf_prop <= 0.6 ~ 3,
                                                             sf_prop >  0.6 & sf_prop <= 0.8 ~ 2,
                                                             sf_prop >  0.8 & sf_prop <= 1 ~ 1,
                                                             TRUE ~ NA))

result <- sample_dados %>% left_join(analise, by = 'group_id')
sample_n(result, 20)

result %>% select(frames) %>% summary()
# frames
# Min.   :  5.00
# 1st Qu.:  5.00
# Median :  6.00
# Mean   : 11.72
# 3rd Qu.: 12.00
# Max.   :178.00
# result <- result %>% mutate(cat_frames = case_when(frames <= 6  ~ '1_mediana_6f',
#                                                    frames > 6 & frames <= 12 ~ '2_media_2x_6f_12f',
#                                                    frames > 12 & frames <= 50 ~ '3_longa_12f_50f',
#                                                    frames > 50 ~ '4_muito_longa_50f'))

# frames
# Min.   :  5.00
# 1st Qu.:  5.75
# Median : 10.50
# Mean   : 27.70
# 3rd Qu.: 49.50
# Max.   :187.00

result %>% mutate(
  dif_ime_claros = nota_ime - nota_claros) %>%
  group_by(dif_ime_claros) %>%
  tally() %>%
  mutate(perc = scales::percent(n / sum(n)))
# dif_ime_claros     n perc
# <dbl> <int> <chr>
# 1             -4     2 0.8%
# 2             -3    22 8.8%
# 3             -2    40 16.0%
# 4             -1    51 20.4%
# 5              0    89 35.6%
# 6              1    43 17.2%
# 7              2     2 0.8%
# 8              3     1 0.4%

# dif_ime_claros     n perc
# <dbl> <int> <chr>
# 1             -4    12 4.0%
# 2             -3    43 14.3%
# 3             -2    63 21.0%
# 4             -1    66 22.0%
# 5              0    82 27.3%
# 6              1    32 10.7%
# 7              2     2 0.7%


result %>% mutate(
  dif_ime_claros = nota_ime - nota_claros) %>%
  group_by(cat_frames, dif_ime_claros) %>%
  tally() %>%
  mutate(perc = scales::percent(n / sum(n))) %>%
  ungroup() %>%
  filter(!cat_frames %in% c('1_mediana_5f'))



result %>%
  mutate(dif_ime_claros = nota_ime - nota_claros) %>%
  filter(!str_detect(obs_1, 'Ladrilho')) %>%
  filter(is.na(obs_2) | !str_detect(obs_2, 'Ladrilho')) %>%
  filter(dif_ime_claros <= -3) %>%
  sample_n(10) %>%
  select(group_id, frames, sf_prop, nota_ime, nota_claros) %>%
  arrange(group_id) %>%
  slice(11:33)

result %>%
  # group_by(obs_1) %>% tally()
  filter(!str_detect(obs_1, 'Ladrilho')) %>%
  filter(is.na(obs_2) | !str_detect(obs_2, 'Ladrilho')) %>%
  mutate(dif_ime_claros = nota_ime - nota_claros) %>%
  group_by(dif_ime_claros) %>%
  tally() %>%
  mutate(perc = scales::percent(n / sum(n)))

result %>% group_by(cat_frames) %>% tally() %>% mutate(perc = scales::percent(n / sum(n)))
# cat_frames            n perc
# <chr>             <int> <chr>
# 1 1_mediana_6f        138 55.2%
# 2 2_media_2x_6f_12f    50 20.0%
# 3 3_longa_12f_50f      57 22.8%
# 4 4_muito_longa_50f     5 2.0%

result %>% mutate(
  dif_ime_claros = nota_ime - nota_claros) %>%
  group_by(cat_frames, dif_ime_claros) %>%
  tally() %>%
  mutate(perc = scales::percent(n / sum(n))) %>%
  ungroup() %>%
  filter(str_starts(cat_frames, '4_'))
# cat_frames   dif_ime_claros     n perc
# <chr>                 <dbl> <int> <chr>
# 1 1_mediana_6f             -3     7 5.1%
# 2 1_mediana_6f             -2    21 15.2%
# 3 1_mediana_6f             -1    23 16.7%
# 4 1_mediana_6f              0    57 41.3%
# 5 1_mediana_6f              1    28 20.3%
# 6 1_mediana_6f              2     1 0.7%
# 7 1_mediana_6f              3     1 0.7%
# 1 2_media_2x_6f_12f             -3     8 16%
# 2 2_media_2x_6f_12f             -2     7 14%
# 3 2_media_2x_6f_12f             -1    10 20%
# 4 2_media_2x_6f_12f              0    14 28%
# 5 2_media_2x_6f_12f              1    10 20%
# 6 2_media_2x_6f_12f              2     1 2%
# 1 3_longa_12f_50f             -4     2 3.5%
# 2 3_longa_12f_50f             -3     7 12.3%
# 3 3_longa_12f_50f             -2    10 17.5%
# 4 3_longa_12f_50f             -1    18 31.6%
# 5 3_longa_12f_50f              0    15 26.3%
# 6 3_longa_12f_50f              1     5 8.8%
# 1 4_muito_longa_50f             -2     2 40%
# 2 4_muito_longa_50f              0     3 60%