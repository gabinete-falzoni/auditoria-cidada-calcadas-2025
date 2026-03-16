library('tidyverse')
library('tidylog')
library('sf')
library('ggplot2')
library('ggspatial')
library('patchwork')



# Estrutura de pastas
# pasta_base  <- '/mnt/fern/Dados/2025_Auditoria_Calcadas/pinheiros'
pasta_base  <- '/media/livre/Expansion/projetos/2025_Auditoria_Calcadas'
pasta_proc  <- sprintf('%s/01_dados_processados', pasta_base)
pasta_analises <- sprintf('%s/04_analises', pasta_proc)

dados <- sprintf('%s/resumo_pontos_por_lote.gpkg', pasta_analises)
dados <- read_sf(dados)
# dados %>% st_drop_geometry() %>% select(group_id) %>% distinct()
dados <- dados %>% select(group_id, frames = video_duration,
                          surfaceproblem_prop, testada_m,
                          n_contrib, n_cond,
                          imagepath,
                          codlog, logradouro, numero, cep,
                          geom) %>%
  mutate(lado = case_when(as.integer(numero) %% 2 == 0 ~ 'Lado Par',
                          as.integer(numero) %% 2 == 1 ~ 'Lado Ímpar',
                          TRUE ~ ''),
         .after = 'numero')


# sample_dados <- dados %>% sample_n(2)
# sample_dados %>%
#   st_drop_geometry() %>%
#   mutate(imagepath2 = str_c('/mnt/fern/Dados/projetos/2025_Auditoria_Calcadas/01_dados_processados/', imagepath)) %>%
#   select(imagepath2) %>%
#   pull()
#
# sample_dados <- sample_dados %>%
#   mutate(imagepath = basename(imagepath)) %>%
#   mutate(
#   imagepath = str_c('/mnt/fern/Dados/gitlab/auditoria-cidada-calcadas-2025/R/testing/01_image_sequences/', imagepath)
# )
#
# sample_dados %>% st_drop_geometry() %>% select(imagepath) %>% pull()
#
# # Assume your .qmd file is in: /media/livre/Expansion/projetos/2025_Auditoria_Calcadas/01_dados_processados/R/testing/
# qmd_dir <- "/mnt/fern/Dados/gitlab/auditoria-cidada-calcadas-2025/R/testing/"
# sample_dados <- sample_dados %>%
#   mutate(image_relpath = gsub("_", "\\\\_",
#                               fs::path_rel(imagepath, start = qmd_dir)))
#
#
# sample_dados %>% st_drop_geometry() %>% select(image_relpath) %>% pull()

set.seed(123)
# sample_dados <- dados %>% sample_n(2)

dados %>% st_drop_geometry() %>% select(frames) %>% summary()
# frames
# Min.   :  5.00
# 1st Qu.:  5.00
# Median :  5.00
# Mean   : 10.26
# 3rd Qu.: 10.00
# Max.   :187.00
dados %>% st_drop_geometry() %>% select(testada_m) %>% mutate(testada_m = round(as.double(testada_m))) %>% summary()
# testada_m
# Min.   :  0.00
# 1st Qu.:  5.00
# Median :  6.00
# Mean   : 10.93
# 3rd Qu.: 10.00
# Max.   :320.00
# NA's   :370

# Classificar
sample_dados <- dados %>%
  rename(sf_prop = surfaceproblem_prop) %>%
  mutate(cat_frames = case_when(frames <= 5  ~ '1_mediana_5f',
                                frames > 5 & frames <= 10 ~ '2_media_2x_5f_10f',
                                frames > 10 & frames <= 50 ~ '3_longa_10f_50f',
                                frames > 50 ~ '4_muito_longa_50f')) %>%
  mutate(nota_ime = case_when(sf_prop >= 0.0 & sf_prop <= 0.2 ~ 5,
                              sf_prop >  0.2 & sf_prop <= 0.4 ~ 4,
                              sf_prop >  0.4 & sf_prop <= 0.6 ~ 3,
                              sf_prop >  0.6 & sf_prop <= 0.8 ~ 2,
                              sf_prop >  0.8 & sf_prop <= 1 ~ 1,
                              TRUE ~ NA))

# Amostragem estratificada balanceada - vamos querer pelo menos 15 ocorrência de
# cada uma das categorias: nota_ime e cat_frames
n_per_stratum <- 15
sample_dados <-
  sample_dados %>%
  group_by(nota_ime, cat_frames) %>%
  slice_sample(n = n_per_stratum) %>%
  # Se alguma das categorias não tiverem quantidade suficiente, usar o máximo possível
  # slice_sample(n = min(n(), n_per_stratum)) %>%
  ungroup()

sample_dados %>% st_drop_geometry() %>% group_by(nota_ime) %>% tally()
sample_dados %>% st_drop_geometry() %>% group_by(cat_frames) %>% tally()


# Atualizar endereço das imagens para absolute path
sample_dados <- sample_dados %>%
  mutate(
    imagepath = str_c('/mnt/fern/Dados/projetos/2025_Auditoria_Calcadas/01_dados_processados/', imagepath)
  )
sample_dados %>% st_drop_geometry() %>% select(imagepath) %>% pull()
sample_dados

# Gravar amostragem
out_file <- '/mnt/fern/Dados/gitlab/auditoria-cidada-calcadas-2025/R/testing/sample_dados_estrat_balanceada.csv'
write_delim(sample_dados, out_file, delim = ';')

