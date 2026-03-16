library('tidyverse')
library('tidylog')


# Estrutura de pastas
# pasta_base <- '/media/livre/Expansion'
pasta_base <- '/mnt/fern/Dados'
pasta_audi <- sprintf('%s/projetos/2025_Auditoria_Calcadas', pasta_base)
pasta_proc <- sprintf('%s/01_dados_processados', pasta_audi)
pasta_analises <- sprintf('%s/04_analises', pasta_proc)
foto_lotes <- sprintf('%s/fotos_resumo_por_lote', pasta_analises)
dir.create(foto_lotes, showWarnings = FALSE, recursive = TRUE)

pontos_lotes <- sprintf('%s/resumo_pontos_por_lote.gpkg', pasta_analises)
pontos_lotes <- read_sf(pontos_lotes)
pontos_lotes <- pontos_lotes %>% st_drop_geometry() %>% select(imagepath) %>% arrange(imagepath)

pontos_lotes <-
  pontos_lotes %>%
  mutate(imagepath = str_c('/mnt/fern/Dados/projetos/2025_Auditoria_Calcadas/01_dados_processados/', imagepath))


for (i in pontos_lotes$imagepath) {
  # i <- pontos_lotes$imagepath[1]
  out_file <- sprintf('%s/%s', foto_lotes, basename(i))
  file.copy(i, to = out_file)
}

pontos_lotes %>% filter(is.na(imagepath))