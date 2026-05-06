# Exporta PDFs dos problemas encontrados na auditoria, tendo a flag de problemas
# de superfície e/ou buracos (análise manual) como base
# O script está pensado para ser rodado após as imagens serem publicadas no
# Mapillary e depois baixadas, para que o link de navegação remeta à plataforma

# Se quarto gerar somente arquivos HTML em vez de PDF, ver:
# https://stackoverflow.com/questions/75899982/quarto-in-rstudio-unable-to-render-document-as-pdf
# tinytex::install_tinytex() # para instalar
# tinytex::is_tinytex() # testar, deve ser igual a TRUE


library('tidyverse')
library('tidylog')
library('sf')
library('tinytex')
library('quarto')
library('readxl')


# Estrutura de pastas
# pasta_base  <- '/media/livre/Expansion/projetos/2025_Auditoria_Calcadas'
pasta_base  <- '/mnt/fern/Dados/projetos/2025_Auditoria_Calcadas'
pasta_proc  <- sprintf('%s/01_dados_processados', pasta_base)
pasta_analises <- sprintf('%s/05_resultados', pasta_proc)
pasta_publi    <- sprintf('%s/06_publicacao', pasta_proc)
pasta_pdfs     <- sprintf('%s/PDFs', pasta_publi)
pasta_travs    <- sprintf('%s/travessias', pasta_pdfs)
pasta_l_pub    <- sprintf('%s/lotes_alcada_publica', pasta_pdfs)
pasta_l_priv   <- sprintf('%s/lotes_privados', pasta_pdfs)
pasta_l_priv_buracos <- sprintf('%s/buracos', pasta_l_priv)
dir.create(pasta_travs, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_l_pub, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_l_priv_buracos, recursive = TRUE, showWarnings = FALSE)


# Resultados da auditoria, filtrados por keyframe
dados <- sprintf('%s/auditoria_calcadas_bras.gpkg', pasta_analises)
dados <- read_sf(dados)

# Shape de fotos baixadas do Mapillary, após a publicação - esse shape vai ter
# o ID da foto no Mapillary, necessário para construir o link de visualização
fotos_mapillary <- file.path(pasta_publi, 'shapes_base/fotos_auditoria.gpkg')
fotos_mapillary <- read_sf(fotos_mapillary) %>% select(id)

# Pedido de LAI à SMSUB - Atualizão dos lotes marcacos como readeqaudos no PEC
# Calçadas -- nenhuma está no perímetro de interesse
# lai_smsub <- sprintf('%s/94817_ANEXO E-SIC 94817.xlsx', pasta_publi)
# lai_smsub <- read_excel(lai_smsub, skip = 1)
# lai_smsub %>% filter(str_starts(Quadra, 'F') & SUB == 'MO')


# ------------------------------------------------------------------------------
# Isolar problemas em meios de quadra
# ------------------------------------------------------------------------------

# dados %>% st_drop_geometry() %>% select(group_id) %>% distinct()
quadras <- dados %>%
  filter(kf_flag & quadra_flag_bur_sup) %>%
  select(kf_group_id, frames = kf_video_duration,
         quadra_flag_buraco,
         lo_tp_lote, cc_pec, cc_situac, lote_alcada_publica, lote_tp_prop,
         testada_m, n_contrib, n_cond,
         imagepath,
         codlog, logradouro, numero, cep,
         geom) %>%
  mutate(lado = case_when(as.integer(numero) %% 2 == 0 ~ 'Lado Par',
                          as.integer(numero) %% 2 == 1 ~ 'Lado Ímpar',
                          TRUE ~ ''),
         .after = 'numero') %>%
  mutate(
    # imagepath = str_c('/media/livre/Expansion/projetos/2025_Auditoria_Calcadas/01_dados_processados/', imagepath)
    imagepath = str_c('/mnt/fern/Dados/projetos/2025_Auditoria_Calcadas/01_dados_processados/', imagepath)
  ) %>%
  arrange(kf_group_id)


# quadras %>% st_drop_geometry() %>% select(frames) %>% summary()
# frames
# Min.   :  5.000
# 1st Qu.:  5.000
# Median :  5.000
# Mean   :  9.253
# 3rd Qu.:  8.000
# Max.   :178.000
# quadras %>% st_drop_geometry() %>% select(testada_m) %>% mutate(testada_m = round(as.double(testada_m))) %>% summary()
# testada_m
# Min.   :  0.00
# 1st Qu.:  5.00
# Median :  6.00
# Mean   : 11.39
# 3rd Qu.: 10.00
# Max.   :320.00
# NA's   :10

quadras <- quadras %>%
  # Lote é do PEC Calçadas?
  mutate(cc_pec = ifelse(!is.na(cc_pec), 'SIM', 'NÃO'),
         # Se é do PEC, já foi readequada?
         cc_situac = ifelse(!is.na(cc_situac), 'SIM', 'NÃO'),
         # Lote é municipal?
         lo_tp_lote = ifelse(lo_tp_lote == 'M', 'SIM', 'NÃO'),
         # Propriedade do lote segundo a SEFAZ
         # lo_tp_lote
         # Lote é de alçada pública?
         lote_alcada_publica = ifelse(lote_alcada_publica == TRUE, 'SIM', 'NÃO')
         )


# Puxar ID da foto no Mapillary, a partir do latlon
quadras <- quadras %>% st_join(fotos_mapillary) %>% relocate(id, .before = 'kf_group_id')
# Somente em três fotos não pegou o id
# quadras %>% filter(!is.na(id))


for (i in seq_len(nrow(quadras))) {
  # i <- 1; row <- quadras[i, ]
  row <- quadras[i, ]

  # # Criar segunda imagem, alguns segundos para a frente
  # img_number <- str_extract(row$imagepath, '\\d{5}_ms')
  # new_number <- sprintf('%05d_ms', as.integer(str_extract(img_number, '\\d{5}')) + 3)
  # row <- row %>% mutate(imagepath2 = str_replace(imagepath, img_number, new_number))
  # # row$imagepath
  # # row$imagepath2

  out_file <- sprintf("auditoria_cidada_id_%04d.pdf", row$kf_group_id)

  quarto_render(
    input = "quarto/feature_report_1.qmd",
    output_file = out_file,
    execute_params = list(
      title = paste("**Problema de superfície ou buraco** - ID Imagem", sprintf("%04d", row$kf_group_id)),
      text_blocks = list(
        paste0("."),
        paste0("**Lote é de alçada pública?** ", row$lote_alcada_publica),
        paste0("- **Pertence ao PEC Calçadas?** ", row$cc_pec),
        paste0("- **Adequado PEC (Geosampa)?** ", row$cc_situac),
        paste0("- **Lote municipal (Geosampa)?** ", row$lo_tp_lote),
        paste0("- **Prop. pública (SEFAZ):** ", row$lote_tp_prop),
        paste0("."),
        paste0("**SQL:** ", row$n_contrib, " | **Cond:** ", row$n_cond, " | **CODLOG:** ", row$codlog, " | **CEP:** ", row$cep),
        paste0("**Logradouro:** ", str_trim(str_squish(row$logradouro)), ", ", row$lado, ", Altura N° ", as.integer(row$numero)),
        paste0("**LAT:** ", str_sub(st_coordinates(row$geom)[, 2], 1, 9), ", **LON:** ", str_sub(st_coordinates(row$geom)[, 1], 1, 9)),
        paste0("**Links:**
        [Localização no Mapa](https://www.mapillary.com/app/user?lat=",
               st_coordinates(row$geom)[, 2],
               "&lng=",
               st_coordinates(row$geom)[, 1],
               "&z=18&menu=false&dateFrom=2025-11-08&dateTo=2025-11-17&username%5B%5D=gabinete_falzoni&&pKey=",
               row$id,
               ") |  [Ver no Street View - Mapillary](https://www.mapillary.com/app/user/gabinete_falzoni?lat=",
               st_coordinates(row$geom)[, 2],
               "&lng=",
               st_coordinates(row$geom)[, 1],
               "&z=18&menu=false&dateFrom=2025-11-08&dateTo=2025-11-17&username%5B%5D=gabinete_falzoni&pKey=",
               row$id,
               "&focus=photo)"),
        # paste0("**Links:**
        # [Localização no Google Maps](https://www.google.com/maps?q=",
        #        st_coordinates(row$geom)[, 2], ",",
        #        st_coordinates(row$geom)[, 1],
        #        ") |  [Ver no Google Street View](http://maps.google.com/?cbll=",
        #        st_coordinates(row$geom)[, 2], ",",
        #        st_coordinates(row$geom)[, 1],
        #        "&cbp=12,20.09,,0,5&layer=c)"),
        paste0(" ")
      ),
      imagepath = row$imagepath

    )
  )

  # Mover arquivo PDF para a pasta de saída
  file.rename(file.path("quarto", out_file),
              file.path(pasta_pdfs, out_file)
              )
}


# Redistribuir PDFs entre pastas de lotes públicos e lotes privados
for (i in seq_len(nrow(quadras))) {
  # row <- quadras[1, ]
  row <- quadras[i, ]

  in_file <- sprintf("auditoria_cidada_id_%04d.pdf", row$kf_group_id)
  if (row$lote_alcada_publica == 'SIM') {
    file.rename(file.path(pasta_pdfs, in_file),
                file.path(pasta_l_pub, in_file)
    )
  } else {
    file.rename(file.path(pasta_pdfs, in_file),
                file.path(pasta_l_priv, in_file)
    )
  }
}


# Deixar marcações de buracos em lotes privados em pasta própria
for (i in seq_len(nrow(quadras))) {
  # row <- quadras[1, ]
  row <- quadras[i, ]

  in_file <- sprintf("auditoria_cidada_id_%04d.pdf", row$kf_group_id)
  if (row$quadra_flag_buraco) {
    file.rename(file.path(pasta_l_priv, in_file),
                file.path(pasta_l_priv_buracos, in_file)
    )
  }
}


# ------------------------------------------------------------------------------
# Isolar problemas em travessias
# ------------------------------------------------------------------------------

travessias <- dados %>%
  filter(trav_group_id & trav_horizontal & descartar == FALSE) %>%
  select(point_id,
         trav_group_id, matches('^trav_'),
         testada_m, n_contrib, n_cond,
         imagepath,
         codlog, logradouro, numero, cep,
         geom) %>%
  mutate(
    imagepath = str_c('/media/livre/Expansion/projetos/2025_Auditoria_Calcadas/01_dados_processados/', imagepath)
  ) %>%
  arrange(trav_group_id)


# travessias %>% st_drop_geometry() %>% group_by(trav_group_id) %>% tally() %>% select(n) %>% summary()
# n
# Min.   : 5.00
# 1st Qu.:15.00
# Median :17.00
# Mean   :17.43
# 3rd Qu.:19.00
# Max.   :42.00

# Para puxar um endereço próximo, vamos pegar essas infos do primeiro frame do grupo
enderecos <- travessias %>%
  st_drop_geometry() %>%
  group_by(trav_group_id) %>%
  summarise(logradouro = first(logradouro),
            numero = first(numero),
            cep = first(cep)) %>%
  ungroup() %>%
  mutate(lado = case_when(as.integer(numero) %% 2 == 0 ~ 'Lado Par',
                          as.integer(numero) %% 2 == 1 ~ 'Lado Ímpar',
                          TRUE ~ ''),
         .after = 'numero')


# Puxar só o keyframe da travessia
travessias <- travessias %>%
  select(-c(logradouro, numero, cep)) %>%
  group_by(trav_group_id) %>%
  mutate(row_number = row_number()) %>%  # Add a row number for each group
  filter(row_number == ceiling(n() / 2)) %>%  # Get the middle row
  select(-row_number)  # Optionally remove the row_number column


travessias <- travessias %>% left_join(enderecos, by = 'trav_group_id')

# Manter somente travessias com algum tipo de problema
travessias <- travessias %>%
  filter(trav_rampa == FALSE | trav_inadequada | trav_reparar_rampa | trav_repintar_horiz | trav_reparar_pavim)


# Mudar booleans para texto, para facilitar leitura
travessias <- travessias %>%
  mutate(trav_rampa          = ifelse(trav_rampa, 'SIM', 'NÃO'),
         trav_inadequada     = ifelse(trav_inadequada, 'SIM', 'NÃO'),
         trav_reparar_rampa  = ifelse(trav_reparar_rampa, 'SIM', 'NÃO'),
         trav_repintar_horiz = ifelse(trav_repintar_horiz, 'SIM', 'NÃO'),
         trav_reparar_pavim  = ifelse(trav_reparar_pavim, 'SIM', 'NÃO'),
           )



for (i in seq_len(nrow(travessias))) {
  # row <- travessias[1, ]
  row <- travessias[i, ]

  # Criar segunda imagem, alguns segundos para a trás e para a frente
  img_number <- str_extract(row$imagepath, '\\d{5}_ms')

  img_number_0 <- as.integer(str_extract(img_number, '\\d{5}'))
  if (img_number_0 > 7) {
    img_number_0 <- sprintf('%05d_ms', img_number_0 - 7)
  } else if (between(img_number_0, 4, 7)) {
    img_number_0 <- sprintf('%05d_ms', img_number_0 - 4)
  } else {
    img_number_0 <- sprintf('%05d_ms', img_number_0)
  }

  img_number_1 <- as.integer(str_extract(img_number, '\\d{5}'))
  if (img_number_1 > 4) {
    img_number_1 <- sprintf('%05d_ms', img_number_1 - 4)
  } else {
    img_number_1 <- sprintf('%05d_ms', img_number_1)
  }

  img_number_3 <- sprintf('%05d_ms', as.integer(str_extract(img_number, '\\d{5}')) + 3)

  row <- row %>% mutate(imagepath_0 = str_replace(imagepath, img_number, img_number_0),
                        imagepath_1 = str_replace(imagepath, img_number, img_number_1),
                        imagepath_2 = imagepath,
                        imagepath_3 = str_replace(imagepath, img_number, img_number_3),
                        )
  # row$imagepath
  # row$imagepath2

  out_file <- sprintf("auditoria_cidada_travessias_id_%04d.pdf", row$trav_group_id)

  quarto_render(
    input = "quarto/feature_report_2.qmd",
    output_file = out_file,
    execute_params = list(
      title = paste("**Problema em travessia sinalizada** - ID Imagem", sprintf("%04d", row$trav_group_id)),
      text_blocks = list(
        # paste0("**Surface problem (gradação 0.0 a 1.0):** ", row$surfaceproblem_prop, " (", row$frames, " frames)"),
        # paste0("**SQL:** ", row$n_contrib, " | **Cond:** ", row$n_cond, " | **CODLOG:** ", row$codlog, " | **CEP:** ", row$cep),
        paste("**ATENÇÂO: Se problemas não aparecerem nas imagens, ver via links Google abaixo**"),
        paste0("**Logradouro:** ", str_trim(str_squish(row$logradouro)), ", ", row$lado, ", Altura N° ", as.integer(row$numero), " | **CEP:** ", row$cep),
        # paste0("**LAT:** ", str_sub(st_coordinates(row$geom)[, 2], 1, 9), ", **LON:** ", str_sub(st_coordinates(row$geom)[, 1], 1, 9)),

        paste0("**Possui rampa(s)?** ", row$trav_rampa, " | **Rampa(s) inadequada(s)?** ", row$trav_inadequada, " | **Reparar rampa(s)?** ", row$trav_reparar_rampa),
        paste0("**Repintar faixa de pedestres?** ", row$trav_repintar_horiz, " | **Reparar pavimento?** ", row$trav_reparar_pavim),
        paste0("\n\n"),
        paste0("**Links:**
        [Localização no Mapa](https://www.mapillary.com/app/user?lat=",
               st_coordinates(row$geom)[, 2],
               "&lng=",
               st_coordinates(row$geom)[, 1],
               "&z=18&menu=false&dateFrom=2025-11-08&dateTo=2025-11-17&username%5B%5D=gabinete_falzoni&&pKey=",
               row$id,
               ") |  [Ver no Street View - Mapillary](https://www.mapillary.com/app/user/gabinete_falzoni?lat=",
               st_coordinates(row$geom)[, 2],
               "&lng=",
               st_coordinates(row$geom)[, 1],
               "&z=18&menu=false&dateFrom=2025-11-08&dateTo=2025-11-17&username%5B%5D=gabinete_falzoni&pKey=",
               row$id,
               "&focus=photo)"),
        # paste0("**Links:**
        # [Localização no Google Maps](https://www.google.com/maps?q=",
        #        st_coordinates(row$geom)[, 2], ",",
        #        st_coordinates(row$geom)[, 1],
        #        ") |  [Ver no Google Street View](http://maps.google.com/?cbll=",
        #        st_coordinates(row$geom)[, 2], ",",
        #        st_coordinates(row$geom)[, 1],
        #        "&cbp=12,20.09,,0,5&layer=c)"),
        paste0(" ")
      ),
      imagepath_0 = row$imagepath_0,
      imagepath_1 = row$imagepath_1,
      imagepath_2 = row$imagepath_2,
      imagepath_3 = row$imagepath_3
    ),
    quiet = TRUE
  )

  # Mover arquivo PDF para a pasta de saída
  file.rename(file.path("quarto", out_file),
              file.path(pasta_travs, out_file)
  )
}
