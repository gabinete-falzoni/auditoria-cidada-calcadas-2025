# Junta a base de lotes do GeoSampa à de IPTU, pegando dados de logradouro, número, CEP etc#

library('tidyverse')
library('tidylog')
library('sf')
library('mapview')
library('janitor')


pasta_base <- '/media/livre/Expansion'
pasta_iptu <- sprintf('%s/dados/Geosampa/CADASTRO/IPTU/IPTU_2025/', pasta_base)
pasta_proj <- sprintf('%s/projetos/2025_Auditoria_Calcadas/01_dados_processados/00_shapes_base', pasta_base)


# ------------------------------------------------------------------------------
# IPTU
# ------------------------------------------------------------------------------

# Base de dados do IPTU - contém dados de logradouro e número da rua
iptu <- list.files(pasta_iptu, pattern = '.csv$', full.names = TRUE)[1]
iptu <- read_delim(iptu, delim = ';', col_types = cols(.default = "c"))
iptu <- iptu %>% clean_names()

# sql vai ser a chave de conexão entre as bases
iptu <- iptu %>% mutate(sql = str_sub(numero_do_contribuinte, 1, 10), .before = 1)

# Simplificar dataframe de IPTU
iptu <- iptu %>% select(sql,
                        n_contrib = numero_do_contribuinte,
                        n_cond    = numero_do_condominio,
                        codlog    = codlog_do_imovel,
                        logradouro = nome_de_logradouro_do_imovel,
                        numero    = numero_do_imovel,
                        testada_m = testada_para_calculo,
                        esquinas  = quantidade_de_esquinas_frentes,
                        andares   = quantidade_de_pavimentos,
                        tipo_uso  = tipo_de_uso_do_imovel,
                        # complem   = complemento_do_imovel,
                        # bairro    = bairro_do_imovel,
                        cep       = cep_do_imovel,
                        # tipo_const = tipo_de_padrao_da_construcao,
                        # tipo_terreno = tipo_de_terreno
)

# # Remover trailing spaces da coluna de complemento
# iptu <- iptu %>%
#   mutate(complem = trimws(complem),
#          complem = ifelse(complem == '', as.character(NA), complem))

# # Ajustar formatos das colunas
# iptu <- iptu %>%
#   mutate(numero    = as.integer(numero),
#          testada_m = as.double(testada_m),
#          esquinas  = as.integer(esquinas),
#          andares   = as.integer(andares))


# ------------------------------------------------------------------------------
# Perímetro da auditoria
# ------------------------------------------------------------------------------

# Área de perímetro da auditoria
perimetro <- read_sf(sprintf('%s/lotes_perimetro_auditoria_geosampa.gpkg', pasta_proj))
# perimetro <- perimetro %>% select(-layer)
# mapview(perimetro)

# Formar coluna de sql, que vai ser a chave de conexão entre as bases
perimetro <-
  perimetro %>%
  mutate(sql = str_c(lo_setor, lo_quadra, lo_lote), .before = 1) %>%
  # select(sql, lo_condomi, geom) %>%
  select(-primaryindex) %>%
  arrange(sql, lo_condomi)

# Remover camadas que podem ter vindo do merge layers do QGIS
if ('layer' %in% names(perimetro)) {perimetro <-  perimetro %>% select(-layer) }
if ('path' %in% names(perimetro)) { perimetro <- perimetro %>% select(-path) }


# ------------------------------------------------------------------------------
# Junção das bases - condomínios devem ser tratados diferente de não-condomínios
# ------------------------------------------------------------------------------

# Lotes tipo “F” – Fiscal são unidades inscritas no cadastro fiscal (Cadastro de
# Contribuinte Imobiliário e com cobrança de Imposto Predial e Territorial Urbano
# – IPTU) e recebem uma numeração a partir do 0001 dentro de cada Quadra Fiscal.

# Lotes tipo “EL” (versão online) ou “M” (versão download) - Espaço Livre ou Lote
# Municipal são áreas públicas municipais tais como praças, canteiros, parques.

# Lotes tipo “V” – Vilas são áreas de circulação internas à vilas.

# Quando condomínio é '00-0', o lote do sql é diferente de '0000':
# sql        lo_condomi
# <chr>      <chr>
# 1 0020010002 00
# 2 0020010003 00
# 3 0020010004 00
# 4 0020020001 00
# 5 0020030000 01

# Não condomínios - Associação é direta com os dados de IPTU via sql. Aqui, vamos
# considerar os lotes tanto fiscais quanto os municipais - praças, canteiros e
# parques. Podemos ignorar os lotes de vila
perimetro_cond_00 <- perimetro %>% filter(lo_condomi == '00') %>% filter(lo_tp_lote != 'V')
# Condomínios - Associação é mais complicada
perimetro_cond_XX <- perimetro %>% filter(lo_condomi != '00') %>% filter(lo_tp_lote == 'F')


# Associar lotes fiscais não condomínio aos dados de IPTU
# Lotes fiscais que ficam sem associação de logradouro e número: 97
perimetro_cond_00 <- perimetro_cond_00 %>% left_join(iptu, by = 'sql') %>% mutate(numero = as.integer(numero))
# perimetro_cond_00 %>% st_drop_geometry() %>% filter(is.na(n_contrib)) %>% select(1:8)



# Criar coluna temporária de junção: setor + quadra + condomínio
iptu2 <-
  iptu %>%
  mutate(numero = as.integer(numero)) %>%
  mutate(sq_cond = str_c(str_sub(sql, 1, 6), str_sub(n_cond, 1, 2), sep = '_'), .before = 1)

# Identificar números relativos aos lotes (menor, maior e todos juntos)
perimetro_cond_XX <-
  perimetro_cond_XX %>%
  # st_drop_geometry() %>%
  # Coluna temporária de junção: setor + quadra + condomínio
  mutate(sq_cond = str_c(str_sub(sql, 1, 6), str_sub(lo_condomi, 1, 2), sep = '_'), .before = 1) %>%
  # Remover sql para não ficar duplicado ao fazer o left_join()
  select(-sql) %>%
  left_join(iptu2, by = 'sq_cond') %>%
  # Todas as colunas de dados vão ter o sq_cond como base. De todas, vamos puxar
  # todos os valores presentes e ordenados, para que fiquem no shapefile de
  # saída. Nos casos em que há um valor só, ele fica único na coluna (ex. os
  # valores de codlog, esquinas, andares etc são únicos por condomínio)
  group_by(sq_cond) %>%
  summarise(sql        = paste(sort(unique(sql)), collapse = ", "),
            lo_setor   = paste(sort(unique(lo_setor)), collapse = ", "),
            lo_quadra  = paste(sort(unique(lo_quadra)), collapse = ", "),
            lo_lote    = paste(sort(unique(lo_lote)), collapse = ", "),
            lo_condomi = paste(sort(unique(lo_condomi)), collapse = ", "),
            lo_tp_quad = paste(sort(unique(lo_tp_quad)), collapse = ", "),
            lo_tp_lote = paste(sort(unique(lo_tp_lote)), collapse = ", "),
            n_contrib  = paste(sort(unique(n_contrib)), collapse = ", "),
            n_cond     = paste(sort(unique(n_cond)), collapse = ", "),
            codlog     = paste(sort(unique(codlog)), collapse = ", "),
            logradouro = paste(sort(unique(logradouro)), collapse = ", "),
            testada_m  = mean(as.double(testada_m)),
            testada_val = paste(sort(unique(testada_m)), collapse = ", "),
            esquinas   = paste(sort(unique(esquinas)), collapse = ", "),
            andares    = max(andares),
            cep        = paste(sort(unique(cep)), collapse = ", "),
            # Nas colunas de números, vamos criar duas: uma com os valores mínimos
            # e máximos (coluna resumida), outra com todos os valores de número
            numero        = paste(min(numero), max(numero), sep = '-'),
            numeros_todos = paste(sort(unique(numero)), collapse = "-"),
            # Agregar tipos de uso como abaixo, com o tipo e a quantidade identificada
            # por condomínio (valor que aparece entre parênteses)
            # Apartamento em condomínio (12) - Loja em edifício em condomínio (unidade autônoma) (3)
            tipo_uso = paste0(
              sapply(sort(unique(tipo_uso)), function(x) {
                count <- sum(tipo_uso == x)
                paste0(x, " (", count, ")")
              }),
              collapse = ", "
            )
            ) %>%
  ungroup() %>%
  select(-sq_cond)

# Alguns sql ficaram vazios - recompor
# perimetro_cond_XX %>% arrange(sql)
perimetro_cond_XX <- perimetro_cond_XX %>% mutate(sql = ifelse(sql == '', str_c(lo_setor, lo_quadra, lo_lote), sql))


# Adicionar colunas extras nos lotes sem condomínio, para junção das bases
perimetro_cond_00 <- perimetro_cond_00 %>% mutate(numeros_todos = numero,
                                                  testada_val   = testada_m)

# Juntar tudo para exportar
perimetro_out <-
  rbind(perimetro_cond_00, perimetro_cond_XX) %>%
  arrange(sql, numero) %>%
  relocate(c(sql, n_contrib, n_cond, geom), .after = last_col()) %>%
  relocate(c(numeros_todos, cep), .after = numero) %>%
  # Ajustar formatos das colunas
  mutate(testada_m = as.double(testada_m),
         esquinas  = as.integer(esquinas))


# out_gpkg <- sprintf('%s/lotes_perimetro_auditoria_com_numero.gpkg', pasta_proj)
out_gpkg <- sprintf('%s/lotes_perimetro_auditoria_com_numero.gpkg', pasta_proj)
st_write(perimetro_out, out_gpkg, driver = 'GPKG', append = FALSE, delete_layer = TRUE)

