library('tidyverse')
library('tidylog')
library('sf')
library('mapview')
library('Hmisc')


options(scipen = 999)

# pasta_base <- '/Volumes/Expansion/Dados_Comp_Gabinete/OD2023'
pasta_base <- '/mnt/fern/Dados/dados/Metro/OD_2023'

zonas <- sprintf('%s/pacote-anexos-od_10-02-25/002_Site Metro Mapas/Shape/Zonas_2023.shp', pasta_base)
zonas <- read_sf(zonas, options = "ENCODING=WINDOWS-1252")

zonas_od <- zonas %>% st_drop_geometry() %>% select(zona = NumeroZona, NomeZona, NomeMunici)

od <- sprintf('%s/pacote-anexos-od_10-02-25/Banco2023_divulgacao_310125.csv', pasta_base)
od <- read_delim(od, delim = ';', col_types = cols(.default = col_character()))
head(od)


od_short <-
  od %>%
  mutate(fe_via = str_replace(fe_via, ',', '.')) %>%
  mutate(fe_via = as.double(fe_via)) %>%
  mutate(distancia = str_replace(distancia, ',', '.')) %>%
  mutate(distancia = as.double(distancia)) %>%
  select(zona_o, muni_o, zona_d, muni_d, motivo_o, motivo_d, tipvg, modo1, modo2, modo3, modo4, modoprin, tipvg, h_saida, h_cheg, duracao, tp_esauto, pe_bici, distancia, fe_via)

od_short <- od_short %>% mutate(across(1:17, as.numeric))

od_short <-
  od_short %>%
  mutate(modoprin_g = case_when(modoprin %in% c(1, 2, 3) ~ '01. trilhos',
                                modoprin %in% c(4, 5, 6)       ~ '02. onibus munic_metrop',
                                modoprin %in% c(7, 8)    ~ '03. fretado_escolar',
                                modoprin %in% c(9, 10)   ~ '04. automovel',
                                modoprin %in% c(11, 12)  ~ '05. taxi_app',
                                modoprin %in% c(13, 14, 15) ~ '06. moto',
                                modoprin %in% c(16) ~ '07. bicicleta',
                                modoprin %in% c(17) ~ '08. a pé',
                                modoprin %in% c(18) ~ '09. outros',
                                TRUE ~ NA)) %>%
  mutate(tipo = ifelse(tipvg == '2', 'motorizadas', 'coletivas_ativas'))


# Viagens com destino às zonas OD de interesse:
od_bras <- od_short %>% filter(zona_d %in% c(13:17))
# od_bras %>% select(zona_d) %>% distinct()

# Viagens atraídas
od_bras %>% group_by(zona_d) %>% summarise(n = sum(fe_via))
# zona_d      n
# <dbl>  <dbl>
# 1     13 30726.
# 2     14 45775.
# 3     15 25612.
# 4     16 50737.
# 5     17 24616.

# 77.4 das viagens atraídas em modos coletivos e ativos nas 5 zonas
od_bras %>% group_by(modoprin_g) %>% summarise(n = sum(fe_via)) %>% mutate(perc = round(n / sum(n) * 100, 1))
od_bras %>% group_by(tipo) %>% summarise(n = sum(fe_via)) %>% mutate(perc = round(n / sum(n) * 100, 1))

# 73.3 das viagens atraídas em modos coletivos e ativos nas 3 zonas de cima
od_bras %>% filter(zona_d %in% c(13, 14, 17)) %>% group_by(modoprin_g) %>% summarise(n = sum(fe_via)) %>% mutate(perc = round(n / sum(n) * 100, 1))
od_bras %>% filter(zona_d %in% c(13, 14, 17)) %>% group_by(tipo) %>% summarise(n = sum(fe_via)) %>% mutate(perc = round(n / sum(n) * 100, 1))

od_bras %>% filter(zona_d %in% c(13, 14, 17)) %>% group_by(zona_o) %>% summarise(n = sum(fe_via)) %>% mutate(perc = round(n / sum(n) * 100, 1)) %>% filter(perc > 1.1) %>% head(20) %>% left_join(zonas_od, by = c('zona_o' = 'zona'))

this <- od_bras %>% filter(zona_d %in% c(13, 14, 17)) %>% group_by(zona_o) %>% summarise(n = sum(fe_via)) %>% mutate(perc = round(n / sum(n) * 100, 1)) %>% filter(perc > 1.1)

zonas %>% filter(NumeroZona %in% this$zona_o) %>% mapview()



# Por Que Viajou A Pé ou Bicicleta? - viagens originadas no município de SP
# 1 - Pequena distância
# 7 - Atividade física
od_short %>%
  filter(muni_o == 36 | muni_d == 36) %>%
  filter(muni_o == 36) %>%
  select(-c(muni_o, muni_d)) %>%
  filter(!is.na(pe_bici) & tipvg != 1) %>%
  group_by(tipvg, pe_bici) %>%
  summarise(n = sum(fe_via)) %>%
  mutate(perc = round(n / sum(n) * 100, 1)) %>%
  filter(perc > 10)
# tipvg pe_bici        n  perc
# <dbl>   <dbl>    <dbl> <dbl>
# 1     3       1 5223395.  94.7
# 2     4       1  155328.  65
# 3     4       7   33557.  14


od_bras %>%
  select(-c(muni_o, muni_d, matches('h_'), tp_esauto, modoprin_g, tipo, modoprin)) %>%
  group_by(tipvg) %>%
  # select(distancia) %>% summary()
  summarise(
    weighted_mean = weighted.mean(distancia, fe_via),
    # weighted_median = Hmisc::wtd.quantile(distancia, weights = fe_via, probs = 0.5),
    quantiles = list(Hmisc::wtd.quantile(distancia, weights = fe_via,
                                         probs = c(0.25, 0.5, 0.75)))
  ) %>%
  unnest_wider(quantiles, names_repair = "unique")










# Distâncias a pé (sem detour)
od_short %>%
  filter(muni_o == 36) %>%
  select(-c(muni_o, muni_d, matches('h_'), tp_esauto, modoprin_g, tipo, modoprin)) %>%
  filter(tipvg == 3) %>%
  # select(distancia) %>% summary()
  summarise(
    weighted_mean = weighted.mean(distancia, fe_via),
    # weighted_median = Hmisc::wtd.quantile(distancia, weights = fe_via, probs = 0.5),
    quantiles = list(Hmisc::wtd.quantile(distancia, weights = fe_via,
                                         probs = c(0.25, 0.5, 0.75)))
  ) %>%
  unlist()
# weighted_mean quantiles.25% quantiles.50% quantiles.75%
# 691.3029      224.2944      418.9224      710.7756

# Distâncias a pé (com detour de 1.333)
od_short %>%
  filter(muni_o == 36) %>%
  select(-c(muni_o, muni_d, matches('h_'), tp_esauto, modoprin_g, tipo, modoprin)) %>%
  filter(tipvg == 3) %>%
  mutate(distancia = distancia * 1.333) %>%
  # select(distancia) %>% summary()
  summarise(
    weighted_mean = weighted.mean(distancia, fe_via),
    # weighted_median = Hmisc::wtd.quantile(distancia, weights = fe_via, probs = 0.5),
    quantiles = list(Hmisc::wtd.quantile(distancia, weights = fe_via,
                                         probs = c(0.25, 0.5, 0.75)))
  ) %>%
  unlist()
# weighted_mean quantiles.25% quantiles.50% quantiles.75%
# 921.5068      298.9845      558.4236      947.4639


# Distâncias em bicicleta (sem detour)
od_short %>%
  filter(muni_o == 36) %>%
  select(-c(muni_o, muni_d, matches('h_'), tp_esauto, modoprin_g, tipo, modoprin)) %>%
  filter(tipvg == 4) %>%
  # select(distancia) %>% summary()
  summarise(
    weighted_mean = weighted.mean(distancia, fe_via),
    # weighted_median = Hmisc::wtd.quantile(distancia, weights = fe_via, probs = 0.5),
    quantiles = list(Hmisc::wtd.quantile(distancia, weights = fe_via,
                                         probs = c(0.25, 0.5, 0.75)))
  ) %>%
  unlist()
# weighted_mean quantiles.25% quantiles.50% quantiles.75%
# 3277.7267      891.4673     1835.8118     3683.1128


# Distâncias em bicicleta (com detour de 1.333)
od_short %>%
  filter(muni_o == 36) %>%
  select(-c(muni_o, muni_d, matches('h_'), tp_esauto, modoprin_g, tipo, modoprin)) %>%
  filter(tipvg == 4) %>%
  mutate(distancia = distancia * 1.333) %>%
  # select(distancia) %>% summary()
  summarise(
    weighted_mean = weighted.mean(distancia, fe_via),
    # weighted_median = Hmisc::wtd.quantile(distancia, weights = fe_via, probs = 0.5),
    quantiles = list(Hmisc::wtd.quantile(distancia, weights = fe_via,
                                         probs = c(0.25, 0.5, 0.75)))
  ) %>%
  unlist()
# weighted_mean quantiles.25% quantiles.50% quantiles.75%
# 4369.210      1188.326      2447.137      4909.589











# Modos coletivos vs individuais, em milhões
# 1 - Coletivo
# 2 - Individual
# 3 - A pé
# 4 - Bicicleta
od_short %>%
  group_by(tipvg) %>%
  summarise(n = sum(fe_via) / 1000000) %>%
  mutate(perc = as.character(round(n / sum(n) * 100, 1)))
# tipvg      n perc
# <chr>  <dbl> <chr>
# 1 1     12.3   34.4
# 2 2     12.9   36.1
# 3 3     10.1   28.2
# 4 4      0.472 1.3

# Motorizadas vs Não motorizadas, em milhões
# 1 - Coletivo
# 2 - Individual
# 3 - A pé
# 4 - Bicicleta
od_short %>%
  mutate(tipo = ifelse(tipvg %in% c('1', '2'), 'motorizadas', 'ativas')) %>%
  group_by(tipo) %>%
  summarise(n = sum(fe_via) / 1000000) %>%
  mutate(perc = as.character(round(n / sum(n) * 100, 1)))
# tipo            n perc
# <chr>       <dbl> <chr>
# 1 ativas       10.5 29.5
# 2 motorizadas  25.1 70.5


# Em São Paulo, capital
od_sp <- od_short %>% filter(muni_o == '36')

# Viagens totais
od_short %>% summarise(n = sum(fe_via) / 1000000) # 35.7
od_sp %>% summarise(n = sum(fe_via) / 1000000) # 21.1

# Modos coletivos vs individuais, em milhões
# 1 - Coletivo
# 2 - Individual
# 3 - A pé
# 4 - Bicicleta
od_sp %>%
  group_by(tipvg) %>%
  summarise(n = sum(fe_via) / 1000000) %>%
  mutate(perc = as.character(round(n / sum(n) * 100, 1)))
# tipvg     n perc
# <chr> <dbl> <chr>
# 1 1     8.37  39.7
# 2 2     6.94  32.9
# 3 3     5.52  26.2
# 4 4     0.239 1.1

# Coletivos + Ativos
od_sp %>%
  mutate(tipo = ifelse(tipvg == '2', 'motorizadas', 'coletivas_ativas')) %>%
  group_by(tipo) %>%
  summarise(n = sum(fe_via) / 1000000) %>%
  mutate(perc = as.character(round(n / sum(n) * 100, 1)))
# tipo                 n perc
# <chr>            <dbl> <chr>
# 1 coletivas_ativas 14.1  67.1
# 2 motorizadas       6.94 32.9

# Motorizadas vs Não motorizadas, em milhões
# 1 - Coletivo
# 2 - Individual
# 3 - A pé
# 4 - Bicicleta
od_sp %>%
  mutate(tipo = ifelse(tipvg %in% c('1', '2'), 'motorizadas', 'ativas')) %>%
  group_by(tipo) %>%
  summarise(n = sum(fe_via) / 1000000) %>%
  mutate(perc = as.character(round(n / sum(n) * 100, 1)))
# tipo            n perc
# <chr>       <dbl> <chr>
# 1 ativas       5.75 27.3
# 2 motorizadas 15.3  72.7

# Por modo principal
# 01 - Metrô
# 02 - Trem
# 03 - Monotrilho
# 04 - Ônibus/micro-ônibus/van do município de São Paulo
# 05 - Ônibus/micro-ônibus/van de outros municípios
# 06 - Ônibus/micro-ônibus/van metropolitano
# 07 - Transporte Fretado
# 08 - Transporte Escolar
# 09 - Dirigindo Automóvel
# 10 - Passageiro de Automóvel
# 11 - Táxi Convencional
# 12 - Táxi não Convencional / aplicativo
# 13 - Dirigindo Moto
# 14 - Passageiro de Moto
# 15 - Passageiro de Mototáxi
# 16 - Bicicleta
# 17 - A Pé
# 18 - Outros
od_sp %>%
  mutate(modoprin = as.integer(modoprin)) %>%
  group_by(modoprin) %>%
  summarise(n = sum(fe_via) / 1000) %>%
  mutate(perc = as.character(round(n / sum(n) * 100, 1)))
# modoprin        n perc
# <int>    <dbl> <chr>
# 1        1 2382.    11.3
# 2        2  578.    2.7
# 3        3   42.5   0.2
# 4        4 3969.    18.8
# 5        5    8.68  0
# 6        6   73.2   0.3
# 7        7   45.7   0.2
# 8        8 1270.    6
# 9        9 4188.    19.9
# 10       10 1471.    7
# 11       11   46.7   0.2
# 12       12  574.    2.7
# 13       13  573.    2.7
# 14       14   48.7   0.2
# 15       15    0.142 0
# 16       16  239.    1.1
# 17       17 5515.    26.2
# 18       18   34.1   0.2



od_sp %>%
  mutate(modoprin = as.integer(modoprin)) %>%
  mutate(modoprin_g = case_when(modoprin %in% c(1, 2, 3) ~ '01. trilhos',
                                modoprin %in% c(4)       ~ '02. onibus munic',
                                modoprin %in% c(5, 6)    ~ '03. onibus metrop',
                                modoprin %in% c(7, 8)    ~ '04. fretado_escolar',
                                modoprin %in% c(9, 10)   ~ '05. automovel',
                                modoprin %in% c(11, 12)  ~ '06. taxi_app',
                                modoprin %in% c(13, 14, 15) ~ '07. moto',
                                modoprin %in% c(16) ~ '08. bicicleta',
                                modoprin %in% c(17) ~ '09. a pé',
                                modoprin %in% c(18) ~ '10. outros',
                                TRUE ~ NA)) %>%
  group_by(modoprin_g) %>%
  summarise(n = sum(fe_via) / 1000000) %>%
  mutate(perc = as.character(round(n / sum(n) * 100, 1)))
# modoprin_g               n perc
# <chr>                <dbl> <chr>
# 1 01. trilhos         3.00   14.3
# 2 02. onibus munic    3.97   18.8
# 3 03. onibus metrop   0.0819 0.4
# 4 04. fretado_escolar 1.32   6.2
# 5 05. automovel       5.66   26.9
# 6 06. taxi_app        0.621  2.9
# 7 07. moto            0.622  3
# 8 08. bicicleta       0.239  1.1
# 9 09. a pé            5.52   26.2
# 10 10. outros          0.0341 0.2


modoprin



# Bicicletas usadas, mas não como modo principal: 2 viagens = 378 com fator de expansão
od_sp %>%
  filter(modo2 == 16 | modo3 == 16 | modo4 == 16) %>%
  mutate(fe_via = str_replace(fe_via, ',', '.')) %>%
  mutate(fe_via = as.double(fe_via)) %>%
  group_by(muni_o) %>%
  summarise(n = sum(fe_via))

# Bicicletas: 674 viagens pesquisadas (fator expansão: 239139)
od_sp %>%
  filter(tipvg == '4') %>%
  mutate(fe_via = str_replace(fe_via, ',', '.')) %>%
  mutate(fe_via = as.double(fe_via)) %>%
  group_by(modo1) %>%
  summarise(n = sum(fe_via))

od_sp %>%
  mutate(fe_via = str_replace(fe_via, ',', '.')) %>%
  mutate(fe_via = as.double(fe_via)) %>%
  group_by(tipvg) %>%
  summarise(n = sum(fe_via)) %>%
  mutate(perc = as.character(round(n / sum(n) * 100, 3)))

od_sp %>%
  mutate(fe_via = str_replace(fe_via, ',', '.')) %>%
  mutate(fe_via = as.double(fe_via)) %>%
  group_by(modoprin) %>%
  summarise(n = sum(fe_via)) %>%
  mutate(perc = as.character(round(n / sum(n) * 100, 3))) %>%
  ungroup() %>%
  arrange(n)






od %>%
  mutate(fe_via = str_replace(fe_via, ',', '.')) %>%
  mutate(fe_via = as.double(fe_via)) %>%
  group_by(tipvg) %>%
  summarise(n = sum(fe_via)) %>%
  mutate(perc = as.character(round(n / sum(n) * 100, 3)))
