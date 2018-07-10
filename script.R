library(readr)
library(readxl)
library(pxR)
library(janitor)
library(stringr)
library(matrixStats)
library(rvest)
library(tidyr)
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)

# Read election data ------------------------------------------------------

# Generate column indexes
general_c <- seq(13, 21, by = 2)
votes_c <- seq(24, 70, by = 4)
pct_c <- seq(25, 70, by = 4)

# 2017 results
system('tar -xzf results/TXMUNAU09????????.tar.gz -C results/ && iconv -f WINDOWS-1252 -t UTF-8 results/TXMUNAU09089 results/TXMUNAU09179 results/TXMUNAU09259 results/TXMUNAU09439 > results/results_2017.txt')

# 2015 results
system('tar -xzf results/TXMUNAU09ANT.tar.gz -C results/ && iconv -f WINDOWS-1252 -t UTF-8 results/TXMUNAU09089ANT results/TXMUNAU09179ANT results/TXMUNAU09259ANT results/TXMUNAU09439ANT > results/results_2015.txt')

# 2017 results
df <- read_csv2('results/results_2017.txt', col_names = FALSE) %>%
  mutate_at(vars(10:21, votes_c, pct_c), as.numeric) %>%
  mutate_at(vars(general_c, pct_c), funs(. / 100)) %>%
  rename(
    name = X7,
    tables = X10,
    census = X11,
    counted_v = X12,
    counted_p = X13,
    total_v = X14,
    total_pct = X15,
    abs_v = X16,
    abs_pct = X17,
    blank_v = X18,
    blank_pct = X19,
    null_v = X20,
    null_pct = X21
  ) %>%
  mutate(
    id = paste(X3, X5, sep = ""),
    cup_v = case_when(
      X23 == 'CUP' ~ X24,
      X27 == 'CUP' ~ X28,
      X31 == 'CUP' ~ X32,
      X35 == 'CUP' ~ X36,
      X39 == 'CUP' ~ X40,
      X43 == 'CUP' ~ X44,
      X47 == 'CUP' ~ X48,
      X51 == 'CUP' ~ X52,
      X55 == 'CUP' ~ X56,
      X59 == 'CUP' ~ X60,
      X63 == 'CUP' ~ X64,
      X67 == 'CUP' ~ X68
    ),
    erc_v = case_when(
      X23 == 'ERC-CatSí' ~ X24,
      X27 == 'ERC-CatSí' ~ X28,
      X31 == 'ERC-CatSí' ~ X32,
      X35 == 'ERC-CatSí' ~ X36,
      X39 == 'ERC-CatSí' ~ X40,
      X43 == 'ERC-CatSí' ~ X44,
      X47 == 'ERC-CatSí' ~ X48,
      X51 == 'ERC-CatSí' ~ X52,
      X55 == 'ERC-CatSí' ~ X56,
      X59 == 'ERC-CatSí' ~ X60,
      X63 == 'ERC-CatSí' ~ X64,
      X67 == 'ERC-CatSí' ~ X68
    ),
    junts_v = case_when(
      X23 == 'JUNTSxCAT' ~ X24,
      X27 == 'JUNTSxCAT' ~ X28,
      X31 == 'JUNTSxCAT' ~ X32,
      X35 == 'JUNTSxCAT' ~ X36,
      X39 == 'JUNTSxCAT' ~ X40,
      X43 == 'JUNTSxCAT' ~ X44,
      X47 == 'JUNTSxCAT' ~ X48,
      X51 == 'JUNTSxCAT' ~ X52,
      X55 == 'JUNTSxCAT' ~ X56,
      X59 == 'JUNTSxCAT' ~ X60,
      X63 == 'JUNTSxCAT' ~ X64,
      X67 == 'JUNTSxCAT' ~ X68
    ),
    cup_pct = case_when(
      X23 == 'CUP' ~ X25,
      X27 == 'CUP' ~ X29,
      X31 == 'CUP' ~ X33,
      X35 == 'CUP' ~ X37,
      X39 == 'CUP' ~ X41,
      X43 == 'CUP' ~ X45,
      X47 == 'CUP' ~ X49,
      X51 == 'CUP' ~ X53,
      X55 == 'CUP' ~ X57,
      X59 == 'CUP' ~ X61,
      X63 == 'CUP' ~ X65,
      X67 == 'CUP' ~ X69
    ),
    erc_pct = case_when(
      X23 == 'ERC-CatSí' ~ X25,
      X27 == 'ERC-CatSí' ~ X29,
      X31 == 'ERC-CatSí' ~ X33,
      X35 == 'ERC-CatSí' ~ X37,
      X39 == 'ERC-CatSí' ~ X41,
      X43 == 'ERC-CatSí' ~ X45,
      X47 == 'ERC-CatSí' ~ X49,
      X51 == 'ERC-CatSí' ~ X53,
      X55 == 'ERC-CatSí' ~ X57,
      X59 == 'ERC-CatSí' ~ X61,
      X63 == 'ERC-CatSí' ~ X65,
      X67 == 'ERC-CatSí' ~ X69
    ),
    junts_pct = case_when(
      X23 == 'JUNTSxCAT' ~ X25,
      X27 == 'JUNTSxCAT' ~ X29,
      X31 == 'JUNTSxCAT' ~ X33,
      X35 == 'JUNTSxCAT' ~ X37,
      X39 == 'JUNTSxCAT' ~ X41,
      X43 == 'JUNTSxCAT' ~ X45,
      X47 == 'JUNTSxCAT' ~ X49,
      X51 == 'JUNTSxCAT' ~ X53,
      X55 == 'JUNTSxCAT' ~ X57,
      X59 == 'JUNTSxCAT' ~ X61,
      X63 == 'JUNTSxCAT' ~ X65,
      X67 == 'JUNTSxCAT' ~ X69
    ),
    ind_vote = cup_v + erc_v + junts_v,
    ind_pct = cup_pct + erc_pct + junts_pct,
    no_ind_vote = total_v - null_v - blank_v - ind_vote,
    no_ind_pct = 100 - null_pct - blank_pct - ind_pct
  ) %>%
  filter(X6 == '99') %>%
  select(id, everything()) %>%
  select(id, name, census, total_pct, ind_vote, ind_pct, no_ind_vote, no_ind_pct)

# 2015 results
df_2015 <- read_csv2('results/results_2015.txt', col_names = FALSE) %>%
  mutate_at(vars(10:21, votes_c, pct_c), as.numeric) %>%
  mutate_at(vars(general_c, pct_c), funs(. / 100)) %>%
  rename(
    name = X7,
    tables = X10,
    census = X11,
    counted_v = X12,
    counted_p = X13,
    total_v = X14,
    total_pct_2015 = X15,
    abs_v = X16,
    abs_pct = X17,
    blank_v = X18,
    blank_pct = X19,
    null_v = X20,
    null_pct = X21
  ) %>%
  mutate(
    id = paste(X3, X5, sep = ""),
    cup_v = case_when(
      X23 == 'CUP' ~ X24,
      X27 == 'CUP' ~ X28,
      X31 == 'CUP' ~ X32,
      X35 == 'CUP' ~ X36,
      X39 == 'CUP' ~ X40,
      X43 == 'CUP' ~ X44,
      X47 == 'CUP' ~ X48,
      X51 == 'CUP' ~ X52,
      X55 == 'CUP' ~ X56,
      X59 == 'CUP' ~ X60,
      X63 == 'CUP' ~ X64,
      X67 == 'CUP' ~ X68
    ),
    jxsi_v = case_when(
      X23 == 'JxSí' ~ X24,
      X27 == 'JxSí' ~ X28,
      X31 == 'JxSí' ~ X32,
      X35 == 'JxSí' ~ X36,
      X39 == 'JxSí' ~ X40,
      X43 == 'JxSí' ~ X44,
      X47 == 'JxSí' ~ X48,
      X51 == 'JxSí' ~ X52,
      X55 == 'JxSí' ~ X56,
      X59 == 'JxSí' ~ X60,
      X63 == 'JxSí' ~ X64,
      X67 == 'JxSí' ~ X68
    ),
    cup_pct = case_when(
      X23 == 'CUP' ~ X25,
      X27 == 'CUP' ~ X29,
      X31 == 'CUP' ~ X33,
      X35 == 'CUP' ~ X37,
      X39 == 'CUP' ~ X41,
      X43 == 'CUP' ~ X45,
      X47 == 'CUP' ~ X49,
      X51 == 'CUP' ~ X53,
      X55 == 'CUP' ~ X57,
      X59 == 'CUP' ~ X61,
      X63 == 'CUP' ~ X65,
      X67 == 'CUP' ~ X69
    ),
    jxsi_pct = case_when(
      X23 == 'JxSí' ~ X25,
      X27 == 'JxSí' ~ X29,
      X31 == 'JxSí' ~ X33,
      X35 == 'JxSí' ~ X37,
      X39 == 'JxSí' ~ X41,
      X43 == 'JxSí' ~ X45,
      X47 == 'JxSí' ~ X49,
      X51 == 'JxSí' ~ X53,
      X55 == 'JxSí' ~ X57,
      X59 == 'JxSí' ~ X61,
      X63 == 'JxSí' ~ X65,
      X67 == 'JxSí' ~ X69
    ),
    ind_vote = cup_v + jxsi_v,
    ind_pct = cup_pct + jxsi_pct,
    no_ind_vote = total_v - null_v - blank_v - ind_vote,
    no_ind_pct = 100 - blank_pct - ind_pct
  ) %>%
  filter(X6 == '99') %>%
  select(id, everything()) %>%
  select(id, name, census, total_pct_2015, ind_vote, ind_pct, no_ind_vote, no_ind_pct)

# Census data -------------------------------------------------------------
# Població de 2 anys i més segons coneixement del català
# https://www.idescat.cat/pub/?id=censph&n=17&by=mun
cat <- read_csv('src/t17mun.csv', skip = 9, n_max = 947) %>%
  clean_names() %>%
  mutate(id = substr(codi, 1, nchar(codi) - 1),
         lenten = as.numeric(lenten),
         el_sap_parlar = as.numeric(el_sap_parlar),
         el_sap_llegir = as.numeric(el_sap_llegir),
         el_sap_escriure = as.numeric(el_sap_escriure),
         no_lenten = as.numeric(no_lenten),
         poblacio_de_2_anys_i_mes = as.numeric(poblacio_de_2_anys_i_mes),
         lenten_pct = (lenten / poblacio_de_2_anys_i_mes) * 100,
         el_sap_parlar_pct = (el_sap_parlar / poblacio_de_2_anys_i_mes) * 100,
         el_sap_llegir_pct = (el_sap_llegir / poblacio_de_2_anys_i_mes) * 100,
         el_sap_escriure_pct = (el_sap_escriure / poblacio_de_2_anys_i_mes) * 100) %>%
  select(id, everything())

# Població de 16 anys i més segons nivell d'instrucció
# https://www.idescat.cat/pub/?id=censph&n=15&by=mun
es <- read_csv('src/t15mun.csv', skip = 9, n_max = 465) %>%
  clean_names() %>%
  mutate(id = substr(codi, 1, nchar(codi) - 1),
         no_sap_llegir_o_escriure = as.numeric(gsub("\\..", "0", no_sap_llegir_o_escriure)),
         sense_estudis = as.numeric(gsub("\\..", "0", sense_estudis)),
         educacio_primaria = as.numeric(gsub("\\..", "0", educacio_primaria)),
         eso = as.numeric(gsub("\\..", "0", eso)),
         batxillerat_superior = as.numeric(gsub("\\..", "0", batxillerat_superior)),
         fp_grau_mitja = as.numeric(gsub("\\..", "0", fp_grau_mitja)),
         fp_grau_superior = as.numeric(gsub("\\..", "0", fp_grau_superior)),
         diplomatura =  as.numeric(gsub("\\..", "0", diplomatura)),
         grau_universitari =  as.numeric(gsub("\\..", "0", grau_universitari)),
         llicenciatura_i_doctorat =  as.numeric(gsub("\\..", "0", llicenciatura_i_doctorat)),
         total = as.numeric(gsub("\\..", "0", total)),
         educacio_superior = fp_grau_superior + diplomatura + grau_universitari + llicenciatura_i_doctorat,
         educacio_basica = educacio_primaria + eso + batxillerat_superior + fp_grau_mitja,
         sense_educacio = no_sap_llegir_o_escriure + sense_estudis,
         educacio_superior_pct = (educacio_superior / total) * 100,
         educacio_basica_pct = (educacio_basica / total) * 100,
         sense_educacio_pct = (sense_educacio / total) * 100)  %>%
  select(id, everything())

# Població segons lloc de naixement per comunitats autònomes
# https://www.idescat.cat/pub/?id=censph&n=12&by=mun
pop <- read_csv('src/t12mun.csv', skip = 6, n_max = 947) %>%
  clean_names() %>%
  mutate(id = substr(codi, 1, nchar(codi) - 1),
         catalunya_pct = catalunya / total * 100,
         resta_estat_pct = resta_estat / total * 100,
         estrangers_pct = estranger / total * 100)  %>%
  select(id, everything())

# Median age
# Estadística del Padrón Continuo a 1 de enero de 2016. Datos por municipios
# 1.6 Población por sexo, municipios y edad (año a año).
# http://www.ine.es/dynt3/inebase/es/index.htm?type=pcaxis&file=pcaxis&path=%2Ft20%2Fe245%2Fp05%2F%2Fa2016
age_input <- as.data.frame(read.px("src/00000006.px"))

age_input <- clean_names(age_input) %>%
  filter(sexo == 'Ambos sexos' &
         edad_ano_a_ano != 'Total' &
         municipios != 'Total') %>%
  mutate(edad_ano_a_ano =  as.numeric(gsub("100 y más", "100", edad_ano_a_ano)),
         id = substring(municipios, 1, 5),
         municipios = substring(municipios, 7)) %>%
  select(id, everything())

age <- tibble(id = unique(age_input$id), median_age = NA)

get_median_age <- function(x) {
  weightedMedian(filter(age_input, id == x)$edad_ano_a_ano, filter(age_input, id == x)$value)
}

# Calculate median age per city (takes a while...)
for (i in seq_along(age$id)) {
  age[[2]][i] <- get_median_age(age[[1]][i])
}

age$median_age <- round(age$median_age, 2)

# Income
# Posicionamiento de los municipios mayores de 1.000 habitantes por Renta bruta media
# http://www.agenciatributaria.es/AEAT/Contenidos_Comunes/La_Agencia_Tributaria/Estadisticas/Publicaciones/sites/irpfmunicipios/2015/jrubik1ba3b6ffb879f0b4654305cde4f7da3038a346e9.html
url <- "http://www.agenciatributaria.es/AEAT/Contenidos_Comunes/La_Agencia_Tributaria/Estadisticas/Publicaciones/sites/irpfmunicipios/2015/jrubik1ba3b6ffb879f0b4654305cde4f7da3038a346e9.html"

income_table <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="table01"]') %>%
  html_table()

income <- income_table[[1]]

income <- clean_names(income) %>%
  plyr::rename(c("x" = "municipio")) %>%
  filter(grepl('-', municipio)) %>%
  mutate(id = str_sub(municipio, -5, -1),
         municipio = str_sub(municipio, 0, -7),
         renta_disponiblemedia = renta_disponiblemedia * 1000,
         renta_bruta_media = renta_bruta_media * 1000) %>%
  select(id, everything())

# Pop density
pop_density <- read_csv('src/pop_density.csv')

#merged <- merge(df, est)
merged <- merge(df, df_2015[,c("id", "total_pct_2015")])
merged <- merge(merged, age)
merged <- merge(merged, cat)
merged <- merge(merged, pop)
merged <- merge(merged, pop_density)
merged <- merge(merged, es, by.x = 'id', by.y = 'id', all.x = TRUE)
merged <- merge(merged, income, all.x = TRUE)

# Extract separator after comma (l', la, etc)
merged <- separate(merged, name, c("name", "name_sep"), sep = "\\, ")
merged$name_sep <- tools::toTitleCase(merged$name_sep)

merged$name <- paste(merged$name_sep, merged$name, sep=" ")
merged$name_sep <- NULL
merged$name <- gsub("NA", "", merged$name)
merged$name <- str_trim(merged$name, side = c("left"))

# Replace wrong names
merged$name[merged$name == "Monells i Sant Sadurní De l'Heura Cruïlles"] <- "Cruïlles, Monells i Sant Sadurní de l'Heura"
merged$name[merged$name == "Camallera i Llampaies Saus"] <- "Saus, Camallera i Llampaies"

# Generate final data
write_csv(mutate(merged,
                 diff = total_pct - total_pct_2015,
                 ind_pct = round(ind_pct, 2),
                 no_ind_pct = round(no_ind_pct, 2),
                 el_sap_parlar_pct = round(el_sap_parlar_pct, 2),
                 resta_estat_pct = round(resta_estat_pct, 2),
                 estrangers_pct = round(estrangers_pct, 2),
                 educacio_superior_pct = round(educacio_superior_pct, 2),
                 educacio_basica_pct = round(educacio_basica_pct, 2)
          ) %>%
          select(id,
                 name,
                 census,
                 total_pct,
                 total_pct_2015,
                 diff,
                 ind_vote,
                 ind_pct,
                 no_ind_vote,
                 no_ind_pct,
                 el_sap_parlar_pct,
                 resta_estat_pct,
                 estrangers_pct,
                 educacio_superior_pct,
                 educacio_basica_pct,
                 renta_disponiblemedia,
                 median_age,
                 density), 'resultados_2017.csv', na = "")
