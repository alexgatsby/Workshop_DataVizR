# EXEMPLO 1 - MAPAS
# passo 1: preparação básica ----------------

## pacotes

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (sf) == FALSE) {install.packages("sf"); require (sf)}


## limpeza

rm(list = ls())
gc()

# passo 2: preparação específica --------------------

## dados de mortalidade infantil --------------------

## nascidos vivos

## importar

dbn <- import (here ('data/nascvivos_estadosreg_cada10anos.csv'), encoding='Latin-1')

## long para wide

dbn <- dbn %>% select (-Total) %>% pivot_longer(`2000`:`2020`)

## limpeza

colnames (dbn) <- c('local', 'ano', 'nnasc')

dbn$local <- dbn$local %>% str_remove('.. ')
dbn$local <- dbn$local %>% str_replace('Regi', 'Região ')

## número de mortes infantis

## importar

dbm <- import (here ('data/obitinf_estadosreg_cada10anos.csv'), encoding='Latin-1')

## long para wide

dbm <- dbm %>% select (-Total) %>% pivot_longer (`2000`:`2020`)

## limpeza

colnames (dbm) <- c('local', 'ano', 'nobit')

dbm$local <- dbm$local %>% str_remove('.. ')
dbm$local <- dbm$local %>% str_replace('Regi', 'Região ')

## junção e cálculo

db <- full_join(dbn, dbm)

db$tmi <- 1000*(db$nobit/db$nnasc)

db %>% count (local)
db$local <- ifelse (db$local=='Total', 'Brasil', db$local)

## dados do mapa do Brasil --------------------------

## importar online

if (require (geobr) == FALSE) {install.packages("geobr"); require (geobr)}

map <- read_state(year=2020)

## conferir

plot (map$geom)

## limpar

map %>% count (name_state) %>% View ()

map$local <- ifelse (map$name_state=='Amazônas', 'Amazonas', 
                     ifelse (map$name_state=='Rio De Janeiro', 'Rio de Janeiro',
                             ifelse (map$name_state=='Rio Grande Do Norte', 'Rio Grande do Norte',
                                     ifelse (map$name_state=='Rio Grande Do Sul', 'Rio Grande do Sul', 
                                             ifelse (map$name_state=='Mato Grosso Do Sul', 'Mato Grosso do Sul', map$name_state)))))

map <- map %>% select (geom, local)

## juntando bases --------------

glimpse (map)
glimpse (db)

db <- full_join(db, map)

# passo 3: gráfico -------------------------

## vamos fazer juntos --------------

## gabarito --------------

db %>%
  filter (ano=='2020') %>%
  ggplot () +
  geom_sf (aes(geometry=geom, fill=tmi), color=0) +
  scale_fill_gradient(low='#fee0d2', high='#a50f15') +
  theme_void () +
  theme (legend.position = 'left') +
  labs (fill='Taxa de\nMortalidade Infantil',
        title='Mapa de calor da mortalidade infantil do\nBrasil por Estado em 2020',
        subtitle='Fontes: DataSUS e IBGE.',
        caption='Workshop Python-R-ArcGis.')

ggsave ('resultados/exemplo1_mapas.png', width=8, height=6)


# EXEMPLO 2 - MAPAS ---------------------------

# passo 1: preparação básica -------------------

#já feito

# passo 2: preparação específica -------------------

# dados do shape de Recife ---------------

map <- read_sf ('data/Bairros/Bairro.shp')

plot (map$geometry)

recife <- st_union(map$geometry)

plot (recife)

# pontos de UBS ---------------

db <- import (here ('data/unidades-basica-saude.csv'))

pontos <- db %>% 
  select (latitude, longitude, rpa) %>%
  drop_na () %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4236)

# passo 3: gráfico ---------------------

cores <- c('#8dd3c7', '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462', '#b3de69', '#fccde5')

ggplot () +
  geom_sf (data=recife, aes(geometry=geometry,), color='black', alpha=0) +
  geom_sf (data=pontos, aes(color=as.character(rpa))) +
  scale_color_manual(values=cores[3:9]) +
  theme_void () +
  theme (legend.position = 'left') +
  labs (color='RPA',
        title='Unidades Básicas de Saúde do Recife',
        subtitle = 'Fonte: Portal de Dados Abertos da PCR.',
        caption = 'Workshop Python-R-ArcGis.')

ggsave ('resultados/exemplo2_mapas.png', width=5, height=6)

# TAREFA DE CASA --------------------

# Fazer um mapa de calor da mortalidade infantil do Brasil com bolhas para as taxas das capitais.
# Base de dados: dados_capitais
