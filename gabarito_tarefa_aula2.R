# configurações ----------------------

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (sf) == FALSE) {install.packages("sf"); require (sf)}
if (require (geobr) == FALSE) {install.packages("geobr"); require (geobr)}

rm(list = ls())
gc()

# dados do mapa -------------------

map <- read_state(year=2020)

plot (map$geom)

map$local <- ifelse (map$name_state=='Amazônas', 'Amazonas', 
                     ifelse (map$name_state=='Rio De Janeiro', 'Rio de Janeiro',
                             ifelse (map$name_state=='Rio Grande Do Norte', 'Rio Grande do Norte',
                                     ifelse (map$name_state=='Rio Grande Do Sul', 'Rio Grande do Sul', 
                                             ifelse (map$name_state=='Mato Grosso Do Sul', 'Mato Grosso do Sul', map$name_state)))))

map <- map %>% select (geom, local)

# dados de mortalidade infantil -------------

## nascimentos

dbn <- import (here ('data/nascvivos_estadosreg_cada10anos.csv'), encoding='Latin-1')

dbn <- dbn %>% select (-Total) %>% pivot_longer(`2000`:`2020`)

colnames (dbn) <- c('local', 'ano', 'nnasc')

dbn$local <- dbn$local %>% str_remove('.. ')
dbn$local <- dbn$local %>% str_replace('Regi', 'Região ')

## mortes

dbm <- import (here ('data/obitinf_estadosreg_cada10anos.csv'), encoding='Latin-1')

dbm <- dbm %>% select (-Total) %>% pivot_longer (`2000`:`2020`)

colnames (dbm) <- c('local', 'ano', 'nobit')

dbm$local <- dbm$local %>% str_remove('.. ')
dbm$local <- dbm$local %>% str_replace('Regi', 'Região ')

## junção e cálculo

db <- full_join(dbn, dbm) %>% filter (ano=='2020')

db$tmi <- 1000*(db$nobit/db$nnasc)

db$local <- ifelse (db$local=='Total', 'Brasil', db$local)

# juntando as bases -------------

db <- full_join(db, map)

# dados das capitais ------------------------

cap <- import (here ('data/dados_capitais.csv'))

pontos <- cap %>% 
  select (latitude, longitude, tmi2020) %>%
  drop_na () %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4236)

# gráfico --------------------

ggplot () +
  geom_sf (data=db, aes(geometry=geom, fill=tmi), color=0) +
  geom_sf (data=pontos, aes(size=tmi2020), alpha=.7, color='#80b1d3') +
  scale_fill_gradient(low='#fee0d2', high='#a50f15') +
  theme_void() +
  labs (title='Mortalidade infantil dos Estados e Capitais em 2020',
        fill='Mortalidade Infantil\ndos Estados',
        size='Mortalidade Infantil\ndas Capitais',
        subtitle = 'Fonte: DataSUS.',
        caption = 'Workshop Python-R-ArcGis.')

ggsave ('resultados/tarefa_aula2.png', width=8, height=6)
