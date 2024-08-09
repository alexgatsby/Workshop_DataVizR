# configurações ----------------

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (scales) == FALSE) {install.packages("scales"); require (scales)}


rm(list = ls())
gc()

# dados de natalidade --------------

dbn <- import (here ('data/nascvivos_mun.csv'), encoding='Latin-1')

dbn <- dbn %>%
  select (Município, `2000`, `2010`, `2020`) %>%
  pivot_longer(`2000`:`2020`) %>%
  rename ('ano'=name,
          'nnasc'=value,
          'local'=Município) %>%
  mutate (nnasc=as.numeric(nnasc),
          estado=str_sub(local,start=1, end=2)) %>%
  filter (estado != ' M')

# dados de mortalidade -------------

dbm <- import (here ('data/obitinf_mun.csv'), encoding='Latin-1')

dbm <- dbm %>%
  select (Município, `2000`, `2010`, `2020`) %>%
  pivot_longer(`2000`:`2020`) %>%
  rename ('ano'=name,
          'nobit'=value,
          'local'=Município) %>%
  mutate (nobit=as.numeric(nobit),
          estado=str_sub(local,start=1, end=2)) %>%
  filter (estado != ' M')

# junção e cálculos -----------------

db <- full_join(dbm, dbn)

db$tmi <- 1000*db$nobit/db$nnasc

# limpeza de regiões -------------

norte <- c("12", "16", "13", "15", "11", "14", "17")
nordeste <- c("27", "29", "23", "21", "25", "26", "22", "24", "28")
centro_oeste <- c("53", "52", "51", "50")
sudeste <- c("32", "31", "33", "35")
sul <- c("41", "43", "42")

db$regiao <- case_when(db$estado %in% norte ~ 'Região Norte',
                       db$estado %in% nordeste ~ 'Região Nordeste',
                       db$estado %in% centro_oeste ~ 'Região Centro-Oeste',
                       db$estado %in% sudeste ~ 'Região Sudeste',
                       db$estado %in% sul ~ 'Região Sul')

# variações ------------------


db <- db %>% 
  select (-nnasc) %>%
  pivot_wider (names_from = 'ano', values_from = c('nobit', 'tmi'))

db$var2000_2020 <- db$tmi_2020-db$tmi_2000

# gráfico ------------------

cores <- c('#8dd3c7', '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462', '#b3de69', '#fccde5')

db %>%
  filter (regiao != is.na(regiao)) %>%
  filter (var2000_2020>=-50) %>%
  ggplot (aes(x=tmi_2020, y=var2000_2020))  +
  geom_point(aes(size=nobit_2020/1000, color=regiao), alpha=.5) +
  geom_hline(aes(yintercept = 10), linetype='dotted') +
  geom_text(aes(label='Redução de 10',
                x=70, y=11), size=2) +
  geom_hline(aes(yintercept = 15), linetype='dotted') +
  geom_text(aes(label='Redução de 15',
                x=70, y=16), size=2) +
  scale_color_manual(values=cores[3:8]) +
  theme_classic() +
  labs (y='Declínio na\ntaxa de mortalidade nfantil',
        x='Taxa de Mortalidade Infantil de 2020',
        size='Mortes infantis\n(milhares), 2020',
        color=NULL,
        title='Variação da taxa de mortalidade infantil dos municípios brasileiros',
        subtitle='Fonte: DataSUS.',
        caption='Workshop Python-R-ArcGis.')

ggsave ('resultados/tarefa_aula1.png', width=10, height = 7)

