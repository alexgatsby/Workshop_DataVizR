# EXEMPLO 1 - GRÁFICOS
# passo 1: preparação básica ----------------

## pacotes

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (scales) == FALSE) {install.packages("scales"); require (scales)}

## limpeza

rm(list = ls())
gc()

# passo 2: preparação específica ------------------

## nascidos vivos -------------------------------

## importar

dbn <- import (here ('data/nascvivos_estadosreg_cada10anos.csv'), encoding='Latin-1')

## long para wide

dbn <- dbn %>% select (-Total) %>% pivot_longer(`2000`:`2020`)

## limpeza

colnames (dbn) <- c('local', 'ano', 'nnasc')

dbn$local <- dbn$local %>% str_remove('.. ')
dbn$local <- dbn$local %>% str_replace('Regi', 'Região ')

## número de mortes infantis ---------------------

## importar

dbm <- import (here ('data/obitinf_estadosreg_cada10anos.csv'), encoding='Latin-1')

## long para wide

dbm <- dbm %>% select (-Total) %>% pivot_longer (`2000`:`2020`)

## limpeza

colnames (dbm) <- c('local', 'ano', 'nobit')

dbm$local <- dbm$local %>% str_remove('.. ')
dbm$local <- dbm$local %>% str_replace('Regi', 'Região ')

## junção e cálculo ------------------------

db <- full_join(dbn, dbm)

db$tmi <- 1000*(db$nobit/db$nnasc)

db %>% count (local)
db$local <- ifelse (db$local=='Total', 'Brasil', db$local)

# passo 3: gráfico --------------------------------
## vamos fazer juntos ------------------

## gabarito --------------------
regioes <- c('Região Sul', 'Região Sudeste', 'Região Norte', 'Região Nordeste', 'Região Centro-Oeste', 'Brasil')

db %>%
  filter (ano=='2020') %>%
  filter (local %in% regioes) %>%
  ggplot (aes (x=reorder(local, desc(tmi)), y=tmi)) +
  geom_col (fill='lightblue') +
  geom_text (aes(label=round(tmi)),
             vjust=-1) +
  scale_y_continuous (limits=c(0,20)) +
  theme_classic() +
  labs (x=NULL,
        y=NULL,
        title='Taxa de mortalidade infantil até 1 ano de idade\npara o Brasil e suas regiões para em 2020',
        subtitle='Fonte: DataSUS.',
        caption='Workshop Python-R-ArcGis.')

ggsave ('resultados/exemplo1_graficos.png', height=5, width=5)



# EXEMPLO 2 - GRÁFICOS -----------------------------
# passo 1: preparação básica ------------

#já feita

# passo 2: preparação específica ----------------------

#já feita

# passo 3: gráfico -------------------

## vamos fazer juntos -------------------

## gabarito --------------------

db %>%
  filter (local %in% regioes) %>%
  ggplot (aes(x=reorder(local, desc(tmi)), y=tmi)) +
  geom_col (aes (fill=ano), position=position_dodge2(width=.9)) +
  geom_hline(aes(yintercept = 12, linetype = 'Meta do Banco Mundial'), color='#33a02c') +
  geom_text(aes(label=round(tmi), group=ano),
            position = position_dodge2(width=.9),
            vjust=-1) +
  scale_linetype_manual(values='dotted') +
  scale_fill_manual(values=c('#e0e0e0', '#4d9221', '#276419')) +
  scale_y_continuous(limits=c(0,30)) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs (x=NULL,
        y=NULL,
        fill=NULL,
        title='Taxa de mortalidade infantil até 1 ano de idade para o Brasil e suas regiões por ano',
        subtitle='Fonte: DataSUS.',
        caption='Workshop Python-R-ArcGis.',
        linetype=NULL)

ggsave ('resultados/exemplo2_graficos.png', width=10, height=6)

# EXEMPLO 3 - GRÁFICOS ------------------------------

## passo 1: preparação básica --------------------

#já feita

## passo 2: preparação específica ---------------------


## importar

dbm20 <- import (here ('data/obitinf_tipocausa_2020.csv'), encoding='Latin-1')
dbm10 <- import (here ('data/obitinf_tipocausa_2010.csv'), encoding='Latin-1')
dbm00 <- import (here ('data/obitinf_tipocausa_2000.csv'), encoding='Latin-1')

## limpar

dbm20$ano <- '2020'
dbm10$ano <- '2010'
dbm00$ano <- '2000'

dbm10$`2. Causas mal definidas` <- as.numeric(dbm10$`2. Causas mal definidas`)

dbm <- bind_rows(dbm20, dbm10, dbm00)

dbm$`1.1. Reduzível pelas ações de imunização` <- as.numeric(dbm$`1.1. Reduzível pelas ações de imunização`)

dbm <- dbm %>% pivot_longer(`1.1. Reduzível pelas ações de imunização`:Total)

colnames (dbm) <- c('local', 'ano', 'causa', 'nobit')


dbm %>% count (local)
dbm$local <- dbm$local %>% str_remove('.. ')
dbm$local <- dbm$local %>% str_replace('Regi', 'Região ')
dbm$local <- ifelse (dbm$local=='Total', 'Brasil', dbm$local)

dbm %>% count (causa)
dbm$causa <- case_when(dbm$causa == '1.1. Reduzível pelas ações de imunização' ~ 'Reduzível pelas ações de imunização',
                       dbm$causa == '1.2.1 Reduzíveis atenção à mulher na gestação' ~ 'Reduzível pela atenção à mulher na gestação',
                       dbm$causa == '1.2.2 Reduz por adequada atenção à mulher no parto' ~ 'Reduzível por adequada atenção à mulher no parto',
                       dbm$causa == '1.2.3 Reduzíveis adequada atenção ao recém-nascido' ~ 'Reduzível por adequada atenção ao recém-nascido',
                       dbm$causa == '1.3. Reduz ações diagnóstico e tratamento adequado' ~ 'Reduzível por ações de diagnóstico e tratamento adequado',
                       dbm$causa == '1.4. Reduz. ações promoção à saúde vinc. Aç. At' ~ 'Reduzível por ações adequadas de promoção à saúde,\nvinculadas a ações adequadas de atenção à saúde',
                       dbm$causa == '2. Causas mal definidas' ~ 'Causas mal definidas',
                       dbm$causa == '3. Demais causas (não claramente evitáveis)' ~ 'Demais causas',
                       dbm$causa == 'Total' ~ 'Total')

## percentuais

dbm_total <- dbm %>%
  filter (causa=='Total') %>%
  rename ('ntotal' = nobit) %>%
  select (-causa)

dbm <- left_join(dbm, dbm_total) %>%
  filter (causa!='Total')

## passo 3: gráfico ------------------------

## vamos fazer juntos ------------

## gabarito --------------

cores <- c('#8dd3c7', '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462', '#b3de69', '#fccde5')

dbm %>%
  filter (ano == '2020') %>%
  filter (local %in% regioes) %>%
  group_by (local, causa) %>%
  summarise (nobit=sum(nobit, na.rm=T),
             perc_causa = 100*nobit/ntotal) %>%
  ggplot (aes (x=perc_causa, y=reorder(causa, perc_causa))) +
  geom_col (aes(fill=causa)) +
  geom_text (aes(x=1,
                 label=paste0(round(perc_causa,0), '%')),
             hjust=0) +
  facet_grid(cols=vars(local)) +
  scale_fill_manual(values=cores) +
  theme_classic() +
  theme (legend.position = 'none') +
  labs (x=NULL,
        y=NULL,
        title='Mortes infantis por tipo de causa e região',
        subtitle='Fonte: DataSUS.',
        caption='Workshop Python-R-ArcGis')

ggsave ('resultados/exemplo3_graficos.png', width=12, height=5)

# EXEMPLO 4 - GRÁFICOS --------------------------

# passo 1: preparação básica ------------------

#já feito

# passo 2: preparação específica --------------------

glimpse (db)

# variações

dbvar <- db %>%
  select (-c(nnasc)) %>%
  pivot_wider(names_from = 'ano', values_from = c('tmi', 'nobit')) %>%
  mutate (var2000_2020 = tmi_2020-tmi_2000)

# regiões

norte <- c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins")

nordeste <- c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe")

centro_oeste <- c("Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul")

sudeste <- c("Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo")

sul <- c("Paraná", "Rio Grande do Sul", "Santa Catarina")

dbvar$regiao <- case_when(dbvar$local %in% norte ~ 'Região Norte',
                          dbvar$local %in% nordeste ~ 'Região Nordeste',
                          dbvar$local %in% centro_oeste ~ 'Região Centro-Oeste',
                          dbvar$local %in% sudeste ~ 'Região Sudeste',
                          dbvar$local %in% sul ~ 'Região Sul')

dbvar %>% count (regiao)

# passo 3: gráfico -----------------------

## vamos fazer juntos -----------

## gabarito -----------------

dbvar %>%
  filter (regiao != is.na(regiao)) %>%
  ggplot (aes(x=tmi_2020, y=var2000_2020*(-1)))  +
  geom_point(aes(size=nobit_2020/1000, color=regiao), alpha=.5) +
  geom_hline(aes(yintercept = 10), linetype='dotted') +
  geom_text(aes(label='Redução de 10',
                x=16, y=11)) +
  geom_hline(aes(yintercept = 15), linetype='dotted') +
  geom_text(aes(label='Redução de 15',
                x=16, y=16)) +
  scale_color_manual(values=cores[3:8]) +
  theme_classic() +
  labs (y='Declínio na\ntaxa de mortalidade nfantil',
        x='Taxa de Mortalidade Infantil de 2020',
        size='Mortes infantis\n(milhares), 2020',
        color=NULL,
        title='Variação da taxa de mortalidade infantil dos Estados brasileiros',
        subtitle='Fonte: DataSUS.',
        caption='Workshop Python-R-ArcGis.')

ggsave ('resultados/exemplo4_graficos.png', width=10, height = 7)


# TAREFA DE CASA -----------------

# Use as bases de dados 'nascvivos_mun' e 'obitinf_mun' para reproduzir o gráfico do exemplo 4 para municípios.
# Dica: os dois primeiros dígitos do código municipal dizem qual o Estado! Você pode ver no seguinte endereço: <https://www.ibge.gov.br/explica/codigos-dos-municipios.php>.

