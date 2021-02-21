rm(list = ls())
pacman::p_load('tidyverse', 'geobr', 'sf')


temp <- tempfile()
url = 'http://download-integrasus.saude.ce.gov.br/download'
download.file(url, temp, quiet = TRUE, mode = "wb")
data <- read_csv2(unz(temp, "casos_coronavirus_2021_02_20.csv"))


### Serie diária do covid19 CE
dataConf <- data %>% #filter(codigoMunicipioCaso == 230400) %>%
  select(identificadorCaso, codigoMunicipioCaso, municipioCaso, bairroCaso, estadoCaso,
         idadeCaso, sexoCaso, faixaEtaria, dataNascimento, dataInicioSintomas,
         dataColetaExame, dataResultadoExame, resultadoFinalExame) %>%
  mutate(dataInicioSintomas = ifelse(dataInicioSintomas == 'NaT', NA, dataInicioSintomas),
         data1 = as.Date(coalesce(dataInicioSintomas, dataColetaExame, dataResultadoExame)),
         estadoCaso = if_else(is.na(estadoCaso), 'CE', estadoCaso)) %>%
  filter(estadoCaso == 'CE' & resultadoFinalExame == 'Positivo') %>%
  distinct(identificadorCaso, .keep_all = T) %>% drop_na(data1) %>%
  #filter(data1 > '2020-10-31') %>%
  group_by(data1) %>% summarise(conf = n()) %>% mutate(confCum = cumsum(conf))



#dataConf %>% #filter(data1 > '2020-10-31') %>%
ggplot(dataConf, aes(x = data1, y = conf))+
  geom_line(color = 'purple')+
  scale_x_date(breaks = "30 days", date_labels = "%d%b%Y")+
  scale_y_continuous(breaks = seq(0, max(dataConf$conf), by = 300))+
  theme_bw()


### Municipios e casos confirmados de covid
mun_covid <- data %>%
  select(identificadorCaso, codigoMunicipioCaso, municipioCaso, bairroCaso, estadoCaso,
         idadeCaso, sexoCaso, faixaEtaria, dataNascimento, dataInicioSintomas,
         dataColetaExame, dataResultadoExame, resultadoFinalExame) %>%
  mutate(dataInicioSintomas = ifelse(dataInicioSintomas == 'NaT', NA, dataInicioSintomas),
         data1 = as.Date(coalesce(dataInicioSintomas, dataColetaExame, dataResultadoExame)),
         estadoCaso = if_else(is.na(estadoCaso), 'CE', estadoCaso)) %>%
  filter(estadoCaso == 'CE' & resultadoFinalExame == 'Positivo') %>%
  #& (data1 >= '2020-08-15' & data1 >= '2020-10-15')) %>%
  distinct(identificadorCaso, .keep_all = T) %>%
  group_by(codigoMunicipioCaso) %>%
  summarise(conf = n()) %>%
  drop_na(codigoMunicipioCaso)


## População
#nome = c('mun', 'cod_mun', 'gentilico', 'prefeito', 'area', 'pop20')
#tipos = c('text', 'numeric', 'numeric', 'text', 'numeric')
pop <- readxl::read_xlsx('2020CEpop.xlsx', skip = 2, n_max = 185)[,1:6]
colnames(pop) <- c('mun', 'cod_mun', 'gentilico', 'prefeito', 'area', 'pop20')

pop <- pop %>% mutate(cod_mun1 = as.numeric(cod_mun),
                      cod_mun1 = as.numeric(substr(cod_mun1,1,6))) %>%
  select(cod_mun1, area, pop20)




## Geometria
mun <- read_municipality(code_muni = 'CE', year = 2019) %>%
  mutate(codigoMunicipioCaso = as.numeric(substr(code_muni,1,6)),
         name_muni = as.character(name_muni)) %>%
  left_join(mun_covid, by = c('codigoMunicipioCaso')) %>%
  left_join(pop, by = c('codigoMunicipioCaso'='cod_mun1')) %>%
  mutate(conf2 = (conf/pop20)*1000,
         conf3 = (conf/area))
mun <- cbind(mun, st_coordinates(st_centroid(mun$geom)))


## Casos confirmados por 100mil/hab
rm(data)
lista = mun$name_muni

for(i in lista) {
  show(g <- mun %>% filter(name_muni == i) %>%
         #mutate(conf_esc = cut(conf2, c(4000, 8000, 12000))) %>%
         ggplot()+
         geom_sf(aes())+
         #geom_label(aes(X, Y, label = round(conf2,2)), size=0.8, color = 'white') +
         geom_text(aes(X, Y, label = round(conf2,2)), size=8, color = 'Black') +
         #geom_point(data=pble1, aes(x=pble1$X, y=pble1$Y,
         #                          color=pble1$TRATAMENTO, group=pble1$TRATAMENTO),
         #         size=1.5, alpha=ifelse(pble1$TRATAMENTO==1, 0.1,0.1))+
         #annotate(geom = "text", x = -32.42409	, y = -3.854335, label = "Fernando de Noronha",
         #        fontface = "italic", color = "darkblue", size = 3) +
         theme_minimal() +
         #scale_fill_viridis_c(option = 6, begin = 0.2, end = 0.8) +
         #scale_fill_distiller(type = "seq",
         #                    palette = "Reds",
         #                   direction = 1) +
         labs(title = i, x = "", y = "", colour = "", fill = "Casos (mil/hab)")+
         theme(text=element_text(family="Times", face="bold", size=12),
               legend.position="right", #panel.grid = element_line(colour = "transparent"),
               panel.background = element_blank(),
               axis.text = element_blank(),
               axis.ticks = element_blank()))
  #facet_wrap(vars(codigoMunicipioCaso))

  assign(paste0('g', i), g)
}

