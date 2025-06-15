library(tidyverse)
install.packages("rgbif")
library(rgbif)

##baixar ocorrências Gbif
?occ_search
velvet <- occ_search(scientificName = "Barbourisia rufa", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)
dim(velvet)
dim(velvet$data)
# checar campos
# checar campos
velvet$data %>% names
gbif_issues
# checar problemas reportados
issues_gbif <- velvet$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)
##
velvet1 <- velvet$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) 
library(dplyr)
velvet1 <- velvet1 %>% 
  distinct() 
# checar niveis dos fatores
lapply(velvet1, unique)
library(bdc)
library(CoordinateCleaner)
# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = velvet1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")
# checar coordenadas válidas e próximas a capitais
cl <- velvet1 %>%
  CoordinateCleaner::clean_coordinates(species = "acceptedScientificName",
                                       lat = "decimalLatitude",
                                       lon = "decimalLongitude",
                                       tests = c("capitals", 
                                                 "centroids","equal", 
                                                 "gbif", "institutions", 
                                                 "outliers", "seas", 
                                                 "zeros"))
# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.cap`)) +
  coord_quickmap() +
  theme_classic()
# pontos no mar
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.cen`)) +
  coord_quickmap() +
  theme_classic()
# investigar niveis suspeitos
velvet1 %>% 
  distinct(waterBody) %>% 
  pull()
# waterBody
velvet1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
  geom_bar(stat = 'identity') 
# fonte das regioes erradas
velvet1 %>% 
  filter(waterBody %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire")) %>% 
  distinct(datasetName)
velvet1 <- velvet1 %>% 
  filter(!waterBody %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire"))
# 14 ocorrencias
velvet1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science")) %>% 
  data.frame()
# 14 ocorrencias
velvet1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science")) %>% 
  data.frame()
# filtrar todas do dataset suspeito
velvet_noDiveboard <- velvet1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))

velvet_noDiveboard %>% 
  filter(decimalLatitude > 25) %>% 
  arrange(-decimalLatitude) %>% 
  data.frame()

velvet_ok <- velvet_noDiveboard %>% 
  filter(decimalLatitude < 31) 
#####
install.packages("ggmap")
library(ggmap)
install.packages("maps")
library(maps)
install.packages("mapdata")
library(mapdata)

world <- map_data('world')

# checar pontos
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = velvet_ok, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Barbourisia rufa")))

# checar profundidade
velvet_ok %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

#####OBIS#####
#Com os dados tratados, a partir daqui um grafico com
a profundidade e em qual oceano o Barbourisia se encontra

velvet <- robis::occurrence("Barbourisia rufa")
# checar dados
names(velvet)

velvet1 <- velvet %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

# check problemas reportados (flags)
velvet1 %>% 
  distinct(flags)

# check NA em datasetName
velvet1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         is.na(datasetName)) %>% 
  distinct(waterBody)

# depth ok
velvet1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Caribbean Sea", "atlantique")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

# checar niveis
#Gerar um gráfico de onde se tem registros dessa espécie
velvet1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Caribbean Sea", "atlantique")) %>% 
  lapply(., unique)

# aplicar filtros
velvet_ok <- velvet1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Caribbean Sea", "atlantique"))
library(ggplot2)
install.packages("ggplot")

# plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = velvet_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Barbourisia rufa")))

velvet_final <- velvet_ok %>% 
  filter(decimalLongitude > 0 | decimalLongitude < -100)

# plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = velvet_final, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Barbourisia rufa")))

# unir GBIF e OBIS

# ver diferencas
setdiff(names(velvet_ok), names(velvet_ok))
setdiff(names(velvet_final), names(velvet_final))

##
all_data <- bind_rows(velvet_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      velvet_final %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
##
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>%   
  rownames_to_column("occ") %>%  
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%  
  mutate(scientificName = "Barbourisia rufa") %>%  
  dplyr::select(-rn)  

# mapear ocorrencias
#Por fim, depois de verificado se há resultados duplicados, 
um mapa de ocorência da espécie com os resultados do GBIF e OBIS
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Barbourisia rufa")))
loadhistory(".Rhistory")##terminei em casa
if (!dir.exists("data")) dir.create("data")
write.csv(all_data, "data/occ_GBIF-OBIS_par_hepa.csv", row.names = FALSE)

# funcao para classificar ocorrencias suspeitas
flag_outlier <- function(df, species){
  # funcao para classificar ocorrencias suspeitas
  # baseada no calculo do centroide de todas ocorrencias
  # indica como 'check' as ocorrencias que tem distancias até o centroide
  # acima do 90th quantil (default) das distancias calculadas
  dados <- df %>% 
    dplyr::filter(scientificName == species); 
  
  dados2 <- geosphere::distVincentyEllipsoid(
    dados %>%
      summarise(centr_lon = median(decimalLongitude),
                centr_lat = median(decimalLatitude)),
    dados %>% 
      dplyr::select(decimalLongitude, decimalLatitude)
  ) %>% 
    bind_cols(dados) %>% 
    rename(dist_centroid = '...1') %>% 
    mutate(flag = ifelse(dist_centroid < quantile(dist_centroid, probs = 0.90), "OK",
                         ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.90) & dist_centroid < quantile(dist_centroid, probs = 0.95), "check > Q90",
                                ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.95), "check > Q95", "OK")))) 
  print(dados2)
  
}  
# classificar ocorrências
marcados <- velvet$data %>% 
  data.frame() %>% 
  dplyr::select(scientificName, decimalLongitude, decimalLatitude, datasetName) %>% 
  distinct() %>% 
  flag_outlier(., species = "Barbourisia rufa (Parr, 1945)")  
library(ggplot2)
library(dplyr)
# mapa
#mapa checado se há outliers
ggplot() 
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados, 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = flag)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Barbourisia rufa")))
View(marcados)
library(maps)
library(mapdata)
world <- map_data("world")
#########
library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
library(stringr) 
marcados <- velvet_final %>%
  filter(str_detect(scientificName, fixed("Barbourisia rufa"))) %>%
  select(scientificName, decimalLongitude, decimalLatitude, datasetName) %>%
  distinct()
nrow(marcados)
world <- map_data("world")
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "gray60", color = "white") +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados,
             aes(x = decimalLongitude, y = decimalLatitude),
             color = "darkred", size = 2, alpha = 0.7) +
  labs(x = "Longitude", y = "Latitude",
       title = expression(italic("Barbourisia rufa"))) +
  theme(legend.position = "none")
library(CoordinateCleaner)
flags <-
  clean_coordinates(
    x = marcados,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    tests = c("equal", "gbif",
              "zeros", "seas")
  )
###verificar outliers
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados %>% 
               filter(flag != "OK"), 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = datasetName)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Barbourisia rufa")))
###criar coluna flag
marcados <- marcados %>%
  mutate(flag = ifelse(decimalLongitude >= -180 & decimalLongitude <= 180 &
                         decimalLatitude >= -90 & decimalLatitude <= 90,
                       "OK", "Check"))
##não apareceu outliers no mapa, essa espécie possui poucos registros de
ocorrência, devido ao seu habitat, o Barbourisia vive em águas profundas, sendo
visto poucas vezes
