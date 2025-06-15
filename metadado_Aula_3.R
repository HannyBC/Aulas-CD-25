##Metadados Aula 3##

#Aula com o objetivo de: acessar bancos de dados abertos, o banco de dado
utilizado foi o GBIF. A escolha da espécie a ser pesquisada foi a 
critério do aluno. Neste caso, escolhi a espécie do peixe Barbourisia rufa

#Inicialmente foram utilizados os pacotes tidyverse e rgbif

#O código "occ_search()" vem do pacote "rgbif", e tem como função buscar os 
registros referentes à espécie pesquisada no banco de dados do GBIF

#O código abaixo foi utilizado como forma de baixar as ocorrÊncias com
coordenadas geográficas disponíveis, isso será importante para a confecção 
do mapa, e também nesse código, pede-se para excluir as ocorrências sem essas
coordendas

#?occ_search
velvet <- occ_search(scientificName = "Barbourisia rufa", 
                     hasCoordinate = TRUE,
                     hasGeospatialIssue=FALSE)#

#O objeto para esse caso foi nomeado velvet, por ser um dos nomes populares que
essa espécie é conhecida

#Busca de dataframe sobre a espécie e informções das variáeis como:
latitude, longitude, país etc
#Com o dataframe selecionado e com as devidas correções, o código:
"filter(code %in% issues_gbif)" é responsável por filtrar esses dados e 
retorná-los para nós

#Informações de interesse foram selecionadas através de:
dplyr::select

#Informações duplicadas removidas:
library(dplyr)
velvet1 <- velvet1 %>% 
  distinct() 
#Em: bdc::bdc_coordinates_outOfRange; é um pacote de padronização de dados
e em seguida foi verificado se tinha amostras fora da faixa determinada
#Depois, teste aplicado a fim de corrigir dados duplicados e afins, em capitais,
cidades, países e etc