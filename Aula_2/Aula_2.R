Aula_2 <-read.table("planilha_aula_2.txt",header=T,stringsAsFactors = T)
library(tidyverse)
iris %>% 
  select(Species, Sepal.Length:Petal.Width) %>% 
  pivot_longer(cols = -Species, names_to = "variavel", 
               values_to = "valores") %>% 
  ggplot(aes(x = valores, fill = Species)) +
  geom_histogram() +
  facet_wrap(~ variavel, scales = 'free_x') +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "tamanho (mm)") +
  scale_fill_discrete(
    expression(bold("Species:")),
    labels = c(expression(italic("Iris setosa")), 
               expression(italic("Iris versicolor")), 
               expression(italic("Iris virginica"))))  
install.packages("validate")
library(validate)
#####
rules <- validator(in_range(latitude, min = -90, max = 90),
                   in_range(latitude, min = -180, max = 180),
                   is.character(site),
                   is.numeric(data),
                   all_complete(especie))

out <- confront(Aula_2, rules)
summary(out)
plot(out)
install.packages("taxadb")
library(taxadb)
          
#### check taxa

species <- Aula_2 %>% 
  distinct(especie) %>% 
  pull() %>% 
  c("Iris murchosa", .) %>% 
  filter_name(., provider = "itis") %>% 
  data.frame() %>% 
  bind_cols(especie = Aula_2 %>% 
              distinct(especie) %>% 
              pull())
####
library(vegan)
####
especie <- Aula_2 %>%
  distinct(especie) %>%
  mutate(
    acceptedNameUsageID = paste0("ID_", row_number()),
    scientificName = especie  
  )

Aula_2 <- Aula_2 %>% 
  dplyr::mutate(eventID = paste(site, data, sepal = "_"), # create indexing fields 
                occurrenceID = paste(site, data, amostra, sepal = "_")) %>% 
  left_join(especie %>% 
              select(especie, acceptedNameUsageID, scientificName)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = longitude, # rename fields according to DwC 
                decimalLatitude = latitude,
                eventDate = data) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")

## create eventCore
eventCore <- Aula_2 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 

## create occurrence
occurrences <- Aula_2 %>% 
  select(eventID, occurrenceID, scientificName, acceptedNameUsageID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 

## create measurementsOrFacts [com erro]
eMOF <- Aula_2 %>% 
  select(eventID, occurrenceID, recordedBy, sepal_lenght_cm:sepal_Width_cm) %>%  
  pivot_longer(cols = sepal_lenght_cm:sepal_Width_cm,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("sepal_lenght_cm", "sepal_Width_cm", "petal_Width_cm", "petal_lenght_cm"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))
names(Aula_2)
names(Aula_2)[names(Aula_2) == "sepal_lenght_cm"] <- "sepal_length_cm"
MOF <- Aula_2 %>%
  select(eventID, occurrenceID, recordedBy, sepal_length_cm:sepal_width_cm) %>%
  pivot_longer(
    cols = c(sepal_length_cm, sepal_width_cm, petal_length_cm),
    names_to = "measurementType",
    values_to = "measurementValue"
  ) %>%
  mutate(
    measurementUnit = "cm",
    measurementType = plyr::mapvalues(
      measurementType,
      from = c("sepal_length_cm", "sepal_width_cm", "petal_length_cm"),
      to   = c("sepal length", "sepal width", "petal width", "petal length")
    ))
names(Aula_2)
