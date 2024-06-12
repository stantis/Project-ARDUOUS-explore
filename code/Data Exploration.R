# This is the code used to generate all tables and figures in the associated publication. 
# The data can be downloaded at either IsoBank LINK or IsoArch LINK 
# Place downloaded .csv in folder 'input', labeled 'data.csv' for the below code to work as-is
# Setup -------------------------------------------------------------------

library(dplyr); library(tidyr); library(ggplot2); library(forcats); library(googlesheets4); 
library(geodata); library(tidyterra); library(terra); library(viridis); library(purrr)

#df <- read_sheet("https://docs.google.com/spreadsheets/d/1zEkz_KKvY1ek7ejBohuy7ZazTrOwHazpraRPD6lCtZY")
 df <- read.csv("input/data.csv") %>% 
   mutate(collection_decimal_latitude = as.numeric(collection_decimal_latitude), 
          ratio = X87Sr_86Sr)

# let's simplify some naming schema and classifications
df <- df %>% 
  mutate(material_type_simp = case_when(
    material_type == 'inorganic-organic composite : inorganic material : dry deposition' ~ 'dry deposition', 
    material_type == 'inorganic-organic composite : rock' ~ 'rock', 
    material_type == 'inorganic-organic composite : rock : bulk rock' ~ 'rock', 
    material_type == 'inorganic-organic composite : rock : rock mineral separate' ~ 'rock mineral', 
    material_type == 'inorganic-organic composite : sediment' ~ 'sediment', 
    material_type == 'inorganic-organic composite : sediment : sediment mineral' ~ 'sediment', 
    material_type == 'inorganic-organic composite : soil' ~ 'soil', 
    material_type == 'inorganic-organic composite : soil : soil mineral' ~ 'soil mineral', 
    material_type == 'inorganic-organic composite : water : total water' ~ 'water', 
    material_type == 'inorganic-organic composite : water : water particulate' ~ 'water particulate', 
    material_type == 'inorganic-organic composite : water : water solute' ~ 'water solute', 
    material_type == 'organic composite : lichen' ~ 'lichen', 
    material_type == 'organic composite : litter' ~ 'litter', 
    material_type == 'organism : animal : animal tissue' ~ 'animal, undefined tissue', 
    material_type == 'organism : animal : animal tissue : antler' ~ 'antler', 
    material_type == 'organism : animal : animal tissue : bone' ~ 'bone', 
    material_type == 'organism : animal : animal tissue : egg : shell' ~ 'eggshell', 
    material_type == 'organism : animal : animal tissue : excreta' ~ 'excreta', 
    material_type == 'organism : animal : animal tissue : feather' ~ 'feather', 
    material_type == 'organism : animal : animal tissue : hair' ~ 'hair', 
    material_type == 'organism : animal : animal tissue : otolith' ~ 'otolith', 
    material_type == 'organism : animal : animal tissue : shell' ~ 'shell', 
    material_type == 'organism : animal : animal tissue : tooth' ~ 'tooth', 
    material_type == 'organism : animal : animal tissue : tooth : dentine' ~ 'dentine', 
    material_type == 'organism : animal : animal tissue : tooth : enamel' ~ 'enamel', 
    material_type == 'organism : animal : animal tissue : tusk' ~ 'tusk', 
    material_type == 'organism : animal : whole animal' ~ 'whole animal', 
    material_type == 'organism : plant : plant tissue' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : bark' ~ 'plant bark', 
    material_type == 'organism : plant : plant tissue : blade' ~ 'plant blade', 
    material_type == 'organism : plant : plant tissue : flowering body' ~ 'flowering body', 
    material_type == 'organism : plant : plant tissue : leaf' ~ 'leaf', 
    material_type == 'organism : plant : plant tissue : plant fruiting body' ~ 'fruit', 
    material_type == 'organism : plant : plant tissue : root' ~ 'root', 
    material_type == 'organism : plant : plant tissue : seed' ~ 'seed', 
    material_type == 'organism : plant : plant tissue : whole plant' ~ 'whole plant', 
    material_type == 'organism : plant : plant tissue : wood' ~ 'wood'
  )
  )

df <- df %>% 
  mutate(material_type_group = case_when(
    material_type == 'inorganic-organic composite : inorganic material : dry deposition' ~ 'dry deposition', 
    material_type == 'inorganic-organic composite : rock' ~ 'rock', 
    material_type == 'inorganic-organic composite : rock : bulk rock' ~ 'rock', 
    material_type == 'inorganic-organic composite : rock : rock mineral separate' ~ 'rock', 
    material_type == 'inorganic-organic composite : sediment' ~ 'sediment', 
    material_type == 'inorganic-organic composite : sediment : sediment mineral' ~ 'sediment', 
    material_type == 'inorganic-organic composite : soil' ~ 'soil', 
    material_type == 'inorganic-organic composite : soil : soil mineral' ~ 'soil', 
    material_type == 'inorganic-organic composite : water : total water' ~ 'water', 
    material_type == 'inorganic-organic composite : water : water particulate' ~ 'water', 
    material_type == 'inorganic-organic composite : water : water solute' ~ 'water', 
    material_type == 'organic composite : lichen' ~ 'organic other', 
    material_type == 'organic composite : litter' ~ 'organic other', 
    material_type == 'organism : animal : animal tissue' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : antler' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : bone' ~ 'bone', 
    material_type == 'organism : animal : animal tissue : egg : shell' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : excreta' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : feather' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : hair' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : otolith' ~ 'otolith', 
    material_type == 'organism : animal : animal tissue : shell' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : tooth' ~ 'tooth', 
    material_type == 'organism : animal : animal tissue : tooth : dentine' ~ 'tooth', 
    material_type == 'organism : animal : animal tissue : tooth : enamel' ~ 'tooth', 
    material_type == 'organism : animal : animal tissue : tusk' ~ 'tooth', 
    material_type == 'organism : animal : whole animal' ~ 'animal, other', 
    material_type == 'organism : plant : plant tissue' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : bark' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : blade' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : flowering body' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : leaf' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : plant fruiting body' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : root' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : seed' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : whole plant' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : wood' ~ 'plant'
  ))

animaldf <- subset(df, material_type_group %in% c("animal, other", "bone", "tooth", "otolith"))

animaldf %>%
  ggplot(aes(x = fct_infreq(material_type_group))) +
  geom_bar() +
  labs(x = "group") + 
  theme_classic()

table(animaldf$scientific_name)

plant <- c("Abies", "Acacia", "Abutilon", "Acanthus", "Acca sellowiana", "Acer", 
           "Adesmia", "Aegopodium", "Aesculus", "Agathis", "Agave")

# stopped at letter N
# migratory or home ranges 100+ across
large <- c("Acrocephalus schoenobaenus", "Antilocapra americana", "Canis lupus", 
           "Cervus canadensis", "Cervus elaphus", "Cervus elaphus L.", "Ctenopharyngodon idella", 
           "Alosa sapidissima", "Calidris alpina", "Damaliscus pygargus", "Dendroica caerulescens", 
           "Elephantidae", "Esox", "Esox lucius", "Gazella subgutturosa", "Haematopus", 
           "Kobus kob", "Lota lota", "Mammuthus", 
)

# migratory or home ranges between 6-99 km across
medium <- c("Aepyceros melampus", "Alligator sinensis", "Antidorcas marsupialis",
            "Archaeolemur", "Archaeolemur edwardsi", "Archaeolemur majori", "Arianta Arbustorum", 
            "Avahi laniger", "Babakotia radofilai", "Bison", "Bison bison", "Bos", 
            "Bos taurus", "Bos taurus L.", "Bovidae", "Caiman yacare", "Capra",
            "Capra aegagrus hircus", "Capreolus", "Capreolus capreolus", "Capreolus capreolus L.", 
            "Caprinae", "Caprini", "Castor canadensis", "Cathartidae", "Cervidae", 
            "Cercopithecus ascanius", "Cheirogaleus crossleyi", "Cingulata", "Crocuta crocuta", 
            "Cynomys", "Atherinosoma microstoma", "Brachyplatystoma rousseauxii",
            "Cerdocyon thous", "Cyprinus carpio", "Dasypodidae sp.", "Diceros bicornis", 
            "Dicotyles tajacu", "Equidae", "Eulemur rubriventer", "Eulemur rufifrons", 
            "Felis", "Hexaprotodon guldbergi", "Hippopotamus lemerlei", "Hippotragus niger", 
            "Iguanidae", "Lagomorpha", "Lama vicugna", "Lemur catta", "Leopardus sp.", 
            "Leopardus wiedii", "Lepilemur", "Lepilemur petteri", "Leporidae", "Lepus", 
            "Lepus americanus", " Lepus saxatilis", "Lomo guanicoe", "M. agilis", 
            "M. giganteus", "M. piceus", "M. rufus", "Martes martes L.", "Megaladapis madagascariensis", 
            "Meleagris gallopavo", "Microcebus griseorufus", "Micropterus dolomieu", 
            "Micropterus salmoides", "Mustela frenata", "Mustelidae", "Myocastor coipus", 
            
)

# small home ranges, 5km or less
small <- c("Arvicola terrestris L", "Bivalvia", "Blattodea", "Bufonidae", "Bulimulidae", 
           "Cavia", "Cavia porcellus", "Cepaea hortensis", "Cepaea nemoralis", "Chilostoma sp.",  
           "Clausiliidae", "Corbicula sp.", "Cornu aspersum", "Cricetidae", "Cryptomys hottentotus", 
           "Ctenomys sp.", "Arvicolinae", "Eligmodontia sp.", "Eliurus majori", 
           "Eliurus minor", "Erinaceus europ.", "Fruticicolidae", "Galea sp.", "Gastropoda", 
           "Geomyidae", "Geomys bursarius", "Gerbilliscus brantsii", "Gyraulus convexiusculus", 
           "Helix aspersa", "Helix pomatia", "Helix sp.", "Marmota xaviventris", 
           "Meriones sp.", "Micaelamyus namaquensis", "Microtus arvalis/agrestis", 
           "Mollusca", "Mus", "Mus musculus", 
           
)

# Data Visualization ------------------------------------------------------

df %>%
  ggplot(aes(x = fct_infreq(material_type_group))) +
  geom_bar() +
  labs(x = "group") + 
  theme_classic()

df %>%
  ggplot(aes(x = ratio)) +
  geom_histogram(binwidth = 0.001, fill = 'midnightblue') +
  #geom_density(fill = 'midnightblue', alpha = 0.6) +
  labs(x = expression(paste(""^{87},"Sr/"^86,"Sr")), 
       y = "Count") +
  xlim(0.7, 0.85) + 
  theme_classic()

ggplot() +
  geom_histogram(data = df, aes(x = '87Sr/86Sr'))
# World Map ---------------------------------------------------------------

w <- world(path=tempdir())
v <- vect(df, geom= c("collection_decimal_longitude", "collection_decimal_latitude"), 
          crs = "")
crs(v) = crs(w)

plot(w)
plot(v, add = T)

#tiff(filename = paste(i,"world10.tiff"), 
#    width = 1402, height = 748, units = "px", pointsize = 18,
#   compression = c("lzw"))
terra::plot(w, col = "grey96",  ylim = c(-55 , 83.500904), axes = F)
terra::plot(v, add = T, legend = F, col = (c('#50b691')), cex = 0.5)
#north(type = 2, label = '', xy = 'bottomleft') 
#sbar(2000, 'bottomleft', type="bar", below="km", label=c('', 500, 1000, 2000), cex = 0.8)

ggplot() +
  geom_spatvector(data = w, fill = 'grey90') + 
  geom_spatvector(data = v, aes(color = material_type_group), size = 0.4) +
  #scale_color_viridis(option = "turbo", discrete = T) + 
  scale_colour_brewer(palette = "Paired") +
  theme_light() + 
  theme(legend.position = "bottom", 
        #panel.background = element_rect(fill = 'grey20')
        ) + 
  labs(color = "Material Type (simplified)") + 
  ylim(-55, 83.5) +
  guides(colour = guide_legend(override.aes = list(size = 2)))
ggsave("output/Figure1.tiff")
# Unique References/DOIS --------------------------------------------------

dois <- dplyr::distinct(df, related_publication_id)
refs <- dplyr::distinct(df, related_publication_citation)
write.csv(file = 'output/dois.csv', dois) #easily save the reference dois
write.csv(file = 'output/refs.csv', refs) #easily save the reference list

# Check PACHAMAMA for Sr --------------------------------------------------

test <- read_delim("input/saaid_v.2.0_2023_humans.csv", 
        delim = ";", escape_double = FALSE, trim_ws = TRUE)

test <- test %>% rename(ratio = '87Sr/86Sr') %>%
  filter(!is.na(ratio)) %>% 
  select(Entry, Country, Site_Name, Sample_Id, ratio, Original_Full_Reference, Link_to_source)

check <- dplyr::distinct(test, Original_Full_Reference)
write.csv(file = 'output/checkhumans.csv', check)


plants <- read_delim("input/saaid_v.2.0_2023_plants.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

plants <- plants %>% rename(ratio = '87Sr/86Sr') %>%
  filter(!is.na(ratio)) %>% 
  select(Entry, Country, Site_Name, Sample_Id, ratio, Original_Full_Reference)

check <- dplyr::distinct(plants, Original_Full_Reference)
write.csv(file = 'output/checkplants.csv', check)
