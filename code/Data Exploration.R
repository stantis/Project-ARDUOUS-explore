# This is the code used to generate all tables and figures in the associated publication. 
# The data can be downloaded at either IsoBank LINK or IsoArch LINK 
# Place downloaded .csv in folder 'input', labeled 'data.csv' for the below code to work as-is
# Setup -------------------------------------------------------------------

library(dplyr); library(tidyr); library(ggplot2); library(forcats); 
library(geodata); library(tidyterra); library(terra); library(viridis); library(purrr)
library(ggpubr)

files = list.files(
  path = "input/",
  pattern = "*.csv"
  )

df <- lapply(paste0("input/",files), read.csv)
df <- as.data.frame(df) %>% 
  rename(Sr_ratio = 'X87Sr_86Sr', 
         Sr_ratio_error = 'X87Sr_86Sr_error_1SD')

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
    material_type == 'organism : animal : excreta' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : feather' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : hair' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : otolith' ~ 'otolith', 
    material_type == 'organism : animal : animal tissue : shell' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : tooth' ~ 'tooth', 
    material_type == 'organism : animal : animal tissue : tooth : dentin' ~ 'tooth', 
    material_type == 'organism : animal : animal tissue : tooth : enamel' ~ 'tooth', 
    material_type == 'organism : animal : animal tissue : tusk' ~ 'tooth', 
    material_type == 'organism : animal : whole animal' ~ 'animal, other', 
    material_type == 'organism : plant : plant tissue' ~ 'plant',
    material_type == 'organism : plant : whole plant' ~'plant',
    material_type == 'organism : plant : plant tissue : unknown' ~'plant',
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

# migratory or home ranges 100+ across
large <- c("Acrocephalus schoenobaenus", "Alces alces", "Antilocapra americana", 
           "Bison", "Bison bison", "Canis lupus", "H. molitrix", "Leopardus pardalis",
           "Cervus", "Cervus canadensis", "Cervus elaphus", "Cervus elaphus L.", 
           "Charadrius hiaticula", "Ctenopharyngodon idella", "Damaliscus dorcas dorcas bontebok", 
           "Damaliscus lunatus", "Equid sp.", "Equus", "Equus sp.", "Equus cabalus",
           "Alosa sapidissima", "Calidris alpina", "Damaliscus pygargus", "Dendroica caerulescens", 
           "Elephantidae", "Esox", "Esox lucius", "Gazella subgutturosa", "Haematopus", 
           "Kobus kob", "Lota lota", "Mammuthus", "Numenius", "Numenius phaeopus", 
           "Odocoileus", "Odocoileus virginianus", "Oncorhynchus tshawytscha", 
           "Pluvialis apricaria", "Puma", "Rangifer", "Rangifer tarandus", 
           "Salminus brasiliensis", "Salmo salar", "Sander lucioperca", "Sander vitreus", 
           "Setophaga caerulescens", "Syncerus caffer caffer", "Tachycineta bicolor", 
           "Tringa totanus", "Vanellinae", "Kobus ellipsiprymnus", "M. primigenius", 
           "Phoca groenlandica", "Pusa hispida", "R. tarandus", "Raphicerus campestris steenbok",
           "Rhinocerotidae", "Ursus arctos"
)

# migratory or home ranges between 6-99 km across
medium <- c("Aepyceros melampus", "Alligator sinensis", "Antidorcas marsupialis", "Aplodinotus grunniens",
            "Arapaima spp", "Erethizontidae", "Erinaceinae", "Erinaceus europaeus", "Erinaceus europeus",
            "Archaeolemur", "Archaeolemur edwardsi", "Archaeolemur majori", "Arianta arbustorum", 
            "Avahi laniger", "Babakotia radofilai", "Bos", "Castor fiber",
            "Bos taurus", "Bos taurus L.", "Bovidae", "Caiman yacare", "Capra",
            "Capra aegagrus hircus", "Capreolus", "Capreolus capreolus", "Capreolus capreolus L.", 
            "Caprinae", "Caprini", "Castor canadensis", "Cathartidae", "Cervidae", 
            "Cercopithecus ascanius", "Cercopithecus mitis", "Cheirogaleus crossleyi", 
            "Cingulata", "Crocuta crocuta", "Cuniculus", "Cuniculus paca",
            "Cynomys", "Atherinosoma microstoma", "Brachyplatystoma rousseauxii",
            "Cerdocyon thous", "Cyprinus carpio", "Dasypodidae sp.", "Dasyprocta punctata",
            "Diceros bicornis", "Dasypus novemcinctus", "Didelphis", "Didelphis marsupialis",
            "Dicotyles tajacu", "Equidae", "Eulemur rubriventer", "Eulemur rufifrons", 
            "Felis", "Hare Lepus timidus/L. europaeus P.", "Hystrix", "Ichneumia albicauda",
            "Hexaprotodon guldbergi", "Hippopotamus amphibius", "Hippopotamus lemerlei", "Hippotragus niger", 
            "Iguanidae", "Lagomorpha", "Lama vicugna", "Lemur catta", "Leopardus sp.", 
            "Leopardus wiedii", "Lepilemur", "Lepilemur petteri", "Leporidae", "Lepus", 
            "Lepus americanus", "Lepus saxatilis", "Lomo guanicoe", "M. agilis", 
            "M. giganteus", "M. piceus", "M. rufus", "Martes martes L.", "Megaladapis madagascariensis", 
            "Meleagris gallopavo", "Microcebus griseorufus", "Micropterus dolomieu", 
            "Micropterus salmoides", "Mustela frenata", "Mustelidae", "Myocastor coipus", 
            "Oreochromis niloticus", "Ovis", "Ovis aries L.", "Pan troglodytes", 
            "Panthera leo", "Papio", "Papio anubis", "Papio ursinus", "Pecari tajacu", 
            "Phacochoerus africanus", "Phascolarctos cinereus", "Pomoxis nigromaculatus", 
            "Procyon lotor", "Propithecus coquereli", "Propithecus verreauxi", 
            "Raphicerus campestris", "Redunca arundinum",  
            "Scolopacidae", "Serpentes", "Sylvicapra grimmia", "Sylvilagus", 
            "Sylvilagus cunicularius", "Tapirella bairdii", "Tapiridae", "Tapirus terrestris", 
            "Tayassu pecari", "Tayassuidae", "Tragelaphus scriptus", "Vulpes vulpes", 
            "Vulpes vulpes L.", "Leptus saxatilis", "Lepus sp.", "Lepus timidus", "Lepus townsendii", 
            "Lutra lutra", "Mazama", "Mazama sp.", "Meles meles", "Mephitis mephitis", 
            "Microcebus rufus", "Mustela erminea", "Mustela nivalis", "Rabbit", 
            "Struthioniformes"
)
#	Castor fiber L, Eurasian beaver, is an aquatic species to that might affect things.
# small home ranges/migration patterns, 5km^2 or less
small <- c("Acomys cahirinus", "Akodon sp.", "Anomalocardia brasiliana", "Anthozoa",
          "Anura", "Apodemus flavicollis", "Arvicola amphibius", "Arvicola terrestris", "Bivalvia", 
          "Blattodea", "Bufo", "Bufonidae", "Bulimulidae", "Lemmus trimucronatus",
           "Cavia", "Cavia porcellus", "Cepaea hortensis", "Cepaea nemoralis", "Chilostoma sp.",  
           "Clausiliidae", "Corbicula sp.", "Cornu aspersum", "Cricetidae", "Crocidura", 
          "Crocidura flavescens deltae", "Cryptomys hottentotus", "Dicrostonyx groenlandicus", "Dicrostonyx groenlandicus rubricatus",
           "Ctenomys sp.", "Arvicolinae", "Eligmodontia sp.", "Eliurus majori", "Eliurus",
           "Eliurus minor", "Erinaceus europ.", "Fruticicolidae", "Galea sp.", "Gastropoda", 
           "Geomyidae", "Geomys bursarius", "Gerbilliscus brantsii", "Gyraulus convexiusculus", 
           "Helix aspersa", "Helix pomatia", "Helix sp.", "Marmota xaviventris", "Helicidae",
           "Meriones sp.", "Micaelamyus namaquensis", "Microtus arvalis/agrestis", 
           "Mollusca", "Mus", "Mus musculus", "Neocyclotus dysoni", "Nesokia indica",
           "Octodon sp.", "Ondatra zibethicus", "Orthalicus princeps", "Orthogeomys hispidus", 
           "Orycteropus afer", "Oryctolagus", "Oryctolagus cuniculus", "Oryctolagus sp.", 
           "Ostrea sp.", "Otocyon megalotis", "Otomys irroratus", "Otomys unisulcatus", 
           "Pachychilus sp.", "Pectinidae", "Pedetes capensis", "Perforatella incarnata",
           "Philander opossum", "Phyllotis sp.", "Pomacea flagellata", "Pomatias elegans", 
           "Procavia capensis", "Pronolagys rupestris", "Psammobates geometricus", 
           "Pupillidae", "Radix xauricularia", "Rhabdomys pumilio", "Sciuridae", 
           "Sigmodon hispidus", "Soricidae", "Spalax ehrenbergi", "Suncus murinus",
           "Talpa sp.", "Tatera indica", "Tenrecidae", "Thomomys talpoides", "Trichia", 
           "Trochoidea hoggarensis", "Xerotricha hoggarensis", "Xerus inauris", 
           "Zaedyus pichiy", "Zootelcus insularis", "Euglandina cylindracea", "Snail", 
          "Macroscelides/Elephaniuius sp. elephant shrew", "Macroscelididae", "Masticophis mentovarius", 
          "Megalonaias nervosa", "Microfauna", "Microgale cowani", "microtus agrestis", 
          "Microtus miurus", "Microtus oeconomus", "Microtus oeconomus macfarlani", 
          "Microtus oeconomus operarius", "Microtus pennsylvanicus", "Microtus xanthognathus", 
          "Mole", "Mouse, species undet.", "Muridae", "mus trimucronatus", "Myodes rutilus", 
          "Myodes rutilus dawsoni", "Neomys fodiens", "Rodentia", "Sciurus vulgaris", 
          "Sorex araneus", "Sorex minutus", "Talpa europeana", "Neotominae"
           # several aquatic species in here actually, such as clams
           )

df <- df %>% 
  mutate(migration = case_when(
    scientific_name %in% large ~ 'large', 
    scientific_name %in% medium ~ 'medium', 
    scientific_name %in% small ~ 'small'))

animaldf <- subset(df, material_type_group %in% c("animal, other", "bone", "tooth", "otolith"))

migration_list <- as.data.frame(c(large, medium, small)) %>% 
  rename(scientific_name = 'c(large, medium, small)')

missing_names <- anti_join(animaldf, migration_list) %>% 
  filter(!is.na(scientific_name))
table(missing_names$scientific_name) # what's 'missing' at this point are species I'm ignoring for various reasons
# Some things I'm ignoring because they're too tied with humans: Canis lupus familiaris, Rattus rattus,
# Ovis/Capris/Ares genus (if generic and not a specific wild species), Sus, Lama
# really generic families/orders are also left off (e.g., Reptilia, Rhicocerotidae)
# extinct species where the ranges just aren't necessarily well known ('e.g. archaeolemurs)
# Probably a lot of errors as I'm not a biologist but ah well. 

# Data Visualization ------------------------

df %>%
  ggplot() +
  geom_bar(aes(x = fct_infreq(material_type_group))) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "group") + 
  theme_classic()

df %>%
  ggplot(aes(x = Sr_ratio)) +
  geom_histogram(binwidth = 0.001, fill = 'midnightblue') +
  #geom_density(fill = 'midnightblue', alpha = 0.6) +
  labs(x = expression(paste(""^{87},"Sr/"^86,"Sr")), 
       y = "Count") +
  #xlim(0.7, 0.85) + 
  theme_classic()
ggsave("output/FigureHistogram.png", width = 7, height = 5, units = c("in"), dpi = 300)

ggplot() +
  geom_histogram(data = df, aes(x = Sr_ratio))
# World Map ---------------------------------------------------------------
cols <- c("animal, other" = "#FB9A99", 
           "bone" = "#FDBF6F",
           "organic other" = "#B2DF8A",
           "otolith" = "#6A3D9A", 
           "plant" = "#33A02C", 
          "rock" = "#E31A1C", 
          "sediment" = "#A6CEE3", 
          "soil" = "#FF7F00", 
          "tooth" = "#FFFF99", 
          "water" = "#1F78B4", 
          "dry deposition" = "#CAB2D6")

w <- world(path=tempdir())
v <- vect(df, geom= c("collection_decimal_longitude", "collection_decimal_latitude"), 
          crs = "")
crs(v) = crs(w)

plot(w)
plot(v, add = T)

B <- ggplot() +
  geom_spatvector(data = w, fill = 'grey90') + 
  geom_spatvector(data = v, aes(color = material_type_group), size = 0.6) +
  #scale_color_viridis(option = "turbo", discrete = T) + 
  scale_color_manual(values = cols, 
                     name = '') +
  theme_light() + 
  theme(legend.position = "bottom", 
        #panel.background = element_rect(fill = 'grey20')
        ) + 
  theme(legend.position = 'none') + 
  ylim(-55, 83.5) +
  guides(colour = guide_legend(override.aes = list(size = 2)))
#ggsave("output/FigureGlobalMap.png", width = 7, height = 5, units = c("in"), dpi = 300)

A <- df %>%
  ggplot(aes(x = fct_infreq(material_type_group), fill = material_type_group)) +
  geom_bar() +
  scale_fill_manual(values = cols, 
                     name = '') +
  labs(x = "Group", 
       y = 'Count', 
       fill = "Material Type (simplified)") + 
  theme_classic() + 
 # theme(axis.text.x = element_text(angle = -20, nudge_y = -0.8, hjust = 1)) + 
  NULL

ggarrange(A, B, nrow = 2, heights = c(0.6, 1), labels = "AUTO")
ggsave("output/FigureBarMap.png", width = 10, height = 10, units = c("in"), dpi = 300)

# Unique References/DOIS --------------------------------------------------
refs <- df %>% 
  select(related_publication_id, related_publication_citation) %>% 
  distinct(related_publication_citation, .keep_all = TRUE)
refsNoDOI <- filter(refs, related_publication_id == "")
dois <- dplyr::distinct(df, related_publication_id)

write.csv(file = 'output/dois.csv', dois) #easily save the reference dois
write.csv(file = 'output/refs.csv', refs) #easily save the reference list
write.csv(file = 'output/refsNoDOI.csv', refsNoDOI)
# also a quick count of who is the corresponding author
count(dplyr::distinct(df, investigator_name))

# Reference Standards -----------------------------------------------------
refValueUnique <- df %>% 
  distinct(related_publication_citation, .keep_all = TRUE) %>% 
  filter(primary_reference_material %in% c('NBS-987', 'NBS 987', 'NBS987', 'SRM-987', 'SRM 987', 'SRM 987'))


ggplot() + 
geom_histogram(data = refValueUnique, 
                   aes(x = reported_reference_value)
                       ) + 
  theme_classic()

# or is a table better?
refValueUnique <- df %>% 
  distinct(related_publication_citation, .keep_all = TRUE) %>% 
    filter(primary_reference_material %in% c('NBS-987', 'NBS 987', 'NBS987', 'SRM-987', 'SRM 987', 'SRM 987'))

count(dplyr::distinct(refValueUnique, reported_reference_value))
table(refValueUnique$reported_reference_value) #with 66 unique values, I'm realizing this is 


# what countries? (WIP) ---------------------------------------------------------

# What countries are represented in this dataset? 

# gotta make SpatVector w into a SpatRaster first
wRast = rast(w, nrow=20000, ncol=20000)
wRast = rasterize(w, wRast, field = "NAME_0")
v$country <- extract(wRast$NAME_0, v, ID = F)

table(v$country)
countriescount <- as.data.frame(v$country)
count(dplyr::distinct(countriescount, v$country))
