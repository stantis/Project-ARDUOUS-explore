# don't forget of course to set up your working directory
library(dplyr); library(tidyr); library(ggplot2); library(readr)

df <- read.csv("input/ProjectARDUOUS090724.csv") %>% 
  mutate(collection_decimal_latitude = as.numeric(collection_decimal_latitude), # this was going in as character, let's just fix that
         ratio = X87Sr_86Sr)


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

#just a look at animal parts used for Sr analysis
animaldf <- subset(df, material_type_group %in% c("animal, other", "bone", "tooth", "otolith"))
animaldf %>%
  ggplot(aes(x = fct_infreq(material_type_group))) +
  geom_bar() +
  labs(x = "group") + 
  theme_classic()

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
