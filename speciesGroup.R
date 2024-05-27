#### for name_genus_group in main.m 
ngg = data.frame(species=rownames(TSS_max),genus=NA,PFG=NA,
                 speciesS=tolower(gsub("[^a-zA-Z]", "", rownames(TSS_max))))
ngg$genus = sub("_.*$", "", ngg$species)
ngg$PFG <- newPFG$PFG[match(ngg$speciesS, newPFG$name)]
exclude.sp = c("Globigerina_bulloides","Globoconella_inflata","Heterostylites_longicornis",
               "Pantachogon_haeckeli","Phalacroma_rotundatum","Mesoporos_perforatus","Prorocentrum_balticum" )
name_genus_group = ngg[!ngg$species %in% exclude.sp,1:3]
write.csv(name_genus_group, file="/net/meso/work/siyzhang/dominic/name_genus_group.csv", row.names = FALSE)
## classify planktons as zooplankton and phytoplankton / PFGs
library(dplyr)
data=read.csv("/net/meso/work/siyzhang/dominic/speciesGroup.csv")
class_all = unique(data[,3])
# Define the mapping
plankton_type <- c("Haptophyta" = "phyto", "Bacillariophyceae" = "phyto",
                   "Dinoflagellata" = "phyto", "Chrysophyceae" = "phyto",   
                   "Euglenophyta" = "phyto",      "Cryptophyta" = "phyto",     
                   "Jellyfish" = "zoo",         "Copepoda" = "zoo",         
                   "Chaetognatha" = "zoo",    "Malacostraca" = "zoo",
                   "Pteropoda" = "zoo",        "Foraminifera" = "zoo",
                   "Chordata" = "zoo", "Other Arthropoda"="zoo",
                   "Annelida" ="zoo"
                   # ... Add other classes as needed
)

# Apply the mapping
data <- data %>%
  mutate(Type = plankton_type[Group.for.background.data])
data=data[,-1]
colnames(data)=c("species","class","type")
write.csv(data, file="/net/meso/work/siyzhang/dominic/speciesGroup.csv", row.names = FALSE)
