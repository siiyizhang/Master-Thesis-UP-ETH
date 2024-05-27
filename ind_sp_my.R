##### find indicator species
library(dplyr)
library(tidyr)
#@@@@@@ First, calculate the total area of each biome per month
#@@@@@@ plankton_simple_biome: calculated from networkAnalysis2.Rmd
load("/net/meso/work/siyzhang/paper/01Data/plankton_simple_biome.RData") 
# structure: index | lon | lat | month | species 1-852 | biome number | actual area of this pixel
load("/net/meso/work/siyzhang/paper/01Data/att_new.RData")
# att for "attributes"; structure: species | type (phyto or zoo) | PFG
suppressMessages({
  total_biome_area_per_month <- plankton_simple_biome %>%
    group_by(biome, month) %>%
    summarise(total_biome_area = sum(area, na.rm = TRUE)) %>%
    ungroup()
})
#@@@@@@ if you want to group some biomes into one biome and see how this changes the result:
#@@@@@@ (in my case, there is much less indicator species if you group biomes)
# plankton_large_biome = plankton_simple_biome
# plankton_large_biome$biome[plankton_simple_biome$biome %in% c(2, 6, 10, 11)] = 1
# plankton_large_biome$biome[plankton_simple_biome$biome %in% c(12, 13)] = 2
# plankton_large_biome$biome[plankton_simple_biome$biome %in% c(3,4)] = 3
# plankton_large_biome$biome[plankton_simple_biome$biome %in% c(1,5,7,8,9)] = 4
# large_biome_area_per_month <- plankton_large_biome %>%
#   group_by(biome, month) %>%
#   summarise(total_biome_area = sum(area, na.rm = TRUE)) %>%
#   ungroup()

#@@@@@ Calculate the total area covered by each species in each biome per month
species_area_per_biome_month=list();species_area_large_biome_month=list();
get_sp_covergae = function(plankton_biome,biome_num,total_biome_area){
  suppressMessages({
  coverage=list()
  for (i in 1:852){
  coverage[[i]] <- data.frame(
    biome = rep(1:biome_num, each = 12),
    month = rep(1:12,biome_num)
  )
  tmp <- plankton_biome[plankton_biome[,att_new$species[i]]==1,c(att_new$species[i],"biome","month","area")] %>%
    group_by(biome, month) %>%
    summarise(species_area = sum(area, na.rm = TRUE)) %>%
    ungroup()
  coverage[[i]]=left_join(coverage[[i]],tmp,by=c("biome","month"))
  coverage[[i]]$species_area[is.na(coverage[[i]]$species_area)]=0
}

# Join the total biome area with the species area to compare
for (i in 1:852){
  coverage[[i]] <- coverage[[i]] %>%
    left_join(total_biome_area, by = c("biome", "month"))
}

# Calculate the percentage of biome area covered by each species
for (i in 1:852){
  coverage[[i]] <- coverage[[i]] %>%
    mutate(percent = round((species_area / total_biome_area),3)) 
}
  return(coverage)
  })
}
species_area_per_biome_month=get_sp_covergae(plankton_simple_biome,13,total_biome_area_per_month) 
# it takes ~1 minute to get the result
#species_area_large_biome_month=get_sp_covergae(plankton_large_biome,4,large_biome_area_per_month)

# Iterate over the list to process each species
sp_90per=vector();
combined_df <- bind_rows(species_area_per_biome_month, .id = "sp_id") # .id is optional, adds an identifier for each original dataframe
combined_df_simple=combined_df[,c(1,2,3,6)]
combined_df_simple <- combined_df_simple %>%
  group_by(biome, sp_id) %>%
  summarise(annual_perc = mean(percent, na.rm = TRUE)) %>%
  ungroup()

percentile_df <- combined_df %>%
  group_by(biome, month) %>%
  summarise(
    percentile_25 = quantile(percent, probs = 0.25, na.rm = TRUE), # Calculate the 25th percentile
    percentile_75 = quantile(percent, probs = 0.75, na.rm = TRUE),  # Calculate the 75th percentile
    percentile_10 = quantile(percent, probs = 0.10, na.rm = TRUE),
    percentile_90 = quantile(percent, probs = 0.90, na.rm = TRUE)  
  )

#combined_df_large_biome <- bind_rows(species_area_large_biome_month, .id = "sp_id") 
# percentile_df_large_biome <- combined_df_large_biome %>%
#   group_by(biome, month) %>%
#   summarise(
#     percentile_25 = quantile(percent, probs = 0.25, na.rm = TRUE), # Calculate the 25th percentile
#     percentile_75 = quantile(percent, probs = 0.75, na.rm = TRUE)  # Calculate the 75th percentile
#   )
#@@@@@ several possible definitions
#@@@@@ 1. >75% area coverage in one biome and <25% in all the other 
#@@@@@                            -> 244 entries in 6 biomes: 2,6,9,10,11,13
ind_sp=replicate(n=6,data.frame(indicat_species=NA,biome=NA,month=NA),simplify = FALSE)
for (i in 1:852) {
  for (m in 1:12){
    current_df <- species_area_per_biome_month[[i]][species_area_per_biome_month[[i]]$month==m,]
    current_df[is.na(current_df)]=0
    if (length(current_df[current_df$percent>=0.75,"biome"])==1 &
        length(current_df[current_df$percent<=0.25,"biome"])==12){
      ind_sp[[1]]=rbind(ind_sp[[1]],
                        c(att_new$species[i], unlist(unique(current_df[current_df$percent>=0.75,"biome"])),  m))
    }
  }
} # with percentage
#@@@@@ 1.1 >90% area coverage in one biome and <10% in all the other 
#@@@@@                            -> 244 entries in 6 biomes: 2,6,9,10,11,13
# ind_sp=replicate(n=6,data.frame(indicat_species=NA,biome=NA,month=NA),simplify = FALSE)
# for (i in 1:852) {
#   for (m in 1:12){
#     current_df <- species_area_per_biome_month[[i]][species_area_per_biome_month[[i]]$month==m,]
#     current_df[is.na(current_df)]=0
#     if (length(current_df[current_df$percent>=0.9,"biome"])==1 &
#         length(current_df[current_df$percent<=0.1,"biome"])==12){
#       ind_sp[[1]]=rbind(ind_sp[[1]],
#                         c(att_new$species[i], unlist(unique(current_df[current_df$percent>=0.9,"biome"])),  m))
#     }
#   }
# } # with percentage
#@@@@@ 2. >75% percentile (among the area coverage of all the other species) in one biome and <25% in all the other
##########                            -> 261 entries in 2 biomes
for (i in 1:852) {
  for (m in 1:12){
    current_df <- species_area_per_biome_month[[i]][species_area_per_biome_month[[i]]$month==m,]
    current_df[is.na(current_df)]=0
    for ( j in 1:13){
      current_vector <- species_area_per_biome_month[[i]][species_area_per_biome_month[[i]]$month==m &
                                                            species_area_per_biome_month[[i]]$biome==j ,]
      area_percent = current_vector$percent
      if (!is.na(area_percent)){
        other_percent = current_df$percent[current_df$biome!=j]
        upper = percentile_df$percentile_75[percentile_df$month==m & percentile_df$biome==j]
        lower = percentile_df$percentile_25[percentile_df$month==m & percentile_df$biome!=j]
        if (area_percent>upper & all(other_percent<=lower,na.rm = T)){
          ind_sp[[2]]=rbind(ind_sp[[2]],
                       c(att_new$species[i],   j,  m))
        }
      }
    }
  }
} # with percentile
#### 2.1 >90% percentile (among the area coverage of all the other species) in one biome and <10% in all the other
##########                            -> 261 entries in 2 biomes
for (i in 1:852) {
  for (m in 1:12){
    current_df <- species_area_per_biome_month[[i]][species_area_per_biome_month[[i]]$month==m,]
    current_df[is.na(current_df)]=0
    for ( j in 1:13){
      current_vector <- species_area_per_biome_month[[i]][species_area_per_biome_month[[i]]$month==m &
                                                            species_area_per_biome_month[[i]]$biome==j ,]
      area_percent = current_vector$percent
      if (!is.na(area_percent)){
        other_percent = current_df$percent[current_df$biome!=j]
        upper = percentile_df$percentile_90[percentile_df$month==m & percentile_df$biome==j]
        lower = percentile_df$percentile_10[percentile_df$month==m & percentile_df$biome!=j]
        if (area_percent>upper & all(other_percent<=lower,na.rm = T)){
          ind_sp[[2]]=rbind(ind_sp[[2]],
                            c(att_new$species[i],   j,  m))
        }
      }
    }
    
  }
} # with percentile
##### Def. 3: reduce biome number to 4, >90% area coverage in one biome and <10% in all the other 

for (i in 1:852) {
  for (m in 1:12){
    current_df <- species_area_large_biome_month[[i]][species_area_large_biome_month[[i]]$month==m,]
    current_df[is.na(current_df)]=0
    if (length(current_df[current_df$percent>=0.9,"biome"])==1 &
        length(current_df[current_df$percent<=0.1,"biome"])==12){
      ind_sp[[3]]=rbind(ind_sp[[3]],
                        c(att_new$species[i],   unlist(unique(current_df[current_df$percent>=0.9,"biome"])),  m))
    }
  }
} # with percentage
######## Def. 4: >75% percentile (among the area coverage of all the other species) in one biome and <25% in all the other
for (i in 1:852) {
  for (m in 1:12){
    current_df <- species_area_large_biome_month[[i]][species_area_large_biome_month[[i]]$month==m,]
    current_df[is.na(current_df)]=0
    for ( j in 1:4){
      current_vector <- species_area_large_biome_month[[i]][species_area_large_biome_month[[i]]$month==m &
                                                            species_area_large_biome_month[[i]]$biome==j ,]
      area_percent = current_vector$percent
      if (!is.na(area_percent)){
        other_percent = current_df$percent[current_df$biome!=j]
        upper = percentile_df$percentile_75[percentile_df$month==m & percentile_df$biome==j]
        lower = percentile_df$percentile_25[percentile_df$month==m & percentile_df$biome!=j]
        if (area_percent>upper & all(other_percent<=lower,na.rm = T)){
          ind_sp[[4]]=rbind(ind_sp[[4]],
                            c(att_new$species[i],   j,  m))
        }
      }
    }
    
  }
} # with percentile
######## Def. 5: small biomes;
########         >75% percentile in one biome and <25% absolute coverage 
########         ----> 918 entries (species * biome * month) in biome 2, 6, 10, 11, 13
for (i in 1:852) {
  for (m in 1:12){
    current_df <- species_area_per_biome_month[[i]][species_area_per_biome_month[[i]]$month==m,]
    current_df[is.na(current_df)]=0
    for ( j in 1:13){
      current_vector <- species_area_per_biome_month[[i]][species_area_per_biome_month[[i]]$month==m &
                                                            species_area_per_biome_month[[i]]$biome==j ,]
      area_percent = current_vector$percent
      if (!is.na(area_percent)){
        other_percent = current_df$percent[current_df$biome!=j]
        upper = percentile_df$percentile_75[percentile_df$month==m & percentile_df$biome==j]
        if (area_percent>upper & 
            length(current_df[current_df$percent<=0.25,"biome"])==12){
          ind_sp[[5]]=rbind(ind_sp[[5]],
                            c(att_new$species[i],  j,  m))
        }
      }
    }
    
  }
} # with percentile
########  Def 6: >75% absolute in one biome and <25% percentile
########         ----> 918 entries (species * biome * month) in biome 2, 6, 10, 11, 13
for (i in 1:852) {
  for (m in 1:12){
    current_df <- species_area_per_biome_month[[i]][species_area_per_biome_month[[i]]$month==m,]
    current_df[is.na(current_df)]=0
    for ( j in 1:13){
      current_vector <- species_area_per_biome_month[[i]][species_area_per_biome_month[[i]]$month==m &
                                                            species_area_per_biome_month[[i]]$biome==j ,]
      area_percent = current_vector$percent
      if (!is.na(area_percent)){
        other_percent = current_df$percent[current_df$biome!=j]
        lower = percentile_df$percentile_25[percentile_df$month==m & percentile_df$biome!=j]
        if (length(current_df[current_df$percent>=0.75,"biome"])==1 & 
            all(other_percent<=lower,na.rm = T) ){
          ind_sp[[6]]=rbind(ind_sp[[6]],
                            c(att_new$species[i],  j,  m))
        }
      }
    }
    
  }
} # with percentile
ind_sp_full=left_join(ind_sp[[1]],att_new,by = c("indicat_species" = "species"))

ind_sp_simple = ind_sp_full
ind_sp_simple$month <- as.character(ind_sp_simple$month)

# Now group by 'indica_species', 'biome', and 'PFG', and collapse the 'month' column
ind_sp_simple <- ind_sp_simple %>%
  group_by(biome, indicat_species, PFG) %>%
  summarise(month = paste(unique(month), collapse = ","), .groups = 'drop')
write.csv(ind_sp_simple, "indicator species.csv", row.names = FALSE)
########core species in each biome * month
##################################

for (i in 1:13){
  for (m in 1:12)
  {
    
  }
}
