## find unique species for each biome
name_genus_group = data.frame("name"=att_new[,1],"genus"=NA,"group"=att_new[,3])
name_genus_group$genus= sapply(strsplit(name_genus_group$name, "_"), `[`, 1)
