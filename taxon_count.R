library(dplyr)
library(ggrepel)
library(scales)
##### count the important taxon in a network 
vertex_data=list()
setwd("/net/meso/work/siyzhang/network/genus_contribution")
for (i in 1:13){
  vertex_data[[i]] <- data.frame(name = V(g_10[[i]])$name, 
                                 PFG = V(g_10[[i]])$PFG, 
                                 strength = V(g_10[[i]])$strength,
                                 degree = degree(g_10[[i]]),
                                 bc = betweenness(g_10[[i]]))
  split_data <- do.call(rbind, strsplit(vertex_data[[i]]$name, "_", fixed = TRUE))
  genus_pfg = paste0(split_data[,1]," (",vertex_data[[i]]$PFG,")")
  vertex_data[[i]]$genus <- genus_pfg
  vertex_data[[i]] = vertex_data[[i]][vertex_data[[i]]$strength!=0,]
}

edge_data=list()
setwd("/net/meso/work/siyzhang/network/genus_contribution")
for (i in 1:13){
  edge_names_vector_df <- as.data.frame(get.edgelist(g_10[[i]], names = TRUE))
  edge_names_vector = paste(edge_names_vector_df$V1,edge_names_vector_df$V2,sep="_____")
  edge_data[[i]] <- data.frame(name = edge_names_vector,
                                 weight = E(g_10[[i]])$weight)
}
for (i in 1:13){
  vertex_data[[i]] <- data.frame(name = V(g_all[[i]])$name, 
                                 PFG = V(g_all[[i]])$PFG, 
                                 strength = V(g_all[[i]])$strength,
                                 degree = degree(g_all[[i]]),
                                 bc = betweenness(g_all[[i]]))
  split_data <- do.call(rbind, strsplit(vertex_data[[i]]$name, "_", fixed = TRUE))
  genus_pfg = paste0(split_data[,1]," (",vertex_data[[i]]$PFG,")")
  vertex_data[[i]]$genus <- genus_pfg
}

plot_pie = function(property){
  for (i in 1:13){
    strength_by_genus <- vertex_data[[i]] %>%
      group_by(genus) %>%
      summarize(total_strength = sum(get(property)), species_count = n())
    strength_by_genus$percent = strength_by_genus$total_strength / sum(strength_by_genus$total_strength) *100
    strength_by_genus = strength_by_genus[strength_by_genus$total_strength != 0,]
    strength_by_genus <- strength_by_genus %>%
      arrange(desc(percent))
    # Create a new category for "Other" and sum the percentages of all genera outside the top 10
    top_genus <- strength_by_genus %>%
      slice(1:10) %>%
      bind_rows(summarise(strength_by_genus %>%
                            slice(11:n()), 
                          genus = "Other", 
                          percent = sum(percent)))
    top_genus <- top_genus %>%
      mutate(label_position = cumsum(percent) - 0.5 * percent)
    top_genus$fill <- ifelse(top_genus$genus == "Other", "grey", "white")
    png(paste0(biome_names[i],".png"),width = 7, height = 7, units = 'in',res=200)
    plot=ggplot(top_genus, aes(x = "", y = percent, fill = fill)) +
      geom_bar(width = 1, stat = "identity", color = "black") +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values = c("grey", "white")) +
      # labs(title=paste0("% of Total",property_name)) +
      geom_text_repel(data = subset(top_genus, genus != "Other"), 
                      aes(y = label_position, label = genus), 
                      direction = "y", 
                      size = 4, 
                      nudge_x = 1.5, # Adjust text nudge as needed 
                      segment.curvature = -0.1)+
      #arrow = arrow(length = unit(0.03, "npc"))) +
      # Remove legend
      geom_text( data = subset(top_genus, genus == "Other"),
                 aes(label = genus, y = label_position),  # Ensure label_position for 'Other' is correctly calculated
                 size = 4,
                 vjust = -0.5) +
      theme(legend.position = "none") 
    print(plot)
    dev.off()
  }
}
strength_by_genus=list()
for(i in 1:13){
  strength_by_genus[[i]] <- vertex_data[[i]] %>%
    group_by(genus) %>%
    summarize(total_strength = sum(get(property)), species_count = n())
  strength_by_genus[[i]]$percent = strength_by_genus[[i]]$total_strength / sum(strength_by_genus[[i]]$total_strength) *100
  strength_by_genus[[i]] = strength_by_genus[[i]][strength_by_genus[[i]]$total_strength != 0,]
  strength_by_genus[[i]] <- strength_by_genus[[i]] %>%
    arrange(desc(percent))
}
bc_by_genus=list()
for(i in 1:13){
  bc_by_genus[[i]] <- vertex_data[[i]] %>%
    group_by(genus) %>%
    summarize(total_bc = sum(bc), species_count = n())
  bc_by_genus[[i]]$percent = bc_by_genus[[i]]$total_bc / sum(bc_by_genus[[i]]$total_bc) *100
  bc_by_genus[[i]] = bc_by_genus[[i]][bc_by_genus[[i]]$total_bc != 0,]
  bc_by_genus[[i]] <- bc_by_genus[[i]] %>%
    arrange(desc(percent))
}
sum(strength_by_genus[[1]]$percent[1:10]); sum(bc_by_genus[[1]]$percent[1:10])
sum(strength_by_genus[[9]]$percent[1:10]); sum(bc_by_genus[[9]]$percent[1:10])
sum(strength_by_genus[[2]]$percent[1:10]); sum(bc_by_genus[[2]]$percent[1:10])
sum(strength_by_genus[[10]]$percent[1:10]); sum(bc_by_genus[[10]]$percent[1:10])

setwd("/net/meso/work/siyzhang/network/genus_contribution/strength")
plot_pie("strength")
setwd("/net/meso/work/siyzhang/network/genus_contribution/degree")
plot_pie("degree")
setwd("/net/meso/work/siyzhang/network/genus_contribution/b_centrality")
plot_pie("bc")

setwd("/net/meso/work/siyzhang/network/genus_contribution/strength/all")
plot_pie("strength")
setwd("/net/meso/work/siyzhang/network/genus_contribution/degree/all")
plot_pie("degree")
setwd("/net/meso/work/siyzhang/network/genus_contribution/b_centrality/all")
plot_pie("bc")
##### degree
library(openxlsx)
filePath <- "vertex_data.xlsx"
write.xlsx(vertex_data, file = filePath)


###### pareto distribution?


plot_hist=function(i){
  my_breaks <- seq(min(vertex_data[[i]]$degree), max(vertex_data[[i]]$degree), length.out = 11) 
  hist(vertex_data[[i]]$degree , breaks=my_breaks ,
       main = biome_names[i], 
       xlab = "Degree", ylab = "Frequency")   
  my_breaks <- seq(min(vertex_data[[i]]$bc), max(vertex_data[[i]]$bc), length.out = 11) 
  hist(vertex_data[[i]]$bc , breaks=my_breaks ,
       main = biome_names[i], 
       xlab = "Betweenness", ylab = "Frequency")   
}
par(mfrow = c(4, 2), mar = c(4, 4, 2, 1))
plot_hist(1)
plot_hist(9)
plot_hist(2)
plot_hist(10)
