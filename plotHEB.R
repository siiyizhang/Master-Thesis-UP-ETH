## try hierarchical edge bundling
# Libraries
library(ggraph)
library(igraph)
library(dplyr)
library(ggplot2)
# create a data frame giving the hierarchical structure of your individuals. 
# Origin on top, then groups, then subgroups
d_plankton <- data.frame(from="plankton", to=c("zoo","phyto"))
all_phyto = c("Haptophyta", "Bacillariophyceae", "Dinoflagellata", "Chrysophyceae", "Euglenophyta", "Cryptophyta")
all_zoo = c("Jellyfish","Copepoda","Chaetognatha","Malacostraca","Pteropoda",
            "Foraminifera","Chordata","Other Arthropoda","Annelida")
d_phyto=data.frame(from="phyto",to=all_phyto); d_zoo=data.frame(from="zoo",to=all_zoo)
hierarchy=rbind(d_plankton, d_phyto, d_zoo)
for (i in all_phyto){
  df = att[att$class==i,2:1]
  colnames(df) = c("from","to")
  hierarchy=rbind(hierarchy,df)
}
for (i in all_zoo){
  df = att[att$class==i,2:1]
  colnames(df) = c("from","to")
  hierarchy=rbind(hierarchy,df)
}

# create a vertices data.frame. One line per object of our hierarchy, giving features of nodes.
vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) ) 

# Create a graph object with the igraph library
mygraph <- graph_from_data_frame( hierarchy, vertices=vertices )
# This is a network object, you visualize it as a network like shown in the network section!

# With igraph: 
plot(mygraph, vertex.label="", edge.arrow.size=0, vertex.size=2)
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() +
  theme_void()

mat=adjc_matrices[[13]]*10
mat_integer <- matrix(round(mat), nrow = nrow(mat), ncol = ncol(mat))
colnames(mat_integer)=colnames(mat);rownames(mat_integer)=rownames(mat)
directed_mat <- mat_integer
directed_mat[lower.tri(directed_mat)] <- 0
g_test = graph_from_adjacency_matrix(directed_mat)
connect = as.data.frame(as.matrix(g_test, matrix.type = "edgelist"));colnames(connect)=c("from","to")
#remove edeges only appearing onece

# The connection object must refer to the ids of the leaves:
from <- match( connect$from, vertices$name)
to <- match( connect$to, vertices$name)
# Open a PNG device
png("KCE_HEB.png")


# plot
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", tension = 0.9) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()


# Close the device
dev.off()