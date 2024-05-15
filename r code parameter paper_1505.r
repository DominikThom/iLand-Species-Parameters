library(ggtree)
library(ape)
library(RColorBrewer)



##### Now the GitHub Code
conn <- dbConnect(RSQLite::SQLite(), dbname = "C:/Arbeit/Manuscripts/Parameter data paper/all_species_database.sqlite")
species_all <- dbReadTable(conn, "species")
dbDisconnect(conn)




### distance matrix using all numerical traits of tree species, excluding biomass allocation a and b parameters as they belong into an equation
names(species_all)
traits=species_all[c(6:10, 13:14, 23:31, 33:47, 49:59, 61:dim(species_all)[2])] # select all relevant ones
traits=select_if(traits, is.numeric) # for another check
traits=data.frame(species=species_all[,2],traits)
#names(traits) # remove allocation functions - leave phenology for now which actually is some trait
#traits=traits[,c(1:6, 15:dim(traits)[2])]


# I am removing duplicated species - after harmonizing north America, this will not be necessary!
#traits=traits[!duplicated(traits$species),]

glimpse(traits)
summary(traits)
dim(traits) # 53 numeric parameters



rownames(traits)=c(traits$species) # facilitates plotting later
traits_dist <- daisy(traits[,-1], # remove species column from analysis 
                     metric = "gower",
                     stand=T) # stand does the z transformation (scaling)

summary(traits_dist)


# check which species are most similar
gower_mat <- as.matrix(traits_dist)

# Output most similar pair
traits[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ] # Quercus petraea and Quercus robur


# Output most dissimilar pair
traits[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ] # Abies lasiocarpa and Robinia pseudoacacia










### Cluster analyses

# visualyze dissimilarity with AHC
par(mfrow=c(1,1), mar=c(2,3,2,2), oma=c(1,1,1,1))
#windows()
traits_dist_clust=hclust(traits_dist, method = "ward.D2") 
#plot(traits_dist_clust,labels=c(as.character(traits$species)), ylab="Gower distance", main="") # pretty cool how the species of the taxa and all gymnosperms are in groups together, although I did not use any means over taxa to fill gaps...
#rect.hclust(traits_dist_clust, k=4, border="red")
#groups <- cutree(traits_dist_clust, k=4) # cut tree into 4 clusters and get the cluster classification



#library(ape)
#plot(as.phylo(traits_dist_clust), cex = 0.6, label.offset = 0.5, type = "fan")
pdf("C:/Arbeit/Manuscripts/Parameter data paper/Gower distance.pdf", width=20, height=30)

par(mfrow=c(1,1), mar=c(4,4,2,12), oma=c(1,1,1,1))
dend <- traits_dist_clust %>% as.dendrogram
#dend %>% plot
dend %>% set("labels") %>%
  set("branches_k_color", k=10) %>% set("labels_colors", k=10) %>%
  plot(horiz = TRUE, xlab="Gower distance")




dev.off()





# alternative visualization
phylo <- as.phylo(traits_dist_clust)

pdf("C:/Arbeit/Manuscripts/Parameter data paper/Gower_distance_phylo.pdf", width=10, height=10)

par(mfrow=c(1,1), mar=c(4,4,2,2), oma=c(1,1,1,1))
plot(phylo, type="fan", cex=0.6)

dev.off()







# Load the phylogenetic tree
phylo <- as.phylo(traits_dist_clust)  # Assuming `traits_dist_clust` can be converted to a `phylo` object

# Define your grouping vector for the branches based on traits_dist_clust
# This vector should have values from 1 to 10 representing the groups each branch belongs to.
# Replace `groups` with your actual grouping vector.
groups <- cutree(traits_dist_clust, k = 10)  # Example grouping vector (replace with your actual method)

# Define a color palette with 10 distinct colors using RColorBrewer
palette <- brewer.pal(10, "Set3")

# Map groups to colors
colors <- palette[groups]

# Create a ggtree plot
tree_plot <- ggtree(phylo, layout = "fan")  # Using fan layout as an example

# Add branch colors based on groups
tree_plot <- tree_plot + geom_tree(aes(color = factor(groups)))

# Add labels and customize the plot as desired
tree_plot <- tree_plot + geom_tiplab(size = 3, color = "black")

# Customize the color scale
tree_plot <- tree_plot + scale_color_manual(values = palette)

# Save the plot to a PDF file
pdf("C:/Arbeit/Manuscripts/Parameter data paper/Gower_distance_phylo_ggtree.pdf", width = 10, height = 10)
print(tree_plot)
dev.off()