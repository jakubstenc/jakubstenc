#-------------READ ME-----------####

   # This script serves to the first exploration and pre-analyses of the data from Handrkov 2011-2024
   # Log of possible problems, changes and comments etrc. can be found here: https://docs.google.com/document/d/1iAdDP0WE-Z7KCjPCh7StA5mAd4BrJD0UY1tZUGOXFSA/edit?tab=t.0


#-------------READ ME-----------#

# Packages #####

library(bipartite)
library("OpenImageR")
library("jpeg")
library(magick)
library(igraph)
library (iNEXT)
library("dplyr") 
library(tidyr)
#--------------#

# Colour definitions  ####
seda<- rgb(150,150,150, max = 255, alpha = 60, names = "blue")

#--------------#



# Data import ####
jine_ctverce_spojene_11_24 <- read.csv("data/jine_ctverce_spojene_11_24.csv")
net1124_edited<- jine_ctverce_spojene_11_24

plant_dens <- read.csv("data/osnovy11_24.csv", row.names = "X")



#--------------#

# Unification of identificator ####
# This identificator is to identify one visit of the plot - thus it combines year, plot, month, day, hour, minute
net1124_edited$id_i<-as.character(net1124_edited$id_i)
net1124_edited$id_i<-with(net1124_edited,paste(rok,osnova,mesic*100+den,hod*100+min,sep="-"))
unique(net1124_edited$id_i)
#--------------#


net1124_edited$rok <- factor(net1124_edited$rok, levels = c("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))

# Standardization by sampling effort ####

## Data preparation before standardization for sampling effort ####
net1124_edited <-  net1124_edited[which(!net1124_edited$beh%in%c("Kratiny12_beh14","Kratiny12_beh12","Kratiny12_beh13","Kratiny12_beh11","Handrkov12_beh11", "Handrkov12_beh12", "Handrkov12_beh13")), ] # only Handrkov

## Standardization computation ####
# Standartization is computed by taking all the individual visits to the plot (coded by id_i) and the number of visits to the plot per year is obtained in following code

poc.obejiti <-  net1124_edited %>% group_by(id_i, rok, osnova) %>% summarize(count=n()) 
poc.obejiti$obejito <- 1    

obejiti <-  tapply(poc.obejiti$obejito, list(poc.obejiti$osnova, poc.obejiti$rok),sum, na.rm=T) 


obejiti2 <- obejiti 
obejiti2 <- data.frame(obejiti2[!rownames(obejiti2)%in%c(30, 31, 42,43,97),])
colnames(obejiti2) <- c(11:24)
obejiti2$osnova <- rownames(obejiti2) 
obejiti3 <- obejiti2 %>% pivot_longer(cols=c(colnames(obejiti2)[-15]),
                                      names_to=c('rok'),
                                      values_to='obejito')

### plot sampling effort #####
#pdf("Sampling_effort.pdf")
par(mfrow=c(1,1))
plot(tapply(obejiti3$obejito, list( obejiti3$rok), mean, na.rm=T), type="n", main="Sampling effort per year", ylab = "Average number of censuses per plot", xlab="Year",  col=rgb(0,0,0,0.25), axes = F, cex.lab=1.5, ylim= c(0,50))

lines(tapply(obejiti3$obejito, list( obejiti3$rok), mean, na.rm=T), lwd = 2)

axis(1, at=c(1:14), labels = c(2011:2024))
axis(2)
mean_plots <-tapply(obejiti3$obejito, list( obejiti3$rok), mean, na.rm=T)

sd_plots <-tapply(obejiti3$obejito, list( obejiti3$rok), sd, na.rm=T)

# Vertical arrow
arrows(x0=c(1:14), y0=mean_plots-sd_plots, x1=c(1:14), y1=mean_plots+sd_plots, code=3, angle=90, length=0.1, col="black", lwd=2)

#dev.off()
#--------------#


## Merge sampling effort with original table #####
net1124_edited <- merge( jine_ctverce_spojene_11_24,obejiti3, by = c("osnova", "rok"), all = TRUE)  # Use all=TRUE for a full outer join


# Data preparation #####
## data cleaning ####
net1124_edited <-  net1124_edited[which(!net1124_edited$osnova%in%c("L0","L1","L10","L11","L12","L2","L3","L4","L5","L6","L7","L8","L9")), ] # only plots in the meadow
net1124_edited <-  net1124_edited[!net1124_edited$druhK_opraveno%in%c("UNDETERMINED_trifolium","Epilobium_sp","Kanutia_arvensis","Leontodon_sp","nic","Hypericum_perforatum","Lycopus_europaeus","Galium_boreale","Galium_glaucum"), ] # only plots in the meadow
net1124_edited <-  net1124_edited[which(net1124_edited$opyluje%in%c("T","?" )), ] # only pollinators
net1124_edited <-  net1124_edited[!net1124_edited$pocet.x%in%c("NA"), ] # NA purification

## Plant corrections ####
net1124_edited[net1124_edited$druhK_opraveno%in% c("Cuscuta_sp") ,]$druhK_opraveno <- "Cuscuta_europaea"
net1124_edited[net1124_edited$druhK_opraveno%in% c("Myosotis_sp") ,]$druhK_opraveno <- "Myosotis_palustris"
net1124_edited[net1124_edited$druhK_opraveno%in% c("Stellaria_sp") ,]$druhK_opraveno <- "Stellaria_graminea"
net1124_edited[net1124_edited$druhK_opraveno%in% c("Lysimachia_sp") ,]$druhK_opraveno <- "Lysimachia_vulgaris"

net1124_edited <- droplevels(net1124_edited)


## new variable - number of observations corrected by smapling eefort per plot within a year
net1124_edited$corrected_number <- net1124_edited$pocet.x/net1124_edited$obejito

#--------------#

# General network data ####

net1124  <-   apply( tapply(net1124_edited$pocet.x, list(net1124_edited$rok, net1124_edited$osnova), sum)/tapply(net1124_edited$obejito, list(net1124_edited$rok, net1124_edited$osnova), unique), 1, sum, na.rm=T)


net1124 <- tapply(net1124_edited$corrected_number, list(net1124_edited$druhK_opraveno, net1124_edited$sitodruh), sum)

net1124[is.na(net1124)] <- 0

#--------------#


## Datasets per individual years ####

for (i in unique(net1124_edited$rok)) {
  
  # Create the data matrix for the current value of i
  net <- with(net1124_edited[net1124_edited$rok %in% c(i),], 
              tapply(corrected_number, list(druhK_opraveno, sitodruh), sum))
  net[is.na(net)] <- 0
  
  # Dynamically assign the matrix to a variable named "net<i>"
  assign(paste("net", i, sep = ""), net)
}

nets <- paste("net", unique(net1124_edited$rok), sep = "")


## Datasets per hour ####

for (i in unique(net1124_edited$hod)) {
  
  # Create the data matrix for the current value of i
  net <- with(net1124_edited[net1124_edited$hod %in% c(i)&net1124_edited$rok=="21",], 
              tapply(corrected_number, list(druhK_opraveno, sitodruh), sum))
  net[is.na(net)] <- 0
  
  # Dynamically assign the matrix to a variable named "net<i>"
  assign(paste("net_hod_", i, sep = ""), net)
}

nets <- paste("net_hod_", unique(net1124_edited$hod), sep = "")



# Visualisations ####

## agrregated network


plotweb(
  net1124, 
  method = "normal", 
  text.rot = 90, 
  labsize = 1, 
  ybig = 0.8, 
  low.y = 0.6, 
  high.y = 0.98, 
  plot.axes = FALSE, 
  y.width.low = 0.05, 
  y.width.high = 0.05, 
  low.spacing = 0.01, 
  high.spacing = 0.01, 
  bor.col.low = seda, 
  col.low = seda, 
  col.high = seda, 
  bor.col.high = seda, 
  bor.col.interaction = seda, 
  col.interaction = seda,
)
# Add a title with the year
title(main = paste("Bipartite Web - Year:", gsub("net","",i)))  









################## site v case ####

# Create the names for the nets
nets <- paste("net", sort(unique(net1124_edited$rok)), sep = "")

# Loop through nets
for (i in nets) {
  # Use `get` to retrieve the actual object from the variable name
  net_matrix <- get(i)
  
  # Pass the matrix to the plotweb function
  plotweb(
    net_matrix, 
    method = "normal", 
    text.rot = 90, 
    labsize = 1, 
    ybig = 0.8, 
    low.y = 0.6, 
    high.y = 0.98, 
    plot.axes = FALSE, 
    y.width.low = 0.05, 
    y.width.high = 0.05, 
    low.spacing = 0.01, 
    high.spacing = 0.01, 
    bor.col.low = seda, 
    col.low = seda, 
    col.high = seda, 
    bor.col.high = seda, 
    bor.col.interaction = seda, 
    col.interaction = seda,
  )
  # Add a title with the year
  title(main = paste("Bipartite Web - Year:", gsub("net","",i)))  
}







################## site v case ####

# Create the names for the nets
nets <- paste("net_hod_", 7:18, sep = "")

# Loop through nets
for (i in nets) {
  # Use `get` to retrieve the actual object from the variable name
  net_matrix <- get(i)
  
  # Pass the matrix to the plotweb function
  plotweb(
    net_matrix, 
    method = "normal", 
    text.rot = 90, 
    labsize = 1, 
    ybig = 0.8, 
    low.y = 0.6, 
    high.y = 0.98, 
    plot.axes = FALSE, 
    y.width.low = 0.05, 
    y.width.high = 0.05, 
    low.spacing = 0.01, 
    high.spacing = 0.01, 
    bor.col.low = seda, 
    col.low = seda, 
    col.high = seda, 
    bor.col.high = seda, 
    bor.col.interaction = seda, 
    col.interaction = seda,
  )
  # Add a title with the year
  title(main = paste("Hour:", gsub("net_hod_","",i)))  
}


par(oma=c(0,0,0,0), mar=c(8,8,8,8))
# Loop through each year
for (i in sort(unique(net1124_edited$rok))) {
  # Dynamically retrieve the matrix for the current year
  net_matrix <- get(paste("net", i, sep = ""))
  
  # Create a bipartite graph from the incidence matrix
  bg <- graph_from_incidence_matrix(net_matrix)
  
  # Generate bipartite projections
  proj <- bipartite_projection(bg, multiplicity = TRUE)
  
  # Detect communities using a clustering algorithm
  communities <- cluster_louvain(proj$proj1)
  
  # Assign cluster membership as a node color
  V(proj$proj1)$color <- membership(communities)
  
  # Define the layout for the circle
  l <- layout_in_circle(proj$proj1)
  
  # Define vertex size by degree
  deg = centr_degree(proj$proj1, mode="all")
  V(proj$proj1)$size = 5*sqrt(deg$res)
  
  # Shorten species names to the desired format (e.g., nam_nam)
  V(proj$proj1)$name <- sapply(
    V(proj$proj1)$name, 
    function(x) {
      parts <- strsplit(x, "_")[[1]]
      paste0(substr(parts[1], 1, 3), "_", substr(parts[2], 1, 3))
    }
  )
  
  # Calculate label positions based on vertex coordinates
  label_pos <- apply(l, 1, function(coords) {
    angle <- atan2(coords[2], coords[1])  # Calculate angle
    # Adjust label positions outward based on angle
    list(
      x = coords[1] + 0.2 * cos(angle),
      y = coords[2] + 0.2 * sin(angle)
    )
  })
  
  # Extract label x and y positions
  label_x <- sapply(label_pos, function(pos) pos$x)
  label_y <- sapply(label_pos, function(pos) pos$y)
  
  # Plot the graph with adjusted labels
  plot(
   proj$proj1,
    layout = l,
    edge.width = E(proj$proj1)$weight**0.5,
    vertex.color = V(proj$proj1)$color, # Use cluster-based colors
    vertex.label = NA,  # Suppress default labels
    vertex.size = V(proj$proj1)$size,
    main = paste("Year:", i)
  )
  
  # Add labels at adjusted positions
  text(
    x = label_x, 
    y = label_y, 
    labels = V(proj$proj1)$name,
    cex = 0.7,  # Label size
    col = "black"  # Label color
  )
}

  


par(oma=c(0,0,0,0), mar=c(8,8,8,8))
# Loop through each year
for (i in c(7:18)) {
  # Dynamically retrieve the matrix for the current year
  net_matrix <- get(paste("net_hod_", i, sep = ""))
  
  # Create a bipartite graph from the incidence matrix
  bg <- graph_from_incidence_matrix(net_matrix)
  
  # Generate bipartite projections
  proj <- bipartite_projection(bg, multiplicity = TRUE)
  
  # Detect communities using a clustering algorithm
  communities <- cluster_louvain(proj$proj1)
  
  # Assign cluster membership as a node color
  V(proj$proj1)$color <- membership(communities)
  
  # Define the layout for the circle
  l <- layout_in_circle(proj$proj1)
  
  # Define vertex size by degree
  deg = centr_degree(proj$proj1, mode="all")
  V(proj$proj1)$size = 5*sqrt(deg$res)
  
  # Shorten species names to the desired format (e.g., nam_nam)
  V(proj$proj1)$name <- sapply(
    V(proj$proj1)$name, 
    function(x) {
      parts <- strsplit(x, "_")[[1]]
      paste0(substr(parts[1], 1, 3), "_", substr(parts[2], 1, 3))
    }
  )
  
  # Calculate label positions based on vertex coordinates
  label_pos <- apply(l, 1, function(coords) {
    angle <- atan2(coords[2], coords[1])  # Calculate angle
    # Adjust label positions outward based on angle
    list(
      x = coords[1] + 0.2 * cos(angle),
      y = coords[2] + 0.2 * sin(angle)
    )
  })
  
  # Extract label x and y positions
  label_x <- sapply(label_pos, function(pos) pos$x)
  label_y <- sapply(label_pos, function(pos) pos$y)
  
  # Plot the graph with adjusted labels
  plot(
    proj$proj1,
    layout = l,
    edge.width = E(proj$proj1)$weight**0.5,
    vertex.color = V(proj$proj1)$color, # Use cluster-based colors
    vertex.label = NA,  # Suppress default labels
    vertex.size = V(proj$proj1)$size,
    main = paste("hour:", i)
  )
  
  # Add labels at adjusted positions
  text(
    x = label_x, 
    y = label_y, 
    labels = V(proj$proj1)$name,
    cex = 0.7,  # Label size
    col = "black"  # Label color
  )
}










# Loop through each year
for (i in sort(unique(net1124_edited$rok))) {
  # Dynamically retrieve the matrix for the current year
  net_matrix <- get(paste("net", i, sep = ""))  # Assuming these are already generated matrices
  
  net_matrix = net_matrix[rowSums(net_matrix[])>0,] 
  
  
  # Compute Jaccard similarity (or use "bray" for Bray-Curtis)
  similarity_matrix <- vegdist(net_matrix, method = "jaccard")
  
  # Convert to a similarity matrix
  similarity_matrix <- 1 - as.matrix(similarity_matrix)

  pheatmap::pheatmap(similarity_matrix, treeheight_row = 0, treeheight_col = 0,,col =hcl.colors(150))
  
}
  











# Initialize an empty list to store the Müller Index results for each year
muller_index_by_year <- list()

# Get the union of all plant species across all years to ensure consistency
all_plants <- unique(unlist(lapply(unique(net1124_edited$rok), function(year) {
  net_matrix <- get(paste("net", year, sep = ""))
  return(rownames(net_matrix))
})))

# Loop through each year in the dataset
for (year in unique(net1124_edited$rok)) {
  
  # Dynamically retrieve the network matrix for the current year (e.g., 'net_2015')
  net_matrix <- get(paste("net", year, sep = ""))
  
  # Initialize an empty vector to store the Müller Index for each plant species in the current year
  muller_index <- numeric(length(all_plants))
  names(muller_index) <- all_plants  # Ensure we have the same species in each year
  
  # Loop through each plant species to compute the Müller Index for the current year
  for (i in 1:nrow(net_matrix)) {
    plant_name <- rownames(net_matrix)[i]
    
    # If the plant exists in the current year, compute the Müller Index
    if (plant_name %in% all_plants) {
      pollinators_i <- which(net_matrix[i, ] > 0)  # Pollinators that interact with plant i
      interactions_i <- net_matrix[i, pollinators_i]  # Interaction values for plant i
      
      # Compute the numerator: Sum of shared interactions with other plants
      numerator <- 0
      for (j in 1:nrow(net_matrix)) {
        if (i != j) {  # Skip comparing the plant with itself
          pollinators_j <- which(net_matrix[j, ] > 0)  # Pollinators that interact with plant j
          interactions_j <- net_matrix[j, pollinators_j]  # Interaction values for plant j
          
          # Find the shared pollinators between plant i and plant j
          shared_pollinators <- intersect(pollinators_i, pollinators_j)
          
          # Sum the interaction strength for shared pollinators (the weight)
          shared_interactions <- sum(interactions_i[match(shared_pollinators, pollinators_i)] * interactions_j[match(shared_pollinators, pollinators_j)])
          
          # Add to the numerator
          numerator <- numerator + shared_interactions
        }
      }
      
      # Compute the denominator: Sum of squared interactions for plant i
      denominator <- sum(interactions_i^2)
      
      # Calculate the Müller Index for plant i (if the denominator is not zero)
      if (denominator > 0) {
        muller_index[plant_name] <- numerator / denominator
      } else {
        muller_index[plant_name] <- 0  # If no interactions, the Müller Index is zero
      }
    }
  }
  
  # Normalize the Müller Index for the current year
  muller_index_normalized <- (muller_index - min(muller_index)) / (max(muller_index) - min(muller_index))
  
  # Store the Müller Index for the current year in the list
  muller_index_by_year[[paste(year, sep = "")]] <- muller_index_normalized
}

# Now we need to combine the Müller Index for all years into a data frame
muller_df <- data.frame(Plant = all_plants)  # Start with all plants

# Loop through each year in muller_index_by_year and add it to the data frame
for (year in names(muller_index_by_year)) {
  # Add the Müller Index values for the current year
  muller_df[[year]] <- muller_index_by_year[[year]]
}

# View the summary table
print(muller_df)

# Reshape the data frame for plotting
library(reshape2)
muller_df_long <- melt(muller_df, id.vars = "Plant", variable.name = "Year", value.name = "MullerIndex")

# Ensure that 'Year' is a factor and ordered by the year
muller_df_long$Year <- factor(muller_df_long$Year, levels = sort(unique(as.numeric(muller_df_long$Year)))+10)


# Plotting the trends across years for each plant species
library(ggplot2)
ggplot(muller_df_long, aes(x = Year, y = MullerIndex, group = Plant, color = Plant)) +
  geom_line() +
  labs(
    title = "Müller Index Trends Across Years for Plant Species",
    x = "Year",
    y = "Müller Index"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Can local dominance lead to local specialisation? ####


# Can temporal variation lead to  temporal specialisation? #####















































(plotweb((net11), method="normal", text.rot = 90, labsize =1, ybig = 0.8, low.y = 0.6, high.y = 0.98, plot.axes = FALSE, y.width.low = 0.05, y.width.high = 0.05, low.spacing=0.01, high.spacing=0.01,bor.col.low = seda ,col.low = seda,col.high = seda,bor.col.high = seda,  bor.col.interaction = seda, col.interaction=seda))

plotModuleWeb(computeModules(net1124))




bg=graph_from_incidence_matrix(net1124) 

proj=bipartite_projection(bg, multiplicity = T)
proj


l <- layout_in_circle(proj$proj1)

plot(proj$proj1,layout =l,  mode="directed",  edge.width=E(proj$proj1)$weight**.5, main = "" ,  vertex.color = "tomato",vertex.frame.color ="tomato",vertex.size = 15  )

m <- layout_with_fr(proj$proj1)
g <- layout_with_gem(proj$proj1)
n <- layout_nicely(proj$proj1)
r <- layout_with_graphopt(proj$proj1)
sp <- layout_on_sphere(proj$proj1)





E(proj$proj1)$colour = seda
E(proj$proj1)[E(proj$proj1)%in% which(get.edges(proj$proj1,E(proj$proj1))[,2]==31)]$colour = Col_Succ # timhle dohledavam edge dle vertexu

s <- layout_as_star(proj$proj1 , center = V(proj$proj1)[c("Suc_pra")])

plot(proj$proj1 ,layout =s, edge.width=E(proj$proj1)$weight**.1, main = "", edge.color=E(proj$proj1)$colour )

# MULLER INDEX ####


# Turn-over  ####

###############x
# Get the set of all species (plants and pollinators) from all years
all_species <- union(unique(net1124_edited$druhK_opraveno), unique(net1124_edited$sitodruh))

# Loop through each year to create consistent matrices
nets <- list()  # List to store matrices
for (i in unique(net1124_edited$rok)) {
  
  # Create the matrix for the current year
  net <- with(net1124_edited[net1124_edited$rok %in% c(i),], 
              tapply(corrected_number, list(druhK_opraveno, sitodruh), sum))
  
  # Ensure the row and column names are consistent with the full list of species
  # Add missing species with 0 interactions
  rownames(net) <- make.names(rownames(net), unique = TRUE)  # Ensure unique names
  colnames(net) <- make.names(colnames(net), unique = TRUE)
  
  # Add any missing species with no interactions (initialize them with 0)
  missing_rows <- setdiff(all_species, rownames(net))
  missing_cols <- setdiff(all_species, colnames(net))
  
  # Add rows and columns with zero values for missing species
  if (length(missing_rows) > 0) {
    net <- rbind(net, matrix(0, nrow = length(missing_rows), ncol = ncol(net)))
    rownames(net)[(nrow(net) - length(missing_rows) + 1):nrow(net)] <- missing_rows
  }
  if (length(missing_cols) > 0) {
    net <- cbind(net, matrix(0, nrow = nrow(net), ncol = length(missing_cols)))
    colnames(net)[(ncol(net) - length(missing_cols) + 1):ncol(net)] <- missing_cols
  }
  
  # Replace NA values with 0
  net[is.na(net)] <- 0
  
  # Store the matrix in the list
  nets[[paste("net", i, sep = "")]] <- net
}


# Now, nets will contain properly aligned matrices for each year

# Calculate interaction turnover (β_int) between two years
turnover_results <- list()

for (i in 1:(length(nets) - 1)) {
  
  # Get the matrices for the current and next year
  net1 <- nets[[i]]
  net2 <- nets[[i + 1]]
  
  # Calculate the number of shared interactions (a)
  shared_interactions <- sum((net1 > 0) & (net2 > 0))
  
  # Calculate the number of unique interactions in each network (b and c)
  unique_net1 <- sum(net1 > 0 & net2 == 0)
  unique_net2 <- sum(net2 > 0 & net1 == 0)
  
  # Calculate turnover (β_int)
  beta_int <- (unique_net1 + unique_net2) / (shared_interactions + unique_net1 + unique_net2)
  
  # Store the result in the turnover results list
  turnover_results[[paste(names(nets)[i], names(nets)[i + 1], sep = "_")]] <- beta_int
}

# View the turnover results
turnover_results



# Prepare data for bar plot
turnover_df <- data.frame(
  Year_Pair = names(turnover_results),
  Turnover = unlist(turnover_results)
)

# Create bar plot
ggplot(turnover_df[1:11,], aes(x = Year_Pair, y = Turnover, fill = Turnover)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Interaction Turnover Between Years", x = "Year Pair", y = "Interaction Turnover (β_int)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Function to calculate interaction rewiring (β_rw) between two networks
calculate_rw <- function(net1, net2) {
  
  # Ensure that the nets are binary (presence-absence matrix)
  net1_binary <- net1 > 0
  net2_binary <- net2 > 0
  
  # Find the number of interactions that are shared (both present in net1 and net2)
  shared_interactions <- sum(net1_binary & net2_binary)
  
  # Find the number of rewired interactions (shared interactions but with different species pairs)
  rewired_interactions <- sum((net1_binary & net2_binary) & (net1 != net2))
  
  # Calculate total possible interactions (this is the total number of species pairs in the two networks)
  total_possible_interactions <- length(net1) * (length(net1) - 1) / 2
  
  # Compute β_rw as the ratio of rewired interactions to total possible interactions
  beta_rw <- rewired_interactions / total_possible_interactions
  
  return(beta_rw)
}

# Loop over the years to calculate interaction rewiring (β_rw) for each pair of successive years
rewiring_matrix <- matrix(0, nrow = length(nets) - 1, ncol = length(nets) - 1)
rownames(rewiring_matrix) <- names(nets)[1:(length(nets) - 1)]
colnames(rewiring_matrix) <- names(nets)[2:length(nets)]

for (i in 1:(length(nets) - 1)) {
  
  # Get the matrices for the current and next year
  net1 <- nets[[i]]
  net2 <- nets[[i + 1]]
  
  # Calculate interaction rewiring (β_rw) between the two networks
  beta_rw <- calculate_rw(net1, net2)
  
  # Store the result in the rewiring matrix
  rewiring_matrix[i, i + 1] <- beta_rw
  rewiring_matrix[i + 1, i] <- beta_rw  # Symmetric matrix
}

# View the rewiring matrix
rewiring_matrix

# Load pheatmap library
library(pheatmap)

# Create heatmap of the rewiring matrix
pheatmap(
  rewiring_matrix,
  display_numbers = TRUE,    # Display rewiring values in the cells
  color = colorRampPalette(c("white", "blue"))(50), # Heatmap color gradient (white to blue)
  main = "Interaction Rewiring (β_rw) Across Successive Years",  # Title
  clustering_distance_rows = "euclidean",  # Cluster rows based on similarity
  clustering_distance_cols = "euclidean",  # Cluster columns based on similarity
  clustering_method = "complete",  # Linkage method for clustering
  fontsize = 10,  # Font size for labels
  angle_col = 45  # Angle for column labels
)


# Jaccard index #####
# Loop through each year to create individual bipartite matrices and calculate Jaccard Index
jaccard_data <- data.frame(plant_pair = integer(0), year = integer(0), jaccard_index = numeric(0))  # Data frame to store Jaccard results

for (i in unique(net1124_edited$rok)) {
  
  # Create the matrix for the current year
  net <- with(net1124_edited[net1124_edited$rok %in% c(i),], 
              tapply(corrected_number, list(druhK_opraveno, sitodruh), sum))
  
  # Ensure the row and column names are consistent with the full list of species
  rownames(net) <- make.names(rownames(net), unique = TRUE)  # Ensure unique names
  colnames(net) <- make.names(colnames(net), unique = TRUE)
  
  # Add rows and columns for missing species
  missing_rows <- setdiff(all_species, rownames(net))
  missing_cols <- setdiff(all_species, colnames(net))
  
  # Add missing species with 0 interactions
  if (length(missing_rows) > 0) {
    net <- rbind(net, matrix(0, nrow = length(missing_rows), ncol = ncol(net)))
    rownames(net)[(nrow(net) - length(missing_rows) + 1):nrow(net)] <- missing_rows
  }
  if (length(missing_cols) > 0) {
    net <- cbind(net, matrix(0, nrow = nrow(net), ncol = length(missing_cols)))
    colnames(net)[(ncol(net) - length(missing_cols) + 1):ncol(net)] <- missing_cols
  }
  
  # Replace NA values with 0
  net[is.na(net)] <- 0
  
  # Store the matrix in the list
  nets[[paste("net", i, sep = "")]] <- net
  
  # Calculate Jaccard Index for plant-pair sharing
  # Loop through all pairs of plants
  plant_names <- rownames(net)
  
  # Ensure that both plant_names[p1] and plant_names[p2] exist in the matrix
  for (p1 in 1:(length(plant_names) - 1)) {
    for (p2 in (p1 + 1):length(plant_names)) {
      
      # Check if plant names are present in the matrix
      if (plant_names[p1] %in% rownames(net) && plant_names[p2] %in% rownames(net)) {
        # Get the pollinator sets for both plants
        p1_pollinators <- which(net[plant_names[p1], ] > 0)
        p2_pollinators <- which(net[plant_names[p2], ] > 0)
        
        # Calculate the Jaccard Index
        jaccard_index <- length(intersect(p1_pollinators, p2_pollinators)) / 
          length(union(p1_pollinators, p2_pollinators))
        
        # Add the result to the data frame
        jaccard_data <- rbind(jaccard_data, data.frame(plant_pair = paste(plant_names[p1], plant_names[p2], sep = "_"),
                                                       year = i,
                                                       jaccard_index = jaccard_index))
      } else {
        # If plant names do not exist in the matrix, skip to next pair
        next
      }
    }
  }
}

# Now that we have all Jaccard indices calculated, we can visualize the changes over years
library(ggplot2)
ggplot(jaccard_data, aes(x = year, y = plant_pair, fill = jaccard_index)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Pollinator Sharing Across Years", x = "Year", y = "Plant Pair")

# Optionally, you can fit a linear model or use a GAM to assess whether the sharing changes significantly over time
library(mgcv)
gam_model <- gam(jaccard_index ~ s(year) + plant_pair, data = jaccard_data)
summary(gam_model)














library(vegan)

# Bray-Curtis dissimilarity
bray_matrix <- vegdist(net11, method = "bray")

# Convert to a matrix for inspection
bray_matrix <- as.matrix(bray_matrix)
print(bray_matrix)




### Je variabilita v podobnosti polinacnich spekter vetsi v case nebo prostoru? 



