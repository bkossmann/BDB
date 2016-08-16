library(dplyr)
library(readr)
library(igraph)

setwd("~/Desktop/world-development-indicators")

not_countries <- list("Arab World", "Caribbean small states", "Central Europe and the Baltics", "Channel Islands", "Dominica", "East Asia & Pacific (all income levels)", "East Asia & Pacific (developing only)", "Europe & Central Asia (all income levels)", "Europe & Central Asia (developing only)", "European Union", "Fragile and conflict affected situations", "Heavily indebted poor countries (HIPC)", "High income", "High income: nonOECD", "High income: OECD", "Latin America & Caribbean (all income levels)", "Latin America & Caribbean (developing only)", "Least developed countries: UN classification", "Low & middle income", "Low income", "Lower middle income", "Middle East & North Africa (all income levels)", "Middle East & North Africa (developing only)", "Middle income", "OECD members", "Other small states", "Sub-Saharan Africa (all income levels)", "Sub-Saharan Africa (developing only)", "West Bank and Gaza", "World", "Euro area", "North America", "Pacific island small states", "Small states", "South Asia", "Upper middle income")

#Get all countries' GDPs since 1990
indicators <- read_csv("Indicators.csv")
country_indicators <- indicators[ ! indicators$CountryName %in% not_countries, ]
GDPPC_1990 <- country_indicators[ country_indicators$Year > "1990" & country_indicators$Year <= "2012" & country_indicators$IndicatorCode == "NY.GDP.PCAP.CD", ]
countries <- unique(GDPPC_1990[, c("CountryName") ])

#Take in dataframe, return all PCGDP values for that country
GDP_vec <- function(countryname) { 
    output <- unlist(GDPPC_1990[GDPPC_1990$CountryName == countryname, c("Value")])
    if (length(output) == 22) { #Need length-22 vector for covariance, otherwise throw it out
      return(as.vector(output))
      } 
    else { 
      cat("Insufficient data:", countryname, "\n")
      return()
      }
  }

GDP_list <- list()
#For each country with complete data, add to GDP list
for (x in unlist(countries)) {
  if (length(GDP_vec(x)) != 0){ GDP_list <- c(GDP_list, list(GDP_vec(x))) }
  else {countries <- countries[countries != x,]} #Remove countries with insufficient data from list
}

#Make the cross-correlation matrix
GDP_mat <- data.frame(GDP_list)
colnames(GDP_mat) <- unlist(countries)
cor_mat <- cor(GDP_mat)

#hm_palette <- colorRampPalette(colors("red", "white", "blue"), )
#heatmap(cor_mat, Rowv=NA, Colv="Rowv", symm=TRUE, col=hm_palette, cexRow=2, cexCol=2) #Saved as 5k pixels wide

#Get rid of diagonal elements
#This is a hack, but it will work in this case
for (i in seq(1,length(matrix))){ 
  if (matrix[i] == 1.0) { matrix[i] = 0 }
}

prune_mat <- function(mat, cutoff){ #If cutoff is set to 0.25, 25% of the points will be purged

  #Get rid of bottom half of non-zero correlations to simplify network
  #This also eliminates negative values, which the graph can't abide
  non_zeros = list()
  for (i in seq(1, length(mat))) {
    if (mat[i] > 0){
    non_zeros <- c(non_zeros, mat[i])
    }
  }
  non_zeros <- sort(unlist(non_zeros))
  mat_med = non_zeros[round(length(non_zeros)*cutoff)]
  for (i in seq(1,length(mat))) {
    if (mat[i] < mat_med){ mat[i] = 0 }
  }
  
  #The range of values is now from 0.91 to 0.99. This is not a big enough range
  #to appropriately weight the graph edges.
  min_not_zero = 1
  for (i in seq(1, length(mat))) { 
    if (mat[i] > 0 && mat[i] < min_not_zero) { 
      min_not_zero = mat[i]
    }
  }
  print(min_not_zero)
  #This spreads the edge weights between zero and 1.
  spread = max(mat) - min_not_zero
  print(spread)
  for (i in seq(1, length(mat))) {
    if ( mat[i] > 0 ){
      mat[i] = (mat[i] - min_not_zero) / spread
    }
  }
  return(mat)
}

cor_mat <- prune_mat(cor_mat, 0.98)

#Set up the network
gdp_net <- graph_from_adjacency_matrix(cor_mat, mode = "undirected", weighted = TRUE, diag = FALSE)

#plot.igraph(gdp_net, vertex.shape = "none", edge.curved = FALSE, edge.width = 0.1)

#We should delete all vertices with 0 neighbors, as they will mess with the communities
gdp_net <- delete.vertices(gdp_net, which(degree(gdp_net) < 1))

gdp_comms <- cluster_edge_betweenness(gdp_net, directed = FALSE)
comm_members <- membership(gdp_comms)
comm_members
#plot.igraph(gdp_net, vertex.shape = "none", edge.curved = FALSE, edge.width = 1.5, vertex.label.cex = 0.6)

#layout <- layout.fruchterman.reingold(gdp_net)

#tkplot(gdp_net, layout=layout,  vertex.size=1, vertex.label.color="black")
