pacman::p_load(shiny, sf, tmap, bslib, tidyverse, spdep, sfdep, shinydashboard, shinythemes, lubridate, ggpubr, cluster, factoextra, NbClust, ClustGeo, heatmaply, corrplot, psych, GGally)
library(proxy)

prep_data <- function(data, yr, reg){
  filtered_data <- data %>% 
    filter(year==yr,region==reg)
  
  
  
  return(filtered_data)
}

run_clust <- function(filtered_data, meth, n_clusters){
  print("---------- Inside run_clust ----------")
  
  filtered_data <- filtered_data %>% select(-Shape_Leng, -Shape_Area)
  # filtered_data <- st_transform(filtered_data, crs = 32649)
  
  print(filtered_data)
  print(st_crs(filtered_data))
  
  
  attribute_data <- filtered_data %>% 
    select(crimes, crime_rate) %>% 
    st_drop_geometry()
  
  
  proxmat <- dist(attribute_data, method = meth)
  hclust_ward <- hclust(proxmat, method = 'ward.D')
  
  
  nongeo_cluster <- hclustgeo(proxmat)

  # plot(nongeo_cluster, cex = 0.5)
  # rect.hclust(nongeo_cluster, 
  #             k = 6, 
  #             border = 2:5)
  
  
  #mapping the clusters
  groups <- as.factor(cutree(nongeo_cluster, k=n_clusters))
  ngeo_cluster <- cbind(filtered_data, as.matrix(groups)) %>%
    rename(`CLUSTER` = `as.matrix.groups.`)
  
  
  dist <- st_distance(filtered_data, filtered_data)
  distmat <- as.dist(dist)
  
  print(33333)

  cr <- choicealpha(proxmat, distmat, range.alpha = seq(0, 1, 0.1), K=n_clusters, graph = TRUE) # return this
  
  clustG <- hclustgeo(proxmat, distmat, alpha = 0.2)
  groups <- as.factor(cutree(clustG, k=n_clusters))
  sf_Gcluster <- cbind(filtered_data, as.matrix(groups)) %>%
    rename(`CLUSTER` = `as.matrix.groups.`)
  
  sf_Gcluster <- st_as_sf(sf_Gcluster)
  print(sf_Gcluster)

  return(list(ngeo_cluster = ngeo_cluster, sf_Gcluster = sf_Gcluster, cr = cr))
  # return(ngeo_cluster)
}

