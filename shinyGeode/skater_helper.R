library(spdep)
library(sf)
library(tmap)
library(dplyr)

prep_data <- function(data, yr, reg){
    filtered_data <- data %>% 
        filter(year==yr,region==reg)
    return(filtered_data)
}

# modified from prof code + wrangling process
run_skater <- function(data, meth, n_clusters){
    attribute_data <- data %>% 
        select(crimes, crime_rate) %>% 
        st_drop_geometry()
    
    
    proxmat <- dist(attribute_data, method = 'euclidean')
    hclust_ward <- hclust(proxmat, method = 'ward.D')
    groups <- as.factor(cutree(hclust_ward, k = 6))
    sf_cluster <- cbind(data, as.matrix(groups)) %>%
      rename(`CLUSTER` = `as.matrix.groups.`)
    
    sp_data <- as_Spatial(data)
    nb <- poly2nb(sp_data)
    coords <- st_coordinates(st_centroid(st_geometry(data)))
    
    nb[17] <- as.integer(64)
    nb[[69]] <- append(nb[[69]], as.integer(68))
    
    components <- n.comp.nb(nb)
    distance_matrix <- st_distance(st_centroid(st_geometry(data)))
    
    # without this nested for loop, there will be sub-graphs
    for (i in 1:(components$nc - 1)) {
      for (j in (i + 1):components$nc) {
        component_i <- which(components$comp.id == i)
        component_j <- which(components$comp.id == j)
        sub_distance_matrix <- distance_matrix[component_i, component_j, drop = FALSE]
        min_dist <- which(sub_distance_matrix == min(sub_distance_matrix), arr.ind = TRUE)
        point_i <- component_i[min_dist[1, 1]]
        point_j <- component_j[min_dist[1, 2]]
        nb[[point_i]] <- unique(c(nb[[point_i]], point_j))
        nb[[point_j]] <- unique(c(nb[[point_j]], point_i))
      }
    }
    
    n.comp.nb(nb)$nc
    
    lcosts <- nbcosts(nb, attribute_data)
    w <- nb2listw(nb, lcosts, style = "B")
    mst <- mstree(w)
    clustN <- spdep::skater(edges = mst[, 1:2],
      data = attribute_data,
      method = meth,
      ncuts = n_clusters - 1)

    groups_mat <- as.matrix(clustN$groups)
    skater_cluster <- cbind(sf_cluster, as.factor(groups_mat)) %>%
      rename(`CLUSTER` = `as.factor.groups_mat.`)
    
    return(skater_cluster)
}
