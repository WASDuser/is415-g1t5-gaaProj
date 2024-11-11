pacman::p_load(shiny, sf, tmap, bslib, tidyverse, spdep,
               sfdep, shinydashboard, shinythemes, lubridate)

msia <- read_rds("data/rds/msia.rds")
msia_left_join <- read_rds("data/rds/msia_left_join.rds")
msia_total_crimes <- read_rds("data/rds/msia_total_crimes.rds")
east_msia <- read_rds("data/rds/EAST_MSIA.rds")

##### visualisation #####


#========================#
###### Shiny UI ######
#========================#  

#|fig-width: 12
#|fig-height: 10
#|
basemap <- tm_shape(msia_total_crimes) +
  tm_borders(alpha = 0.5) +
  tm_text("ADM2_EN", size=0.4) + 
  tm_layout(
    main.title = "Basemap with districts",
    main.title.size = 1,
    main.title.position = "center",
    legend.show = FALSE,
    frame = FALSE)


ui <- navbarPage(
  title = "Malaysia",
  fluid = TRUE,
  theme=shinytheme("cosmo"),
  id = "navbarID",
  navbarMenu("EDA",
             tabPanel("Basemap",
                      fluidPage(
                        fluidRow(
                          column(12, 
                                 sliderInput(inputId = "basemap_opacity",
                                             label = "Basemap Transparency",
                                             min = 0,
                                             max = 1,
                                             value = 0.5)
                          )
                        ),
                        fluidRow(
                          column(12,
                                 tmapOutput("basemapOutput", width = "100%", height = 580)
                          )
                        )
                      )
             ),
             tabPanel("Visualisation",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "variable",
                                      label = "Types of Crimes",
                                      choices = list("total crimes" = "total_crimes", 
                                                     "causing injury" = "total_crimes_causing_injury",
                                                     "murder" = "total_crimes_murder",
                                                     "rape" = "total_crimes_rape",
                                                     "robbery gang armed" = "total_crimes_robbery_gang_armed",
                                                     "robbery gang unarmed" = "total_crimes_robbery_gang_unarmed",
                                                     "robbery gang solo armed" = "total_crimes_robbery_solo_armed",
                                                     "robbery gang solo unarmed" = "total_crimes_robbery_solo_unarmed"
                                      ),
                                      selected = "total_crimes"),
                          selectInput(inputId = "classification",
                                      label = "Classification method:",
                                      choices = list("sd" = "sd", 
                                                     "equal" = "equal", 
                                                     "pretty" = "pretty", 
                                                     "quantile" = "quantile", 
                                                     "kmeans" = "kmeans", 
                                                     "hclust" = "hclust", 
                                                     "bclust" = "bclust", 
                                                     "fisher" = "fisher", 
                                                     "jenks" = "jenks"),
                                      selected = "equal"),
                          sliderInput(inputId = "classes",
                                      label = "Number of classes",
                                      min = 5,
                                      max = 10,
                                      value = c(6)),
                          selectInput(inputId = "colour",
                                      label = "Colour scheme:",
                                      choices = list("blues" = "Blues", 
                                                     "reds" = "Reds", 
                                                     "greens" = "Greens",
                                                     "Yellow-Orange-Red" = "YlOrRd",
                                                     "Yellow-Orange-Brown" = "YlOrBr",
                                                     "Yellow-Green" = "YlGn",
                                                     "Orange-Red" = "OrRd"),
                                      selected = "YlOrRd"),
                          sliderInput(inputId = "opacity",
                                      label = "Level of transparency",
                                      min = 0,
                                      max = 1,
                                      value = c(0.5))
                        ),
                        mainPanel(
                          tmapOutput("mapPlot",
                                     width = "100%", 
                                     height = 580)
                        )
                      )
             ),
  ),
  navbarMenu("Spatial Weights and Application",
             tabPanel("Global Measures",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Adaptive Weight Matrix Plot",
                                   plotOutput("adaptiveWeightPlot", width = "100%", height = 600)
                          ),
                          tabPanel("Moran's I Histogram",
                                   plotOutput("moranHistogram", height = 400)
                          ),
                          tabPanel("Spatial Correlogram",
                                   plotOutput("spatialCorrelogram", height = 400),
                                   verbatimTextOutput("correlogramResults")
                          )
                        )
                      )
             ),
             tabPanel("Local Measures",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Local MI plots",
                                   plotOutput("localMIPlots", width = "100%", height = 600)
                          ),
                          tabPanel("Moran Scatter plots",
                                   selectInput("scatterPlotType", "Select Plot Type:",
                                               choices = c("Moran Scatterplot" = "standard", "Moran Scatterplot with Standardized Variable" = "standardized")
                                   ),
                                   plotOutput("scatterPlots", height = 400)
                          ),
                          tabPanel("LISA Map",
                                   plotOutput("lisaMap", height = 400),
                          ),
                          tabPanel("Gi Map",
                                   plotOutput("giMap", height = 500)
                          ),
                        )
                      )
             ),
  ),
  navbarMenu("Spatial Model")
)

#========================#
###### Shiny Server ######
#========================# 

server <- function(input, output){
  output$basemapOutput <- renderTmap({
    tm_shape(msia_total_crimes) +
      tm_fill(col = "lightblue", alpha = input$basemap_opacity) +  # Add color and transparency
      tm_borders(alpha = 0.5) +
      tm_text("ADM2_EN", size = 0.7) + 
      tm_layout(
        main.title = "Basemap with districts",
        main.title.size = 1,
        main.title.position = "center",
        legend.show = FALSE,
        frame = FALSE
      ) +
      tm_view(set.view = c(lon = 112.796783, lat = 3.420404, zoom = 7))  # More zoomed-in view
  })
  
  output$mapPlot <- renderTmap({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(msia_total_crimes) +  # Show ADM2_EN on hover
      tm_fill(
        input$variable,
        n = input$classes,
        style = input$classification,
        palette = input$colour,
        alpha = input$opacity,
        popup.vars = c("Total Crimes" = "total_crimes", "District" = "ADM2_EN", "State" = "ADM1_EN")
      ) +
      tm_borders(lwd = 0.1, alpha = 1) +
      tm_view(set.zoom.limits = c(6.5, 8))
  })
  
  
  #==========================================================
  # Global Measures of Spatial AutoCorrelation
  #==========================================================   
  
  longitude <- map_dbl(msia_total_crimes$geometry, ~st_centroid(.x)[[1]])
  latitude <- map_dbl(msia_total_crimes$geometry, ~st_centroid(.x)[[2]])
  coords <- cbind(longitude, latitude)
  
  knn6 <- knn2nb(knearneigh(coords, k=6))
  
  rsknn6 <- nb2listw(knn6, 
                     style="W", 
                     zero.policy = TRUE)
  
  
  moran.test(msia_total_crimes$total_crimes, 
             listw=rsknn6, 
             zero.policy = TRUE, 
             na.action=na.omit)
  
  
  set.seed(1234)
  bperm_moran= moran.mc(msia_total_crimes$total_crimes, 
                        listw=rsknn6, 
                        nsim=999, 
                        zero.policy = TRUE, 
                        na.action=na.omit)
  
  data <- data.frame(simulated_morans_i = bperm_moran$res[1:999])
  
  ggplot(data, aes(x = simulated_morans_i)) +
    geom_histogram(binwidth = (max(data$simulated_morans_i) - min(data$simulated_morans_i)) / 20, 
                   color = "black", fill = "pink") +  # Histogram
    geom_vline(xintercept = 0, color = "red", linetype = "solid") +  # Vertical line at x=0
    labs(x = "Simulated Moran's I", y = "Frequency") +  # Labels for x and y axis
    theme_minimal()  # Clean minimal theme
  
  
  MI_corr <- sp.correlogram(knn6,                            
                            msia_total_crimes$total_crimes,
                            order=6,  #lag-value: 6                           
                            method="I",                            
                            style="W",
                            zero.policy = TRUE) 
  
  #==========================================================
  # Render Global Output Maps
  #==========================================================
  
  output$adaptiveWeightPlot <- renderPlot({
    # Assuming msia_total_crimes and knn6 are pre-defined in the server
    plot(msia_total_crimes$geometry, border = "lightgrey") # Plot the geometry
    plot(knn6, coords, pch = 19, cex = 0.6, add = TRUE, col = "red") # Overlay knn plot
  })
    
    
  output$moranHistogram <- renderPlot({
    data <- data.frame(simulated_morans_i = bperm_moran$res[1:999])
    
    ggplot(data, aes(x = simulated_morans_i)) +
      geom_histogram(binwidth = (max(data$simulated_morans_i) - min(data$simulated_morans_i)) / 20, 
                     color = "black", fill = "pink") +
      geom_vline(xintercept = 0, color = "red", linetype = "solid") +
      labs(x = "Simulated Moran's I", y = "Frequency") +
      theme_minimal()
  })
  
  # Spatial correlogram
  output$spatialCorrelogram <- renderPlot({
    plot(MI_corr, main="Spatial Correlogram of Total Crimes")
  })
  
  output$correlogramResults <- renderPrint({
    # Replace this with actual correlogram results calculation
    correlogramResults <- "Sample results for correlogram"  # Placeholder for actual results
    print(MI_corr)
  })
  
    #==========================================================
    # Local Measures of Spatial AutoCorrelation
    #==========================================================   
    
  
  knn6_lw <- nb2listw(knn6, style = 'W')
  
  fips <- order(msia_total_crimes$ADM2_EN)
  localMI <- localmoran(msia_total_crimes$total_crimes, knn6_lw)
  
  
  msia_total_crimes.localMI <- cbind(msia_total_crimes,localMI) %>%
    rename(Pr.Ii = Pr.z....E.Ii..)
  
  quadrant <- vector(mode="numeric",length=nrow(localMI))
  msia_total_crimes$lag_total_crimes <- lag.listw(knn6_lw, msia_total_crimes$total_crimes)
  
  DV <- msia_total_crimes$lag_total_crimes - mean(msia_total_crimes$lag_total_crimes)    
  LM_I <- localMI[,1] - mean(localMI[,1])    
  signif <- 0.05       
  quadrant[DV <0 & LM_I>0] <- 1
  quadrant[DV >0 & LM_I<0] <- 2
  quadrant[DV <0 & LM_I<0] <- 3  
  quadrant[DV >0 & LM_I>0] <- 4     
  quadrant[localMI[,5]>signif] <- 0
  
  ####GI stats for adaptive
  fips <- order(msia_total_crimes$ADM2_EN) 
  gi.adaptive <- localG(msia_total_crimes$total_crimes, knn6_lw) 
  msia_total_crimes.gi <- cbind(msia_total_crimes, as.matrix(gi.adaptive)) %>%   
    rename(gstat_adaptive = as.matrix.gi.adaptive.)  
  
  # Find the threshold for hotspot areas (e.g., top 5% of Gi scores) 
  threshold <- quantile(msia_total_crimes.gi$gstat_adaptive, 0.95)    
  hotspots <- msia_total_crimes.gi %>%   
    filter(gstat_adaptive >= threshold)
  
  
  hotspots_province_gstatscore <- hotspots %>%   
    select(ADM2_EN, gstat_adaptive) %>%   
    arrange(desc(gstat_adaptive))   
  
  
  
  
    #==========================================================
    # Render Local output maps
    #==========================================================
  
  output$localMIPlots <- renderPlot({
    # Read in the map data if not already read elsewhere
    localMI_statistic.map <- read_rds("data/rds/localMI_statistic.map.rds")
    localMI_pvalues.map <- read_rds("data/rds/localMI_pvalues.map.rds")
    
    # Arrange the maps side by side
    tmap_arrange(localMI_statistic.map, localMI_pvalues.map, asp = 1, ncol = 2)
  })
  
  output$scatterPlots <- renderPlot({
    # Check the selected scatter plot type
    if (input$scatterPlotType == "standard") {
      # Standard Moran Scatterplot
      moran.plot(msia_total_crimes$total_crimes, knn6_lw,
                 labels = as.character(msia_total_crimes$ADM2_EN),
                 xlab = "Total Crimes over all years",
                 ylab = "Spatially Lag Total Crimes over all years")
    } else if (input$scatterPlotType == "standardized") {
      # Standardize the variable and then plot
      msia_total_crimes$Z.total_crimes <- scale(msia_total_crimes$total_crimes) %>% as.vector
      moran.plot(msia_total_crimes$Z.total_crimes, knn6_lw,
                 labels = as.character(msia_total_crimes$ADM2_EN),
                 xlab = "z- Total Crimes over all years",
                 ylab = "Spatially Lag z- Total Crimes over all years")
    }
  })
  
  output$lisaMap <- renderPlot({
    
    total_crimes <- qtm(msia_total_crimes, "total_crimes")
    
    
    msia_total_crimes.localMI$quadrant <- quadrant
    colors <- c("#ffffff", "#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")
    clusters <- c("insignificant", "low-low", "low-high", "high-low", "high-high")
    
    LISAmap <- tm_shape(msia_total_crimes.localMI) +
      tm_fill(col = "quadrant", 
              style = "cat", 
              palette = colors[c(sort(unique(quadrant)))+1], 
              labels = clusters[c(sort(unique(quadrant)))+1],
              popup.vars = c("")) +
      tm_view(set.zoom.limits = c(11,17)) +
      tm_borders(alpha=0.5)
    
    tmap_arrange(total_crimes, LISAmap, 
                 asp=1, ncol=2)
  })
  
  output$giMap <- renderPlot({
    Gimap <- tm_shape(msia_total_crimes.gi) +    
      tm_fill(col = "gstat_adaptive",            
              style = "pretty",            
              palette="-RdBu",            
              title = "local Gi") +    
      tm_borders(alpha = 0.5) +   
      tm_shape(hotspots) +    
      tm_text("ADM2_EN", size = 0.7, col="blue") +    
      tm_layout(     
        main.title = "Gi map showing hotspots",     
        main.title.size = 1,     
        main.title.position = "center",     
        legend.show = TRUE,     
        legend.text.size = 1,   # Reduce the text size     
        frame = FALSE)     
    
    print(Gimap)
  })

    
}

shinyApp (ui=ui, server=server)

