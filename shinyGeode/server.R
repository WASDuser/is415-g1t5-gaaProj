function(input, output, session) {
    
    
    dataset_reactive <- reactive({
        req(input$dataset)
        data <- switch(input$dataset,
            "crime_merged_sf" = crime_merged_sf)
        data
    })
    
    
    esda_dataset_reactive <- reactive({
        req(input$esda_dataset)
        data <- switch(input$esda_dataset,
            "crime_merged_sf" = crime_merged_sf)
        data
    })
    
    
    # Preview the dataset with up to 20 rows (when "Preview" is selected)
    output$data_table <- DT::renderDataTable({
        req(input$display_option == "preview")
        
        data <- dataset_reactive()
        
        data_display_options <- list( # default display
            dom = 'Bfrtip',
            buttons = c('copy', 'excel', 'pdf'),
            pageLength = 20,
            scrollX = TRUE,
            scrollY = '333px',
            fixedHeader = TRUE)
        
        DT::datatable(
            data, 
            options=data_display_options, 
            extensions = 'Buttons',
            rownames = FALSE)
    })
    
    
    # Render code output for str() or glimpse()
    output$code_output <- renderPrint({
        data <- dataset_reactive()
        
        if (input$display_option == "str") {
            str(data)
        } else if (input$display_option == "glimpse") {
            glimpse(data)
        }
    })
    
    
    # Dynamic description based on the dataset
    output$description_text <- renderText({
        switch(input$dataset,
            "crime_merged_sf" = "Description for crime_merged_sf"
        )
    })
    
    
    # Update variable choices based on selected dataset
    observe({
        req(input$dataset)
        data <- dataset_reactive()
        
        # Get numeric variable names
        numeric_vars <- names(data)[sapply(data, is.numeric)]
        
        updateSelectInput(session, "variable", 
            choices = numeric_vars,
            selected = NULL) # Reset selection
    })
    
    
    # Dynamic UI for variable selection based on continuous/categorical
    output$var_select <- renderUI({
        data <- dataset_reactive()
        if (input$var_type == "cat") {
            selectInput("variable", "Choose Categorical Variable:", 
                choices = names(data)[sapply(data, is.character)])
        } else {
            selectInput("variable", "Choose Continuous Variable:", 
                choices = names(data)[sapply(data, is.numeric)])
        }
    })
    
    
    # Display summary statistics
    output$summary_table <- renderPrint({
        req(input$submit_eda)    # Ensure that the submit button has been clicked
        req(input$variable)       # Ensure that a variable has been selected
        
        data <- dataset_reactive()
        variable <- data[[input$variable]] # Get the selected variable
        summary_output <- summary(as.data.frame(variable)) # Get summary statistics
        colnames(summary_output) <- input$variable # Set the selected variable as column name
        
        summary_output
    })
    
    
    # Dynamic chart type options
    observeEvent(input$var_type, {
        updateSelectInput(session, "chart_type", choices = if (input$var_type == "cat") {
            c("Bar Chart")
        } else {
            # [FIX] - time-series plot
            # c("Histogram", "Boxplot", "Time Series")
            
            c("Histogram", "Boxplot")
        })
    })
    
    
    # Render EDA plot/statistics upon submit button click 
    observeEvent(input$submit_eda, {
        req(input$submit_eda)
        output$eda_plot <- renderPlot({
            data <- dataset_reactive()
            
            color <- "#428bca"
            
            if (input$chart_type == "Bar Chart") {
                ggplot(data, aes(x = .data[[input$variable]])) + geom_bar(fill=color) + 
                    theme(axis.text.x = element_text(angle = 30))
                
            } else if (input$chart_type == "Histogram") {
                ggplot(data, aes(x = .data[[input$variable]])) + 
                    geom_histogram(binwidth = 5, color="black", fill=color)
                
            } else if (input$chart_type == "Boxplot") {
                ggplot(data, aes(y = .data[[input$variable]])) + geom_boxplot(fill=color)
                
            }
            # [FIX] - time-series plot
            # else if (input$chart_type == "Time Series") {
            #     data$date.y <- as.Date(data$date.y)
            #     data <- data %>% 
            #         group_by(date.y) %>% 
            #         summarise(value = mean(.data[[input$variable]], na.rm = TRUE))
            #     
            #     ggplot(data, aes(x = date.y, y = value)) + 
            #         geom_line()
            # }
        })})
    
    
    # Dynamic choro var options
    observe({
        data <- esda_dataset_reactive()
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        
        updateSelectInput(session, "esda_variable", 
            choices = numeric_cols,
            selected = numeric_cols[1]  # Set a default selection
        )
    })
    
    
    # Dynamic crime type options
    observe({
        data <- esda_dataset_reactive()
        crime_types <- unique(data$type)
        
        updateSelectInput(session, "crime_type",
            choices = crime_types,
            selected = crime_types[1])
    })
    
    
    # Dynamic crime type options - local
    observe({
        data <- esda_dataset_reactive()
        crime_types <- unique(data$type)
        
        updateSelectInput(session, "local_crime_type",
            choices = crime_types,
            selected = crime_types[1])
    })
    
    
    # Dynamic crime type options - global
    observe({
        data <- esda_dataset_reactive()
        crime_types <- unique(data$type)
        
        updateSelectInput(session, "global_crime_type",
            choices = crime_types,
            selected = crime_types[1])
    })
    
    
    # Dynamic region options
    observe({
        data <- esda_dataset_reactive()
        regions <- unique(data$region)
        
        updateSelectInput(session, "region",
            choices = regions,
            selected = regions[1])
    })
    
    
    # Dynamic region options - local
    observe({
        data <- esda_dataset_reactive()
        regions <- unique(data$region)
        
        updateSelectInput(session, "local_region",
            choices = regions,
            selected = regions[1])
    })
    
    
    # Dynamic region options - global
    observe({
        data <- esda_dataset_reactive()
        regions <- unique(data$region)
        
        updateSelectInput(session, "global_region",
            choices = regions,
            selected = regions[1])
    })
    
    
    # Dyanmic classification options
    observe({
        styles <- list(
            "sd" = "sd", 
            "equal" = "equal", 
            "pretty" = "pretty", 
            "quantile" = "quantile", 
            "kmeans" = "kmeans", 
            "hclust" = "hclust", 
            "bclust" = "bclust", 
            "fisher" = "fisher", 
            "jenks" = "jenks",
            "cont" = "cont",
            "order" = "order",
            "log10" = "log10") # TODO: handle log(0) error
        
        updateSelectInput(session, 'classification',
            choices = styles,
            selected = styles[1])
    })
    
    
    # Dynamic colors
    observe({
        c_palette <- list(
            "blues" = "Blues", 
            "reds" = "Reds", 
            "greens" = "Greens",
            "Yellow-Orange-Red" = "YlOrRd",
            "Yellow-Orange-Brown" = "YlOrBr",
            "Yellow-Green" = "YlGn",
            "Orange-Red" = "OrRd")
        
        updateSelectInput(session, 'colors',
            choices = c_palette,
            selected = c_palette[1])
    })
    
    
    # Dyanmic time period options - local
    observe({
        data <- esda_dataset_reactive()
        
        if ("date.x" %in% colnames(data)){ # hardcode
            years <- unique(year(data$date.x))
        } else{years <- unique(year(data$date))}
        
        updateSelectInput(session, 'local_time_period',
            choices = years,
            selected = years[1])
    })
    
    
    # Dyanmic time period options - global
    observe({
        data <- esda_dataset_reactive()
        
        if ("date.x" %in% colnames(data)){ # hardcode
            years <- unique(year(data$date.x))
        } else{years <- unique(year(data$date))}
        
        updateSelectInput(session, 'global_time_period',
            choices = years,
            selected = years[1])
    })
    
    
    # Choloropleth output
    choropleth_map <- eventReactive(input$submit_esda,{
        req(input$esda_variable)
        req(input$esda_dataset)
        req(input$crime_type)
        req(input$region)
        req(input$classification)
        req(input$n_classes)
        req(input$colors)
        
        chosen_data <- input$esda_dataset
        chosen_var <- input$esda_variable
        chosen_crime <- input$crime_type
        chosen_region <- input$region
        chosen_class <- input$classification
        chosen_n <- input$n_classes
        chosen_color <- input$colors
        
        data <- esda_dataset_reactive() %>% ungroup() %>% st_as_sf() %>%
            filter(region == chosen_region) %>% 
            filter(type == chosen_crime)
        
        # for debugging ---------------------------------------------------
        print(chosen_data)
        print('---------------------------------------------------------------')
        print(chosen_var)
        print('---------------------------------------------------------------')
        print(chosen_crime)
        print('---------------------------------------------------------------')
        print(chosen_region)
        print('---------------------------------------------------------------')
        print(chosen_class)
        print('---------------------------------------------------------------')
        print(chosen_n)
        print('---------------------------------------------------------------')
        print(chosen_color)
        print('---------------------------------------------------------------')
        
        str(data)

        tmap_mode('view')
        tmap_options(check.and.fix = TRUE)
        tm_shape(data) +
            tm_fill(
                col = chosen_var,
                palette = chosen_color,
                style = chosen_class,
                n = chosen_n,
            ) +
            tm_layout(
                main.title = paste(chosen_crime))
    })
    output$choro_map <- renderTmap({
        choropleth_map()
    })
    
    
    # Global Moran I - table
    globalMIResults <- eventReactive(input$submit_global,{
        chosen_crime <- input$global_crime_type
        chosen_region <- input$global_region
        chosen_year <- input$global_time_period
        
        data <- esda_dataset_reactive() %>% ungroup() %>% st_as_sf() %>% 
            filter(region == chosen_region) %>% 
            filter(type == chosen_crime) %>% 
            # missing data if any
            filter(!is.na(crime_rate))
        
        if ("date.x" %in% colnames(data)){ # hardcode
            data <- data %>% filter(year(data$date.x) == chosen_year)
        } else{data <- data %>% filter(year(data$date) == chosen_year)}
        
        nb <- st_contiguity(data$geometry, queen = as.logical(input$global_contiguity))
        
        # thanks to santhya~
        nb[17]<- as.integer(19) #handle case for langkawi, which is not connected to the others by admin boundary
        
        # empty nb indices
        # crime_boundary: 17, 63
        # west: 17
        # east: 20
        
        # Computing Contiguity Spatial Weights
        wm_q <- data %>%
            mutate(
                nb = nb,
                wt = st_weights(nb, style = input$global_MoranWeights))
        
        global_moran_results <- global_moran_perm(
            wm_q$crime_rate,
            wm_q$nb,
            wm_q$wt,
            nsim = as.numeric(input$global_MoranSims))
        
        return(global_moran_results)
    })
    output$global_output <- renderDataTable({
        
        data_display_options <- list(
            pageLength = 1,
            scrollX = TRUE,
            scrollY = '333px',
            fixedHeader = TRUE)
        
        data <- globalMIResults() %>% as.data.frame()
        
        DT::datatable(
            data, 
            options=data_display_options, 
            rownames = FALSE)
    })
    
    
    # Global Moran I - histogram
    globalHist <- eventReactive(input$submit_global,{
        global_moran_results <- globalMIResults()
        
        hist_plot <- hist(
            global_moran_results$res,
            freq=TRUE,
            breaks=10,
            xlab="Histogram of Simulated Moran's Is"
        ) 
        abline(v=0, col='red')
    })
    output$global_hist <- renderPlot({
        globalHist()
    })
    
    
    # LISA
    localMIResults <- eventReactive(input$MoranUpdate,{
        
        chosen_crime <- input$local_crime_type
        chosen_region <- input$local_region
        chosen_year <- input$local_time_period
        
        data <- esda_dataset_reactive() %>% ungroup() %>% st_as_sf() %>% 
            filter(region == chosen_region) %>% 
            filter(type == chosen_crime) %>% 
            # missing data if any
            filter(!is.na(crime_rate))
        
        if ("date.x" %in% colnames(data)){ # hardcode
            data <- data %>% filter(year(data$date.x) == chosen_year)
        } else{data <- data %>% filter(year(data$date) == chosen_year)}
        
        nb <- st_contiguity(data$geometry, queen = as.logical(input$Contiguity))
        
        # thanks to santhya~
        nb[17]<- as.integer(19) #handle case for langkawi, which is not connected to the others by admin boundary
        
        # empty nb indices
        # crime_boundary: 17, 63
        # west: 17
        # east: 20
        
        # Computing Contiguity Spatial Weights
        wm_q <- data %>%
            mutate(
                nb = nb,
                wt = st_weights(nb, style = input$MoranWeights))
        
        # Computing Local Moran's I
        
        lisa <- wm_q %>%
            mutate(
                local_moran = local_moran(
                    wm_q$crime_rate,
                    wm_q$nb,
                    wm_q$wt,
                    nsim = as.numeric(input$MoranSims)),.before = 1) %>%
            unnest(local_moran)
        
        lisa <- lisa %>%
            rename(
                "local moran(ii)" = "ii", "expectation(eii)" = "eii",
                "variance(var_ii)" = "var_ii", "std deviation(z_ii)" = "z_ii",
                "p_value" = "p_ii")
        
        # glimpse(data)
        # View(nb)
        # View(wm_q)
        # View(lisa)
        return(lisa)
    })
    
    # LISA - statistical
    output$LocalMoranMap <- renderTmap({
        df <- localMIResults()
        
        if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
        
        # Map creation using tmap
        localMI_map <- tm_shape(df) +
            tm_fill(col = input$localmoranstats,
                style = "pretty",
                palette = "RdBu",
                title = input$localmoranstats) +
            tm_borders() +
            tm_view(set.zoom.limits = c(6, 7))
        
        localMI_map
    })
    
    # LISA - the "presentable" one according to prof
    output$LISA <- renderTmap({
        df <- localMIResults()
        
        if(is.null(df)) return()
        
        lisa_sig <- df  %>%
            filter(p_value < as.numeric(input$MoranConf))
        
        lisamap <- tm_shape(df) +
            tm_polygons() +
            tm_borders() +
            
            tm_shape(lisa_sig) +
            tm_fill(col = input$LisaClass,
                palette = "-RdBu",
                title = (paste("Significance:", input$LisaClass))) +
            tm_borders(alpha = 0.4) +
            tm_view(set.zoom.limits = c(6, 7))
        
        lisamap
    })
    
    
    
    #========================#
    ##### Render Hclust ######
    #========================#
    
    hclust_params <- eventReactive(input$HclustPlots, {
      list(
        proxMethod = input$proxMethod,
        hclustMethod = input$hclustMethod,
        year = input$year4,
        region = input$region4,
        optimalClust = input$optimalClust
      )
    })
    
    
    dendogram <- eventReactive(input$HclustPlots, {
      params <- hclust_params()
      
      rate_crime_data <- rate_crime_prep %>% 
        filter(year == params$year) %>% 
        filter(region == params$region)
      
      rate_crime_data <- as.data.frame(rate_crime_data)
      row.names(rate_crime_data) <- rate_crime_data$district 
      
      rate_crime_data <- rate_crime_data %>% 
        select(-c(2))
      
      rate_crime_data.std <- normalize(rate_crime_data)
      
      rate_crime_data_numeric <- rate_crime_data %>% select(where(is.numeric))
      rate_crime_data_numeric <- rate_crime_data_numeric %>% drop_na()
      proxmat <- dist(rate_crime_data_numeric, method = params$proxMethod)
      
      return(proxmat)
    })
    
    output$dendogram <- renderPlot({
      req(dendogram())
      
      prox <- dendogram()
      params <- hclust_params()
      
      hclust_ward <- hclust(prox, method = params$hclustMethod)
      plot(hclust_ward, cex = 0.7)
      rect.hclust(hclust_ward, 
                  k = params$optimalClust, 
                  border = 2:5)
    })
    
    
    
    heatmap<-eventReactive(input$HclustPlots,{
      
      print("button clicked, generating heatmap...")
      
      rate_crime_data <- rate_crime_prep %>% 
        filter(year == input$year4) %>% 
        filter(region == input$region4)
      
      rate_crime_data <- as.data.frame(rate_crime_data)
      row.names(rate_crime_data) <- rate_crime_data$district 
      
      rate_crime_data<- rate_crime_data %>% 
        select(-c(2))
      
      rate_crime_data.std <- normalize(rate_crime_data)
      
      return(rate_crime_data.std)
      
    })
    
    heatmap <- eventReactive(input$HclustPlots, {
      params <- hclust_params()
      
      rate_crime_data <- rate_crime_prep %>% 
        filter(year == params$year) %>% 
        filter(region == params$region)
      
      rate_crime_data <- as.data.frame(rate_crime_data)
      row.names(rate_crime_data) <- rate_crime_data$district 
      
      rate_crime_data <- rate_crime_data %>% 
        select(-c(2))
      
      rate_crime_data.std <- normalize(rate_crime_data)
      
      return(rate_crime_data.std)
    })
    
    output$heatmap <- renderPlotly({
      rate_crime_data.std <- heatmap()
      params <- hclust_params()
      
      map <- heatmaply(
        rate_crime_data.std,
        Colv = NA,
        dist_method = "euclidean",
        hclust_method = params$hclustMethod,
        seriate = "OLO",
        colors = Purples,
        k_row = params$optimalClust,
        margins = c(NA, 200, 60, NA),
        fontsize_row = 4,
        fontsize_col = 5,
        main = "Geographic Segmentation of Malaysia by Crime Type",
        xlab = "Crime Type",
        ylab = "Districts"
      )
      map
    })
    
    hclustMap <- eventReactive(input$HclustPlots, {
      params <- hclust_params()
      
      rate_crime_data <- rate_crime_prep %>% 
        filter(year == params$year) %>% 
        filter(region == params$region)
      
      rate_crime_data <- as.data.frame(rate_crime_data)
      row.names(rate_crime_data) <- rate_crime_data$district 
      
      rate_crime_data <- rate_crime_data %>% 
        select(-c(2))
      
      rate_crime_data.std <- normalize(rate_crime_data)
      
      rate_crime_data_numeric <- rate_crime_data %>% select(where(is.numeric))
      rate_crime_data_numeric <- rate_crime_data_numeric %>% drop_na()
      proxmat <- dist(rate_crime_data_numeric, method = "euclidean")
      
      hclust_ward <- hclust(proxmat, method = params$hclustMethod)
      groups <- as.factor(cutree(hclust_ward, k = params$optimalClust))
      
      rate_crime_cluster <- cbind(rate_crime_data, as.matrix(groups)) %>%
        rename(`CLUSTER` = `as.matrix(groups)`)
      
      return(rate_crime_cluster)
    })
    
    output$hclustMap <- renderTmap({
      rate_crime_cluster <- hclustMap()
      rate_crime_cluster <- st_as_sf(rate_crime_cluster)
      qtm(rate_crime_cluster, "CLUSTER")
    })
    
    
    #ClustGeo
    clustGeo_dataset_reactive <- reactive({
      req(input$clustGeo_dataset)
      data <- switch(input$clustGeo_dataset,
                     "crime_merged_sf" = crime_merged_sf)
      
    })
    
    observe({
      dist_methods <- list("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "mahalanobis")
      updateSelectInput(session, 'clustGeo_method',
                        choices = dist_methods,
                        selected = dist_methods[1])
    })
    
    source('clustGeo.R')
    
    # observeEvent(input$run_clustGeo,{
    #   print('---------- start ----------')
    #   data <- clustGeo_dataset_reactive()
    #   # glimpse(data)
    #   selected_year <- 2023
    #   selected_region <- 'Peninsular'
    #   
    #   filtered_data <- prep_data(data, selected_year, selected_region)
    #   # glimpse(filtered_data)
    #   
    #   # Run clustering algorithm
    #   clust_result <- run_clust(filtered_data, meth = input$clustGeo_method, n_clusters = input$nClust)
    #   
    #   # Render map
    #   output$clustGeoMap <- renderTmap({
    #     tmap_mode('plot')
    #     map <- qtm(clust_result, "CLUSTER") +
    #       tm_borders(alpha = 0.5) +
    #       tm_view(set.zoom.limits = c(6, 7))
    #     map
    #   })
    #   
    #   # Render choicealpha graph
    #   output$choicealpha <- renderPlotly({
    #     # Assuming `choicealpha` returns a plot object, adapt this to match your function
    #     choicealpha_graph <- choicealpha(filtered_data)
    #     ggplotly(choicealpha_graph)
    #   })
    #   print('---------- end ----------')
    # })
    
    observeEvent(input$run_clustGeo, {
      req(input$nClust, input$clustGeo_method, input$clustGeo_region)
      data <- clustGeo_dataset_reactive()
      

      # Filter dataset by region and year
      filtered_data <- prep_data(data, yr = 2023, reg = input$clustGeo_region)
      # glimpse(filtered_data)

      # Run clustering algorithm
      clust_result <- run_clust(filtered_data, meth = input$clustGeo_method, n_clusters = input$nClust)
      
      # Extract individual results from the list
      ngeo_cluster <- clust_result$ngeo_cluster
      sf_Gcluster <- clust_result$sf_Gcluster
      cr <- clust_result$cr

      print(clust_result)
      # Render map
      output$clustGeoMap <- renderTmap({
        tmap_mode('view')
        map <- qtm(sf_Gcluster, "CLUSTER") +
          tm_borders(alpha = 0.5) +
          tm_view(set.zoom.limits = c(6, 7))
        map
      })

      # Render choicealpha graph
      output$choicealpha <- renderPlotly({
        # Assuming `choicealpha` returns a plot object, adapt this to match your function
        choicealpha_graph <- cr
        ggplotly(choicealpha_graph)
      })
    })
    
#-----------------------------------------------------------------------------------------------------------
    
    # SKATER
    skater_dataset_reactive <- reactive({
        req(input$skater_dataset)
        data <- switch(input$skater_dataset,
            "crime_merged_sf" = crime_merged_sf)
        data
    })

    # Dyanmic methods - SKATER
    observe({
        dist_methods <- list("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "mahalanobis")
        updateSelectInput(session, 'skater_method',
            choices = dist_methods,
            selected = dist_methods[1])
    })
    
    source('skater_helper.R')
    
    observeEvent(input$run_skater,{
        print('---------- start ----------')
        data <- skater_dataset_reactive()
        # glimpse(data)
        selected_year <- 2023
        selected_region <- 'Peninsular'

        filtered_data <- prep_data(data, selected_year, selected_region)
        # glimpse(filtered_data)
        skater_clust <- run_skater(filtered_data, input$skater_method, input$n_clusters)
        # glimpse(skater_clust)
        output$skater_plot <- renderTmap({
            tmap_mode('plot')
            map <- qtm(skater_clust, "Skater_CLUSTER") +
                tm_borders(alpha = 0.5) +
                tm_view(set.zoom.limits = c(6, 7))
            map
        })
        print('---------- end ----------')
    })
    
}