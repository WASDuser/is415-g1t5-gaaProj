source("descriptions.R")
source('skater_helper.R')
source('clustgeo_helper.R')

function(input, output, session) {
    # ------------------------------ DATA ------------------------------
    
    # Preview the dataset with up to 20 rows (when "Preview" is selected)
    output$data_table <- DT::renderDataTable({
        req(input$display_option == "preview")
        
        load_data()
        
        data_display_options <- list( # default display
            dom = 'Bfrtip',
            buttons = c('copy', 'excel', 'pdf'),
            pageLength = 20,
            scrollX = TRUE,
            scrollY = '333px',
            fixedHeader = TRUE)
        
        DT::datatable(
            crime_merged_sf, 
            options=data_display_options, 
            extensions = 'Buttons',
            rownames = FALSE)
    })
    
    
    # Render code output for str() or glimpse()
    output$code_output <- renderPrint({
        load_data()
        
        if (input$display_option == "str") {
            str(crime_merged_sf)
        } else if (input$display_option == "glimpse") {
            glimpse(crime_merged_sf)
        }
    })
    
    
    # description based on the dataset
    output$data_desc <- renderText({
      description_text <- descriptions[['data_desc']]
      HTML(description_text)
    })
    
    # ------------------------------ END OF DATA ------------------------------
    
    # ------------------------------ EDA ------------------------------
    # Update variable choices based on selected dataset
    observe({
        load_data()
        
        # Get numeric variable names
        numeric_vars <- names(crime_merged_sf)[sapply(crime_merged_sf, is.numeric)]
        
        updateSelectInput(session, "variable", 
            choices = numeric_vars,
            selected = NULL) # Reset selection
    })
    
    
    # Dynamic UI for variable selection based on continuous/categorical
    output$var_select <- renderUI({
        load_data()
        if (input$var_type == "cat") {
            selectInput("variable", "Choose Categorical Variable:", 
                choices = names(crime_merged_sf)[sapply(crime_merged_sf, is.character)])
        } else {
            selectInput("variable", "Choose Continuous Variable:", 
                choices = names(crime_merged_sf)[sapply(crime_merged_sf, is.numeric)])
        }
    })
    
    
    # Display summary statistics
    output$summary_table <- renderPrint({
        req(input$submit_eda)    # Ensure that the submit button has been clicked
        req(input$variable)       # Ensure that a variable has been selected
        
        load_data()
        variable <- crime_merged_sf[[input$variable]] # Get the selected variable
        summary_output <- summary(as.data.frame(variable)) # Get summary statistics
        colnames(summary_output) <- input$variable # Set the selected variable as column name
        
        summary_output
    })
    
    
    # Dynamic chart type options
    observeEvent(input$var_type, {
        updateSelectInput(session, "chart_type", choices = if (input$var_type == "cat") {
            c("Bar Chart")
        } else {
            c("Histogram", "Boxplot")
        })
    })
    
    
    # Render EDA plot/statistics upon submit button click 
    observeEvent(input$submit_eda, {
        req(input$submit_eda)
        output$eda_plot <- renderPlot({
            load_data()
            
            color <- "#428bca"
            
            if (input$chart_type == "Bar Chart") {
                ggplot(crime_merged_sf, aes(x = .data[[input$variable]])) + geom_bar(fill=color) + 
                    theme(axis.text.x = element_text(angle = 30))
                
            } else if (input$chart_type == "Histogram") {
                ggplot(crime_merged_sf, aes(x = .data[[input$variable]])) + 
                    geom_histogram(binwidth = 5, color="black", fill=color)
                
            } else if (input$chart_type == "Boxplot") {
                ggplot(crime_merged_sf, aes(y = .data[[input$variable]])) + geom_boxplot(fill=color)
                
            }
        })})
    
    
    # description
    output$eda_desc <- renderText({
      description_text <- descriptions[['eda_desc']]
      HTML(description_text)
    })
    
    # ------------------------------ END OF EDA ------------------------------
    
    # ------------------------------ CHORO ------------------------------
    
    # Dyanmic time period options - choro
    observe({
      load_data()
      years <- unique(year(crime_merged_sf$date.y))
      
      updateSelectInput(session, 'time_period',
        choices = years,
        selected = years[1])
    })
    
    # Choloropleth output
    choropleth_map <- eventReactive(input$submit_esda,{
      
      load_data()
      
      req(input$esda_variable)
      req(input$crime_type)
      req(input$region)
      req(input$classification)
      req(input$n_classes)
      req(input$colors)
      
      chosen_var <- input$esda_variable
      chosen_year <- input$time_period
      chosen_crime <- input$crime_type
      chosen_region <- input$region
      chosen_class <- input$classification
      chosen_n <- input$n_classes
      chosen_color <- input$colors
      
      crime_merged_sf <- crime_merged_sf %>% ungroup() %>% st_as_sf() %>%
        filter(region == chosen_region) %>% 
        filter(year == chosen_year) %>% 
        filter(type == chosen_crime)
      
      tmap_mode('view')
      tmap_options(check.and.fix = TRUE)
      tm_shape(crime_merged_sf) +
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
    
    
    # Choropleth desc
    output$choropleth_desc <- renderText({
      description_text <- descriptions[['choropleth_desc']]
      HTML(description_text)
    })
    
    
    # Dynamic crime type options
    observe({
        load_data()
        crime_types <- unique(crime_merged_sf$type)
        
        updateSelectInput(session, "crime_type",
            choices = crime_types,
            selected = crime_types[1])
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
    
    
    # Dynamic region options
    observe({
      load_data()
      regions <- unique(crime_merged_sf$region)
      
      updateSelectInput(session, "region",
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
    # ------------------------------ END OF CHORO ------------------------------
    
    # ------------------------------ GLOBAL PARAMS ------------------------------
    # Dynamic crime type options - global
    observe({
      load_data()
      crime_types <- unique(crime_merged_sf$type)
      
      updateSelectInput(session, "global_crime_type",
        choices = crime_types,
        selected = crime_types[1])
    })
    
    # Dynamic region options - global
    observe({
      load_data()
      regions <- unique(crime_merged_sf$region)
      
      updateSelectInput(session, "global_region",
        choices = regions,
        selected = regions[1])
    })
    
    # Dyanmic time period options - global
    observe({
      load_data()
      years <- unique(year(crime_merged_sf$date.y))
      
      updateSelectInput(session, 'global_time_period',
        choices = years,
        selected = years[1])
    })
    # ------------------------------ END OF GLOBAL PARAMS ------------------------------
    
    # ------------------------------ END OF GLOBAL OUTPUT ------------------------------
    # Global Moran I - table
    globalMIResults <- eventReactive(input$submit_global,{
        load_data()
      
        chosen_crime <- input$global_crime_type
        chosen_region <- input$global_region
        chosen_year <- input$global_time_period
        
        crime_merged_sf <- crime_merged_sf %>% ungroup() %>% st_as_sf() %>% 
            filter(region == chosen_region) %>% 
            filter(type == chosen_crime) %>% 
            filter(!is.na(crime_rate))
        
        nb <- st_contiguity(crime_merged_sf$geometry, queen = as.logical(input$global_contiguity))
        
        # credit to santhya
        # this is to langkawi, which is not connected to the others by admin boundary
        nb[17]<- as.integer(19)

        wm_q <- crime_merged_sf %>%
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
    
    # global desc
    output$global_desc <- renderText({
      description_text <- descriptions[['global_desc']]
      HTML(description_text)
    })
    # ------------------------------ END OF GLOBAL OUTPUT ------------------------------
    
    # ------------------------------ LOCAL PARAMS ------------------------------
    # Dynamic crime type options - local
    observe({
      load_data()
      crime_types <- unique(crime_merged_sf$type)
      
      updateSelectInput(session, "local_crime_type",
        choices = crime_types,
        selected = crime_types[1])
    })
    
    # Dynamic region options - local
    observe({
      load_data()
      regions <- unique(crime_merged_sf$region)
      
      updateSelectInput(session, "local_region",
        choices = regions,
        selected = regions[1])
    })
    
    # Dyanmic time period options - local
    observe({
      load_data()
      years <- unique(year(crime_merged_sf$date.y))
      
      updateSelectInput(session, 'local_time_period',
        choices = years,
        selected = years[1])
    })
    # ------------------------------ END OF LOCAL PARAMS ------------------------------
    
    # ------------------------------ LOCAL OUTPUT ------------------------------
    # LISA
    localMIResults <- eventReactive(input$MoranUpdate,{
        load_data()
        
        chosen_crime <- input$local_crime_type
        chosen_region <- input$local_region
        chosen_year <- input$local_time_period
        
        crime_merged_sf <- crime_merged_sf %>% ungroup() %>% st_as_sf() %>% 
            filter(region == chosen_region) %>% 
            filter(type == chosen_crime) %>% 
            filter(!is.na(crime_rate))

        crime_merged_sf <- crime_merged_sf %>% filter(year(crime_merged_sf$date.y) == chosen_year)
        
        nb <- st_contiguity(crime_merged_sf$geometry, queen = as.logical(input$Contiguity))
        
        nb[17]<- as.integer(19)
        
        wm_q <- crime_merged_sf %>%
            mutate(
                nb = nb,
                wt = st_weights(nb, style = input$MoranWeights))
        
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

        return(lisa)
    })
    
    # LISA - statistical
    output$LocalMoranMap <- renderTmap({
        df <- localMIResults()
        
        # debugging - if empty data
        # if(is.null(df) || nrow(df) == 0) return()
        
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
    
    # local desc
    output$local_desc <- renderText({
      description_text <- descriptions[['local_desc']]
      HTML(description_text)
    })
    # ------------------------------ END OF LOCAL OUTPUT ------------------------------
    
    # ------------------------------ HCLUST ------------------------------
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
      load_data()
      params <- hclust_params()
      
      rate_crime_prep <- rate_crime_prep %>% 
        filter(year == params$year) %>% 
        filter(region == params$region)
      
      rate_crime_prep <- as.data.frame(rate_crime_prep)
      row.names(rate_crime_prep) <- rate_crime_prep$district 
      
      rate_crime_prep <- rate_crime_prep %>% 
        select(-c(2))
      
      rate_crime_data.std <- normalize(rate_crime_prep)
      
      rate_crime_data_numeric <- rate_crime_prep %>% select(where(is.numeric))
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
    
    heatmap <- eventReactive(input$HclustPlots, {
      load_data()
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
      load_data()
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
    
    # hclust desc
    output$hclust_desc <- renderText({
      description_text <- descriptions[['hclust_desc']]
      HTML(description_text)
    })
    
    # ------------------------------ END OF HCLUST ------------------------------
    
    # ------------------------------ CLUSTGEO ------------------------------
    observe({
      dist_methods <- list("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "mahalanobis")
      updateSelectInput(session, 'clustGeo_method',
                        choices = dist_methods,
                        selected = dist_methods[1])
    })
    
    
    observe({
      load_data()
      years <- unique(year(crime_merged_sf$date.y))
      
      updateSelectInput(session, 'clustgeo_time_period',
        choices = years,
        selected = years[1])
    })
    
    
    observeEvent(input$run_clustGeo, {
      req(input$nClust, input$clustGeo_method, input$clustGeo_region)
      
      load_data()

      # Filter dataset by region and year
      filtered_data <- prep_data(data=crime_merged_sf, yr = input$clustgeo_time_period, reg = input$clustGeo_region)
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
      
      print(str(cr))

      output$choicealpha <- renderPlotly({
        # Extract relevant data from the 'choicealpha' object
        cr_data <- cr$desired_component  # Replace with the actual extraction logic
        
        # Check if the extracted data has the correct format
        if ("variable" %in% names(cr_data) && "value" %in% names(cr_data)) {
          cr_df <- as.data.frame(cr_data)  # Ensure it's a data frame
          
          # Create a ggplot object
          p <- ggplot(cr_df, aes(x = variable, y = value)) + 
            geom_bar(stat = "identity")
          
          # Convert to Plotly and return
          ggplotly(p)
        } else {
          print("cr does not contain 'variable' and 'value' columns")
        }
      })
      
    })
    
    # ------------------------------ END OF CLUSTGEO ------------------------------
    
    # ------------------------------ SKATER ------------------------------
    
    # Dyanmic time period options - skater
    observe({
      load_data()
      years <- unique(year(crime_merged_sf$date.y))
      
      updateSelectInput(session, 'skater_time_period',
        choices = years,
        selected = years[1])
    })
    
    # Dynamic region options - skater
    observe({
      load_data()
      regions <- unique(crime_merged_sf$region)
      
      updateSelectInput(session, "skater_region",
        choices = regions,
        selected = regions[1])
    })

    # Dyanmic methods - SKATER
    observe({
        dist_methods <- list("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "mahalanobis")
        updateSelectInput(session, 'skater_method',
            choices = dist_methods,
            selected = dist_methods[1])
    })
    
    observeEvent(input$run_skater,{
        print('---------- start ----------')
        # data <- crime_merged_sf
      
        load_data()
        
        # glimpse(data)
        selected_year <- input$skater_time_period
        selected_region <- input$skater_region

        filtered_data <- prep_data(data=crime_merged_sf, selected_year, selected_region)
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
    
    # skater desc
    output$skater_desc <- renderText({
      description_text <- descriptions[['skater_desc']]
      HTML(description_text)
    })
    # ------------------------------ END OF SKATER ------------------------------
}