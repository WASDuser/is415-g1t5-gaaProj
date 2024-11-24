navbarPage(
    "GeoPEACE",
    id = 'navbarID',
    fluid = TRUE,
    theme = shinytheme('sandstone'),
    collapsible = TRUE,
    
    tabPanel('Data',
        sidebarLayout(
            sidebarPanel(
                # selectInput("dataset", "Select Dataset:",
                #     choices = list(
                #         "crime_merged_sf" = "crime_merged_sf"
                #     )),
                
                
                radioButtons("display_option", "Select display option for crime_merged_sf):",
                    choices = list(
                        "Preview" = "preview",
                        "str()" = "str",
                        "glimpse()" = "glimpse"))
            ),
            mainPanel(
                conditionalPanel(
                    condition = "input.display_option == 'preview'",
                    DTOutput("data_table")
                ),
                conditionalPanel(
                    condition = "input.display_option != 'preview'",
                    verbatimTextOutput("code_output")
                ),
                tags$br(),
                uiOutput("data_desc"),
                tags$br(),
                tableOutput("summary_stats"),
            )
        )
    ),
    
    
    tabPanel('EDA',
        sidebarLayout(
            sidebarPanel(
                # selectInput("esda_dataset", "Select Dataset:",
                #     choices = list(
                #         "crime_merged_sf" = "crime_merged_sf"
                #     )),
                
                selectInput("var_type", "Select Variable Type:",
                    choices = list(
                        "Categorical" = "cat",
                        "Continuous" = "cont")),
                
                uiOutput("var_select"),
                
                selectInput("chart_type", "Select Chart Type:",
                    choices = NULL,
                    selected = NULL),
                
                actionButton("submit_eda", "Generate Chart")),
            
            mainPanel(
                conditionalPanel(
                    condition = "input.submit_eda > 0",
                    plotOutput("eda_plot")
                ),
                
                conditionalPanel(
                    condition = "input.submit_eda > 0 && input.var_type == 'cont'",
                    verbatimTextOutput("summary_table")
                ),
                tags$br(),
                uiOutput("eda_desc")
            )
        )
    ),
    # ),
    
    tabPanel('ESDA',
        sidebarLayout(
            sidebarPanel(
                conditionalPanel(
                    condition="input.tabs=='choroTab'", h3("Choropleth settings"),
                    
                    actionButton("submit_esda", "Generate Choropleth"),hr(),
                    
                    # selectInput("esda_dataset", "Select Dataset:", 
                    #     choices = list(
                    #         "crime_merged_sf" = "crime_merged_sf"
                    #     )),
                    selectInput("time_period", "Select Year:",
                        choices = NULL,
                        selected = NULL),
                    
                    selectInput("esda_variable", "Select Variable for Choropleth:",
                        choices = list(
                            "crimes"="crimes",
                            "crime_rate"="crime_rate"
                        )),
                    
                    selectInput("crime_type", "Select Crime Type:",
                        choices = NULL,
                        selected = NULL),
                    
                    selectInput("region", "Select Region:",
                        choices = NULL,
                        selected = NULL),
                    
                    selectInput("classification", "Select Classification option:",
                        choices = NULL,
                        selected = NULL),
                    
                    tags$small("*log methods do not work if there are NULL values"),
                    tags$br(),
                    tags$small("*continuous: cont,order,log"),
                    tags$br(),
                    tags$small("*categorical: the rest"),
                    tags$br(),tags$br(),
                    
                    sliderInput("n_classes", "Number of classes",
                        min = 3, max = 10, value = c(5)),
                    
                    selectInput("colors", "Color Palette:",
                        choices = NULL,
                        selected = NULL),
                ),
                
                
                conditionalPanel(
                    condition="input.tabs=='globalTab'", h3("Global settings"),
                    
                    actionButton("submit_global", "Generate Plot"), hr(),
                    
                    # tags$small("*only west works for now*"),
                    # selectInput("esda_dataset", "Select Dataset:",
                    #     choices = list(
                    #         "crime_merged_sf" = "crime_merged_sf"
                    #     )),
                    
                    selectInput("global_time_period", "Select year:",
                        choices = NULL,
                        selected = NULL),
                    
                    selectInput("global_crime_type", "Select Crime Type:",
                        choices = NULL,
                        selected = NULL),
                    
                    selectInput("global_region", "Select Region:",
                        choices = NULL,
                        selected = NULL),
                    
                    radioButtons("global_contiguity", "Contiguity Method:",
                        choices = c(
                            "Queen" = TRUE,
                            "Rook" = FALSE),
                        selected = "TRUE",
                        inline = TRUE),
                    
                    selectInput("global_MoranWeights", "Spatial Weights Style",
                        choices = c(
                            "W: Row standardised" = "W",
                            "B: Binary" = "B",
                            "C: Globally standardised" = "C",
                            "U: C / no of neighbours" = "U",
                            "minmax" = "minmax",
                            "S: Variance" = "S"),
                        selected = "W"),
                    
                    sliderInput("global_MoranSims", "Number of Simulations:",
                        min = 49, max = 999, value = 99, step = 100),
                ),
                
                
                conditionalPanel(
                    condition="input.tabs=='localTab'", h3("Local settings"),
                    
                    actionButton("MoranUpdate", "Update Plot"),hr(),
                    
                    # tags$small("*only west works for now*"),
                    # selectInput("esda_dataset", "Select Dataset:", 
                    #     choices = list(
                    #         "crime_merged_sf" = "crime_merged_sf"
                    #     )),
                    
                    selectInput("local_time_period", "Select year:",
                        choices = NULL,
                        selected = NULL),
                    
                    selectInput("local_crime_type", "Select Crime Type:",
                        choices = NULL,
                        selected = NULL),
                    
                    selectInput("local_region", "Select Region:",
                        choices = NULL,
                        selected = NULL),
                    
                    radioButtons("Contiguity", "Contiguity Method:",
                        choices = c(
                            "Queen" = TRUE, 
                            "Rook" = FALSE),
                        selected = "TRUE",
                        inline = TRUE),
                    
                    selectInput("MoranWeights", "Spatial Weights Style",
                        choices = c(
                            "W: Row standardised" = "W",
                            "B: Binary" = "B",
                            "C: Globally standardised" = "C",
                            "U: C / no of neighbours" = "U",
                            "minmax" = "minmax",
                            "S: Variance" = "S"),
                        selected = "W"),
                    
                    sliderInput("MoranSims", "Number of Simulations:", 
                        min = 49, max = 999, value = 99, step = 100),
                    
                    hr(),
                    
                    radioButtons("MoranConf", "Select Confidence level",
                        choices = c(
                            "0.95" = 0.05, 
                            "0.99" = 0.01),
                        selected = 0.05,
                        inline = TRUE),
                    
                    selectInput("LisaClass", "Select Lisa Classification",
                        choices = c(
                            "mean" = "mean",
                            "median" = "median",
                            "pysal" = "pysal"),
                        selected = "mean"),
                    
                    selectInput("localmoranstats", "Select Local Moran's Stat:",
                        choices = c(
                            "local moran(ii)" = "local moran(ii)",
                            "expectation(eii)" = "expectation(eii)",
                            "variance(var_ii)" = "variance(var_ii)",
                            "std deviation(z_ii)" = "std deviation(z_ii)",
                            "P-value" = "p_value"),
                        selected = "local moran(ii)")
                ),
                
                
            ),
            mainPanel(
                tabsetPanel(id='tabs',
                    
                    tabPanel("Crime Rate Choropleth", value = 'choroTab',
                        conditionalPanel(
                            condition = "input.submit_esda",
                            tmapOutput("choro_map"),
                            tags$br(),
                        ),
                        uiOutput("choropleth_desc"),
                    ),
                    tabPanel("Global", value = 'globalTab',
                        # Placeholder content for the second map
                        conditionalPanel(
                            condition = "input.submit_global",
                            plotOutput("global_hist"),
                            DTOutput("global_output"),  # Placeholder for future map
                            tags$br(),
                        ),
                        uiOutput("global_desc"),
                    ),
                    tabPanel("Local", value = 'localTab',
                        # Placeholder content for the third map
                        conditionalPanel(
                            condition = "input.MoranUpdate",
                            tmapOutput("LocalMoranMap"),
                            tmapOutput("LISA"),
                            tags$br(),
                        ),
                        uiOutput("local_desc"),
                    ),
                )
            )
        )
    ),
    
    navbarMenu('Clustering',
               tabPanel('hclust',
                        sidebarLayout(
                          
                          
                          sidebarPanel(
                            
                            
                            selectInput(inputId = "region4",
                                        label = "Region",
                                        choices = list("Peninsular" = "Peninsular",
                                                       "East" = "East"),
                                        selected = "Peninsular"),
                            
                            
                            selectInput(inputId = "year4",
                                        label = "Year",
                                        choices = list("2016" = "2016",
                                                       "2017" = "2017",
                                                       "2018" = "2018",
                                                       "2019" = "2019",
                                                       "2020" = "2020",
                                                       "2021" = "2021",
                                                       "2022" = "2022",
                                                       "2023" = "2023"),
                                        selected = "2020"),
                            
                            
                            selectInput(inputId = "proxMethod",
                                        label = "Proximity Method",
                                        choices = list("Euclidean" = "euclidean",
                                                       "Maximum" = "maximum",
                                                       "Manhattan" = "manhattan",
                                                       "Canberra" = "canberra",
                                                       "Binary" = "binary",
                                                       "Minkowski" = "minkowski"),
                                        selected = "euclidean"),
                            
                            selectInput(inputId = "hclustMethod",
                                        label = "Hclust Method",
                                        choices = list(
                                          "Ward's Minimum Variance (ward.D)" = "ward.D",
                                          "Ward's Minimum Variance (ward.D2)" = "ward.D2",
                                          "Single Linkage (single)" = "single",
                                          "Complete Linkage (complete)" = "complete",
                                          "Average Linkage (average)" = "average",
                                          "McQuitty's Method (mcquitty)" = "mcquitty",
                                          "Median Linkage (median)" = "median",
                                          "Centroid Linkage (centroid)" = "centroid"
                                        ),
                                        selected = "ward.D"),
                            
                            sliderInput(inputId = "optimalClust", 
                                        label = "Number of Clusters", 
                                        min = 3, max = 15,
                                        value = 6, step = 1),
                            
                            
                            actionButton("HclustPlots", "Generate Plots"),
                            hr(),
                            
                            
                          ),
                          
                          
                          mainPanel(
                            
                            tabsetPanel(id='tabs2',
                                        
                                        tabPanel("Hclust Dendogram", value = 'dendoTab',
                                                 conditionalPanel(
                                                   condition = "input.HclustPlots",
                                                   plotOutput("dendogram"),
                                                 )
                                        ),
                                        
                                        tabPanel("Heatmap", value = 'heatmapTab',
                                                 conditionalPanel(
                                                   condition = "input.HclustPlots",
                                                   plotlyOutput("heatmap"),
                                                 )
                                                 
                                        ),
                                        
                                        tabPanel("Map", value = 'hcMapTab',
                                                 conditionalPanel(
                                                   condition = "input.HclustPlots",
                                                   tmapOutput("hclustMap"),
                                                 )
                                        ),
                            ),
                              uiOutput("hclust_desc"),
                          )
                          
                          
                        )),
               
        tabPanel('clustGEO',
            sidebarLayout(
              sidebarPanel(
                # Input: Select dataset
                #selectInput("clustGeo_dataset", "Select Dataset:",
                            #choices = list("crime_merged_sf" = "crime_merged_sf")),
                
                selectInput(inputId = "clustGeo_region",
                            label = "Select Region",
                            choices = list("Peninsular" = "Peninsular",
                                           "East" = "East"),
                            selected = "Peninsular"),
                  
                  selectInput("clustgeo_time_period", "Select year:",
                      choices = NULL,
                      selected = NULL),
                
                # Input: Select Distance Method
                selectInput("clustGeo_method", "Select Distance Method:",
                            choices = NULL,
                            selected = NULL),
                
                sliderInput(inputId = "nClust", 
                            label = "Number of Clusters", 
                            min = 3, max = 15,
                            value = 6, step = 1),
                
                # Button to trigger clustGeo clustering
                actionButton("run_clustGeo", "Run clustGeo")
              ),
              
              mainPanel(
                tabsetPanel(id='clustGeoTab',
                            tabPanel("clustGeo Map", value = 'clustGeoMapTab',
                                     tmapOutput("clustGeoMap")
                            ),
                            tabPanel("ChoiceAlpha Graph", value = 'choicealphaTab',
                                     plotlyOutput("choicealpha")
                            )
                )
              )
            )),
        tabPanel('SKATER',
            sidebarLayout(
                sidebarPanel(
                    # Input: Select dataset
                    # selectInput("skater_dataset", "Select Dataset:",
                    #     choices = list("crime_merged_sf" = "crime_merged_sf")),
                    selectInput("skater_region", "Select Region:",
                        choices = NULL,
                        selected = NULL),
                    
                    selectInput("skater_time_period", "Select year:",
                        choices = NULL,
                        selected = NULL),
                    
                    # Input: Select Distance Method
                    selectInput("skater_method", "Select Distance Method:",
                        choices = NULL,
                        selected = NULL),
                    
                    # Input: Number of clusters for SKATER
                    numericInput("n_clusters", "Number of Clusters (k):", value = 6, min = 2, max = 10),
                    
                    # Button to trigger SKATER clustering
                    actionButton("run_skater", "Run SKATER")
                ),
                
                mainPanel(
                    # Output: Plot for SKATER results
                    tmapOutput("skater_plot"),
                    uiOutput("skater_desc"),
                )
            ))
        )
    )