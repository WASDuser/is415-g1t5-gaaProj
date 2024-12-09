navbarPage(
    "GeoPEACE",
    id = 'navbarID',
    fluid = TRUE,
    theme = shinytheme('sandstone'),
    collapsible = TRUE,
    
    tabPanel('Data',
        sidebarLayout(
            sidebarPanel(
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
                    selectInput("time_period", "Select Year:",
                                choices = list("2016" = "2016",
                                               "2017" = "2017",
                                               "2018" = "2018",
                                               "2019" = "2019",
                                               "2020" = "2020",
                                               "2021" = "2021",
                                               "2022" = "2022",
                                               "2023" = "2023"),
                                selected = "2020"),
                    
                    selectInput("esda_variable", "Select Variable for Choropleth:",
                        choices = list(
                            "crimes"="crimes",
                            "crime_rate"="crime_rate"
                        )),
                    
                    selectInput("crime_type", "Select Crime Type:",
                                choices = list("Causing Injury" = "causing_injury",
                                               "Murder" = "murder",
                                               "Rape" = "rape",
                                               "Armed Gang Robber" = "robbery_gang_armed",
                                               "Unarmed Gang Robbery" = "robbery_gang_unarmed",
                                               "Armed Solo Robbery" = "robbery_solo_armed",
                                               "Unarmed Solo Robbery" = "robbery_solo_unarmed"),
                                selected = "causing_injury"),
                    
                    selectInput("region", "Select Region:",
                                choices = list("Peninsular" = "Peninsular",
                                               "East" = "East"),
                                selected = "Peninsular"),
                    
                    selectInput("classification", "Select Classification option:",
                                choices = list("sd" = "sd", 
                                               "Equal" = "equal", 
                                               "Pretty" = "pretty", 
                                               "Quantile" = "quantile", 
                                               "K-means" = "kmeans", 
                                               "Hclust" = "hclust", 
                                               "Bclust" = "bclust", 
                                               "Fisher" = "fisher", 
                                               "Jenks" = "jenks"),
                                selected = "pretty"),
                    
                    tags$small("*log methods do not work if there are NULL values"),
                    tags$br(),
                    tags$small("*continuous: cont,order,log"),
                    tags$br(),
                    tags$small("*categorical: the rest"),
                    tags$br(),tags$br(),
                    
                    sliderInput("n_classes", "Number of classes",
                        min = 3, max = 10, value = c(5)),
                    
                    selectInput("colors", "Color Palette:",
                                choices = list("Blues" = "Blues", 
                                               "Reds" = "Reds", 
                                               "Greens" = "Greens",
                                               "Yellow-Orange-Red" = "YlOrRd",
                                               "Yellow-Orange-Brown" = "YlOrBr",
                                               "Yellow-Green" = "YlGn",
                                               "Orange-Red" = "OrRd",
                                               "Purples" = "Purples"),
                                selected = "Blues"),
                ),
                
                
                conditionalPanel(
                    condition="input.tabs=='globalTab'", h3("Global settings"),
                    
                    actionButton("submit_global", "Generate Plot"), hr(),
                    
                    selectInput("global_time_period", "Select year:",
                                choices = list("2016" = "2016",
                                               "2017" = "2017",
                                               "2018" = "2018",
                                               "2019" = "2019",
                                               "2020" = "2020",
                                               "2021" = "2021",
                                               "2022" = "2022",
                                               "2023" = "2023"),
                                selected = "2020"),
                    
                    selectInput("global_crime_type", "Select Crime Type:",
                                choices = list("Causing Injury" = "causing_injury",
                                               "Murder" = "murder",
                                               "Rape" = "rape",
                                               "Armed Gang Robber" = "robbery_gang_armed",
                                               "Unarmed Gang Robbery" = "robbery_gang_unarmed",
                                               "Armed Solo Robbery" = "robbery_solo_armed",
                                               "Unarmed Solo Robbery" = "robbery_solo_unarmed"),
                                selected = "causing_injury"),
                    
                    selectInput("global_region", "Select Region:",
                                choices = list("Peninsular" = "Peninsular",
                                               "East" = "East"),
                                selected = "Peninsular"),
                    
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
                    
                    selectInput("local_time_period", "Select year:",
                                choices = list("2016" = "2016",
                                               "2017" = "2017",
                                               "2018" = "2018",
                                               "2019" = "2019",
                                               "2020" = "2020",
                                               "2021" = "2021",
                                               "2022" = "2022",
                                               "2023" = "2023"),
                                selected = "2020"),
                    
                    selectInput("local_crime_type", "Select Crime Type:",
                                choices = list("Causing Injury" = "causing_injury",
                                               "Murder" = "murder",
                                               "Rape" = "rape",
                                               "Armed Gang Robber" = "robbery_gang_armed",
                                               "Unarmed Gang Robbery" = "robbery_gang_unarmed",
                                               "Armed Solo Robbery" = "robbery_solo_armed",
                                               "Unarmed Solo Robbery" = "robbery_solo_unarmed"),
                                selected = "causing_injury"),
                    
                    selectInput("local_region", "Select Region:",
                                choices = list("Peninsular" = "Peninsular",
                                               "East" = "East"),
                                selected = "Peninsular"),
                    
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
                        conditionalPanel(
                            condition = "input.submit_global",
                            plotOutput("global_hist"),
                            DTOutput("global_output"),
                            tags$br(),
                        ),
                        uiOutput("global_desc"),
                    ),
                    tabPanel("Local", value = 'localTab',
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
                
                selectInput(inputId = "clustGeo_region",
                            label = "Select Region",
                            choices = list("Peninsular" = "Peninsular",
                                           "East" = "East"),
                            selected = "Peninsular"),
                  
                selectInput("clustgeo_time_period", "Select year:",
                            choices = list("2016" = "2016",
                                           "2017" = "2017",
                                           "2018" = "2018",
                                           "2019" = "2019",
                                           "2020" = "2020",
                                           "2021" = "2021",
                                           "2022" = "2022",
                                           "2023" = "2023"),
                            selected = "2020"),
                
                selectInput("clustGeo_method", "Select Distance Method:",
                            
                            choices = list("Euclidean" = "euclidean",
                                           "Maximum" = "maximum",
                                           "Manhattan" = "manhattan",
                                           "Canberra" = "canberra",
                                           "Binary" = "binary",
                                           "Minkowski" = "minkowski"),
                            selected = "euclidean"),
                
                sliderInput(inputId = "nClust", 
                            label = "Number of Clusters", 
                            min = 3, max = 15,
                            value = 6, step = 1),
                
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
                ),
                  uiOutput("clustgeo_desc"),
              )
            )),
        
        tabPanel('SKATER',
            sidebarLayout(
                sidebarPanel(
                    selectInput("skater_region", "Select Region:",
                                choices = list("Peninsular" = "Peninsular",
                                               "East" = "East"),
                                selected = "Peninsular"),
                    
                    selectInput("skater_time_period", "Select year:",
                                choices = list("2016" = "2016",
                                               "2017" = "2017",
                                               "2018" = "2018",
                                               "2019" = "2019",
                                               "2020" = "2020",
                                               "2021" = "2021",
                                               "2022" = "2022",
                                               "2023" = "2023"),
                                selected = "2020"),
                    
                    selectInput("skater_method", "Select Distance Method:",
                                choices = list("Euclidean" = "euclidean",
                                               "Maximum" = "maximum",
                                               "Manhattan" = "manhattan",
                                               "Canberra" = "canberra",
                                               "Binary" = "binary",
                                               "Minkowski" = "minkowski"),
                                selected = "euclidean"),
                    
                    numericInput("n_clusters", "Number of Clusters (k):", value = 6, min = 2, max = 10),
                    
                    actionButton("run_skater", "Run SKATER")
                ),
                
                mainPanel(
                    tmapOutput("skater_plot"),
                    uiOutput("skater_desc"),
                )
            ))
        ))