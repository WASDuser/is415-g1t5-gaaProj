descriptions <- list(
  data_desc = 
  "
  <b>Dataset Preview Description: \"Don't Do Crime\"</b><br><br>
  This dataset provides a detailed account of crime incidents in the district of Batu Pahat, located in the state of Johor, Malaysia. The data captures spatial and temporal details of various crimes over multiple years. Below is a breakdown of the key columns and their meanings:<br><br>
  <b>Key Columns:</b><br>
  <ul>
    <li><b>district</b>: The geographical district where the crime was recorded (e.g., Batu Pahat).</li>
    <li><b>state</b>: The state to which the district belongs (e.g., Johor).</li>
    <li><b>Shape_Leng</b>: A numeric value representing the perimeter length of the district's geographical shape (in an appropriate spatial unit).</li>
    <li><b>Shape_Area</b>: A numeric value representing the area of the district's geographical shape.</li>
    <li><b>type</b>: The category of the crime (e.g., 'causing_injury,' 'murder,' 'rape').</li>
    <li><b>date.y</b>: The date when the crime data was recorded (e.g., '2016-01-01').</li>
    <li><b>year</b>: The year corresponding to the crime record (e.g., 2016).</li>
    <li><b>crimes</b>: The total number of reported crimes for the given type, district, and year.</li>
    <li><b>crime_rate</b>: The crime rate calculated based on the population of the district. This provides a standardized measure for comparison.</li>
    <li><b>category</b>: A broader classification of the crime type (e.g., 'assault').</li>
    <li><b>region</b>: Indicates whether the data belongs to Peninsular Malaysia or East Malaysia (e.g., 'Peninsular').</li>
    <li><b>geometry</b>: Spatial geometry object representing the geographical boundaries of the district. This is used for mapping and spatial analysis.</li>
  </ul>
  <br><b>Dataset Features:</b><br>
  <ul>
    <li>The data spans multiple years (2016–2023), allowing for temporal trends and comparisons.</li>
    <li>Crimes are categorized into types such as <i>causing injury, murder, and rape</i>, providing a diverse view of crime types.</li>
    <li>Includes <b>spatial attributes</b> (e.g., Shape_Leng, Shape_Area, and geometry) for advanced geospatial analysis.</li>
    <li><b>Standardized crime rates</b> enable comparisons across districts and years, accounting for population differences.</li>
    <li>Focused on <b>Batu Pahat</b>, providing localized insights into crime trends in a specific district of Johor.</li>
  </ul>
  <br><b>Potential Uses:</b><br>
  <ol>
    <li><b>Temporal Analysis</b>: Track how crime types and rates have changed over the years.</li>
    <li><b>Spatial Analysis</b>: Use geographical features to map and visualize crime distribution.</li>
    <li><b>Policy Insights</b>: Provide localized data for law enforcement and policymakers to allocate resources effectively.</li>
    <li><b>Clustering and Hotspot Detection</b>: Group areas with similar crime patterns for targeted interventions.</li>
  </ol>
  This dataset is ideal for exploring trends, analyzing relationships, and conducting geospatial analysis on crime in Batu Pahat, Peninsular Malaysia.
  ",
  eda_desc = "
    <b>Bar Chart</b>
    <br>A bar chart is a graphical representation of categorical data. Each category is represented by a bar whose length corresponds to its frequency or count. This is useful for comparing discrete groups or categories in your dataset.<br>
    <b>Boxplot</b>
    <br>A boxplot is a standardized way of displaying the distribution of data based on a five-number summary: minimum, first quartile, median, third quartile, and maximum. It highlights outliers and variability within a dataset.<br>
    <b>Histogram</b>
    <br>A histogram is a graphical representation of the distribution of numerical data. It groups data into bins or intervals, allowing for the visualization of frequency distributions and data spread.
  ",
  choropleth_desc = "",
  global_desc = "",
  local_desc = "*output does not show for years before 2020, and for the EAST region",
  hclust_desc = "*despite optimisation attempts, all 3 clustering tabs may cause application to crash due to memory usage",
  clustgeo_desc = "*despite optimisation attempts, all 3 clustering tabs may cause application to crash due to memory usage<br><br>*may take a couple minutes to load",
  skater_desc = "*despite optimisation attempts, all 3 clustering tabs may cause application to crash due to memory usage"
)
