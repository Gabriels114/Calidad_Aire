# ui.R
dashboardPage(
  dashboardHeader(title = "TroyaUAQ"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Informe", tabName = "informe", icon = icon("file-alt")),
      menuItem("Análisis", tabName = "analisis", icon = icon("chart-line")),
      menuItem("Base de Datos", tabName = "datos", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "informe",
              div(h1("Informe sobre la Calidad del Aire en la Facultad de Informática"),
                  style = "text-align: center;"),
              p(strong("Captura de datos mediante sensores")),
              p("Los datos de calidad del aire fueron capturados mediante diversos sensores, cada uno enfocado en medir distintos contaminantes en el ambiente interior. Los sensores utilizados fueron:"),
              tags$ul(
                tags$li("DHT11 - Temperatura y Humedad"),
                tags$li("CCS811 - CO2 y TVOC (Compuestos Orgánicos Volátiles Totales)"),
                tags$li("MQ-7 - Monóxido de Carbono (CO)"),
                tags$li("MQ-135 - Índice general de calidad del aire (AIRQ)")
              ),
              h3("Descripción general del problema"),
              p("Últimamente se ha estado registrando aire de mala calidad en Juriquilla. Es necesario determinar si la calidad del aire es adecuada para trabajar en el Centro de Desarrollo (CD)."),
              h3("Retos"),
              p("Los principales retos identificados en este proyecto incluyen:"),
              tags$ul(
                tags$li("Problemas con los sensores iniciales."),
                tags$li("Búsqueda de sensores de calidad y económicos."),
                tags$li("Coordinación y calibración de sensores."),
                tags$li("Ajustes estéticos en la visualización de los datos."),
                tags$li("Buscar librerías para crear dashboards interactivos y conectar los datos.")
              ),
              h3("Significados de correlaciones y respaldos"),
              p("Se realizó un análisis de correlaciones y ajustes de significancia en los datos, los cuales incluyen hallazgos sobre cómo ciertos contaminantes afectan la calidad cognitiva y el rendimiento de los individuos."),
              h3("Datos duros"),
              p("Los datos indican que las puntuaciones cognitivas mejoraron significativamente en entornos con menores concentraciones de CO2 y COV, además de buena ventilación. Otros hallazgos incluyen:"),
              tags$ul(
                tags$li("CO2 y COV se asociaron independientemente con las puntuaciones cognitivas."),
                tags$li("Disminución significativa en el rendimiento de la toma de decisiones entre 1000 y 2500 ppm de CO2, en comparación con 600 ppm."),
                tags$li("La temperatura afectó significativamente la precisión de las tareas, pero los niveles de TVOC no lo hicieron."),
                tags$li("Interacciones entre CO2, COV y otros parámetros como sonido, luz y temperatura también afectan la comodidad y productividad de los ocupantes."),
                tags$li("Un aumento en las concentraciones de PM2.5 y mayor concentración de CO2 se asoció con tiempos de respuesta más lentos y menor precisión en las pruebas cognitivas.")
              ),
              p("Según la OMS, más de 1.5 millones de muertes fueron causadas por contaminación de aire interior en el año 2000."),
              h3("Objetivo"),
              p(strong("¿Qué?"), " Detectar la concentración de contaminantes en el aire de diversas áreas de interiores de la Facultad de Informática."),
              p(strong("¿Para qué?"), " Identificar los componentes que estén afectando negativamente la salud de la comunidad estudiantil y la calidad de aprendizaje en la FIF."),
              p(strong("¿Cómo?"), " Recolección de concentración de componentes selectos en el Centro de Desarrollo y análisis en R."),
              h3("Contribución principal"),
              p("Este informe tiene como objetivo presentar un reporte sobre generalidades de la calidad del aire y los contaminantes presentes en los interiores de la FIF."),
              h3("Descripción de datos"),
              p("Los datos fueron obtenidos del aire interior del Centro de Desarrollo (CD), con un total de 4202 muestras, y se analizaron 6 variables."),
              h3("Medidas de asociación (Hallazgos)"),
              p("Se encontraron las siguientes asociaciones:"),
              tags$ul(
                tags$li("AIRQ está inversamente relacionado con la temperatura: AIRQ ∝ 1/Temp."),
                tags$li("El AIRQ mayor indica peor calidad del aire."),
                tags$li("El filtro de AC viejo parece afectar la calidad del aire.")
              ),
              h3("Conclusiones"),
              p("Las conclusiones principales del estudio son:"),
              tags$ul(
                tags$li("Temperatura dentro del rango ideal: 21-23°C. Mediana: 22.6°C."),
                tags$li("Humedad dentro del rango ideal: 40-60%. Mediana: 41%."),
                tags$li("CO2 dentro del rango ideal: < 800ppm. Mediana: 706ppm."),
                tags$li("TVOC dentro del rango ideal: < 0.25ppm. Mediana: 0.046ppm."),
                tags$li("CO dentro del rango ideal: 0-5ppm. Mediana: 0.33ppm."),
                tags$li("El Centro de Desarrollo tiene un buen aire interior."),
                tags$li("Correlación negativa entre temperatura y AIRQ. Es necesario cambiar el filtro del AC."),
                tags$li("Para mejorar la calidad del aire, se debe reducir más los niveles de CO2 y TVOC.")
              ),
              h3("Trabajos a futuro"),
              p("Se proponen los siguientes trabajos a futuro:"),
              tags$ul(
                tags$li("Establecer un clasificador para distinguir franjas horarias y concentración de gases nocivos."),
                tags$li("Comprobar efectividad de agrupación con variación cruzada."),
                tags$li("Desarrollar una interfaz visual para comunicar los resultados."),
                tags$li("Analizar salones individuales y otras aulas."),
                tags$li("Analizar la concentración de microorganismos en el aire."),
                tags$li("Aplicar análisis de aire de interiores en los laboratorios de la Facultad de Química."),
                tags$li("Desarrollar un sensor para analizar la calidad del aire en tiempo real y establecer alertas de calidad.")
              ),
              h3("Bibliografía"),
              tags$ul(
                tags$li("Allen JG, MacNaughton P, et al. (2016). 'Associations of cognitive function scores with carbon dioxide, ventilation, and volatile organic compound exposures in office workers.' Environmental Health Perspectives."),
                tags$li("Satish, U., et al. (2012). 'Is CO2 an indoor pollutant? Direct effects of low-to-moderate CO2 concentrations on human decision-making performance.' Environmental Health Perspectives."),
                tags$li("Zhao, Z., et al. (2025). 'The combined impacts of indoor temperature and total volatile organic compounds on cognitive performance of university students.' The Science of the Total Environment."),
                tags$li("Kaushik AK, et al. (2022). 'Effect of Indoor Environment on Occupant Air Comfort and Productivity in Office Buildings.' Sustainability."),
                tags$li("Harvard T.H. Chan School of Public Health (2024). 'Office air quality may affect employees’ cognition, productivity.' Harvard T.H. Chan School of Public Health."),
                tags$li("Mannan, M., & Al-Ghamdi, S. G. (2021). 'Indoor Air Quality in Buildings: A Comprehensive Review on the Factors Influencing Air Pollution in Residential and Commercial Structures.' International Journal of Environmental Research and Public Health.")
              ),
              h3("Contacto"),
              tabsetPanel(
                tabPanel("Fernando Cárdenas",
                         tags$ul(
                           tags$li("Email institucional: fcardenas04@alumnos.uaq.mx"),
                           tags$li("Email: fercard.go@gmail.com"),
                           tags$li("LinkedIn: ", a("fernando", href = "https://www.linkedin.com/in/fercardgo/"))
                         )
                ),
                tabPanel("Ana Ramírez",
                         tags$ul(
                           tags$li("Email institucional: aramirez285@alumnos.uaq.mx"),
                           tags$li("Email: anarramirez.ralo@gmail.com"),
                           tags$li("LinkedIn: ", a("Ana", href = "https://www.linkedin.com/in/ana-ramirez-lopez-6110b7296/"))
                         )
                ),
                tabPanel("Kay García",
                         tags$ul(
                           tags$li("Email institucional: kgarcia85@alumnos.uaq.mx"),
                           tags$li("Email: kayychez@gmail.com"),
                           tags$li("LinkedIn: ", a("Kay", href = "https://www.linkedin.com/in/kay-garcia-icad/"))
                         )
                ),
                tabPanel("Gabriel Gudiño",
                         tags$ul(
                           tags$li("Email institucional: jgudino27@alumnos.uaq.mx"),
                           tags$li("Email: gabriels114@gmail.com"),
                           tags$li("LinkedIn: ", a("Gabriel", href = "https://www.linkedin.com/in/gabriel-gudiño-lara/"))
                         )
                )
              )
      ),
      tabItem(tabName = "analisis",
              sidebarLayout(
                sidebarPanel(
                  actionButton("actualizar", "Actualizar Datos"),
                  dateRangeInput("fecha", "Seleccionar fechas:",
                                 start = Sys.Date() - 30,
                                 end = Sys.Date()),
                  selectInput("variable", "Variable:",
                              choices = c("Temp", "Hum", "CO2", "TVOC", "AIRQ", "CO")),
                  sliderInput("crit_val", "Valor crítico de correlación:",
                              min = 0, max = 1, value = 0.2, step = 0.01)
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Histogramas",
                             plotOutput("histograma"),
                             selectInput("estadistica", "Selecciona estadística:",
                                         choices = c("Ninguno", "Moda", "Mínimo", "Q1", "Q2", "Q3", "Máximo", "Rango", "Media", "Mediana"),
                                         selected = "Ninguno"),
                             numericInput("bins", "Número de bins:", min = 1, value = 30)
                    ),
                    tabPanel("Serie de Tiempo", plotOutput("serie_tiempo")),
                    tabPanel("Boxplot", plotOutput("boxplot")),
                    tabPanel("Dispersión",
                             plotOutput("dispersión"),
                             selectInput("yvar", "Variable Y:",
                                         choices = c("Temp", "Hum", "CO2", "TVOC", "AIRQ", "CO"), selected = "Temp"),
                             sliderInput("num_clusters", "Número de clusters:", min = 1, max = 10, value = 3)
                    ),
                    tabPanel("Matriz de Correlación", plotOutput("correlacion")),
                    tabPanel("Estadísticas", uiOutput("tabla")),
                    tabPanel("Matriz de Correlación", plotOutput("corr_plot")),
                    tabPanel("Matriz de Covarianza", plotOutput("cov_plot")),
                    tabPanel("Dependencias Significativas", plotOutput("dep_plot")),
                    tabPanel("Grafo de Asociación", plotOutput("graph_plot")),
                    tabPanel("MLE y EM Estimación", plotOutput("mle_em_plot"))
                  )
                )
              )
      ),
      tabItem(tabName = "datos",
              h2("Base de datos", style = "text-align: center;"),
              downloadButton("descargar_excel", "Descargar Excel"),
              downloadButton("descargar_csv", "Descargar CSV"),
              DTOutput("tabla_datos")
      )
    )
  )
)
