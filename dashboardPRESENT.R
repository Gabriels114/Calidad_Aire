library(shiny)
library(shinydashboard)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(GGally)
library(DT)
library(writexl)
library(readr)
library(igraph)

googlesheets4::gs4_deauth() 

sheet_id <- "16rXqiNUI00xbXZkVNZnfhp-eHXoIf_zEnJ2wcRgQZxM"

# Función de análisis de asociación
Asociacion <- function(dt, crit_val){
  colms <- ncol(dt)
  
  # Matriz de covarianza
  mat_cov <- cov(dt)
  tab_cov <- melt(mat_cov)
  colnames(tab_cov) <- c("fila","columna","covar")
  gg_cov <- ggplot(tab_cov, aes(x=fila, y=columna, fill=covar)) +
    geom_tile() +
    geom_text(aes(label=round(covar,2)),color="white",size=3) +
    coord_fixed()
  
  # Matriz de correlación
  mat_corr <- cor(dt)
  tab_corr <- melt(mat_corr)
  colnames(tab_corr) <- c("fila","columna","corr")
  gg_corr <- ggplot(tab_corr, aes(x=fila, y=columna, fill=corr)) +
    geom_tile() +
    geom_text(aes(label=round(corr,2)),color="white",size=3) +
    coord_fixed()
  
  # Dependencias significativas
  dependencias <- abs(mat_corr) >= crit_val
  for (i in 1:colms){
    dependencias[i,i] <- FALSE
  }
  
  # Matriz de correlaciones relevantes
  mat_graph <- mat_corr
  mat_graph[,] <- NA
  for(i in 1:colms){
    for(j in i:colms){
      if(dependencias[i,j]){
        mat_graph[i,j] <- mat_corr[i,j]
      } else {
        mat_graph[i,j] <- 0
      }
    }
  }
  
  tab_dep <- melt(mat_graph)
  colnames(tab_dep) <- c("fila","columna","dcorr")
  gg_dep <- ggplot(tab_dep, aes(x=fila, y=columna, fill=dcorr)) +
    geom_tile() +
    geom_text(aes(label=round(dcorr,2)),color="white",size=3) +
    coord_fixed()
  
  grafo <- graph_from_adjacency_matrix(mat_graph, mode = "upper", weighted = TRUE)
  
  return(list(
    mat_cov = mat_cov,
    mat_corr = mat_corr,
    mat_graph = mat_graph,
    grafo = grafo,
    gg_cov = gg_cov,
    gg_corr = gg_corr,
    gg_dep = gg_dep
  ))
}

fnDataAugmentation <- function(datos, size = 5000) {
  n <- length(datos)
  if (n >= size) {
    return(datos)
  }
  extrasamples <- size - n
  data_aumentada <- sample(datos, size = extrasamples, replace = TRUE)
  augmenteddata <- c(datos, data_aumentada)
  return(augmenteddata)
}

fnModa <- function(datos) {
  frecuencias <- table(datos)
  moda <- names(frecuencias)[frecuencias == max(frecuencias)]
  return(as.numeric(moda[1]))
}

fnCV <- function(datos) {
  CV <- sd(datos, na.rm = TRUE) / mean(datos, na.rm = TRUE) * 100
  return(CV)
}

fnAsimetria <- function(datos) {
  n <- length(datos)
  sumatoria <- sum((datos - mean(datos, na.rm = TRUE))^3, na.rm = TRUE)
  m3 <- sumatoria / n
  S3 <- sd(datos, na.rm = TRUE)^3
  asimetric <- m3 / S3
  return(asimetric)
}

fnKurtosis <- function(datos) {
  n <- length(datos)
  sumatoria <- sum((datos - mean(datos, na.rm = TRUE))^4, na.rm = TRUE)
  kurtosis <- (sumatoria / (n * (sd(datos, na.rm = TRUE)^4))) - 3
  return(kurtosis)
}

fnCuasiGaussian <- function(x) {
  datosamp <- fnDataAugmentation(x, size = 5000)
  test <- ks.test(datosamp, "pnorm", 
                  mean = mean(datosamp, na.rm = TRUE), 
                  sd = sd(datosamp, na.rm = TRUE))
  return(ifelse(test$p.value > 0.05, "Gaussiana", "No Gaussiana"))
}

# interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "TroyaUAQ"),
  
  # Barra lateral de navegación
  dashboardSidebar(
    sidebarMenu(
      menuItem("Informe", tabName = "informe", icon = icon("file-alt")),
      menuItem("Análisis", tabName = "analisis", icon = icon("chart-line")),
      menuItem("Base de Datos", tabName = "datos", icon = icon("table"))
    )
  ),
  
  # Cuerpo de la aplicación
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
      
      # Sección para análisis y gráficos
      tabItem(tabName = "analisis", 
              sidebarLayout(
                sidebarPanel(
                  actionButton("actualizar", "Actualizar Datos"),
                  dateRangeInput("fecha", "Seleccionar fechas:", 
                                 start = Sys.Date() - 30, 
                                 end = Sys.Date()),
                  selectInput("variable", "Variable:", choices = c("Temp", "Hum", "CO2", "TVOC", "AIRQ", "CO")),
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
                             selectInput("yvar", "Variable Y:", choices = c("Temp", "Hum", "CO2", "TVOC", "AIRQ", "CO"), selected = "Temp"),
                             sliderInput("num_clusters", "Número de clusters:", min = 1, max = 10, value = 3)
                    ),
                    tabPanel("Matriz de Correlación", plotOutput("correlacion")),
                    tabPanel("Estadísticas", uiOutput("tabla")),
                    tabPanel("Matriz de Correlación", plotOutput("corr_plot")),
                    tabPanel("Matriz de Covarianza", plotOutput("cov_plot")),
                    tabPanel("Dependencias Significativas", plotOutput("dep_plot")),
                    tabPanel("Grafo de Asociación", plotOutput("graph_plot")),
                    tabPanel("MLE y EM Estimación", 
                             plotOutput("mle_em_plot"))
                  )
                )
              )
      ),
      tabItem(
        tabName = "datos",
        h2("Base de datos", style = "text-align: center;"),
        downloadButton("descargar_excel", "Descargar Excel"),
        downloadButton("descargar_csv", "Descargar CSV"),
        DTOutput("tabla_datos")
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  datos <- reactive({
    input$actualizar
    
    df <- isolate({
      df <- read_sheet(sheet_id)
      df <- df[-1, ]
      df <- df %>% mutate(across(c(Temp, Hum, CO2, TVOC, AIRQ, CO), as.numeric))
      df$TVOC <- df$TVOC / 1000
      df$DateTime <- as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
      df
    })
    
    df %>% filter(
      DateTime >= as.POSIXct(input$fecha[1]),
      DateTime <= as.POSIXct(input$fecha[2] + 1)
    )
  })
  
  results_asoc <- reactive({
    req(datos())
    cdiaq <- datos()[, c("Temp", "Hum", "CO2", "TVOC", "AIRQ", "CO")]
    Asociacion(cdiaq, input$crit_val)
  })
  
  output$corr_plot <- renderPlot({
    results_asoc()$gg_corr
  })
  
  output$cov_plot <- renderPlot({
    results_asoc()$gg_cov
  })
  
  output$dep_plot <- renderPlot({
    results_asoc()$gg_dep
  })
  
  output$graph_plot <- renderPlot({
    plot.igraph(results_asoc()$grafo, 
                vertex.shape = "square", 
                vertex.color = "cyan",
                edge.label = round(E(results_asoc()$grafo)$weight, 3),
                edge.arrow.size = 0.5,
                layout = layout_in_circle)
  })
  
  output$histograma <- renderPlot({
    variable <- input$variable
    estadistica <- input$estadistica
    bins <- input$bins   
    
    # Calcular las estadísticas 
    media <- mean(datos()[[variable]], na.rm = TRUE)
    mediana <- median(datos()[[variable]], na.rm = TRUE)
    moda <- fnModa(datos()[[variable]])  
    q1 <- quantile(datos()[[variable]], 0.25, na.rm = TRUE)
    q3 <- quantile(datos()[[variable]], 0.75, na.rm = TRUE)
    min_val <- min(datos()[[variable]], na.rm = TRUE)
    max_val <- max(datos()[[variable]], na.rm = TRUE)
    cv <- fnCV(datos()[[variable]])  
    asimetria <- fnAsimetria(datos()[[variable]]) 
    curtosis <- fnKurtosis(datos()[[variable]])  
    
    plot <- ggplot(datos(), aes(x = .data[[variable]])) +
      geom_histogram(aes(y = ..density..), bins = bins, fill = "lightblue", color = "black", alpha = 0.5) + 
      geom_density(color = "red", lwd = 1.2) +  
      labs(
        title = paste("Distribución de", variable),
        x = variable,
        y = "Frecuencia"
      ) +
      theme_minimal() + 
      theme(
        plot.title = element_text(hjust = 0.5),  
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    
    # Añadir la estadística seleccionada
    if (estadistica == "Moda") {
      plot <- plot + geom_vline(aes(xintercept = moda), color = "purple", linetype = "dashed", size = 1) 
    } else if (estadistica == "Mínimo") {
      plot <- plot + geom_vline(aes(xintercept = min_val), color = "orange", linetype = "dashed", size = 1) 
    } else if (estadistica == "Q1") {
      plot <- plot + geom_vline(aes(xintercept = q1), color = "green", linetype = "dashed", size = 1)  
    } else if (estadistica == "Q2") {
      plot <- plot + geom_vline(aes(xintercept = mediana), color = "blue", linetype = "dashed", size = 1) 
    } else if (estadistica == "Q3") {
      plot <- plot + geom_vline(aes(xintercept = q3), color = "green", linetype = "dashed", size = 1) 
    } else if (estadistica == "Máximo") {
      plot <- plot + geom_vline(aes(xintercept = max_val), color = "red", linetype = "dashed", size = 1)  
    } else if (estadistica == "Rango") {
      plot <- plot + geom_vline(aes(xintercept = min_val), color = "orange", linetype = "dashed", size = 1)  
      plot <- plot + geom_vline(aes(xintercept = max_val), color = "red", linetype = "dashed", size = 1)  
    } else if (estadistica == "Media") {
      plot <- plot + geom_vline(aes(xintercept = media), color = "blue", linetype = "dashed", size = 1)  
    } else if (estadistica == "Mediana") {
      plot <- plot + geom_vline(aes(xintercept = mediana), color = "green", linetype = "dashed", size = 1)  
    }
    
    plot
  })
  
  output$serie_tiempo <- renderPlot({
    df <- datos()
    variable <- input$variable
    
    ggplot(df, aes(x = DateTime, y = .data[[variable]])) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(alpha = 0.4, size = 1.5, color = "darkblue") +
      labs(
        title = paste("Serie de Tiempo de", variable),
        x = "Fecha y Hora",
        y = variable
      ) 
  })
  
  output$boxplot <- renderPlot({
    ggplot(datos(), aes(y = .data[[input$variable]])) +
      geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
      labs(title = paste("Diagrama de Caja de", input$variable), y = input$variable)
  })
  
  output$dispersión <- renderPlot({
    df <- datos()
    x_var <- input$yvar       
    y_var <- input$variable   
    num_clusters <- input$num_clusters  
    
    df_scaled <- scale(df[, c(x_var, y_var)])
    
    set.seed(43)
    clusters <- kmeans(df_scaled, centers = num_clusters)
    
    ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]], color = as.factor(clusters$cluster))) +
      geom_point(alpha = 0.6, size = 2) +
      labs(
        title = paste("Dispersión entre", y_var, "y", x_var, "con", num_clusters, "Clusters"),
        x = x_var,
        y = y_var,
        color = "Cluster"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  })
  
  output$correlacion <- renderPlot({
    df_cor <- datos()[, c("Temp", "Hum", "CO2", "TVOC", "AIRQ", "CO")]
    
    ggpairs(
      df_cor,
      progress = FALSE,
      lower = list(continuous = wrap("points", color = "lightblue", alpha = 0.6)),
      upper = list(continuous = wrap("cor", size = 4, color = "lightblue")),
      diag = list(continuous = wrap("densityDiag", fill = "lightblue"))
    )
  })
  
  output$tabla <- renderUI({
    variable <- input$variable
    datos_filtrados <- datos()[[variable]]
    
    Media <- mean(datos_filtrados, na.rm = TRUE)
    Moda <- fnModa(datos_filtrados)
    Mediana <- median(datos_filtrados, na.rm = TRUE)
    Varianza <- var(datos_filtrados, na.rm = TRUE)
    DesviacionEstandar <- sd(datos_filtrados, na.rm = TRUE)
    CoefVar <- fnCV(datos_filtrados)
    Asimetria <- fnAsimetria(datos_filtrados)
    Kurtosis <- fnKurtosis(datos_filtrados)
    CuasiGaussian <- fnCuasiGaussian(datos_filtrados)
    
    Cuartil1 <- quantile(datos_filtrados, 0.25, na.rm = TRUE)
    Cuartil2 <- Mediana  # Cuartil 2 es la Mediana
    Cuartil3 <- quantile(datos_filtrados, 0.75, na.rm = TRUE)
    Minimo <- min(datos_filtrados, na.rm = TRUE)
    Maximo <- max(datos_filtrados, na.rm = TRUE)
    Rango <- Maximo - Minimo
    
    tagList(
      h3("Momento estadístico"),
      tags$hr(style = "border-color: blue;"),
      strong("Tendencia Central"),
      tags$p(paste("Media:", round(Media, 2))),
      tags$p(paste("Mediana:", round(Mediana, 2))),
      tags$p(paste("Moda:", round(Moda, 2))),
      tags$hr(style = "border-color: red;"),
      strong("Dispersión"),
      tags$p(paste("Varianza:", round(Varianza, 2))),
      tags$p(paste("Desviación estándar:", round(DesviacionEstandar, 2))),
      tags$p(paste("Coeficiente de Variación:", round(CoefVar, 2), "%")),
      tags$hr(style = "border-color: green;"),
      strong("Forma"),
      tags$p(paste("Asimetría:", round(Asimetria, 2))),
      tags$p(paste("Curtosis:", round(Kurtosis, 2))),
      tags$p(paste("Prueba Cuasi-Gaussiana:", CuasiGaussian)),
      tags$hr(style = "border-color: purple;"),
      strong("Posición"),
      tags$p(paste("Cuartil 1 (Q1):", round(Cuartil1, 2))),
      tags$p(paste("Cuartil 2 (Q2 - Mediana):", round(Cuartil2, 2))),
      tags$p(paste("Cuartil 3 (Q3):", round(Cuartil3, 2))),
      tags$p(paste("Mínimo:", round(Minimo, 2))),
      tags$p(paste("Máximo:", round(Maximo, 2))),
      tags$p(paste("Rango:", round(Rango, 2)))
    )
  })
  
  
  
  output$mle_em_plot <- renderPlot({
    req(datos(), input$variable)
    var <- input$variable
    sensor <- na.omit(datos()[[var]])
    nsample <- length(sensor)
    if(nsample < 2) return()
    
    # Crear dataframe para MLE
    df_mle <- data.frame(
      x = 1:nsample,
      y = sensor,
      mu = cummean(sensor),
      sd = sqrt(cumsum((sensor - cummean(sensor))^2)/(1:nsample - 1))
    )
    
    # Crear dataframe para EM
    p <- 0.9
    mu_em <- numeric(nsample)
    v_em <- numeric(nsample)
    mu_em[1] <- sensor[1]
    v_em[1] <- 0
    
    for(i in 2:nsample) {
      mu_em[i] <- p * mu_em[i-1] + (1-p) * sensor[i]
      v_em[i] <- p * v_em[i-1] + (1-p) * (sensor[i] - mu_em[i-1])^2
    }
    
    df_em <- data.frame(
      x = 1:nsample,
      y = sensor,
      mu = mu_em,
      sd = sqrt(v_em)
    )
    
    plot_mle <- ggplot(df_mle, aes(x = x)) +
      geom_point(aes(y = y), alpha = 0.5, color = "gray60") +
      geom_line(aes(y = mu), color = "purple", linewidth = 1) +
      geom_ribbon(aes(ymin = mu - sd, ymax = mu + sd), 
                  fill = "hotpink", alpha = 0.3) +
      labs(title = paste("Estimación MLE para", var),
           x = "Número de Muestra", y = var) +
      theme_minimal()
    
    plot_em <- ggplot(df_em, aes(x = x)) +
      geom_point(aes(y = y), alpha = 0.5, color = "gray60") +
      geom_line(aes(y = mu), color = "skyblue", linewidth = 1) +
      geom_ribbon(aes(ymin = mu - sd, ymax = mu + sd), 
                  fill = "hotpink", alpha = 0.3) +
      labs(title = paste("Estimación EM para", var),
           x = "Número de Muestra", y = var) +
      theme_minimal()
    
    # Combinar gráficos
    gridExtra::grid.arrange(plot_mle, plot_em, ncol = 1)
  })
  
  output$tabla_datos <- renderDT({
    datos() %>%
      datatable(options = list(
        scrollX = TRUE,
        pageLength = 10,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        ),
        searchHighlight = FALSE,
        searching = FALSE
      ), rownames = FALSE) %>%
      formatStyle(names(datos()), backgroundColor = 'white')
  })
  
  output$descargar_excel <- downloadHandler(
    filename = function() {
      paste("datos-aire-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(datos(), file)
    }
  )
  
  output$descargar_csv <- downloadHandler(
    filename = function() {
      paste("datos-aire-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(datos(), file)
    }
  )
  
}

# Ejecutar la aplicación
shinyApp(ui, server)
