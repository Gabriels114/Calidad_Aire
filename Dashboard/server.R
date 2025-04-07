# server.R

# Cargar funciones definidas en el módulo
source("modules/funciones.R", local = TRUE)

server <- function(input, output, session) {

  datos <- reactive({
    input$actualizar

    df <- isolate({
      df <- read_sheet(sheet_id)
      df <- df[-1, ]
      df <- df %>% mutate(across(c(Temp, Hum, CO2, TVOC, AIRQ, CO), as.numeric))
      df$TVOC <- df$TVOC / 1000
      df$DateTime <- as.POSIXct(paste(df$Date, df$Time), format = "%Y-%m-%d %H:%M:%S")
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
      plot <- plot + geom_vline(aes(xintercept = min_val), color = "orange", linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = max_val), color = "red", linetype = "dashed", size = 1)
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
      geom_ribbon(aes(ymin = mu - sd, ymax = mu + sd), fill = "hotpink", alpha = 0.3) +
      labs(title = paste("Estimación MLE para", var),
           x = "Número de Muestra", y = var) +
      theme_minimal()

    plot_em <- ggplot(df_em, aes(x = x)) +
      geom_point(aes(y = y), alpha = 0.5, color = "gray60") +
      geom_line(aes(y = mu), color = "skyblue", linewidth = 1) +
      geom_ribbon(aes(ymin = mu - sd, ymax = mu + sd), fill = "hotpink", alpha = 0.3) +
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
