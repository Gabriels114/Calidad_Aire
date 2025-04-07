# server.R

# Fuente de las funciones auxiliares
source("functions.R", local = TRUE)

server <- function(input, output, session) {

  # Carga y filtrado de datos
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

  # Cálculo de asociaciones
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
      labs(title = paste("Distribución de", variable),
           x = variable,
           y = "Frecuencia") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))

    # Añadir la estadística seleccionada
    if (estadistica == "Moda") {
      plot <- plot + geom_vline(aes(xintercept = moda), color = "purple", linetype = "dashed", size = 1)
    } else if (estadistica == "Mínimo") {
      plot <- plot + geom_vline(aes(xintercept = min_val), color = "orange", linetype = "dashed", size = 1)
    } else if (estadistica == "Q1") {
      plot <- plot + geom_vline(aes(xintercept = q1), color = "green", linetype = "dashed", size = 1)
    } else if (estadistica == "Q2") {
      plot <- plot + geom_vline(aes(xintercept = mediana), color = "blue", linetype = "dashed", size = 1)
    } else if (estadistica ==
