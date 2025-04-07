# functions.R

# Función de análisis de asociación
Asociacion <- function(dt, crit_val) {
  colms <- ncol(dt)

  # Matriz de covarianza
  mat_cov <- cov(dt)
  tab_cov <- melt(mat_cov)
  colnames(tab_cov) <- c("fila", "columna", "covar")
  gg_cov <- ggplot(tab_cov, aes(x = fila, y = columna, fill = covar)) +
    geom_tile() +
    geom_text(aes(label = round(covar, 2)), color = "white", size = 3) +
    coord_fixed()

  # Matriz de correlación
  mat_corr <- cor(dt)
  tab_corr <- melt(mat_corr)
  colnames(tab_corr) <- c("fila", "columna", "corr")
  gg_corr <- ggplot(tab_corr, aes(x = fila, y = columna, fill = corr)) +
    geom_tile() +
    geom_text(aes(label = round(corr, 2)), color = "white", size = 3) +
    coord_fixed()

  # Dependencias significativas
  dependencias <- abs(mat_corr) >= crit_val
  for (i in 1:colms) {
    dependencias[i, i] <- FALSE
  }

  # Matriz de correlaciones relevantes
  mat_graph <- mat_corr
  mat_graph[,] <- NA
  for (i in 1:colms) {
    for (j in i:colms) {
      if (dependencias[i, j]) {
        mat_graph[i, j] <- mat_corr[i, j]
      } else {
        mat_graph[i, j] <- 0
      }
    }
  }

  tab_dep <- melt(mat_graph)
  colnames(tab_dep) <- c("fila", "columna", "dcorr")
  gg_dep <- ggplot(tab_dep, aes(x = fila, y = columna, fill = dcorr)) +
    geom_tile() +
    geom_text(aes(label = round(dcorr, 2)), color = "white", size = 3) +
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
