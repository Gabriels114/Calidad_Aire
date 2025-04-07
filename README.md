# 🌬️ Proyecto Calidad del Aire – TroyaUAQ📊

Bienvenido al repositorio del proyecto **Calidad del Aire – TroyaUAQ**, una iniciativa multidisciplinaria que evalúa la calidad del aire interior en la Facultad de Informática de la UAQ. Nuestro objetivo es analizar cómo los contaminantes atmosféricos impactan en la salud, concentración y productividad de los estudiantes y personal docente.

![Air Quality](https://github.com/Anmol-Baranwal/Cool-GIFs-For-GitHub/blob/main/weather.gif?raw=true)

---

## 🧠 Tecnologías y Ramas Involucradas

Este proyecto integra diversas disciplinas y tecnologías:

### 📡 **IoT (Internet of Things)**
- Microcontrolador **ESP32** para adquisición y transmisión de datos.
- Sensores ambientales:
  - ![DHT11](https://img.shields.io/badge/DHT11-Temp%2FHumidity-blue?style=flat) **DHT11** (Temperatura y Humedad)
  - ![MQ135](https://img.shields.io/badge/MQ--135-Air%20Quality-green?style=flat) **MQ-135** (Calidad del Aire - CO₂, NH₃, Benceno)
  - ![MQ7](https://img.shields.io/badge/MQ--7-CO-red?style=flat) **MQ-7** (Monóxido de Carbono)
  - ![CCS811](https://img.shields.io/badge/CCS811-CO2/TVOC-orange?style=flat) **CCS811** (Dioxido de Carbono y compuestos volátiles totales en el aire)
- Pantalla **LCD 1602A** para visualización local.
- Comunicación WiFi con **Google Sheets**.
- Lenguaje **C++**

### 📊 **Ciencia de Datos y Estadística**
- Análisis exploratorio y descriptivo.
- Medidas de tendencia central, dispersión y forma.
- Técnicas de correlación y dependencia.
- Visualización de datos mediante dashboards interactivos.

### 💻 **Desarrollo Web con R/Shiny**
- Aplicación web para análisis de datos.
- Integración con hojas de cálculo de Google.
- Visualizaciones interactivas y descarga de resultados.

### 🌱 **Salud Ambiental y Ergonomía**
- Comparación con valores recomendados por la OMS.
- Identificación de riesgos ambientales en aulas.
- Propuesta de acciones para mejorar las condiciones interiores.

---

## 🎯 Objetivos del Proyecto

### 🎯 Objetivo General
Detectar y analizar la concentración de contaminantes en espacios interiores para evaluar su impacto en la salud, el rendimiento cognitivo y la productividad de la comunidad universitaria.

### 🛠️ Metodología

1. **Captura de Datos:**
   - Sensores conectados a un ESP32 recopilan información ambiental.
   - Visualización en pantalla LCD.
   - Registro automático en Google Sheets.

2. **Análisis en R:**
   - Procesamiento de datos desde Google Sheets.
   - Análisis estadístico y representación gráfica.
   - Detección de patrones y valores atípicos.

3. **Visualización en Shiny:**
   - Dashboard interactivo accesible por navegador.
   - Gráficas de series de tiempo, histogramas, boxplots y más.
   - Descarga de reportes en `.xlsx` y `.csv`.

---

## 📦 Dependencias

### R / Shiny
```r
shiny, shinydashboard, googlesheets4, ggplot2, dplyr, reshape2,
gridExtra, GGally, DT, writexl, readr, igraph
```

### Arduino / ESP32
- [LiquidCrystal](https://www.arduino.cc/en/Reference/LiquidCrystal)
- [DHTesp](https://github.com/beegee-tokyo/DHTesp)
- [MQ135](https://github.com/GeorgK/MQ135)
- [MQUnifiedsensor](https://github.com/miguelbalboa/MQUnifiedsensor)

---

## 🚀 Cómo Ejecutar el Proyecto

### 🧪 Aplicación Shiny (R)

1. Instala **R** y **RStudio**.
2. Instala las dependencias con:
```r
install.packages(c("shiny", "shinydashboard", "googlesheets4", "ggplot2",
                 "dplyr", "reshape2", "gridExtra", "GGally", "DT",
                 "writexl", "readr", "igraph"))
```
3. Abre y ejecuta el archivo `app.R`:
```r
shiny::runApp("Funciones y Dashboard")
```

### 🔧 Código Arduino/ESP32

1. Abre el IDE de Arduino.
2. Carga el archivo `Control_Sensores_CA.ino` o `calib135.ino`.
3. Verifica tener instaladas todas las librerías mencionadas.
4. Conecta el ESP32, selecciona el puerto y sube el código.
5. Visualiza datos en el LCD y valida en Google Sheets.

---

## 🔮 Futuras Mejoras

- Extender análisis estadístico en la app.
- Incorporar más sensores (PM2.5, CO, O₃).
- Crear sistema de alertas por niveles peligrosos.
- Comunicación con base de datos en la nube (Firebase, MongoDB).
- Visualización en dashboards como Power BI o Grafana.

![Future Ideas](https://github.com/Anmol-Baranwal/Cool-GIFs-For-GitHub/blob/main/ideas.gif?raw=true)

---

## 🧑‍🤝‍🧑 Equipo de Desarrollo

| 👤 Nombre              | 📧 Email institucional        | 📧 Email personal         | 📅 LinkedIn |
|----------------------|-------------------------------|------------------------------|----------|
| Gabriel Gudiño 🚀       | jgudino27@alumnos.uaq.mx        | gabriels114@gmail.com       | [Perfil](https://www.linkedin.com/in/gabriel-gudiño-lara/) |
| Ana Ramírez 🌿         | aramirez285@alumnos.uaq.mx      | anarramirez.ralo@gmail.com  | [Perfil](https://www.linkedin.com/in/ana-ramirez-lopez-6110b7296/) |
| Fernando Cárdenas 💡    | fcardenas04@alumnos.uaq.mx      | fercard.go@gmail.com        | [Perfil](https://www.linkedin.com/in/fercardgo/) |
| Kay García 🎨           | kgarcia85@alumnos.uaq.mx        | kayychez@gmail.com          | [Perfil](https://www.linkedin.com/in/kay-garcia-icad/) |
