# global.R
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

# Desautenticación de Google Sheets
googlesheets4::gs4_deauth()

# ID de la hoja de Google Sheets
sheet_id <- "16rXqiNUI00xbXZkVNZnfhp-eHXoIf_zEnJ2wcRgQZxM"
