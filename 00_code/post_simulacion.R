# Análisis post-simulación

library(dplyr)
library(openxlsx)

setwd("C:/Users/goyan/Desktop/20191227_SimulaConvencion/")

# Parámetro: 1000 simulaciones, Chile Vamos separado

resultados = load('98_output/simulaciones_concejales/experimento_008_1000/resultados_simulacion.RData')
seats_raw = read.csv('02_parameter/Cupos_Concejales.csv', fileEncoding = 'UTF-8-BOM')

coalicion = read.xlsx('98_output/simulaciones_concejales/experimento_008_1000/simulacion_coa.xlsx')#wrapper_total[[1]]
partido = read.xlsx('98_output/simulaciones_concejales/experimento_008_1000/simulacion_partido.xlsx')#wrapper_total[[2]]
detalle = read.xlsx('98_output/simulaciones_concejales/experimento_008_1000/simulacion_detalle.xlsx')#wrapper_total[[3]][[1]]

detalle = merge(detalle,seats_raw, by="ID")


Proba_k_concejales<-function(k,Party){ 
largo = length((unique(detalle$Simulacion)))

detalle_resumen= detalle %>% 
	filter(Cupos_Partido == k & Sigla == Party)%>%
    group_by(Comuna) %>%
	summarise(Probabilidad = n(), .groups = 'drop')%>%
    arrange(desc(Probabilidad))
    
    return(detalle_resumen)
}
