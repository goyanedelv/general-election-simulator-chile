##########################################
# Código maestro de simulación electoral #
##########################################

rm(list=ls())
    
#setwd("C:/Users/goyan/Desktop/Simulador_electoral")

# Cargar librerias
source('00_code/libraries.R')
# Cargar funciones de simulación
source('00_code/simulador.R')
#Cargar funciones de creación de escenarios
source('00_code/scenario_creator.R')

SIMULATE_NOW_MANY()

