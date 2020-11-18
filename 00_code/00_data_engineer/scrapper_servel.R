	library(openxlsx)
	library(dplyr)
    rm(list=ls())

	setwd("C:/Users/goyan/Desktop/20191227_SimulaConvencion/")

	wrapper = data.frame(Comuna=NULL,
						Sub.Pacto=NULL,
						Votacion=NULL)

    # Función que los lee
	links = c(
		'http://www.servel.cl/wp-content/uploads/2017/02/01_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/02_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/03_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/04_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/05_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/06_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/07_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/08_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/09_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/10_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/11_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/12_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/13_Resultados_Mesa_Concejales_TER_1.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/13_Resultados_Mesa_Concejales_TER_2.xlsx',	
		'https://www.servel.cl/wp-content/uploads/2017/03/14_Resultados_Mesa_Concejales_TER.xlsx',
		'https://www.servel.cl/wp-content/uploads/2017/02/15_Resultados_Mesa_Concejales_TER.xlsx')


	for(i in 1:length(links)){

    	data = read.xlsx(links[i])

  		data_agged = data %>% 
			group_by(Comuna, Sub.Pacto, Partido) %>%
			summarise(Votacion = sum(Votos.TER), .groups = 'drop')

		wrapper = rbind(wrapper,data_agged)
		print(paste0(i," de ",length(links)))

	}

wrapper_2 = wrapper[!wrapper$Partido == 'NA',]

write.csv(wrapper_2,'01_input/original_servel.csv', row.names=FALSE, fileEncoding = 'UTF-8')

