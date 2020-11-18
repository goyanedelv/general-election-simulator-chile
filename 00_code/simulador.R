# 1. Carga de datos
		parameters = read_yaml('02_parameter/parameters.yml', fileEncoding = 'UTF-8')

	# 0.1. Leer data
		data_original = read.csv('01_input/data_original_concejales_2016.csv', sep=';', fileEncoding = 'UTF-8-BOM')

	# 0.4. Cupos por unidad (comuna/distrito)

	if(parameters['modo'][[1]] == 'concejales'){

		seats_raw = read.xlsx('02_parameter/01_diccionarios_cupos/Cupos_Concejales.xlsx')
		asientos <- seats_raw$Cupo
	} else if(parameters['modo'][[1]] == 'convencionales'){
		seats_raw = read.xlsx('02_parameter/01_diccionarios_cupos/Cupos_Diputados.xlsx') #update
		asientos <- seats_raw$Cupo
	} else if(parameters['modo'][[1]] == 'diputados'){
		seats_raw = read.xlsx('02_parameter/01_diccionarios_cupos/Cupos_Diputados.xlsx')
		asientos <- seats_raw$Cupo
	}

	# 0.5. Cargar coaliciones
		dicc_02_raw = read.csv("02_parameter/02_diccionario_coaliciones/diccionario_siglacoa_v2.csv", sep=';')
		scenario = parameters['coaliciones'][[1]]

	# 0.6. Cargar de datos a pipeline de simulación
		data_dip17 = data_original

	# 0.7. Overrider del ID municipal (default) por ID distrito (modo: concejales, convecionales)
		if(parameters['modo'][[1]] != 'concejales'){ 
			distrito_comuna = read.xlsx("02_parameter/00_otros_diccionarios/id_distrito_a_id_comuna.xlsx")
			data_dip17 = merge(data_dip17,distrito_comuna)
			data_dip17$ID = data_dip17$ID_2
			data_dip17$ID_2 = NULL

		}

# 2. Construir coaliciones
	dicc_02 = dicc_02_raw[,c("Sigla",scenario)]
	colnames(dicc_02) <- c("Sigla","Coalicion") 
	data_original_coa = merge(data_original,dicc_02, by = "Sigla")

# 3. Construimos la generalización del D'Hondt

	# 3.1. A nivel de Pacto
	diccionario_PactoCupos <-function(distrito,data){
		# Esta función entrega el número de cupos por pacto para cada distrito

		transiente = data[data$ID == distrito,]
		transiente_pacto = transiente %>% 
			group_by(Coalicion) %>%
			summarise(Votacion = sum(Votacion), .groups = 'drop')

		transiente_pacto = transiente_pacto[order(-transiente_pacto$Votacion),]
		raw = dHondt(transiente_pacto$Votacion, transiente_pacto$Coalicion,asientos[distrito])

		#rownames(transiente_pacto) = NULL

		largo_raw = length(raw)
		largo_full = nrow(transiente_pacto)

		reper = largo_full - largo_raw

		if(reper == 0){
			transiente_pacto$Cupos = raw
		}
		else{
			raw_modified = c(raw,rep(0,reper))
			transiente_pacto$Cupos = raw_modified
		}

		transiente_pacto$ID = distrito

		return(transiente_pacto)
	}
	 
	# 3.2. A nivel de Partido
	diccionario_PartidoCupos <- function(distrito, data){

		transiente = data[data$ID == distrito,]
		transiente_partido = transiente %>% 
			group_by(Sigla, Coalicion) %>%
			summarise(Votacion = sum(Votacion), .groups = 'drop')

		reveal_cupos = diccionario_PactoCupos(distrito,data)
		reveal_cupos$Votacion=NULL
		reveal_cupos$ID=NULL

		transiente_partido = merge(transiente_partido,reveal_cupos, by = "Coalicion")

		transiente_partido$Cupos_Partido = 0

		lista_coalicion = unique(transiente_partido$Coalicion)

		output = data.frame(Coalicion = NULL, Sigla = NULL, Votacion = NULL, Cupos= NULL, Cupos_Partido=NULL)
		for(i in 1:length(lista_coalicion)){

			subtransiente= transiente_partido[transiente_partido$Coalicion == lista_coalicion[i],]

			if(subtransiente$Cupos[1]>1){
				subtransiente = subtransiente[order(-subtransiente$Votacion),]
				rownames(subtransiente) = NULL
				Cupos = subtransiente$Cupos[1]
				raw= dHondt(subtransiente$Votacion, subtransiente$Sigla,Cupos)
					largo_raw = length(raw)
					largo_full = nrow(subtransiente)

					reper = largo_full - largo_raw

					if(reper == 0){
						subtransiente$Cupos_Partido = raw
						output=rbind(output,subtransiente)
					}
					else{
						raw_modified = c(raw,rep(0,reper))
						subtransiente$Cupos_Partido = raw_modified
						output=rbind(output,subtransiente)

					}
			}
			else if(subtransiente$Cupos[1]==1){
				maxi = max(subtransiente$Votacion)
				subtransiente$Cupos_Partido[subtransiente$Votacion == maxi] = 1
				output=rbind(output,subtransiente)

			}
			else{
				subtransiente$Cupos_Partido = 0
				output=rbind(output,subtransiente)
			}

		}
		output = output[order(-output$Cupos_Partido),]
		output$ID = distrito


		return(output)

	}

# 4. Funcion de simulacion

	ElectoSimulate <- function(data){

		wrapper=data.frame(Coalicion=NULL,Sigla=NULL,Votacion=NULL,Cupos_Partido=NULL,ID=NULL)
			
		#ptime <- system.time({	
			for(z in 1:length(asientos)){
				#OPORTUNIDAD GIGANTE PARA PARALELIZAR
				wrapper = bind_rows(wrapper,diccionario_PartidoCupos(z,data))

			}

		return(wrapper)

}

# 5. Función simulación 1 escenario
#	SIMULATE_NOW_SINGLE <-function(){ 
#
#		# 5.1. Simular data pre-base (dip 17)
#
#		gran_tabla = ElectoSimulate(data_original_coa)
#
#		print(tbl_df(gran_tabla[[1]]), n=40)
#		print(tbl_df(gran_tabla[[2]]), n=40)
#
#
#		# 5.2. Guardar datos
#
#		write.xlsx(gran_tabla[[1]], file = paste0("98_output/",scenario,"_pacto_concejales.xlsx"))
#		write.xlsx(gran_tabla[[2]], file = paste0("98_output/",scenario,"_partido_concejales.xlsx"))
#		write.xlsx(gran_tabla[[3]], file = paste0("98_output/",scenario,"_distritos_concejales.xlsx"))
#
#		paste0("Datos guardados en SimulaConvencion/98_output/")

#}
# 6. Función simulación MÚLTIPLES escenario
	SIMULATE_NOW_MANY <-function(){ 
		n_escenarios = parameters['n_simulaciones'][[1]]

		multiples_escenarios = Simulador_escenarios_parallel(full_new, full, n_escenarios)

		nucleos = parameters['n_cores'][[1]]

		if(nucleos >= detectCores()){
			print(paste0('Parametro de nucleos excede el maximo, nucleos establecido en ',detectCores()-1))
			nucleos = detectCores() - 1
		}

		print('Simulando elecciones:')
		ptime_2 <- system.time({
		rr <- mclapply(multiples_escenarios, ElectoSimulate, mc.cores = nucleos)
		
			})[3]
		
		print(paste0(n_escenarios, ' escenarios simulados en ',round(ptime_2,0),' segundos'))
		
		vector_sim = 1:n_escenarios
	
		rr_2 <- mapply(cbind, rr, "Simulacion"=vector_sim, SIMPLIFY=F)

		rr_2_df = do.call(rbind.data.frame, rr_2)
		

		wrapper_total_detalle = rr_2_df[rr_2_df$Votacion>0,]

		wrapper_coa = wrapper_total_detalle %>% 
					group_by(Coalicion, Simulacion) %>%
					summarise(Votacion = sum(Cupos_Partido), .groups = 'drop')
		colnames(wrapper_coa) = c('Coalicion', 'Simulacion','Asientos_ganados')

		coa_global = wrapper_coa %>% group_by(Coalicion) %>%
					summarise(Asientos_ganados = mean(Asientos_ganados), .groups = 'drop')

		wrapper_party = wrapper_total_detalle %>% 
					group_by(Sigla, Simulacion) %>%
					summarise(Votacion = sum(Cupos_Partido), .groups = 'drop')
		colnames(wrapper_party) = c('Partido', 'Simulacion','Asientos_ganados')

		
		new_output_path = paste0('98_output/',parameters['experiment_tag'][[1]])
		dir.create(new_output_path)
		write_yaml(parameters, file=paste0(new_output_path, '/used_parameter.yml'), fileEncoding = "UTF-8")
		write.xlsx(wrapper_coa, file=paste0(new_output_path, '/simulacion_coa.xlsx'), row.names = FALSE)
		write.xlsx(wrapper_party, file=paste0(new_output_path,'/simulacion_partido.xlsx'), row.names = FALSE)
		write.xlsx(coa_global, file=paste0(new_output_path,'/coalicion_resumen_promedio.xlsx'), row.names = FALSE)
		write.xlsx(wrapper_total_detalle, file=paste0(new_output_path,'/simulacion_detalle.xlsx'), row.names = FALSE)
}

