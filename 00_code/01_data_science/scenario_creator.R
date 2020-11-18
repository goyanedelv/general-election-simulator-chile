# TO-DO
    # Variabilidad DC
    # [DONE] Variabilidad Evopoli-no-2016
    # [DONE] Variabilidad RD

# Carga datos y define carpeta
    #setwd("C:/Users/goyan/Desktop/20191227_SimulaConvencion/")

    df = data_original #read.csv('01_input/data_original_concejales_2016.csv', sep=';', fileEncoding = 'UTF-8-BOM')
    data_jak = read.csv('03_supplemental_data/votacion_jak.csv')
    data_rd = read.csv('03_supplemental_data/votacion_rd.csv')
	
    #dicc_02_raw = read.csv("02_parameter/diccionario_siglacoa_v2.csv", sep=';')
	scenario = parameters['coaliciones'][[1]]
	dicc_02 = dicc_02_raw[,c("Sigla",scenario)]
	colnames(dicc_02) <- c("Sigla","Coalicion") 
#   
    # Diccionario id comuna <-> id distrito
	distrito_comuna = read.xlsx("02_parameter/00_otros_diccionarios/id_distrito_a_id_comuna.xlsx")

# Preámbulo
    totals_comuna <- df %>% 
        group_by(ID) %>%
        summarise(Votacion = sum(Votacion))

    full = merge(df,totals_comuna, by ='ID')
    colnames(full) = c('ID', 'Sigla','Comuna','Votacion','Total_comuna')

    full$Share = full$Votacion/full$Total_comuna

    full_new = full[c('ID', 'Sigla', 'Votacion')]


Simulador_puntual <- function(full_new,full, sim_tag){ 
    # DC variabilidad intrínseca
            for (i in 1:346){
            
            dc_lower = parameters['lower_dc'][[1]]
            dc_upper = parameters['upper_dc'][[1]]

            votacion_dc = full_new$Votacion[full_new$Sigla == 'DC' & full_new$ID == i]
            
            full_new$Votacion[full_new$Sigla == 'DC' & full_new$ID == i] = runif(1,
                                                                            round(votacion_dc*dc_lower,0),
                                                                            round(votacion_dc*dc_upper,0))
    
        }
    
    
    # RN, DC, EVO absorben al PRI

        elasticidad_rn_pri = parameters['elesticidad_rn_pri'][[1]]
        elasticidad_evo_pri = parameters['elesticidad_evo_pri'][[1]]
        elasticidad_dc_pri = 1-elasticidad_rn_pri-elasticidad_evo_pri

        for (i in 1:346){
            votacion_rn = full_new$Votacion[full_new$Sigla == 'RN' & full_new$ID == i]
            votacion_evo = full_new$Votacion[full_new$Sigla == 'EVO' & full_new$ID == i]
            votacion_dc = full_new$Votacion[full_new$Sigla == 'DC' & full_new$ID == i]
    
            votacion_pri = full_new$Votacion[full_new$Sigla == 'PRI' & full_new$ID == i]
            
            full_new$Votacion[full_new$Sigla == 'RN' & full_new$ID == i] = round(votacion_rn + votacion_pri*elasticidad_rn_pri,0)
            full_new$Votacion[full_new$Sigla == 'EVO' & full_new$ID == i] = round(votacion_evo + votacion_pri*elasticidad_evo_pri,0)
            full_new$Votacion[full_new$Sigla == 'DC' & full_new$ID == i] = round(votacion_dc + votacion_pri*elasticidad_dc_pri,0)
    
            full_new$Votacion[full_new$Sigla == 'PRI' & full_new$ID == i] = 0

        }
    #

    # Crea JAK
        data_jak$share_jak[is.na(data_jak$share_jak)] = 0

        factor_ajuste_maximo_jak = parameters['factor_ajuste_maximo_jak'][[1]]
        factor_ajuste_minimo_jak = parameters['factor_ajuste_minimo_jak'][[1]]


        elasticidad_jak_rn = parameters['elasticidad_jak_rn'][[1]]
        elasticidad_jak_udi = 1-elasticidad_jak_rn

        for (i in 1:346){

            share_jak = data_jak$share_jak[data_jak$ID == i]

            if(share_jak<quantile(data_jak$share_jak)[3]){
                votacion_jak = runif(1,0,round(sum(full$Votacion[full$ID == i])*share_jak*factor_ajuste_maximo_jak,0))

            }
            else{
                votacion_jak = runif(1,
                round(sum(full$Votacion[full$ID == i])*share_jak*factor_ajuste_minimo_jak,0),
                round(sum(full$Votacion[full$ID == i])*share_jak*factor_ajuste_maximo_jak,0))

            }


            wrapper = data.frame(ID = i,
                                Sigla = 'PREP',
                                Votacion = votacion_jak)

            votacion_rn = full_new$Votacion[full_new$Sigla == 'RN' & full_new$ID == i]
            full_new$Votacion[full_new$Sigla == 'RN' & full_new$ID == i] =round(max(0, votacion_rn-votacion_jak*elasticidad_jak_rn),0)

            votacion_udi = full_new$Votacion[full_new$Sigla == 'UDI' & full_new$ID == i]
            full_new$Votacion[full_new$Sigla == 'UDI' & full_new$ID == i] =round(max(0, votacion_udi-votacion_jak*elasticidad_jak_udi),0)

            full_new = bind_rows(full_new, wrapper)
        }
    #

    # Mejora Evopoli

        #p_no_2016 = quantile(full$Share[full$Sigla == "EVO" & full$Votacion >0], parameters['percentil_no2016_evo'][[1]])
        
        p_no_2016_lower = quantile(full$Share[full$Sigla == "EVO" & full$Votacion >0], parameters['percentil_no2016_evo_lower'][[1]])
        p_no_2016_upper = quantile(full$Share[full$Sigla == "EVO" & full$Votacion >0], parameters['percentil_no2016_evo_upper'][[1]])

        p_max = quantile(full$Share[full$Sigla == "EVO" & full$Votacion >0], parameters['percentil_maximo_evo'][[1]])

        elasticidad_evo_rn = parameters['elasticidad_evo_rn'][[1]]
        elasticidad_evo_udi = 1-elasticidad_evo_rn

        for (i in 1:346){
            #print(i)
            votacion_i = full_new$Votacion[full_new$Sigla == 'EVO' & full_new$ID == i]
            if(votacion_i>0){

                votacion_evo_lower = votacion_i
                votacion_evo_upper = round(sum(full_new$Votacion[full_new$ID == i])*p_max,0)

                votacion_evo = runif(1, votacion_evo_lower, max(votacion_evo_lower,votacion_evo_upper))

                nuevos_votos_evo = votacion_evo - votacion_i
                
                votacion_rn = full_new$Votacion[full_new$Sigla == 'RN' & full_new$ID == i]
                full_new$Votacion[full_new$Sigla == 'RN' & full_new$ID == i] =max(0, votacion_rn-nuevos_votos_evo*elasticidad_evo_rn)

                votacion_udi = full_new$Votacion[full_new$Sigla == 'UDI' & full_new$ID == i]
                full_new$Votacion[full_new$Sigla == 'UDI' & full_new$ID == i] =max(0, votacion_udi-nuevos_votos_evo*elasticidad_evo_udi)

                full_new$Votacion[full_new$Sigla == 'EVO' & full_new$ID == i] = votacion_evo

            }
            else{
                # TO-DO: agregarle variablidad nueva
                nuevos_votos_evo = runif(1,
                                        round(sum(full$Votacion[full$ID == i])*p_no_2016_lower,0),
                                        round(sum(full$Votacion[full$ID == i])*p_no_2016_upper,0))

                votacion_rn = full_new$Votacion[full_new$Sigla == 'RN' & full_new$ID == i]
                full_new$Votacion[full_new$Sigla == 'RN' & full_new$ID == i] =max(0, votacion_rn-nuevos_votos_evo*elasticidad_evo_rn)

                votacion_udi = full_new$Votacion[full_new$Sigla == 'UDI' & full_new$ID == i]
                full_new$Votacion[full_new$Sigla == 'UDI' & full_new$ID == i] =max(0, votacion_udi-nuevos_votos_evo*elasticidad_evo_udi)

                full_new$Votacion[full_new$Sigla == 'EVO' & full_new$ID == i] = nuevos_votos_evo

            }


        }
    #

    # Mejora RD

        elasticidad_rd_ppd = parameters['elasticidad_rd_ppd'][[1]]
        elasticidad_rd_ps = parameters['elasticidad_rd_ps'][[1]]

            for (i in 1:346){
                
                # TO-DO, parametrizar robo a PPD y PS
                votacion_rd_dip_2017 = data_rd$vota_rd[data_rd$ID == i]
                votacion_rd_con_2016 = full_new$Votacion[full_new$ID == i & full_new$Sigla == 'RD']
                
                votacion_rd_upper = max(votacion_rd_dip_2017,votacion_rd_con_2016)
                votacion_rd_lower = votacion_rd_con_2016
                
                
                votacion_rd = runif(1,votacion_rd_lower, votacion_rd_upper)

                nuevos_votos_rd = votacion_rd - votacion_rd_con_2016
                
                full_new$Votacion[full_new$Sigla == 'RD' & full_new$ID == i] = votacion_rd

                votacion_ppd =  full_new$Votacion[full_new$Sigla == 'PPD' & full_new$ID == i] 
                full_new$Votacion[full_new$Sigla == 'PPD' & full_new$ID == i] = max(0, votacion_ppd-nuevos_votos_rd*elasticidad_rd_ppd)

                votacion_ps =  full_new$Votacion[full_new$Sigla == 'PS' & full_new$ID == i] 
                full_new$Votacion[full_new$Sigla == 'PS' & full_new$ID == i] = max(0, votacion_ps-nuevos_votos_rd*elasticidad_rd_ps)


            }
    #
    
    # Variabilidad DC
    # TO-DO, variabilidad DC


    full_new$Simulacion = sim_tag
	
    full_new = merge(full_new,dicc_02, by = "Sigla")

    # Overrider del ID municipal (default) por ID distrito (modo: concejales, convecionales)
	if(parameters['modo'][[1]] != 'concejales'){ 
		full_new = merge(full_new,distrito_comuna)
		full_new$ID = full_new$ID_2
		full_new$ID_2 = NULL
			
		full_new = full_new %>% group_by(ID, Sigla, Simulacion, Coalicion) %>% summarise(Votacion = sum(Votacion), .groups = 'drop')
	}
   
    return(full_new)
}

### Evauador

Evaluador_simulacion <- function(df_original, df_simulado){
    rd_1 = sum(df_original$Votacion[df_original$Sigla == 'RD'])
    rd_2 = sum(df_simulado$Votacion[df_simulado$Sigla == 'RD'], na.omit =TRUE )

    rn_1 = sum(df_original$Votacion[df_original$Sigla == 'RN'])
    rn_2 = sum(df_simulado$Votacion[df_simulado$Sigla == 'RN'], na.omit =TRUE )

    udi_1 = sum(df_original$Votacion[df_original$Sigla == 'UDI'])
    udi_2 = sum(df_simulado$Votacion[df_simulado$Sigla == 'UDI'], na.omit =TRUE )

    evo_1 = sum(df_original$Votacion[df_original$Sigla == 'EVO'])
    evo_2 = sum(df_simulado$Votacion[df_simulado$Sigla == 'EVO'], na.omit =TRUE )

    pri_1 = sum(df_original$Votacion[df_original$Sigla == 'PRI'])
    pri_2 = sum(df_simulado$Votacion[df_simulado$Sigla == 'PRI'], na.omit =TRUE )

    prep_1 = sum(df_original$Votacion[df_original$Sigla == 'PREP'])
    prep_2 = sum(df_simulado$Votacion[df_simulado$Sigla == 'PREP'], na.omit =TRUE )

    data.frame(Partido = c('RD','RN','UDI','EVO','PRI','PREP'),Inicial = c(rd_1,rn_1,udi_1,evo_1,pri_1,prep_1), Simulado = c(rd_2,rn_2,udi_2,evo_2,pri_2,prep_2))

}

# Simular 100 elecciones
Simulador_escenarios <-function(full_new, full, n_simulaciones){ 
    #n_simulaciones = 10
    simulaciones = list()

    for (j in 1:n_simulaciones){

        df_transiente = Simulador_puntual(full_new, full, j)
        #big_wrapper = bind_rows(big_wrapper, df_transiente)
        simulaciones[[j]] = df_transiente
    	print(paste0('Creando escenarios. Progreso: ', round(100*j/n_simulaciones,1),'%'))

    }
    return(simulaciones)

}

Simulador_escenarios_parallel <-function(full_new, full, n_simulaciones){ 

    vec_sim = 1:n_simulaciones
    print('Creando escenarios:')
    simulaciones = pbmapply(function(x,y,z) Simulador_puntual(full_new,full,z), z=vec_sim, SIMPLIFY = FALSE)
    return(simulaciones)

}
