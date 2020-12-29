# Módulo de visualización de resultados

SEATS = sum(seats_raw$Cupo)

create_densities_party <- function(data, new_output_path){
    party_global = data %>% group_by(Partido) %>%
				summarise(Asientos_ganados = median(Asientos_ganados), .groups = 'drop')

    parties = party_global$Partido[party_global$Asientos_ganados >0]
    
    data_2 = data[data$Partido %in% parties,]
    
    g <- ggplot(data_2, aes(Partido, Asientos_ganados))
    g + geom_violin() + 
    labs(title="Distribucion de representantes por partido", 
        x="Partido",
        y="Representantes electos")
    
    file=paste0(new_output_path, '/party_result.png')
    g
    ggsave(file, width = 24, height = 16, unit = 'cm')
}

create_densities_coalition <- function(data, new_output_path){
    party_global = data %>% group_by(Coalicion) %>%
				summarise(Asientos_ganados = median(Asientos_ganados), .groups = 'drop')

    parties = party_global$Coalicion[party_global$Asientos_ganados >0]
    
    data_2 = data[data$Coalicion %in% parties,]
    
    g <- ggplot(data_2, aes(Coalicion, Asientos_ganados))
    g + geom_violin() + 
    labs(title="Distribucion de representantes por coalicion", 
        x="Coalicion",
        y="Representantes electos")
    
    file=paste0(new_output_path, '/coa_result.png')
    g
    ggsave(file, width = 24, height = 16, unit = 'cm')
}

create_chamber <- function(data, new_output_path){
    
    seats <- function(N,M, r0=2.5){ 
        radii <- seq(r0, 1, len=M)

        counts <- numeric(M)
        pts = do.call(rbind,
                    lapply(1:M, function(i){
                    counts[i] <<- round(N*radii[i]/sum(radii[i:M]))
                    theta <- seq(0, pi, len = counts[i])
                    N <<- N - counts[i]
                    data.frame(x=radii[i]*cos(theta), y=radii[i]*sin(theta), r=i,
                                theta=theta)
                    }  )
        )
        pts = pts[order(-pts$theta,-pts$r),]
        pts
        }

    election <- function(seats, counts){
        stopifnot(sum(counts)==nrow(seats))
        seats$party = rep(1:length(counts),counts)
        seats
        }

    party_global = data %>% group_by(Coalicion) %>%
				summarise(Asientos_ganados = median(Asientos_ganados), .groups = 'drop')
    
    party_global$Asientos_ganados = round(party_global$Asientos_ganados/sum(party_global$Asientos_ganados),2)

    party_global$Asientos_ganados = round(SEATS*party_global$Asientos_ganados,0)
    party_global = party_global[party_global$Asientos_ganados>0,]

    layout = seats(SEATS,10)
    
    df = data.frame(coa=party_global$Coalicion, col=seq(1:nrow(party_global)))
    result = election(layout, party_global$Asientos_ganados)
    result$coa = df$coa[result$party]

    plotted_chamber= ggplot(result, aes(x, y)) + geom_point(aes(colour = coa, size = 3)) +
        theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), 
        axis.title.x=element_blank(),  axis.title.y=element_blank(), panel.background = element_rect(fill = "white", colour = "white") ) + 
        scale_color_discrete(name='Coalicion')
    
    file=paste0(new_output_path, '/chamber.png')
    plotted_chamber
    ggsave(file, width = 24, height = 16, unit = 'cm')

}

create_waffle <- function(data, new_output_path, name_tag, titulo){

    color_position = data.frame(
        Partido = c('PC', 'PH', 'RD','ECO','PRO','PS','PRSD','PPD','DC','EVO','RN', 'UDI','PREP'),
        Partido_col = c('01_PC', '01_PH', '02_RD','03_ECO','04_PRO','05_PS','06_PRSD','07_PPD','08_DC','09_EVO','10_RN', '11_UDI','12_PREP'))

    color_coa = data.frame(
        Coalicion = c('PC-FA', 'UC', 'CHV'),
        Coalicion_col = c('01_PC-FA', '02_UC', '03_CHV'))

    data = subset(data, data$Asientos_ganados > 0)

    niveles = nrow(data)

    separa = rep(': ', niveles)

    if( colnames(data)[1] == 'Coalicion'){
        data = merge(data, color_coa)
        data$Coalicion = NULL

        names(data)[names(data) == "Coalicion_col"] <- "Coalicion"
        labels_num = paste0(data$Coalicion, separa, as.character(data$Asientos_ganados))
    }
    else{
        data = merge(data, color_position)
        data$Partido = NULL

        names(data)[names(data) == "Partido_col"] <- "Partido"

        labels_num = paste0(data$Partido, separa, as.character(data$Asientos_ganados))

    }

    if(parameters['modo'][[1]] == 'concejales'){
        ROWS = 100
    }
    else{
        ROWS = 5
    }

    new = unlist(split(as.numeric(data$Asientos_ganados), labels_num))

    file=paste0(new_output_path, name_tag)
    waffle(new, rows=ROWS, size=0.5, colors=brewer.pal(niveles,"Spectral"), title=titulo, xlab="1 cuadrado = 1 asiento")
    ggsave(file, width = 24, height = 16, unit = 'cm')
    # colors=brewer.pal(niveles,"Set1")

}