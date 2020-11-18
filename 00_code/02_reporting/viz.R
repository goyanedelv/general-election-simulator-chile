#data = read.xlsx('C:/Users/goyan/Desktop/Simulador_electoral/98_output/experimento_019_generalized_3/simulacion_partido.xlsx')
#data = read.xlsx('C:/Users/goyan/Desktop/Simulador_electoral/98_output/experimento_019_generalized_3/simulacion_coa.xlsx')

create_densities_party <- function(data, new_output_path){
    party_global = data %>% group_by(Partido) %>%
				summarise(Asientos_ganados = mean(Asientos_ganados), .groups = 'drop')

    parties = party_global$Partido[party_global$Asientos_ganados >0]
    
    data_2 = data[data$Partido %in% parties,]
    
    g <- ggplot(data_2, aes(Partido, Asientos_ganados))
    g + geom_violin() + 
    labs(title="Distribucion de representantes electos", 
        x="Partido",
        y="Representantes electos")
    
    file=paste0(new_output_path, '/party_result.png')
    g
    ggsave(file, width = 24, height = 16, unit = 'cm')
}

create_densities_coalition <- function(data, new_output_path){
    party_global = data %>% group_by(Coalicion) %>%
				summarise(Asientos_ganados = mean(Asientos_ganados), .groups = 'drop')

    parties = party_global$Coalicion[party_global$Asientos_ganados >0]
    
    data_2 = data[data$Coalicion %in% parties,]
    
    g <- ggplot(data_2, aes(Coalicion, Asientos_ganados))
    g + geom_violin() + 
    labs(title="Distribucion de representantes electos", 
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
				summarise(Asientos_ganados = mean(Asientos_ganados), .groups = 'drop')
    
    party_global$Asientos_ganados = round(party_global$Asientos_ganados/sum(party_global$Asientos_ganados),2)

    party_global$Asientos_ganados = round(155*party_global$Asientos_ganados,0)
    party_global = party_global[party_global$Asientos_ganados>0,]
    #sum(party_global$Asientos_ganados)

    layout = seats(155,10)
    
    df = data.frame(coa=party_global$Coalicion, col=seq(1:nrow(party_global)))
    result = election(layout, party_global$Asientos_ganados)
    result$coa = df$coa[result$party]

    plotted_chamber= ggplot(result, aes(x, y)) + geom_point(aes(colour = coa)) +
        theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), 
        axis.title.x=element_blank(),  axis.title.y=element_blank(), panel.background = element_rect(fill = "white", colour = "white") ) + 
        scale_color_discrete(name='Coalicion')
    
    file=paste0(new_output_path, '/chamber.png')
    plotted_chamber
    ggsave(file, width = 24, height = 16, unit = 'cm')

}
#create_chamber(data)