#-----------------------------------#
# IPESPE
#-----------------------------------#
# INFOGOV
# Relatorio Agua e Saneamento
#-----------------------------------#
# Claudio A. Monteiro
# 16/10/2018
#-----------------------------------#

#setwd("~/Documents/TRABALHOS/git_lab_projects/analises-ipespe")

# carregar pacotes
library(readxl); library(ggplot2); library(Amelia); library(stringr)

# carregar base de dados
Agregado_SNIS_total <- read_excel("dados/agregadoSNISfinal.xls")

# desabilitar notacao cientifica
options(scipen=999)

### tema ggplot
tema_massa <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.ticks = element_line(size = 1, colour = "grey70" ),
          axis.text.x = element_text(colour= "black",size=10,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=10,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=11,angle=90,hjust=.5,vjust=0.5,face="plain"),
          title = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(6, "mm"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"),
          plot.background = element_blank())
}


#=====================================#
#       TRATAMENTO DOS DADOS
#=====================================#

#===== criar base de dados =====#

# selecionar variaveis de referencia
SNISdata <- Agregado_SNIS_total[,c(1:10)]

# adicionar indicadores de interesse a base  
SNISdata$AG001_pop_atendida_abastecimento_agua <- Agregado_SNIS_total$`AG001 - População total atendida com abastecimento de água (Habitantes)`
SNISdata$IN022_consumo_percapita_litrohabdia <- Agregado_SNIS_total$`IN022 - Consumo médio percapita de água (l/hab./dia)`
SNISdata$IN056_indice_de_coleta_esgoto <- Agregado_SNIS_total$`IN005 - Tarifa média de água (R$/m³)`
SNISdata$IN016_indice_tratamento_esgoto_percentual <- Agregado_SNIS_total$`IN016 - Índice de tratamento de esgoto (percentual)`
SNISdata$ES005_volume_esgoto_coletado_metro3 <- Agregado_SNIS_total$`ES005 - Volume de esgotos coletado (1.000 m³/ano)`

#===== remover casos faltantes =====#
missmap(SNISdata)
SNISdata <- data.frame(SNISdata)
SNISdata <- SNISdata[complete.cases(SNISdata),]

#=================================================#
# EVOLUCAO TEMPORAL DO INDICADOR NO ESTADO DE SP
#=================================================#

dataVar= SNISdata$IN022_consumo_percapita_litrohabdia
data = SNISdata
legendaVar = 'Consumo Percapita de Água (l/hab/dia)'
chave = 'media'

#============================ funcao grafica

cleanStr <- function(string){
  library(stringi); library(stringr)
  string = str_replace_all(string, ' ', '_')
  string = str_replace_all(string, '-', '_')
  string = str_replace_all(string, '%', '')
  string = str_replace_all(string, '/', '_')
  string = stri_trans_general(string, "latin-ascii")
  string = tolower(string)
  return(string)
}

timePlot = function(dataTime, legendaVar, chave){
  plot = ggplot(data = dataTime, aes(x = tempo, y = x, group = 1)) +
            geom_line(color = "#2F2F4F", size = 1) +
            #stat_smooth(method = lm, color= "#c77521", se = F)+
            geom_label(aes(label = dataTime$label), size= 3)+
            labs(x = "", y= legendaVar)+
            scale_x_continuous(breaks = seq(1995, 2016))+
            tema_massa()%+replace% 
            theme(axis.text.x = element_text(size=10, angle = 50, hjust=1,vjust=.9,face="plain")) 
  # se agregar scale y
  if (chave == 'agregar'){
    plot = plot + scale_y_continuous(breaks = c(min(dataTime$x), mean(dataTime$x), max(dataTime$x)) )
  }
  # salvar grafico
  ggsave(paste0('tempoSP_', cleanStr(legendaVar),'.png'), 
         plot, 
         path = 'resultados',
         width = 8,
         height = 4,
         units = 'in')
  return(plot)
}

#============================ processamento

dev01 <- function(data, dataVar, chave, legendaVar){
  
  if (chave=='agregar'){
    # agregar valores
    dataTime = aggregate(dataVar, by = list(tempo = data$Ano.de.Referência), sum)
  }
  
  if (chave=='media'){
    # tirar media valores
    dataTime = aggregate(dataVar, by = list(tempo = data$Ano.de.Referência), mean)
    dataTime$x = round(dataTime$x,2)
  }
  
  # else { 
  #  return("Chave não encontrada! Tente 'agregar' ou 'media' ")
  #}
  
  # label
  dataTime$label = NA
  dataTime$label[c(2,6,11,16,21)] <- dataTime$x[c(2,6,11,16,21)] 
  # grafico
  plot = timePlot(dataTime, legendaVar, chave)
  return(plot)
}

# executar funcao
dev01(SNISdata, SNISdata$AG001_pop_atendida_abastecimento_agua, 'agregar','Pop. com Abastecimento de Água')
dev01(SNISdata, SNISdata$IN022_consumo_percapita_litrohabdia, 'media','Consumo Percapita de Água (l-hab-dia)')
dev01(SNISdata, SNISdata$IN056_indice_de_coleta_esgoto, 'media','Índice de Coleta de Esgoto')
dev01(SNISdata, SNISdata$IN016_indice_tratamento_esgoto_percentual, 'media','Índice Tratamento de Esgoto')
dev01(SNISdata, SNISdata$ES005_volume_esgoto_coletado_metro3, 'agregar','Volume Esgoto Coletado (m³)' )


#================================================#
# 10 MAIORES E 10 MENORES CIDADES EM 2016
#================================================#

SNIS2016 <- SNISdata[SNISdata$Ano.de.Referência == 2016,]
data = SNIS2016
var = 12
chave = 'mais'

#dev.off()
dev02 <- function(data, var, chave , legendaVar){
  
  # ordenar e selecionar casos
  dataOrd = data[order(data[,var]),]
  dataOrd$selec = c(1:length(dataOrd[,var])) # selecionar variavel
  dataOrd = dataOrd[dataOrd$selec <= 10 | dataOrd$selec >= (length(dataOrd$selec)-9) ,]
  dataOrd$Município = factor(dataOrd$Município, levels = unique(dataOrd$Município[order(dataOrd[var])]), ordered=TRUE)
  
  if (chave == 'menos'){
    
    dataOrd = dataOrd[1:10,]
  
    } 
  
  if (chave == 'mais'){
    
    dataOrd = dataOrd[11:20,]
  
    }
  
  # produzir grafico
  plot = ggplot(data = dataOrd, aes(x = `Município`, y = dataOrd[,var]))+
            geom_bar(fill = "black", position = "dodge", stat="identity")+
            geom_label(aes(label = dataOrd[,var]), size = 2.8)+
            labs(x = '', y = legendaVar)+
            tema_massa()+
            coord_flip()
  
  # salvar grafico
  ggsave(paste0('10cidades_', chave, '_',cleanStr(legendaVar),'.png'), 
         plot, 
         path = 'resultados',
         width = 7,
         height = 5,
         units = 'in')
   return(plot)
}

dev02(SNIS2016, 11, 'menos','Pop. com Abastecimento de Água')
dev02(SNIS2016, 11, 'mais','Pop. com Abastecimento de Água')

dev02(SNIS2016, 12, 'menos','Consumo Percapita de Água (l-hab-dia)')
dev02(SNIS2016, 12, 'mais','Consumo Percapita de Água (l-hab-dia)')

dev02(SNIS2016, 13, 'menos','Índice de Coleta de Esgoto')
dev02(SNIS2016, 13, 'mais','Índice de Coleta de Esgoto')

dev02(SNIS2016, 15, 'menos','Volume Esgoto Coletado (m³)' )
dev02(SNIS2016, 15, 'mais','Volume Esgoto Coletado (m³)' )


#================================================#
# CRESCIMENTO E DECRESCIMENTO POR CIDADE
#================================================#

# colors
library(wesanderson)
library(RColorBrewer)
library(viridis)

data = SNISdata
key = 11
chave = 'sobe'

# definir funcao
dev03 <- function(data, key, chave, legendaVar){
  
  library(dplyr); library(ggplot2); library(viridis); library(ggrepel); library(RColorBrewer)
  
  # pre-processing
  data10 = data[data$Ano.de.Referência == 2010, c(1,2,key)]
  colnames(data10)[3] <- 'var10' 
  data16 = data[data$Ano.de.Referência ==2016, c(1,2,key)]
  colnames(data16)[3] <- 'var16'
  dataComp = merge(data10, data16, by = 'Código.do.Município')
  dataComp = mutate(dataComp, diferenca = dataComp$var16/dataComp$var10)
  
  # variavel de selecao 
  dataComp = dataComp[order(dataComp$diferenca),]
  dataComp$selec = c(1:length(dataComp$diferenca))
  
  if (chave=='desce'){
    dataSelection = dataComp[dataComp$selec <= 4, ]
    tipo='decrescimento_'
  }
  
  if (chave=='sobe'){
    dataSelection = dataComp[dataComp$selec >= (length(dataComp$selec)-3), ] 
    tipo='crescimento_'
  }
  
  
  dataSelected = merge(data, dataSelection, by = 'Código.do.Município')
  
  # selecionar anos
  dataSelected = dataSelected[dataSelected$Ano.de.Referência >= 2010,]
  
  # construir label
  labela = dataSelected[,key]
  label = ifelse(dataSelected$Ano.de.Referência == 2010 | dataSelected$Ano.de.Referência == 2016, labela, NA)

 plot = ggplot(data = dataSelected, aes(x = `Ano.de.Referência`, y = dataSelected[,key], label = label))  +
    geom_line(aes(group = `Município`, color = `Município`, linetype = `Município`), size = 1) + 
    geom_label_repel(aes(fill = `Município`), color = 'white', size = 3)+
    scale_x_continuous(breaks = seq(2010, 2016))+
    labs(x = "", y = legendaVar )+
    scale_linetype_manual(name = 'Municípios', values = c(1, 2, 3, 4)) +
    scale_color_manual(name = 'Municípios',values=c(wes_palette(4, name="Cavalcanti1")))+
    scale_fill_manual(name = 'Municípios',values=c(wes_palette(4, name="Cavalcanti1")))+
    tema_massa()
    #theme(legend.position="bottom")
  
  # salvar grafico
  ggsave(paste0(tipo, cleanStr(legendaVar),'.png'), 
         plot, 
         path = 'resultados',
         width = 8,
         height = 4,
         units = 'in')
  return(plot)

  }

dev03(SNISdata, 12, chave= 'desce', 'Consumo Percapita de Água (l-hab-dia)')
dev03(SNISdata, 13, chave= 'desce', 'Índice de Coleta de Esgoto')
dev03(SNISdata, 14, chave= 'desce', 'Índice Tratamento de Esgoto')
dev03(SNISdata, 15, chave= 'desce', 'Volume Esgoto Coletado (m³)' )

dev03(SNISdata, 12, chave= 'sobe', 'Consumo Percapita de Água (l-hab-dia)')
dev03(SNISdata, 13, chave= 'sobe', 'Índice de Coleta de Esgoto')
dev03(SNISdata, 14, chave= 'sobe', 'Índice Tratamento de Esgoto')
dev03(SNISdata, 15, chave= 'sobe', 'Volume Esgoto Coletado (m³)' )

display_wes_palette(4, name="Cavalcanti1")

#==========================================#
# ATLAS BRASIL
#==========================================#

library(readxl)
AtlasBrasil_Consulta <- read_excel("dados/AtlasBrasil_Consulta.xlsx")

#===============================#
#  comparativos estaduais



#====== tratar base de dados

datavar = AtlasBrasil_Consulta$`% da população em domicílios com água encanada 2000`

treatAtlas <- function(data){
  
}

Datavar = AtlasBrasil_Consulta$`% da população em domicílios com água encanada 2000`
data = AtlasBrasil_Consulta 

#======= produzir graficos

dev04 <- function(data, Datavar , legendaVar, title){
  
  library(ggplot2); 
  # ordenar e selecionar casos
  data$Espacialidades = factor(data$Espacialidades, levels = unique(data$Espacialidades[order(Datavar)]), ordered=TRUE)
  
  # produzir grafico
  plot = ggplot(data = data, aes(x = `Espacialidades`, y = Datavar))+
    geom_bar(fill = "black", position = "dodge", stat="identity")+
    geom_label(aes(label = Datavar), size = 2.4)+
    labs(title = title, x = '', y = legendaVar)+
    tema_massa()+
    coord_flip()
  
  return(plot)
}

library(ggpubr)

#=============== agua

agua1 <- dev04(AtlasBrasil_Consulta, 
               AtlasBrasil_Consulta$`% da população em domicílios com água encanada 2000`, 
               "Pop. com Água Encanada", "2000")

agua2 <- dev04(AtlasBrasil_Consulta, 
               AtlasBrasil_Consulta$`% da população em domicílios com água encanada 2010`, 
               "Pop. com Água Encanada", "2010")

ggarrange(agua1, agua2, ncol = 2, nrow = 1)
ggsave("comparativo_EstadosAGUA.png", path = "resultados", width = 8, height = 6, units = "in")

#=============== agua banheiro
aguabanheiro1 <- dev04(AtlasBrasil_Consulta, 
                       AtlasBrasil_Consulta$`% da população em domicílios com banheiro e água encanada 2000`,
                       "Pop. com Banheiro e Água Encanada", "2000")

aguabanheiro2 <- dev04(AtlasBrasil_Consulta, 
                       AtlasBrasil_Consulta$`% da população em domicílios com banheiro e água encanada 2010`,
                       "Pop. com Banheiro e Água Encanada", "2010")

ggarrange(aguabanheiro1, aguabanheiro2, ncol = 2, nrow = 1)
ggsave("comparativo_EstadosAGUABANHEIRO.png", path = "resultados", width = 8, height = 6, units = "in")

#=========== lixo
lixo1 <- dev04(AtlasBrasil_Consulta, 
               AtlasBrasil_Consulta$`% da população em domicílios com coleta de lixo 2000`,
               "Pop. com Coleta de Lixo", "2000")

lixo2 <- dev04(AtlasBrasil_Consulta, 
               AtlasBrasil_Consulta$`% da população em domicílios com coleta de lixo 2010`,
               "Pop. com Coleta de Lixo", "2010")

ggarrange(lixo1, lixo2, ncol = 2 ,nrow = 1)
ggsave("comparativo_EstadosLIXO.png", path = "resultados", width = 8, height = 6, units = "in")
