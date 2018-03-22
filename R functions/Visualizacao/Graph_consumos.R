library(ggplot2)
library(ggrepel)
#source("R/tarifas.R")
# Create test data.
# newDFCD <- buildTarifDF(dBTE, "cd")
# 
# consumo<- buildConsumDF(newDFCD)
# 
# 
# fatura <- buildFatura(newDFCD)

#mesAno- "Dec/2017
G4_Consumo_Anual_PHorario <- function(consumoDF, faturaDF, anoMes) {
  ano <- as.numeric(strsplit(anoMes,"-")[[1]][1])
  mes <- as.numeric(strsplit(anoMes,"-")[[1]][2])
  anoMes_string <- paste0(month.abb[mes], "/", ano)
  consumoDF <- subset(consumoDF, as.yearmon(consumoDF$Date, format = "%b/%Y") <= as.yearmon(anoMes_string, format = "%b/%Y"))
  #Filtrar dados de consumo
  temp = consumoDF[,1:6]
  #Para utilizar dados dos ultimos 12 meses
  temp = tail(temp, 13)
  #Adicionar coluna de reativa com valores a 0  ao consumo os dfs 
  #terem as mesmas dimensoes
  temp$Reativa <- 0
  ano_max_min <- c(max(year(as.yearmon(temp$Date, format = "%b/%Y"))),min(year(as.yearmon(temp$Date, format = "%b/%Y"))))
  #Filtrar dados da df fatura para aparecer so as colunas 
  #com os valores de fatura dos periodos horarios
  faturaTemp <- faturaDF[,5:9]
  faturaTemp$Date <- faturaDF$Date
  faturaTemp <- tail(faturaTemp, 13)
  
  dat_agg <- data.frame(CountConsumo=c(sum(temp$P), sum(temp$C), sum(temp$VN), sum(temp$SV), sum(temp$Reativa)), CountFatura = c(sum(faturaTemp$custo_P), sum(faturaTemp$custo_C), sum(faturaTemp$custo_VN), sum(faturaTemp$custo_SV), sum(faturaTemp$custo_reativa)), category = c("Ponta","Cheia","Vazio Normal","Super Vazio","Reativa"), stringsAsFactors = FALSE)
  if(dat_agg$CountFatura[5] == 0 & dat_agg$CountConsumo[5] == 0) {
    dat_agg <- data.frame(CountConsumo=c(sum(temp$P), sum(temp$C), sum(temp$VN), sum(temp$SV)), CountFatura = c(sum(faturaTemp$custo_P), sum(faturaTemp$custo_C), sum(faturaTemp$custo_VN), sum(faturaTemp$custo_SV)), category = c("Ponta","Cheia","Vazio Normal","Super Vazio"), stringsAsFactors = FALSE)  
  }
  dat_agg$ConsumoPerc <- dat_agg$CountConsumo/sum(dat_agg$CountConsumo)
  dat_agg$FaturaPerc <- dat_agg$CountFatura/sum(dat_agg$CountFatura)
  dat_agg$category <- factor(dat_agg$category, levels = dat_agg$category)
  
  if(nrow(dat_agg) == 5) {
    dat_agg <- dat_agg %>%
      mutate(Colour = ifelse(.$category == "Cheia", "#FFD11A", ifelse(.$category == "Ponta", "#FF0000", ifelse(.$category == "Reativa", "#000000",ifelse(.$category == "Super Vazio", "#009933", "#0066FF"))))) %>%
      group_by(category)
  } else {
    dat_agg <- dat_agg %>%
      mutate(Colour = ifelse(.$category == "Cheia", "#FFD11A", ifelse(.$category == "Ponta", "#FF0000", ifelse(.$category == "Super Vazio", "#009933", "#0066FF")))) %>%
      group_by(category)
  }
  
  dat_agg$ConsumoPerc[dat_agg$ConsumoPerc == 0] <- NA
  
  First <- ggplot(dat_agg) +
    geom_bar(aes(x=0,y = 0, fill = category, group ="identity"), stat='identity', size=0.15, na.rm = TRUE) +
    geom_bar(aes(x=1,y = ConsumoPerc, fill = category, group ="identity"), colour="black", width = 1.1, position = "fill", stat='identity', size=0.15) +
    geom_bar(aes(x=2,y = FaturaPerc, fill = category, group ="identity"),  colour="black", width = 1.1, position = "fill", stat='identity', size=0.15) +
    geom_text(aes(x= 1, y=ConsumoPerc, label = ifelse(ConsumoPerc == 0,"", paste(round(ConsumoPerc*100), "%", sep = ""))), size=4, position = position_fill(vjust = 0.5), na.rm = TRUE, color = "white") +
    geom_text(aes(x= 2, y=FaturaPerc, label = ifelse(FaturaPerc == 0,"", paste(round(FaturaPerc*100), "%", sep = ""))), angle = ifelse(dat_agg$FaturaPerc < 0.01,90,0),  size=4, position = position_fill(vjust = 0.5), na.rm = TRUE, color = "white") +
    geom_text(aes(x=0,y = 0, label = "Consumo")) +
    geom_text(aes(x=3,y = 0, label = "Fatura")) +
    scale_fill_manual(aes(x=category, y=FaturaPerc), values = dat_agg$Colour) +
    guides(fill=guide_legend(title=paste0(ano_max_min[2],"/", substr(ano_max_min[1],3,4)))) +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(), axis.title=element_blank(),    
          axis.ticks=element_blank(), axis.text = element_blank(),
          legend.title = element_text(face="bold"),
          panel.border = element_rect(colour = "black", fill=NA),
          legend.box.background = element_rect(colour = "black"))
  
  First + coord_polar('y')
}
#df- df do ciclo em questao (diario, semanal ou opcional)
#TT- "MT" ou "BTE"
#monthYear- "Nov/2017"
G6_Perfil_Diario_Mensal <- function(df, TT, anoMes) {
  mes <- as.numeric(strsplit(anoMes,"-")[[1]][2])
  ano <- as.numeric(strsplit(anoMes,"-")[[1]][1])
  mes_ano_string <- paste0(month.abb[mes], "-",ano)
  
  c_mes <- buildConsumDF_mes(df, TT, anoMes)
  c_mes <- melt(c_mes, id=c("Day"))
  c_mes <- c_mes[with(c_mes, order(Day)),]
  if (min(c_mes$value)>1000) {
    c_mes$value <- round(c_mes$value/1000, digits = 2)
    str_unit <- "MWh" 
  } else {
    str_unit <- "kWh" 
  }
  
  AUX <- c_mes$PHorario
  AUX[AUX == "C"] <- "Cheia"
  AUX[AUX == "P"] <- "Ponta"
  AUX[AUX == "VN"] <- "Vazio Normal"
  AUX[AUX == "SV"] <- "Super Vazio"
  
  AUX <- factor(AUX, levels = c("Ponta","Cheia","Vazio Normal","Super Vazio"))
  
  #Criar df com soma de consumos em todos os pHorarios de 1 dia
  #para achar o maximo e expandir o limite y do graf em conformidade
  sum_dia <- aggregate(x= c_mes["value"],by = list(Day = c_mes$Day), FUN = sum)
  
  ggplot(c_mes, aes(x=Day, y = value, width=0.8)) +
    geom_bar(aes(x=Day, y = value, fill= AUX), stat = 'identity', position = 'stack') +
    scale_x_continuous(labels = c(paste(c(1:31))), expand = c(0,0), breaks = c(1:31)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values=c("#FF0000", "#FFD11A","#0066FF", "#009933"), name=paste0(mes_ano_string)) +
    ylab(paste0("Consumo de eletricidade [",str_unit,"/dia]")) +
    xlab("Dia do mês") +
    expand_limits(y=round(sum_dia$value)*1.10) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor.x = element_blank(),
          axis.title.x=element_text(face="bold"),
          axis.title.y=element_text(face="bold"),
          panel.grid.major.x = element_blank(),
          legend.title = element_text(face="bold"),
          panel.border = element_rect(colour = "black", fill=NA),
          legend.box.background = element_rect(colour = "black"))
}
