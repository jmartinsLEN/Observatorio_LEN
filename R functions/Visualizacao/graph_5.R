library(ggplot2)
library(ggrepel)
library(scales)

build_pUnitario <- function(df, TT,anoMes) {
  mes <- as.numeric(strsplit(anoMes,"-")[[1]][2])
  ano <- as.numeric(strsplit(anoMes,"-")[[1]][1])
  cal <- create_calendar(ano,ano,"cal")
  
  
  df_mes <- subset(df, month(df$timestamp) == mes & year(df$timestamp) == ano)
  
  for(date in as.Date(df_mes$timestamp)) {
    if(!isFeriado(as.Date(date)) & (wday(as.Date(date)) > 1 & wday(as.Date(date)) < 7)) {
      dia <- day(as.Date(date))
      break()
    }
  }
  
  df_mes <- df_mes[ymd_hms(df_mes$timestamp) >= ymd_hms(paste0(ano,"-",mes,"-",dia, " 00:00:00")) & 
                     ymd_hms(df_mes$timestamp) <= ymd_hms(paste0(ano,"-",mes,"-",dia," 24:00:00")),]
  
  periodo <- calcula_periodo(mes)
  
  if(TT == "BTE") {
    df_mes <- transform(df_mes, Preco_Unitario = ifelse(periodo == 1 | periodo == 4 & df_mes$PHorario == df_mes$PHorario, 
                                                        round(tarifComerc[df_mes$PHorario]+t(tar_ativa_BTE[paste0(ano), paste0(df_mes$PHorario,"1e4")])+impostoConsumo, digits = 2), 
                                                        round(tarifComerc[df_mes$PHorario]+t(tar_ativa_BTE[paste0(ano), paste0(df_mes$PHorario,"2e3")])+impostoConsumo, digits = 2)))
  } else if(TT == "MT") {
    df_mes <- transform(df_mes, Preco_Unitario = ifelse(periodo == 1 | periodo == 4 & df_mes$PHorario == df_mes$PHorario, 
                                                        round(tarifComerc[df_mes$PHorario]+t(tar_ativa_MT[paste0(ano), paste0(df_mes$PHorario,"1e4")])+impostoConsumo, digits = 2), 
                                                        round(tarifComerc[df_mes$PHorario]+t(tar_ativa_MT[paste0(ano), paste0(df_mes$PHorario,"2e3")])+impostoConsumo, digits = 2)))
    
  }
  
  df_output <- data.frame(timestamp = df_mes$timestamp, Preco_Unitario= df_mes$Preco_Unitario)
  return(df_output)
}

calcula_periodo <- function(mes) {
  if(mes >= 1 & mes <= 3) {
    periodo <- 1
  } else if(mes >= 4 & mes <= 6) {
    periodo <- 2
  } else if(mes >= 7 & mes <= 10) {
    periodo <- 3
  } else {
    periodo <- 4
  }
  
  return(periodo)
}


G5_preco_unitario <- function(df, TT, anoMes) {
  df_pUnitario <- build_pUnitario(df, TT, anoMes)
  df_pUnitario <- melt(df_pUnitario, id=c("timestamp"))
  
  ggplot(df_pUnitario, aes(x=timestamp, y = value, width=0.8)) +
    geom_line(aes(x=timestamp, y = value, colour=variable), stat = 'identity') +
    scale_fill_discrete("") +
    ylab(paste0("Preco Unitario")) +
    xlab("") +
    theme_minimal(base_size = 9) +
    geom_path(colour="red") +
    #expand_limits(y=c(0,max(df_pUnitario$value))) +
    scale_x_datetime(date_breaks="1 hour", labels = date_format("%H:%M"), expand=c(0,0)) +
    scale_color_manual(labels = c(paste0("Preço (", day(df_pUnitario$timestamp[1]), "-", month.abb[month(df_pUnitario$timestamp[1])], "-",year(df_pUnitario$timestamp[1]), ")")), values = c("red")) +
    theme(panel.grid.minor.x = element_blank(),
          axis.title.x=element_text(face="bold"),
          axis.title.y=element_text(face="bold"),
          panel.grid.major.x = element_blank(),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
}


G5_preco_unitario(newDFCD,"MT", "2016-4")