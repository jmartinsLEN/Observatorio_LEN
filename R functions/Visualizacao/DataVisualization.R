
### Gráfico 1:  Consumo mensal últimos 2 anos e previsão deste ano
G1_Consumo_Mensal <- function(df)
{
  if (nrow(df) >= 1 & nrow(df) <= 12) {Xlabel_ano = 30.5}
  else if (nrow(df) > 12 & nrow(df) <= 24) {Xlabel_ano = c(18.5,30.5)}
  else if (nrow(df) > 24 & nrow(df) <= 36) {Xlabel_ano = c(6.5,18.5,30.5)}
  
  ### Unidades inteligentes que se adaptam à dimensão dos valores:
  SA = aggregate(df$Consumo, by = list(Ano = df$Ano), FUN = sum)
  
  
  if (min(SA$x)>10000) {label_ano = c(paste(round(SA$x/1000,1),"MWh/ano"))} else {
    label_ano = c(paste(round(SA$x,1),"kWh/ano"))}
  
  if (min(df$Consumo)>1000) {
    df$Consumo <- round(df$Consumo/1000, digits = 2)
    df$Trans <- round(df$Trans/1000, digits = 2)
    str_unit <- "MWh"}
  else {str_unit <- "kWh"}
  
  Ylabel_ano = min(df$Consumo)*0.5
  
  NL = (36-nrow(df))
  if (NL != 0) {NAS = df[0,]
  NAS[1:NL,2] = df[1,2] - seq(NL/12,1/12,-1/12)
  NAS[,1] = year(NAS[,2])
  NAS[,3:7] = df[1,3:7]
  df = rbind(NAS,df)}
  
  ### Retira o ano no eixo das abcissas:
  # Mes_toplot <- substr(df$Mes_ano,1,nchar(as.character(df$Mes_ano))-5)
  Mes_toplot <- substr(Meses_pt(df$MesAno),1,3)
  
  ### Obtem a linha da matriz em que ocorre a transição entre histórico
  ### e previsão, caso haja:
  if (max(df$Trans)>0) {Pos = min(which(df$Trans>0))+1}
  
  df[is.na(df$GAP),3:5] =  -1
  df[is.na(df$GAP),8] = df[first(which(!is.na(df$GAP))),8]
  
  YMAX = df$Consumo
  YMAX[-seq(2,35,3)] = 0
  
  ### Um gráfico linear de consumo mensal:
  g = ggplot(df, aes(x=factor(MesAno),shape = HoP)) +
    geom_line(aes(y=Consumo,group=HoP,colour = HoP,linetype = HoP),size = 1.5) +
    geom_point(aes(y=Consumo,colour = HoP,size=HoP),show.legend = FALSE) +
    geom_line(aes(y=Consumo,group=HoP,colour = HoP,linetype = HoP),size = 1.5) +
    geom_vline(alpha=0.4,xintercept=c(0.5,12.5,24.5),linetype=2) +
    geom_segment(aes(x=seq(1,36,1),y=YMAX * 0.05,xend=seq(1,36,1),yend=YMAX * 0.95),
                 linetype = "dotted",colour = "grey60",alpha = 0.8) +
    geom_hline(yintercept = 0) +
    geom_ribbon(aes(ymin=0,ymax=Consumo,group=HoP,fill=HoP),alpha=.2,show.legend = FALSE) +
    geom_ribbon(aes(ymin=0,ymax=Trans,group=GAP,fill=GAP),alpha=.2,show.legend = FALSE) +
    scale_fill_manual(values=c("#F8766D","#F8766D", "#00BFC4", "#00BFC4")) +
    scale_x_discrete(labels = Mes_toplot,limits=df$Mes_ano,expand = c(0,0.5)) +
    scale_y_continuous(limits = c(0,max(df$Consumo)*1.15),breaks = pretty(c(0,df$Consumo),n = 4),expand = c(0,0)) +
    expand_limits(y=max(df$Consumo)*1.24) +
    ylab(paste0("Consumo de eletricidade\n[",str_unit,"/mês]")) +
    annotate("text",
             x = c(2,14,26),
             y = max(df$Consumo)*1.1,
             label = c(df[1,1],df[13,1],df[25,1]),
             alpha=0.3,size=4) +
    annotate("text",
             x = Xlabel_ano,
             y = Ylabel_ano,
             label = label_ano,
             size=4,alpha=.6,fontface="italic") +
             {if (max(df$Trans)>0) annotate("point",x=Pos,y=df[Pos,3],colour ="#00BFC4",size=2.5,shape=19)}+
    theme_minimal(base_size = 10) +
    theme(axis.title.x=element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,
                                     face="bold",size = 8),
          axis.text.y = element_text(face="bold",size=8),
          axis.ticks.y = element_line(),
          axis.ticks.x = element_line(),
          legend.position = "top",
          legend.direction = "horizontal",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.text = element_text(size = 7.5),
          legend.title = element_blank()) +
    scale_shape_manual(values = c(19, 15))+
    scale_size_manual(values = c(2.5, 0))+
    scale_colour_discrete(labels=c("Histórico", "Previsão")) +
    scale_linetype_manual(guide = "none",name = "Linetype",values = c("solid","longdash"))
  
  suppressWarnings(print(g))
  
}

### Gráfico 2:  Consumo dos 24 últimos meses (12 + 12 homólogos)
G2_Consumo_12Hom <- function(df,TT)
{
  
  if (min(df$Consumo_m)>1000) {df$Consumo_m <- round(df$Consumo_m/1000, digits = 2); str_unit <- "MWh"}
  else {str_unit <- "kWh"}
  
  ### Define vetor de meses para labels do eixo x:
  Mes_tolabel <- substr(Meses_pt(paste0("1992-",df$Mes,"-01")),1,3)
  Mes_toaxis <- row.names(df)[1:12]
  
  ### Definir strings para a legenda:
  LL = c("Últimos 12 meses", "Período homólogo") #label da legenda
  YMIN = min(df$Consumo_m) # valor mínimo de consumo "normalizado"
  
  YMAX = c(YMIN*0.98/0.97,pmax(df$Consumo_m[2:11],df$Consumo_m[14:23]),YMIN*0.98/0.97)
  
  ggplot(df, aes(x=Mes_toaxis,group=PoH,shape=PoH)) +
    geom_ribbon(aes(ymin=YMIN*0.95,ymax=Consumo_m, linetype=NA),filter(df, df$PoH == "1"),
                alpha=.8,fill="grey80",color="black") +
    geom_segment(aes(x=seq(1,12,1),y=YMIN*0.98,xend=seq(1,12,1),yend=YMAX*0.97),
                 filter(df, df$PoH == "-1"),linetype = "dotted",colour = "grey40",alpha = 0.8) +
    geom_line(aes(y=Consumo_m),filter(df, df$PoH == "1"),size=.5,linetype="dashed") +
    geom_point(aes(y=Consumo_m),filter(df, df$PoH == "1"),size=2,alpha=.6) +
    geom_line(aes(y=Consumo_m),filter(df, df$PoH == "-1"),size=1.3,colour="#F8766D") +
    geom_point(aes(y=Consumo_m),filter(df, df$PoH == "-1"),size=3,colour="#F8766D") +
    scale_shape_manual(values = c(15,19),labels=LL,
                       guide = guide_legend(override.aes = list(
                         alpha=c(1,.6),
                         colour=c("#F8766D","black")))) +
    scale_x_discrete(labels = Mes_tolabel,limits=Mes_toaxis,expand = c(0,0.3)) +
    scale_y_continuous(breaks = pretty(df$Consumo_m,n = 4),
                       expand = c(0,0)) +
    expand_limits(y=max(df$Consumo_m)*1.08) +
    theme_minimal(base_size = 10) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_line(),
          axis.text.x = element_text(face="bold",size = 8),
          axis.text.y = element_text(face="bold",size = 8),
          legend.title = element_blank(),
          # legend.direction="horizontal",
          # legend.position = c(0.5,0.95),
          legend.text = element_text(size = 7.5))+
    ylab(paste0("Consumo de eletricidade\n[",str_unit,"/mês]"))
  
}

### Gráfico 3:  Potência de tomada, contratada, instalada (MT) e mínima (MT)
###             nos últimos 18 meses
G3_Potencia_Mensal <- function(df,TT)
{
  ### Filtrar os dados apenas à janela de 18 meses históricos pretendida:
  df = df[df$HoP == "-1",]
  df = df[(nrow(df)-17):nrow(df),]
  
  ### Meses em português no eixo das abcissas:
  Mes_toplot <- paste0(substr(Meses_pt(df$MesAno),1,3),"\n",substr(df$Ano,3,4))
  
  ### Data cleaning e tratamento da library Reshape2 para obter df em long format: 
  if (TT == "BTE") {
    df = cbind(df[,1:2],df[,4:5])
    LL = c("Potência\ntomada", "Potência\ncontratada") #label da legenda
  }
  else if (TT == "MT") {
    df = cbind(df[,1:2],df[,4:7])
    LL = c("Potência\ninstalada [kVA]", "Potência\nmínima [kW]",
           "Potência\ntomada [kW]","Potência\ncontratada [kW]") #label da legenda
  }
  
  df = melt(df, id.vars = c("Ano","MesAno"))
  
  if (TT == "BTE") {
    ggplot(df,aes(x=factor(MesAno),group = variable,colour=variable,linetype=variable,size=variable)) +
      geom_segment(aes(x=seq(1,18,1),y=min(df$value)*0.05,xend=seq(1,18,1),yend=value*0.97),
                   filter(df, df$variable == "Pot_con"),linetype = "dotted",colour = "grey20",alpha = 0.8,size=0.5) +
      geom_line(aes(y=value)) +
      scale_x_discrete(labels = Mes_toplot,expand = c(0,0.3)) +
      scale_y_continuous(breaks = pretty(c(min(df$value*0.05),df$value), n = 5),expand = c(0.01,0)) +
      scale_colour_manual(values = c("#F8766D","black"),labels=LL) +
      scale_size_manual(values = c(1.3,1.5),labels=LL) +
      scale_linetype_manual(values = c("solid","longdash"),labels=LL) +
      ylab("Potência elétrica\n[kW]") +
      expand_limits(y=max(df$value)*1.1) +
      theme_minimal(base_size = 10) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.y = element_line(),
            axis.text.x = element_text(face="bold"),
            axis.text.y = element_text(face="bold",size = 8),
            legend.title = element_blank(),
            # legend.direction = "horizontal",
            # legend.position = c(0.75,0.94),
            legend.key.height = unit(1.5,"cm"),
            legend.text = element_text(size = 7.5))
  }
  
  else if (TT == "MT") {
    
    df$variable <- factor(df$variable, levels = c("Pot_ins","Pot_min","Pot_tom","Pot_con"))
    
    ggplot(df,aes(x=factor(MesAno),group = variable,colour=variable,linetype=variable,size=variable)) +
      geom_segment(aes(x=seq(1,18,1),y=min(df$value)*0.05,xend=seq(1,18,1),yend=value*0.97),
                   filter(df, df$variable == "Pot_ins"),linetype = "dotted",colour = "grey20",alpha = 0.8,size=0.5) +
      geom_line(aes(y=value)) +
      scale_x_discrete(labels = Mes_toplot,expand = c(0,0.3)) +
      scale_y_continuous(breaks = pretty(df$value, n = 5),expand = c(0.02,0)) +
      scale_colour_manual(values = c("#00BFC4","#f7dc6f","#F8766D","black"),labels=LL) +
      scale_size_manual(values = c(1.5,1,1.3,1.5),labels=LL) +
      scale_linetype_manual(values = c("longdash","solid","solid","longdash"),labels=LL) +
      ylab("Potência elétrica\n[kW e kVA]") +
      expand_limits(y=max(df$value)*1.1) +
      theme_minimal(base_size = 10) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.y = element_line(),
            axis.text.x = element_text(face="bold"),
            axis.text.y = element_text(face="bold",size = 8),
            legend.title = element_blank(),
            # legend.direction = "horizontal",
            # legend.position = c(0.5,0.98),
            legend.key.height = unit(1,"cm"),
            legend.text = element_text(size = 7.5))
  }
}

### Gráfico 4: Proporção percentual de consumo e despesa por categoria de ciclo
G4_Consumo_Anual_PHorario <- function(consumoDF, faturaDF, anoMes) 
{
  consumoDF <- subset(consumoDF, as.yearmon(consumoDF$Date, format = "%b/%Y") 
                      <= as.yearmon(anoMes, format = "%Y-%m"))
  faturaDF <- subset(faturaDF, as.yearmon(faturaDF$Date, format = "%b/%Y") 
                      <= as.yearmon(anoMes, format = "%Y-%m"))
  
  ### Filtrar dados apenas às colunas de consumo relevantes para este gráfico:
  temp = consumoDF[,1:6]
  
  #### Para utilizar dados do mês atual mais os 11 meses anteriores:
  temp = tail(temp, 12)
  
  #### Adicionar coluna de reativa com valores a 0 ao consumo para os dfs 
  #### terem as mesmas dimensoes
  temp$Reativa <- 0
  YY <- c(min(year(as.yearmon(temp$Date, format = "%b/%Y"))),
          max(year(as.yearmon(temp$Date, format = "%b/%Y"))))
  
  if (YY[1] == YY[2]) {ano_index = YY[1]} else {
    ano_index = paste0(YY[1],"/", substr(YY[2],3,4))}
  
  
  ### Filtrar dados da df fatura para aparecer so as colunas 
  ### com os valores de fatura dos periodos horarios
  faturaTemp <- faturaDF[,5:9]
  
  faturaTemp <- tail(faturaTemp, 12)  # mesmo que anterior com df temp
  
  ### Criação de df que agrega os valores dos dois df anteriores por categoria do ciclo:
  dat_agr <- data.frame(CountConsumo = c(sum(temp$P), sum(temp$C), sum(temp$VN), sum(temp$SV), sum(temp$Reativa)), 
                        CountFatura = c(sum(faturaTemp$custo_P), sum(faturaTemp$custo_C), sum(faturaTemp$custo_VN), sum(faturaTemp$custo_SV), sum(faturaTemp$custo_reativa)), 
                        category = c("Ponta","Cheia","Vazio Normal","Super Vazio","Reativa"), 
                        stringsAsFactors = FALSE)
  
  if(dat_agr$CountFatura[5] == 0 & dat_agr$CountConsumo[5] == 0) {  # Se não houver reativa
    dat_agr <- dat_agr[-5,]}                                        # retirar do df essa categoria (linha 5)
  
  ### Cálculo das percentagens por categoria:
  dat_agr$ConsumoPerc <- dat_agr$CountConsumo/sum(dat_agr$CountConsumo)
  dat_agr$FaturaPerc <- dat_agr$CountFatura/sum(dat_agr$CountFatura)
  
  ### Ordena categorias para a legenda:
  dat_agr$category <- factor(dat_agr$category, levels = dat_agr$category)
  
  CORES = c("Ponta" = "#F8766D",
            "Cheia" = "#f7dc6f",
            "Vazio Normal" = "#58d68d",
            "Super Vazio" = "#00BFC4",
            "Reativa" = "#bb8fce")
  
  # Se surgir alguma vez um zero no gráfico no consumo em reactiva descomentar:
  # dat_agr$ConsumoPerc[dat_agr$ConsumoPerc == 0] <- NA
  
  ggplot(dat_agr) +
    geom_bar(aes(x=0,y = 0, fill = category, group ="identity"), stat='identity', size=0.15, na.rm = TRUE) +
    geom_bar(aes(x=1,y = ConsumoPerc, fill = category, group ="identity"), width = 1.1, position = "fill", stat='identity', size=0.15) +
    geom_bar(aes(x=2,y = FaturaPerc, fill = category, group ="identity"),  width = 1.1, position = "fill", stat='identity', size=0.15) +
    geom_text(aes(x= 1, 
                  y=ConsumoPerc, 
                  label = ifelse(ConsumoPerc == 0,"", paste(round(ConsumoPerc*100), "%", sep = ""))), 
              size=3.5, 
              position = position_fill(vjust = 0.5), 
              na.rm = TRUE, 
              color = "black") +
    geom_text(aes(x= 2, 
                  y=FaturaPerc, 
                  label = ifelse(FaturaPerc == 0,"", paste(round(FaturaPerc*100), "%", sep = ""))), 
              angle = ifelse(dat_agr$FaturaPerc < 0.01,90,0),  
              size=3.5, 
              position = position_fill(vjust = 0.5), 
              na.rm = TRUE, 
              color = "black") +
    geom_vline(xintercept = c(0.45,1.45,2.55)) +
    annotate("text",x=-.5,y = 0, label = "Consumo", size = 3.5) +
    annotate("text",x=3,y = 0, label = "Fatura", size = 3.5) +
    coord_polar('y') +
    scale_fill_manual(aes(x=category, y=FaturaPerc), values = CORES) +
    guides(fill=guide_legend(title=ano_index)) +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(), axis.title=element_blank(),    
          axis.ticks=element_blank(), axis.text = element_blank(),
          legend.title = element_text(face="bold"),
          legend.spacing.y = unit(20,"line"),
          legend.text = element_text(size = 7.5))
}

### Gráfico 5: Perfil do preço ao longo do dia (primeiro dia útil do mês)
G5_preco_unitario <- function(df, consumoDF, faturaDF, TT, anoMes)
{
  
  df_pUnitario <- build_pUnitario(df, consumoDF, faturaDF, TT, anoMes)
  
  df_pUnitario$P = df_pUnitario$P * 1.23  # IVA 
  
  df_pUnitario$H = factor(df_pUnitario$H, levels = c("P","C","VN","SV"))
  
  CORES = c("P" = "#F8766D",
            "C" = "#f7dc6f",
            "VN" = "#58d68d",
            "SV" = "#00BFC4")
  
  LABELS = c("Ponta",
             "Cheia",
             "Vazio\nNormal",
             "Super\nVazio")
  
  ggplot(df_pUnitario, aes(x=timestamp, y = P)) +
    geom_line(size = 3, colour = "grey60", alpha = 0.4) +
    geom_point(aes(colour=H),size = 3,shape = 15) +
    annotate("text",
             x = df_pUnitario$timestamp[4],
             hjust = 0,
             y = max(df_pUnitario$P)*1.15,
             label = paste(day(df_pUnitario$timestamp[1]),
                           "de",
                           Meses_pt(df_pUnitario$timestamp[1]),
                           "de",
                           year(df_pUnitario$timestamp[1])),
             fontface="italic",size=5,alpha=.2) +
    scale_x_datetime(date_breaks = "2 hour", labels = date_format("%H:%M"), expand=c(0.01,0)) +
    scale_colour_manual(values=CORES, labels = LABELS) +
    ylab(paste0("Preço da eletricidade\nc/ IVA [EUR/kWh]")) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size = 12),
          legend.title = element_blank(),
          axis.text.x = element_text(size = 8,face="bold",angle = 90),
          axis.text.y = element_text(size = 8,face="bold"),
          legend.key.height =  unit(1, "cm"),
          legend.text = element_text(size = 7.5))
}

### Gráfico 6: Perfil de consumo mensal por categoria tarifária
G6_Perfil_Diario_Mensal <- function(df, TT, anoMes) 
{
  ano <- as.numeric(substr(anoMes,1,4))
  mes <- as.numeric(substr(anoMes,6,7))
  mes_ano_string <- paste(Meses_pt(mes),ano)
  
  c_mes <- buildConsumDF_mes(df, TT, anoMes)
  c_mes <- melt(c_mes, id=c("Day"))
  c_mes <- c_mes[with(c_mes, order(Day)),]
  
  ### Unidades inteligentes que se adaptam à dimensão dos valores:
  if (min(c_mes$value)>100) {
    c_mes$value <- round(c_mes$value/1000, digits = 2)
    str_unit <- "MWh"
  } else {
    str_unit <- "kWh"
  }
  
  CORES = c("P" = "#F8766D",
            "C" = "#f7dc6f",
            "VN" = "#58d68d",
            "SV" = "#00BFC4")
  
  LABELS = c("Ponta",
             "Cheia",
             "Vazio\nNormal",
             "Super\nVazio")
  
  c_mes$PHorario <- factor(c_mes$PHorario, levels = c("P","C","VN","SV"))
  
  ### Criar df com soma de consumos em todos os pHorarios de 1 dia
  ### para achar o maximo e expandir o limite y do graf em conformidade:
  sum_dia <- aggregate(x= c_mes["value"],by = list(Day = c_mes$Day), FUN = sum)
  
  ggplot(c_mes, aes(x=Day, y = value, width=0.8)) +
    geom_bar(aes(x=Day, y = value, fill= PHorario), stat = 'identity', position = 'stack') +
    scale_x_continuous(labels = c(paste(c(1:31))), expand = c(0,0), breaks = c(1:31)) +
    scale_y_continuous(limits = c(0,max(sum_dia$value)),breaks = pretty(c(0,sum_dia$value), n =6),expand = c(0, 0)) +
    scale_fill_manual(values=CORES, name=mes_ano_string, labels = LABELS) +
    ylab(paste0("Consumo de eletricidade\n[",str_unit,"/dia]")) +
    expand_limits(y=round(sum_dia$value)*1.2) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor.x = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size = 12),
          panel.grid.major.x = element_blank(),
          legend.title = element_text(face="bold"),
          axis.text.x = element_text(face="bold",size = 8),
          axis.text.y = element_text(face="bold",size = 8),
          legend.text = element_text(size = 7.5))
}

### Gráfico 7:  Consumo do mês em análise, por tipologia:
G7_Consumo_Mes <- function(df,M)
{
  df = df[df$Mes == M,]  # Filtra df ao mês pretendido
  
  ### Cria colunas com máximo, mínimo e média por tipologia em todas as colunas:
  for (i in 1:nrow(df)) {df$MAX[i] <- max(df[(df[,5] == df[i,5]),2])}
  for (i in 1:nrow(df)) {df$MIN[i] <- min(df[(df[,5] == df[i,5]),2])}
  
  df$USDF <- factor(df$USDF, levels = c("U","S","D","F"))  # reordena tipologias para legenda
  
  LL=c("2ª Feira","3ª Feira","4ª Feira","5ª Feira","6ª Feira","Sábado","Domingo")  # Labels eixo x
  
  if (sum(df$Consumo)>1000) {TOTAL <- round(sum(df$Consumo)/1000, digits = 2); str_unit <- "MWh"}  
  else {TOTAL <- sum(df$Consumo); str_unit <- "kWh"}
  
  ### adaptado de: 
  ### https://stackoverflow.com/questions/19643234/fill-region-between-two-loess-smoothed-lines-in-r-with-ggplot
  
  # create plot object with loess regression lines 
  g <- ggplot(df) +
    geom_smooth(aes(WDAY,Consumo),method = "loess",span=.52,se = FALSE,colour="grey50",size=.6)
  g1 <- g +
    geom_smooth(aes(WDAY,MAX),method = "loess",span=.52,se = FALSE,colour="grey60") +
    geom_smooth(aes(WDAY,MIN),method = "loess",span=.52,se = FALSE,colour="grey60")
  
  # build plot object for rendering 
  gg1 <- ggplot_build(g1)
  
  # extract data for the loess lines from the 'data' slot
  df2 <- data.frame(x = gg1$data[[1]]$x,
                    mean = gg1$data[[1]]$y,
                    ymax = gg1$data[[2]]$y,
                    ymin = gg1$data[[3]]$y) 
  
  # use the loess data to add the 'ribbon' to plot 
  g +
    geom_ribbon(data = df2, aes(x = x, ymin = ymin, ymax = ymax),
                fill = "grey80", alpha = .8) +
    geom_jitter(aes(WDAY,Consumo,colour=USDF),width = .04,size = 4,shape=18) +
    # stat_summary(aes(WDAY,Consumo),fun.y = mean, fun.ymin = min, fun.ymax = max,colour = "black",alpha=.4) +
    geom_text_repel(aes(WDAY,Consumo,
                        label = as.numeric(substr(as.character(Dia),9,10))),
                    size=2.5, force = .008, 
                    # direction = "x", 
                    nudge_x = .25,
                    fontface = 'bold',segment.color = 'grey50') +
    annotate("text",
             x = 0.65,
             y = c(first(df2$ymin),first(df2$mean),first(df2$ymax)),
             label = c("min.","med.","max."),
             alpha=0.4,size=3) +
    annotate("text",
             x = seq(1,7,1),
             y = 1.15*min(df$Consumo)-.15*max(df$Consumo),
             label = LL,
             fontface="bold",size=3,alpha=.6) +
    annotate("text",
             x = 1,
             hjust = 0,
             y = max(df$Consumo)*1.12,
             label = paste(Meses_pt(df[1,1]),year(df[1,1]),"-",TOTAL,str_unit),
             fontface="italic",size=4,alpha=.2) +
    scale_colour_manual(values = c("#f7dc6f","#00BFC4","#F8766D","#bb8fce"),
                        labels=c("Dias úteis","Sábados","Domingos","Feriados\nem dia útil")) +
    scale_y_continuous(breaks = pretty(df$Consumo, n = 6)) +
    scale_x_continuous(expand = c(0.08,0)) +
    ylab("Consumo de eletricidade\n[kWh]") +
    theme_minimal(base_size = 10) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_line(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(face="bold",size = 8),
          legend.title = element_blank(),
          legend.text = element_text(size = 7.5))
}

### Gráfico 8: Perfis diários de potência média do mês M, por tipologia:
G8_Potencia_D_USDF <- function(df)
{
  TOT_CONS = sum(df$Activa)/4
  
  dfMD = AUX_G8_Media_Dia(df)[[1]]
  LI = AUX_G8_Media_Dia(df)[[2]]   # Label do índice
  
  MIN = AUX_G8_Media_Dia(df)[[3]]
  FRAC = MIN*24*(nrow(df)/96)/TOT_CONS*100
  
  ano <- as.numeric(substr(dfMD[1,1],1,4))
  mes <- as.numeric(substr(dfMD[1,1],6,7))
  mes_ano_string <- paste(Meses_pt(mes),ano)
  
  labels = c("Dias úteis","Sábados","Domingos","Feriados\nem dia útil")
  
  LABELS = paste0(labels," (",LI,")")
  
  LABELS = c(LABELS,paste0("Mínimo\n(",MIN," kW, ",round(FRAC,0),"%)"))
  
  CORES = c("U" = "#f7dc6f",
            "S" = "#00BFC4",
            "D" = "#F8766D",
            "F" = "#bb8fce",
            "MIN" = "grey60")
  
  ggplot(dfMD,aes(timestamp,value,group=variable,colour=variable)) +
    geom_line(size = .7) +
    scale_x_datetime(date_breaks = "2 hour", labels = date_format("%H:%M"), expand = c(0.01,0)) +
    scale_colour_manual(name = mes_ano_string,
                        values = CORES,
                        labels = LABELS) +
    scale_y_continuous(limits = c(0,max(dfMD$value))) +
    theme_minimal(base_size = 10) +
    ylab("Potência elétrica\n[kW]") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_line(),
          axis.text.x = element_text(face="bold",size = 8,angle = 90),
          # axis.text.x = element_blank(),
          axis.text.y = element_text(face="bold",size = 8),
          legend.title = element_text(face="bold",size = 8),
          legend.text = element_text(size = 7.5))
}

### Gráfico 9: Perfis diários de potência (filtrado por tipologia TIP) do mês M:
G9_Potencia_D <- function(df,TIP)
{
  df[,1] = df[1:96,1]  # Instrução que faz com que todos os dias sejam dia 1 para
  # sobrepor curvas no gráfico
  
  if (TIP == "U") {  # Só dias úteis
    df = df[df$WDAY < 6,]
    NAME = "Dias úteis"
    NCOLOR = 23
    COLOR = "#f7dc6f"
    TITLE = ""} else if (TIP == "S") {
      df = df[df$WDAY == 6,]
      NAME = "Sábados"
      NCOLOR = 5
      COLOR = "#00BFC4"
      TITLE = "Potência elétrica [kW]"} else {
        df = df[df$WDAY == 7,]
        NAME = "Domingos"
        NCOLOR = 5
        COLOR = "#F8766D"
        TITLE = ""}
  
  ano <- as.numeric(substr(df[1,1],1,4))
  mes <- as.numeric(substr(df[1,1],6,7))
  mes_ano_string <- paste(Meses_pt(mes),ano)
  
  LABELS = unique(as.numeric(substr(df$Dia,9,10)))
  
  g <- ggplot(df,aes(timestamp,Activa,group=Dia,colour=factor(Dia))) + 
    geom_line(size = .7) +
    scale_x_datetime(date_breaks = "2 hour", labels = date_format("%H:%M"), expand = c(0.01,0)) +
    theme_minimal(base_size = 10) +
    scale_colour_manual(name = paste0(NAME,"\n",mes_ano_string),
                        values = colorRampPalette(c("grey70",COLOR))(NCOLOR),
                        labels = LABELS) +
    scale_y_continuous(limits = c(0,max(df$Activa)), breaks = pretty(df$Activa, 3)) +
    ylab(TITLE) +
    guides(color=guide_legend(ncol=2)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_line(),
          axis.text.x = element_text(face="bold",size = 8,angle = 90),
          axis.text.y = element_text(face="bold",size = 8),
          legend.title = element_text(face="bold",size = 8),
          legend.text = element_text(size = 7.5),
          legend.key.height = unit(0.3,"cm"),
          legend.key.width = unit(0.4,"cm")) +
    if (NAME != "Domingos") {theme(axis.text.x = element_blank())}
  
  return(g)
}

### Gráfico 12: Comparação de perfis médios mensais, mês homólogo do ano anterior:
G12_Comp_Hom <- function(df_D,df_Hom)
{
  Mes = paste(Meses_pt(df_D[1,1]),year(df_D[1,1]))
  Mes_H = paste(Meses_pt(df_Hom[1,1]),year(df_Hom[1,1]))
  
  ### Criar novo df com potências médias:
  df_D = Aux_Perfil_M(df_D)                          # Perfil médio de potência de df (Mês atual)
  df_Hom = Aux_Perfil_M(df_Hom)                      # Perfil médio de potência de df1 (Mês homólogo)
  df = cbind(df_D,df_Hom[,2])                          # Juntar num df ambos os anteriores perfis médios de potência
  colnames(df) <- c("timestamp","Media","Media_Hom")   # Atribuir nomes às colunas do df
  
  if (df_D[1,2] < df_Hom[1,2]) {                       # Se a primeira entrada atual for inferior à homóloga,
    FILLS = c("#58d68d","#F8766D")                   # começar com cor verde (diminuição)
    LABELS = c("Diminuição","Aumento")}  else {        # senão é vermelho (aumento).   
      FILLS = c("#F8766D","#58d68d")
      LABELS = c("Aumento","Diminuição")}                
  
  df = Aux_Hom(df)   # Para criar df para o plot.

  LINHAS <- c("Mês atual"="solid","Mês homólogo"="dashed")
  
  ggplot(df, aes(timestamp, ymin = Media, ymax = Media_Hom)) +
    geom_ribbon(aes(group = factor(segment),fill = factor(segment%%2)),alpha=0.7) +
    geom_ribbon(aes(ymin = 0,ymax = MIN,group=1),fill = "grey80",alpha=0.8) +
    geom_line(aes(y = Media, group = 1, linetype = "Mês atual"), size = .75) +
    geom_line(aes(y = Media_Hom, group = 2, linetype = "Mês homólogo"), size = .75) +
    scale_linetype_manual(values = LINHAS, labels = c(Mes,Mes_H)) +
    scale_fill_manual(values = FILLS, labels = LABELS) +
    scale_x_datetime(date_breaks = "2 hour", labels = date_format("%H:%M"), expand = c(0.01,0)) +
    theme_minimal(base_size = 10) +
    ylab("Potência elétrica [kW]") +
    guides(fill=guide_legend(ncol = 1,nrow=2,byrow=TRUE,order = 2),
           linetype=guide_legend(ncol = 1,nrow=2,byrow=TRUE,keywidth = unit(.8,"cm"),order = 1)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_line(),
          axis.text.x = element_text(face="bold",size = 8,angle = 90),
          axis.text.y = element_text(face="bold",size = 8),
          legend.title = element_blank(),
          legend.text = element_text(size = 7.5),
          legend.spacing.y =  unit(0, "cm"),
          legend.position = "top",
          legend.direction = "horizontal")
}

### Função auxiliar ao gráfico G8, que devolve as potências médias por tipologia,
### em long format (melt), e o número de dias por tipologia num vetor: 
AUX_G8_Media_Dia <- function(df)
{
  # Adiciona coluna com horas, minutos e segundos apenas
  df$HMS = format.Date(df[,1],"%H:%M:%S")
  
  NU = nrow(df[df$USDF == "U",])/96   # Nº de dias uteis
  NS = nrow(df[df$USDF == "S",])/96   # Nº de sábados
  ND = nrow(df[df$USDF == "D",])/96   # Nº de domingos
  NF = nrow(df[df$USDF == "F",])/96   # Nº de feriados
  NUSDF = c(NU,NS,ND,NF)              # vetor com valores anteriores, 
                                      # utilizados no gráfico G8
  
  dfU = df[df$USDF == "U",]  # df só com dias uteis
  dfS = df[df$USDF == "S",]  # df só com sábados
  dfD = df[df$USDF == "D",]  # df só com domingos
  if (NF != 0) {dfF = df[df$USDF == "F",]}  # df só com feriados, se houver
  
  ### Potência média de quarto de hora por tipologia:
  dfUM <- aggregate(x = dfU["Activa"],by = list(dfU$HMS),FUN = mean)[,2]
  dfSM <- aggregate(x = dfS["Activa"],by = list(dfS$HMS),FUN = mean)[,2]
  dfDM <- aggregate(x = dfD["Activa"],by = list(dfD$HMS),FUN = mean)[,2]
  if (NF != 0) {dfFM <- aggregate(x = dfF["Activa"],by = list(dfF$HMS),FUN = mean)[,2]}
  
  dfM = data.frame(timestamp = df[1:96,1])
  
  ### Composição de novo df com os 4 dias médios por tipologia:
  dfM <- cbind(dfM,dfUM,dfSM,dfDM)
  colnames(dfM) <- c("timestamp","U","S","D")
  if (NF != 0) {dfM <- cbind(dfM,dfFM)  # se houver feriados
  colnames(dfM) <- c("timestamp","U","S","D","F")}
  
  ### Coluna dos mínimo:
  dfM$MIN <- round(mean(c(min(dfM[,-1]),min(dfM[,2]),min(dfM[,3]),min(dfM[,4]))),0)
  
  ### Conversão para long format:
  dfML = melt(dfM, id.vars = c("timestamp"))
  
  return(list(dfML,NUSDF,dfM$MIN[1]))
}

### Função auxiliar aos gráficos G9 a G13, que devolve o perfil médio de potência: 
Aux_Perfil_M <- function(df)
{
  TS = df[1:96,1]
  dfM <- aggregate(x = df["Activa"],by = list(df$HMS),FUN = mean)
  dfM[,1] <- TS
  return(dfM)
}

### Função auxiliar que devolve df processado para gráficos dos perfis diários 
### em meses homólogos (G12):
Aux_Hom <- function(df)
{
  ### Nova coluna com o sinal (+1/-1) da diferença de potências médias:
  df$sign <- sign(df$Media_Hom - df$Media)
  
  ### Nova coluna X com as linhas posteriores à mudança de sinal (onde as curvas se intersetam):
  df$X = NA
  for (i in 2:nrow(df)) {
    if (df$sign[i] != df$sign[i-1]) {df$X[i] = i}
  }

  ### Novo data frame PTS onde serão armazenadas as coordenadas dos pontos onde as curvas 
  ### se intersetam. Este é inicializado com as linhas posteriores à mudança de sinal:
  if (length(unique(df$X))!=1) {  # Se houver pontos de intersecção
    PTS = data.frame(Linha = sort(unique(df$X)))
    
    PTS$x = NA
    PTS$y = NA
    for (j in 1:nrow(PTS)) {            # Ciclo que calcula as coordenadas dos pontos de
      y11= df[PTS[j,1]-1,"Media"]       # intersecção, com recurso a regras básicas de 
      y21= df[PTS[j,1]-1,"Media_Hom"]   # geometria linear (i.e.: y = s(x - x0) + y0)
      y12= df[PTS[j,1],"Media"]
      y22= df[PTS[j,1],"Media_Hom"]
      s1 = y12 - y11
      s2 = y22 - y21
      x = -(y11 - y21)/(s1 - s2)
      PTS$x[j] = PTS[j,1] - 1 + x       # Coordenada x
      PTS$y[j] = y11 + s1*x             # Coordenada y
    }
    
    ### As seguintes instruções acrescentam as coordenadas anteriores ao df inicial, 
    ### nas linhas respetivas, e organiza as zonas entre intersecções por segmentos:
    df$X[PTS$Linha] <- PTS$x
    df$X[-PTS$Linha] <- NA
    df$Y <- NA
    df$Y[PTS$Linha] <- PTS$y
    df$Y1 <- df$Y  # Duplicação da linha com as coordenadas y, auxiliar
    df$segment <- findInterval(row(df)[,1], c(df$X[which(!is.na(df$X))]))
    
    ### Restantes operações matriciais são auxiliares para criação do df final, adaptadas da solução proposta na
    ### seguinte página web: https://learnr.wordpress.com/2009/10/22/ggplot2-two-color-xy-area-combo-chart/
    df$X1 <- c(tail(df$X, -1), NA)
    df$Y2 <- c(tail(df$Y1, -1), NA)
    df$Y3 <- df$Y2
    
    df1 <- df[, c(1:3, 8)]
    df2 <- df[!is.na(df$X), c(5:7, 8)]
    df3 <- df[!is.na(df$X1), c(9:11, 8)]
    
    names(df2) <- names(df1)
    names(df3) <- names(df1)
    
    ### Conversão das coordenadas x numéricas em timestamp: 
    df2$timestamp <- df1[1,1]+15*60*(df2$timestamp-1)
    df3$timestamp <- df1[1,1]+15*60*(df3$timestamp-1)
    
    combo <- rbind(df1, df2)
    combo <- rbind(combo, df3)
    combo <- combo[is.finite(combo$Media), ]
    combo <- combo[order(combo$timestamp), ]
    
    ### Duas novas colunas no df final, combo, com os máximos e mínimos locais:
    combo$MIN <- pmin(combo$Media,combo$Media_Hom)
    combo$MAX <- pmax(combo$Media,combo$Media_Hom)
    
    return(combo)} else {  # Caso não haja intersecções
      df$segment = 1
      df$MIN <- pmin(df$Media,df$Media_Hom)
      df$MAX <- pmax(df$Media,df$Media_Hom)
      return(df)}
}