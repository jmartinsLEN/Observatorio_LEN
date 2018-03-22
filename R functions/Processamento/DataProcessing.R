
### Função que agrega dados de consumo e potência associados aos 
### últimos dois anos e acrescenta uma previsão dos meses restantes 
### do ano actual:
Agr_Mes <- function(df,TT,PI,M) #df - data frame; TT - Tensão; PI - Pot. Instalada (MT); M - mês do relatório
{
  ### Para criar uma nova coluna no df com o registo do mês:
  df$MesAno <- as.yearmon(cut(df$timestamp, breaks = "month"))
  
  ### Para cortar até ao mês M para o qual se pretende o relatório:
  CM = as.yearmon(M)
  df = df[!(df$MesAno > CM),]
  
  ### Para criar uma nova coluna no df com a energia consumida, em kWh:
  df$Consumo <- df$Activa / 4
  
  ### Para obter os meses e anos que figuram no df (data frame):
  MesAno_ts <- unique(df$MesAno)
  Anos <- year(MesAno_ts)
  
  ### Para criar novo df que agrega informação mensal, df_AM:
  df_AM = data.frame(Ano=Anos,
                     MesAno=MesAno_ts, 
                     Consumo=rowsum(df$Consumo, df$MesAno))
  
  ### Para adicionar coluna ao df_AM com 
  ### Potência de tomada (máx. mensal):
  df_AM$Pot_tom <- aggregate(x = df["Activa"], 
                             by = list(month = df$MesAno), 
                             FUN = max)[,2]
  
  ### Para adicionar coluna ao df_AM com potência contratada, se BTE:
  df_AM$Pot_con <- 0
  
  if (TT == "BTE") {
    for (i in 1:nrow(df_AM))
      if (i <= 12)  {df_AM$Pot_con[i] <- max(41.41,df_AM$Pot_tom[1:i])}
    else {df_AM$Pot_con[i] <- max(41.41,df_AM$Pot_tom[(i-11):i])}
  }
  
  if (TT == "MT") {
    PM = PI*0.93*0.5 # Potência Mínima
    for (i in 1:nrow(df_AM))
      if (i <= 12)  {df_AM$Pot_con[i] <- max(PM,df_AM$Pot_tom[1:i])}
    else {df_AM$Pot_con[i] <- max(PM,df_AM$Pot_tom[(i-11):i])}
    
    df_AM$Pot_ins <- PI
    df_AM$Pot_min <- PM
  }
  return(df_AM)
}

Agr_Mes_HP <- function(df,TT,PI,M) #df - data frame; TT - Tensão; PI - Pot. Instalada (MT); M - mês do relatório
{  
  df_AM = Agr_Mes(df,TT,PI,M)
  
  ### Criar df com histórico ultimos 2 anos mais o presente até mês actual: 
  HIST <- df_AM[df_AM$Ano %in% c(last(df_AM$Ano)-2,
                                 last(df_AM$Ano)-1,
                                 last(df_AM$Ano)), ]
  
  ### Quantos meses faltam (MF) a este ano e quantos anos anteriores (AA) existem? 
  MF = 12-month(last(HIST[,2]))
  AA = ceiling((nrow(HIST)-(12-MF))/12)  
  
  # if (nrow(HIST) > 24) {MF = 36 - nrow(HIST); AA = 2}
  # else if (nrow(HIST) > 12 & nrow(HIST) <= 24) {MF = 24 - nrow(HIST); AA = 1}
  # else {MF = 12 - month(last(df_AM$MesAno)); AA = 0}
  
  
  
  ### Criar df para previsão do restante ano actual:
  PREV <- data.frame(matrix(nrow = MF, ncol = ncol(HIST)))
  names(PREV) <- names(HIST)
  
  ### Caso exista previsão, cria vector Meses_anos_mis com anos e meses em 
  ### português para meses incluídos na previsão (restantes do ano):
  if (MF != 0) {
    PREV[,2] <- last(HIST$MesAno) + seq(1/12,MF/12,1/12)
    PREV[,1] <- last(HIST$Ano)
    
    PM = month(HIST[1,2])
    
    # AQUI INSERIR ALGORITMO DE PREVISÃO
    if (AA != 0) {PREV[,3] <- forecast(HIST,AA,MF,PM)}
    else {PREV[,3] <- last(HIST[,3])}
    PREV[,4] <- last(HIST[,4])
    PREV[,5] <- last(HIST[,5])
    if (TT == "MT") {
      PREV[,6] <- last(HIST[,6])
      PREV[,7] <- last(HIST[,7])
    }
  }
  
  ### Agrega ambos os data data frames (histórico e previsão) num só:
  df_AM_HP <- rbind(HIST,PREV)
  
  ### Criar vector que identifica se entrada é 
  ### Histórico, H = -1, ou Previsão, P = 1 (auxiliar para gráfico G1):
  HoP <- vector(mode = "character",length = nrow(df_AM_HP))
  HoP[1:nrow(HIST)] <- -1
  if (MF != 0) {HoP[(nrow(HIST)+1):nrow(df_AM_HP)] <- 1}
  
  ### Criar vector com consumo do último mês histórico
  ### e primeiro mês de previsão (auxiliar para gráfico G1):
  Trans <- vector(mode = "numeric",length = nrow(df_AM_HP))
  if (MF != 0) {Trans[nrow(HIST):(nrow(HIST)+1)] <- 
    df_AM_HP[nrow(HIST):(nrow(HIST)+1),3]}
  
  ### Criar vector que identifica (1) último mês histórico
  ### e primeiro mês de previsão (auxiliar para gráfico G1):
  GAP <- vector(mode = "character",length = nrow(df_AM_HP))
  if (MF != 0) {GAP[nrow(HIST):(nrow(HIST)+1)] <- 1}
  
  ### Agregar os 3 vectores anteriores ao data frame final:
  df_AM_HP <- cbind(df_AM_HP,HoP,Trans,GAP)
  
  return(df_AM_HP)
}


forecast <- function(HIST,AA,MF,PM) {
  
  # EXP = (1/(1.5^seq(1,MF,1)))
  # PREV = (HIST[(12*AA-MF-PM+2):(12*AA-PM+1),3]*(1-EXP)+last(HIST[,3])*EXP)
  
  Y = last(HIST[,1])
  SA = rowsum(HIST$Consumo,HIST$Ano)
  
  a = 0.62   # há um ano
  b = 0.10   # há um mês
  c = 0   # há dois meses
  d = 0   # há três meses
  e = 0.005   # total anual há dois anos
  f = 0.01   # total anual há um ano
  g = 0    # OPT # rácio entre somatório anual até mês em questão e período homólogo
  
  # Previsão óptima combinada entre TeatSLuiz, CastSaoJor e MuseuJuPom:
  # a = 0.48   # há um ano
  # b = 0.18   # há um mês
  # c = 0.08   # há dois meses
  # d = 0.05   # há três meses
  # e = 0.005   # total anual há dois anos
  # f = 0.01   # total anual há um ano
  # g = 0.35    # OPT # rácio entre somatório anual até mês em questão e período homólogo
  
  # Previsão perfeita para 2017 em casos em que consumo anual aumentou (TeatSLuiz, CastSaoJor)
  # Não tinha o coeficiente "e".
  # a = 0.62   # há um ano
  # b = 0.1   # há um mês
  # c = 0.005   # total anual há dois anos
  # d = 0.01   # total anual há um ano
  
  
  PREV = rep(0,MF)
  HP = c(HIST[,3],PREV)
  
  # RM = c(28.25,31,30,31,30,31,31,30,31,30,31)/(365.25/12)
  
  for (i in 1:MF) {
    
    PREV[i] = a * HIST[(12*AA-MF-PM+1+i),3]
    
    
    PREV[i] = PREV[i] + b * HP[length(HP)-MF-1+i]
    
    PREV[i] = PREV[i] + c * HP[length(HP)-MF-2+i]
    
    PREV[i] = PREV[i] + d * HP[length(HP)-MF-3+i]
    
    if (AA == 2) {
      PREV[i] = (PREV[i] + e * SA[1]/(13-PM)*12 +
                   f * SA[2])
    }
    if (AA == 1) {
      PREV[i] = PREV[i] + f * SA[1]/(13-PM)*12
    }
    
    # PREV[i] = PREV[i] * (1 + g * (sum(HP[(length(HP)-11):(length(HP)-MF-1+i)])/
    #   sum(HP[(length(HP)-23):(length(HP)-12-MF-1+i)]) - 1))
    
    PREV[i] = PREV[i] * (1 + g * (sum(HP[(length(HP)-MF-3+i):(length(HP)-MF-1+i)])/
                                    sum(HP[(length(HP)-15-MF+i):(length(HP)-13-MF+i)]) - 1))
    
    # PREV[i] = PREV[i] * RM[11-MF+i]
    
    HP[length(HP)-MF+i] = PREV[i]
    
    
  }
  
  # PREV = PREV * (1 + g * (sum(HP[(length(HP)-11):(length(HP)-MF)])/
  #                           sum(HP[(length(HP)-23):(length(HP)-12-MF)]) - 1))
  
  # PREV = PREV * (1 + g * (sum(HP[(length(HP)-MF-2):(length(HP)-MF)])/
  #                           sum(HP[(length(HP)-MF-12):(length(HP)-MF-14)]) - 1))
  
  return(PREV)
}



### Função que agrega dados de consumo por dia com tipologia:
Agr_Dia_USDF <- function(df,M)
{
  ### Para criar uma nova coluna no df com a energia consumida, em kWh:
  df$Consumo <- df$Activa / 4
  
  ### Para criar uma nova coluna no df com o registo do dia:
  df$Dia <- as.Date(cut(df$timestamp, breaks = "day"))
  
  ### Para cortar ao mês M para o qual se pretende o relatório:
  df = df[!(substr(df$Dia,1,7) > M),]
  
  ### Para obter os dias (sem repetições) que figuram no df (data frame):
  Dias_ts <- unique(df$Dia)
  
  ### Para criar novo df que agrega informação diária, df_agg_d:
  df_agg_d = data.frame(Dia=Dias_ts, 
                        Consumo=rowsum(df$Consumo, df$Dia))
  df_agg_d$Mes <- format.Date(cut(df_agg_d$Dia, breaks = "month"), format = "%Y-%m")
  
  ### Criar vector coluna com o weekday (1 - 2ª feira, 7 - Domingo):
  Wdy <- wday(df_agg_d$Dia,week_start = 1)
  
  ### Criar vector coluna com TRUE/FALSE se for feriado:
  source("Calendar.R")
  Fer <- isFeriado(df_agg_d$Dia)
  
  ### Adicionar ao df coluna com tipologia USDF:
  USDF <- vector(mode = "character",length = length(Wdy))
  USDF[(Wdy < 6) & (Fer == FALSE)] <- "U"  #Dias úteis
  USDF[(Wdy < 6) & (Fer == TRUE)] <- "F"   #Feriados
  USDF[(Wdy == 6)] <- "S"                  #Sábados
  USDF[(Wdy == 7)] <- "D"                  #Domingos
  df_agg_d$USDF <- USDF
  df_agg_d$WDAY <- Wdy
  
  return(df_agg_d)
}

### Função que agrega dados de consumo "normalizados "associados 
### aos últimos 12 meses e 12 meses homólogos anteriores:
Agr_Mes_12Hom <- function(df,M)
{
  df_agg_d <- Agr_Dia_USDF(df,M)
  
  ### Agregar Consumo por tipologia primeiro e depois por mês:
  AUX <- aggregate(x = df_agg_d["Consumo"], 
                   by = list(df_agg_d$USDF,df_agg_d$Mes), 
                   FUN = sum)
  
  ### Agregar Dias (Nr.) por tipologia primeiro e depois por mês:
  AUX1 <- aggregate(x = df_agg_d["Dia"], 
                    by = list(df_agg_d$USDF,df_agg_d$Mes), 
                    FUN = length)
  
  ### Criar df com informação agregada por mês e tipologia:
  df_agg_m_t = data.frame(Mes=AUX$Group.2,                 #Mes (AAAA-MM)
                          USDF=AUX$Group.1,                #Tipologia USDF
                          Count=AUX1$Dia,                  #Count - Nr. de dias por tipologia
                          Consumo=AUX$Consumo,             #Consumo por mês e tipologia
                          Pot_m=AUX$Consumo/(AUX1$Dia*24)) #Potência média por mês e tipologia (1 dia - 24 horas)
  
  ### Para filtrar o df anterior apenas aos 24 últimos meses (12 últimos
  ### + 12 homólogos anteriores é preciso criar um vector com os 
  ### meses presentes no df (sem repetição):
  M_unique = unique(as.character(df_agg_m_t$Mes))
  
  ### Vector com os últimos 24 meses (12 últimos + 12 homólogos anteriores):
  M_12Hom = M_unique[(length(M_unique)-23):length(M_unique)]
  
  ### Df filtrado com os dados apenas para os últimos 24 meses:
  df_agg_m_t_12Hom = df_agg_m_t[df_agg_m_t$Mes %in% M_12Hom,]
  
  ### Calcular a média de dias por tipologia nestes 24 meses (4 valores):
  Count_t = rowsum(df_agg_m_t_12Hom$Count, df_agg_m_t_12Hom$USDF)/24 #24 meses
  
  ### Atribuir estes 4 valores médios a cada tipologia no df criado:
  df_agg_m_t_12Hom[,"Count_m"] = NA
  df_agg_m_t_12Hom[(df_agg_m_t_12Hom$USDF == "D"),6] = Count_t[1]
  df_agg_m_t_12Hom[(df_agg_m_t_12Hom$USDF == "F"),6] = Count_t[2]
  df_agg_m_t_12Hom[(df_agg_m_t_12Hom$USDF == "S"),6] = Count_t[3]
  df_agg_m_t_12Hom[(df_agg_m_t_12Hom$USDF == "U"),6] = Count_t[4]
  
  ### Aqui é calculado consumo médio "normalizado" (kWh) sobre 24 meses 
  ### por mês e por tipologia, que é o número de horas (dias x 24 h) médio
  ### multiplicado pela potência média desse mês (kW):
  df_agg_m_t_12Hom[,"Consumo_m"] = df_agg_m_t_12Hom[,5]*df_agg_m_t_12Hom[,6]*24
  
  ### Para obter um vector com os meses em PT, para o eixo x dos gráficos:
  source("Meses_pt.R") 
  #Meses_str <- Meses_pt(as.Date(paste0(M_12Hom,"-01")))
  #Meses_vec <- format.Date(paste0(M_12Hom,"-01"),format="%Y-%m")
  Meses_vec <- month(as.Date(paste0(M_12Hom,"-01")))
  
  ### Vector que identifica se estamos no ano presente (1) ou homólogo anterior (-1)
  PoH = vector(); PoH[13:24] = "-1"; PoH[1:12] = "1"
  
  ### De seguida, agregam-se consumos "normalizados" por tipologia para obter
  ### os consumos "médios"normalizados" mensais finais (kWh):
  df_agg_m_12Hom = data.frame(Mes=Meses_vec,
                              PoH=PoH,
                              Consumo=rowsum(df_agg_m_t_12Hom$Consumo, df_agg_m_t_12Hom$Mes),
                              Consumo_m=rowsum(df_agg_m_t_12Hom$Consumo_m, df_agg_m_t_12Hom$Mes))
  
  return(df_agg_m_12Hom)
}

### Função que associa tipologia aos dados de potência em quarto horário, para mês M:
USDF_M <- function(df,M)
{
  df <- Corta_Mes(df,M)
  
  df = df[,1:2]
  
  ### Criar vector coluna com o weekday (1 - 2ª feira, 7 - Domingo):
  Wdy <- wday(df$timestamp,week_start = 1)
  
  ### Criar vector coluna com TRUE/FALSE se for feriado:
  source("Calendar.R")
  Fer <- isFeriado(df$timestamp)
  
  ### Adicionar ao df coluna com tipologia USDF:
  USDF <- vector(mode = "character",length = length(Wdy))
  USDF[(Wdy < 6) & (Fer == FALSE)] <- "U"  #Dias úteis
  USDF[(Wdy < 6) & (Fer == TRUE)] <- "F"   #Feriados
  USDF[(Wdy == 6)] <- "S"                  #Sábados
  USDF[(Wdy == 7)] <- "D"                  #Domingos
  df$USDF <- USDF
  df$WDAY <- Wdy
  
  df$Dia <- as.Date(cut(df$timestamp, breaks = "day"))
  df$Sem <- week(df$Dia)
  
  return(df)
}

### Função que separa dia de horas, minutos e segundos para os dados de potência 
### em quarto horário, para mês M:
D_HMS_M <- function(df,M)
{
  df <- Corta_Mes(df,M)
  
  df = df[,1:2]
  df$Dia <- as.Date(cut(df$timestamp, breaks = "day"))
  
  df$HMS <- format.Date(df$timestamp,"%H:%M:%S")

  return(df)
}

### Função que devolve último mês completo (i.e., tem todos os quarto horários)
### presente no data frame convertido ao formato standard, RegInst:
U_MONTH <- function(df)
{
  return(format(as.yearmon(last(df[,1])), "%Y-%m"))
  
  # UM = as.numeric(format(last(df[,1]),"%m"))     # Último mês (mês da última entrada no df)
  # UMS = paste0("0",UM)    # Último mês em string
  # MAS = paste0("0",UM-1)  # Mês anterior em string
  # UA = as.numeric(format(last(df[,1]),"%Y"))     # Último ano (ano da última entrada no df)
  # {if (UM != 12) PM = UM + 1 else if (UM == 12) PM = 1}  # Próximo mês
  # UQM = as.POSIXlt(format.Date(paste0(UA,"-",PM,"-01"), "%Y-%m-%d %H:%M:%S"))-15*60   # Último dia do último mês, às 23:45.
  # if (last(df[,1]) == UQM)  # Se o último dia no df for igual ao último dia desse mês...
  # { return(paste0(UA,"-",substr(UMS,nchar(UMS)-1,nchar(UMS))))  # Devolve mês da última entrada do df
  # } else
  # { {if (UM != 1) return(paste0(UA,"-",substr(MAS,nchar(MAS)-1,nchar(MAS)))) else if (UM == 1) return(paste0(UA-1,"-",12))}  # Devolve mês anterior
  # }
}

### Função que devolve os meses homólogos anterior e um ano anterior, com base no mês M:
M_HOM <- function(M)
{
  return(list(format(as.yearmon(M)-1/12, "%Y-%m"),
              format(as.yearmon(M)-1, "%Y-%m")))
  
  # ### Mês homólogo anterior:
  # Nr_M = as.numeric(substr(M,6,7))
  # Nr_M_Hom1 = paste0("0",as.character(Nr_M - 1))
  # if (Nr_M != 1) {
  #   M_Hom1 = paste0(substr(M,1,5),substr(Nr_M_Hom1,nchar(Nr_M_Hom1)-1,nchar(Nr_M_Hom1)))
  # } else {
  #   M_Hom1 = paste0(as.numeric(substr(M,1,4))-1,"-12")
  # }
  # 
  # ### Mês homólogo do ano anterior:
  # M_Hom12 = paste0(as.numeric(substr(M,1,4))-1,substr(M,5,7))
  # 
  # return(list(M_Hom1,M_Hom12))
}

### Função que corta o df com quarto horários apenas ao mês pretendido:
Corta_Mes <- function(df,M)
{
  df <- df[as.yearmon(df[,1]) == as.yearmon(M),]
  
  # ### Para criar uma nova coluna auxiliar no df com o registo do mês:
  # df$Meses <- as.Date(cut(df$timestamp, breaks = "month"))
  # 
  # ### Filtra e de seguida remove a coluna auxiliar com mês:
  # df <- df[df$Meses == paste0(M,"-01"),]
  # df <- df[,1:4]
}