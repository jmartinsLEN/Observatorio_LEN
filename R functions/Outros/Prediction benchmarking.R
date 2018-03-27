
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


#_______________________________________________________________________________#

# M = "2017-12"
# HP = Agr_Dia_USDF(RegInst,M)
# HP$AA[8:nrow(HP)] = HP$Consumo[1:(nrow(HP)-7)]
# HP$BB[15:nrow(HP)] = HP$Consumo[1:(nrow(HP)-14)]
# HP$CC[22:nrow(HP)] = HP$Consumo[1:(nrow(HP)-21)]
# HP$DD[29:nrow(HP)] = HP$Consumo[1:(nrow(HP)-28)]
# HP$USDF[HP$USDF == "U"] = 1
# HP$USDF[HP$USDF == "S"] = 2
# HP$USDF[HP$USDF == "D"] = 3
# HP$USDF[HP$USDF == "F"] = 4
# HP$USDF = as.numeric(HP$USDF)
# 
# # install library
# install.packages("neuralnet")
# 
# # load library
# library(neuralnet)
# 
# set.seed(800)
# data <- HP[29:nrow(HP),c(-1,-3:-5)]
# apply(data,2,function(x) sum(is.na(x)))
# 
# index <- seq(1,500,1)
# index <- sample(1:nrow(data),round(0.6*nrow(data)))
# train <- data[index,]
# test <- data[-index,]
# lm.fit <- glm(Consumo~., data=train)
# summary(lm.fit)
# pr.lm <- predict(lm.fit,test)
# RMSE.lm <- sqrt(sum((pr.lm - test$Consumo)^2)/nrow(test))
# 
# 
# 
# maxs <- apply(data, 2, max)
# mins <- apply(data, 2, min)
# 
# scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
# 
# train_ <- scaled[index,]
# test_ <- scaled[-index,]
# 
# n <- names(train_)
# f <- as.formula(paste("Consumo ~", paste(n[!n %in% "Consumo"], collapse = " + ")))
# nn <- neuralnet(f,data=train_,hidden=1,linear.output=T)
# 
# plot(nn)
# 
# pr.nn <- neuralnet::compute(nn,test_[,2:3])
# 
# pr.nn_ <- pr.nn$net.result*(max(data$Consumo)-min(data$Consumo))+min(data$Consumo)
# test.r <- (test_$Consumo)*(max(data$Consumo)-min(data$Consumo))+min(data$Consumo)
# 
# RMSE.nn <- sqrt(sum((test.r - pr.nn_)^2)/nrow(test_))
# 
# print(paste(RMSE.lm,RMSE.nn))
# 
# PREV <- as.data.frame(pr.nn_)
# PREV$V2 <- test.r
# PREV$E_NN <- ( PREV$V1 - PREV$V2 ) / PREV$V2 * 100
# PREV$LM <- pr.lm
# PREV$E_LM <- ( PREV$LM - PREV$V2 ) / PREV$V2 * 100
# 
# #____________________________________________________#
# 
# 
# 
# M = "2017-12"
# HP = Agr_Mes_HP_v2(RegInst,M)
# 
# HP$AA[13:nrow(HP)] = HP$Consumo[1:(nrow(HP)-12)]
# 
# HP$BB[2:nrow(HP)] = HP$Consumo[1:(nrow(HP)-1)]
# 
# HP$CC[3:nrow(HP)] = HP$Consumo[1:(nrow(HP)-2)]
# 
# HP$DD[4:nrow(HP)] = HP$Consumo[1:(nrow(HP)-3)]
# 
# HP$EE = NA; HP$EE[13] = 0; HP[14:24,8] = cumsum(HP$Consumo[13:23]); HP$EE[25] = 0; HP[26:36,8] = cumsum(HP$Consumo[25:35])
# 
# HP$FF[4:nrow(HP)] = HP$Consumo[1:(nrow(HP)-3)] + HP$Consumo[2:(nrow(HP)-2)] + HP$Consumo[3:(nrow(HP)-1)]
# 
# 
# # install library
# install.packages("neuralnet")
# 
# # load library
# library(neuralnet)
# 
# 
# 
# 
# 
# set.seed(40)
# library(MASS)
# data <- HP[13:36,-1:-2]
# apply(data,2,function(x) sum(is.na(x)))
# 
# # index <- sample(1:nrow(data),round(0.7*nrow(data)))
# train <- data[1:18,]
# test <- data[19:24,]
# lm.fit <- glm(Consumo~., data=train)
# # summary(lm.fit)
# pr.lm <- predict(lm.fit,test)
# RMSE.lm <- sqrt(sum((pr.lm - test$Consumo)^2)/nrow(test))
# 
# 
# 
# maxs <- apply(data, 2, max)
# mins <- apply(data, 2, min)
# 
# scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
# 
# train_ <- scaled[1:18,]
# test_ <- scaled[19:24,]
# 
# n <- names(train_)
# f <- as.formula(paste("Consumo ~", paste(n[!n %in% "Consumo"], collapse = " + ")))
# nn <- neuralnet(f,data=train_,hidden=c(3,2),linear.output=T)
# 
# plot(nn)
# 
# pr.nn <- neuralnet::compute(nn,test_[,2:7])
# 
# pr.nn_ <- pr.nn$net.result*(max(data$Consumo)-min(data$Consumo))+min(data$Consumo)
# test.r <- (test_$Consumo)*(max(data$Consumo)-min(data$Consumo))+min(data$Consumo)
# 
# RMSE.nn <- sqrt(sum((test.r - pr.nn_)^2)/nrow(test_))
# 
# print(paste(RMSE.lm,RMSE.nn))
# 
# PREV <- as.data.frame(pr.nn_)
# PREV$V2 <- test.r
# PREV$E_NN <- ( PREV$V1 - PREV$V2 ) / PREV$V2 * 100
# PREV$LM <- pr.lm
# PREV$E_LM <- ( PREV$LM - PREV$V2 ) / PREV$V2 * 100
# 
# #__________________________________________________________________________________________#

PBM <- function(RegInst) {
  M = "2017-12"
  RegInst_AM_HP = Agr_Mes_HP_v2(RegInst,M)
  
  SM = RegInst_AM_HP$Consumo[(nrow(RegInst_AM_HP)-10):nrow(RegInst_AM_HP)]
  
  SA = rowsum(RegInst_AM_HP$Consumo,RegInst_AM_HP$Ano)[3]
  
  MAT = data.frame(matrix(ncol = 12, nrow = 11))
  names(MAT) = c(months(seq(as.Date("2020/2/1"), by = "month", length.out = 11), abbreviate = TRUE),"Total")
  
  for (i in 1:11) {
    
    M = as.yearmon("2016-12")+i/12
    rownames(MAT)[i] = as.character(M)
    
    HP = Agr_Mes_HP_v2(RegInst,M)
    
    MAT[i,i:11] = (HP$Consumo[(nrow(HP)-11+i):nrow(HP)] / SM[i:11] - 1)*100
    
    MAT[i,12] = (rowsum(HP$Consumo,HP$Ano)[3] / SA - 1)*100
    
    
  }
  MAT = round(MAT,2)
  return(MAT)
}


Agr_Mes_HP_v2 <- function(df,M) #df - data frame; M - mês do relatório
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
  
  ### Criar df com histórico ultimos 2 anos mais o presente até mês actual: 
  HIST <- df_AM[df_AM$Ano %in% c(last(df_AM$Ano)-2,
                                 last(df_AM$Ano)-1,
                                 last(df_AM$Ano)), ]
  
  ### Quantos meses faltam (MF) a este ano e quantos anos anteriores (AA) existem? 
  MF = 12 - month(last(HIST[,2]))
  AA = ceiling((nrow(HIST) - (12 - MF)) / 12)
  
  # if (nrow(HIST) > 24) {MF = 36 - nrow(HIST); AA = 2}
  # else if (nrow(HIST) > 12 & nrow(HIST) <= 24) {MF = 24 - nrow(HIST); AA = 1}
  # else {MF = 12 - month(last(df_AM$MesAno)); AA = 0}
  
  
  
  ### Criar df para previsão do restante ano actual:
  PREV <- data.frame(matrix(nrow = MF, ncol = 3))
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
    
  }
  ### Agrega ambos os data data frames (histórico e previsão) num só:
  df_AM_HP <- rbind(HIST,PREV)
  
  return(df_AM_HP)
}