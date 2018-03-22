source("Variaveis_tarifas.R")
source("Calendar.R")

########################################################
#******************************************************#
#********************Funcoes***************************#
#******************************************************#
########################################################

#Funcao que introduz coluna com hora legal (Inverno/Verao)
#dependendo do timestamp(data)
#'@param df dataframe normalizada(BTE/MT)
#'@return output dataframe normalizado com coluna
#'        extra correspondente a hora legal
defineLegalH <- function(df) {
  output <- df
  
  #Variaveis com ano inicial e final para achar
  #os dias de mudanca de hora, pois correspondem ao
  #dia de mudanca da hora legal
  anoI = min(year(df$timestamp))
  anoF = max(year(df$timestamp))
  
  ano <- anoI
  while(ano <= anoF) {
    marMudHora <- getDiaMudancaHora(ano, 3)
    outMudHora <- getDiaMudancaHora(ano, 10)
    
    output$HorarioLegal[with(df, year(df$timestamp) == ano & (df$timestamp < marMudHora | df$timestamp >= outMudHora))] <- "I"
    output$HorarioLegal[with(df, year(df$timestamp) == ano & (df$timestamp >= marMudHora & df$timestamp < outMudHora))] <- "V"
    
    ano <- ano+1
  }
  
  return(output)
}


#Funcao que adiciona uma coluna com o periodo horario(PHorario) ao 
#dataframe recebido
#'@param df dataframe normalizado de BTE/MT
#'@param periodoHorario string que corresponde ao periodo horario
#'             que se pretende preencher 
#'@param hora string que contem o intervalo horario que 
#'            e caracteristico do periodo horario em questao
#'@param pSemana string que indica o periodo semanal a ser
#'               calculado
#'@return output dataframe que contem coluna com periodo horario
#'               corresponte preenchido
whichCHora <- function(df, periodoHorario, hora, pSemana, hLegal) {
  df <- df[df$HorarioLegal == hLegal,]
  output <- df
  #Preenche periodo horario no periodo semanal recebido
  if(pSemana == "Segunda-Sexta") {
    switch (periodoHorario,
            'P' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, (wday(df$timestamp) >= 2 & wday(df$timestamp) <= 6)
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "P"
              
            } 
            },
            'C' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, (wday(df$timestamp) >= 2 & wday(df$timestamp) <= 6)
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "C"
              
            } 
            },
            'VN' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, (wday(df$timestamp) >= 2 & wday(df$timestamp) <= 6)
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "VN"
              
            } 
            },
            'SV' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, (wday(df$timestamp) >= 2 & wday(df$timestamp) <= 6)
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "SV"
            } 
            }
    )
  } else if(pSemana == "Sabado") {
    switch (periodoHorario,
            'P' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, wday(df$timestamp) == 7
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "P"
              
            } 
            },
            'C' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, wday(df$timestamp) == 7
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "C"
              
            } 
            },
            'VN' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, wday(df$timestamp) == 7
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "VN"
              
            } 
            },
            'SV' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, wday(df$timestamp) == 7
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "SV"
            } 
            }
    )
  } else if(pSemana == "Domingo") {
    switch (periodoHorario,
            'P' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, wday(df$timestamp) == 1
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "P"
              
            } 
            },
            'C' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, wday(df$timestamp) == 1
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "C"
              
            } 
            },
            'VN' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, wday(df$timestamp) == 1
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "VN"
              
            } 
            },
            'SV' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, wday(df$timestamp) == 1
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "SV"
            } 
            }
    )
  } else if(pSemana == "Segunda-Domingo") {
    switch (periodoHorario,
            'P' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, (wday(df$timestamp) >= 1 & wday(df$timestamp) <= 7)
                                   &format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "P"
              
            } 
            },
            'C' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df,(wday(df$timestamp) >= 1 & wday(df$timestamp) <= 7)
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "C"
              
            } 
            },
            'VN' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, (wday(df$timestamp) >= 1 & wday(df$timestamp) <= 7)
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "VN"
              
            } 
            },
            'SV' = {for(j in 1:length(hora[[1]])) {
              output$PHorario[with(df, (wday(df$timestamp) >= 1 & wday(df$timestamp) <= 7)
                                   & format(df$timestamp, "%H:%M:%S") >= strsplit(hora[[1]][j],"-")[[1]][1]
                                   & format(df$timestamp, "%H:%M:%S") < strsplit(hora[[1]][j],"-")[[1]][2])] <- "SV"
            } 
            }
    )
  }
  
  return(output)
}


#Funcao que constroi um dataframe que periodos
#horarios que caracterizam o ciclo semanal
#'@param df dataframe normalizado de BTE/MT
#'@param hLegal string que indica que horario legal
#'              se pretende considerar (Inverno ou Verao)
#'@return output dataframe que contem coluna de periodo
#'               horario preenchido no horario legal recebido
calculaCS <- function(df, hLegal) {
  output <- df
  
  if(hLegal == "I") {
    for(i in 1:length(csSegSexInv)) {
      hora <- strsplit(csSegSexInv[i],"/")
      output <- whichCHora(output, names(csSegSexInv[i]), hora, "Segunda-Sexta", hLegal)
    }
    for(i in 1:length(csSabInv)) {
      hora <- strsplit(csSabInv[i],"/")
      output <- whichCHora(output, names(csSabInv[i]), hora, "Sabado", hLegal)
    }
    for(i in 1:length(csDomInv)) {
      hora <- strsplit(csDomInv[i],"/")
      output <- whichCHora(output, names(csDomInv[i]), hora, "Domingo", hLegal)
    }
  }else if(hLegal == "V") {
    for(i in 1:length(csSegSexVer)) {
      hora <- strsplit(csSegSexVer[i],"/")
      output <- whichCHora(output, names(csSegSexVer[i]), hora, "Segunda-Sexta", hLegal)
    }
    for(i in 1:length(csSabVer)) {
      hora <- strsplit(csSabVer[i],"/")
      output <- whichCHora(output, names(csSabVer[i]), hora, "Sabado", hLegal)
    }
    for(i in 1:length(csDomVer)) {
      hora <- strsplit(csDomVer[i],"/")
      output <- whichCHora(output, names(csDomVer[i]), hora, "Domingo", hLegal)
    }
  }
  
  return(output)
}


#Funcao que constroi um dataframe que periodos
#horarios que caracterizam o ciclo opcional
#'@param df dataframe normalizado de MT
#'@param hLegal string que indica que horario legal
#'              se pretende considerar (Inverno ou Verao)
#'@return output dataframe que contem coluna de periodo
#'               horario preenchido no horario legal recebido
calculaOP <- function(df, hLegal) {
  output <- df
  
  if(hLegal == "I") {
    for(i in 1:length(opSegSexInv)) {
      hora <- strsplit(opSegSexInv[i],"/")
      output <- whichCHora(output, names(opSegSexInv[i]), hora, "Segunda-Sexta", hLegal)
    }
    for(i in 1:length(opSabInv)) {
      hora <- strsplit(opSabInv[i],"/")
      output <- whichCHora(output, names(opSabInv[i]), hora, "Sabado", hLegal)
    }
    for(i in 1:length(opDomInv)) {
      hora <- strsplit(opDomInv[i],"/")
      output <- whichCHora(output, names(opDomInv[i]), hora, "Domingo", hLegal)
    }
  }else if(hLegal == "V") {
    for(i in 1:length(opSegSexVer)) {
      hora <- strsplit(opSegSexVer[i],"/")
      output <- whichCHora(output, names(opSegSexVer[i]), hora, "Segunda-Sexta", hLegal)
    }
    for(i in 1:length(opSabVer)) {
      hora <- strsplit(opSabVer[i],"/")
      output <- whichCHora(output, names(opSabVer[i]), hora, "Sabado", hLegal)
    }
    for(i in 1:length(opDomVer)) {
      hora <- strsplit(opDomVer[i],"/")
      output <- whichCHora(output, names(opDomVer[i]), hora, "Domingo", hLegal)
    }
  }
  return(output)
}


#Funcao que constroi um dataframe com os periodos
#horarios que caracterizam o ciclo diario
#'@param df dataframe normalizado de BTE
#'@param hLegal string que indica que horario legal
#'              se pretende considerar (Inverno ou Verao)
#'@return output dataframe que contem coluna de periodo
#'               horario preenchido no horario legal recebido
calculaCD <- function(df, hLegal) {
  output <- df
  if(hLegal == "I") {
    for(i in 1:length(cdSegDomInv)) {
      hora <- strsplit(cdSegDomInv[i],"/")
      output <- whichCHora(output, names(cdSegDomInv[i]), hora, "Segunda-Domingo", hLegal)
    }
    
  }else if(hLegal == "V") {
    for(i in 1:length(cdSegDomVer)) {
      hora <- strsplit(cdSegDomVer[i],"/")
      output <- whichCHora(output, names(cdSegDomVer[i]), hora, "Segunda-Domingo", hLegal)
    }
    
  }
  return(output)
}


#Funcao que constroi um dataframe baseado no ciclo pretendido
#'@param df dataframe normalizado de BTE/MT
#'@param option string que indica qual o ciclo pretendido
#'@return output dataframe que contem coluna de periodo
#'               horario preenchido com base no ciclo horario
#'               recebido e coluna que indica se o periodo horario
#'               e Fora Vazio(TRUE) ou Vazio(FALSE)
buildCicloDF <- function(df, option) {
  output <- defineLegalH(df)
  output$DayofWeek <- wday(df$timestamp)
  switch (option,
          "cs" = {
            outputI <- calculaCS(output, "I")
            outputV <- calculaCS(output, "V")
            output <- rbind(outputI, outputV)
            output <- output[order(as.POSIXct(output$timestamp), decreasing = FALSE),]
          },
          "op" = {
            outputI <- calculaOP(output, "I")
            outputV <- calculaOP(output, "V")
            output <- rbind(outputI, outputV)
            output <- output[order(as.POSIXct(output$timestamp), decreasing = FALSE),]
          },
          "cd" = {
            outputI <- calculaCD(output, "I")
            outputV <- calculaCD(output, "V")
            output <- rbind(outputI, outputV)
            output <- output[order(as.POSIXct(output$timestamp), decreasing = FALSE),]
          }
  )
  
  output$FV[(output$PHorario == "P" | output$PHorario == "C")] <- TRUE 
  output$FV[(output$PHorario == "SV" | output$PHorario == "VN")] <- FALSE 
  
  return(output)
}


#Funcao que constroi um dataframe que contem dados de consumo
#'@param df dataframe correspondente ao ciclo pretendido
#'@param TT string que indica tensao (BTE ou MT)
#'@param PI inteiro que indica potencia instalada
#'@return consumoDF dataframe que correspondente aos dados
#'                  consumo da instalacao em questao
buildConsumDF <- function(df, TT, PI) {
  consumoDF <- data.frame()
  
  Month <- month(df$timestamp)
  Year <- year(df$timestamp)
  PHorario <- df$PHorario
  SomaAtiva <- df$Activa
  
  #Para criar data frame que agrega ativa por mes, ano e periodo
  #horario, separando a potencia por cada perido horario
  temp <- aggregate(SomaAtiva ~ Month+Year+PHorario, FUN = sum)
  temp1 <- melt(temp, c("Month","Year", "PHorario"), "SomaAtiva")
  consumoDF <- cast(temp1, Month + Year ~ PHorario)
  consumoDF <- arrange(consumoDF, Year)
  #Mudar meses de numero para abreviatura do nome
  consumoDF$Month <- month.abb[consumoDF$Month]
  #Unir nome de mes com o ano
  consumoDF <- unite(consumoDF, Date, c(Month, Year), sep = "/")
  
  #Para criar dataframe com o numero de horas de ponta
  hp <- PHorario=="P"
  HP <- aggregate(hp ~ Month+Year, FUN = sum)
  HP$hp <- HP$hp/4
  
  consumoDF$C <- consumoDF$C/4
  consumoDF$P <- consumoDF$P/4
  consumoDF$SV <- consumoDF$SV/4
  consumoDF$VN <- consumoDF$VN/4
  
  #Para criar coluna de Energia que corresponde a soma
  #das colunas referentes a energia em cada perido horario
  consumoDF$Energia <-  rowSums(consumoDF[2:5], na.rm = TRUE)
  
  consumoDF$HP <- HP$hp
  consumoDF$PHP <- round(consumoDF$P/consumoDF$HP, digits = 2)
  ### Para adicionar coluna ao consumoDF com 
  ### Pot?ncia de tomada (m?x. mensal):
  consumoDF$Pot_tom <- aggregate(x = df["Activa"], 
                                 by = list(month = substr(df$timestamp, 1, 7)), 
                                 FUN = max)[,2]
  
  ### Para adicionar coluna ao consumoDF com pot?ncia contratada:
  consumoDF$Pot_con <- 0 
  if(TT == "BTE") {
    for (i in 1:length(consumoDF$Pot_tom))
      if (i <= 12)  {
        consumoDF$Pot_con[i] <- max(41.41,consumoDF$Pot_tom[1:i])
      }
    else {
      consumoDF$Pot_con[i] <- max(41.41,consumoDF$Pot_tom[(i-11):i])
    }
  } else if(TT == "MT") {
    PM = PI*0.93*0.5 # Pot?ncia M?nima
    for (i in 1:nrow(consumoDF))
      if (i <= 12)  {
        consumoDF$Pot_con[i] <- max(PM,consumoDF$Pot_tom[1:i])
      }
    else {
      consumoDF$Pot_con[i] <- max(PM,consumoDF$Pot_tom[(i-11):i])
    }
    consumoDF$Pot_ins <- PI
    consumoDF$Pot_min <- PM
  }
  
  rm(temp,temp1)
  
  return(consumoDF)  
}


#Mes ano ex: "2017-12"
buildConsumDF_mes <- function(df, TT, anoMes) {
  mes <- as.numeric(strsplit(anoMes,"-")[[1]][2])
  ano <- as.numeric(strsplit(anoMes,"-")[[1]][1])
  consumoDF <- data.frame()
  
  consumoDF <- subset(df, month(df$timestamp) == mes & year(df$timestamp) == ano)
  Day <- day(consumoDF$timestamp)
  PHorario <- consumoDF$PHorario
  SomaConsumo <- consumoDF$Activa/4
  
  #Para criar data frame que agrega ativa por mes, ano e periodo
  #horario, separando a potencia por cada perido horario
  temp <- aggregate(SomaConsumo ~ Day+PHorario, FUN = sum)
  temp1 <- melt(temp, c("Day", "PHorario"), "SomaConsumo")
  consumoDF <- cast(temp1, Day ~ PHorario)
  
  consumoDF[is.na(consumoDF)] = 0
  
  return(consumoDF)
}


#Funcao que calcula os custos da energia reativa
#'@param df dataframe correspondente ao ciclo pretendido
#'@param TT string que indica tensao (BTE ou MT)
#'@return custo_reativa dataframe que contem custo da energia
#'                      reativa nos meses correspondentes
buildReativa <- function(df, TT) {
  #Cria df que contem coluna de Activa agregada por mes (YYYY-mm) 
  agg_activa <- aggregate(x = df["Activa"], 
                          by = list(month = substr(df$timestamp, 1, 7)), 
                          FUN = sum)
  
  #Cria df que contem coluna de Indutiva agregada por mes (YYYY-mm)
  #so somando nos periodos horarios fora do vazio
  agg_indutiva <- aggregate(x = df["Indutiva"], 
                            by = list(month = substr(df$timestamp, 1, 7), FV= df$FV),
                            FUN = sum, drop = FALSE)
  agg_indutiva <- agg_indutiva[agg_indutiva["FV"]==TRUE,]
  
  #Cria df que contem coluna de Capacitiva agregada por mes (YYYY-mm)
  #so somando nos periodos horarios dentro do vazio
  agg_capacitiva <- aggregate(x = df["Capacitiva"], 
                              by = list(month = substr(df$timestamp, 1, 7), FV= df$FV), 
                              FUN = sum)
  agg_capacitiva <- agg_capacitiva[agg_capacitiva["FV"] == FALSE,]
  
  #Cria df que contem as colunas anteriores agregadas por mes
  df_agg_mes <- data.frame(month = agg_activa["month"], Activa=agg_activa["Activa"]/4, Indutiva=agg_indutiva["Indutiva"]/4, Capacitiva=agg_capacitiva["Capacitiva"]/4)
  
  #Calcula o factor reativa(valor reativa e perdoada se for menos de 30% da activa)
  df_agg_mes <- transform(df_agg_mes, RFactor=ifelse(df_agg_mes$Indutiva > df_agg_mes$Activa*0.3, df_agg_mes$Indutiva - (df_agg_mes$Activa*0.3), 0))
  #Adiciona coluna que corresponde a 10% da Energia Activa
  df_agg_mes$ActivaPercent <- df_agg_mes$Activa*0.1
  
  #Calcula escalao de reactiva (se escalao == 1: escalao 1, se escalao == 2: 10% da activa no escalao 1 e o restante no escalao 2, 
  #se escalao >= 3: 10% da activa no escalao 1 + 10% da activa no escalao 2 e o restante no escalao 3)
  df_agg_mes$Escalao[df_agg_mes$ActivaPercent > 0] <- (df_agg_mes$RFactor%/%df_agg_mes$ActivaPercent)+1
  
  df_agg_mes <- transform(df_agg_mes, "n.1"=ifelse(df_agg_mes$Escalao == 1, round(df_agg_mes$RFactor, digits = 2), round(df_agg_mes$ActivaPercent, digits = 2)))
  df_agg_mes <- transform(df_agg_mes, "n.2"=ifelse(df_agg_mes$Escalao <= 1, 0,ifelse(df_agg_mes$Escalao == 2, round((df_agg_mes$RFactor-df_agg_mes$n.1), digits = 2),round(df_agg_mes$ActivaPercent, digits = 2))))
  df_agg_mes$n.3 <- round((df_agg_mes$RFactor-df_agg_mes$n.2-df_agg_mes$n.1), digits = 2)
  
  if(TT == "BTE") {
    #Factor multiplicativo para tarifa de reativa consoante o escalao
    factor <- c(0.33,1,2)
    Custo_Indutiva <- as.data.frame(round(t(t(df_agg_mes[,8:10])*factor), digits = 2))
    #Soma de todos os factores de indutiva e multiplicacao pela tarifa reativa do ano em questao
    Custo_IndutivaSum <- transform(Custo_Indutiva, custo = round(rowSums(Custo_Indutiva)*tar_Reativa_BTE[substr(df_agg_mes$month, 1, 4), "Indutiva"], digits =2))
    #Para calcular o custo acrescido do IVA em vigor
    Custo_IndutivaSum$custoIVA <- round(Custo_IndutivaSum$custo*1.23, digits = 2)
    #Soma de todos os factores de capacitiva e multiplicacao pela tarifa reativa do ano em questao
    Custo_Capacitiva <- data.frame(custo_capacitiva=round(df_agg_mes$Capacitiva*tar_Reativa_BTE[substr(df_agg_mes$month, 1, 4), "Capacitiva"], digits =2))
    
    reativa_custo <- data.frame(month = df_agg_mes$month, indutiva = Custo_IndutivaSum$custo, indutiva_IVA = Custo_IndutivaSum$custoIVA, capacitiva = Custo_Capacitiva$custo_capacitiva, capacitiva_IVA = round(Custo_Capacitiva$custo_capacitiva*1.23, digits = 2))
  } else if(TT == "MT") {
    #Factor multiplicativo para tarifa de reativa consoante o escalao
    factor <- c(0.33,1,3)
    Custo_Indutiva <- as.data.frame(round(t(t(df_agg_mes[,8:10])*factor), digits = 2))
    #Soma de todos os factores de indutiva e multiplicacao pela tarifa reativa do ano em questao
    Custo_IndutivaSum <- transform(Custo_Indutiva, custo = round(rowSums(Custo_Indutiva)*tar_Reativa_MT[substr(df_agg_mes$month, 1, 4), "Indutiva"], digits =2))
    #Para calcular o custo acrescido do IVA em vigor
    Custo_IndutivaSum$custoIVA <- round(Custo_IndutivaSum$custo*1.23, digits = 2)
    #Soma de todos os factores de capacitiva e multiplicacao pela tarifa reativa do ano em questao
    Custo_Capacitiva <- data.frame(custo_capacitiva=round(df_agg_mes$Capacitiva*tar_Reativa_MT[substr(df_agg_mes$month, 1, 4), "Capacitiva"], digits =2))
    
    reativa_custo <- data.frame(month = df_agg_mes$month, indutiva = Custo_IndutivaSum$custo, indutiva_IVA = Custo_IndutivaSum$custoIVA, capacitiva = Custo_Capacitiva$custo_capacitiva, capacitiva_IVA = round(Custo_Capacitiva$custo_capacitiva*1.23, digits = 2))
    
  }
  
  return(reativa_custo)
}


#Funcao que constroi um dataframe que contem dados de fatura
#e que e construido apartir dos dados de consumo
#'@param df dataframe correspondente ao ciclo pretendido
#'@param TT string que indica tensao (BTE ou MT)
#'@param PI inteiro que indica potencia instalada
#'@return fatura dataframe que correspondente aos dados
#'                  de fatura da instalacao em questao
#'                  que contem os custos da energia
#'                  (Ativa e reativa) ja com as tarifas 
#'                  e impostos incluidos
buildFatura <- function(df, consumoDF, TT, PI) {
  Sys.setlocale("LC_ALL","English")
  #Cria dataframe correspondente ao consumo da instalacao
  
  if(TT == "BTE") {
    #Verifica os periodos(1e4 ou 2e3) a que corresponde a data(mes/ano)
    #em questao para a tarifa ser aplicada corretamente para cada
    #periodo horario (P,C,VN,SV)
    consumoDF <- transform(consumoDF, custo_P = ifelse((as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Jan",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                        & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Mar",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))) 
                                                       | (as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Oct",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                          & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Dec",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))), round(consumoDF$P*(tarifComerc["P"]+tar_ativa_BTE[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "P1e4"]+impostoConsumo), digits = 2), round(consumoDF$P*(tarifComerc["P"]+tar_ativa_BTE[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "P2e3"]+impostoConsumo), digits = 2)))
    
    consumoDF <- transform(consumoDF, custo_C = ifelse((as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Jan",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                        & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Mar",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))) 
                                                       | (as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Oct",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                          & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Dec",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))), round(consumoDF$C*(tar_ativa_BTE[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "C1e4"]+impostoConsumo), digits = 2), round(consumoDF$C*(tarifComerc["C"]+tar_ativa_BTE[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "C2e3"]+impostoConsumo), digits = 2)))
    
    consumoDF <- transform(consumoDF, custo_VN = ifelse((as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Jan",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                         & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Mar",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))) 
                                                        | (as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Oct",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                           & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Dec",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))), round(consumoDF$VN*(tarifComerc["VN"]+tar_ativa_BTE[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "VN1e4"]+impostoConsumo), digits = 2), round(consumoDF$VN*(tarifComerc["VN"]+tar_ativa_BTE[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "VN2e3"]+impostoConsumo), digits = 2)))
    
    consumoDF <- transform(consumoDF, custo_SV = ifelse((as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Jan",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                         & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Mar",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))) 
                                                        | (as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Oct",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                           & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Dec",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))), round(consumoDF$SV*(tarifComerc["SV"]+tar_ativa_BTE[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "SV1e4"]+impostoConsumo), digits = 2), round(consumoDF$SV*(tarifComerc["SV"]+tar_ativa_BTE[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "SV2e3"]+impostoConsumo), digits = 2)))
    
    #Cria data com os custos da reativa
    reativa <- buildReativa(df, TT)
    
    #Cria dataframe com todos os dados de fatura
    #para ser calculado o valor total no mes correspondente
    fatura <- data.frame(Date = consumoDF$Date,
                         CAV = CAV[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "CAV"],
                         custo_PHP = round(consumoDF$PHP*custo_Potencia_BTE[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "PHP"], digits = 2), 
                         custo_PContratada = round(consumoDF$Pot_con*custo_Potencia_BTE[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "Pot_Con"], digits = 2), 
                         custo_P = consumoDF$custo_P, 
                         custo_C= consumoDF$custo_C, 
                         custo_VN = consumoDF$custo_VN, 
                         custo_SV = consumoDF$custo_SV, 
                         custo_reativa = reativa$indutiva)
    fatura$Total <- rowSums(fatura[2:9], na.rm = TRUE)
    
  } else if(TT == "MT") {
    consumoDF <- transform(consumoDF, custo_P = ifelse((as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Jan",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                        & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Mar",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))) 
                                                       | (as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Oct",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                          & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Dec",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))), round(consumoDF$P*(tarifComerc["P"]+tar_ativa_MT[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "P1e4"]+impostoConsumo), digits = 2), round(consumoDF$P*(tarifComerc["P"]+tar_ativa_MT[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "P2e3"]+impostoConsumo), digits = 2)))
    
    consumoDF <- transform(consumoDF, custo_C = ifelse((as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Jan",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                        & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Mar",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))) 
                                                       | (as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Oct",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                          & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Dec",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))), round(consumoDF$C*(tar_ativa_MT[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "C1e4"]+impostoConsumo), digits = 2), round(consumoDF$C*(tarifComerc["C"]+tar_ativa_MT[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "C2e3"]+impostoConsumo), digits = 2)))
    
    consumoDF <- transform(consumoDF, custo_VN = ifelse((as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Jan",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                         & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Mar",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))) 
                                                        | (as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Oct",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                           & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Dec",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))), round(consumoDF$VN*(tarifComerc["VN"]+tar_ativa_MT[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "VN1e4"]+impostoConsumo), digits = 2), round(consumoDF$VN*(tarifComerc["VN"]+tar_ativa_MT[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "VN2e3"]+impostoConsumo), digits = 2)))
    
    consumoDF <- transform(consumoDF, custo_SV = ifelse((as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Jan",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                         & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Mar",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))) 
                                                        | (as.yearmon(consumoDF$Date, format = "%b/%Y") >= paste("Oct",year(as.yearmon(consumoDF$Date, format = "%b/%Y")),sep=" ")
                                                           & as.yearmon(consumoDF$Date, format = "%b/%Y") <= paste("Dec",year(as.yearmon(consumoDF$Date, format = "%b/%Y")))), round(consumoDF$SV*(tarifComerc["SV"]+tar_ativa_MT[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "SV1e4"]+impostoConsumo), digits = 2), round(consumoDF$SV*(tarifComerc["SV"]+tar_ativa_MT[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "SV2e3"]+impostoConsumo), digits = 2)))
    
    
    reativa <- buildReativa(df, TT)
    
    fatura <- data.frame(Date = consumoDF$Date,
                         CAV = CAV[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "CAV"],
                         custo_PHP = round(consumoDF$PHP*custo_Potencia_MT[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "PHP"], digits = 2), 
                         custo_PContratada = round(consumoDF$Pot_con*custo_Potencia_MT[paste(year(as.yearmon(consumoDF$Date, format = "%b/%Y"))), "Pot_Con"], digits = 2), 
                         custo_P = consumoDF$custo_P, 
                         custo_C= consumoDF$custo_C, 
                         custo_VN = consumoDF$custo_VN, 
                         custo_SV = consumoDF$custo_SV, 
                         custo_reativa = reativa$indutiva)
    fatura$Total <- rowSums(fatura[2:9], na.rm = TRUE)
  }
  return(fatura)
}

### Função usada no G5:
build_pUnitario <- function(df, consumoDF, faturaDF, TT, anoMes) {
  mes <- as.numeric(strsplit(anoMes,"-")[[1]][2])
  ano <- as.numeric(strsplit(anoMes,"-")[[1]][1])
  
  df_mes <- subset(df, month(df$timestamp) == mes & year(df$timestamp) == ano)
  
  for(date in as.Date(df_mes$timestamp)) {
    if(!isFeriado(as.Date(date)) & (wday(as.Date(date)) > 1 & wday(as.Date(date)) < 7)) {
      dia <- day(as.Date(date))
      break()
    }
  }
  
  df_mes <- df_mes[day(df_mes$timestamp) == dia,]
  
  periodo <- calcula_periodo(mes)
  
  preco_HP <- round(faturaDF[as.yearmon(faturaDF$Date, format = "%b/%Y") == as.yearmon(paste0(month.abb[mes],"/",ano), format = "%b/%Y"),]$custo_PHP/
                      consumoDF[as.yearmon(consumoDF$Date, format = "%b/%Y") == as.yearmon(paste0(month.abb[mes],"/",ano), format = "%b/%Y"),]$P,digits = 4)
  
  
  if(TT == "BTE") {
    df_mes <- transform(df_mes, Preco_Unitario = ifelse((periodo == 1 | periodo == 4) & (df_mes$PHorario == "P"), 
                                                        round(tarifComerc[df_mes$PHorario]+t(tar_ativa_BTE[paste0(ano), paste0(df_mes$PHorario,"1e4")])+preco_HP+impostoConsumo, digits = 4), 
                                                        ifelse((periodo == 1 | periodo == 4) & (df_mes$PHorario != "P"),
                                                               round(tarifComerc[df_mes$PHorario]+t(tar_ativa_BTE[paste0(ano), paste0(df_mes$PHorario,"1e4")])+impostoConsumo, digits = 4), 
                                                               ifelse((periodo == 2 | periodo == 3) & (df_mes$PHorario == "P"), 
                                                                      round(tarifComerc[df_mes$PHorario]+t(tar_ativa_BTE[paste0(ano), paste0(df_mes$PHorario,"2e3")])+preco_HP+impostoConsumo, digits = 4), 
                                                                      round(tarifComerc[df_mes$PHorario]+t(tar_ativa_BTE[paste0(ano), paste0(df_mes$PHorario,"2e3")])+impostoConsumo, digits = 4)))))
  } else if(TT == "MT") {
    df_mes <- transform(df_mes, Preco_Unitario = ifelse((periodo == 1 | periodo == 4) & (df_mes$PHorario == "P"), 
                                                        round(tarifComerc[df_mes$PHorario]+t(tar_ativa_MT[paste0(ano), paste0(df_mes$PHorario,"1e4")])+preco_HP+impostoConsumo, digits = 4), 
                                                        ifelse((periodo == 1 | periodo == 4) & (df_mes$PHorario != "P"),
                                                               round(tarifComerc[df_mes$PHorario]+t(tar_ativa_MT[paste0(ano), paste0(df_mes$PHorario,"1e4")])+impostoConsumo, digits = 4), 
                                                               ifelse((periodo == 2 | periodo == 3) & (df_mes$PHorario == "P"), 
                                                                      round(tarifComerc[df_mes$PHorario]+t(tar_ativa_MT[paste0(ano), paste0(df_mes$PHorario,"2e3")])+preco_HP+impostoConsumo, digits = 4), 
                                                                      round(tarifComerc[df_mes$PHorario]+t(tar_ativa_MT[paste0(ano), paste0(df_mes$PHorario,"2e3")])+impostoConsumo, digits = 4)))))
  }
  
  df_output <- data.frame(timestamp = df_mes$timestamp, 
                          P = df_mes$Preco_Unitario,
                          H = df_mes$PHorario)
  return(df_output)
}

### Fun??o usada  no G5:
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