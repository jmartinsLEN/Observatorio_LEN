########################################################################
#**********************************************************************#
#*Ficheiro que contem as funcoes para fazer conversao de ficheiros BTE*#
#**********************************************************************#
########################################################################
source("Calendar.R")

#Funcao que permite inserir uma linha num determinado indice de uma Data Frame
insertRow <- function(existingDF, newrow, index) {
  existingDF[seq(index+1,nrow(existingDF)+1),] <- existingDF[seq(index,nrow(existingDF)),]
  existingDF[index,] <- newrow
  
  return(existingDF)
}

#Funcao que permite a conversao de um ficheiro BTE para uma Data Frame
#com a questao da mudanca de hora normalizada
convertBTE <- function(cil) {
  bteDF <- read.csv(paste0("./data/BTE/",cil,".csv"))
  bteDF <- head(bteDF,-4)
  bteDF$`Data.hora` <- as.POSIXct(bteDF$`Data.hora`, "%d/%m/%Y %H:%M%OS", tz="GMT")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
  bteDF$Week <- week(bteDF$`Data.hora`)
  bteDF$DayWeek <- wday(bteDF$`Data.hora`)
  
  yearI = min(year(bteDF$Data.hora))
  yearF = max(year(bteDF$Data.hora))
  
  i <- yearI
  while(i <= yearF) {
    s <- with(bteDF, subset(bteDF, month(bteDF$Data.hora) == 3 & year(bteDF$Data.hora) == i))
    diamudancahoraMarco <- getDiaMudancaHora(i,3)
    if(nrow(s) > 0 & as.POSIXct(paste0(diamudancahoraMarco, " ", "00:00:00")) %in% s$Data.hora) {
      print(diamudancahoraMarco)
      marIndex <- which(format(bteDF$Data.hora, "%d/%m/%Y") == format(as.Date(diamudancahoraMarco), "%d/%m/%Y")
                        & format(bteDF$Data.hora, "%H:%M:%S") == "00:45:00" 
                        & year(bteDF$Data.hora) == i
                        & bteDF$Week == max(s$Week) & bteDF$DayWeek == 1)
      
      newRow <- bteDF[marIndex,]
      for(j in 1:4) {
        newRow$Data.hora <- newRow$Data.hora + 15*60
        bteDF <- insertRow(bteDF, newRow, marIndex+j)
      }
    }
    i <- i+1
  }
  
  bteDF <- bteDF[!duplicated(bteDF$Data.hora),]
  
  bteDF <- bteDF[,1:4]
  colnames(bteDF) <- c("timestamp","Activa","Indutiva","Capacitiva")
  row.names(bteDF) <- 1:nrow(bteDF)
  
  ### Remove ?ltimo m?s se incompleto
  if (as.POSIXlt(last(bteDF[,1]), tz = "UTC") != 
      as.POSIXlt(as.yearmon(last(bteDF[,1]))+1/12, tz = "UTC")-15*60) {
    bteDF = bteDF[as.yearmon(bteDF[,1]) != as.yearmon(last(bteDF[,1])),]
  }
  
  ### Remove primeiro m?s se incompleto
  if (as.POSIXlt(bteDF[1,1], tz = "UTC") != 
      as.POSIXlt(as.yearmon(bteDF[1,1]), tz = "UTC") &&
      as.POSIXlt(bteDF[1,1], tz = "UTC") != 
      as.POSIXlt(as.yearmon(bteDF[1,1]), tz = "UTC")+15*60) {
    bteDF = bteDF[as.yearmon(bteDF[,1]) != as.yearmon(bteDF[1,1]),]
  }

  
  return(bteDF)
}

#Funcao que permite a conversao de um ficheiro BTE para uma Data Frame
#com a questao da mudanca de hora normalizada
convertADP <- function(CPE) {
  bteDF <- read.csv(paste0("./data/BTE/",CPE,".csv"), sep = ";")
  bteDF$`times` <- as.POSIXct(bteDF$`times`, tz = "UTC")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
  bteDF$Week <- week(bteDF$`times`)
  bteDF$DayWeek <- wday(bteDF$`times`)
  
  yearI = min(year(bteDF$times))
  yearF = max(year(bteDF$times))
  
  i <- yearI
  while(i <= yearF) {
    s <- with(bteDF, subset(bteDF, month(bteDF$times) == 3 & year(bteDF$times) == i))
    diamudancahoraMarco <- getDiaMudancaHora(i,3)
    if(nrow(s) > 0 & as.POSIXct(paste0(diamudancahoraMarco, " ", "00:00:00")) %in% s$times) {
      print(diamudancahoraMarco)
      marIndex <- which(format(bteDF$times, "%d/%m/%Y") == format(as.Date(diamudancahoraMarco), "%d/%m/%Y")
                        & format(bteDF$times, "%H:%M:%S") == "00:45:00" 
                        & year(bteDF$times) == i
                        & bteDF$Week == max(s$Week) & bteDF$DayWeek == 1)
      
      newRow <- bteDF[marIndex,]
      for(j in 1:4) {
        newRow$times <- newRow$times + 15*60
        bteDF <- insertRow(bteDF, newRow, marIndex+j)
      }
    }
    i <- i+1
  }
  
  bteDF <- bteDF[!duplicated(bteDF$times),]
  
  bteDF <- bteDF[,1:4]
  colnames(bteDF) <- c("timestamp","Activa","Indutiva","Capacitiva")
  row.names(bteDF) <- 1:nrow(bteDF)
  
  ### Remove ?ltimo m?s se incompleto
  if (as.POSIXlt(last(bteDF[,1]), tz = "UTC") != 
      as.POSIXlt(as.yearmon(last(bteDF[,1]))+1/12, tz = "UTC")-15*60) {
    bteDF = bteDF[as.yearmon(bteDF[,1]) != as.yearmon(last(bteDF[,1])),]
  }
  
  ### Remove primeiro m?s se incompleto
  if (as.POSIXlt(bteDF[1,1], tz = "UTC") != 
      as.POSIXlt(as.yearmon(bteDF[1,1]), tz = "UTC") &&
      as.POSIXlt(bteDF[1,1], tz = "UTC") != 
      as.POSIXlt(as.yearmon(bteDF[1,1]), tz = "UTC")+15*60) {
    bteDF = bteDF[as.yearmon(bteDF[,1]) != as.yearmon(bteDF[1,1]),]
  }
  
  
  return(bteDF)
}
