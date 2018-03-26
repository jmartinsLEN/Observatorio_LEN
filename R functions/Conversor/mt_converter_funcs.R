###############################################################
#*************************************************************#
#*Ficheiro que contem as funcoes para fazer conversao das MTs*#
#*************************************************************#
###############################################################
#Funcao que cria uma lista com 3 colunas (ativa, indutiva, capacitiva),
#que contem o nome dos ficheiros ordenados pelo ano e mes
fileReaderMT <- function(cil) {
  activa <- c()
  indutiva <- c()
  capacitiva <- c()
  for(file in list.files(path = paste("./data/MT/", cil, sep = ""))) {
    file_name <- strsplit(file, '.xls')[[1]][1]
    switch (strsplit(file_name, '_')[[1]][1],
            "A" = {activa <- append(activa, file)},
            "R" = {indutiva <- append(indutiva, file)},
            "F" = {capacitiva <- append(capacitiva, file)}
    )
  }
  
  instList <- list("activa" = activa, "indutiva" = indutiva, "capacitiva" = capacitiva)
  
  
  instList$activa <- instList$activa[order(as.numeric(gsub(".*_.*_|\\..*", "", instList$activa)))]
  instList$indutiva <- instList$indutiva[order(as.numeric(gsub(".*_.*_|\\..*", "", instList$indutiva)))]
  instList$capacitiva <- instList$capacitiva[order(as.numeric(gsub(".*_.*_|\\..*", "", instList$capacitiva)))]
  
  return(instList)
  
}

#Funcao que formata a data presente no nome do ficheiro (mm/yyy)
getDate <- function(fileName) {

  fileName <- strsplit(fileName,'.xls')[[1]][1]
  fileDate <- strsplit(fileName,"_")[[1]][3]
  year <- strtrim(fileDate, 4)
  month <- substr(fileDate,5,6)
  date <- paste(month, year, sep = "/")
  
  return (date)
}

#Funcao que transforma a matriz do ficheiro de uma componente energetica
#(ativa, indutiva ou capacitiva) numa coluna formatada com a datas/horas e
#correspondentes valores da componente energenica nessa data e hora
convertMtx2col <- function(currentDFrame, fileName) {
  finalDFrame <- data.frame()
  date <- getDate(fileName)
  i<-2
  while(i <= ncol(currentDFrame)) {
    tDFrame <- data.frame(with(currentDFrame, as.POSIXct(paste(currentDFrame[1,i], "/" , date , " ", currentDFrame[-1,1], sep = ""), "%d/%m/%Y %H:%M%OS", tz="GMT")))
    dataDFrame <- currentDFrame[-1,i]
    if(all(is.na(dataDFrame))) {
      i <- i+1
      next()
    }
    cFrame <- cbind(tDFrame, dataDFrame)
    finalDFrame <- rbind(finalDFrame, cFrame)
    
    
    i <-i+1
  }
  colnames(finalDFrame) <- c("V1","V2")
  
  rm(i, date)
  return(finalDFrame)
}

#Cria uma unica Data Frame que contem as colunas correspondentes as 
#componentes energeticas (Ativa e indutiva e capacitiva)
create_EnergCompColumn <- function(EnergcompDF, cil) {
  convertedDframe <- data.frame()
  
  for(mtCompFileName in EnergcompDF) {
    compDf <- read_xls(paste("./data/MT/", cil, "/", mtCompFileName, sep = ""), range="B27:CT58")
    compDf_Traspose <- as.data.frame(t(compDf))
    colnames(compDf_Traspose) <- compDf_Traspose["Dia",]
    compDf_Traspose <- cbind("Dia hora"= rownames(compDf_Traspose), compDf_Traspose, stringsAsFactors=FALSE)
    
    tempDframe <- convertMtx2col(compDf_Traspose, mtCompFileName)
    convertedDframe <- rbind.fill(convertedDframe, tempDframe)
  }
  
  return(convertedDframe)
}

#Funcao que faz a conversao de um conjunto de ficheiros MT para uma Data Frame 
#normalizada utilizando as funcoes anteriores
convertMT <- function(cil) {
  convertedMtDF <- data.frame()
  
  filesMtList <-fileReaderMT(cil)
  
  activaColumnDF <- create_EnergCompColumn(filesMtList$activa, cil)
  indutivaColumnDF <- create_EnergCompColumn(filesMtList$indutiva, cil)
  capacitivaColumnDF <- create_EnergCompColumn(filesMtList$capacitiva, cil)
  
  convertedMtDF <- activaColumnDF
  convertedMtDF <- cbind(convertedMtDF, indutivaColumnDF$V2)
  convertedMtDF <- cbind(convertedMtDF, capacitivaColumnDF$V2)
  
  colnames(convertedMtDF) <- c("timestamp", "Activa", "Indutiva", "Capacitiva")
  #convertedMtDF$`Data Hora` <- as.POSIXct(convertedMtDF$`Data Hora`, "%d/%m/%Y %H:%M%OS", tz="GMT")
  
  ### Remove a ?ltima linha do df, referente ? meia noite do dia seguinte:
  convertedMtDF <- head(convertedMtDF,-1)
  
  return(convertedMtDF)
}