##################################################################
#****************************************************************#
#*Ficheiro que contem a funcao para converter ficheiros BTE e MT*#
#****************************************************************#
##################################################################


library(readxl)
library(chron)
library(dplyr)
library(plyr)
library(lubridate)

source("R/bte_converter_funcs.R")
source("R/mt_converter_funcs.R")

#Esta funcao converte os dados provenientes de ficheiros(.cvs e xls) em dados formatados
#independentemente do seu conteudo (BTE ou MT)
converter <- function(cils) {
  btes <- c()
  mts <- c()
  for(file in list.files(path = "./data/BTEs")) {
    for(cil in cils) {
      if(cil == strsplit(file, '.csv')[[1]][1]) {
        btes <- append(btes, cil)
      }
    }
  }
  
  for(folder_name in basename(list.files(path = "./data/MTs"))) {
    for(cil in cils) {
      if(cil == folder_name) {
        mts <- append(mts, cil)
      }
    }
  }
  
  for(mt in mts) {
    #adicionar dados convertidos na base de dados
    convertMt(mt)
  }
  
  for(bte in btes) {
    #adicionar dados convertidos na base de dados
    convertBTE(bte)
  }
  
  print(paste("BTEs: ", btes))
  print(paste("MTs: ", mts))
}

#converter(c("6516815", "6517031", "6517365"))
#fileReaderMT("6517365")

dMT <- convertMT("6517365")
#dBTE <- convertBTE("6516815")