##################################################################
#****************************************************************#
#*Ficheiro que contem a funcao para converter ficheiros BTE e MT*#
#****************************************************************#
##################################################################


#Esta funcao converte os dados provenientes de ficheiros(.cvs e xls) em dados formatados
#independentemente do seu conteudo (BTE ou MT)

convertN <- function(cils) {
  
  source("bte_converter_funcs.R")
  source("mt_converter_funcs.R")
  
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
    #adicionar dados convertidos na base de dados (acrescentar ligação à DBLEN)
    convertMT(mt)
  }
  
  for(bte in btes) {
    #adicionar dados convertidos na base de dados (acrescentar ligação à DBLEN)
    convertBTE(bte)
  }
  
  print(paste("BTEs: ", btes))
  print(paste("MTs: ", mts))
}


