library(scales)
library(ggplot2)
library(readxl)
library(chron)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(reshape)
library(reshape2)
library(knitr)
library(rmarkdown)
library(bizdays)
library(ggrepel)
library(zoo)
library(bindrcpp)
library(gridExtra)
setwd("~/Projects/Observatorio LEN")
source("Meses_pt.R")



### Inserir código da instalação.
CodInst <- "MuseuJuPom"
CodInst <- "TeatSLuiz"
CIL <- "XXXXXXXXX"

### Inserir data inicial e final para a análise.
### Formato: "MM/DD/AAAA"

Dia_i <- "01/01/2017"
Dia_f <- "01/01/2018"

### Para converter os dados em formato standard:

source("DataConverter.R")
source("bte_converter_funcs.R")
source("mt_converter_funcs.R")

RegInst <- convertBTE("3874085")  #MuseuJuPom
RegInst <- convertBTE("8509886")  #BiblioCoru
RegInst <- convertMT("10311278")  #TeatSLuiz
RegInst <- convertMT("6517365")   #MUDE


RegInstMUDE <- convertMT("6517365")
RegInstDepositoPB <- convertMT("6517604")
RegInstTeatMMatos <- convertMT("6518053")
RegInstCineSJorge <- convertMT("6518567")
RegInstTeatAberto <- convertMT("8320094")
RegInstTeatSLuiz <- convertMT("10311278")

### Isto serviu para adicionar Janeiro e Fevereiro de 2018 à base de dados,
### a partir dos dados de MT do ficheiro "Dados" do R.
for (i in 1:nrow(df_ToReport)) {
  if (df_ToReport$TT[i] == "BTE") {
    RegInst = convertBTE(df_ToReport$CIL[i])
    
    RegInst <- RegInst[as.yearmon(RegInst[,1]) == "Feb 2018" |
                         as.yearmon(RegInst[,1]) == "Jan 2018",]

    InsDB(df_ToReport$CodInst[i],RegInst)
    
  }
  
  
}
df_ToReport = df_ToReport[c(-4,-5,-7),]

RegInst = convertADP(df_ToReport$CPE[24])
InsDB(df_ToReport$CodInst[24],RegInst)

RegInst = convertADP(df_ToReport$CPE[25])
InsDB(df_ToReport$CodInst[25],RegInst)


### Caso tabela não exista ainda na DB, criar usando função seguinte:
CreateTableDB(df_ToReport$CodInst[25])




# MUDE	      6517365
# DepositoPB	6517604
# TeatMMatos	6518053
# CineSJorge	6518567
# TeatAberto	8320094
# TeatSLuiz 	10311278
# MuseuJuPom	3874085
# GabEstOlis	6516815
# PalacioPim	6517030
# MuseuBorPi	6517031
# CastSaoJor	6517110
# CastSJIlum	6517111
# CastSJRest	6517112
# CastSJLuga	6517113
# PadraoDesc	6518187
# ArqFotogra	6608114
# CasaFerPes	6608773
# TeatTabord	7098362
# MuseuFado	  7540657
# MuseuMario	8377474
# GalAvIndia	8504351
# BiblioCoru	8509886
# BiblioOrlR	10113298
# ArqMunicip	10223918



### Para actualizar dados na base de dados:

source("UploadDataToDB.R")
UplDB(CodInst,RegInst_d)

### Para carregar os dados da base de dados LEN:    
### Utilizando source, atenção ao directório caso
### o script esteja numa pasta diferente.

source("ImportDatafromDB.R")
RegInst_ts = ImpDB_TS(CodInst,Dia_i,Dia_f)
RegInst = ImpDB(CodInst)
RegInst = ImpDB("CastSaoJor")
TabInst = ImpTI()

df_ToReport = data.frame(
  CodInst = TabInst[TabInst$Relatorio == 1,2],
  Gestao = TabInst[TabInst$Relatorio == 1,3],
  AreaCML = TabInst[TabInst$Relatorio == 1,4],
  CIL = TabInst[TabInst$Relatorio == 1,5],
  PI = TabInst[TabInst$Relatorio == 1,6],
  CC = TabInst[TabInst$Relatorio == 1,7],
  TT = TabInst[TabInst$Relatorio == 1,8],
  NomeInst = TabInst[TabInst$Relatorio == 1,10],
  CPE = TabInst[TabInst$Relatorio == 1,13])



### Para obter um data frame com informação agregada por mês:

source("DataProcessing.R")

### Sobre que mês se debruça este relatório (último mês com dados completos)?
M = U_MONTH(RegInst)
M = "2017-12" #formato desta data M: "AAAA-MM"
M_Hom1 = M_HOM(M)[[1]]
M_Hom12 = M_HOM(M)[[2]]

TT = "BTE" #Gama de tensão
TT = "MT"
PI = 2500 #Pot. Instalada = 2500 kVA (MT - Teatro S. Luiz)

AreaCML = "Cultura"
Gestao = "EGEAC"
NomeInst = "Teatro S. Luiz"

RegInst_AM = Agr_Mes(RegInst,TT,PI,M)

RegInst_AM_HP = Agr_Mes_HP(RegInst,TT,PI,M)

RegInst_AM12 = Agr_Mes_12Hom(RegInst,M)

RegInst_ADT = Agr_Dia_USDF(RegInst,M)

RegInst_USDF = USDF_M(RegInst,M)

RegInst_D = D_HMS_M(RegInst,M)

RegInst_D_Hom1 = D_HMS_M(RegInst,M_Hom1)

RegInst_D_Hom12 = D_HMS_M(RegInst,M_Hom12)

#--------------- JORGE --------------------#
source("Tarifas.R")

#Opcoes de ciclos: CC = "cs", "cd", "op".
newDFCD <- buildCicloDF(RegInst, CC)

consumo <- buildConsumDF(newDFCD, TT, PI)

fatura <- buildFatura(newDFCD, consumo, TT, PI)
#------------------------------------------#



### Para obter gráficos... :

source("DataVisualization.R")

G1_Consumo_Mensal(RegInst_AM_HP)

G2_Consumo_12Hom(RegInst_AM12,TT)

G3_Potencia_Mensal(RegInst_AM_HP,TT) #RegInst_AM também porque usa os mesmos dados de base

G4_Consumo_Anual_PHorario(consumo,fatura,M)

G5_preco_unitario(newDFCD,consumo,fatura,TT,M)

G6_Perfil_Diario_Mensal(newDFCD,TT,M)

G7_Consumo_Mes(RegInst_ADT,M)

G8_Potencia_D_USDF(RegInst_USDF)

G9 = G9_Potencia_D(RegInst_USDF,"U")  #Gráfico 9: Perfis de potência diários dos dias úteis

G10 = G9_Potencia_D(RegInst_USDF,"S") #Gráfico 10: Perfis de potência diários dos Sábados

G11 = G9_Potencia_D(RegInst_USDF,"D")  #Gráfico 11: Perfis de potência diários dos Domingos

grid.arrange(G9,G10,G11)

G12_Comp_Hom(RegInst_D,RegInst_D_Hom1)  #Gráfico 12: Mês homólogo anterior

G12_Comp_Hom(RegInst_D,RegInst_D_Hom12) #Gráfico 13: Mês homólogo um ano anterior

########################
### GERAR RELATÓRIOS ###
########################

CILs = c("3874085",
         "10311278",
         "6517110")

CILs = "6517110"

CILs = c("3874085")
CILs = c("10311278")
CILs = c("8509886")
CILs = df_ToReport$CIL[24]

Ano_Mes = c("2017-03","2017-06","2017-09")


source("DoReport.R")

# Passar CILS como 1º argumento, caso se pretenda definir para que 
# instalações os relatórios são gerados, caso contrário serão gerados 
# relatórios para todas as instalações.
# 
# Passar Ano-mês como 2º argumento no formato "AAAA-MM", caso se 
# pretenda relatório para mês(es) especifico(s). Caso contrário relatório 
# é gerado para o último mês completo presente nos dados respectivos 
# à instalação. Vários meses usar vector com comando c()
DoReport(CILs,Ano_Mes = "2017-12")



rmarkdown::render("Relatorio_GR.Rmd")
###

################
### FORECAST ###
################


source("DataProcessing.R")
source("Prediction benchmarking.R")
source("ImportDatafromDB.R")


RegInst = ImpDB("CastSaoJor");
MAT_CastSaoJor = PBM(RegInst)

RegInst = ImpDB("TeatSLuiz");
MAT_TeatSLuiz = PBM(RegInst)

RegInst = ImpDB("MuseuJuPom");
MAT_MuseuJuPom = PBM(RegInst)

RegInst = ImpDB("ETAFoTelha");
MAT_ETAFoTelha = PBM(RegInst)
RegInst = ImpDB("EEOlivais");
MAT_EEOlivais = PBM(RegInst)
RegInst = ImpDB("EEVilaFran");
MAT_EEVilaFran = PBM(RegInst)

ListMAT = list()
for (i in 6:23) {
  RegInst = ImpDB(df_ToReport$CodInst[i]);
  ListMAT[[i]] = PBM(RegInst)
}
ListMAT[[]] 
#

###

defineLegalH()  # horário verão inverno...

rm(list = ls())
