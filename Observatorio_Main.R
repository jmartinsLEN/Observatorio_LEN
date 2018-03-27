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
source("R functions/Outros/Meses_pt.R")
source("R Functions/Outros/Calendar.R")
setwd("~/Observatorio_R/observatorio_r")




##############################################################################
########################  IMPORTAR E CONVERTER DADOS  ########################
##############################################################################
###                                                                        ###
### Importar dados de ficheiros de telecontagem provenientes da EDP ou do  ###
### cliente. Converter dados no formato aplicável para esta aplicação.     ###
###                                                                        ###
##############################################################################
source("R functions/Conversor/bte_converter_funcs.R")
source("R functions/Conversor/mt_converter_funcs.R")

### Para converter os dados em formato standard (RegInst - Registo de consumos
### em quarto horário da instalação, utilizar CIL como input):
RegInst <- convertBTE(cil)  # Caso instalação seja BTE
RegInst <- convertMT(cil)   # Caso instalação seja MT

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

### Para converter dados provenientes da empresa ADP (utilizar CPE como input):
RegInst = convertADP(CPE)

##############################################################################




##############################################################################
#########################  LIGAÇÃO À BASE DE DADOS  ##########################
##############################################################################
###                                                                        ###
### Criar e editar tabelas na base de dados com os dados recolhidos sobre  ###
### as instalações. Carregar dados da base de dados para a aplicação.      ###
###                                                                        ###
##############################################################################
source("R functions/Ligacao DB/ImportDatafromDB.R")
source("R functions/Ligacao DB/UploadDataToDB.R")

### Para actualizar informação na base de dados:
CreateTableDB(CodInst)   # Criar nova tabela caso não exista
InsDB(CodInst,RegInst)   # Inserir novos valores na tabela
UpdDB(CodInst,RegInst)   # Actualizar valores já existentes

### Para carregar os dados da base de dados LEN:    
RegInst = ImpDB(CodInst)

### Para carregar dados numa janela temporal definida por
### uma data inicial e final (formato: "MM/DD/AAAA")
RegInst_ts = ImpDB_TS(CodInst,Dia_i,Dia_f)  

### Para carregar a tabela de instalações da base de dados:
TabInst = ImpTI()

### Para retirar dessa tabela informação relativa às instalações
### marcadas com um "x" na sua primeira coluna, indicando que 
### relatório é pretendido:
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

### Esta rotina serviu para adicionar Janeiro e Fevereiro de 2018 
### à base de dados, a partir dos dados de telecontagem BTE:
for (i in 1:nrow(df_ToReport)) {
  if (df_ToReport$TT[i] == "BTE") {
    RegInst = convertBTE(df_ToReport$CIL[i])
    RegInst <- RegInst[as.yearmon(RegInst[,1]) == "Feb 2018" |
                         as.yearmon(RegInst[,1]) == "Jan 2018",]
    InsDB(df_ToReport$CodInst[i],RegInst)
  }
}

##############################################################################




##############################################################################
#############################  PROCESSAR DADOS  ##############################
##############################################################################
###                                                                        ###
### Com base nos dados de consumo, realizar todo o tipo de tratamentos e   ###
### análises para posterior visualização e inclusão nos relatórios.        ###
###                                                                        ###
##############################################################################
source("R functions/Processamento/DataProcessing.R")

### Sobre que mês se debruça este relatório (último mês com dados completos)?
### Formato da data M: "AAAA-MM"
M = U_MONTH(RegInst)  # Devolver último mês presente nos dados.
M = "2017-12" 
M_Hom1 = M_HOM(M)[[1]]   # Mês anterior
M_Hom12 = M_HOM(M)[[2]]  # Mês homólogo

### Alguns inputs que podem ser inseridos manualmente ou retirados da base de
### dados, como descrito na secção anterior:
TT = "BTE"  # Gama de tensão
TT = "MT"
PI = 2500  # Potência Instalada = 2500 kVA (MT - Teatro S. Luiz)
AreaCML = "Cultura"
Gestao = "EGEAC"
NomeInst = "Teatro S. Luiz"

RegInst_AM = Agr_Mes(RegInst,TT,PI,M)  # Agregar por mês, histórico completo.

RegInst_AM_HP = Agr_Mes_HP(RegInst,TT,PI,M)  # Agregar por mês, histórico 3 anos e previsão.

RegInst_AM12 = Agr_Mes_12Hom(RegInst,M)  # Agregar por mês último ano e ano homólogo.

RegInst_ADT = Agr_Dia_USDF(RegInst,M)  # Agregar por dia com tipologia.

RegInst_USDF = USDF_M(RegInst,M)  # Associar tipologia aos consumos, apenas um mês.

RegInst_D = D_HMS_M(RegInst,M)  # Desagregar timestamp em dia e horário, apenas um mês.

RegInst_D_Hom1 = D_HMS_M(RegInst,M_Hom1)  # Mesmo que anterior.

RegInst_D_Hom12 = D_HMS_M(RegInst,M_Hom12)  # Mesmo que anterior.

#--------------- JORGE --------------------#
source("R functions/Processamento/Tarifas.R")

#Opcoes de ciclos: CC = "cs", "cd", "op".
newDFCD <- buildCicloDF(RegInst, CC)

consumo <- buildConsumDF(newDFCD, TT, PI)

fatura <- buildFatura(newDFCD, consumo, TT, PI)
#------------------------------------------#

##############################################################################




##############################################################################
#############################  VISUALIZAR DADOS  #############################
##############################################################################
###                                                                        ###
### Com base nos dados processados, gerar diferentes gráficos com a        ###
### informação útil acerca dos consumos de electricidade das instalações.  ###
###                                                                        ###
##############################################################################
source("R functions/Vizualizacao/DataVisualization.R")

G1_Consumo_Mensal(RegInst_AM_HP)

G2_Consumo_12Hom(RegInst_AM12,TT)

G3_Potencia_Mensal(RegInst_AM_HP,TT)

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

##############################################################################




##############################################################################
#############################  GERAR RELATÓRIOS  #############################
##############################################################################
source("R functions/Gerar relatórios/DoReport.R")

### Algumas variantes de inputs:
CILs = c("3874085","10311278","6517110")
CILs = "6517110"
CILs = "3874085"
CILs = "10311278"
CILs = "8509886"
CILs = df_ToReport$CIL[24]
Ano_Mes = c("2017-03","2017-06","2017-09")

# Passar CILS como 1º argumento, caso se pretenda definir para que 
# instalações os relatórios são gerados, caso contrário serão gerados 
# relatórios para todas as instalações.
# 
# Passar Ano-mês como 2º argumento no formato "AAAA-MM", caso se 
# pretenda relatório para mês(es) especifico(s). Caso contrário relatório 
# é gerado para o último mês completo presente nos dados respectivos 
# à instalação. Vários meses usar vector com comando c()
DoReport(CILs, Ano_Mes = "2017-12")


### Gerar um relatório com os dados carregados no Global Environment do R:
rmarkdown::render("Relatorio_GR.Rmd")

##############################################################################




##############################################################################
########################### FORECAST BENCHMARKING ############################
##############################################################################
source("R functions/Processamento/DataProcessing.R")
source("R functions/Outros/Prediction benchmarking.R")
source("R functions/Ligacao DB/ImportDatafromDB.R")


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

##############################################################################




##############################################################################

defineLegalH()  # horário verão inverno...

rm(list = ls())

##############################################################################