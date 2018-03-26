########################
### GERAR RELATÓRIOS ###
########################

#Função que gera relatórios
#'@param CILs
#'@param Ano_Mes
DoReport <- function(CILs,Ano_Mes) {
  
  options(OutDec= ",")
  
  source("ImportDatafromDB.R")
  source("DataProcessing.R")
  source("Tarifas.R")
  source("DataVisualization.R")
  source("bte_converter_funcs.R")
  source("mt_converter_funcs.R")
  
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
    stringsAsFactors = FALSE)
  
  if (missing(CILs)) {ToReport = df_ToReport}
  else {
    CILSs = paste(CILs, collapse = "|")
    ToReport = filter(df_ToReport, grepl(CILSs, CIL))}
  
  if (missing(Ano_Mes)) {N = nrow(ToReport); NAM = 1} 
  else {N = nrow(ToReport) * length(Ano_Mes); NAM = length(Ano_Mes)}
  
  pb = winProgressBar(title = paste("Gerando",N,"relatório(s)..."), min = 0,
                      max = N * 15, width = 300)
  p = 0
  
  for (i in 1:N) {
    
    p = p + 1; setWinProgressBar(pb, p)
    
    j = ceiling(i/NAM)
    
    CodInst = ToReport$CodInst[j]
    Gestao = ToReport$Gestao[j]
    AreaCML = ToReport$AreaCML[j]
    CIL = ToReport$CIL[j]
    PI = ToReport$PI[j]
    CC = ToReport$CC[j]
    TT = ToReport$TT[j]
    NomeInst = ToReport$NomeInst[j]
    
    RegInst = ImpDB(CodInst)
    
    # if (TT == "MT") {RegInst = convertMT(CIL)}
    # else if (TT == "BTE") {RegInst = convertBTE(CIL)}
    
    p = p + 1; setWinProgressBar(pb, p)
    
    if (missing(Ano_Mes)) {M = U_MONTH(RegInst)} 
    else {M = Ano_Mes[i-NAM*(j-1)]}
    M_Hom1 = M_HOM(M)[[1]]
    M_Hom12 = M_HOM(M)[[2]]
    
    p = p + 1; setWinProgressBar(pb, p)
    
    RegInst_AM = Agr_Mes(RegInst,TT,PI,M)
    p = p + 1; setWinProgressBar(pb, p)
    RegInst_AM_HP = Agr_Mes_HP(RegInst,TT,PI,M)
    p = p + 1; setWinProgressBar(pb, p)
    RegInst_AM12 = Agr_Mes_12Hom(RegInst,M)
    p = p + 1; setWinProgressBar(pb, p)
    RegInst_ADT = Agr_Dia_USDF(RegInst,M)
    p = p + 1; setWinProgressBar(pb, p)
    RegInst_USDF = USDF_M(RegInst,M)
    p = p + 1; setWinProgressBar(pb, p)
    RegInst_D = D_HMS_M(RegInst,M)
    p = p + 1; setWinProgressBar(pb, p)
    RegInst_D_Hom1 = D_HMS_M(RegInst,M_Hom1)
    p = p + 1; setWinProgressBar(pb, p)
    RegInst_D_Hom12 = D_HMS_M(RegInst,M_Hom12)
    p = p + 1; setWinProgressBar(pb, p)
    newDFCD <- buildCicloDF(RegInst,CC)
    p = p + 1; setWinProgressBar(pb, p)
    consumo <- buildConsumDF(newDFCD,TT,PI)
    p = p + 1; setWinProgressBar(pb, p)
    fatura <- buildFatura(newDFCD,consumo,TT,PI)
    p = p + 1; setWinProgressBar(pb, p)
    
    outdir = paste0(AreaCML,"_",CodInst,"_",format.Date(paste0(M,"-01"),"%Y%m"),'.pdf')
    
    ### Para gerar um relatório:
    rmarkdown::render("Relatorio_GR.Rmd",output_file = outdir)
    
    p = p + 1; setWinProgressBar(pb, p)
    
    file.rename(from = outdir, 
                to = paste0("Relatórios/",outdir))
  }
  
  options(OutDec= ".")
  
  close(pb)
}