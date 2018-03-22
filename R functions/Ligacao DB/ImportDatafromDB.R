# ### Para obter os nomes das tabelas existentes na base de dados conectada:
# 
# sqlTables(con, tableType = "TABLE")$TABLE_NAME
# 
# ### Para obter os dados das tabelas como data frames:
# 
# ## M?todo 1: sqlFetch (directo para um df)
# 
# Lista_Inst <- sqlFetch(con, "Lista de instala??es")
# #str(Lista_Inst)
# 
# #Reg_Inst_MuseuJuPom <- sqlFetch(con, "RegistodeinstalacaoMuseuJuPom")
# #str(Reg_Inst_MuseuJuPom)
# 
# ## M?todo 2: sqlQuery (usado nas fun??es abaixo)

library(RODBC)

ImpDB_TS <- function(CodInst,Dia_i,Dia_f)
{
  TS_i <- paste0(Dia_i," 00:00:00")
  TS_f <- paste0(Dia_f," 00:00:00")
  
  con <- odbcConnect("DBLEN")
  
  qry_TS <- paste0("SELECT timestamp, Activa, Indutiva, Capacitiva FROM Registodeinstalacao",CodInst," WHERE timestamp >= #",TS_i,"# AND timestamp < #",TS_f,"#")
  
  df_TS <- sqlQuery(con, qry_TS,as.is=c(TRUE,FALSE,FALSE,FALSE))
  
  df_TS$timestamp <- as.POSIXlt(df_TS$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  
  odbcCloseAll()
  
  return(df_TS)
  
  rm(con)
}

ImpDB <- function(CodInst)
{
  library(RODBC)
  
  con <- odbcConnect("DBLEN")
  
  qry <- paste0("SELECT timestamp, Activa, Indutiva, Capacitiva FROM Registodeinstalacao",CodInst)
  
  df <- sqlQuery(con, qry,as.is=c(TRUE,FALSE,FALSE,FALSE))
  df = df[order(df$timestamp),]
  
  df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  
  odbcCloseAll()
  
  return(df)
  
  rm(con)
}

ImpTI <- function()
{
  library(RODBC)
  
  con <- odbcConnect("DBLEN")
  
  qry <- "SELECT * FROM Tabeladeinstalacoes"
  
  df <- sqlQuery(con, qry,as.is=TRUE)
  
  odbcCloseAll()
  
  return(df)
  
  rm(con)
}