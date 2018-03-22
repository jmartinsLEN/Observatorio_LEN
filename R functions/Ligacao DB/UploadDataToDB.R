
### Faz update da informação na DBLEB, com timestamp.

UplDB <- function(CodInst,df)
{
  library(RODBC)
  
  con <- odbcConnect("DBLEN")
  
  # create progress bar
  pb <- winProgressBar(title = "Loading to DB...", min = 0,
                       max = length(df$timestamp), width = 300)
  
  for (i in 1:(length(df$timestamp))) {
    
    qry <- paste0("UPDATE Registodeinstalacao",CodInst," SET ",
                  "Activa=",df$Activa[i],", ",
                  "Indutiva =",df$Indutiva[i],", ",
                  "Capacitiva=",df$Capacitiva[i],
                  " WHERE timestamp = #",df$timestamp[i],"#")
    
    qry
    sqlQuery(con, qry)
    
    setWinProgressBar(pb, i, title=paste("Loading to DB...",round(i/length(df$timestamp)*100, 0),
                                         "% done"))
  }
  close(pb)
  
  odbcCloseAll()
  
  rm(con,qry,i,pb)
}

InsDB <- function(CodInst,df)
{
  library(RODBC)
  
  con <- odbcConnect("DBLEN")
  
  # create progress bar
  pb <- winProgressBar(title = "Loading to DB...", min = 0,
                       max = length(df$timestamp), width = 300)
  
  for (i in 1:(length(df$timestamp))) {
    
    qry <- paste0("INSERT INTO Registodeinstalacao",CodInst," ([timestamp], Activa, Indutiva, Capacitiva) VALUES (",
                  "#",df$timestamp[i],"#, ",
                  df$Activa[i],", ",
                  df$Indutiva[i],", ",
                  df$Capacitiva[i],")")
    
    qry
    sqlQuery(con, qry)
    
    setWinProgressBar(pb, i, title=paste("Loading to DB...",round(i/length(df$timestamp)*100, 0),
                                         "% done"))
  }
  close(pb)
  
  odbcCloseAll()
  
  rm(con,qry,i,pb)
}


CreateTableDB <- function(CodInst) {
  con <- odbcConnect("DBLEN")
  qry <- paste0("CREATE TABLE Registodeinstalacao",CodInst," ( [timestamp] datetime, Activa int, Indutiva int, Capacitiva int )")
  sqlQuery(con, qry)
  odbcCloseAll()
  rm(con)
}

### OLD! ...Faz update da informação na DBLEB, para timestamp apenas com dia no
### calendário, acrescentando quarto horário com base numa fórmula matemática.

# UplDB_woT <- function(CodInst,df)
# {
#   library(RODBC)
#   
#   con <- odbcConnect("DBLEN")
#   
#   # create progress bar
#   pb <- winProgressBar(title = "Loading to DB...", min = 0,
#                        max = length(df$timestamp), width = 300)
#   
#   
#   
#   
#   for (d in 0:(length(df$timestamp)/96)) {
#     
#     for (q in 1:96) {
#       
#       i = d*96 + q
#       t = format.Date(df$timestamp[i]+15*60*(q-1), "%m/%d/%Y %H:%M:%S")
#       
#       qry <- paste0("UPDATE Registodeinstalacao",CodInst," SET ",
#                     "Activa=",df$Activa[i],", ",
#                     "Indutiva =",df$Indutiva[i],", ",
#                     "Capacitiva=",df$Capacitiva[i],
#                     " WHERE timestamp = #",t,"#")
#       
#       sqlQuery(con, qry)
#       
#       setWinProgressBar(pb, i, title=paste("Loading to DB...",round(i/length(df$timestamp)*100, 0),
#                                            "% done"))
#     }
#   }
#   close(pb)
#   
#   odbcCloseAll()
#   
#   rm(con,qry,d,q,i,t,pb)
# }
