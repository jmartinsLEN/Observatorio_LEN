library(bizdays)
library(lubridate)

#Funcao que calcula data da pascoa segundo ano especificado no argumento da funcao
data_Pascoa <- function(ano) {
  a <- ano %% 19
  b <- ano %/% 100
  c <- ano %% 100
  d <- b %/% 4
  e <- b %% 4
  f <- (b + 8) %/% 25
  g <- (b - f + 1) %/% 3
  h <- (19 * a + b - d - g + 15) %% 30
  i <-  c %/% 4
  k <- c %% 4
  l <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- (a + 11 * h + 22 * l) %/% 451
  mes <- (h + l - 7 * m + 114) %/% 31
  dia <- ((h + l - 7 * m + 114) %% 31) + 1
  
  data <- as.Date(paste(ano, "-", mes, "-", dia, sep = ""))
  
  return(data)
}

#Calcula data da sexta feira santa no ano especificado no argumento da funcao
data_SextaSanta <- function(ano) {
  data <- as.Date(data_Pascoa(ano))-2
  return(as.Date(data))
}

#Calcula data do feriado de corpo de Cristo no ano especificado no argumento da funcao
data_CorpusChristi <- function(ano) {
  data <- as.Date(data_Pascoa(ano))+60

  if(wday(data) != 5) {
    diff <- 5 - wday(data)
    data <- as.Date(data) + diff
  }
  
  return(as.Date(data))
}

#Calcula data do carnaval no ano especificado no argumento da funcao
data_Carnaval <- function(ano) {
  data <- as.Date(data_Pascoa(ano))-47
  
  if(wday(data) != 3) {
    diff <- 3 - wday(data)
    data <- as.Date(data) + diff
  }  

  return(as.Date(data))
}

#Retorna vector que contem feriados no ano especificado
datas_feriados <- function(ano) {
  feriados <- c()
  
  #ano novo
  feriados <- append(feriados, as.Date(paste(ano, "-", "01-01", sep = "")))
  #liberdad
  feriados <- append(feriados, as.Date(paste(ano, "-", "04-25", sep = "")))
  #trabalhador
  feriados <- append(feriados, as.Date(paste(ano, "-", "05-01", sep = "")))
  #portugal
  feriados <- append(feriados, as.Date(paste(ano, "-", "06-10", sep = "")))
  #sto antonio(opcional)
  feriados <- append(feriados, as.Date(paste(ano, "-", "06-13", sep = "")))
  #Assuncao nossa sra
  feriados <- append(feriados, as.Date(paste(ano, "-", "08-15", sep = "")))
  #impl republica
  feriados <- append(feriados, as.Date(paste(ano, "-", "10-05", sep = "")))
  #todos os santos
  feriados <- append(feriados, as.Date(paste(ano, "-", "11-01", sep = "")))
  #rest independ
  feriados <- append(feriados, as.Date(paste(ano, "-", "12-01", sep = "")))
  #imac conceicao
  feriados <- append(feriados, as.Date(paste(ano, "-", "12-08", sep = "")))
  #Natal
  feriados <- append(feriados, as.Date(paste(ano, "-", "12-25", sep = "")))
  
  #Pascoa
  feriados <- append(feriados, data_Pascoa(ano))
  #Sexta Santa
  feriados <- append(feriados, data_SextaSanta(ano))
  #Carnaval(opcional)
  feriados <- append(feriados, data_Carnaval(ano))
  #Corpo Cristo
  feriados <- append(feriados, data_CorpusChristi(ano))
  
  feriados <- feriados[order(feriados)]
  
  return(feriados)
}
  
#Funcao que determina se a data especificada no argumento corresponde a um dia de feriado
isFeriado <- function(data) {
  feriados <- c()
  YRS = unique(year(data))
  for (i in 1:length(YRS)) {
    feriados <- append(feriados, datas_feriados(YRS[i]))
  }
  return(as.Date(data) %in% feriados)
}

#Cria um calendario que contem feriados desde o ano de inicio e final especificados
#como argumento da funcao  
create_calendar <- function(anoI, anoF, calName) {
  ano <- anoI
  feriados <- c()
  while (ano <= anoF) {
    feriados <- append(feriados, datas_feriados(ano))  
    ano <- ano + 1
  }
  
  calendario <- create.calendar(calName, holidays = feriados, start.date = paste(anoI, "-01-01", sep = ""), end.date = paste(anoF, "-12-31", sep = ""), weekdays = c("saturday", "sunday"))
  
  return(calendario)
} 

# Funcao que devolve dataframe com dia de mudanca de hora 
# no ano que se pretende
#'@param ano numero inteiro correspondente ao ano
#'@param mes numero inteiro correspondete ao mes
#'@return day string com a data correspondente a mudanca de hora
getDiaMudancaHora <- function(ano, mes) {
  df <- data.frame()
  #Mudanca de hora ocorre no mes 3 e 10.
  if(mes == 10 | mes == 3) {
    #Cria dataframe com sequencia de dias no mes em questao
    df <- as.data.frame(seq(as.Date(paste(ano,"-",mes,"-","1", sep = "")), as.Date(paste(ano,"-",mes,"-","31", sep = "")), "days"))
    colnames(df) <- c("timestamp")
    #Adiciona coluna com o numero do dia da semana (1- domingo,...)
    df$DayOfWeek <- wday(df$timestamp)
    #Adiciona coluna com o numero da semana do ano 
    df$Week <- week(df$timestamp)
    #Cria subset com todos os domingos do mes
    s <- subset(df, df$DayOfWeek == 1)
    #Mudanca de hora ocorre no ultimo domingo do mes em questao
    day <- df$timestamp[which(df$Week == max(s$Week) & df$DayOfWeek == 1)]
  } else {
    day <- NA
  }
  
  return(day)
}