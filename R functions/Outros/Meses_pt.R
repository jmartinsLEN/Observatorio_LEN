Meses_pt<-function(TS)
{
  u <- vector(mode = "character",length = length(TS))
  
  # Obter o mês equivalente em língua portuguesa
  for (i in 1:length(TS)) {
    m=months(as.Date(TS[i]),abbr=TRUE)
    if (m=="Jan") {u[i]="Janeiro"}
    else if (m=="Feb") {u[i]="Fevereiro"}
    else if (m=="Mar") {u[i]="Março"}
    else if (m=="Apr") {u[i]="Abril"}
    else if (m=="May") {u[i]="Maio"}
    else if (m=="Jun") {u[i]="Junho"}
    else if (m=="Jul") {u[i]="Julho"}
    else if (m=="Aug") {u[i]="Agosto"}
    else if (m=="Sep") {u[i]="Setembro"}
    else if (m=="Oct") {u[i]="Outubro"}
    else if (m=="Nov") {u[i]="Novembro"}
    else if (m=="Dec") {u[i]="Dezembro"}
  }
  return(u)
}