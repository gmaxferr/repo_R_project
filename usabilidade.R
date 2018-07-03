
# ========================
# == Funcoes Auxiliares ==
# ========================

# install.packages('lubridate')
library(lubridate)

subtractDate <- function (date1, date2){
  # Retorna resultado em minutos
  #return(as.numeric(difftime(date1,date2)) * 60)
  
  diff=toDate(toString(date1))-toDate(toString(date2))
  min1 = as.numeric(getHour(toDate(toString(date1)))) * 60 + as.numeric(getMinuts(toDate(toString(date1))))
  min2 = as.numeric(getHour(toDate(toString(date2)))) * 60 + as.numeric(getMinuts(toDate(toString(date2))))
  
  if(is.na(min1) || is.na(min2)){
    timediff = 0
  }else{
    timediff = min1-min2
  }
  
  diffNum=((as.numeric(diff)*24*60)+ timediff)
  
  return(diffNum)
}

incrementDate <- function (date, minut){
  # Adiciona 'minuts' minutos a 'date'
  result = toDate(toString(date)) + minutes(minut)
  return(result)
}

getYear <- function(date){
  #[YYYY]-MM-DD HH:MM:SS
  return(substring(toString(date), 1, 4))
}
getMonth <- function(date){
  #YYYY-[MM]-DD HH:MM:SS
  return(substring(toString(date), 6, 7))
}
getDay <- function(date){
  #YYYY-MM-[DD] HH:MM:SS
  return(substring(toString(date), 9, 10))
}
getHour <- function(date){
  #YYYY-MM-DD [HH]:MM:SS
  return(substring(toString(date), 12, 13))
}
getMinuts <- function(date){
  #YYYY-MM-DD HH:[MM]:SS
  return(substring(toString(date), 15, 16))
}

joinDateTime <- function(date,time){
  # Combines a 'date' and a 'time' string into a date+time type
  return(strptime(paste(date, time, sep= " "), format = "%Y-%m-%d %H:%M", tz = "GMT"))
}

toDate <- function(date){
  return(as.POSIXct(date))
}

# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# ===============================
# ==== MAIN USABILITY SCRIPT ====
# ===============================

# Esta package permite processamento paralelo de operacoes,
# o que facilita e acelera calculos necessarios
# install.packages('doMC')
library('doMC')
registerDoMC(cores=4)

if(!file.exists("./Resources/vpn_sessions_2.csv")){
  # Leitura do ficheiro 'vpnsessions.txt'
  vpn_sessions = read.csv("./Resources/vpnsessions.txt", sep=',')
  # View(vpn_sessions)
  
  servidores = vpn_sessions[,1]
  protocolos = vpn_sessions[,2]
  datas = vpn_sessions[, 3]
  horas_ini = vpn_sessions[, 4]
  datas_ini = c()
  duracoes = vpn_sessions[, 6]
  hora_fim = c()
  
  cont = 1
  while(cont <= length(duracoes)){
    
    aux = toString(joinDateTime(datas[cont], horas_ini[cont]))
    datas_ini = c(datas_ini, aux)
    hora_fim = c(hora_fim, toString(incrementDate(aux, duracoes[cont])))
    
    cont = cont +1
  }

  vpn_sessions = data.frame(Servidor=servidores,Protocolo=protocolos, Data_Ini = datas_ini, Data_Fim = hora_fim, Duracao=duracoes )
  
  write.csv(vpn_sessions, "./Resources/vpn_sessions_2.csv")
}else {
  vpn_sessions = read.csv("./Resources/vpn_sessions_2.csv", sep=',')
}

# View(vpn_sessions)










