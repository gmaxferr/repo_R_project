
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

# ========================================================================================================================
# ====                                  MAIN USABILITY SCRIPT                                                         ====
# ========================================================================================================================

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
  vpn_sessions = vpn_sessions[2:6]
}

# View(vpn_sessions)



# ------------------------------------
# Exercício 2
# ------------------------------------


# Alínea a)
# - - - - - - 

# TOTAL - SEMPRE

all_vsrv8 = subset(vpn_sessions, Servidor=="vsrv8")
all_vsrv10 = subset(vpn_sessions, Servidor=="vsrv10")
all_vsrv16 = subset(vpn_sessions, Servidor=="vsrv16")
all_vsrv17 = subset(vpn_sessions, Servidor=="vsrv17")

# VSVR10 - 2017

startDate = joinDateTime("2017-01-01", "00:00:00")
endDate = joinDateTime("2017-12-31","23:59:00")
index = c()
cont = 1
while(cont <= nrow(all_vsrv10)){
  ini = toDate(all_vsrv10[[cont,4]])
  if(subtractDate(startDate, ini) <= 0 && subtractDate(endDate, ini) >= 0){
    index = c(index, cont)
  }
  cont = cont +1
}
vsrv10 = all_vsrv10[index,]

# VSVR16 - Marco 2017

startDate = joinDateTime("2017-03-01", "00:00:00")
endDate = joinDateTime("2017-03-31","23:59:00")
index = c()
cont = 1
while(cont <= nrow(all_vsrv16)){
  ini = toDate(all_vsrv16[[cont,4]])
  if(subtractDate(startDate, ini) <= 0 && subtractDate(endDate, ini) >= 0){
    index = c(index, cont)
  }
  cont = cont +1
}
vsrv16 = all_vsrv16[index,]

# VSVR17 - 28 Fevereiro 2017

startDate = joinDateTime("2017-02-28", "00:00:00")
endDate = joinDateTime("2017-02-28","23:59:00")
index = c()
cont = 1
while(cont <= nrow(all_vsrv17)){
  ini = toDate(all_vsrv17[[cont,4]])
  if(subtractDate(startDate, ini) <= 0 && subtractDate(endDate, ini) >= 0){
    index = c(index, cont)
  }
  cont = cont +1
}
vsrv17 = all_vsrv17[index,]







# Alínea b)
# - - - - - - 


# Inicio de Janeiro e Fim de Fevereira: utilizadas para limitar os valores
startDate = joinDateTime("2017-01-01", "00:00:00")
endDate = joinDateTime("2017-02-28","23:59:00")
index = c()
cont = 1
while(cont <= nrow(all_vsrv8)){
 
  ini = toDate(all_vsrv8[[cont,4]])
  if(subtractDate(startDate, ini) <= 0 && subtractDate(endDate, ini) >= 0){
    index = c(index, cont)
  }
  
  cont = cont +1
}

# Fazemos depois a filtragem consoante os indices encontrados que tem o valores
# que pretendemos.
vsrv8 = all_vsrv8[index,]
View(vsrv8)
print(vsrv8[vsrv8[5] == 0,])
# Calculo do tempo de operacao e numero de falhas

n_falhas = nrow(vsrv8[vsrv8[5] == 0,]) + nrow(vsrv8[vsrv8[5] == 1,])
tempo_total = subtractDate(tail(vsrv8, 1)$Data_Ini, head(vsrv8, 1)$Data_Ini)

func = n_falhas / tempo_total

# Alínea c)
# - - - - - - 




# Alínea d)
# - - - - - - 





# Alínea e)
# - - - - - - 




# Alínea f)
# - - - - - - 









