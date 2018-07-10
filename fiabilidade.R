
# ========================
# == Funcoes Auxiliares ==
# ========================

# install.packages('lubridate')
library(lubridate)

toDate <- function(date){
  
  if(length(toString(date)) <= 10 && length(toString(date)) > 8){
    result = paste(toString(date), "00:00:00", sep = " ")
    return(as.POSIXct(result))
  }
  
  return(as.POSIXct(date))
}

subtractDate <- function (date1, date2){
  # Retorna resultado em minutos
  #return(as.numeric(difftime(date1,date2)) * 60)
  date1 = toDate(toString(date1))
  date2 = toDate(toString(date2))
  diff= date1 - date2
  
  return(as.numeric(diff*60))
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


# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# ========================================================================================================================
# ====                                  MAIN FIABILITY SCRIPT                                                         ====
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

# ===== ALGUNS INDICADORES =====


# Duracao das sessoes por servidores:
all_vsrv8 = subset(vpn_sessions, Servidor=="vsrv8")
all_vsrv10 = subset(vpn_sessions, Servidor=="vsrv10")
all_vsrv11 = subset(vpn_sessions, Servidor=="vsrv11")
all_vsrv16 = subset(vpn_sessions, Servidor=="vsrv16")
all_vsrv17 = subset(vpn_sessions, Servidor=="vsrv17")

boxplot(all_vsrv8$Duracao[all_vsrv8$Duracao < 400],
        all_vsrv10$Duracao[all_vsrv10$Duracao < 400],
        all_vsrv11$Duracao[all_vsrv11$Duracao < 400],
        all_vsrv16$Duracao[all_vsrv16$Duracao < 400],
        all_vsrv17$Duracao[all_vsrv17$Duracao < 400],
        names = c("vsrv8", "vsrv10", "vsrv11", "vsrv16", "vsrv17"))

# Duracao das sessoes por protocolo:

all_pptp = subset(vpn_sessions, Protocolo=="PPTP")
all_sstp = subset(vpn_sessions, Protocolo=="SSTP")
all_softether = subset(vpn_sessions, Protocolo=="SOFTETHER")
all_openvpn_l2 = subset(vpn_sessions, Protocolo=="OPENVPN_L2")
all_openvpn_l3 = subset(vpn_sessions, Protocolo=="OPENVPN_L3")

boxplot(all_pptp$Duracao[all_pptp$Duracao < 400],
        all_sstp$Duracao[all_sstp$Duracao < 400],
        all_softether$Duracao[all_softether$Duracao < 400],
        all_openvpn_l2$Duracao[all_openvpn_l2$Duracao < 400],
        all_openvpn_l3$Duracao[all_openvpn_l3$Duracao < 400],
        names = c("PPTP", "SSTP", "SOFTETHER", "OPENVPN_L2", "OPENVPN_L3"))



# Tempo (medio) entre falhas por servidor:

FAILURE = c(0, 1)
# VSRV8

failure_index_vsrv8 = which(all_vsrv8$Duracao %in%  FAILURE)
resulting_failure_data_vsrv8 = all_vsrv8[failure_index_vsrv8,] # Error lines in data frame
time_between_failures_vsrv8 = c()

cont = 2
while(cont <= nrow(resulting_failure_data_vsrv8)){
  before = resulting_failure_data_vsrv8[[cont-1, 3]]
  after = resulting_failure_data_vsrv8[[cont, 3]]
  time_between_failures_vsrv8 = c(time_between_failures_vsrv8,
                                  subtractDate(after, before))
  cont = cont + 1
}
MTBF_vsrv8 = mean(time_between_failures_vsrv8)



# VSRV10
failure_index_vsrv10 = which(all_vsrv10$Duracao %in%  FAILURE)
resulting_failure_data_vsrv10 = all_vsrv10[failure_index_vsrv10,] # Error lines in data frame
time_between_failures_vsrv10 = c()

cont = 2
while(cont <= nrow(resulting_failure_data_vsrv10)){
  before = resulting_failure_data_vsrv10[[cont-1, 3]]
  after = resulting_failure_data_vsrv10[[cont, 3]]
  time_between_failures_vsrv10 = c(time_between_failures_vsrv10,
                                   subtractDate(after, before))
  cont = cont + 1
}
MTBF_vsrv10 = mean(time_between_failures_vsrv10)

# VSRV11

failure_index_vsrv11 = which(all_vsrv11$Duracao %in%  FAILURE)
resulting_failure_data_vsrv11 = all_vsrv11[failure_index_vsrv11,] # Error lines in data frame
time_between_failures_vsrv11 = c()

cont = 2
while(cont <= nrow(resulting_failure_data_vsrv11)){
  before = resulting_failure_data_vsrv11[[cont-1, 3]]
  after = resulting_failure_data_vsrv11[[cont, 3]]
  time_between_failures_vsrv11 = c(time_between_failures_vsrv11,
                                   subtractDate(after, before))
  cont = cont + 1
}
MTBF_vsrv11 = mean(time_between_failures_vsrv11)


# VSRV16

failure_index_vsrv16 = which(all_vsrv16$Duracao %in%  FAILURE)
resulting_failure_data_vsrv16 = all_vsrv16[failure_index_vsrv16,] # Error lines in data frame
time_between_failures_vsrv16 = c()

cont = 2
while(cont <= nrow(resulting_failure_data_vsrv16)){
  before = resulting_failure_data_vsrv16[[cont-1, 3]]
  after = resulting_failure_data_vsrv16[[cont, 3]]
  time_between_failures_vsrv16 = c(time_between_failures_vsrv16,
                                   subtractDate(after, before))
  cont = cont + 1
}
MTBF_vsrv16 = mean(time_between_failures_vsrv16)


# VSRV17

failure_index_vsrv17 = which(all_vsrv17$Duracao %in%  FAILURE)
resulting_failure_data_vsrv17 = all_vsrv17[failure_index_vsrv17,] # Error lines in data frame
time_between_failures_vsrv17 = c()

cont = 2
while(cont <= nrow(resulting_failure_data_vsrv17)){
  before = resulting_failure_data_vsrv17[[cont-1, 3]]
  after = resulting_failure_data_vsrv17[[cont, 3]]
  time_between_failures_vsrv17 = c(time_between_failures_vsrv17,
                                   subtractDate(after, before))
  cont = cont + 1
}
MTBF_vsrv17 = mean(time_between_failures_vsrv17)

# graficamente... (barplot)
MTBFs=c(MTBF_vsrv8, MTBF_vsrv10,MTBF_vsrv11, MTBF_vsrv16 ,MTBF_vsrv17)
max(MTBFs)
barplot(MTBFs, names.arg = c("vsrv8", "vsrv10", "vsrv11", "vsrv16", "vsrv17"), main = "MTBF por servidor", ylim = c(0,700))


# ==============================



# Alínea a)
# - - - - - - 

# TOTAL - SEMPRE


# all_vsrv8 = subset(vpn_sessions, Servidor=="vsrv8")
# all_vsrv10 = subset(vpn_sessions, Servidor=="vsrv10")
# all_vsrv11 = subset(vpn_sessions, Servidor=="vsrv11")
# all_vsrv16 = subset(vpn_sessions, Servidor=="vsrv16")
# all_vsrv17 = subset(vpn_sessions, Servidor=="vsrv17")


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
  cont = cont + 1
}
vsrv17 = all_vsrv17[index,]
dados_17_fev = vsrv17[c(3,5)] # duracao e data

# calculo disponibilidade
cont = 1
disponibilidades = c()
dia = 1
datas = c("2017-01-01 23:59:00", "2017-01-02 23:59:00", "2017-01-03 23:59:00", "2017-01-04 23:59:00", "2017-01-05 23:59:00", "2017-01-06 23:59:00", "2017-01-07 23:59:00", "2017-01-08 23:59:00", "2017-01-09 23:59:00", "2017-01-10 23:59:00", "2017-01-11 23:59:00", "2017-01-12 23:59:00", "2017-01-13 23:59:00", "2017-01-14 23:59:00", "2017-01-15 23:59:00", "2017-01-16 23:59:00", "2017-01-17 23:59:00", "2017-01-18 23:59:00", "2017-01-19 23:59:00", "2017-01-20 23:59:00", "2017-01-21 23:59:00", "2017-01-22 23:59:00", "2017-01-23 23:59:00", "2017-01-24 23:59:00", "2017-01-25 23:59:00", "2017-01-26 23:59:00", "2017-01-27 23:59:00", "2017-01-28 23:59:00")
while(cont <= nrow(dados_17_fev)){
  
  if(subtractDate(datas[[dia]],dados_17_fev[1,cont]))
    disp = dados_17_fev[]
  
  cont = cont + 1
}


# Alínea b)
# - - - - - -
print("===============")
print("   ALINEA B    ")
print("===============")
# ===================================================================================================
fiabilidades=c()
fiabilidades2=c()
for (i in 1:2){
  if (i==1) {
    print("Calculo para Janeiro...")
    periodo = 31*24*60
    startDate = joinDateTime("2017-01-01", "00:00:00")
    endDate = joinDateTime("2017-01-31","23:59:00")
    index = c()
    cont = 1
    while(cont <= nrow(all_vsrv8)){
      
      ini = toDate(all_vsrv8[[cont,4]])
      if(subtractDate(startDate, ini) <= 0 && subtractDate(endDate, ini) >= 0){
        index = c(index, cont)
      }
      
      cont = cont +1
    }
    dataServer = all_vsrv8[index,]
  } else {
    print("Calculo para Fevereiro...")
    periodo = 28*24*60
    startDate = joinDateTime("2017-02-01", "00:00:00")
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
    dataServer = all_vsrv8[index,]
  }
  if(i==1){
    
    #Numero de sessões
    nAcessos_1 = as.numeric(nrow(dataServer))
    
    deltaT_1 = periodo/nAcessos_1
    
    #Numero de sessões que não são falhas
    dataServerUpCount_1 = as.numeric(nrow(dataServer[dataServer$Duracao>1,]))
    tempoOperacao_1 = dataServerUpCount_1*deltaT_1
    
    #Numero de sessões que são falhas
    dataServerDownCount_1 = as.numeric(nrow(dataServer[dataServer$Duracao<=1,]))
    
    taxaMediaFalhas_1 = dataServerDownCount_1/tempoOperacao_1
    for(x in 1:120){
      fiabilidade = exp(1)^(-taxaMediaFalhas_1*x)
      fiabilidade=fiabilidade*100
      fiabilidades=c(fiabilidades,fiabilidade)
    }
    
  } else if(i==2){
    #Numero de sessões
    nAcessos_2 = as.numeric(nrow(dataServer))
    
    deltaT_2 = periodo/nAcessos_2
    
    #Numero de sessões que não são falhas
    dataServerUpCount_2 = as.numeric(nrow(dataServer[dataServer$Duracao>1,]))
    tempoOperacao_2 = dataServerUpCount_2*deltaT_2
    
    #Numero de sessões que são falhas
    dataServerDownCount_2 = as.numeric(nrow(dataServer[dataServer$Duracao<=1,]))
    
    taxaMediaFalhas_2 = dataServerDownCount_2/tempoOperacao_2
    for(x in 1:120){
      fiabilidade = exp(1)^(-taxaMediaFalhas_2*x)
      fiabilidade=fiabilidade*100
      fiabilidades2=c(fiabilidades2,fiabilidade)
    }
  }
}

tempo_total = periodo
# Taxa media de falhas Janeiro: 
taxaMediaFalhas_1 # = 0.016
# R(t) = e ^ -0.0160t
# Taxa media de falhas Fevereiro: 
taxaMediaFalhas_2 # = 0.0026
# R(t) = e ^ -0.0026t

# Total dos dois meses:

x=1:tempo_total
x2=1:tempo_total
plot(1:tempo_total,exp(-1 * taxaMediaFalhas_2 * x)*100,type="l",col="red", xlim = c(0,tempo_total), ylim = c(0,100), xlab = "t (min)", ylab = "Disponibilidade")
lines(x2,exp(-1 * taxaMediaFalhas_1 * x2)*100,col="blue")
legend(tempo_total/1.4,100,legend=c("Janeiro","Fevereiro"), col=c("blue","red"),
       lty=c(1,2,3), ncol=1)
title(main="Função de fiabilidade para o servidor vsrv8 (2 Meses)")
# Primeiras duas horas:

x=0:120
x2=0:120
plot(fiabilidades2,type="l",col="red", xlim = c(0,120), ylim = c(0,100), xlab = "t (min)", ylab = "Disponibilidade")
lines(fiabilidades,col="blue")
legend(120/1.4,100,legend=c("Janeiro","Fevereiro"), col=c("blue","red"),
       lty=c(1,2,3), ncol=1)
title(main="Função de fiabilidade para o servidor vsrv8 (2 Horas)")


# Alínea c)
# - - - - - - 

startDate = joinDateTime("2017-12-01", "00:00:01")
endDate = joinDateTime("2017-12-31","23:59:01")
index = c()
cont = 1
while(cont <= nrow(all_vsrv16)){
  ini = toDate(all_vsrv16[[cont,3]])
  if(subtractDate(startDate, ini) <= 0 && subtractDate(endDate, ini) >= 0){
    index = c(index, cont)
  }
  cont = cont +1
}
vsrv16_dez = all_vsrv16[index,]
#View(vsrv16_dez)

failure_index_vsrv16 = which(vsrv16_dez$Duracao %in%  FAILURE)
resulting_failure_data_vsrv16 = vsrv16_dez[failure_index_vsrv16,] # Error lines in data frame
#View(resulting_failure_data_vsrv16)

tempo_atividade_dez = subtractDate(tail(vsrv16_dez$Data_Ini, 1), head(vsrv16_dez$Data_Ini, 1))
tempo_falhas = nrow(resulting_failure_data_vsrv16)
taxa_falhas_vsrv16_total_dez = tempo_falhas/tempo_atividade_dez
# taxa de falhas do mes = 0.1351

taxa_falhas_vsrv16 = c()
cont = 1
tempo_dia = 1440 #minutos
n_falhas = 1
dia = 1
while(cont <= nrow(resulting_failure_data_vsrv16)){
  aux = ""
  if(dia < 10){
    aux = "0"
  }
  dia_string = paste(aux, toString(dia), sep ="")
  data_ini = toDate(paste("2017-12-", " 00:00:01", sep = dia_string))
  data_end = toDate(paste("2017-12-", " 23:59:01", sep = dia_string))
  data = resulting_failure_data_vsrv16[[cont,3]]
  if(subtractDate(data_ini, data) <= 0 && subtractDate(data_end, data) >= 0){
    n_falhas = n_falhas + 1
  }else{
    taxa = n_falhas/tempo_dia
    taxa_falhas_vsrv16 = c(taxa_falhas_vsrv16, taxa)
    dia = dia + 1
    n_falhas = 1
    cont = cont -1
  }
  
  if(cont == nrow(resulting_failure_data_vsrv16)){
    taxa = n_falhas/tempo_dia
    taxa_falhas_vsrv16 = c(taxa_falhas_vsrv16, taxa)
  }
  cont = cont + 1
}

# grafico taca de falhas por dia
barplot(taxa_falhas_vsrv16, ylim=c(0, 0.025), ylab ="Taxa de Falha", xlab="Dia", names.arg = 1:31)

# teste a media:
# amostra grande (31 dias) -> Dist. normal (segundo o TLC) -> logo podemos utilizar o t.test
#
# H0: media = 0.01
# H1: media < 0.01

t.test(taxa_falhas_vsrv16, alternative = "less", conf.level = 0.95, mu = 0.01)
# p-value = 3.778e-05  < 0.05, logo, rejeitamos H0.
# Dest forma, temos evidencia estatistica que nos permite concluir,
# com um nivel de significancia de 5%, que a taxa media de falhas e
# inferior a 0.01

# Alínea d)
# - - - - - - 

# Teste as medias da taxa de falhas dos dois servidores vsrv10 e vsrv17
# no mes de dezembro do ano de 2017
# Amostra grande (31 dias -> segundo o TLC temos uma dist. normal, logo podemos utilizar o t.test)
#
# H0: media_vsrv17 = media_vsrv10
# H1: media_vsrv17 != media_vsrv10


startDate = joinDateTime("2017-12-01", "00:00:01")
endDate = joinDateTime("2017-12-31","23:59:01")
index = c()
cont = 1
while(cont <= nrow(all_vsrv17)){
  ini = toDate(all_vsrv17[[cont,3]])
  if(subtractDate(startDate, ini) <= 0 && subtractDate(endDate, ini) >= 0){
    index = c(index, cont)
  }
  cont = cont +1
}
vsrv17_dez = all_vsrv17[index,]

#View(vsrv17_dez)
cont = 1
index = c()
while(cont <= nrow(all_vsrv10)){
  ini = toDate(all_vsrv10[[cont,3]])
  if(subtractDate(startDate, ini) <= 0 && subtractDate(endDate, ini) >= 0){
    index = c(index, cont)
  }
  cont = cont +1
}
vsrv10_dez = all_vsrv10[index,]


failure_index_vsrv17 = which(vsrv17_dez$Duracao %in%  FAILURE)
resulting_failure_data_vsrv17 = vsrv17_dez[failure_index_vsrv17,] # Error lines in data frame
#View(resulting_failure_data_vsrv17)

failure_index_vsrv10 = which(vsrv10_dez$Duracao %in%  FAILURE)
resulting_failure_data_vsrv10 = vsrv10_dez[failure_index_vsrv10,] # Error lines in data frame
#View(resulting_failure_data_vsrv10)

tempo_atividade_dez = subtractDate(tail(vsrv17_dez$Data_Ini, 1), head(vsrv17_dez$Data_Ini, 1))
tempo_falhas = nrow(resulting_failure_data_vsrv17)
taxa_falhas_vsrv17_total_vsrv17_dez = tempo_falhas/tempo_atividade_dez
# taxa de falhas do mes = 0.1351

tempo_atividade_dez = subtractDate(tail(vsrv10_dez$Data_Ini, 1), head(vsrv10_dez$Data_Ini, 1))
tempo_falhas = nrow(resulting_failure_data_vsrv10)
taxa_falhas_vsrv10_total_vsrv10_dez = tempo_falhas/tempo_atividade_dez
# taxa de falhas do mes = 0.1351

taxa_falhas_vsrv17 = c()
cont = 1
tempo_dia = 1440 #minutos
n_falhas = 1
dia = 1
while(cont <= nrow(resulting_failure_data_vsrv17)){
  aux = ""
  if(dia < 10){
    aux = "0"
  }
  dia_string = paste(aux, toString(dia), sep ="")
  data_ini = toDate(paste("2017-12-", " 00:00:01", sep = dia_string))
  data_end = toDate(paste("2017-12-", " 23:59:01", sep = dia_string))
  data = resulting_failure_data_vsrv17[[cont,3]]
  if(subtractDate(data_ini, data) <= 0 && subtractDate(data_end, data) >= 0){
    n_falhas = n_falhas + 1
  }else{
    taxa = n_falhas/tempo_dia
    taxa_falhas_vsrv17 = c(taxa_falhas_vsrv17, taxa)
    dia = dia + 1
    n_falhas = 1
    cont = cont -1
  }
  
  if(cont == nrow(resulting_failure_data_vsrv17)){
    taxa = n_falhas/tempo_dia
    taxa_falhas_vsrv17 = c(taxa_falhas_vsrv17, taxa)
  }
  cont = cont + 1
}

mtbf_vsrv10_dez = c()
cont = 1
n_falhas = 1
dia = 1
duracao = 0
last = "2017-12-01 00:00:01"
# MAke this search in the all_vsrv10 and do the operation tehrre without filter: brute force all the way
while(cont <= nrow(resulting_failure_data_vsrv10)){
  aux = ""
  if(dia < 10){
    aux = "0"
  }
  dia_string = paste(aux, toString(dia), sep ="")
  data_ini = toDate(paste("2017-12-", " 00:00:01", sep = dia_string))
  data_end = toDate(paste("2017-12-", " 23:59:01", sep = dia_string))
  data = resulting_failure_data_vsrv10[[cont,3]]
  
  if(subtractDate(data_ini, data) <= 0 && subtractDate(data_end, data) >= 0){
    duracao = duracao + subtractDate(data, last)
    n_falhas = n_falhas + 1
  }else{
    if(duracao == 0 || n_falhas == 0){
      mtbf_vsrv10_dez = c(mtbf_vsrv10_dez, -1)
    }else {
      mtbf_now = duracao / n_falhas
      mtbf_vsrv10_dez = c(mtbf_vsrv10_dez, mtbf_now)
    }
    last = data
    
    
    dia = dia + 1
    n_falhas = 0
    duracao = 0
  }
  
  if(cont == nrow(resulting_failure_data_vsrv10)){
    mtbf_now = duracao / n_falhas
    mtbf_vsrv10_dez = c(mtbf_vsrv10_dez, mtbf_now)
  }
  cont = cont + 1
}

# Voltando ao teste as medias das taxas de falha dos dois servidores:

t.test(taxa_falhas_vsrv17, mtbf_vsrv10_dez, paired = F, conf.level = 0.95)


# Alínea e)
# - - - - - - 




# Alínea f)
# - - - - - - 









