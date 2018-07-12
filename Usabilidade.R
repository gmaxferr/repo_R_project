




# ========================================================================================================================
# ====                                  MAIN USABILITY SCRIPT                                                         ====
# ========================================================================================================================



inquerito = read.csv("./Resources/dados_inquerito.csv", sep = ",")
# View(inquerito)

# variaveis:
i_MENOR_18 = "Menos de 18"
i_18_25 = "18 - 25"
i_26_35 = "26 - 35"
i_36_45 = "36 - 45"
i_MAIS_45 = "Mais de 45"
s_MASCULINO = "Masculino"
s_FEMININO = "Feminino"

SEXO = 1
IDADE = 2
NACIONALIDADE = 3
REGISTO_SITE = 4
PESQUISA_POR_NOME = 5
ADICIONAR_CARRINHO_1 = 6
PESQUISA_POR_AUTOR = 7
SELECIONAR_LIVRO_AUTOR = 8
ADICIONAR_LIVRO_FAVORITOS = 9
ACEDER_FAVORITOS = 10
ADICIONAR_CARRINHO_2 = 11
REMOVER_LIVRO_CARRINHO = 12
INICIAR_COMPRA = 13
MOTIVOS = 14

attach(inquerito)

# Inqueridos por sexo

n_masculino = length(inquerito[inquerito$Sexo == s_MASCULINO,SEXO])
n_feminino = length(inquerito[inquerito$Sexo == s_FEMININO,SEXO])
n_total = n_masculino + n_feminino
# Temos uma amostra grande (n_total = 85 inqueridos)
# Pelo Torema do Limite Central, podemos pressupor uma distribuicao normal dos dados

perc_masc = round((n_masculino / n_total) * 100, 2)
perc_fem = round((n_feminino / n_total) * 100, 2)
# perc_masc -> 62.35%
# perc_fem  -> 37.65%
label_perc_masc = paste("(", "%)", sep=toString(perc_masc))
label_perc_fem = paste("(", "%)", sep=toString(perc_fem))

# graficamente:
pie(c(perc_masc, perc_fem),
    c(paste(s_MASCULINO, label_perc_masc),
      paste(s_FEMININO, label_perc_fem)),
    col = c("#DAF7A6", "#900C3F"))
title(main = "Sexo dos Inquiridos")

# Nacionalidade:

perc_pt = (length(inquerito[inquerito$Nacionalidade=="Portugal",NACIONALIDADE]) / n_total) * 100
label_perc_pt = paste("(", "%)", sep=toString(perc_pt))
pie(perc_pt, col = "#DAF7A6", c(paste("Portugal", label_perc_pt)))
title(main = "Nacionalidade dos Inquiridos")


# Idade:

n_menos18 = length(inquerito[inquerito$Idade == i_MENOR_18, IDADE])
n_18_25 = length(inquerito[inquerito$Idade == i_18_25, IDADE])
n_26_35 = length(inquerito[inquerito$Idade == i_26_35, IDADE])
n_36_45 = length(inquerito[inquerito$Idade == i_36_45, IDADE])
n_mais45= length(inquerito[inquerito$Idade == i_MAIS_45, IDADE])


barplot(c(n_menos18, n_18_25, n_26_35, n_36_45, n_mais45),
        names.arg = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        col = c("#DAF7A6", "#FFC300", "#FF5733", "#C70039", "#900C3F"))
title(main = "Idade dos Inquiridos")


# 1. Registo no Site:

dados_registo = inquerito[REGISTO_SITE]
registo_p1 = length(dados_registo[dados_registo==1])
registo_p2 = length(dados_registo[dados_registo==2])
registo_p3 = length(dados_registo[dados_registo==3])
registo_p4 = length(dados_registo[dados_registo==4])
registo_p5 = length(dados_registo[dados_registo==5])
pontuacoes_registo = c(registo_p1, registo_p2, registo_p3, registo_p4, registo_p5)

# 2. Pesquisa por nome:

dados_pesquisa_por_nome = inquerito[PESQUISA_POR_NOME]
pesquisa_por_nome_p1 = length(dados_pesquisa_por_nome[dados_pesquisa_por_nome==1])
pesquisa_por_nome_p2 = length(dados_pesquisa_por_nome[dados_pesquisa_por_nome==2])
pesquisa_por_nome_p3 = length(dados_pesquisa_por_nome[dados_pesquisa_por_nome==3])
pesquisa_por_nome_p4 = length(dados_pesquisa_por_nome[dados_pesquisa_por_nome==4])
pesquisa_por_nome_p5 = length(dados_pesquisa_por_nome[dados_pesquisa_por_nome==5])
pontuacoes_pesquisa_por_nome = c(pesquisa_por_nome_p1, pesquisa_por_nome_p2, pesquisa_por_nome_p3, pesquisa_por_nome_p4, pesquisa_por_nome_p5)

# 3. Adicionar livro ao carrinho
# (partindo da sua pagina de detalhes)

dados_adicionar_carrinho_1 = inquerito[ADICIONAR_CARRINHO_1]
adicionar_carrinho_1_p1 = length(dados_adicionar_carrinho_1[dados_adicionar_carrinho_1==1])
adicionar_carrinho_1_p2 = length(dados_adicionar_carrinho_1[dados_adicionar_carrinho_1==2])
adicionar_carrinho_1_p3 = length(dados_adicionar_carrinho_1[dados_adicionar_carrinho_1==3])
adicionar_carrinho_1_p4 = length(dados_adicionar_carrinho_1[dados_adicionar_carrinho_1==4])
adicionar_carrinho_1_p5 = length(dados_adicionar_carrinho_1[dados_adicionar_carrinho_1==5])
pontuacoes_adicionar_carrinho_1 = c(adicionar_carrinho_1_p1, adicionar_carrinho_1_p2, adicionar_carrinho_1_p3, adicionar_carrinho_1_p4, adicionar_carrinho_1_p5)

# 4. Pesquisa por autor

dados_pesquisa_por_autor = inquerito[PESQUISA_POR_AUTOR]
pesquisa_por_autor_p1 = length(dados_pesquisa_por_autor[dados_pesquisa_por_autor==1])
pesquisa_por_autor_p2 = length(dados_pesquisa_por_autor[dados_pesquisa_por_autor==2])
pesquisa_por_autor_p3 = length(dados_pesquisa_por_autor[dados_pesquisa_por_autor==3])
pesquisa_por_autor_p4 = length(dados_pesquisa_por_autor[dados_pesquisa_por_autor==4])
pesquisa_por_autor_p5 = length(dados_pesquisa_por_autor[dados_pesquisa_por_autor==5])
pontuacoes_pesquisa_por_autor = c(pesquisa_por_autor_p1, pesquisa_por_autor_p2, pesquisa_por_autor_p3, pesquisa_por_autor_p4, pesquisa_por_autor_p5)

# 5. Selecionar livro pretendido do autor

dados_selecionar_livro_autor = inquerito[SELECIONAR_LIVRO_AUTOR]
selecionar_livro_autor_p1 = length(dados_selecionar_livro_autor[dados_selecionar_livro_autor==1])
selecionar_livro_autor_p2 = length(dados_selecionar_livro_autor[dados_selecionar_livro_autor==2])
selecionar_livro_autor_p3 = length(dados_selecionar_livro_autor[dados_selecionar_livro_autor==3])
selecionar_livro_autor_p4 = length(dados_selecionar_livro_autor[dados_selecionar_livro_autor==4])
selecionar_livro_autor_p5 = length(dados_selecionar_livro_autor[dados_selecionar_livro_autor==5])
pontuacoes_selecionar_livro_autor = c(selecionar_livro_autor_p1, selecionar_livro_autor_p2, selecionar_livro_autor_p3, selecionar_livro_autor_p4, selecionar_livro_autor_p5)


# 6. Adicionar livro aos favoritos

dados_adicionar_favoritos = inquerito[ADICIONAR_LIVRO_FAVORITOS]
adicionar_favoritos_p1 = length(dados_adicionar_favoritos[dados_adicionar_favoritos==1])
adicionar_favoritos_p2 = length(dados_adicionar_favoritos[dados_adicionar_favoritos==2])
adicionar_favoritos_p3 = length(dados_adicionar_favoritos[dados_adicionar_favoritos==3])
adicionar_favoritos_p4 = length(dados_adicionar_favoritos[dados_adicionar_favoritos==4])
adicionar_favoritos_p5 = length(dados_adicionar_favoritos[dados_adicionar_favoritos==5])
pontuacoes_adicionar_favoritos = c(adicionar_favoritos_p1, adicionar_favoritos_p2, adicionar_favoritos_p3, adicionar_favoritos_p4, adicionar_favoritos_p5)

# 7. Aceder a lista de favoritos

dados_aceder_favoritos = inquerito[ACEDER_FAVORITOS]
aceder_favoritos_p1 = length(dados_aceder_favoritos[dados_aceder_favoritos==1])
aceder_favoritos_p2 = length(dados_aceder_favoritos[dados_aceder_favoritos==2])
aceder_favoritos_p3 = length(dados_aceder_favoritos[dados_aceder_favoritos==3])
aceder_favoritos_p4 = length(dados_aceder_favoritos[dados_aceder_favoritos==4])
aceder_favoritos_p5 = length(dados_aceder_favoritos[dados_aceder_favoritos==5])
pontuacoes_aceder_favoritos = c(aceder_favoritos_p1, aceder_favoritos_p2, aceder_favoritos_p3, aceder_favoritos_p4, aceder_favoritos_p5)


# 8. Adicionar livro ao carrinho
# (partindo da pagina dos favoritos)

dados_adicionar_carrinho_2 = inquerito[ADICIONAR_CARRINHO_2]
adicionar_carrinho_2_p1 = length(dados_adicionar_carrinho_2[dados_adicionar_carrinho_2==1])
adicionar_carrinho_2_p2 = length(dados_adicionar_carrinho_2[dados_adicionar_carrinho_2==2])
adicionar_carrinho_2_p3 = length(dados_adicionar_carrinho_2[dados_adicionar_carrinho_2==3])
adicionar_carrinho_2_p4 = length(dados_adicionar_carrinho_2[dados_adicionar_carrinho_2==4])
adicionar_carrinho_2_p5 = length(dados_adicionar_carrinho_2[dados_adicionar_carrinho_2==5])
pontuacoes_adicionar_carrinho_2 = c(adicionar_carrinho_2_p1, adicionar_carrinho_2_p2, adicionar_carrinho_2_p3, adicionar_carrinho_2_p4, adicionar_carrinho_2_p5)


# 9. Remover livro do carrinho

dados_remover_livro_carrinho = inquerito[REMOVER_LIVRO_CARRINHO]
remover_livro_carrinho_p1 = length(dados_remover_livro_carrinho[dados_remover_livro_carrinho==1])
remover_livro_carrinho_p2 = length(dados_remover_livro_carrinho[dados_remover_livro_carrinho==2])
remover_livro_carrinho_p3 = length(dados_remover_livro_carrinho[dados_remover_livro_carrinho==3])
remover_livro_carrinho_p4 = length(dados_remover_livro_carrinho[dados_remover_livro_carrinho==4])
remover_livro_carrinho_p5 = length(dados_remover_livro_carrinho[dados_remover_livro_carrinho==5])
pontuacoes_remover_livro_carrinho = c(remover_livro_carrinho_p1, remover_livro_carrinho_p2, remover_livro_carrinho_p3, remover_livro_carrinho_p4, remover_livro_carrinho_p5)


# 10. Iniciar Compra

dados_iniciar_compra = inquerito[INICIAR_COMPRA]
iniciar_compra_p1 = length(dados_iniciar_compra[dados_iniciar_compra==1])
iniciar_compra_p2 = length(dados_iniciar_compra[dados_iniciar_compra==2])
iniciar_compra_p3 = length(dados_iniciar_compra[dados_iniciar_compra==3])
iniciar_compra_p4 = length(dados_iniciar_compra[dados_iniciar_compra==4])
iniciar_compra_p5 = length(dados_iniciar_compra[dados_iniciar_compra==5])
pontuacoes_iniciar_compra = c(iniciar_compra_p1, iniciar_compra_p2, iniciar_compra_p3, iniciar_compra_p4, iniciar_compra_p5)

# Resultados em gráfico:

barplot(c(pontuacoes_registo,
          pontuacoes_pesquisa_por_nome,
          pontuacoes_adicionar_carrinho_1,
          pontuacoes_pesquisa_por_autor,
          pontuacoes_selecionar_livro_autor,
          pontuacoes_adicionar_favoritos,
          pontuacoes_aceder_favoritos,
          pontuacoes_adicionar_carrinho_2,
          pontuacoes_remover_livro_carrinho,
          pontuacoes_iniciar_compra),
        space = 1,
        col = c("#DAF7A6", "#FFC300", "#FF5733", "#C70039", "#900C3F"),
        names.arg = c(" ", " ", "1", " ", " ",
                      " ", " ", "2", " ", " ",
                      " ", " ", "3", " ", " ",
                      " ", " ", "4", " ", " ",
                      " ", " ", "5", " ", " ",
                      " ", " ", "6", " ", " ",
                      " ", " ", "7", " ", " ",
                      " ", " ", "8", " ", " ",
                      " ", " ", "9", " ", " ",
                      " ", " ", "10", " ", " "),
        legend.text = c(1:5)
)
title(main = "Quantidade de votos por tarefa")



# Motivos de Dificuldade

dados_motivos = inquerito[MOTIVOS]
motivos = c()
i=1
while(i <= nrow(dados_motivos)){
  line = dados_motivos[i, 1]
  found = c()
  if(length(line) != 0){
    splited = strsplit(toString(line), ";")[[1]]
    cont = 1
    while(cont <= length(splited)){
      if(!(splited[cont] %in% motivos)){
        motivos = c(motivos, splited[cont])
      }
      cont = cont + 1
    }
  }
  i = i +1
}

i=1
motivos_qtd = c()
while(i <= length(motivos)){
  c=0
  qtd = 0
  while(c <= nrow(dados_motivos)){
    line = dados_motivos[c, 1]
    found = c()
    if(length(line) != 0){
      splited = strsplit(toString(line), ";")[[1]]
      cont = 1
      while(cont <= length(splited)){
        if(splited[cont] == motivos[i]){
          qtd = qtd + 1
        }
        cont = cont + 1
      }
    }
    c=c+1
  }
  i = i +1
  motivos_qtd = c(motivos_qtd, qtd) 
}


total = sum(motivos_qtd)
percs = c()
i = 1
labels = c()
while( i <= length(motivos_qtd)){
  perc = round(motivos_qtd[i]/total, 2)
  start = paste(toString(i), "(")
  labels = c(labels, paste(start, "%)", sep=toString(perc)))
  percs = c(percs, perc)
  i = i + 1
}
pie(percs, col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"), labels)
title(main="Motivos de dificuldade")

# =====
# FALTA
# =====
# BOXPLOT para cada pergunta com as pontuacoes!

registo = mean(inquerito$Registo_Site)
pesquisa_por_nome = mean(inquerito$Pesquisa_por_nome)
adicionar_carrinho_1 = mean(inquerito$Adicionar_carrinho_compras_1)
pesquisa_por_autor = mean(inquerito$Pesquisar_por_autor)
selecionar_livro_autor = mean(inquerito$Selecionar_livro_autor)
adicionar_favoritos = mean(inquerito$Adicionar_livro_favoritos)
aceder_favoritos = mean(inquerito$Aceder_favoritos)
adicionar_carrinho_2 =mean(inquerito$Adicionar_carrinho_compras_2)
remover_livro_carrinho = mean(inquerito$Remover_livro_carrinho)
iniciar_compra = mean(inquerito$Iniciar_compra)

tarefas = c(registo,
            pesquisa_por_nome,
            adicionar_carrinho_1,
            pesquisa_por_autor,
            selecionar_livro_autor,
            adicionar_favoritos,
            aceder_favoritos,
            adicionar_carrinho_2,
            remover_livro_carrinho,
            iniciar_compra)
# Geral medias de pontuacoes (barplot)
barplot(tarefas,
        names = 1:10,
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Médias de Pontuações por Tarefa")
# Overall (boxplot)
boxplot(tarefas, col = c("#DAF7A6", "#ecf7a6", "#FFC300"),
        ylim=c(1,5))
title(main = "Boxplot do Overall das pontuações dadas")


registo = inquerito$Registo_Site
pesquisa_por_nome = inquerito$Pesquisa_por_nome
adicionar_carrinho_1 = inquerito$Adicionar_carrinho_compras_1
pesquisa_por_autor = inquerito$Pesquisar_por_autor
selecionar_livro_autor = inquerito$Selecionar_livro_autor
adicionar_favoritos = inquerito$Adicionar_livro_favoritos
aceder_favoritos = inquerito$Aceder_favoritos
adicionar_carrinho_2 =inquerito$Adicionar_carrinho_compras_2
remover_livro_carrinho = inquerito$Remover_livro_carrinho
iniciar_compra = inquerito$Iniciar_compra


tarefas = c(registo,
            pesquisa_por_nome,
            adicionar_carrinho_1,
            pesquisa_por_autor,
            selecionar_livro_autor,
            adicionar_favoritos,
            aceder_favoritos,
            adicionar_carrinho_2,
            remover_livro_carrinho,
            iniciar_compra)

mean(tarefas)
sd(tarefas)
median(tarefas)
# mean = 3.814
# sd = 0.970
# median = 4

cont = 0
medias = c()
medianas = c()
desvios_padrao = c()
while(cont < 10){
  min = cont*n_total
  max = min+n_total
  this = tarefas[min:max]
  
  medias = c(medias, round(mean(this),3))
  desvios_padrao = c(desvios_padrao, round(sd(this),3))
  medianas = c(medianas, round(median(this),3))
  cont = cont + 1 
}

medias
desvios_padrao
medianas

#
# Segundo os dados nos vetores acima impressos, temos,
# para cada tarefa onde:
#
# 1  - Registo na Wook
# 2  - Pesquisar um livro por nome
# 3  - Adicionar livro pesquisado ao carrinho de compras
# 4  - Pesquisar livro por autor
# 5  - Selecionar livro pretendido do autor
# 6  - Adicionar livro selecionado aos favoritos
# 7  - Aceder a pagina dos favoritos
# 8  - Adicionar livro dos favoritos ao carrinho de compras
# 9  - Remover um livro do carrinho de compras
# 10 - Iniciar processo de compra do livro
#
# Obtemos os seguintes valores:
#
# +===============+
# |    TAREFAS    |
# +===============+
#
# +---------------+
# |       1       |
# +---------------+
#     mean: 3.411
#       sd: 0.890
#   median: 3
#
# +---------------+
# |       2       |
# +---------------+
#     mean: 3.767
#       sd: 0.835
#   median: 4
#
# +---------------+
# |       3       |
# +---------------+
#     mean: 4.058
#       sd: 0.872
#   median: 4
#
# +---------------+
# |       4       |
# +---------------+
#     mean: 3.406
#       sd: 0.937
#   median: 3
#
# +---------------+
# |       5       |
# +---------------+
#     mean: 3.081
#       sd: 0.923
#   median: 3
#
# +---------------+
# |       6       |
# +---------------+
#     mean: 4.116
#       sd: 0.846
#   median: 4
#
# +---------------+
# |       7       |
# +---------------+
#     mean: 3.779
#       sd: 0.925
#   median: 4
#
# +---------------+
# |       8       |
# +---------------+
#     mean: 4.197
#       sd: 0.905
#   median: 4
#
# +---------------+
# |       9       |
# +---------------+
#     mean: 4.337
#       sd: 0.834
#   median: 5
#
# +---------------+
# |       10      |
# +---------------+
#     mean: 4.023
#       sd: 0.957
#   median: 4
#
# +===============+

i = which(inquerito$Idade == i_MENOR_18)
dados_men_18 = inquerito[i,]
i = which(inquerito$Idade == i_18_25)
dados_18_25 = inquerito[i,]
i = which(inquerito$Idade == i_26_35)
dados_26_35 = inquerito[i,]
i = which(inquerito$Idade == i_36_45)
dados_36_45 = inquerito[i,]
i = which(inquerito$Idade == i_MAIS_45)
dados_45_mais = inquerito[i,]

# Menores de 18

p_m_18_registo = (dados_men_18$Registo_Site)
p_m_18_pesq_nome = (dados_men_18$Pesquisa_por_nome)
p_m_18_add_carr_1 = (dados_men_18$Adicionar_carrinho_compras_1)
p_m_18_pesq_autor = (dados_men_18$Pesquisar_por_autor)
p_m_18_sel_livro = (dados_men_18$Selecionar_livro_autor)
p_m_18_add_fav = (dados_men_18$Adicionar_livro_favoritos)
p_m_18_acc_fav = (dados_men_18$Aceder_favoritos)
p_m_18_add_carr_2 = (dados_men_18$Adicionar_carrinho_compras_2)
p_m_18_rm_carr = (dados_men_18$Remover_livro_carrinho)
p_m_18_ini_cp = (dados_men_18$Iniciar_compra)

p_m_18_tarefas = c(p_m_18_registo,
            p_m_18_pesq_nome,
            p_m_18_add_carr_1,
            p_m_18_pesq_autor,
            p_m_18_sel_livro,
            p_m_18_add_fav,
            p_m_18_acc_fav,
            p_m_18_add_carr_2,
            p_m_18_rm_carr,
            p_m_18_ini_cp)

p_m_18_media_total = mean(p_m_18_tarefas)


# 18 a 25

p_18_25_registo = (dados_18_25$Registo_Site)
p_18_25_pesq_nome = (dados_18_25$Pesquisa_por_nome)
p_18_25_add_carr_1 = (dados_18_25$Adicionar_carrinho_compras_1)
p_18_25_pesq_autor = (dados_18_25$Pesquisar_por_autor)
p_18_25_sel_livro = (dados_18_25$Selecionar_livro_autor)
p_18_25_add_fav = (dados_18_25$Adicionar_livro_favoritos)
p_18_25_acc_fav = (dados_18_25$Aceder_favoritos)
p_18_25_add_carr_2 = (dados_18_25$Adicionar_carrinho_compras_2)
p_18_25_rm_carr = (dados_18_25$Remover_livro_carrinho)
p_18_25_ini_cp = (dados_18_25$Iniciar_compra)

p_18_25_tarefas = c(p_18_25_registo,
                    p_18_25_pesq_nome,
                    p_18_25_add_carr_1,
                    p_18_25_pesq_autor,
                    p_18_25_sel_livro,
                    p_18_25_add_fav,
                    p_18_25_acc_fav,
                    p_18_25_add_carr_2,
                    p_18_25_rm_carr,
                    p_18_25_ini_cp)

p_18_25_media_total = mean(p_18_25_tarefas)



# 26 a 35

p_26_35_registo = (dados_26_35$Registo_Site)
p_26_35_pesq_nome = (dados_26_35$Pesquisa_por_nome)
p_26_35_add_carr_1 = (dados_26_35$Adicionar_carrinho_compras_1)
p_26_35_pesq_autor = (dados_26_35$Pesquisar_por_autor)
p_26_35_sel_livro = (dados_26_35$Selecionar_livro_autor)
p_26_35_add_fav = (dados_26_35$Adicionar_livro_favoritos)
p_26_35_acc_fav = (dados_26_35$Aceder_favoritos)
p_26_35_add_carr_2 = (dados_26_35$Adicionar_carrinho_compras_2)
p_26_35_rm_carr = (dados_26_35$Remover_livro_carrinho)
p_26_35_ini_cp = (dados_26_35$Iniciar_compra)

p_26_35_tarefas = c(p_26_35_registo,
                    p_26_35_pesq_nome,
                    p_26_35_add_carr_1,
                    p_26_35_pesq_autor,
                    p_26_35_sel_livro,
                    p_26_35_add_fav,
                    p_26_35_acc_fav,
                    p_26_35_add_carr_2,
                    p_26_35_rm_carr,
                    p_26_35_ini_cp)

p_26_35_media_total = mean(p_26_35_tarefas)



# 36 a 45

p_36_45_registo = dados_36_45$Registo_Site
p_36_45_pesq_nome = dados_36_45$Pesquisa_por_nome
p_36_45_add_carr_1 = dados_36_45$Adicionar_carrinho_compras_1
p_36_45_pesq_autor = dados_36_45$Pesquisar_por_autor
p_36_45_sel_livro = dados_36_45$Selecionar_livro_autor
p_36_45_add_fav = dados_36_45$Adicionar_livro_favoritos
p_36_45_acc_fav = dados_36_45$Aceder_favoritos
p_36_45_add_carr_2 = dados_36_45$Adicionar_carrinho_compras_2
p_36_45_rm_carr = dados_36_45$Remover_livro_carrinho
p_36_45_ini_cp = dados_36_45$Iniciar_compra

p_36_45_tarefas = c(p_36_45_registo,
                    p_36_45_pesq_nome,
                    p_36_45_add_carr_1,
                    p_36_45_pesq_autor,
                    p_36_45_sel_livro,
                    p_36_45_add_fav,
                    p_36_45_acc_fav,
                    p_36_45_add_carr_2,
                    p_36_45_rm_carr,
                    p_36_45_ini_cp)

p_36_45_media_total = mean(p_36_45_tarefas)



# Mais de 45

p_45_mais_registo = dados_45_mais$Registo_Site
p_45_mais_pesq_nome = dados_45_mais$Pesquisa_por_nome
p_45_mais_add_carr_1 = dados_45_mais$Adicionar_carrinho_compras_1
p_45_mais_pesq_autor = dados_45_mais$Pesquisar_por_autor
p_45_mais_sel_livro = dados_45_mais$Selecionar_livro_autor
p_45_mais_add_fav = dados_45_mais$Adicionar_livro_favoritos
p_45_mais_acc_fav = dados_45_mais$Aceder_favoritos
p_45_mais_add_carr_2 = dados_45_mais$Adicionar_carrinho_compras_2
p_45_mais_rm_carr = dados_45_mais$Remover_livro_carrinho
p_45_mais_ini_cp = dados_45_mais$Iniciar_compra

p_45_mais_tarefas = c(p_45_mais_registo,
                      p_45_mais_pesq_nome,
                      p_45_mais_add_carr_1,
                      p_45_mais_pesq_autor,
                      p_45_mais_sel_livro,
                      p_45_mais_add_fav,
                      p_45_mais_acc_fav,
                      p_45_mais_add_carr_2,
                      p_45_mais_rm_carr,
                      p_45_mais_ini_cp)

p_45_mais_media_total = mean(p_45_mais_tarefas)

media_pontuacoes_por_idades = c(mean(p_m_18_media_total),
                                mean(p_18_25_media_total),
                                mean(p_26_35_media_total),
                                mean(p_36_45_media_total),
                                mean(p_45_mais_media_total))

# Por tarefa:

reg = c(mean(p_m_18_registo),
        mean(p_18_25_registo),
        mean(p_26_35_registo),
        mean(p_36_45_registo),
        mean(p_45_mais_registo))
barplot(reg,
        names = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Dificuldade Registo do utilizador por idades")



pn = c(mean(p_m_18_pesq_nome),
       mean(p_18_25_pesq_nome),
       mean(p_26_35_pesq_nome),
       mean(p_36_45_pesq_nome),
       mean(p_45_mais_pesq_nome))
barplot(pn,
        names = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Dificuldade Pesquisa por nome por idades")




a1 = c(mean(p_m_18_add_carr_1),
       mean(p_18_25_add_carr_1),
       mean(p_26_35_add_carr_1),
       mean(p_36_45_add_carr_1),
       mean(p_45_mais_add_carr_1))
barplot(a1,
        names = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Dificuldade Adicionar ao carrinho de compras\n desde da pagina de detalhes do livro idades")


pa = c(mean(p_m_18_pesq_autor),
       mean(p_18_25_pesq_autor),
       mean(p_26_35_pesq_autor),
       mean(p_36_45_pesq_autor),
       mean(p_45_mais_pesq_autor))
barplot(pa,
        names = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Dificuldade Pesquisa por autor por idades")



sel = c(mean(p_m_18_sel_livro),
        mean(p_18_25_sel_livro),
        mean(p_26_35_sel_livro),
        mean(p_36_45_sel_livro),
        mean(p_45_mais_sel_livro))
barplot(sel,
        names = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Dificuldade Encontrar Livro do Autor selecionado por idades")




af = c(mean(p_m_18_add_fav),
       mean(p_18_25_add_fav),
       mean(p_26_35_add_fav),
       mean(p_36_45_add_fav),
       mean(p_45_mais_add_fav))
barplot(af,
        names = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Dificuldade Adicionar aos Favoritos por idades")





acc = c(mean(p_m_18_acc_fav),
        mean(p_18_25_acc_fav),
        mean(p_26_35_acc_fav),
        mean(p_36_45_acc_fav),
        mean(p_45_mais_acc_fav))
barplot(acc,
        names = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Dificuldade Aceder aos Favoritos idades")





a2 = c(mean(p_m_18_add_carr_2),
       mean(p_18_25_add_carr_2),
       mean(p_26_35_add_carr_2),
       mean(p_36_45_add_carr_2),
       mean(p_45_mais_add_carr_2))
barplot(a2,
        names = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Dificuldade Adicionar carrinho na pagina de favoritos por idades")




rm = c(mean(p_m_18_rm_carr),
       mean(p_18_25_rm_carr),
       mean(p_26_35_rm_carr),
       mean(p_36_45_rm_carr),
       mean(p_45_mais_rm_carr))
barplot(rm,
        names = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Dificuldade Remover Livro do Carrinho de Compras por idades")



cp = c(mean(p_m_18_ini_cp),
       mean(p_18_25_ini_cp),
       mean(p_26_35_ini_cp),
       mean(p_36_45_ini_cp),
       mean(p_45_mais_ini_cp))
barplot(cp,
        names = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Dificuldade Iniciar Compra por idades")










# Geral medias de pontuacoes (barplot)
barplot(media_pontuacoes_por_idades,
        names = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        ylim=c(0,5),
        col = c("#DAF7A6", "#ecf7a6", "#FFC300", "#ff9750","#FF5733", "#C70039", "#900C3F"))
title(main = "Médias de Pontuações de Tarefas por idade (Global)")
