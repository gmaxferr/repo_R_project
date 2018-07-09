




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


# Nacionalidade:

perc_pt = (length(inquerito[inquerito$Nacionalidade=="Portugal",NACIONALIDADE]) / n_total) * 100
label_perc_pt = paste("(", "%)", sep=toString(perc_pt))
pie(perc_pt, col = "blue", c(paste("Portugal", label_perc_pt)))


# Idade:

n_menos18 = length(inquerito[inquerito$Idade == i_MENOR_18, IDADE])
n_18_25 = length(inquerito[inquerito$Idade == i_18_25, IDADE])
n_26_35 = length(inquerito[inquerito$Idade == i_26_35, IDADE])
n_36_45 = length(inquerito[inquerito$Idade == i_36_45, IDADE])
n_mais45= length(inquerito[inquerito$Idade == i_MAIS_45, IDADE])


barplot(c(n_menos18, n_18_25, n_26_35, n_36_45, n_mais45),
        names.arg = c(i_MENOR_18, i_18_25, i_26_35, i_36_45, i_MAIS_45),
        col = c("#DAF7A6", "#FFC300", "#FF5733", "#C70039", "#900C3F"))


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
while( i < length(motivos_qtd)){
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



