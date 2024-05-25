### Leon Labre
### 11/10/2022


setwd("D:/Users/leonl/Desktop/Clear/Applied Micro/Labor")


### Cleaning the R environment
rm(list=ls())


### Load Packages (and install packages if needed)
load.lib <- c("data.table","ggplot2","PNSIBGE","readxl","PNADcIBGE","survey",
              "sidrar","forcats")

### Instaling and loading packages
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)







#####Gráfico taxa de informalidade

### PARA SABER INFORMAÇÕES SOBRE OS DADOS:
#info_sidra(8529, wb = TRUE)


# Baixando e tratando a base de dados
dt <- get_sidra(x = 8529,
                variable = 12466,
                period = c("201504-202202"))

trimestre <- c("2015-04", "2016-01", "2016-02", "2016-03", "2016-04", "2017-01", "2017-02", "2017-03", "2017-04", "2018-01", "2018-02", "2018-03", "2018-04",
               "2019-01", "2019-02", "2019-03", "2019-04", "2020-01", "2020-02", "2020-03", "2020-04", "2021-01", "2021-02", "2021-03", "2021-04", "2022-01",
               "2022-02")
setDT(dt)
dt <- dt[,`Trimestre (Código)`:= trimestre]


#Gerando o gráfico

ggplot(data = dt, aes(x =`Trimestre (Código)`,y = Valor,group = 1)) + geom_line(size = 1,color = "BLUE4") +scale_x_discrete(breaks = c("2015-04","2016-02","2017-02","2018-02","2019-02","2020-02","2021-02","2022-02")) +scale_size_manual(values = c(rep(1.3, 7), 1.5, rep(0.5, dim(dt)[1]-6))) +
  theme_bw(base_size = 25) +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  ) +
  labs(
    x = "Ano - Trimestre", y = "Taxa de informalidade",
    title = "Taxa de informalidade(4º trimestre 2015 - 2º trimestre 2022)",
    color = "",
    caption = "Fonte: Sidra/IBGE.")


#####Taxa de informalidade por estado, raça, genêro e educação



#### LEGENDA DAS VARIÁVEIS UTILIZADAS
#UF = Estados
#V4029 = Carteira de trabalho assinada
#VD4002 = Condição de ocupação(Pessoas ocupadas, desocupadas)
#V4019 = Esse negócio/empresa era registrado no Cadastro Nacional da Pessoa Jurídica - CNPJ?
#VD4009 =  Posição na ocupação trab. princ
#V2007 = Sexo
#V2010 = Cor ou raça
#VD3004 = Educação
#V2009 = Idade
#V4010= Código da ocupação (cargo ou função)
#VD4016 = Rendim. habitual trab. princ.
#VD4013 = Faixa das horas habitualmente trabalhadas por semana 
#V4014 = Esse trabalho era na área:Federal, Municipal ou Estadual


# Baixando a base de dados
dadosPNADc <- get_pnadc(year=2022, quarter=2, vars=c("UF","VD4010","V4029","VD4002","V4019","VD4009","V2007","V2010","VD3004","V2009","V4010","VD4016","VD4013","V4014"))




#Estados


#Tratando a base de dados

tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Rondônia"), na.rm = TRUE)

Rondônia <- tx_inf$ratio





tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Acre"), na.rm = TRUE)

Acre <- tx_inf$ratio


tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Amazonas"), na.rm = TRUE)

Amazonas <- tx_inf$ratio


tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Roraima"), na.rm = TRUE)

Roraima <- tx_inf$ratio


tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Pará"), na.rm = TRUE)

Pará <- tx_inf$ratio



tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Amapá"), na.rm = TRUE)

Amapá <- tx_inf$ratio




tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Tocantins"), na.rm = TRUE)

Tocantins <- tx_inf$ratio




tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Maranhão"), na.rm = TRUE)

Maranhão <- tx_inf$ratio





tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Piauí"), na.rm = TRUE)

Piauí <- tx_inf$ratio


tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Ceará"), na.rm = TRUE)

Ceará <- tx_inf$ratio





tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Rio Grande do Norte"), na.rm = TRUE)

Rio_Grande_do_Norte <- tx_inf$ratio






tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Paraíba"), na.rm = TRUE)

Paraíba <- tx_inf$ratio





tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Pernambuco"), na.rm = TRUE)

Pernambuco <- tx_inf$ratio




tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Alagoas"), na.rm = TRUE)

Alagoas <- tx_inf$ratio




tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Sergipe"), na.rm = TRUE)

Sergipe <- tx_inf$ratio




tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Bahia"), na.rm = TRUE)

Bahia <- tx_inf$ratio




tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Minas Gerais"), na.rm = TRUE)

Minas_Gerais <- tx_inf$ratio





tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Rio de Janeiro"), na.rm = TRUE)

Rio_de_Janeiro <- tx_inf$ratio




tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "São Paulo"), na.rm = TRUE)

São_Paulo <- tx_inf$ratio





tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Paraná"), na.rm = TRUE)

Paraná <- tx_inf$ratio



tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Santa Catarina"), na.rm = TRUE)

Santa_Catarina <- tx_inf$ratio



tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Rio Grande do Sul"), na.rm = TRUE)

Rio_Grande_do_Sul <- tx_inf$ratio


tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Mato Grosso do Sul"), na.rm = TRUE)

Mato_Grosso_do_Sul <- tx_inf$ratio



tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Paraná"), na.rm = TRUE)

Paraná <- tx_inf$ratio



tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Mato Grosso"), na.rm = TRUE)

Mato_Grosso <- tx_inf$ratio



tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Goiás"), na.rm = TRUE)

Goiás <- tx_inf$ratio



tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Distrito Federal"), na.rm = TRUE)

Distrito_Federal <- tx_inf$ratio

tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,UF == "Espírito Santo"), na.rm = TRUE)

Espírito_Santo <- tx_inf$ratio

estados <- c("Acre", "Alagoas", 
             "Amapá", "Amazonas",
             "Bahia", "Ceará",
             "Distrito Federal", "Espírito Santo",
             "Goiás","Maranhão",
             "Mato Grosso","Mato Grosso do Sul",
             "Minas Gerais","Pará",
             "Paraíba","Paraná",
             "Pernambuco","Piauí",
             "Rio Grande do Norte","Rio Grande do Sul",
             "Rio de Janeiro","Rondônia","Roraima","Santa Catarina",
             "São Paulo","Sergipe","Tocantins")

inf <- c(Acre, Alagoas, Amapá, Amazonas,Bahia, Ceará,
         Distrito_Federal, Espírito_Santo,
         Goiás,Maranhão,
         Mato_Grosso,Mato_Grosso_do_Sul,
         Minas_Gerais,Pará,
         Paraíba,Paraná,
         Pernambuco,Piauí,
         Rio_Grande_do_Norte,Rio_Grande_do_Sul,
         Rio_de_Janeiro,Rondônia,Roraima,Santa_Catarina,
         São_Paulo,Sergipe,Tocantins)
Estados <- data.table(estados = estados, Tx_inf = inf)


#Gerando o gráfico

ggplot(Estados, aes(x = fct_reorder(estados,Tx_inf), y = Tx_inf)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de informalidade Brasil, por estados(2º trimestre 2022)",
  caption = "Fonte: PNAD contínua. Nota: O número de trabalhadores informais é composto por trabalhadores sem carteira assinada, empregadores e conta própria sem CNPJ, além de trabalhadores familiares auxiliares.")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.background = element_rect("transparent"),
        legend.key = element_rect("transparent")
  )  + coord_flip() +geom_text(hjust = -0.5,
                               position = position_dodge(width = 1),
                               inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(Tx_inf, digits = 2))))





#Raça

#Tratando a base de dados

tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,V2010 == "Branca"), na.rm = TRUE)

branca <- tx_inf$ratio

tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,V2010 == "Parda"), na.rm = TRUE)

parda <- tx_inf$ratio


tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,V2010 == "Amarela"), na.rm = TRUE)

amarela <- tx_inf$ratio

tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,V2010 == "Indígena"), na.rm = TRUE)

Indígena <- tx_inf$ratio



tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,V2010 == "Preta"), na.rm = TRUE)
negro <- tx_inf$ratio

Raça <- c("Amarela", "Branca", "Indígena","Negro","Parda")
inf <- c(amarela,branca,Indígena,negro,parda)
raca <- data.table(Raça = Raça, Tx_inf = inf)

#Gerando o gráfico

ggplot(raca, aes(x = fct_reorder(Raça,inf), y = inf)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de informalidade Brasil, por raça(2º trimestre 2022)",
  caption = "Fonte: PNAD contínua. Nota: O número de trabalhadores informais é composto por trabalhadores sem carteira assinada, empregadores e conta própria sem CNPJ, além de trabalhadores familiares auxiliares.")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.background = element_rect("transparent"),
        legend.key = element_rect("transparent")
  )  + coord_flip() +geom_text(hjust = -0.5,
                               position = position_dodge(width = 1),
                               inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(inf, digits = 2))))





#Genero


#Tratando a base de dados


tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,V2007 == "Mulher"), na.rm = TRUE)
mulher <- tx_inf$ratio




tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,V2007 == "Homem"), na.rm = TRUE)
homem <- tx_inf$ratio


genero <- c("Mulher", "Homem")
inf <- c(homem,mulher)
Genero <- data.table(Gênero = genero, Tx_inf = inf)

#Gerando o gráfico

ggplot(Genero, aes(x = fct_reorder(Gênero ,inf), y = inf)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de informalidade Brasil, por gênero(2º trimestre 2022)",
  caption = "Fonte: PNAD contínua. Nota: O número de trabalhadores informais é composto por trabalhadores sem carteira assinada, empregadores e conta própria sem CNPJ, além de trabalhadores familiares auxiliares.")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.background = element_rect("transparent"),
        legend.key = element_rect("transparent")
  )  + coord_flip() +geom_text(hjust = -0.5,
                               position = position_dodge(width = 1),
                               inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(inf, digits = 2))))






#Educação




#Tratando a base de dados


tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,VD3004 == "Fundamental completo ou equivalente"), na.rm = TRUE)


educ_fund <- tx_inf$ratio

tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,VD3004 == "Médio completo ou equivalente"), na.rm = TRUE)


educ_medio <- tx_inf$ratio



tx_inf <- svyratio(~VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                     VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"|
                     VD4009 == "Empregador" & V4019 == "Não"|
                     VD4009 == "Conta-própria" & V4019 == "Não"|
                     VD4009 == "Trabalhador familiar auxiliar",~VD4002,subset(dadosPNADc,VD3004 == "Superior completo"), na.rm = TRUE)


educ_supe <- tx_inf$ratio



educ <- c("Fundamental Completo", "Médio Completo","Superior Completo")
inf <- c(educ_fund,educ_medio,educ_supe)
educa <- data.table(Educação = educ, Tx_inf = inf)


#Gerando o gráfico

ggplot(educa , aes(x = fct_reorder(educ ,inf), y = inf)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de informalidade Brasil, por educação(2º trimestre 2022)",
  caption = "Fonte: PNAD contínua. Nota: O número de trabalhadores informais é composto por trabalhadores sem carteira assinada, empregadores e conta própria sem CNPJ, além de trabalhadores familiares auxiliares.")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.background = element_rect("transparent"),
        legend.key = element_rect("transparent")
  )  + coord_flip() +geom_text(hjust = -0.5,
                               position = position_dodge(width = 1),
                               inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(inf, digits = 2))))






##### Gap salarial entre setor formal e informal





modeloLin <- svyglm(formula=log(VD4016)~V2007+V2009+V2009^2+VD3004+V2010+VD4010, design=dadosPNADc)
summary(object=modeloLin)



#V2010+VD4013+VD3004


##### Comparação internacional


#Baixando e tratando a base

dt <- read_excel("informal-economy-database.xlsx", sheet = "SEMP_p")

setDT(dt)
#dt<- dt[,.(Economy,Code,`2018`)]

OECD_and_Brazil <- c("Brazil","Austria","Australia","Belgium","Canada","Czech Republic","Denmark",
                     "Estonia","Finland","France","Germany","Greece","Hungary",
                     "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea, Rep.", "Latvia", "Lithuania", "Luxembourg", 
                      "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic",
                     "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom","United States")
latin_america <- c("Guatemala","Honduras","Paraguay","Brazil","Peru","Uruguay","Venezuela, RB","Chile","Colombia","Costa Rica","Mexico")

#Gerando o gráficos

ggplot(dt[Economy %in% OECD_and_Brazil,], aes(x = fct_reorder(Economy,`2018`), y = `2018`)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de informalidade Brasil + OCDE(2018)",
  caption = "Fonte: Informal Economy Database. Legenda: OCDE = Organização para a Cooperação e Desenvolvimento Econômico. Nota: Foi utilizado a proporção de trabalhadores autônomos com relação ao número de trabalhadores totais 
como proxy de taxa informalidade. Foi retirado da amostra os países da américa latina que pertencem a OCDE")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.background = element_rect("transparent"),
        legend.key = element_rect("transparent")
  )  + coord_flip() +geom_text(hjust = -0.5,
                               position = position_dodge(width = 1),
                               inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(`2018`, digits = 2))))



ggplot(dt[Economy %in% latin_america,], aes(x = fct_reorder(Economy,`2018`), y = `2018`)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de informalidade Brasil + América Latina(2018)",
  caption = "Fonte: Informal Economy Database. Legenda: OCDE = Organização para a Cooperação e Desenvolvimento Econômico. Nota: Foi utilizado a proporção de trabalhadores autônomos com relação ao número de trabalhadores totais como proxy de taxa informalidade.")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.background = element_rect("transparent"),
        legend.key = element_rect("transparent")
  )  + coord_flip() +geom_text(hjust = -0.5,
                               position = position_dodge(width = 1),
                               inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(`2018`, digits = 2))))


