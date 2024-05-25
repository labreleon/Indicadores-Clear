### Leon Labre
### 22/09/2022


setwd("D:/Users/leonl/Desktop/Clear/Applied Micro/Crime")

### Cleaning the R environment
rm(list=ls())




#Instação do pacote para  download dos microdados do DataSUS
install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)


### Load Packages (and install packages if needed)
load.lib <- c("data.table","ggplot2","forcats","readxl","maps","geobr","sf","dplyr","basedosdados",
              "PNADcIBGE","survey","remotes","microdatasus")

### Instaling and loading packages
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)



#### Baixando e tratando as bases de dados principais



#Escolhendo os dados de interesse
dados <- fetch_datasus(year_start = 2009, year_end = 2020,information_system = "SIM-DO",vars = c("CAUSABAS","DTOBITO","SEXO","RACACOR","IDADE","CODMUNRES"))
dados <- process_sim(dados)

#Limpeza da base
dados$CAUSABAS <- as.character(dados$CAUSABAS)
dados$CAUSABAS <-  substring(dados$CAUSABAS,1, nchar(dados$CAUSABAS)-1)
dados$DTOBITO <- as.character(dados$DTOBITO)
#dados$DTOBITO <- gsub("^.{0,4}", "", dados$DTOBITO)
dados$DTOBITO <-  substring(dados$DTOBITO,1, nchar(dados$DTOBITO)-6)
setDT(dados)
  
#O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35-Y36
#ou seja: óbitos causados por agressão mais intervenção legal. O número de mortes por causa indeterminada na UF de residência foi obtido pela Causa CID-BR-10: 111, ou seja: eventos cuja intenção é
#indeterminada.



dados <- dados[CAUSABAS == "X85"|CAUSABAS == "X86"|CAUSABAS == "X87"|CAUSABAS == "X88"|CAUSABAS == "X89"|CAUSABAS == "X90"|CAUSABAS == "X91"
               |CAUSABAS == "X92"|CAUSABAS == "X93"|CAUSABAS == "X94"|CAUSABAS == "X95"|CAUSABAS == "X96"|CAUSABAS == "X97"|CAUSABAS == "X98"|CAUSABAS == "X99"|
                 CAUSABAS == "Y00" |CAUSABAS == "Y01" |CAUSABAS == "Y02"|CAUSABAS == "Y03" |CAUSABAS == "Y04" |CAUSABAS == "Y05" |CAUSABAS == "Y06" |CAUSABAS == "Y07" |CAUSABAS == "Y08"| CAUSABAS == "Y09"| CAUSABAS == "Y35"|
               CAUSABAS == "Y10" |CAUSABAS == "Y11" |CAUSABAS == "Y12"|CAUSABAS == "Y13" |CAUSABAS == "Y14" |CAUSABAS == "Y15" |CAUSABAS == "Y16" |CAUSABAS == "Y17" |CAUSABAS == "Y18"| CAUSABAS == "Y19"| CAUSABAS == "Y20"|
               CAUSABAS == "Y21" |CAUSABAS == "Y22" |CAUSABAS == "Y23"|CAUSABAS == "Y24" |CAUSABAS == "Y25" |CAUSABAS == "Y26" |CAUSABAS == "Y27" |CAUSABAS == "Y28" |CAUSABAS == "Y29"| CAUSABAS == "Y30"| CAUSABAS == "Y31"|
               CAUSABAS == "Y32"| CAUSABAS == "Y33"| CAUSABAS == "Y34"|CAUSABAS == "Y36"]
fwrite(dados,"homicidiomaismvci_dados")


#O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35-Y36
#ou seja: óbitos causados por agressão mais intervenção legal.
dados <- dados[CAUSABAS == "X85"|CAUSABAS == "X86"|CAUSABAS == "X87"|CAUSABAS == "X88"|CAUSABAS == "X89"|CAUSABAS == "X90"|CAUSABAS == "X91"
               |CAUSABAS == "X92"|CAUSABAS == "X93"|CAUSABAS == "X94"|CAUSABAS == "X95"|CAUSABAS == "X96"|CAUSABAS == "X97"|CAUSABAS == "X98"|CAUSABAS == "X99"|
                 CAUSABAS == "Y00" |CAUSABAS == "Y01" |CAUSABAS == "Y02"|CAUSABAS == "Y03" |CAUSABAS == "Y04" |CAUSABAS == "Y05" |CAUSABAS == "Y06" |CAUSABAS == "Y07" |CAUSABAS == "Y08"| CAUSABAS == "Y09"| CAUSABAS == "Y35"|
                 CAUSABAS == "Y36"]

fwrite(dados,"base_dados")





####################### Grafico 1

##Baixando e tratando a base de dados


dt <- fread("homicidiomaismvci_dados")

dt <- dt[CAUSABAS == "Y10" |CAUSABAS == "Y11" |CAUSABAS == "Y12"|CAUSABAS == "Y13" |CAUSABAS == "Y14" |CAUSABAS == "Y15" |CAUSABAS == "Y16" |CAUSABAS == "Y17" |CAUSABAS == "Y18"| CAUSABAS == "Y19"| CAUSABAS == "Y20"|
           CAUSABAS == "Y21" |CAUSABAS == "Y22" |CAUSABAS == "Y23"|CAUSABAS == "Y24" |CAUSABAS == "Y25" |CAUSABAS == "Y26" |CAUSABAS == "Y27" |CAUSABAS == "Y28" |CAUSABAS == "Y29"| CAUSABAS == "Y30"| CAUSABAS == "Y31"|
           CAUSABAS == "Y32"| CAUSABAS == "Y33"| CAUSABAS == "Y34",tipo:= "MVCI"]

dt <- dt[CAUSABAS != "Y10" & CAUSABAS != "Y11" & CAUSABAS != "Y12"& CAUSABAS != "Y13" &CAUSABAS != "Y14" &CAUSABAS != "Y15" &CAUSABAS != "Y16" &CAUSABAS != "Y17" &CAUSABAS != "Y18"& CAUSABAS != "Y19"& CAUSABAS != "Y20"&
           CAUSABAS != "Y21" &CAUSABAS != "Y22" &CAUSABAS != "Y23"&CAUSABAS != "Y24" &CAUSABAS != "Y25" &CAUSABAS != "Y26" &CAUSABAS != "Y27" &CAUSABAS != "Y28" &CAUSABAS != "Y29"& CAUSABAS != "Y30"& CAUSABAS != "Y31"&
           CAUSABAS != "Y32"& CAUSABAS != "Y33"& CAUSABAS != "Y34",tipo:= "A + IL"]


dt <- dt[, .(obs = .N), by = c("DTOBITO","tipo")]


##Gerando o gráfico

ggplot(data = dt, aes(x = DTOBITO,y = obs,color = tipo,size = tipo)) + geom_line(size = 1)  + scale_x_continuous(breaks = c(2009,2011,2013,2015,2017,2019,2020)) +scale_size_manual(values = c(rep(1.3, 7), 1.5, rep(0.5, dim(dt)[1]-6))) +
  theme_bw(base_size = 25) +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  ) +
  labs(plot.caption = element_text(hjust = 0),
    x = "", y = "Número de Homicídios",
    title = "Brasil: Número de Homicídios (2009 a 2020)",
    color = "",
    caption = "Fonte: Sistema de Informações sobre Mortalidade – SIM. Legenda: MVCI = Mortes Violentas por Causa Indeterminada;
A+IL = Agressão + intervenção legal.")

####Grafico 2

## Tratando a base de dados


dt <- dt[DTOBITO >2010,]

df <- read_excel("anuario_dados.xlsx", range = "A1:D309")
setDT(df)
df <- df[Estados == "Brasil",]
df <- df[Ano != 2021 ,]
df <- df[,tipo:= "MVI"]
df <- df[,Taxas:= NULL]
df <- df[,Estados := NULL]


setnames(dt, "DTOBITO", "Ano")
setnames(dt, "obs", "Valor")
dt <- rbind(dt, df)



ggplot(data = dt, aes(x = Ano,y = Valor,color = tipo,size = tipo)) + geom_line(size = 1)  + scale_x_continuous(breaks = c(2009,2011,2013,2015,2017,2019,2020)) +scale_size_manual(values = c(rep(1.3, 7), 1.5, rep(0.5, dim(dt)[1]-6))) +
  theme_bw(base_size = 25) +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  ) +
  labs(
    x = "", y = "Número de Homicídios",
    title = "Brasil: Número de Homicídios (2011 a 2020)",
    color = "",
    caption = "Fonte: Sistema de Informações sobre Mortalidade – SIM e Anuário Brasileiro de Segurança Pública. Legenda: MVCI = 
Mortes Violentas por Causa Indeterminada; A+IL = Agressão + intervenção legal; MVI = Mortes Violentas Intencionais.")



#######Grafico 3

#Baixando e tratando a base de dados


# ibge_populacao.brasil
ds<- fread("brasil.csv")

setDT(ds)

ds <- ds[ano >= 2009 & ano < 2021]
ds <- ds[ano > 2010]



dt <- dt[ds, on = .(Ano = ano)]
dt <- dt[,tax:= Valor*100000/populacao]


#Gerando o gráfico

ggplot(data = dt, aes(x = Ano,y = tax,color = tipo,size = tipo)) + geom_line(size = 1)   + scale_x_continuous(breaks = c(2009,2011,2013,2015,2017,2019,2020)) +scale_size_manual(values = c(rep(1.3, 7), 1.5, rep(0.5, dim(dt)[1]-6))) +
  theme_bw(base_size = 25) +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  ) +
  labs(
x = "", y = "Taxa de Homicídios",
title = "Taxa de Homicídios, por 100 mil habitantes (2011 a 2019)",
color = "",
caption = "Fonte: Sistema de Informações sobre Mortalidade – SIM e Anuário Brasileiro de Segurança Pública. Legenda: MVCI = 
Mortes Violentas por Causa Indeterminada; A+IL = Agressão + intervenção legal; MVI = Mortes Violentas Intencionais")








###### Grafico 4 e 5

### Grafico 4


#Baixando e tratando a base de dados

dados <- read_excel("data_cts_intentional_homicide.xlsx", range = "A3:M76341")

setDT(dados)

mean_la <- dados[Subregion == 'Latin America and the Caribbean' & Country != "Brazil" & `Unit of measurement`== "Rate per 100,000 population" & Year >= 2009 & Indicator == "Victims of intentional homicide"&
                 Sex == "Total" & Dimension == "Total"  ,]


mean_la <- mean_la[ ,list(mean=mean(VALUE)), by=list(Year)]
mean_la <- mean_la[ ,Country := "Latin America"]

OECD_and_Brazil <- c("Brazil","Austria","Australia"," Belgium","Canada","Chile","Colombia","Costa Rica","Czech Republic","Denmark",
                     "Estonia","Finland","France","Germany","Greece","Hungary",
                     "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", 
                     "Mexico", "the Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic",
                     "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom of Great Britain and Northern Ireland", "United States of America")
dados <- dados[dados$Country %in% OECD_and_Brazil & `Unit of measurement`== "Rate per 100,000 population" & Year >= 2009 & Indicator == "Victims of intentional homicide"&
                 Sex == "Total" & Dimension == "Total"  ,]


mean_ocde <- dados[Country != "Brazil" ,list(mean=mean(VALUE)), by=list(Year)]
mean_ocde <- mean_ocde[ ,Country := "OCDE"]
mean_brazil <- dados[Country == "Brazil" ,list(mean=mean(VALUE)), by=list(Year)]
mean_brazil <- mean_brazil[ ,Country := "Brazil"]
data_list <- list(mean_brazil, mean_ocde,mean_la)
big_data <- rbindlist(data_list)
dados$Country <- gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", dados$Country)

#Gerando o gráfico

ggplot(big_data , aes(x = Year, y = mean, color = Country)) +
  geom_line(size = 2) +
  theme_bw(base_size = 25) +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  ) +
  labs(
    x = "", y = "Taxa de Homicídios ",
    color = "",
    title = "Taxa de Homicídios - Comparação internacional (2009 a 2020)",
    caption = "Fonte: United Nations Crime Trends Survey. Nota: Taxa de Homicídios por 100 mil habitantes.") 

#Grafico 5

#Gerando o gráfico

ggplot(dados[Year == 2018,], aes(x = fct_reorder(Country,VALUE), y = VALUE)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de Homicídios Brasil + OCDE, por 100 mil Habitantes(2018)",
  caption = "Fonte: United Nations Crime Trends Survey.Nota: Taxa de Homicídios por 100 mil habitantes.")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  )  + coord_flip() +geom_text(hjust = -0.5,
                               position = position_dodge(width = 1),
                               inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(VALUE, digits = 2))))



######## Grafico 6 e 7


#Baixando e tratando a base de dados


dados <- fread("datable_dados")


copia <- dados[, .(obs = .N), by = c("DTOBITO","munResUf")]
copia <- copia[DTOBITO == 2018,]
setnames(copia, "DTOBITO", "ano")
setnames(copia, "munResUf", "name_state")


# br_ibge_populacao.uf

df<- fread("uf.csv")

setDT(df)

df <- df[ano == 2018 ,]




#Dicionário id_estado com os nomes


var_desc <- c("Acre", "Alagoas", 
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

var_sigla <- c("AC", "AL", 
               "AM", "AM",
               "BA", "CE",
               "DF", "ES",
               "GO","MA",
               "MT","MS",
               "MG","PA",
               "PB","PR",
               "PE","PI",
               "RN","RS",
               "RJ","RO","RR","SC",
               "SP","SE","TO")
var_type <- c(12, 27, 16, 13, 29, 23, 53, 32, 52,21,51,50,31,15,25,41,26,22,24,43,33,11,14,42,35,28,17)


dt_kv <- data.table(name = var_desc,
                    codigo_estado = var_type,sigla = var_sigla)

df <- df[dt_kv, on =.(sigla_uf = sigla)]
copia <- copia[dt_kv, on =.(name_state = name)]
copia <- copia[df, on = .(name_state = name , sigla = sigla_uf ,  codigo_estado = codigo_estado,ano = ano)]


copia <- copia[,tax:= obs*100000/populacao]



#Gráfico 6


#Gerando o gráfico

ggplot(copia, aes(x = fct_reorder(name_state,tax), y = tax)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de Homicídios, por Grupo de 100 mil, por UF (2018)",
  caption = "Fonte: Sistema de Informações sobre Mortalidade – SIM  e IBGE.")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  )  + coord_flip() + geom_text(hjust = -0.5,
                                position = position_dodge(width = 1),
                                inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(tax, digits = 2))))



#Baixando e tratando a base de dados

df <- read_excel("anuario_dados.xlsx", range = "A1:D309")
setDT(df)
df <- df[Estados != "Brasil",]

df <- df[Ano == 2018,]



#Grafico 7


#Gerando o gráfico

ggplot(df, aes(x = fct_reorder(Estados,Taxas), y = Taxas)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de Homicídios, por Grupo de 100 mil, por UF (2018)",
  caption = "Fonte: Anuário Brasileiro de Segurança Pública")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  )  + coord_flip() + geom_text(hjust = -0.5,
                                position = position_dodge(width = 1),
                                inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(Taxas, digits = 2))))

#Grafico 8


#Baixando e tratando a base de dados

df <- read_excel("anuario_dados.xlsx", range = "A1:D309")
setDT(df)
df <- df[Estados != "Brasil",]
df <- df[Ano == 2015 | Ano == 2019,]



myFunc <- function(x) x/shift(x)
cols <- c("Taxas")

df <- df[, paste0("change", cols) := lapply(.SD, myFunc), by = Estados, .SDcols = cols][]
df <- df[,changeTaxas:=changeTaxas-1]
df <- df[, .(Estados,changeTaxas )]
df <- na.omit(df, cols=c("changeTaxas"))


#Gerando o gráfico

ggplot(df, aes(x = fct_reorder(Estados,changeTaxas), y = changeTaxas)) + geom_col(position = "dodge2",fill = "BLUE4", alpha=0.4) + labs(
  x = "", y = "",
  title = "Variação Percentual da Taxa de Homicídios, por Grupo de 100 mil, por UF (2015-2019)",
  caption = "Fonte: Anuário Brasileiro de Segurança Pública")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  )  + coord_flip() + geom_text(hjust = -0.5,
                                position = position_dodge(width = 1),
                                inherit.aes = TRUE ,aes(label=sprintf("%1.1f%%", changeTaxas*100)))







##### Gráfico 9


#Baixando e tratatando a base de dados


states <- read_state(
  year=2018, 
  showProgress = FALSE
)

dados <- fread("datable_dados")
copia <- dados[, .(obs = .N), by = c("DTOBITO","munResUf")]
setnames(copia, "DTOBITO", "ano")
setnames(copia, "munResUf", "name_state")


copia <- copia[ano == 2018,]
abbrev_state <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF","ES","GO","MA","MT","MS","	
MG","PA","PB","PR","PE","	
PI","RJ","RN","RS","RO","RR","SC","SP","SE","	
TO")
copia <- copia[,abbrev_state:=abbrev_state]


# br_ibge_populacao.uf


df<- fread("uf.csv")

setDT(df)

df <- df[ano == 2018]




#Dicionário id_estado com os nomes


var_desc <- c("Acre", "Alagoas", 
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

var_sigla <- c("AC", "AL", 
               "AM", "AM",
               "BA", "CE",
               "DF", "ES",
               "GO","MA",
               "MT","MS",
               "MG","PA",
               "PB","PR",
               "PE","PI",
               "RN","RS",
               "RJ","RO","RR","SC",
               "SP","SE","TO")
var_type <- c(12, 27, 16, 13, 29, 23, 53, 32, 52,21,51,50,31,15,25,41,26,22,24,43,33,11,14,42,35,28,17)


dt_kv <- data.table(name = var_desc,
                    codigo_estado = var_type,sigla = var_sigla)

df <- df[dt_kv, on =.(sigla_uf = sigla)]
copia <- copia[dt_kv, on =.(name_state = name)]
copia <- copia[df, on = .(name_state = name , sigla = sigla_uf ,  codigo_estado = codigo_estado,ano = ano)]



copia <- copia[,tax:= obs*100000/populacao]

states <- left_join(states, copia, by = c("code_state" = "codigo_estado" ))


#Gerando o gráfico

ggplot() +
  geom_sf(data=states,aes(fill = tax), color= "black", size=.15) +
  labs(x = "",y = "", subtitle="Taxa de Homicídios por Grupo de 100 mil, por UF (2018)", size=8,fill = "", caption = "Nota: Mapa gerado à partir de dados do Sistema de infor-
mação de mortalidade(SIM) e IBGE.") +
  scale_fill_viridis_c(option = "plasma")+
  theme_bw(base_size = 25) +
  theme(plot.caption = element_text(hjust = 0),
    axis.text.x = element_blank(),
        axis.text.y = element_blank(),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  ) 








######### Gráfico 10

#Baixando e tratando a base de dados



# Para carregar o dado direto no R
# Defina o seu projeto no Google Cloud
set_billing_id("projeto-333523")
query <- bdplyr("br_ms_populacao.municipio")
df <- bd_collect(query)
setDT(df)

## Dados de 2009-2020

df <- df[ano >= 2009,]

#modificando o id de municipio para id estado(os dois primeiros digitos correnpodem ao ddd de cada estado)

df$id_municipio <-  substring(df$id_municipio,1, nchar(df$id_municipio)-5)

#Dicionário id_estado com os nomes


var_desc <- c("Acre", "Alagoas", 
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
var_type <- c(12, 27, 16, 13, 29, 23, 53, 32, 52,21,51,50,31,15,25,41,26,22,24,43,33,11,14,42,35,28,17)


dt_kv <- data.table(name = var_desc,
                    value = var_type)


df$id_municipio <- as.double(df$id_municipio)
df <- df[dt_kv, on =.(id_municipio  = value)]


df <- df[, .(populacao = sum(populacao)), by = c("sexo","grupo_idade","name","ano")]
df <- df[,tipo_idade:= "total"]
df <- df[grupo_idade == "15-19 anos" |grupo_idade == "20-24 anos"| grupo_idade == "25-29 anos",tipo_idade:= "jovem"]
df <- df[, .(populacao = sum(populacao)), by = c("sexo","ano","tipo_idade")]
df <- df[ano < 2021,]




###
dados <- fread("datable_dados")
dados <- dados[,tipo_idade:= "total"]
dados <- dados[IDADEanos >= 15 & IDADEanos <= 29,tipo_idade:= "jovem"]


dt <- dados[, .(obs = .N), by = c("DTOBITO","SEXO","tipo_idade")]
setnames(dt, "DTOBITO", "ano")
rm(dados)
dt <- dt[SEXO != "",]

dt <- dt[SEXO == "Masculino",sexo := "masculino"]
dt <- dt[SEXO == "Feminino", sexo := "feminino"]

dt <- dt[,SEXO:=NULL]

df <- df[dt, on =.(ano = ano,sexo = sexo,tipo_idade = tipo_idade)]
df <- df[,tax:= obs*100000/populacao]

tx_mulher <- df[sexo == "feminino" & tipo_idade == "total",.(ano,sexo,tax)]
tx_mulher <- tx_mulher[,sexo:= "Mulher"]
tx_homem <- df[sexo == "masculino" & tipo_idade == "total",.(ano,sexo,tax)]
tx_homem <- tx_homem[,sexo:= "Homem"]
tx_homem_jovem <- df[sexo == "masculino" & tipo_idade == "jovem",.(ano,sexo,tax)]
tx_homem_jovem <- tx_homem_jovem[,sexo:= "Homem + Jovem"]


dt <- rbind(tx_mulher,tx_homem)
dt <- rbind(dt,tx_homem_jovem)


#Gerando o gráfico


ggplot(data = dt, aes(x = ano,y = tax,color = sexo,size = sexo)) + geom_line(size = 1)  + scale_x_continuous(breaks = c(2009,2011,2013,2015,2017,2019,2020)) +scale_size_manual(values = c(rep(1.3, 7), 1.5, rep(0.5, dim(dt)[1]-6))) +
  theme_bw(base_size = 25) +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  ) +
  labs(
    x = "", y = "Taxa de Homicídios",
    title = "Brasil:Taxa de Homicídios, por Gênero e idade (2009 a 2020)",
    color = "",
    caption = "Fonte: Sistema de Informações sobre Mortalidade – SIM. Legenda: Jovem = Entre 15 e 29 anos. Nota: Taxa de Homicídios, 
por grupo de 100 mil habitantes. ")













############ Gráfico 11 Taxa de Homicídios de Homens Jovens por Grupo de 100 mil, por UF (2019)


#Baixando e tratando a base de dados

dados <- fread("datable_dados")

dados <- dados[IDADEanos >=15 & IDADEanos <=29 & SEXO ==  "Masculino",]
dados <- dados[,SEXO := NULL]
copia <- dados[, .(obs = .N), by = c("DTOBITO","munResUf")]
copia <- copia[DTOBITO == 2018,]
setnames(copia, "DTOBITO", "ano")
setnames(copia, "munResUf", "name_state")

# Para carregar o dado direto no R
query <- bdplyr("br_ms_populacao.municipio")
df <- bd_collect(query)
setDT(df)

## Dados de 2009-2020

df <- df[ano >= 2009,]

#modificando o id de municipio para id estado(os dois primeiros digitos correnpodem ao ddd de cada estado)

df$id_municipio <-  substring(df$id_municipio,1, nchar(df$id_municipio)-5)

#Dicionário id_estado com os nomes


var_desc <- c("Acre", "Alagoas", 
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
var_type <- c(12, 27, 16, 13, 29, 23, 53, 32, 52,21,51,50,31,15,25,41,26,22,24,43,33,11,14,42,35,28,17)


dt_kv <- data.table(name = var_desc,
                    value = var_type)


df$id_municipio <- as.double(df$id_municipio)
df <- df[dt_kv, on =.(id_municipio  = value)]


df <- df[, .(populacao = sum(populacao)), by = c("sexo","grupo_idade","name","ano")]
df <- df[ano == 2018,]
df <- df[grupo_idade == "15-19 anos" |grupo_idade == "20-24 anos"| grupo_idade == "25-29 anos"]
df <- df[, .(populacao = sum(populacao)), by = c("sexo","name","ano")]
df <- df[sexo == "masculino",]


copia <- copia[df, on = .(name_state = name ,ano = ano)]


copia <- copia[,tax:= obs*100000/populacao]

copia <- setorder(copia, cols = "tax")



#Gerando o gráfico

ggplot(copia, aes(x = fct_reorder(name_state,tax), y = tax)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de Homicídios de Homens Jovens por Grupo de 100 mil, por UF (2018)",
  caption = "Fonte: SIM e IBGE")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  )  + coord_flip() + geom_text(hjust = -0.5,
                                position = position_dodge(width = 1),
                                inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(tax, digits = 2))))




######## Gráfico 12 Taxa de Homicídios por Grupo de 100 mil,por raça por UF (2019)


#Baixando de tratando a base de dados

dados <- fread("datable_dados")


copia <- dados[, .(obs = .N), by = c("DTOBITO","munResUf","RACACOR")]
copia <- copia[DTOBITO == 2018,]
setnames(copia, "DTOBITO", "ano")
setnames(copia, "munResUf", "name_state")
copia <-copia[RACACOR != "",]
copia <-copia[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
copia <-copia[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]

copia <- copia[ , .(total = sum(obs)),by = c("name_state","Raça")]
#copia <- copia[ , .(total = sum(total)),by = c("Raça")]




dadosPNADc <- get_pnadc(year=2018, quarter=4, vars=c("UF","V2009","V2010","V2007"))


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Acre"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
acre <- dt[,name_state := "Acre"]

dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Amazonas"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
amazonas <- dt[,name_state := "Amazonas"]

estados <- rbind(acre,amazonas)
rm(acre,amazonas)

dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Roraima"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Roraima <- dt[,name_state := "Roraima"]

estados <- rbind(estados,Roraima)
rm(Pará)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Pará"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Pará <- dt[,name_state := "Pará"]

estados <- rbind(estados,Pará)
rm(Pará)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Amapá"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Amapá <- dt[,name_state := "Amapá"]

estados <- rbind(estados,Amapá)
rm(Amapá)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Tocantins"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Tocantins <- dt[,name_state := "Tocantins"]

estados <- rbind(estados,Tocantins)
rm(Tocantins)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Maranhão"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Maranhão <- dt[,name_state := "Maranhão"]

estados <- rbind(estados,Maranhão)
rm(Maranhão)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Piauí"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Piauí <- dt[,name_state := "Piauí"]

estados <- rbind(estados,Piauí)
rm(Piauí)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Ceará"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Ceará <- dt[,name_state := "Ceará"]

estados <- rbind(estados,Ceará)
rm(Ceará)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Rio Grande do Norte"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Rio_Grande_do_Norte <- dt[,name_state := "Rio Grande do Norte"]

estados <- rbind(estados,Rio_Grande_do_Norte)
rm(Rio_Grande_do_Norte)



dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Paraíba"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Paraíba<- dt[,name_state := "Paraíba"]

estados <- rbind(estados,Paraíba)
rm(Paraíba)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Pernambuco"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Pernambuco<- dt[,name_state := "Pernambuco"]

estados <- rbind(estados,Pernambuco)
rm(Pernambuco)



dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Alagoas"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Alagoas<- dt[,name_state := "Alagoas"]

estados <- rbind(estados,Alagoas)
rm(Alagoas)



dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Sergipe"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Sergipe<- dt[,name_state := "Sergipe"]

estados <- rbind(estados,Sergipe)
rm(Sergipe)



dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Bahia"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Bahia<- dt[,name_state := "Bahia"]

estados <- rbind(estados,Bahia)
rm(Bahia)



dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Minas Gerais"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Minas_Gerais<- dt[,name_state := "Minas Gerais"]

estados <- rbind(estados,Minas_Gerais)
rm(Minas_Gerais)



dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Rio de Janeiro"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Rio_de_Janeiro<- dt[,name_state := "Rio de Janeiro"]

estados <- rbind(estados,Rio_de_Janeiro)
rm(Rio_de_Janeiro)



dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "São Paulo"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
São_Paulo<- dt[,name_state := "São Paulo"]

estados <- rbind(estados,São_Paulo)
rm(São_Paulo)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Paraná"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Paraná<- dt[,name_state := "Paraná"]

estados <- rbind(estados,Paraná)
rm(Paraná)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Santa Catarina"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Santa_Catarina<- dt[,name_state := "Santa Catarina"]

estados <- rbind(estados,Santa_Catarina)
rm(Santa_Catarina)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Rio Grande do Sul"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Rio_Grande_do_Sul<- dt[,name_state := "Rio Grande do Sul"]

estados <- rbind(estados,Rio_Grande_do_Sul)
rm(Rio_Grande_do_Sul)



dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Mato Grosso do Sul"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Mato_Grosso_do_Sul<- dt[,name_state := "Mato Grosso do Sul"]

estados <- rbind(estados,Mato_Grosso_do_Sul)
rm(Mato_Grosso_do_Sul)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Mato Grosso"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Mato_Grosso<- dt[,name_state := "Mato Grosso"]

estados <- rbind(estados,Mato_Grosso)
rm(Mato_Grosso)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Goiás"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Goiás<- dt[,name_state := "Goiás"]

estados <- rbind(estados,Goiás)
rm(Goiás)


dt <- svytotal(x=~V2010, design=subset(dadosPNADc,UF == "Distrito Federal"), na.rm=TRUE)
Raça <- c("Preta","Não Petra") 
RACACOR <- c("Branca","Preta","Amarela","Parda","Indígena","Ignorado")
dt <- data.table(RACACOR = RACACOR, pop = dt)
dt <- dt[RACACOR == "Parda" | RACACOR == "Preta" ,Raça:= "Preta" ]
dt<-dt[RACACOR == "Branca" | RACACOR == "Indígena"|RACACOR == "Amarela" ,Raça:= "Não Preta" ]
dt <- dt[ , .(pop = sum(pop)),by = c("Raça")]
dt <- na.omit(dt)
Distrito_Federal<- dt[,name_state := "Distrito Federal"]

estados <- rbind(estados,Distrito_Federal)
rm(Distrito_Federal)
rm(dt)



copia <- copia[estados, on = .(name_state = name_state ,Raça= Raça)]


copia <- copia[,tax:= total*100000/pop]

copia <- setorder(copia, cols = "tax")


#Gerando os gráficos


ggplot(copia[Raça == "Preta",], aes(x = fct_reorder(name_state,tax), y = tax)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de Homicídios de Negros por Grupo de 100 mil, por UF (2018)",
  caption = "Fonte: SIM e IBGE. Nota: O número de negros foi obtido somando pardos e pretos,todos os ignorados não entraram nas contas.")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.background = element_rect("transparent"),
        legend.key = element_rect("transparent")
  )  + coord_flip() + geom_text(hjust = -0.5,
                                position = position_dodge(width = 1),
                                inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(tax, digits = 2))))

ggplot(copia[Raça == "Não Preta",], aes(x = fct_reorder(name_state,tax), y = tax)) + geom_col(position = "dodge2",fill = "BLUE4") + labs(
  x = "", y = "",
  title = "Taxa de Homicídios de Não Negros por Grupo de 100 mil, por UF (2018)",
  caption = "Fonte: SIM e IBGE. Nota: O número de não negros se deu pela soma dos brancos, amarelos e indígenas, todos os ignorados não entraram nas contas.")+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.background = element_rect("transparent"),
        legend.key = element_rect("transparent")
  )  + coord_flip() + geom_text(hjust = -0.5,
                                position = position_dodge(width = 1),
                                inherit.aes = TRUE ,aes(label=sprintf("%0.2f", round(tax, digits = 2))))

