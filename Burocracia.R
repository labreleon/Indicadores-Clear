### Leon Labre
### 22/09/2022


setwd("D:/Users/leonl/Desktop/Clear/Applied Micro/Public")


### Cleaning the R environment
rm(list=ls())


### Load Packages (and install packages if needed)
load.lib <- c("data.table","foreign","ggplot2","fixest","ggrepel","PNADcIBGE","survey","dplyr")

### Instaling and loading packages
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)


#### Fato 1: Persistência e clustering da qualidade da burocracia

#Baixando e tratando a base

dt <- fread("V-Dem-CY-Full+Others-v12.csv")
dt <- dt[,c("country_name","country_text_id","country_id","year","v2clrspct","v2stcritrecadm","e_gdppc","e_regionpol_6C")]

dt <- dt[year >=1900,]
dt <- na.omit(dt)

dt <- dt[,mean:=(v2clrspct+v2stcritrecadm)/2]
d <- dt$mean - min(dt$mean)
e <- max(dt$mean) - min(dt$mean)
f <- d/e
dt <- dt[,z:=f]

dt <- dt[, .(z = mean(z)), by = c("e_regionpol_6C","year")]
dt$e_regionpol_6C <- as.factor(dt$e_regionpol_6C)




dt <- dt[e_regionpol_6C == 1, name:= "Eastern Europe and Central Asia"]
dt <- dt[e_regionpol_6C == 2, name:= "Latin America"]
dt <- dt[e_regionpol_6C == 3, name:= "MENA"]
dt <- dt[e_regionpol_6C == 4, name:= "Sub-Saharan Africa"]
dt <- dt[e_regionpol_6C == 5, name:= "Europe and North America"]
dt <- dt[e_regionpol_6C == 6, name:= "Asia and Pacific"]




#Gerando o gráfico

ggplot(data = dt, aes(x = year,y = z,color = name,size = name)) + geom_line(size = 1)  + scale_x_continuous(breaks = c(1900,1920,1940,1960,1980,2000,2020))+scale_y_continuous(limits = c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1.0))+scale_size_manual(values = c(rep(1.3, 7), 1.5, rep(0.5, dim(dt)[1]-6))) +
  theme_bw(base_size = 25) +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(0.99, 0.4),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  ) +
  labs(
    x = "", y = "Média do score de Burocracia",
    title = "Fato 1: Persistência e clustering da qualidade da burocracia",
    color = "",
    caption = "Fonte: Varieties of Democracy (VDem) project. Legenda: Score de Burocracia = Recrutamento meritocrático 
+ Administração rigorosa e imparcial.")



#### Fato 2: Desenvolvimento e qualidade da burocracia são positivamente correlacionados

#Baixando e tratando a base

dt <- fread("V-Dem-CY-Full+Others-v12.csv")
dt <- dt[,c("country_name","country_text_id","country_id","year","v2clrspct","v2stcritrecadm","e_gdppc","e_regionpol_6C")]

oil <- c("Kuwait","Qatar","United Arab Emirates","Iraq","Iran","Algeria","Angola","Republic of the Congo","Equatorial Guinea",
         "Gabon","Libya","Nigeria","Saudi Arabia", "Venezuela","Bahrain","Oman")

#oil <- c("COD","MDG","GMB","ETH","AFG","LBR","MOZ","HTI","SLE","MWI","MLI","GIN","ZWE","KEN",
 #        "LAO","LSO","SLB","NIC","HND","EGY","BOL","GTM","TLS","SLV","CPV","FJI","PRY","THA",
  #      "CUB","DOM","MDV","CHN","SUR","BGR","MEX","RUS","TUR","GRC","NPL","BEN","STP","MMR",
   #     "CIV","INC","PHL","VUT","BTN","MAR","GUY","LKA","IDN","TUN","JAM","PER","ZAF","COL","ROU",
    #    "BRA","MUS","ARG","CRI","URY","CHL","BRB","TTO","OMN","MLT","PRT","KOR","CYP","ESP","ITA",
     #   "NLZ","GBR","FRA","AUT","JPN","BEL","DEU","CHE","NOR","LUX","SGP","DNK","USA","NLD","CAN",
      #  "SWE","AUS","FIN")


`%notin%` <- Negate(`%in%`)
dt <- dt[country_name %notin% oil]
#dt <- dt[country_name %in% oil]
dt <- na.omit(dt)

dt <- dt[,mean:=(v2clrspct+v2stcritrecadm)/2]
d <- dt$mean - min(dt$mean)
e <- max(dt$mean) - min(dt$mean)
f <- d/e
dt <- dt[,z:=f]

dt <- dt[year == "2016",]





oil <- c("Argentina","Brazil","Mexico","United States of America","France","Germany","China","Japan","Luxembourg")


#Gerando o gráfico

ggplot(data = dt,
            mapping = aes(x = z , y = e_gdppc))+geom_point(size=2.5,alpha = 0.4,color = ifelse(dt$country_name  %in% oil, "blue4", "grey50")) +
  geom_smooth(method = "lm",se =FALSE)+
  theme_bw(base_size = 25) +
  theme(plot.caption = element_text(hjust = 0),
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_rect("transparent"),
    legend.key = element_rect("transparent")
  ) +
  labs(
    x = "Média do score de Burocracia", y = "PIB per capita (milhares)",
    title = "Fato 2: Desenvolvimento e qualidade da burocracia são positivamente correlacionados",
    color = "",
    caption = "Fonte: Varieties of Democracy (VDem) project. Obs: Relação cross-sectional entre o (log) do PIB per capita e a média 
do score de burocracia em 2016. Os Países da OPEC foram retirados dessa amostra.")+geom_label_repel(data = dt[country_name %in% oil,],aes(label = country_text_id),
                                                                                box.padding   = 0.35, 
                                                                                point.padding = 0.5,
                                                                                segment.color = 'grey50')




#### Fato 3: Aumento da capacidade da burocracia é positivamente correlacionado com crescimento econômico.


#Baixando e tratando a base


dt <- fread("V-Dem-CY-Full+Others-v12.csv")
dt <- dt[,c("country_name","country_text_id","country_id","year","v2clrspct","v2stcritrecadm","e_gdppc","e_regionpol_6C")]

oil <- c("Kuwait","Qatar","United Arab Emirates","Iraq","Iran","Algeria","Angola","Republic of the Congo","Equatorial Guinea",
         "Gabon","Libya","Nigeria","Saudi Arabia", "Venezuela","Bahrain","Oman")

`%notin%` <- Negate(`%in%`)
dt <- dt[country_name %notin% oil]


dt <- dt[year >= 1900,]



#decades <- c(1900,1905,1910,1915,1920,1925, 1930,1935, 1940,1945,1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015)
#dt$decade<- decades[findInterval(dt$year, decades)]

#dt <- dt[, list(e_gdppc = mean(e_gdppc,na.rm=TRUE),v2clrspct= mean(v2clrspct,na.rm=TRUE),v2stcritrecadm = mean(v2stcritrecadm,na.rm=TRUE)), by = c("decade","country_name")]

dt <- dt[,z:=(v2clrspct+v2stcritrecadm)/2]

d <- dt$z - mean(dt$z,na.rm = TRUE)
e <- sd(dt$z,na.rm = TRUE)
f <- d/e
dt <- dt[,z:=f]


dt[, lag.gdp:=c(NA, e_gdppc[-.N]), by=country_name]
dt[, lag.z:=c(NA, z[-.N]), by=country_name]



d <- dt$v2clrspct - mean(dt$v2clrspct,na.rm = TRUE)
e <- sd(dt$v2clrspct,na.rm = TRUE)
f <- d/e
dt <- dt[,v2clrspct:=f]


d <- dt$v2stcritrecadm - mean(dt$v2stcritrecadm,na.rm = TRUE)
e <- sd(dt$v2stcritrecadm,na.rm = TRUE)
f <- d/e
dt <- dt[,v2stcritrecadm:=f]

dt[, lag.v2clrspct:=c(NA, v2clrspct[-.N]), by=country_name]
dt[, lag.v2stcritrecadm:=c(NA,v2stcritrecadm[-.N]), by=country_name]



#dt <- dt[,year:=dt$decade]

#Rodando as regressões


reg<- feols(log(e_gdppc) ~ lag.v2clrspct +lag.gdp | year + country_name,  data = dt)
etable(reg,cluster = "country_name" )


reg<- feols(log(e_gdppc) ~ lag.v2stcritrecadm +lag.gdp | year + country_name,  data = dt)
etable(reg,cluster = "country_name" )


reg<- feols(log(e_gdppc) ~ lag.v2clrspct +lag.v2stcritrecadm +lag.gdp | year + country_name,  data = dt)
etable(reg,cluster = "country_name" )




reg<- feols(log(e_gdppc) ~ lag.z +lag.gdp | year + country_name,  data = dt)
etable(reg,cluster = "country_name" )



#####Prêmio salarial setor Público



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


# Baixando e tratando a base de dados
dadosPNADc <- get_pnadc(year=2022, quarter=2, vars=c("UF","VD4010","V4029","VD4002","V4019","VD4009","V2007","V2010","VD3004","V2009","V4010","VD4016","VD4013","V4014"))

dadosPNADc$variables <- dadosPNADc$variables %>% mutate(publico = case_when(VD4009 == "Empregado no setor público com carteira de trabalho assinada" ~ 1,
                                                                            VD4009 == "Empregado no setor privado com carteira de trabalho assinada" ~ 0,
                                                                            TRUE ~ NA_real_))

#Rodando a regressão

modeloLin <- svyglm(formula=log(VD4016)~V2007+V2009+V2009^2+VD3004+V2010+VD4010+publico, design=dadosPNADc)
summary(object=modeloLin)


