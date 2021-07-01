df<-read.csv('owid_g1.csv')
df

summary(df) #tem 26 atributos

str(df) #tipos das variáveis:character - iso_code continent
#integer - total_cases total_deaths total_tests population
#numeric - total_cases_per_million  total_death_per_million reproduction_rate
#total_tests_per_thousand positive_rate tests_per_case   stringency_index
#population_density   median_age    aged_70_older gdp_per_capita   extreme_poverty
#cardiovasc_death_rate diabetes_prevalence female_smokers    male_smokers
#handwashing_facilities hospital_beds_per_thousand life_expectancy human_development_index

View(df) #ver o dataframe em tabela


df1<-df[-13,] #omitimos a linha 13 porque tinha muitos NA's
df1

View(df1) #ver o novo dataframe em tabela

total_casos<-df1$total_cases #colocamos a variavel total_cases num dataframe
sd(total_casos) #desvio-padrao


boxplot(total_casos) #ver o Total_Casos dos 2 continentes



dfTotalAsia<-df1[which(df1$continent == 'Asia'),] #colocamos nesta variavel apenas os resultados da Asia
dfTotalAsia
boxplot(dfTotalAsia$total_cases)
View(dfTotalAsia) #ver o dataframe em tabela

dfTotalOceania<-df2[which(df2$continent == 'Oceania'),] #colocamos nesta variavel apenas os resultados da Oceania
dfTotalOceania
View(dfTotalOceania) #ver o dataframe em tabela
linhas<-c(5,11,13,29,33,42) #colocamos estas linhas num vetor

dfAsiaSemOutliers<-dfTotalAsia[-linhas,] #omitimos as linhas dos outliers
dfAsiaSemOutliers
View(dfAsiaSemOutliers) #ver o novo dataframe em tabela
boxplot(dfAsiaSemOutliers$total_cases) #ver o total de casos da Asia

boxplot(dfTotalOceania$total_cases) #ver o total de casos da Oceania

#Densidade Populacional Obs: Executar o dfTotalAsia novamente

boxplot(dfTotalAsia$population_density) #ver a densidade populacional da Asia, com outliers

linhas1<-c(4,5,22,24,30,34,35)  #colocamos estas linhas num vetor, os outliers

dfAsiaSemOutliersDP<-dfTotalAsia[-linhas1,] #omitimos as linhas dos outliers
dfAsiaSemOutliersDP
View(dfAsiaSemOutliersDP) #ver o dataframe em tabela
boxplot(dfAsiaSemOutliersDP$population_density) #ver a densidade populacional da Asia

boxplot(dfTotalOceania$population_density) #ver a densidade populacional da Oceania



summary(df1) #ver as estatisticas do dataframe, média, mediana, etc

populacao<-df1$population



dispersao4<-ggplot(data=df1,aes(x=population, y=total_cases))+geom_point(aes(color=continent))
dispersao4
dispersao4+xlab("População")+ylab("Total de Casos")+ggtitle("Grafico de dispersao")


dispersao5<-ggplot(data=df_semIndiaChina,aes(x=population, y=total_cases))+geom_point(aes(color=continent))
dispersao5
dispersao5+xlab("População")+ylab("Total de Casos")+ggtitle("Grafico de dispersao")

plot(populacao,total_casos) #ver o grafico de total_casos por populacao
cor(populacao,total_casos) #ver a correlacao, tem uma correlacao forte (usa o método de Pearson por defeito)
#escolhemos as variáveis mais pertinentes, populacao e total_casos
?cor
cor(populacao,total_casos, method="pearson") #calculo do coeficiente de pearson

cor(populacao,total_casos, method="spearman") #calculo do coeficiente de spearman

cor(populacao,total_casos, method="kendall") #calculo do coeficiente de kendall

modelo_reg<-lm(populacao~total_casos) #coeficiente de regressão linear 
modelo_reg

summary(modelo_reg)

abline(modelo_reg) #para visualizar a reta da regressão


library(ggplot2)

#######Total de Casos por homens fumadores 
?ggplot


#para visualizar o total de casos por fumadores homens
ggplot(data=df1,aes(x=male_smokers, y=total_cases))+geom_point()

#para podermos acrescentar elementos ao grafico, podemos guarda-lo numa variavel
dispersao<-ggplot(data=df1,aes(x=male_smokers, y=total_cases))+geom_point()
dispersao

#para acrescentar a cor relativamente aos casos totais a que continente pertencem

dispersao1<-ggplot(data=df1,aes(x=male_smokers, y=total_cases))+geom_point(aes(color=continent))
dispersao1
dispersao1+xlab("Total de Casos")+ylab("Homens fumadores")+ggtitle("Grafico de dispersao")

#######Total de Casos por mulheres fumadoras 

#para visualizar o total de casos por mulheres fumadoras
ggplot(data=df_semIndia,aes(x=female_smokers, y=total_cases))+geom_point()

#para podermos acrescentar elementos ao grafico, podemos guarda-lo numa variavel
dispersao2<-ggplot(data=df_semIndia,aes(x=female_smokers, y=total_cases))+geom_point()
dispersao2

#para acrescentar a cor relativamente aos casos totais a que continente pertencem

dispersao2<-ggplot(data=df_semIndia,aes(x=female_smokers, y=total_cases))+geom_point(aes(color=continent))
dispersao2
dispersao2+xlab("Percentagem de Mulheres fumadoras")+ylab("Total de Casos")+ggtitle("Total de Casos vs. Mulheres Fumadoras")

#######Total de Testes por Total de Casos

#para visualizar o total de casos por mulheres fumadoras
ggplot(data=df_semIndia,aes(x=total_tests, y=total_cases))+geom_point()

#para podermos acrescentar elementos ao grafico, podemos guarda-lo numa variavel
dispersao3<-ggplot(data=df_semIndia,aes(x=total_tests, y=total_cases))+geom_point()
dispersao3

#para acrescentar a cor relativamente aos casos totais a que continente pertencem

dispersao3<-ggplot(data=df_semIndia,aes(x=total_tests, y=total_cases))+geom_point(aes(color=continent))
dispersao3
dispersao3+xlab("Total de Testes")+ylab("Total de Casos")+ggtitle("Grafico de dispersao")
#como existem muitos NA's no total de testes, no gráfico esses valores ficam a 0

df_semIndia<-df1[-13,]
df_semIndia

View(df_semIndia)

dispersao4<-ggplot(data=df_semIndia,aes(x=df_semIndia$population, y=df_semIndia$total_cases, z=df_semIndia$total_deaths_per_million))+geom_point(aes(color=continent))
dispersao4
dispersao2+xlab("Total de Casos")+ylab("Mulheres fumadoras")+ggtitle("Grafico de dispersao")
View(df_semIndia)



hist(total_casos) #histograma da variavel total_cases

head(df1) #ver as primeiras linhas

ks.test(df1$population,df1$total_cases) #para testar a normalidade da distribuicao
#como o p-value é inferior ao nível de significancia escolhido (1%) não se aplica o teste de Levene

anova_test1<-aov(total_cases~population, data = df1) #ANOVA-test
anova_test1
summary(anova_test1) # rejeita-se a hipótese nula, p-value < 0.01




#Mann_Whitney
df2<-data.frame(df1$total_cases,df1$population) #colocamos as 2 variaveis num dataframe
df2

colnames(df2)<-c('total_cases','population') #mudar os nomes das variaveis
df2

boxplot(df2$total_cases,df2$population)

wilcox.test(df2$total_cases, df2$population) #o p-value é inferior ao nível de significancia

df_semIndiaChina<-df_semIndia[-10,]
df_semIndiaChina

library(scatterplot3d)

with(df_semIndia,{
  s3d<-scatterplot3d(
    x=df_semIndia$total_cases,
    y=df_semIndia$population,
    z=df_semIndia$total_deaths,
    color="red",
    pch=19, #formata os pontos de uma certa forma 
    type="h", # desenhar retas verticais que representam a projecao dos pontos
    main="3D Scatterplot COVID-19", #titulo
    xlab="Total de Casos", # legendar eixo xx
    ylab="População",
    zlab="Total de Mortes")
  #conversao de coordenadas 3D para 2D (para a projecao)
  coords<-s3d$xyz.convert(df_semIndia$total_cases,df_semIndia$population, df_semIndia$total_deaths )
  #para representar texto (etiquetas) junto aos pontos
  text(coords$x, coords$y, labels=row.names(df_semIndia), cex=0.8, pos=4)
})


library(scatterplot3d)

with(df_semIndiaChina,{
  s3d<-scatterplot3d(
    x=df_semIndiaChina$total_cases,
    y=df_semIndiaChina$population,
    z=df_semIndiaChina$total_deaths,
    color="red",
    pch=19, #formata os pontos de uma certa forma 
    type="h", # desenhar retas verticais que representam a projecao dos pontos
    main="3D Scatterplot COVID-19", #titulo
    xlab="Total de Casos", # legendar eixo xx
    ylab="População",
    zlab="Total de Mortes")
  #conversao de coordenadas 3D para 2D (para a projecao)
  coords<-s3d$xyz.convert(df_semIndiaChina$total_cases,df_semIndiaChina$population, df_semIndiaChina$total_deaths )
  #para representar texto (etiquetas) junto aos pontos
  text(coords$x, coords$y, labels=row.names(df_semIndiaChina), cex=0.8, pos=4)
})
dfAsiaSemOutliersChina<-dfAsiaSemOutliers[-8,] #para removar o outlier China
dfAsiaSemOutliersChina

library(scatterplot3d)

with(dfAsiaSemOutliersChina,{
  s3d<-scatterplot3d(
    x=dfAsiaSemOutliersChina$total_cases,
    y=dfAsiaSemOutliersChina$population,
    z=dfAsiaSemOutliersChina$total_deaths,
    color="red",
    pch=19, #formata os pontos de uma certa forma 
    type="h", # desenhar retas verticais que representam a projecao dos pontos
    main="3D Scatterplot COVID-19", #titulo
    xlab="Total de Casos", # legendar eixo xx
    ylab="População",
    zlab="Total de Mortes")
  #conversao de coordenadas 3D para 2D (para a projecao)
  coords<-s3d$xyz.convert(dfAsiaSemOutliersChina$total_cases,dfAsiaSemOutliersChina$population, dfAsiaSemOutliersChina$total_deaths )
  #para representar texto (etiquetas) junto aos pontos
  text(coords$x, coords$y, labels=row.names(dfAsiaSemOutliersChina), cex=0.8, pos=4)
})

df_semIndiaChina

plot(df_semIndiaChina$total_cases,df_semIndiaChina$reproduction_rate)

dispersao_casospopulacaoidade<-ggplot(data=df_semIndiaChina,aes(population_density,total_cases))+geom_point(aes(color=median_age))+geom_text(aes(label=iso_code),hjust=1.2, vjust=1)+theme_bw() #para visualizar um gráfico de dispersão
dispersao_casospopulacaoidade+xlab("Densidade Populacional")+ylab("Total de Casos")+ggtitle("Gráfico de dispersão")+labs(colour="Mediana das Idades")

ks.test(df1$population_density,df1$total_cases) #para testar a normalidade da distribuicao

anova_test<-aov(total_cases~total_tests, data = df_semIndiaChina)
anova_test
summary(anova_test) 

plot(df_semIndiaChina$human_development_index,df_semIndiaChina$cardiovasc_death_rate) #para visualizar num gráfico de dispersão


modelo_reg_multi<-lm(df_semIndiaChina$human_development_index~df_semIndiaChina$cardiovasc_death_rate+df_semIndiaChina$total_deaths+df_semIndiaChina$cardiovasc_death_rate*df_semIndiaChina$human_development_index) #coeficiente de regressão linear 
modelo_reg_multi
summary(modelo_reg_multi) # para ver o R2


plot(df_semIndiaChina$total_cases,df_semIndiaChina$human_development_index)
modelo_reg_multi1<-lm(df_semIndiaChina$total_cases~df_semIndiaChina$human_development_index) #coeficiente de regressão linear 
modelo_reg_multi1
summary(modelo_reg_multi1) # para ver o R2


dispersao_humantotal<-ggplot(data=df_semIndiaChina,aes(human_development_index,cardiovasc_death_rate))+geom_point(aes(color=continent))
dispersao_humantotal+xlab("Índice de Desenvolvimento Humano")+ylab("Percentagem de mortes por doenças cardiovasculares")+ggtitle("Gráfico de dispersão")


View(df_semIndiaChina) # para visualizar o dataframe

library(scatterplot3d)

with(df_semIndiaChina,{
  s3d<-scatterplot3d(
    x=df_semIndiaChina$total_cases,
    y=df_semIndiaChina$population_density,
    z=df_semIndiaChina$hospital_beds_per_thousand,
    color="red",
    pch=19, #formata os pontos de uma certa forma 
    type="h", # desenhar retas verticais que representam a projecao dos pontos
    main="3D Scatterplot COVID-19", #titulo
    xlab="Total de Casos", # legendar eixo xx
    ylab="Densidade Populacional",
    zlab="Camas do Hospital por mil")
  #conversao de coordenadas 3D para 2D (para a projecao)
  coords<-s3d$xyz.convert(df_semIndiaChina$total_cases,df_semIndiaChina$population_density, df_semIndiaChina$median_age )
  #para representar texto (etiquetas) junto aos pontos
  text(coords$x, coords$y, labels=row.names(df_semIndiaChina), cex=0.8, pos=4)
})
df_semIndiaChina




#########Analise de Clusters##############

df_2variaveis<-data.frame(df_semIndiaChina$total_cases,df_semIndiaChina$population_density)
df_2variaveis

library ( factoextra )
fviz_nbclust(df_2variaveis , FUN = hcut , method = "wss", k.max = 10)
fviz_nbclust(df_2variaveis , FUN = hcut , method = "silhouette", k.max = 10)


scaled_df<-scale(df_2variaveis)
dist_df<-dist (scaled_df,method = "euclidean" )
dist_df
AC <- hclust (dist_df, method = "ward")
plot ( AC , hang = -1)






######Analise de componentes principais############

library(readxl)
library(ggplot2)
library(factoextra)

# vamos usar apenas colunas 3 a 26
df_active<-df_semIndiaChina[,c(3,5,15,16,19,25)] 
df_active
#temos de standardizar os dados pelo que fazemos scale=T
res.pca <- prcomp(df_active, scale = T)

res.pca


#scree plot
fviz_eig(res.pca)

#para melhorar a representacao grafica
# no sentido de se perceber o que e' que 
#pertence a cada grupo
grupos <- as.factor(df_semIndiaChina$continent) 

fviz_pca_biplot(res.pca, 
                label="var", 
                habillage=grupos,
                legend.title = "Continente", 
                title= "Gráfico de PCA")



#valores proprios (eigenvalues)
res.pca$eig

#fazer um scree plot (com a % da variancia explicada por cada componente)
fviz_eig(res.pca, geom="line")




# se quisermos verificar a correlacao
library(corrplot)
var<-get_pca_var(res.pca)
corrplot(var$cos2,is.corr = T)

corrplot(var$cos2,method = "number" , is.corr = T)

