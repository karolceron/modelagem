### Curso de modelagem de nicho ecológico  - Módulo 1 ###
## Agosto de 2020 ##
# Karoline Ceron # 
# modificado de Clube da Ciência, José Hidasi Neto (shorturl.at/nuCY1)

########### Primeiro de tudo ################# 
######### Instalar o Java ################
############################################

sessionInfo() #checando qual a sua versão do Windows 
# R version 3.6.3 (2020-02-29)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 18362)

# Fazer o download do Java apropriado
#link para o download https://www.oracle.com/java/technologies/javase-jdk14-downloads.html


#################################################
### Instalando os pacotes ######################
###############################################
install.packages('dismo')
install.packages('maps')
install.packages('raster')

# Carregando os pacotes
library(dismo)
library(maps)
library(raster)

# Importando dados de ocorrência do GBIF
apla<-gbif('Aplastodiscus','perviridis',download=T,geo=T)

# verificando os pontos
plot(apla$lon,apla$lat,col='red',pch=19,xlab='Longitude',ylab='Latitude')
map(add=T)

# definindo limites
apla<-apla[which(apla$lon > -60 & apla$lon < -45),] 
apla<-apla[which(apla$lat > -30 & apla$lat < -22),]

# visualizando
plot(apla$lon,apla$lat,xlim=c(-60,-45),ylim=c(-30,-22),col='red',pch=19,xlab='Longitude',ylab='Latitude')
map(add=T)

# importando os shapefiles
my0 = getData('GADM', country='BRA', level=1) 
#level = 0 (sem divisão), 1(com estados) 
plot(my0)

# baixando as variáveis do WorldClim
climate=getData('worldclim', var='bio',res=5) 
#var = bio (variáveis bioclimáticas)
# res = escala das variáveis, do maior do menor (10 min, 5min, 2.5 min, 30s)
# Bichos com distribuição no Brazil todo (usar 10min)
# Distribuição regional (5min)
# Distribuição regional pra local (2.5min)

# plotando a variável 1
plot(climate$bio1, main='Annual Mean Temperature')

######################################################
################### Modelagem #########################
#######################################################

install.packages('Rtools')
install.packages('installr')

# Você provavelmente encontrará problemas ao tentar instalar o Rtools. 
# Portanto, use o installr para instalar o Rtools:
library(installr)
install.Rtools()

# Escolha a versão 3.3 e deixe-a baixar e instalar. 
# Uma janela provavelmente aparecerá. Clique ok para instalar tudo 

# Instalando o R java
install.packages('rJava')
library(rJava)


############ IMPORTANTE ##################
# Por fim, abra o seguinte site e faça o download do maxent: 
# https://biodiversityinformatics.amnh.org/open_source/maxent/. 
# Agora pegue o arquivo .jar (o arquivo maior) e coloque-o em Documents/R/win-library/3.5/dismo/java.


# Agora vamos organizar os valores de longitude e latitude em um novo arquivo.
aplapoints=apla[,c('lon','lat')]

# Agora vamos dividir esse arquivo em 5 partes. 
# 75% dos dados serão usados para treinar o modelo
# e 25% serão usados para testar se é um bom modelo.

group <- kfold(aplapoints,5)
pres_train <- aplapoints[group!=1,]
pres_test <- aplapoints[group==1,]

# Em seguida, obteremos os dados climáticos apenas para uma parte da 
# América do Sul (lembre-se das coordenadas mínimas e máximas que definimos antes)

ext = extent(-60,-45,-30,-22)
southamerworldclim=crop(climate,ext)

# Agora vamos realizar a modelagem 
# Observe que usaremos nossas variáveis bioclimáticas 
# (cortadas na extensão desejada) e os 75% dos dados de ocorrência que treinarão o modelo.

xm <- maxent(southamerworldclim,pres_train, path='Results')
# essa função criará uma pasta chamada "results", onde terá o arquivo "maxentResults"
# Abra esse arquivo "maxentResults.csv'

plot(xm)

# No gráfico acima, podemos ver a contribuição de cada variável bioclimática 
# no treinamento do modelo. Agora criaremos pseudo-ausências 
# (lugares em que não encontramos as espécies; chamamos pseudo porque ninguém 
# realmente sabe se alguém registrou a ausência em qualquer área). 
# Usaremos randomPoints () para criar 1000 pseudo-ausências. 
# Em seguida, também dividiremos esses dados em 5 partes.

backg = randomPoints(southamerworldclim, n=1000, ext=ext, extf=1.25)
colnames(backg) <- c("lon","lat")
group=kfold(backg, 5)
backg_train <- backg[group!=1,]
backg_test <- backg[group==1,]

# Ok, agora podemos avaliar o modelo. 
# Em outras palavras, podemos ver se o modelo é bom ou ruim. 
# Para isso, usaremos os dados de ocorrência para teste 
# (25% não utilizados para o treinamento do modelo), os dados de ausência 
# para teste (também 25%), o modelo maxent e as variáveis bioclimáticas.

e = evaluate(pres_test, backg_test, xm, southamerworldclim)
plot(e,'ROC')

# Quanto maior o valor da AUC for, melhor será o nosso modelo. 
# Nossa AUC foi de 0,967, portanto não há com que se preocupar. 
# Agora, com base nas ocorrências da espécie que temos, podemos ver 
# a adequação do habitat para ele no Brasil. 
# Fazemos isso usando a função predict ().

p <- predict(southamerworldclim, xm, ext=ext, progress='')
plot(p, main="Maxent, raw values - Present")
plot(my0,add=T)

#salvando em .tiff
writeRaster(p,filename="Aplasto_pre.tif",overwrite=TRUE)

# As áreas verdes são as melhores (perto de 1) para a ocorrência da espécie. 

# Questões? Qual foi o valor do AUC? 
# Quais variáveis tiveram maior importância para a espécie?


############################################################################
############################################################################

# Agora vamos selecionar as variáveis
test<-getValues(southamerworldclim)
cor.matrix <- as.data.frame(cor(test, use="complete.obs"))
cor.matrix
abs(cor.matrix) > 0.8
write.csv(abs(cor.matrix) > 0.8,'cor_matrix.csv') # ver na matriz quais variaveis apresentam cor >0,8

# na planilha de excel, substitua os nome "false" por 'espaço', deixando
# apenas os valores verdadeiros

# Vamos eliminar as variáveis altamente correlacionadas, deixando apenas as selecionadas
southamerworldclim<- dropLayer(southamerworldclim, c(1,3,6,7,13,16)) #colocar aqui o nome das variáveis a serem excluidas
test2<-getValues(southamerworldclim)
cor(test2, use="complete.obs")

# Agora vamos realizar a modelagem 
# Observe que usaremos nossas variáveis bioclimáticas 
# (cortadas na extensão desejada) e os 75% dos dados de ocorrência que treinarão o modelo.

xm <- maxent(southamerworldclim,pres_train, path='Results')
# essa função criará uma pasta chamada "results", onde terá o arquivo "maxentResults"
# Abra esse arquivo "maxentResults.csv'

plot(xm)

# No gráfico acima, podemos ver a contribuição de cada variável bioclimática 
# no treinamento do modelo. Agora criaremos pseudo-ausências 
# (lugares em que não encontramos as espécies; chamamos pseudo porque ninguém 
# realmente sabe se alguém registrou a ausência em qualquer área). 
# Usaremos randomPoints () para criar 1000 pseudo-ausências. 
# Em seguida, também dividiremos esses dados em 5 partes.

backg = randomPoints(southamerworldclim, n=1000, ext=ext, extf=1.25)
colnames(backg) <- c("lon","lat")
group=kfold(backg, 5)
backg_train <- backg[group!=1,]
backg_test <- backg[group==1,]

# Ok, agora podemos avaliar o modelo. 
# Em outras palavras, podemos ver se o modelo é bom ou ruim. 
# Para isso, usaremos os dados de ocorrência para teste 
# (25% não utilizados para o treinamento do modelo), os dados de ausência 
# para teste (também 25%), o modelo maxent e as variáveis bioclimáticas.

e = evaluate(pres_test, backg_test, xm, southamerworldclim)
plot(e,'ROC')

# Quanto maior o valor da AUC for, melhor será o nosso modelo. 
# Nossa AUC foi de 0,967, portanto não há com que se preocupar. 
# Agora, com base nas ocorrências da espécie que temos, podemos ver 
# a adequação do habitat para ele no Brasil. 
# Fazemos isso usando a função predict ().

p <- predict(southamerworldclim, xm, ext=ext, progress='')
plot(p, main="Maxent, raw values - Present")
plot(my0,add=T)

#salvando em .tiff
writeRaster(p,filename="Aplasto_pre.tif",overwrite=TRUE)

# As áreas verdes são as melhores (perto de 1) para a ocorrência da espécie. 


# E agora? Ouve diferença entre as variáveis selecionadas?
# E o AUC, teve melhor performance? 


##################################
########## futuro ###############
#################################

# Agora, vamos ver se a adequação do habitat mudará com base em um cenário 
# relativamente médio (diminuindo lentamente as emissões de carbono) 
# chamado RCP45. Para obter os dados para o ano 2070:

# lembre de baixar na mesma resolução das variáveis do presente
futbio<-getData('CMIP5', var='bio', res=5, rcp=45, model='MI', year=70)

# Em seguida, use os mesmos nomes que usamos para criar nosso modelo no presente,
# usando as futuras variáveis bioclimáticas. 
# Finalmente, temos nossa nova adequação de habitat para o ano 2070 

names(futbio)<-names(climate)

#deixando as mesmas variáveis
futbio<- dropLayer(futbio, c(1,3,6,7,13,16))

# Finalmente, temos nossa nova adequação de habitat para o ano 2070 
pfut<-predict(futbio, xm, ext=ext, progress='')

plot(pfut, main="Maxent, raw values - Future")
plot(my0,add=T)

writeRaster(pfut,filename="Aplasto_fut.tif",overwrite=TRUE)


##############################
##### passado ################
#############################

# não tem uma função para baixar os dados no R diretamente

# importando as variaveis do Last inter-glacial

lig <- stack(list.files(path = "Last inter glacial", pattern='.bil$', full.names=T))

# cortando o shape
ext = extent(-60,-45,-30,-22)
lig=crop(lig,ext)

#cortando as variáveis correlacionadas
lig<- dropLayer(lig, c(1,3,6,7,13,16)) 

# modelando
xm <- maxent(lig,pres_train, path='Results')

# Previsão de distribuição pro Last inter-glacial
plig<-predict(xm, lig)

plot(plig, main="Maxent, raw values - LIG")
plot(my0,add=T)
writeRaster(plig,filename="Aplasto_lig.tif",overwrite=TRUE)


# importando as variaveis do Last Glacial Maximum

lgm <- stack(list.files(path = "Last Glacial Maximum", pattern='.tif$', full.names=T))

# cortando o shape
ext = extent(-60,-45,-30,-22)
lgm=crop(lgm,ext)

#cortando as variáveis correlacionadas
lgm<- dropLayer(lgm, c(1,3,6,7,13,16)) 

# modelando
xm <- maxent(lgm,pres_train, path='Results')

# Previsão de distribuição pro Last Glacial Maximum
plgm<-predict(xm, lgm)

plot(plgm, main="Maxent, raw values - LGM")
plot(my0,add=T)

writeRaster(plgm,filename="Aplasto_lgm.tif",overwrite=TRUE)


# importando as variaveis do Mid Holocene

mid <- stack(list.files(path = "Mid Holocene", pattern='.tif$', full.names=T))

# cortando o shape
ext = extent(-60,-45,-30,-22)
mid=crop(mid,ext)

#cortando as variáveis correlacionadas
mid<- dropLayer(mid, c(1,3,6,7,13,16)) 

# modelando
xm <- maxent(mid,pres_train, path='Results')

# Previsão de distribuição pro Mid Holocene
pmid<-predict(xm, mid)

plot(pmid, main="Maxent, raw values - MID")
plot(my0,add=T)

writeRaster(pmid,filename="Aplasto_MID.tif",overwrite=TRUE)

