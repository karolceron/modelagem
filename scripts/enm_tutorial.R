### Curso de modelagem de nicho ecol�gico  - M�dulo 1 ###
## Agosto de 2020 ##
# Karoline Ceron # 
# modificado de Clube da Ci�ncia, Jos� Hidasi Neto (shorturl.at/nuCY1)

########### Primeiro de tudo ################# 
######### Instalar o Java ################
############################################

sessionInfo() #checando qual a sua vers�o do Windows 
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

# Importando dados de ocorr�ncia do GBIF
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
#level = 0 (sem divis�o), 1(com estados) 
plot(my0)

# baixando as vari�veis do WorldClim
climate=getData('worldclim', var='bio',res=5) 
#var = bio (vari�veis bioclim�ticas)
# res = escala das vari�veis, do maior do menor (10 min, 5min, 2.5 min, 30s)
# Bichos com distribui��o no Brazil todo (usar 10min)
# Distribui��o regional (5min)
# Distribui��o regional pra local (2.5min)

# plotando a vari�vel 1
plot(climate$bio1, main='Annual Mean Temperature')

######################################################
################### Modelagem #########################
#######################################################

install.packages('Rtools')
install.packages('installr')

# Voc� provavelmente encontrar� problemas ao tentar instalar o Rtools. 
# Portanto, use o installr para instalar o Rtools:
library(installr)
install.Rtools()

# Escolha a vers�o 3.3 e deixe-a baixar e instalar. 
# Uma janela provavelmente aparecer�. Clique ok para instalar tudo 

# Instalando o R java
install.packages('rJava')
library(rJava)


############ IMPORTANTE ##################
# Por fim, abra o seguinte site e fa�a o download do maxent: 
# https://biodiversityinformatics.amnh.org/open_source/maxent/. 
# Agora pegue o arquivo .jar (o arquivo maior) e coloque-o em Documents/R/win-library/3.5/dismo/java.


# Agora vamos organizar os valores de longitude e latitude em um novo arquivo.
aplapoints=apla[,c('lon','lat')]

# Agora vamos dividir esse arquivo em 5 partes. 
# 75% dos dados ser�o usados para treinar o modelo
# e 25% ser�o usados para testar se � um bom modelo.

group <- kfold(aplapoints,5)
pres_train <- aplapoints[group!=1,]
pres_test <- aplapoints[group==1,]

# Em seguida, obteremos os dados clim�ticos apenas para uma parte da 
# Am�rica do Sul (lembre-se das coordenadas m�nimas e m�ximas que definimos antes)

ext = extent(-60,-45,-30,-22)
southamerworldclim=crop(climate,ext)

# Agora vamos realizar a modelagem 
# Observe que usaremos nossas vari�veis bioclim�ticas 
# (cortadas na extens�o desejada) e os 75% dos dados de ocorr�ncia que treinar�o o modelo.

xm <- maxent(southamerworldclim,pres_train, path='Results')
# essa fun��o criar� uma pasta chamada "results", onde ter� o arquivo "maxentResults"
# Abra esse arquivo "maxentResults.csv'

plot(xm)

# No gr�fico acima, podemos ver a contribui��o de cada vari�vel bioclim�tica 
# no treinamento do modelo. Agora criaremos pseudo-aus�ncias 
# (lugares em que n�o encontramos as esp�cies; chamamos pseudo porque ningu�m 
# realmente sabe se algu�m registrou a aus�ncia em qualquer �rea). 
# Usaremos randomPoints () para criar 1000 pseudo-aus�ncias. 
# Em seguida, tamb�m dividiremos esses dados em 5 partes.

backg = randomPoints(southamerworldclim, n=1000, ext=ext, extf=1.25)
colnames(backg) <- c("lon","lat")
group=kfold(backg, 5)
backg_train <- backg[group!=1,]
backg_test <- backg[group==1,]

# Ok, agora podemos avaliar o modelo. 
# Em outras palavras, podemos ver se o modelo � bom ou ruim. 
# Para isso, usaremos os dados de ocorr�ncia para teste 
# (25% n�o utilizados para o treinamento do modelo), os dados de aus�ncia 
# para teste (tamb�m 25%), o modelo maxent e as vari�veis bioclim�ticas.

e = evaluate(pres_test, backg_test, xm, southamerworldclim)
plot(e,'ROC')

# Quanto maior o valor da AUC for, melhor ser� o nosso modelo. 
# Nossa AUC foi de 0,967, portanto n�o h� com que se preocupar. 
# Agora, com base nas ocorr�ncias da esp�cie que temos, podemos ver 
# a adequa��o do habitat para ele no Brasil. 
# Fazemos isso usando a fun��o predict ().

p <- predict(southamerworldclim, xm, ext=ext, progress='')
plot(p, main="Maxent, raw values - Present")
plot(my0,add=T)

#salvando em .tiff
writeRaster(p,filename="Aplasto_pre.tif",overwrite=TRUE)

# As �reas verdes s�o as melhores (perto de 1) para a ocorr�ncia da esp�cie. 

# Quest�es? Qual foi o valor do AUC? 
# Quais vari�veis tiveram maior import�ncia para a esp�cie?


############################################################################
############################################################################

# Agora vamos selecionar as vari�veis
test<-getValues(southamerworldclim)
cor.matrix <- as.data.frame(cor(test, use="complete.obs"))
cor.matrix
abs(cor.matrix) > 0.8
write.csv(abs(cor.matrix) > 0.8,'cor_matrix.csv') # ver na matriz quais variaveis apresentam cor >0,8

# na planilha de excel, substitua os nome "false" por 'espa�o', deixando
# apenas os valores verdadeiros

# Vamos eliminar as vari�veis altamente correlacionadas, deixando apenas as selecionadas
southamerworldclim<- dropLayer(southamerworldclim, c(1,3,6,7,13,16)) #colocar aqui o nome das vari�veis a serem excluidas
test2<-getValues(southamerworldclim)
cor(test2, use="complete.obs")

# Agora vamos realizar a modelagem 
# Observe que usaremos nossas vari�veis bioclim�ticas 
# (cortadas na extens�o desejada) e os 75% dos dados de ocorr�ncia que treinar�o o modelo.

xm <- maxent(southamerworldclim,pres_train, path='Results')
# essa fun��o criar� uma pasta chamada "results", onde ter� o arquivo "maxentResults"
# Abra esse arquivo "maxentResults.csv'

plot(xm)

# No gr�fico acima, podemos ver a contribui��o de cada vari�vel bioclim�tica 
# no treinamento do modelo. Agora criaremos pseudo-aus�ncias 
# (lugares em que n�o encontramos as esp�cies; chamamos pseudo porque ningu�m 
# realmente sabe se algu�m registrou a aus�ncia em qualquer �rea). 
# Usaremos randomPoints () para criar 1000 pseudo-aus�ncias. 
# Em seguida, tamb�m dividiremos esses dados em 5 partes.

backg = randomPoints(southamerworldclim, n=1000, ext=ext, extf=1.25)
colnames(backg) <- c("lon","lat")
group=kfold(backg, 5)
backg_train <- backg[group!=1,]
backg_test <- backg[group==1,]

# Ok, agora podemos avaliar o modelo. 
# Em outras palavras, podemos ver se o modelo � bom ou ruim. 
# Para isso, usaremos os dados de ocorr�ncia para teste 
# (25% n�o utilizados para o treinamento do modelo), os dados de aus�ncia 
# para teste (tamb�m 25%), o modelo maxent e as vari�veis bioclim�ticas.

e = evaluate(pres_test, backg_test, xm, southamerworldclim)
plot(e,'ROC')

# Quanto maior o valor da AUC for, melhor ser� o nosso modelo. 
# Nossa AUC foi de 0,967, portanto n�o h� com que se preocupar. 
# Agora, com base nas ocorr�ncias da esp�cie que temos, podemos ver 
# a adequa��o do habitat para ele no Brasil. 
# Fazemos isso usando a fun��o predict ().

p <- predict(southamerworldclim, xm, ext=ext, progress='')
plot(p, main="Maxent, raw values - Present")
plot(my0,add=T)

#salvando em .tiff
writeRaster(p,filename="Aplasto_pre.tif",overwrite=TRUE)

# As �reas verdes s�o as melhores (perto de 1) para a ocorr�ncia da esp�cie. 


# E agora? Ouve diferen�a entre as vari�veis selecionadas?
# E o AUC, teve melhor performance? 


##################################
########## futuro ###############
#################################

# Agora, vamos ver se a adequa��o do habitat mudar� com base em um cen�rio 
# relativamente m�dio (diminuindo lentamente as emiss�es de carbono) 
# chamado RCP45. Para obter os dados para o ano 2070:

# lembre de baixar na mesma resolu��o das vari�veis do presente
futbio<-getData('CMIP5', var='bio', res=5, rcp=45, model='MI', year=70)

# Em seguida, use os mesmos nomes que usamos para criar nosso modelo no presente,
# usando as futuras vari�veis bioclim�ticas. 
# Finalmente, temos nossa nova adequa��o de habitat para o ano 2070 

names(futbio)<-names(climate)

#deixando as mesmas vari�veis
futbio<- dropLayer(futbio, c(1,3,6,7,13,16))

# Finalmente, temos nossa nova adequa��o de habitat para o ano 2070 
pfut<-predict(futbio, xm, ext=ext, progress='')

plot(pfut, main="Maxent, raw values - Future")
plot(my0,add=T)

writeRaster(pfut,filename="Aplasto_fut.tif",overwrite=TRUE)


##############################
##### passado ################
#############################

# n�o tem uma fun��o para baixar os dados no R diretamente

# importando as variaveis do Last inter-glacial

lig <- stack(list.files(path = "Last inter glacial", pattern='.bil$', full.names=T))

# cortando o shape
ext = extent(-60,-45,-30,-22)
lig=crop(lig,ext)

#cortando as vari�veis correlacionadas
lig<- dropLayer(lig, c(1,3,6,7,13,16)) 

# modelando
xm <- maxent(lig,pres_train, path='Results')

# Previs�o de distribui��o pro Last inter-glacial
plig<-predict(xm, lig)

plot(plig, main="Maxent, raw values - LIG")
plot(my0,add=T)
writeRaster(plig,filename="Aplasto_lig.tif",overwrite=TRUE)


# importando as variaveis do Last Glacial Maximum

lgm <- stack(list.files(path = "Last Glacial Maximum", pattern='.tif$', full.names=T))

# cortando o shape
ext = extent(-60,-45,-30,-22)
lgm=crop(lgm,ext)

#cortando as vari�veis correlacionadas
lgm<- dropLayer(lgm, c(1,3,6,7,13,16)) 

# modelando
xm <- maxent(lgm,pres_train, path='Results')

# Previs�o de distribui��o pro Last Glacial Maximum
plgm<-predict(xm, lgm)

plot(plgm, main="Maxent, raw values - LGM")
plot(my0,add=T)

writeRaster(plgm,filename="Aplasto_lgm.tif",overwrite=TRUE)


# importando as variaveis do Mid Holocene

mid <- stack(list.files(path = "Mid Holocene", pattern='.tif$', full.names=T))

# cortando o shape
ext = extent(-60,-45,-30,-22)
mid=crop(mid,ext)

#cortando as vari�veis correlacionadas
mid<- dropLayer(mid, c(1,3,6,7,13,16)) 

# modelando
xm <- maxent(mid,pres_train, path='Results')

# Previs�o de distribui��o pro Mid Holocene
pmid<-predict(xm, mid)

plot(pmid, main="Maxent, raw values - MID")
plot(my0,add=T)

writeRaster(pmid,filename="Aplasto_MID.tif",overwrite=TRUE)

