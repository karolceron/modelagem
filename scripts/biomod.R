## install the latest release of biomod2
#devtools::install_github('biomodhub/biomod2')
# selecionar atualizar todos os pacotes (1)
# instalar todas as dependencias (yes)
# ir instalando isoladamente os pacotes que apresentarem erro
# backports não instalar dependencias

#Sempre colocar o arquivo maxent.jar no diretorio de trabalho

1## load the required packages
library(biomod2)
library(ggplot2)
library(gridExtra)
library(raster)
library(rasterVis)

## read data ----
ProLau_occ<-read.table("crepitans.txt", sep=",", header = T)
summary(ProLau_occ)

# read in Bioclim
files <- list.files('D:/Karol/Desktop/Curso Modelagem/biomod2_video_single_species_modelling/script/wc5', pattern='.bil$', full.names=TRUE)
predictors <- stack(files)

#importando o shape
#bra <- getData('GADM', country='BRA', level=0) #codes = http://kirste.userpage.fu-berlin.de/diverse/doc/ISO_3166.html
#layers1<-mask(crop(predictors,bra), bra)

# crop predictors
e <- extent(-80,-30,-40,10)
layers1 <- crop(predictors, e)

#Visualizando layers
plot(layers1[[1]])
points(ProLau_occ[,1:2])

#we convert the raster object into a data frame to run the PCA
library(ade4)
bioclim_ZA_df <- na.omit(as.data.frame(layers1))
head(bioclim_ZA_df)

pca_ZA <- dudi.pca(bioclim_ZA_df,scannf = F, nf = 2)

#procurando por outliers


plot(pca_ZA$li[, 1:2]) #esta ok

#seleção de variáveis
pdf("pcacreppdf", width = 10, height = 8)
s.corcircle(pca_ZA$co, clabel = .6 )
dev.off()
# We want to select a set of variables that are not too collinear 
# (two variables pointing in orthogonal directions are independent, 
# two variables pointing in the same or opposite directions are highly
# dependent, positively or negatively, respectively), and significantly 
# contribute to the overall environmental variation (the longer the arrow, the
# more important the variable)                                                                                                                        

#indicando quais variáveis é melhor manter
bioclim_ZA_sub<-subset(layers1, c('bio7', 'bio1','bio17', 'bio8')) 
bioclim_ZA_sub <- stack(bioclim_ZA_sub)

## format the data ----
ProLau_data <- 
  BIOMOD_FormatingData(
    resp.var = ProLau_occ['crepitans'],
    resp.xy = ProLau_occ[, c('lat','long')],
    expl.var = bioclim_ZA_sub,
    resp.name = "crepitans",
    PA.nb.rep = 2,
    PA.nb.absences = 500,
    PA.strategy = 'random'
  )

## formatted object summary
ProLau_data

## plot of selected pseudo-absences
plot(ProLau_data)
                 
## define individual models options ---- 
ProLau_opt <- 
  BIOMOD_ModelingOptions(
    GLM = list(type = 'quadratic', interaction.level = 1),
    GBM = list(n.trees = 1000),
    GAM = list(algo = 'GAM_mgcv'),
    CTA = ,
    SRE = ,
    FDA = ,
    MARS = list(interaction.level = 1),
    RF=,
    MAXENT.Phillips =
  )

## run the individual models ----
ProLau_models <- 
  BIOMOD_Modeling(
    data = ProLau_data,
    models = c("GLM", "GBM", "RF", "GAM", "CTA", "SRE", "FDA", "MARS", "MAXENT.Phillips"),
    models.options = ProLau_opt,
    NbRunEval = 2,
    DataSplit = 80,
    VarImport = 3,
    modeling.id = "demo0"
  )


#if maxent present some error
#myBiomodOption <- BIOMOD_ModelingOptions(MAXENT = list(path_to_maxent.jar = 'C:/Users/tomaug/Documents/maxent.jar'))"


## asses individual models quality ----

## get models evaluation scores
ProLau_models_scores <- get_evaluations(ProLau_models)
ProLau_models_scores #TABELA

## ProLau_models_scores is a 5 dimension array containing the scores of the models
dim(ProLau_models_scores)
dimnames(ProLau_models_scores)

## plot models evaluation scores (quanto mais pro canto direito superior, melhor)
models_scores_graph(
  ProLau_models, 
  by = "models", 
  metrics = c("ROC","TSS"), 
  xlim = c(0.5,1), 
  ylim = c(0.5,1)
)


## check variable importance (qual variavel é mais importante nos modelos)
(ProLau_models_var_import <- get_variables_importance(ProLau_models))

## make the mean of variable importance by algorithm
apply(ProLau_models_var_import, c(1,2), mean) #TABELA


## run the ensemble models ----
ProLau_ensemble_models <- 
  BIOMOD_EnsembleModeling(
    modeling.output = ProLau_models,
    em.by = 'all', #usar todos os modelos
    eval.metric = 'TSS',
    eval.metric.quality.threshold = 0.8, #somente modelos com TSS maior que 0.8
    models.eval.meth = c('TSS','ROC'),
    prob.mean = FALSE,
    prob.cv = TRUE, 
    committee.averaging = TRUE,
    prob.mean.weight = TRUE,
    VarImport = 0
  )

## asses ensemble models quality ----
(ProLau_ensemble_models_scores <- get_evaluations(ProLau_ensemble_models)) #TABELA

## do models projections ----

## current projections
ProLau_models_proj_current <- 
  BIOMOD_Projection(
    modeling.output = ProLau_models,
    new.env = bioclim_ZA_sub,
    proj.name = "crepitans",
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
  )
ProLau_ensemble_models_proj_current <- 
  BIOMOD_EnsembleForecasting(
    EM.output = ProLau_ensemble_models,
    projection.output = ProLau_models_proj_current,
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
  )

## future projections

## load LGM
# read in Bioclim
files1 <- list.files('D:/Karol/Desktop/Curso Modelagem/biomod2_video_single_species_modelling/script/Last Glacial Maximum', pattern='.tif$', full.names=TRUE)
predictors <- stack(files1)

# crop predictors
e <- extent(-80,-30,-40,10)
layers1 <- crop(predictors, e)

bioclim_ZA_sub<-subset(layers1, c('bio7', 'bio1','bio17', 'bio8')) 
bioclim_ZA_sub <- stack(bioclim_ZA_sub)

ProLau_models_proj_LGM <- 
  BIOMOD_Projection(
    modeling.output = ProLau_models,
    new.env = bioclim_ZA_sub,
    proj.name = "LGM",
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
  )

ProLau_ensemble_models_proj_LGM<- 
  BIOMOD_EnsembleForecasting(
    EM.output = ProLau_ensemble_models,
    projection.output = ProLau_models_proj_LGM,
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
  )

plot(ProLau_ensemble_models_proj_LGM, str.grep = "EMca|EMwmean")

## load LIG
# read in Bioclim
files1 <- list.files('D:/Karol/Desktop/Curso Modelagem/biomod2_video_single_species_modelling/script/Last inter glacial', pattern='.bil$', full.names=TRUE)
predictors1 <- stack(files1)

# crop predictors
e <- extent(-80,-30,-40,10)
layers1 <- crop(predictors1, e)

bioclim_ZA_sub1<-subset(layers1, c('bio7', 'bio1','bio17', 'bio8')) 
bioclim_ZA_sub1 <- stack(bioclim_ZA_sub1)

ProLau_models_proj_LIG <- 
  BIOMOD_Projection(
    modeling.output = ProLau_models,
    new.env = bioclim_ZA_sub1,
    proj.name = "LIG",
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
  )

ProLau_ensemble_models_proj_LIG <- 
  BIOMOD_EnsembleForecasting(
    EM.output = ProLau_ensemble_models,
    projection.output = ProLau_models_proj_LIG,
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
  )

## check how projections looks like
plot(ProLau_ensemble_models_proj_LIG, str.grep = "EMca|EMwmean")



## load MID
# read in Bioclim
files3 <- list.files('D:/Karol/Desktop/Curso Modelagem/biomod2_video_single_species_modelling/script/Mid Holocene', pattern='.tif$', full.names=TRUE)
predictors3 <- stack(files3)

# crop predictors
e <- extent(-80,-30,-40,10)
layers3 <- crop(predictors3, e)

bioclim_ZA_sub3<-subset(layers3, c('bio7', 'bio1','bio17', 'bio8')) 
bioclim_ZA_sub3 <- stack(bioclim_ZA_sub3)

ProLau_models_proj_MID <- 
  BIOMOD_Projection(
    modeling.output = ProLau_models,
    new.env = bioclim_ZA_sub3,
    proj.name = "MID",
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
  )

ProLau_ensemble_models_proj_MID <- 
  BIOMOD_EnsembleForecasting(
    EM.output = ProLau_ensemble_models,
    projection.output = ProLau_models_proj_MID,
    binary.meth = "TSS",
    output.format = ".img",
    do.stack = FALSE
  )

## check how projections looks like
plot(ProLau_ensemble_models_proj_MID, str.grep = "EMca|EMwmean")
