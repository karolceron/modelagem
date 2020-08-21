###################
#### ntbox #####
#################
# Osorio-Olvera, L., Lira-Noriega, A., Soberón, J., Townsend Peterson, A.,
# Falconi, M., Contreras-Díaz, R. G., ... & Barve, N.
# ntbox: an R package with graphical user interface for modeling and 
# evaluating multidimensional ecological niches. 
# Methods in Ecology and Evolution. In press
#  https://doi.org/10.1111/2041-210X.13452


# Abri a página com o tutorial
https://luismurao.github.io/ntbox_user_guide.html#using-full-extent


#### instalar Rtools
# https://cran.r-project.org/bin/windows/Rtools/
# install program
# executar: 
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# checando:
Sys.which("make")
# testando
install.packages("jsonlite", type = "source")


# instalando pacotes
if (!require('devtools')) install.packages('devtools')
devtools::install_github('luismurao/ntbox')
install.packages("leaflet")

# Abrindo o app
library(ntbox)
run_ntbox()

##############
# open in browser
##############


#inserir o algoritmo maxent
ntbox::maxent_call
