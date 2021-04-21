library(zoo)
library(dplyr)
library(xts)
# Obs.: Utilizo algumas funções retiradas do pacote HDeconomectrics (https://github.com/gabrielrvsc/HDeconometrics) e 
# ForecastingInflation ( https://github.com/gabrielrvsc/ForecastingInflation )



#################################################### Info #################################################
#                                                                                                         #
# Esse código executa os outros códigos referentes a etapa de :                                           #
# 1) Coleta dos dados ;                                                                                   #
# 2) Transformação dos dados;                                                                             #
# 3) Modelagem - Previsão do IPCA                                                                         #
# 4) Apresentação dos resultados shiny app                                                                #
#                                                                                                         #
#   Possivel bug: Testando esse código passei uma ou duas vezes por um bug que julgo ser do compilador.   #
# Quando dou source no código os arquivos carregados na etapa 3) não são passados adiante. Uma maneira    #
# simples de contornar esse problema é de simplesmente reiniciar o R ou rodar esse arquivo linha a linha  #
# ( ctrl + enter ).                                                                                       #
#                                                                                                         #
###########################################################################################################


############################################ SETUP ########################################################


# fazer download dos dados ou carregar do arquivo "df.final"
# Obs.: se sim pode demorar de 8 a 12 minutos pela primeira vez
carregar_dados <- 'não'

# Transformação dos dados ou carregar do arquivo "dados.transformados"
# Obs.: 3 segundos para executar esse código
transformar_dados <- 'não'

# Rodar os modelos ou carregar os resultados prontos do arquivo "todas_previsoes_por_horizonte"
# Obs.: DEMORADO , 8 horas para rodar todos modelos para todos os horizontes
rodar_modelos <- 'não'


###########################################################################################################


############################################ Inicio #######################################################


######################################### 1) Coleta dos dados #############################################


print('1) Coleta dos dados')
if (carregar_dados == 'sim'){
  
  source('Carrega_dados.R',encoding = 'UTF-8',local = TRUE)
  
} else {
  
  df.final <- readRDS('df.final')
  
}


######################################### 2) Transformação dos dados ######################################


print('2) Transformação dos dados')
if (transformar_dados == 'sim'){ 
  
  source('transforma_as_variaveis.R',encoding = 'UTF-8',local = TRUE)
  
} else {
  
  dados.transformados <- readRDS('dados.transformados')
  
}


######################################## 3) Modelagem - Previsão do IPCA ##################################


                                      ############################
                                      # ATENÇÃO : Demorado 8 hrs #
                                      # Possível paralelizar.    #
                                      # Vou trabalhar nisso      #
                                      ############################


print('3) Modelagem - Previsão do IPCA')

if (rodar_modelos == 'sim'){ 
  
  source('regressão_varios_horizontes.R',encoding = 'UTF-8')
  
} else {
  
  todas_previsoes_por_horizonte <- readRDS('todas_previsoes_por_horizonte',local = TRUE)
  
}

######################################## 4) Apresentação dos resultados shiny app #########################


print('4) Apresentação dos resultados shiny app')

source('./funcoes/funcoes_resultados.R',encoding = 'UTF-8',local = TRUE)

source('resultados.R',encoding = 'UTF-8',local = TRUE)

source('./funcoes/shiny.R',encoding = 'UTF-8',local = TRUE)

shinyApp(ui,server)

