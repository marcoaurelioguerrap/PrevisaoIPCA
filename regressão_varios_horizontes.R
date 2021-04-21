comeco <- Sys.time()

# regressão para todos os periodos 

library(forecast)
library(randomForest)
library(bootstrap)
library(boot)
library(glmnet)

# por em um outro código para dar source 

source('./funcoes/ar_rw_rolling_windows.R')
source('./funcoes/ForecastingInflation/func-ucsv.R')
source('./funcoes/ForecastingInflation/func-lasso.R')
source('./funcoes/HDeconometrics/ic.glmnet.R')
source('./funcoes/ForecastingInflation/func-fact.R')
source('./funcoes/ForecastingInflation/func-tfact.R')
source('./funcoes/HDeconometrics/baggit.R')
source('./funcoes/ForecastingInflation/func-boosting.R')
source('./funcoes/HDeconometrics/boosting.R')
source('./funcoes/HDeconometrics/auxiliary_functions.R')
source('./funcoes/HDeconometrics/predict.boosting.R')
source('./funcoes/ForecastingInflation/func-bag.R')
source('./funcoes/HDeconometrics/bagging.R')
source('./funcoes/HDeconometrics/predict.bagging.R')
source('./funcoes/ForecastingInflation/func-csr.R')
source('./funcoes/HDeconometrics/csr.R')
source('./funcoes/HDeconometrics/predict.csr.R')
source('./funcoes/ForecastingInflation/func-jn.R')
source('./funcoes/ForecastingInflation/func-rf.R')
source('./funcoes/ForecastingInflation/func-rfols.R')
source('./funcoes/ForecastingInflation/func-lbvar.R')
source('./funcoes/HDeconometrics/lbvar.R')
source('./funcoes/lbvar/lbvar.R')
source('./funcoes/lbvar/predict.lbvar.R')
source('./funcoes/ForecastingInflation/func-adalassorf.R')



# dados utilizados

dados.utilizados <-  dados.transformados["2003-02/2020-12",]

obs <- nrow(dados.utilizados) - 109

# criando lista para guardar todos os horizontes

todas_previsoes_por_horizonte <- list()

for ( horizonte in 1:13){
  
  # Modelos #
  
  ### Autoregressivo (AR)
  print(paste0('horizonte : ',horizonte, ' -- ','1/15 -- Rodando Modelo AR') )
  
  previsoes_ipca_ar <- ar.rw(dados.utilizados,obs,horizonte)
  
  ### Random Walk
  print(paste0('horizonte : ',horizonte, ' -- ','2/15 -- Rodando Modelo RW') )
  
  previsoes_ipca_rw <- rw.rw(dados.utilizados,obs,horizonte)
  
  
  # as formulas a baixo foram retiradas de https://github.com/gabrielrvsc/ForecastingInflation/tree/master/first-sample 
  # utilizadas no artigo Forecasting Inflation in a Data-Rich Environment:The Benefits of Machine Learning Methods
  


  ### Lasso 
  print(paste0('horizonte : ',horizonte, ' -- ','3/15 -- Rodando Modelo Lasso') )
  
  resultado_lasso <- lasso.rolling.window(as.ts(dados.utilizados),obs,1,horizonte)
  
  previsoes_ipca_lasso <- as.xts(resultado_lasso$pred, order.by = index(dados.utilizados)[-(1:(109))])
  
  
  ### Ridge nao vou usar , muito ruim a previsao
  print(paste0('horizonte : ',horizonte, ' -- ','4/15 -- Rodando Modelo ridge') )
  
  resultado_ridge <- lasso.rolling.window(as.ts(dados.utilizados),obs,indice = 1,lag = horizonte,alpha = 0)
  
  previsoes_ipca_ridge <- as.xts(resultado_ridge$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  ### adaLasso 
  print(paste0('horizonte : ',horizonte, ' -- ','5/15 -- Rodando Modelo adaLasso') ) 
  
  resultado_adalasso <- lasso.rolling.window(as.ts(dados.utilizados),obs,1,horizonte,type = 'adalasso')
  
  previsoes_ipca_adalasso <- as.xts(resultado_adalasso$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  ### flex adalasso
  print(paste0('horizonte : ',horizonte, ' -- ','6/15 -- Rodando Modelo flex-adaLasso') ) 
  
  resultado_flex_adalasso <- lasso.rolling.window(as.ts(dados.utilizados),obs,1,horizonte,type = 'fal')
  
  previsoes_ipca_flex_adalasso <- as.xts(resultado_flex_adalasso$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  # Modelo de fatores
  print(paste0('horizonte : ',horizonte, ' -- ','7/15 -- Rodando Modelo de fatores') )
  
  resultado_fact <- fact.rolling.window(as.data.frame(dados.utilizados),obs,1,horizonte)
  
  previsoes_ipca_fact <- as.xts(resultado_fact$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  # modelo de Target fatores
  print(paste0('horizonte : ',horizonte, ' -- ','8/15 -- Rodando Modelo de Target fatores') )
  
  resultado_target_fact <- tfact.rolling.window(as.ts(dados.utilizados),obs,1,horizonte)
  
  previsoes_ipca_target_fact <- as.xts(resultado_target_fact$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  # Boosting de fatores 
  print(paste0('horizonte : ',horizonte, ' -- ','9/15 -- Rodando Modelo de Boosting de fatores') )
  
  resultado_boosting_fact <- boosting.rolling.window(as.ts(dados.utilizados),obs,1,horizonte)
  
  previsoes_ipca_boosting_fact <- as.xts(resultado_boosting_fact$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  # Ensemble
  
  ### Bagging
  
  # TODO : botar um while true try da vida no bagging ,
  # TODO : verificar as previsoes ,ta muito estranho ,
  # obs .: só está funcionando se tiver mais de 120 observacoes 
  #resultado_bagging <- bagg.rolling.window(as.ts(dados.utilizados),obs,1,horizonte)
  
  #previsoes_ipca_bagging <- as.xts(resultado_bagging$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  ### CSR
  
  print(paste0('horizonte : ',horizonte, ' -- ','10/15 -- Rodando Modelo de CSR') )
  
  resultado_csr <- csr.rolling.window(as.ts(dados.utilizados),obs,1,horizonte)
  
  previsoes_ipca_csr <- as.xts(resultado_csr$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  ### jackknife
  
  # TODO : ver como funciona direit o jackknife do bootstrap
  
  #resultado_jackknife <- jackknife.rolling.window(as.ts(dados.utilizados),obs,1,1)
  
  #previsoes_ipca_jackknife<- as.xts(resultado_jackknife$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  ### Random Forests 
  print(paste0('horizonte : ',horizonte, ' -- ','11/15 -- Rodando Modelo de RF') )
  
  resultado_rf <- rf.rolling.window(as.ts(dados.utilizados),obs,1,horizonte)
  
  previsoes_ipca_rf <- as.xts(resultado_rf$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  ### RF ols
  print(paste0('horizonte : ',horizonte, ' -- ','12/15 -- Rodando Modelo de RFOLS') )
  
  resultado_rfols <- rfols.rolling.window(as.ts(dados.utilizados),obs,1,horizonte)
  
  previsoes_ipca_rfols <- as.xts(resultado_rfols$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  ### RF adalasso
  print(paste0('horizonte : ',horizonte, ' -- ','13/15 -- Rodando Modelo de RFadaLasso') )
  
  resultado_rf_adalasso <- rflasso.rolling.window(as.ts(dados.utilizados),obs,1,horizonte,type = 'fal')
  
  previsoes_ipca_rf_adalasso <- as.xts(resultado_rf_adalasso$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  ### BVAR 
  print(paste0('horizonte : ',horizonte, ' -- ','14/15 -- Rodando Modelo de BVAR') )
  
  resultado_lbvar <- lbvar.rw(as.ts(dados.utilizados),4,horizonte,obs)
  
  previsoes_ipca_lbvar <- as.xts(resultado_lbvar$pred, order.by = index(dados.utilizados)[-(1:109)])
  
  
  # Agrupando os resultados resultados 
  
  previsoes_ipca <- cbind( dados.utilizados[-(1:(109)),'IPCA.'],
                           previsoes_ipca_ar, previsoes_ipca_rw,
                           previsoes_ipca_lasso, previsoes_ipca_ridge,previsoes_ipca_adalasso,
                           previsoes_ipca_flex_adalasso,previsoes_ipca_fact,previsoes_ipca_target_fact,
                           previsoes_ipca_boosting_fact, previsoes_ipca_csr,previsoes_ipca_rf,
                           previsoes_ipca_rfols,previsoes_ipca_rf_adalasso,previsoes_ipca_lbvar)
  
  todas_previsoes_por_horizonte[[horizonte]] <- previsoes_ipca

}


### Modelo UCSV a funcao dele ja produz a previsão para todos horizontes

print(paste0('horizonte : todos -- ','15/15 -- Rodando Modelo UCSV') )

resultado_ucsv <- ucsv.rw(as.numeric(dados.utilizados[,1]),obs,1:13)

previsoes_ipca_ucsv <- as.xts(resultado_ucsv , order.by = index(dados.utilizados)[-(1:109)])


for (horizonte in 1:13){
  
  todas_previsoes_por_horizonte[[horizonte]]$previsoes_ipca_ucsv <- as.xts(c(NA,NA,resultado_ucsv[,horizonte]) ,
                                                                           order.by = index(dados.utilizados)[-(1:107)])
  
}




# para medir o tempo que rodou
fim <- Sys.time()

total <- (fim - comeco)

saveRDS(todas_previsoes_por_horizonte,file = 'todas_previsoes_por_horizonte')

print(paste0('tempo total rodando os modelos : ',format(total, digits = 3)) )

# limpando a memoria 

rm(list = ls()[-c(grep('dados.transformados',ls()),
                  grep('df.final',ls()),
                  grep('todas_previsoes_por_horizonte',ls()) )])


