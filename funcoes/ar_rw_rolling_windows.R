# carrega funcoes para o ar com selecao de lag com melhor BIC
source('./funcoes/ar_selecao_lag_por_bic.R')


### Random Walk e AR

rw.rw <- function(dados,nprev,lag){
  
  janela <- nrow(dados) - nprev
  
  previsoes_ipca_rw <- xts(rep(0,nrow(dados.transformados[janela:nrow(dados)])),
                           order.by = index(dados.transformados[janela:nrow(dados)]))
  
  for ( i in 2:(length(previsoes_ipca_rw)) ){
    
    
    t <- as.Date(as.yearmon(index(previsoes_ipca_rw)[i] )-( (lag/12) - (1/12)) )
    t_1 <- as.Date(index(previsoes_ipca_rw)[i+1])
    
    dados.rw <- tail(dados[paste0('/',t),'IPCA.'],janela)
    
    #previsoes_ipca_rw[t_1,] <- rwf(dados.rw,h = 1,drift = FALSE)$mean[1]
    
    # previsão por Random Walk
    previsoes_ipca_rw[t_1,] <- forecast(arima(dados.rw,order = c(0,1,0)),h = lag)$mean[lag]
    
    
  }
  
  return(previsoes_ipca_rw)
}
#previsoes_ipca_rw_2 <- xts(rep(0,nrow(dados[109:220])),order.by = index(dados[109:220]))

ar.rw <- function(dados,nprev,lag){

  janela <- nrow(dados) - nprev
  
  previsoes_ipca_ar <- xts(rep(0,nrow(dados.transformados[janela:nrow(dados)])),order.by = index(dados[janela:nrow(dados)]))

  for ( i in 2:(length(previsoes_ipca_ar)) ){
    
    t <- as.Date(as.yearmon(index(previsoes_ipca_ar)[i] )-( (lag/12) - (1/12)) )
    t_1 <- as.Date(index(previsoes_ipca_ar)[i+1])

    dados.rw <- tail(dados[paste0('/',t),'IPCA.'],janela)
    
    # previsão por AR selecionando lag por BIC
    previsoes_ipca_ar[t_1,] <-  previsao.ar.melhor.BIC(dados.rw,lead = lag)
    
    
  }
  
  return(previsoes_ipca_ar)
}
