# ar com selecao de lag por BIC #######

BIC_scores_arimas <- function(x,dados) {
  
  BIC(arima(dados,order = c(x,0,0),method = 'ML' ))
  
}

previsao.ar.melhor.BIC <- function(dados,lead = 1){
  # Testando os lags de 1 a 13 t
  lags <- as.matrix(1:11)
  
  # escilhe o melhor ar com melhor BIC
  melhor.BIC <- which.min(apply(lags,1,BIC_scores_arimas,dados=dados))
  
  previsao <- forecast(arima(dados,order = c(melhor.BIC,0,0),method = 'ML' ), h = lead)$mean[lead]
  
  #print(melhor.BIC)
  
  return(previsao)
  
}
