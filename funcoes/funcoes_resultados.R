# funcoes utilizadas no resultados.R

RMSE <- function(y_chapeu,y){
  
  rmse <- sqrt( mean( (y_chapeu - y)^2 , na.rm = TRUE) )
  
  return(rmse)
  
}

MAE <- function(y_chapeu,y){
  
  mae <- mean(abs(y_chapeu-y) , na.rm = TRUE)
  
  return(mae)
  
}






