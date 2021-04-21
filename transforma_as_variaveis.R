comeco <- Sys.time()

# Transformação das variaveis 

# selecionando apenas os dados de 2003 a diante

dados <- df.final['2003/',]

# Transformando variação dos IPCA's e suas prevções do simples para diferença dos logs

dados[,grep('ipca',colnames(dados),ignore.case = TRUE) ][,-c(15:19)] <- log(dados[,grep('ipca',colnames(dados),ignore.case = TRUE) ][,-c(15:19)]+1)



dados.transformados <- dados

# Testando estacionariedade das séries e aplicando diff para as series nao estacionarias

for (coluna in 5:ncol(dados.transformados)) {

  # teste de estacionariedade
  # parece que tem algum erro quando o objeto é xts !
  
  teste <- aTSA::adf.test(as.ts(dados.transformados[,coluna]) ,output = FALSE)
  
  # transformando as variaveis não estacionarias
  if (any(teste$type3[,3] > .01) & !any(dados.transformados[,coluna] < 0, na.rm = TRUE) ){
    
    print(paste0('a serie ', colnames(dados.transformados)[coluna], ' NÃO é estacionária'))
    
        dados.transformados[,coluna] <- diff(log(dados.transformados[,coluna]) )
    
  } else {
    
    print(paste0('a serie ', colnames(dados.transformados)[coluna], ' é estacionária'))
          
  }
  
}


# para medir o tempo que rodou
fim <- Sys.time()

total <- (fim - comeco)

print(paste0('tempo total transformando os dados : ',format(total, digits = 3)) )

rm(list = ls()[-c(grep('dados.transformados',ls()), grep('df.final',ls()))])

saveRDS(dados.transformados,file = 'dados.transformados')
