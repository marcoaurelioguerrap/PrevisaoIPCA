# Dados
comeco <- Sys.time()
library(GetBCBData)
library(rbcb)
library(BatchGetSymbols)
# dados retirados do BACEN

## Ids para os dados retirados do BACEN

ids_diarios <- c(21619,23,24, 1157,432)

# adcionar consumo eletrico 1406 mensal
ids_mensais <- c(433, 189, 190, 7447, 1785, 1788, 27788, 27791, 27810, 256 ,
            27813, 27815,10777, 24369, 24348,28766, 1619,10790,24381,3695,
             22701, 22707, 4503,10825,4480,4479,4472,4494,4473,4495,1406 )

print('1/11  -- baixando do site do Banco Central as series com frequência mensal')

df.bcb.mensal <- gbcbd_get_series(id = ids_mensais ,
                           first.date = '2000-01-01',
                           last.date = Sys.Date(),
                           format.data = 'wide',
                           #series.name = 'ABC',
                           use.memoise = TRUE, 
                           cache.path = tempdir(), # use tempdir for cache folder
                           do.parallel = FALSE)

print('2/11  -- Formatando as series do Bacen com frequência mensal')
# desemprego/desocupação = média de 24369 , 10777 ( essa serie foi descontinuada , estou fazendo a média das duas como quebra galho )

df.bcb.mensal$desemprego_desocupacao <- rowMeans( cbind(df.bcb.mensal$`id = 24369`  , df.bcb.mensal$`id = 10777`) ,na.rm = TRUE)

# rendimento real efetivo do trabalho = média de 24381 , 10790 ( essa serie foi descontinuada , estou fazendo a média das duas como quebra galho ) 

df.bcb.mensal$rendimento_real_efetivo_do_trabalho <- rowMeans( cbind(df.bcb.mensal$`id = 24381`  , df.bcb.mensal$`id = 10790`) ,na.rm = TRUE)

# menos :
# 1785 - Base Monetaria(média nos dias úteis do mês) , 27788 - M1 (média dos dias úteis do mês) - Novo ,
# 10777 - Taxa de desemprego - Região metropolitana - Brasil (na semana)  , Taxa de desocupação - PNADC,
# 24381 - Rendimento médio real efetivo de todos os trabalhos - PNADC , 10790 - Rendimento médio real efetivo das pessoas ocupadas - Total
  
df.bcb.mensal.final <- df.bcb.mensal[, -c(6,8,14,15,20,19)]


colnames(df.bcb.mensal.final) <- c('data','IPCA ','IGP-M ','IGP-DI ','IGP-10 ',
                                   'Base monetária restrita (saldo em final de período) ',
                                   'M1 (saldo em final de período)  ',
                                   'M2 (saldo em final de período)  ',
                                   'Taxa de juros de longo prazo - TJLP ',
                                   'M3 (saldo em final de período)  ',
                                   'M4 (saldo em final de periodo) ',
                                   'Número de horas trabalhadas – indústria de transformação (2006=100) ',
                                   'Estoque de empregos formais – Indústrias de transformação ','salario minimo',
                                   'Taxa de câmbio - Livre - Dólar americano (compra) - Fim de período ',
                                   'Transações correntes saldo ','Balanço de Pagamentos  - saldo ',
                                   'Dívida Líquida Pública do Brasil como per. do PIB','Dívida Líquida Pública do Brasil',
                                   'Dívida Interna do Governo Federal','Dívida Interna Líquida Governo edo Banco Central',
                                   'Dívida Líquida total dos Estados Total','Dívida Líquida estrangeiros dos Estados Total',
                                   'Dívida dos municipios total','Dívida dos municipios estrangeiros','consumo de energia eletrica'
                                   ,'desemprego desocupacao', 
                                   'rendimento real efetivo do trabalho'
                                   )


print('3/11  -- baixando do site do Banco Central as series com frequência diária')


df.bcb.diario <- gbcbd_get_series(id = ids_diarios ,
                                  first.date = '2000-01-01',
                                  last.date = Sys.Date(),
                                  format.data = 'wide',
                                  #series.name = 'ABC',
                                  use.memoise = TRUE, 
                                  cache.path = tempdir(), # use tempdir for cache folder
                                  do.parallel = FALSE)


## Pegando as utlimas observações dos dados com frequencia diaria
print('4/11  -- Formatando as series do Bacen com frequência diária')

df.bcb.diario_para_mensal <- df.bcb.diario %>% 
                                tidyr::fill(c('id = 21619','id = 23','id = 24','id = 1157','id = 432') ) %>%
                                group_by(strftime(ref.date,'%Y-%m')) %>% 
                                filter(ref.date == max(ref.date))

colnames(df.bcb.diario_para_mensal) <- c('data','taxa de câmbio Euro venda',
                                         'Saldo diário de depósitos de poupança', 'Captação líquida diária de depósitos de poupança',
                                         'Taxa média flutuantes DI de depósitos a prazo (CDB/RDB)',
                                         'Taxa de juros - Meta Selic definida pelo Copom' ,'x')


# transformando para data frame

df.bcb.diario_para_mensal.final <- as.data.frame(df.bcb.diario_para_mensal[,-7])

df.bcb.diario_para_mensal.final$data <- as.Date(df.bcb.diario_para_mensal.final$data)


# dados retirados do tesouro


print('5/11  -- carregando o arquivo do tesouro direto com as contas do Governo Central')


serie.tesouro.raw <- readxl::read_xlsx('./dados/serie_historica_fev21.xlsx',sheet = '1.1',range = 'A4:KE73' )


print('6/11  -- Formatando dados das contas do Governo Central')


serie.tesouro.raw <- t(serie.tesouro.raw[c(1,7,8,9,12,13,39,66),-1])
colnames(serie.tesouro.raw ) <- c('data', 'Receita total de impostos' , 'Imposto_importacao',
                                  'IPI','Cofins','Pis_pasep','Despesa total',
                                  'Resultado primário Governo central')

serie.tesouro <- as.data.frame(serie.tesouro.raw,row.names = FALSE)

serie.tesouro[,1]  <-  as.Date(serie.tesouro[,1], origin = "1899-12-30" )

# dados de expectativa

## consultando os dados do bacen

print('7/11  -- carregando os dados de expectativa de inflação do FOCUS')


expectativas.raw <- get_monthly_market_expectations('IPCA')

expectativas.top5.raw <- get_top5s_monthly_market_expectations('IPCA')

## limpando os dados 

print('8/11  -- Formatando dados de expectativa de inflação')


expectativas.raw <- expectativas.raw[which(expectativas.raw$base == 0 ),]

expectativas.top5.raw <- expectativas.top5.raw[which(expectativas.top5.raw$type == 'C' ),]

## calcula qual mês é o calculo
expectativas.raw$t <-  (as.yearmon(paste0(expectativas.raw$reference_month,'-01')) - as.yearmon(expectativas.raw$date))*12 +1

expectativas.top5.raw$t <-  (as.yearmon(as.Date(paste0('01/',expectativas.top5.raw$reference_month),'%d/%m/%Y')) - as.yearmon(expectativas.top5.raw$date))*12 +1

## retira projeções para meses passados
expectativas.raw.1 <- expectativas.raw[which(expectativas.raw$t >= 1 ),]

expectativas.top5.raw.1 <- expectativas.top5.raw[which(expectativas.top5.raw$t >= 1 ),]

## Evita problema de erro numérico 
expectativas.raw.1$t <- round(expectativas.raw.1$t)

expectativas.top5.raw.1$t <- round(expectativas.top5.raw.1$t)

## separa os dados desejados
expectativas_empilhadas <- as.data.frame(expectativas.raw.1[,c('date','median','t')])

expectativas_desvpad_empilhadas <- as.data.frame(expectativas.raw.1[,c('date','sd','t')])

expectativas_empilhadas.top5 <- as.data.frame(expectativas.top5.raw.1[,c('date','median','t')])

## desempilha os dados
expectativas_desempilhado <- reshape(expectativas_empilhadas ,
                              idvar = 'date',v.names = 'median',timevar = 't' ,direction = 'wide')

expectativas_desvpad_desempilhado <- reshape(expectativas_desvpad_empilhadas ,
                                     idvar = 'date',v.names = 'sd',timevar = 't' ,direction = 'wide')

expectativas_desempilhado.top5 <- reshape(expectativas_empilhadas.top5 ,
                                     idvar = 'date',v.names = 'median',timevar = 't' ,direction = 'wide')


## ordenas as colunas ... ignorar o aviso
expectativas_desempilhado <- expectativas_desempilhado[,c(1,order(as.numeric(stringr::str_replace(colnames(expectativas_desempilhado),'median.','')))[-ncol(expectativas_desempilhado)])]

expectativas_desvpad_desempilhado <- expectativas_desvpad_desempilhado[,c(1,order(as.numeric(stringr::str_replace(colnames(expectativas_desvpad_desempilhado),'sd.','')))[-ncol(expectativas_desvpad_desempilhado)])]

expectativas_desempilhado.top5 <- expectativas_desempilhado.top5[,c(1,order(as.numeric(stringr::str_replace(colnames(expectativas_desempilhado.top5),'median.','')))[-ncol(expectativas_desempilhado.top5)])]

## nomea as novas colunas
colnames(expectativas_desempilhado) <- gsub('median.','',colnames(expectativas_desempilhado))

colnames(expectativas_desvpad_desempilhado) <- gsub('sd.','',colnames(expectativas_desvpad_desempilhado))

colnames(expectativas_desempilhado.top5) <- gsub('median.','',colnames(expectativas_desempilhado.top5))



## 
# t + {1,2,...13} median
#View(expectativas_desempilhado[,1:14])
# t + {1,2,...13} median top 5
#View(expectativas_desempilhado.top5[,1:14])
# t + {1 ,2} median^2
#View(expectativas_desempilhado[,2:3]^2)
# t + 12 median^2
#View(expectativas_desempilhado[,12]^2)
# t+ {1,2} std
#View(expectativas_desvpad_desempilhado[,1:3])


# ajustando para mensal os dados das expectativas do IPCA

dados.expectativas <- cbind(expectativas_desempilhado[,1:14],expectativas_desempilhado[,2:3]^2 ,
                            expectativas_desempilhado[,12]^2 , expectativas_desvpad_desempilhado[,2:3])

colnames(dados.expectativas) <- c('data', paste0('e_ipca mediana t + ',1:13), paste0('e_ipca mediana^2 t + ',1:2),
                                  paste0('e_ipca mediana^2 t + ',12), paste0('e_ipca desvio padrao t + ',1:2) )

dados.expectativas.final <- dados.expectativas %>% 
                                group_by(strftime(data,'%Y-%m')) %>% 
                                filter(data == max(data))

# transformando para data frame 
dados.expectativas.final <- as.data.frame(dados.expectativas.final)

dados.expectativas.final$data <- as.Date(dados.expectativas.final$data)

# ajustando para mensal os dados das expectativas top 5

dados.expectativas.top5 <- expectativas_desempilhado.top5[,1:14]

colnames(dados.expectativas.top5) <- c('data', paste0('e_ipca_top5 mediana t + ',1:13))

dados.expectativas.top5.final <- dados.expectativas.top5 %>% 
                                    group_by(strftime(data,'%Y-%m')) %>% 
                                    filter(data == max(data))

# transformando para data frame 
dados.expectativas.top5.final <- as.data.frame(dados.expectativas.top5.final)

dados.expectativas.top5.final$data <- as.Date(dados.expectativas.top5.final$data)


# pegando índice do bovespa

print('9/11 -- carregando os dados de expectativa de inflação do FOCUS')

# set dates
first.date <- as.Date('2000-01-01')
last.date <- Sys.Date()
freq.data <- 'daily'
# set tickers
tickers <- c('^BVSP')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()


print('10/11  -- Formatando as series do Bacen com frequência diária')

df.ibov <- l.out$df.tickers[,c('ref.date','price.adjusted')]

# transformando frequência diaria para mensal

df.ibov.diario_para_mensal <- df.ibov %>% 
  tidyr::fill( 'price.adjusted' ) %>%
  group_by(strftime(ref.date,'%Y-%m')) %>% 
  filter(ref.date == max(ref.date))

colnames(df.ibov.diario_para_mensal) <- c('data','preço.ajustado' ,'x')


# transformando para data frame

df.ibov.diario_para_mensal <- as.data.frame(df.ibov.diario_para_mensal[,-3])

df.ibov.diario_para_mensal$data <- as.Date(df.ibov.diario_para_mensal$data)


print('11/11  -- Agrupando os dados para criar o data frame final')


# junta os dados todos

df1 <- xts(df.bcb.mensal.final[,-1],order.by = df.bcb.mensal.final[,1])
#df1 <- to.monthly(xts(df.bcb.mensal.final[,-1],order.by = df.bcb.mensal.final[,1]),OHLC=FALSE)
df2 <- xts(df.bcb.diario_para_mensal.final[,-c(1,8)], order.by = df.bcb.diario_para_mensal.final[,1])
df3 <- xts(serie.tesouro[,-1],order.by = serie.tesouro[,1])
df4 <- xts(dados.expectativas.final[,-c(1,20)],order.by =  dados.expectativas.final[,1])
df5 <- xts(dados.expectativas.top5.final[,-c(1,15)], order.by = dados.expectativas.top5.final[,1])
df6 <- xts(df.ibov.diario_para_mensal[,2] , order.by = df.ibov.diario_para_mensal$data )
colnames(df6) <- 'ibovespa'


dfs <- list(df1,df2,df3,df4,df5,df6)

for (i in 1:length(dfs)){

  dfs[[i]] <- period.apply(dfs[[i]], endpoints(dfs[[i]], "months"), 
                      function(x) first(na.locf(x, fromLast=TRUE)))
  index(dfs[[i]]) <- as.Date(as.yearmon(index(dfs[[i]])))
}

df.final <- merge(dfs[[1]],dfs[[2]],dfs[[3]],dfs[[4]],dfs[[5]],dfs[[6]])
df.final <- df.final[,-33]

# para medir o tempo que rodou
fim <- Sys.time()

total <- (fim - comeco)

print(paste0('tempo total coletando os dados : ',format(total, digits = 3)) )


# removendo da memoria todos objetos temporários 

rm(list = ls()[-grep('df.final',ls())])

saveRDS(df.final,file = 'df.final')
