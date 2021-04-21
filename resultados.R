# Apresentação

library(ggplot2)
library(kableExtra)


# tabela de erros

# adcionando as previsoes do FOCUS aos resultados para comparação

for (horizonte in 1:13){
  
  
  todas_previsoes_por_horizonte[[horizonte]]$media_todos_modelos <-  as.xts(as.numeric(rowMeans(todas_previsoes_por_horizonte[[horizonte]][,1:15], na.rm = TRUE)), 
                                                                    order.by = index(dados.transformados[109:216, 'IPCA.']) ) 

  # mediana das projeçoes do FOCUS
  # TODO :
  # quebra galho pq ta faltando a projeção de mercado para t+7 TENHO QUE INVESTIGAR ISSO!
  #if(horizonte != 7 ){
    todas_previsoes_por_horizonte[[horizonte]]$FOCUS <-  as.xts(as.numeric(dados.transformados[(109-(horizonte)):(216-(horizonte)),
                                                                                    paste0('e_ipca.mediana.t...',horizonte) ]), 
                                                                order.by = index(dados.transformados[109:216, 'e_ipca.mediana.t...1']) ) 
  #} else {
  #  todas_previsoes_por_horizonte[[horizonte]]$FOCUS <- as.xts(rep(0,108), 
  #                                                             order.by = index(dados.transformados[109:216, 'e_ipca.mediana.t...1']) ) 
    
  #}
  # mediana dos TOP5 forecasters do FOCUS
  
  todas_previsoes_por_horizonte[[horizonte]]$TOP5FOCUS <-  as.xts(as.numeric(dados.transformados[(109-(horizonte)):(216-(horizonte)),
                                                                                             paste0('e_ipca_top5.mediana.t...',horizonte) ]), 
                                                              order.by = index(dados.transformados[109:216, 'e_ipca.mediana.t...1']) ) 
  
  
  
}



####

horizonte <- 1

erros <- todas_previsoes_por_horizonte[[horizonte]][,-1]

# erros das series 
for ( i in 1:18){

  erros[,i] <- todas_previsoes_por_horizonte[[horizonte]]$IPCA. - todas_previsoes_por_horizonte[[horizonte]][,i+1]

}

# Calculando os Raiz dos erros quadráticos médios 

erros.rmse <- as.data.frame(matrix(0, nrow = 13,ncol =18)  )

colnames(erros.rmse) <- colnames(todas_previsoes_por_horizonte[[1]])[-1]

for (horizonte in 1:13) {
  
  erros.rmse[horizonte,] <- apply(as.matrix(todas_previsoes_por_horizonte[[horizonte]][-(1:2),-1]),2,
                                  RMSE,y=as.matrix(todas_previsoes_por_horizonte[[horizonte]][-(1:2),1]) )
  
}

# Calculando a média dos erros absolutos

erros.mae <- as.data.frame(matrix(0, nrow = 13,ncol =18)  )

colnames(erros.mae) <- colnames(todas_previsoes_por_horizonte[[1]])[-1]

for (horizonte in 1:13) {
  
  erros.mae[horizonte,] <- apply(as.matrix(todas_previsoes_por_horizonte[[horizonte]][-(1:2),-1]),2,
                                 MAE,y=as.matrix(todas_previsoes_por_horizonte[[horizonte]][-(1:2),1]) )
  
}


erros.rmse.mae <- as.data.frame(matrix(0, nrow = 26,ncol =14)  )

#colnames(erros.rmse.mae) <- colnames(todas_previsoes_por_horizonte[[1]])[-1]

for (horizonte in 1:13) {
    
  rmse.ref <- RMSE(todas_previsoes_por_horizonte[[horizonte]][-(1:2),3] , 
                   todas_previsoes_por_horizonte[[horizonte]][-(1:2),1])
    
  mae.ref <- MAE(todas_previsoes_por_horizonte[[horizonte]][-(1:2),3] , 
              todas_previsoes_por_horizonte[[horizonte]][-(1:2),1])
  
  
  for ( modelo in 1:18) {
    
    erros.rmse.mae[(2*modelo -1),1] <- colnames(todas_previsoes_por_horizonte[[1]][-(1:2),modelo+1])
    erros.rmse.mae[(2*modelo ),1] <- colnames(todas_previsoes_por_horizonte[[1]][-(1:2),modelo+1])
    
    
    erros.rmse.mae[(2*modelo -1),horizonte+1] <- RMSE(todas_previsoes_por_horizonte[[horizonte]][-(1:2),modelo+1] , 
                          todas_previsoes_por_horizonte[[horizonte]][-(1:2),1])/rmse.ref
    erros.rmse.mae[(2*modelo),horizonte+1] <- MAE(todas_previsoes_por_horizonte[[horizonte]][-(1:2),modelo+1] , 
                          todas_previsoes_por_horizonte[[horizonte]][-(1:2),1])/mae.ref
    
  }
}


# Tabelas de erros


#row.names(tabela_de_erros) <- c(sub('previsoes_ipca_','',colnames(erros.rmse)))

tabela.erros <- erros.rmse.mae
tabela.erros[seq(1,36,2),-1] <-  format(unlist(erros.rmse.mae[seq(2,36,2),-1]),digits = 2)
tabela.erros[seq(2,36,2),-1] <- paste0("(", format(unlist(erros.rmse.mae[seq(2,36,2),-1]),digits = 2),")")

tabela.erros[,1] <- sub('previsoes_ipca_','',tabela.erros[,1])

## tabeka dos RMSE e MAE por periodo 

kbl(tabela.erros , 
    caption = "Tabela - Indicadores (RMSE e MAE) por horizonte para estratégias selecionadas",
    align = "c", digits = 2, format = "html",
    col.names = c('modelo',paste0('t + ',1:13)), escape = F ) %>%
    kable_classic(full_width = F, html_font = "Cambria") %>%
    column_spec(1, bold = FALSE) %>%
    collapse_rows(columns = 1, valign = "top") %>%
    footnote(general = "Os números em parentesis são os valores do erro médio absoluto o restante são as raize quadrática média dos erros",
             general_title = "Nota: "
    ) 




# empilhando os dados para o ggplot
# plotando 

df_previsoes_ipca <- fortify(todas_previsoes_por_horizonte[[12]][-(1:2),])

df_previsoes_ipca$Index <- as.Date(df_previsoes_ipca$Index)

df_previsoes_ipca <- data.frame(df_previsoes_ipca[1],stack(df_previsoes_ipca[2:ncol(df_previsoes_ipca)]))


ggplot() + 
  geom_line(data = df_previsoes_ipca,
            aes(x = Index , y= values , color = ind))


# empilhando todos horizontes 
df_empilhado_todos_horizontes_previsoes_ipca <- data.frame(Index = as.Date('2001-01-01'),values=0,
                                                           ind=as.character('modelo'), horizonte = as.character('T') )

for( horizonte in 1:13){

  df_empilhado_previsoes_ipca <- fortify(todas_previsoes_por_horizonte[[horizonte]][-(1:2),])
  
  df_empilhado_previsoes_ipca$Index <- as.Date(df_empilhado_previsoes_ipca$Index)
  
  df_empilhado_previsoes_ipca <- data.frame(df_empilhado_previsoes_ipca[1],stack(df_empilhado_previsoes_ipca[2:ncol(df_empilhado_previsoes_ipca)]))

  df_empilhado_previsoes_ipca$horizonte <- paste0('t + ',horizonte)
  
  df_empilhado_todos_horizontes_previsoes_ipca <- rbind(df_empilhado_todos_horizontes_previsoes_ipca,df_empilhado_previsoes_ipca)
}


df_empilhado_todos_horizontes_previsoes_ipca <- df_empilhado_todos_horizontes_previsoes_ipca[-1,]


# gráfico das previsções por horizonte

ggplot() + 
  geom_line(data = df_empilhado_todos_horizontes_previsoes_ipca[which(df_empilhado_todos_horizontes_previsoes_ipca$ind == 'IPCA.' |
                                                                        df_empilhado_todos_horizontes_previsoes_ipca$ind == 'previsoes_ipca_rfols' | 
                                                                        df_empilhado_todos_horizontes_previsoes_ipca$ind == 'FOCUS'),],
            aes(x = Index , y= values , color = ind)) + facet_wrap(~horizonte)


# filtrando para comparacao, não está funcionando deitado 

teste <- filter(df_empilhado_todos_horizontes_previsoes_ipca,
            ind == c('FOCUS'))

teste$IPCA <- filter(df_empilhado_todos_horizontes_previsoes_ipca,
                     ind == c('IPCA.'))[,'values']

ggplot() + 
  #geom_line() + 
  ggalt::geom_dumbbell(data = teste, aes(y = Index , x= values, xend= IPCA ),
                       size = .5, alpha = .5,shape = 4, show.legend = TRUE , colour_x = 'red',
                       colour_xend = 'blue'
                       )+ 
  facet_wrap(~horizonte)



# Correlarograma dos erros
colnames(erros) <- c(sub('previsoes_ipca_','',colnames(erros)))

corrplot::corrplot(cor(as.data.frame(erros[complete.cases(erros),-18])) ,
                   method = 'color',addCoef.col = "black",tl.col="black",
                   type = 'upper', tl.srt = 45, order = "AOE")
                   


# gráfico de erros acumulados por mes

teste <- as.xts(rowMeans(erros^2, na.rm =TRUE ), order.by = index(erros)) 

nr <- nrow(teste)
shade <- cbind(upper = rep(0, nr), lower = rep(.30, nr))
shade <- xts(shade, index(teste))


plot(teste, main = "", col = "darkblue", lwd = 1)

# corona virus
addPolygon(shade["2020-02/2020-09"], col = "lightpink", on = -1)

