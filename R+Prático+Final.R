#Função - média e mediana de 20 alunos de uma turma - Notas
notas <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) {
  #Para calcular a média
  soma_notas <- a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t
  media_notas <- soma_notas/20
  cat("A média das notas é:", media_notas, "|")
  
  #Para começar a calcular a mediana
  lista_notas <- c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
  tamanho_lista <- length(lista_notas)
  #Vamos ordenar a lista
  notas_ordenadas <- sort(lista_notas)
  #Calcular a mediana
  if(tamanho_lista%%2==0){
    nota_mediana <- mean(notas_ordenadas[(tamanho_lista/2)+0:1])
    cat(" A mediana das notas é:", nota_mediana, "|")
  } else {
    nota_mediana <- notas_ordenadas[(tamanho_lista+1)/2]
    cat(" A mediana das notas é:", nota_mediana, "|")
  }
  
  #Para 50% menores notas e maiores notas - média
  menores_notas_media <- mean(notas_ordenadas[0:(tamanho_lista/2)])
  maiores_notas_media <- mean(notas_ordenadas[((tamanho_lista/2)+1):tamanho_lista])
  cat(" A média das 50% menores notas é:", menores_notas_media, "|")
  cat(" A média das 50% maiores notas é:", maiores_notas_media)
}

#testando
notas(8,7,7,7,7,8,5,2,4,4,10,10,10,10,4,6,7,7,7,8)

#Função - média e mediana de 20 alunos de uma turma - Idade
idades <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) {
  #Para calcular a média
  soma_idades <- a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t
  media_idades <- soma_idades/20
  cat("A média das idades é:", media_idades, "|")
  
  #Para começar a calcular a mediana
  lista_idades <- c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
  tamanho_lista <- length(lista_idades)
  #Vamos ordenar a lista
  idades_ordenadas <- sort(lista_idades)
  #Calcular a mediana
  if(tamanho_lista%%2==0){
    idade_mediana <- mean(idades_ordenadas[(tamanho_lista/2)+0:1])
    cat(" A mediana das idades é:", idade_mediana, "|")
  } else {
    idade_mediana <- idades_ordenadas[(tamanho_lista+1)/2]
    cat(" A mediana das idades é:", idade_mediana, "|")
  }
  
  #Para 50% menores e maiores idades - média
  menores_idades_media <- mean(idades_ordenadas[0:(tamanho_lista/2)])
  maiores_idades_media <- mean(idades_ordenadas[((tamanho_lista/2)+1):tamanho_lista])
  cat(" A média das 50% menores idades é:", menores_idades_media, "|")
  cat(" A média das 50% maiores idades é:", maiores_idades_media)
}

#testando
idades(10,11,12,9,10,11,10,10,10,10,11,11,10,9,10,10,11,11,10,10)

#trazendo os vetores para fora das funções
notas <- c(8,7,7,7,7,8,5,2,4,4,10,10,10,10,4,6,7,7,7,8)
idades <- c(10,11,12,9,10,11,10,10,10,10,11,11,10,9,10,10,11,11,10,10)

df <- data.frame(notas, idades)

#ordenando
library(dplyr)
df_ordenado_idades <- df[order(df$idades), ]

#obtendo a média das notas para 50% menores idades
df_menor <- head(df_ordenado_idades, nrow(df_ordenado_idades)/2)
media_menor_idade_notas <-mean(df_menor$notas)

#obtendo a média das notas para 50% maiores idades
df_maior <- tail(df_ordenado_idades, nrow(df_ordenado_idades)/2)
media_maior_idade_notas <-mean(df_maior$notas)

#Há relação entre a idade e a nota?
cat(" A diferença percentual entre as maiores notas e as menores notas é de:", ((8.5/5.3)-1)*100)

cat(" A diferença percentual entre as maiores notas e as menores idades é de:", ((10.8/9.8)-1)*100)

cv_notas <- (sd(notas) / mean(notas)) * 100
cv_idades <- (sd(idades) / mean(idades)) * 100
cat("O coeficiente de variação das notas é:", cv_notas, "|")
cat("O coeficiente de variação das idades é:", cv_idades)

#Medida avançada
cor(idades,notas, method = "pearson")
