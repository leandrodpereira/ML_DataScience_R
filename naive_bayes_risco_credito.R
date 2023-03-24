base = read.csv("risco_credito.csv")

install.packages("e1071")
library(e1071)

#Treinamento: gerando uma tabela de probabilidades
classificador = naiveBayes(x = base[-5] , y = base$risco)
print (classificador)

# Previsão
#1: história: boa, dívida: alta, garantias: nenhuma, renda: >35
#2: hitóstia: ruim, dívida: alta, garantias: adequada, renda: <15

historia = c("ruim")
divida = c("alta")
garantia = c("adequada")
renda = c("0_15")

df = data.frame(historia, divida, garantia, renda)

previsao = predict(classificador, newdata = df)
print (previsao)


