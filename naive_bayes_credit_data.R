base = read.csv('credit_data.csv')

#Preprocessamento
#Remove coluna id_cliente
base$clientid = NULL

#Substituindo valores negativos pela média
media = mean(base$age[base$age > 0], na.rm = TRUE)
base$age = ifelse(base$age < 0, media, base$age)

#Substituindo valores faltantes (NA)
base$age = ifelse(is.na(base$age), media, base$age)

#Aplicando escalonamento
base[, 1:3] = scale(base[, 1:3])

#Encode da classe
base$default = factor(base$default, levels = c(0,1))

# Divisão entre treinamento e teste
library(caTools)
divisao = sample.split(base$income, SplitRatio = 0.75) 
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

classifacador = naiveBayes(x = base_treinamento[-4], y = base_treinamento$default)
print(classifacador)

previsao = predict(classifacador, base_teste[-4])
matriz_confusao = table(base_teste[, 4], previsao)
#install.packages("caret")
library(caret)
confusionMatrix(matriz_confusao)

#Comparativo Accuracy:
#Preprocessamento + escalonamento = 0.914
#Preprocessamento = 0.936
#Sem Preprocessamento = 0.926  
