base = read.csv('census.csv')
base$X = NULL

# Divisão entre treinamento e teste
library(caTools)
divisao = sample.split(base$income, SplitRatio = 0.05) 
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

library(RoughSets)

#Conversão de formato
dt_treinamento = SF.asDecisionTable(dataset = base_treinamento, decision.attr = 15)
dt_teste = SF.asDecisionTable(dataset = base_teste, decision.attr = 15)

#Discretização das variáveis numéricas
intervalos = D.discretization.RST(dt_treinamento, nOfInterval = 4)
dt_treinamento = SF.applyDecTable(dt_treinamento, intervalos)
dt_teste = SF.applyDecTable(dt_teste, intervalos)

classificador = RI.CN2Rules.RST(dt_treinamento, K = 1)
print(classificador)

previsoes = predict(classificador, newdata = dt_teste)
matriz_confusao = table(dt_teste[, 15], unlist(previsoes))
print (matriz_confusao)
library(caret)
confusionMatrix(matriz_confusao)

#Accuracy : 0.7682 

