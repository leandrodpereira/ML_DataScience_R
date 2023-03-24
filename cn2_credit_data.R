base = read.csv('credit_data.csv')
base$clientid = NULL
# Calculando a média sem os valores negativos e gerando um novo data grid
media = mean(base$age[base$age > 0], na.rm = TRUE)
base$age = ifelse(base$age < 0, media, base$age)

#Identificando e substituindo valores faltantes (NA)
base[is.na(base$age), ]
base$age = ifelse(is.na(base$age), media, base$age)

#Aplicando escalonamento
base[, 1:3] = scale(base[, 1:3])

#Enconde da classe
base$default = factor(base$default, levels = c(0,1))

# Divisão entre treinamento e teste
library(caTools)
divisao = sample.split(base$income, SplitRatio = 0.75) 
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

#install.packages('RoughSets')
library(RoughSets)

#Conversão de formato
dt_treinamento = SF.asDecisionTable(dataset = base_treinamento, decision.attr = 4)
dt_teste = SF.asDecisionTable(dataset = base_teste, decision.attr = 4)

#Discretização das variáveis numéricas
intervalos = D.discretization.RST(dt_treinamento, nOfInterval = 4)
dt_treinamento = SF.applyDecTable(dt_treinamento, intervalos)
dt_teste = SF.applyDecTable(dt_teste, intervalos)

classificador = RI.CN2Rules.RST(dt_treinamento, K = 5)
print(classificador)
previsoes = predict(classificador, newdata = dt_teste)
matriz_confusao = table(dt_teste[, 4], unlist(previsoes))
library(caret)
confusionMatrix(matriz_confusao)

#Para este algoritmo não há diferença de com ou sem pré-processamento