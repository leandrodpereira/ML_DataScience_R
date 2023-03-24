base = read.csv('credit_data.csv')
base$clientid = NULL
summary(base)

# Pré processamento, filtrando valores negativos e faltantes.
idade_invalida = base[base$age < 0 & !is.na(base$age), ]

# Estratégias:
# 1 - Apagar coluna
base$age = NULL

# 2 - Apagar apenas registros inconsistentes
base = base[base$age > 0, ]

# 3 - Preencher os dados manualmente [Pode ser inviável!] 
# 4 - Preenche os dados com um valor médio
mean(base$age, na.rm = TRUE)
# Calculando a média sem os valores negativos e gerando um novo data grid
media = mean(base$age[base$age > 0], na.rm = TRUE)
base$age = ifelse(base$age < 0, media, base$age)

#Identificando e substituindo valores faltantes (NA)
base[is.na(base$age), ]
base$age = ifelse(is.na(base$age), media, base$age)

#Aplicando escalonamento
base[, 1:3] = scale(base[, 1:3])

# Divisão entre treinamento e teste
library(caTools)
divisao = sample.split(base$income, SplitRatio = 0.75) 
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)
