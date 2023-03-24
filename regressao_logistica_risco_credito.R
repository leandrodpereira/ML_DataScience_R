base = read.csv('risco_credito.csv')
base = base[base$risco != "moderado", ]

#Foi necessario fazer o encode para Y
base$risco = factor(base$risco, levels = c('alto', 'baixo'))

classificador = glm(formula = risco ~., family = binomial, data = base)

# Previsão
#1: história: boa, dívida: alta, garantias: nenhuma, renda: >35
#2: hitóstia: ruim, dívida: alta, garantias: adequada, renda: <15

historia = c("boa", "ruim")
divida = c("alta", "alta")
garantias = c("nenhuma", "adequada")
renda = c("acima_35","0_15")

df = data.frame(historia, divida, garantias, renda)

probabilidades = predict(classificador, type = 'response', newdata = df)
resposta = ifelse(probabilidades > 0.5, 'baixo', 'alto')
