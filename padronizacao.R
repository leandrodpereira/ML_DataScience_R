idade = c(60, 35, 20)
renda = c(30000, 45000, 29500)

base = data.frame(idade, renda)

mean(base$idade)
mean(base$renda)

sd(base$idade)
sd(base$renda)

base = scale(base)
