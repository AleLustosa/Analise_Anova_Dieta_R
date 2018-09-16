diet = read.csv("diet3.csv")

View(diet)

#########
### Análise mista de variância - efeitos simples principais 
#########


### Os efeitos principais da variável tempo representam
### as diferenças médias de peso entre os três momentos da dieta para cada sexo separadamente



######## Criado DF apenas com os sujeitos do sexo masculino


dietm <- diet[diet$gender=="male",]

### Criacao de Df com os niveis de tempo de tratamento

moments <- c("beginning", "middle", "end")

moments_frm <- data.frame(moments)

View(moments_frm)

### Constucao de uma matriz com os valores da medida (peso Kg).

weight_male <- cbind(dietm$weight_beg, dietm$weight_mid, dietm$weight_end)

#### get the means of the dependent variables

model <- lm(weight_male~1)

### Obtendo as variáveis dependentes com o modelo model2

model2 <- Anova(model, idata = moments_frm, idesign = ~moments, type="III")

summary(model2, multivariate=F)

### O mesmo procedimento será usado para as mulheres

#######################


### Os principais efeitos da variável gênero representam
### as diferenças médias de peso entre os individuos masculinos e femininos
### para cada momento da dieta: início, meio, fim


### Eles consistem em três pares de diferenças

### peso médio masculino - peso médio feminino, no início da dieta
### peso médio masculino - peso médio feminino, no meio da dieta
### peso médio masculino - peso médio feminino, no final da dieta

### Iremos avaliar essas diferenças usando o teste t de amostra independente

### A primeira diferença (começo da dieta)

t.test(diet$weight_beg~diet$gender, var.equal=T)


### A segunda diferença (meio da dieta)

t.test(diet$weight_mid~diet$gender, var.equal=T)


### A terceira diferença (final da dieta)

t.test(diet$weight_end~diet$gender, var.equal=T)


