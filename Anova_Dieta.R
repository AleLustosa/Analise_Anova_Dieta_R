diet = read.csv("diet3.csv")

View(diet)

#########
### An�lise mista de vari�ncia - efeitos simples principais 
#########


### Os efeitos principais da vari�vel tempo representam
### as diferen�as m�dias de peso entre os tr�s momentos da dieta para cada sexo separadamente



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

### Obtendo as vari�veis dependentes com o modelo model2

model2 <- Anova(model, idata = moments_frm, idesign = ~moments, type="III")

summary(model2, multivariate=F)

### O mesmo procedimento ser� usado para as mulheres

#######################


### Os principais efeitos da vari�vel g�nero representam
### as diferen�as m�dias de peso entre os individuos masculinos e femininos
### para cada momento da dieta: in�cio, meio, fim


### Eles consistem em tr�s pares de diferen�as

### peso m�dio masculino - peso m�dio feminino, no in�cio da dieta
### peso m�dio masculino - peso m�dio feminino, no meio da dieta
### peso m�dio masculino - peso m�dio feminino, no final da dieta

### Iremos avaliar essas diferen�as usando o teste t de amostra independente

### A primeira diferen�a (come�o da dieta)

t.test(diet$weight_beg~diet$gender, var.equal=T)


### A segunda diferen�a (meio da dieta)

t.test(diet$weight_mid~diet$gender, var.equal=T)


### A terceira diferen�a (final da dieta)

t.test(diet$weight_end~diet$gender, var.equal=T)


