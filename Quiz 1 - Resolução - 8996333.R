#Quiz 1 - EAE0324 - Econometria I
#Prof�. Dra. Paula Pereda da Silva
#Aluno: Jo�o Pedro Viegas de Moraes Leme - n� USP 8996333

#Resolu��o#

install.packages("mfx")
install.packages("haven")
install.packages("sandwich")
install.packages("lmtest")
install.packages("fastDummies")
install.packages("knitr")
install.packages("kableExtra")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readxl")

#[1(a)]#

library(mfx)
library(haven)
library(sandwich)
library(lmtest)
library(fastDummies)
library(knitr)
library(kableExtra)
library(ggplot2)
library(tidyverse)
library(readxl)

df <- read.csv('quiz1.csv')
head(df)
names(df)

#[1(b)]#

#Calculando a propor��o de mulheres brancas#

mean(df$race == "white")

#A propor��o de mulheres brancas na amostra � de 72.88%#

#Calculando a propor��o de mulheres com 40 anos ou mais#

mean(df$age >= 40)

#A propor��o de mulheres com mais de 40 anos na amostra � de 44.43%#

#[1(c)]#

#Calculando a m�dia e desvio padr�o da vari�vel "age"#

mean(df$age) 
sd(df$age)

#A m�dia de idade na amostra � de 39.15 anos e seu desvio-padr�o � de 3.06#

#Calculando a m�dia e desvio-padr�o da vari�vel "wage"#

mean(df$wage)
sd(df$wage)

#A m�dia salarial da amostra � de 7.766, enquanto seu desvio-padr�o � 5.755#

#Calculando a m�dia e o desvio-padr�o da vari�vel "ttl_exp"#

mean(df$ttl_exp)
sd(df$ttl_exp)

#A m�dia da vari�vel "ttl_exp" � 12.53498 e seu desvio-padr�o � de 4.610208#

#[1(d)]#

#Calculando a vari�ncia do sal�rio e da idade na amostra#

var(df$wage)
var(df$age)

#Calculando a covari�ncia entre idade e sal�rio#

cov(df$wage, df$age)

#[1(e)]#

#Plotando um histograma do sal�rio#

hist(df$wage, xlab = "Sal�rio", ylab = "Frequ�ncia", col = blues9, freq = FALSE)

#[1(f)]#

#Descobrindo o efeito sobre o sal�rio de um ano a mais de estudo atrav�s do
#modelo: wage = beta0 + beta1(grade) + u#

wages.lm <- lm( formula = wage ~ grade, data = df  )
summary(wages.lm)$coefficients
plot(wages.lm)

#De acordo com o resultado da regress�o simples proposta acima, um ano adicional
#de educa��o implica em um aumento salarial de 0.7431729.#

#[1(g)]#

#Descobrindo o efeito sobre o sal�rio de um ano a mais de estudo atrav�s do
#modelo: wage = beta0 + beta1(race) + u#

wages_2.lm <- lm( formula = wage ~ race, data = df )
summary(wages_2.lm)$coefficients
plot(wages_2.lm)

#De acordo com o resultado da regress�o simples proposta acima, o coeficiente 
#de uma regress�o simples de race no sal�rio � de 1.165598.#