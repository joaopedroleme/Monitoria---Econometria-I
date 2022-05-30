#Quiz 1 - EAE0324 - Econometria I
#Profª. Dra. Paula Pereda da Silva
#Aluno: João Pedro Viegas de Moraes Leme - nº USP 8996333

#Resolução#

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

#Calculando a proporção de mulheres brancas#

mean(df$race == "white")

#A proporção de mulheres brancas na amostra é de 72.88%#

#Calculando a proporção de mulheres com 40 anos ou mais#

mean(df$age >= 40)

#A proporção de mulheres com mais de 40 anos na amostra é de 44.43%#

#[1(c)]#

#Calculando a média e desvio padrão da variável "age"#

mean(df$age) 
sd(df$age)

#A média de idade na amostra é de 39.15 anos e seu desvio-padrão é de 3.06#

#Calculando a média e desvio-padrão da variável "wage"#

mean(df$wage)
sd(df$wage)

#A média salarial da amostra é de 7.766, enquanto seu desvio-padrão é 5.755#

#Calculando a média e o desvio-padrão da variável "ttl_exp"#

mean(df$ttl_exp)
sd(df$ttl_exp)

#A média da variável "ttl_exp" é 12.53498 e seu desvio-padrão é de 4.610208#

#[1(d)]#

#Calculando a variância do salário e da idade na amostra#

var(df$wage)
var(df$age)

#Calculando a covariância entre idade e salário#

cov(df$wage, df$age)

#[1(e)]#

#Plotando um histograma do salário#

hist(df$wage, xlab = "Salário", ylab = "Frequência", col = blues9, freq = FALSE)

#[1(f)]#

#Descobrindo o efeito sobre o salário de um ano a mais de estudo através do
#modelo: wage = beta0 + beta1(grade) + u#

wages.lm <- lm( formula = wage ~ grade, data = df  )
summary(wages.lm)$coefficients
plot(wages.lm)

#De acordo com o resultado da regressão simples proposta acima, um ano adicional
#de educação implica em um aumento salarial de 0.7431729.#

#[1(g)]#

#Descobrindo o efeito sobre o salário de um ano a mais de estudo através do
#modelo: wage = beta0 + beta1(race) + u#

wages_2.lm <- lm( formula = wage ~ race, data = df )
summary(wages_2.lm)$coefficients
plot(wages_2.lm)

#De acordo com o resultado da regressão simples proposta acima, o coeficiente 
#de uma regressão simples de race no salário é de 1.165598.#