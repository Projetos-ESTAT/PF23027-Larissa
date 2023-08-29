source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

library(psych)
library(dplyr)
library(plyr)
library(readxl)
library(lmtest)
library(tidyverse)
library(car)
library(stats)
library(Metrics)
library(corrplot)
library(olsrr)
library(caret)


# Grande Fortaleza

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Grande Fortaleza") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + CC + AREIA_GROS + AREIA_FINA)

plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

bptest(modelo)

# linearidade

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)

## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)

indice<-c(1:43)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)





# Cariri (muitos valores tirados)

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Cariri") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

perfis <- perfis [(-55),]
perfis <- perfis [(-96),]
perfis <- perfis [(-71),]
perfis <- perfis [(-63),]
perfis <- perfis [(-68),]
perfis <- perfis [(-54),]
perfis <- perfis [(-61),]
perfis <- perfis [(-63),]
perfis <- perfis [(-50),]
perfis <- perfis [(-86),]

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

#modelo <- lm(data = perfis, CAD ~ PROFUND + CC + AREIA_GROS + AREIA_FINA + ARGILA)
#modelo <- lm(data = perfis, CAD ~ DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
#modelo <- lm(data = perfis, CAD ~ CC + AREIA_GROS + ARGILA)
modelo <- lm(data = perfis, CAD ~ CC + ARGILA)


plot(modelo$fitted.values,perfis$CAD)


## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

bptest(modelo)

# linearidade

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)


## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)

indice<-c(1:88)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)




# Centro Sul

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Centro Sul") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

perfis <- perfis [(-68),]
perfis <- perfis [(-65),]
perfis <- perfis [(-12),]
perfis <- perfis [(-85),]

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

modelo <- lm(data = perfis, CAD ~ PROFUND + CC + AREIA_GROS + AREIA_FINA + SILTE)
#modelo <- lm(data = perfis, CAD ~ CC + AREIA_GROS + AREIA_FINA + SILTE)


plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

modelo <- lm(data = perfis, log(CAD) ~ PROFUND + CC + AREIA_GROS + AREIA_FINA + SILTE)

bptest(modelo)

# linearidade

modelo <- lm(data = perfis, CAD ~ PROFUND + CC + AREIA_GROS + AREIA_FINA + SILTE)

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)

## Medidas importantes

modelo <- lm(data = perfis, CAD ~ PROFUND + CC + AREIA_GROS + AREIA_FINA + SILTE)


preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)

indice<-c(1:85)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)





# Litoral Leste

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Litoral Leste") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

perfis <- perfis [(-38),]
perfis <- perfis [(-19),]
perfis <- perfis [(-19),]

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

#modelo <- lm(data = perfis, CAD ~ CC + SILTE)
modelo <- lm(data = perfis, CAD ~ CC + AREIA_GROS)

plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

bptest(modelo)

# linearidade

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)

## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)

indice<-c(1:35)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)




# Litoral Oeste / Vale do Curu

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Litoral Oeste / Vale do Curu") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

modelo <- lm(data = perfis, CAD ~ PROFUND + CC + AREIA_GROS + AREIA_FINA)

plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

bptest(modelo)

# linearidade

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)

## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)

indice<-c(1:61)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)





# Maciço de Baturité

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Maciço de Baturité") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

modelo <- lm(data = perfis, CAD ~ PROFUND + DENSIDADE + CC + SILTE)

plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

bptest(modelo)

# linearidade

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)

## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)

indice<-c(1:44)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)





# Serra da Ibiapaba

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Serra da Ibiapaba") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

modelo <- lm(data = perfis, CAD ~ DENSIDADE + CC)

plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

bptest(modelo)

# linearidade

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)

## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)

indice<-c(1:63)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)





# Sertão Central

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Sertão Central") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

#perfis <- perfis[(-7),]

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

modelo <- lm(data = perfis, sqrt(CAD) ~ PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE)
#modelo <- lm(data = perfis, CAD ~ PROFUND + CC + AREIA_GROS + AREIA_FINA)

plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

modelo <- lm(data = perfis, CAD ~ PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE)

bptest(modelo)

# linearidade

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)

## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)

indice<-c(1:123)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)






# Sertão de Canindé

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Sertão de Canindé") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

perfis <- perfis[(-70),]
perfis <- perfis[(-70),]

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA)
plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

modelo <- lm(data = perfis, log(CAD) ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA)

bptest(modelo)

# linearidade

modelo <- lm(data = perfis, log(CAD) ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA)

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)

## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)
indice<-c(1:105)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)





# Sertão de Sobral

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Sertão de Sobral") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + CC + AREIA_FINA + SILTE)
plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

bptest(modelo)

# linearidade

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)

## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)
indice<-c(1:71)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)





# Sertão dos Crateús (muito longe)

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Sertão dos Crateús") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

perfis <- perfis[(-211),]
# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE)
plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

bptest(modelo)

# linearidade

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)

## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)
indice<-c(1:211)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)






# Sertão dos Inhamuns

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Sertão dos Inhamuns") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

perfis <- perfis[(-89),]
perfis <- perfis[(-91),]
perfis <- perfis[(-103),]

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

modelo <- lm(data = perfis, CAD ~ PROFUND + CC + AREIA_GROS + AREIA_FINA + SILTE)
plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

# Normalidade

shapiro.test(modelo$residuals)

# Homocedasticidade

bptest(modelo)

## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)
indice<-c(1:119)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)






# Vale do Jaguaribe (tirei muitos valores)

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao,) == "Vale do Jaguaribe") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

perfis <- perfis[(-45),]
perfis <- perfis[(-98),]
perfis <- perfis[(-43),]
perfis <- perfis[(-92),]
perfis <- perfis[(-91),]
perfis <- perfis[(-37),]
perfis <- perfis[(-40),]
perfis <- perfis[(-38),]
perfis <- perfis[(-46),]
perfis <- perfis[(-121),]
perfis <- perfis[(-121),]
perfis <- perfis[(-21),]
perfis <- perfis[(-62),]
perfis <- perfis[(-106),]

# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

#modelo <- lm(data = perfis, CAD ~ ALTITUDE + CC + AREIA_FINA + SILTE)
#modelo <- lm(data = perfis, CAD ~ ALTITUDE + DENSIDADE + CC + AREIA_GROS + AREIA_FINA)
#modelo <- lm(data = perfis, CAD ~ ALTITUDE + CC + AREIA_GROS + AREIA_FINA)
modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + CC + AREIA_GROS + AREIA_FINA)


plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

modelo <- lm(data = perfis, log(CAD) ~ ALTITUDE + PROFUND + CC + AREIA_GROS + AREIA_FINA)

bptest(modelo)

# linearidade

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + CC + AREIA_GROS + AREIA_FINA)

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)
## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)
indice<-c(1:124)


# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)







# SEM Sertão dos Crateús

perfis <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

perfis <- perfis %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N,Municipio,regiao,dominios_n) %>%
  filter(str_sub(regiao) != "Sertão dos Crateús") %>%
  select(CAD,ALTITUDE,PROFUND,DENSIDADE,CC,PMP,AREIA_GROS,AREIA_FINA,SILTE,ARGILA,
         DECLIVID,DRENAGEM,UTM_E,UTM_N)

perfis <- perfis[(-211),]
# Correlograma

matrizcor <- cor(perfis)

corrplot(matrizcor, method = "number")

# Variável dependente CAD

## Regressão

modelo <- lm(data = perfis, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(modelo)
coef(modelo)

# Stepwise

modelo_inicial <- lm(modelo, data = perfis)
modelo_stepwise <- step(modelo_inicial, direction = "both")

# Modelo stepwise encontrado

modelo <- lm(data = perfis, log(CAD) ~ ALTITUDE + PROFUND + DENSIDADE + CC + AREIA_GROS + SILTE + ARGILA)
plot(modelo$fitted.values,perfis$CAD)

## Pressupostos

resíduos = modelo$residuals

# histograma dos resíduos
hist(resíduos)

# Normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# Homocedasticidade

bptest(modelo)

# linearidade

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(modelo$residuals)

## Medidas importantes

preditos <- modelo %>% predict(perfis)

# R2

summary(modelo)

# MAE

MAE <- mean(abs(perfis$CAD - preditos));MAE

# RMSE

RMSE <- sqrt(mean((perfis$CAD - preditos)^2));RMSE

# MSE

residuos <- residuals(modelo)^2

MSE <- mean(residuos); MSE

# MSPR

(MSPR = sum((perfis$CAD-preditos)^2)/64)



# Observações influentes

medinflu1<-influence.measures(modelo)
indice<-c(1:211)

# Cook
plot(indice,cooks.distance(modelo), type = "l")
plot(modelo,which=4)

ols_plot_cooksd_chart(modelo)
