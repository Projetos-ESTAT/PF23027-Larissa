source("rdocs/source/packages.R")
pacman::p_load(
  "readxl", "dplyr", "ggplot2", "tidyr",
  "kableExtra", "ggcorrplot", "psych", "purrr",
  "caret", "gvlma", "lmtest", "MASS"
)
library(summarytools)
library(olsrr)
require(lmtest)
require(lawstat)
library(EnvStats)
library(car)
require(leaps)
library(caret) 
library(readxl)
library(Hmisc)
library(corrplot)
library(tidyverse)

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


banco <- read.xlsx("banco/perfis_cad_analiseestatistica_19_07.xlsx", sheetIndex = 1)
banco2 <- banco[banco$CAD + banco$PMP - banco$CC<1,]

reg1 <- lm(data = banco2, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + PMP + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)

summary(reg1)

plot(reg1$fitted.values,banco2$CAD)


reg2 <- lm(data = banco2, CAD ~ CC + PMP)

summary(reg2)

plot(reg2$fitted.values,banco2$CAD)

View(banco[banco$CAD + banco$PMP - banco$CC>1,])


reg3 <- lm(data = banco2, CAD ~ ALTITUDE + PROFUND + DENSIDADE + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)



summary(reg3)

plot(reg3$fitted.values,banco2$CAD)

plot(reg3$fitted.values,reg3$residuals)


reg4 <- lm(data = banco2, CAD ~ ALTITUDE + PROFUND)

summary(reg4)

plot(reg4$fitted.values,banco2$CAD)

plot(reg4$fitted.values,reg4$residuals)

# estimating the variance of y for different values of x
variance = lm(abs(reg4$residuals) ~ reg4$fitted.values)$fitted.values^2
# calculating the weights
weights = (1 / variance)
# weighted regression model
weighted_model = reg3 <- lm(data = banco2, CAD ~ ALTITUDE + PROFUND + DENSIDADE + AREIA_GROS  + SILTE + ARGILA, weights = weights)
summary(weighted_model)
plot(weighted_model$fitted.values,regStep$residuals)
plot(weighted_model$fitted.values,banco2$CAD)
bptest(weighted_model)



banco1<-banco%>%
  dplyr::select(CAD, Altitude = ALTITUDE, `Areia Fina`= AREIA_FINA,`Areia Grossa`=AREIA_GROS, Argila=ARGILA,  Densidade=DENSIDADE, Profundidade = PROFUND,   Silte=SILTE)
corr_matrix = round(cor(banco1), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)




regStep <- step(reg3)

summary(regStep)

plot(regStep$fitted.values,banco2$CAD)

plot(regStep$fitted.values,regStep$residuals)

# estimating the variance of y for different values of x
variance = lm(abs(regStep$residuals) ~ regStep$fitted.values)$fitted.values^2
# calculating the weights
weights = ((1 / variance))
# weighted regression model
weighted_model = reg3 <- lm(data = banco2, CAD ~ ALTITUDE + PROFUND + DENSIDADE + AREIA_GROS  + SILTE + ARGILA, weights = weights)
summary(weighted_model)
plot(weighted_model$fitted.values,regStep$residuals)
plot(weighted_model$fitted.values,banco2$CAD)
bptest(weighted_model)













box <- boxcox(data = banco2, CAD ~ log(ALTITUDE) + log(PROFUND) + log(DENSIDADE) + log(AREIA_GROS) + log(AREIA_FINA) + log(SILTE) + log(ARGILA))

lambda <- box$x[which.max(box$y)]

reg32 <- lm(data = banco2, (CAD ^ lambda - 1) / lambda ~ log(ALTITUDE) + log(PROFUND) + log(DENSIDADE) + log(AREIA_GROS) + log(AREIA_FINA) + log(SILTE) + log(ARGILA))

plot(reg32$fitted.values,banco2$CAD)

plot(reg32$fitted.values,reg32$residuals)


regStep2 <- step(reg32)

summary(regStep2)

plot(regStep2$fitted.values,banco2$CAD)

plot(regStep2$fitted.values,regStep2$residuals)

shapiro.test(reg32$residuals)
qqnorm(reg32$residuals)
qqline(reg32$residuals)
bptest(reg32)
