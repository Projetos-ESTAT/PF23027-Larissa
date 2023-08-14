source("rdocs/source/packages.R")
pacman::p_load(
  "readxl", "dplyr", "ggplot2", "tidyr",
  "kableExtra", "ggcorrplot", "psych", "purrr",
  "caret", "gvlma", "lmtest"
)

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

## pacotes
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
setwd('D:/Downloads/ESTAT/PF23027-Larissa/banco')
banco <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")

# modelo inicial

banco1<-banco%>%
  select(Altitude = ALTITUDE, Profundidade = PROFUND, Densidade=DENSIDADE, CC, PMP, CAD, `Areia Fina`= AREIA_FINA,`Areia Grossa`=AREIA_GROS, Silte=SILTE, Argila=ARGILA)

reg1 <- lm(data = banco, CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + PMP + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)

summary(reg1)

plot(reg1$fitted.values,banco$CAD)

#View(banco[banco$CAD + banco$PMP - banco$CC>1,])

############### matriz de correlações e correlograma ###############

correlação<-as.data.frame(cor(banco1))

# 8.0 Matriz de correlação ----

dados <- banco1 |> # utilizar apenas valores numéricos!
  select(CAD, Altitude, Profundidade, Densidade, CC, PMP, `Areia Fina`,`Areia Grossa`,Silte, Argila)
res2 <- rcorr(as.matrix(dados))
# 8.1 Explorar os parâmetros: desta forma, as correlações insignificantes (<0.05) ficam de fora ----
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# 8.2 Desta forma, ficam com um X ----
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05)
ggsave("correlação.pdf", width = 158, height = 93, units = "mm") 

# # 8.3 Desta forma, inverte o triângulo ----
# corrplot(res2$r, type="lower", order="hclust", 
#          p.mat = res2$P, sig.level = 0.05, insig = "blank")

############# diagnostico inicial #############

# linearidade

plot(reg1$fitted.values, reg1$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # Linha horizontal em y = 0 para auxiliar na visualização

# Normalidade

shapiro.test(reg1$residuals)
ols_test_normality(reg1)

# Independencia

plot(reg1$residuals)

# correlação serial 

dwtest(reg1)

# Homocedasticidade (Breusch-Pagan)

bptest(reg1)

# multicolinearidade

(vi<-vif(reg1))
mean(vi)

'homocedasticidade foi atendida, a normalidade não, a multicolinearidade influencia nos valores das estimativas,
não há autocorrelação nos resíduos, independencia e linearidade ??? '

# TRANSFORMAÇÕES 

boxCox(reg1, ylab ="Log-Verossimilhança") # melhor valode lambda é um ( ou seja, sem transfotmação)

CAD1<- banco1$CAD^(-1)
reg2 <- lm(data = banco,CAD1~ALTITUDE + PROFUND + DENSIDADE + CC + PMP + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(reg2)
ols_test_normality(reg2)

CAD2<- log(banco1$CAD)
reg3 <- lm(data = banco,CAD2~ALTITUDE + PROFUND + DENSIDADE + CC + PMP + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)
summary(reg3)
ols_test_normality(reg3)


