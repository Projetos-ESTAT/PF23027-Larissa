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

pacman::p_load(
  "readxl", "dplyr", "ggplot2", "tidyr",
  "kableExtra", "ggcorrplot", "psych", "purrr",
  "caret", "gvlma", "lmtest"
)
## pacotes
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
setwd('D:/Downloads/ESTAT/PF23027-Larissa/banco')
banco <- read_excel("perfis_cad_analiseestatistica_19_07.xlsx")
caminho <- "D:/Downloads/ESTAT/PF23027-Larissa/resultados"

####################################################################################################################

############### matriz de correlações e correlograma ###############

# seleção das variáveis quantitativas
banco1<-banco%>%
  select(CAD, Altitude = ALTITUDE, `Areia Fina`= AREIA_FINA,`Areia Grossa`=AREIA_GROS, Argila=ARGILA, CC,  Densidade=DENSIDADE, Profundidade = PROFUND, PMP, Domínio = dominios_n, Perfil=PERFIL,  Silte=SILTE, Declividade=DECLIVID, Drenagem=DRENAGEM)

plot(banco1)
########### transformação BOX-COX ###########

model1<-lm((CAD)^0.4~Silte+Altitude+Densidade + `Areia Fina`+`Areia Grossa`+ as.factor(Declividade)+as.factor(Drenagem), data=banco1) # + as.factor(Declividade)+as.factor(Drenagem)   # + 1/PMP + 1/CC
summary(model1)

' a profundidade tira a normalidade e argila dá ruim com o vif, sabe-se lá pq ' 

' A transformação de Box-Cox é uma técnica estatística usada para estabilizar a variância e tornar os dados mais próximos de uma distribuição normal. '

confint(model1, level=.95)

########### Normalidade ###########

residuos=model1$residuals

qqnorm(residuos)
qqline(residuos)
shapiro.test(residuos)
ols_test_normality(residuos)

########### Multicolinearidade ###########

vi<-vif(model1)
vi
mean(vi)

' > vi
                               GVIF Df GVIF^(1/(2*Df))
Silte                  10469.732734  1      102.321712
Altitude                   1.284556  1        1.133382
Densidade                  1.543484  1        1.242370  # com argila
Argila                 10497.609126  1      102.457841
`Areia Fina`           16018.537413  1      126.564361
`Areia Grossa`         29099.250170  1      170.585023
as.factor(Declividade)     1.562396  4        1.057362
as.factor(Drenagem)        1.779032  6        1.049177
> mean(vi)
[1] 2775.571 '

########### painel geral ###########

x11()
par(mfrow=c(2,2))
plot(model1)

' Warning message:
not plotting observations with leverage one:
  588  '

########### homocedasticidade ###########

bptest(model1)  # AQUI DEU RUIM

plot(model1$fitted.values,banco1$CAD) 

########### independência ###########

durbinWatsonTest(model1)

' lag Autocorrelation D-W Statistic p-value
   1       0.3675585       1.26215       0
 Alternative hypothesis: rho != 0 '

plot(resid(model1)) 

# Observações influentes

outlierTest(model1) 

' No Studentized residuals with Bonferroni p < 0.05
Largest |rstudent|:
     rstudent unadjusted p-value Bonferroni p
876 -3.667952         0.00025529      0.30838 '

# Cook

indice<-c(1:1209)
plot(indice,cooks.distance(model1), type = "l")
plot(model1,which=4)

ols_plot_cooksd_chart(model1)

' 687, 834 e 193 '

# Stepwise

modelo_inicial <- lm(model1, data = banco1)
modelo_stepwise <- step(modelo_inicial, direction = "both")

' Step:  AIC=608.94
(CAD)^0.4 ~ Silte + Altitude + `Areia Fina` + `Areia Grossa` + 
    as.factor(Declividade) + as.factor(Drenagem) '  # a densidade sai

