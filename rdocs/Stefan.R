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


banco <- read.xlsx("banco/perfis_cad_analiseestatistica_19_07.xlsx", sheetIndex = 1)


reg1 <- lm(data = banco[banco$CAD + banco$PMP - banco$CC<1,], CAD ~ ALTITUDE + PROFUND + DENSIDADE + CC + PMP + AREIA_GROS + AREIA_FINA + SILTE + ARGILA)

summary(reg1)

plot(reg1$fitted.values,banco$CAD)


reg2 <- lm(data = banco, CAD ~ CC + PMP)

summary(reg2)

plot(reg2$fitted.values,banco$CAD)

View(banco[banco$CAD + banco$PMP - banco$CC>1,])
