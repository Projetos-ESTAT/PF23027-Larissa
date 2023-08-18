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
  select(CAD, Altitude = ALTITUDE, `Areia Fina`= AREIA_FINA,`Areia Grossa`=AREIA_GROS, Argila=ARGILA, CC,  Densidade=DENSIDADE, Profundidade = PROFUND, PMP,  Silte=SILTE)

# matriz de correlações
correlação<-as.data.frame(cor(banco1))

' CAD e CC -> 0,94
  CAD e PMP -> 0,84
  CC E PMP ->  0,97 '

# correlograma
dados <- banco1 |> 
  select(CAD, Altitude, `Areia Fina`,`Areia Grossa`, Argila,  CC, Densidade, Profundidade,PMP, Silte)
res2 <- rcorr(as.matrix(dados))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# correlações insignificantes (<0.05) ficam com um X 
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05)
#ggsave(filename = file.path(caminho,"correlograma.png"), width = 158, height = 93, units = "mm") 
#ggsave(filename = file.path(caminho,"correlograma.pdf"), width = 158, height = 93, units = "mm")

' O argumento order = "hclust" indica um agrupamento hierárquico para determinar a ordem das variáveis, ou seja,
as variáveis que têm correlações mais semelhantes entre si serão agrupadas e exibidas próximas umas das outras'

####################################################################################################################

############### modelo inicial ###############

reg1 <- lm(data = banco, CAD^(-1) ~  ALTITUDE + PROFUND  + SILTE + ARGILA + AREIA_GROS + AREIA_FINA + DENSIDADE)

summary(reg1) 

' Adjusted R-squared:  0.9978 

variáveis significantes: CC, PMP (ALTITUDE e PROFUNDIDADE, considerando alpha=0,05)
Manter as demais variáveis não agrega valor ao modelo.'

plot(reg1$fitted.values,banco$CAD)

' º consideranto todas as variáveis
  º uma outlier visível, reta perfeita'

############# Observações influentes #############

medinflu1<-influence.measures(reg1)
indice<-c(1:1209)

# Utilizando hii
plot(indice,hatvalues(reg1),type="l")

' 623 '

# Betas
dfbetaPlots(reg1)
ols_plot_dfbetas(reg1)

' 843 e 623 '

# Dffits
plot(indice,abs(dffits(reg1)), type = "l")

' 843 '

# Cook
plot(indice,cooks.distance(reg1), type = "l")
plot(reg1,which=4)
ols_plot_cooksd_chart(reg1)

' 623, 782, 843'

# banco<-banco[-843,]

####################################################################################################################

############### diagnóstico inicial ###############

plot(banco1)

' relação linear só entre CC e PMP e elas com relação a CAD '

hist(banco1$CAD)
hist(banco1$Altitude)
hist(banco1$CC)
hist(banco1$PMP)
hist(banco1$Silte)
hist(banco1$Profundidade)
hist(banco1$Argila)
hist(banco1$`Areia Fina`)
hist(banco1$`Areia Grossa`)

' nenhuma das variáveis possui uma distribuição normalizada, a grande parte é assimétrica a esquerda '

resíduos = reg1$residuals

# histograma dos resíduos
hist(resíduos)

# normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)


' os dados não seguem distribuição normal e a regressão pode sofrer muita interferência da 
assimetria dos dados quando construídos sobre conjuntos que não possuam a distribuição normal.

º transformação dos dados '

# linearidade

plot(reg1$fitted.values, reg1$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(reg1$residuals)

# correlação serial 

dwtest(reg1)

' HO) DW=2
  H1) DW diferente de 2
não rejeita-se H0'

# Homocedasticidade (Breusch-Pagan)

bptest(reg1)

' HO) variancias iguais
  H1) há pelo menos uma diferente
p-value = 0.2011 > 0,05'

# multicolinearidade

(vi<-vif(reg1))
mean(vi)

' média do VIF muito superior a 1 (7328.465)'

reduced_data <- subset(banco1, select = -CAD)
corr_matrix = round(cor(reduced_data), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

' Podemos notar uma correlação forte  (valor é superior a 0,8) entre PMP e CC 
   Esse resultado faz sentido? se uma coisa for derivada da outra é possível tirar uma delas '

############### transformação ###############

############### novo modelo ###############

reg4<- lm(data = banco, log(CAD) ~  CC + PMP)

summary(reg4)

plot(reg4$fitted.values,banco$CAD)

resíduos = reg4$residuals

# histograma dos resíduos
hist(resíduos)

# normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# linearidade

plot(reg1$fitted.values, reg1$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(reg4$residuals)

# correlação serial 

dwtest(reg4)

# Homocedasticidade (Breusch-Pagan)

bptest(reg4)

# multicolinearidade

(vi<-vif(reg4))
mean(vi)

plot(reg4$fitted.values,banco$CAD)

####################################################################################################################

############### eliminar a multicolinearidade ############### 

' a) Retirada das variáveis multicolineares do modelo
  b) Análise de componentes principais para criação de fatores ortogonais não correlacionados'

############### criar um novo modelo retirando PMP ou CC ###############

reg2 <- lm(data = banco, CAD ~ ALTITUDE + PROFUND + CC + SILTE + ARGILA + AREIA_GROS + AREIA_FINA + DENSIDADE)

summary(reg2)

'Adjusted R-squared:  0.9102
 variáveis significantes: CC, ALTITUDE, PROFUNDIDADE e DENSIDADE
Manter as demais variáveis não agrega valor ao modelo.'

plot(reg2$fitted.values,banco$CAD)

' comparando os modelos:
 H0 ) a variável removida não têm significância
 H1 ) a variável é significante' 

anova(reg1,reg2)

' para ambas as retiradas, o valor p é muito pequeno (menor que 0,05), portanto rejeitamos a hipótese nula, 
  significando que o segundo modelo não é uma melhoria do primeiro. '

############### agrupamento de variaveis por PCA ###############

' agrupamento das variáveis multicolineares, por meio de técnicas de redução, como Análise de Componentes Principais '

dados<-select(banco, CC, PMP)
padronizados <- scale(dados)
pca <- prcomp(padronizados)
# componentes principais: pca$x
# pca$sdev^2 / sum(pca$sdev^2)
summary(pca)
PCA<- pca$x[, 1]

'0.98717986 0.01282014
uma componente é suficiente'

banco$PCA <- PCA
banco1$PCA <- PCA

'a primeira componente principal foi extraída e representa uma combinação linear de CC e PMP 
que captura a maior parte da variância dos dados, usada como uma nova variável não correlacionada'

banco2 <- banco1[, !colnames(banco1) %in% c("CC", "PMP")]

reg3 <- lm(data = banco, CAD ~ ALTITUDE + PROFUND + PCA + SILTE + ARGILA + AREIA_GROS + AREIA_FINA + DENSIDADE)

summary(reg3)

########## pressupostos ##########

resíduos = reg3$residuals

hist(resíduos)

# normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

# linearidade

plot(reg3$fitted.values, reg3$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

# independencia

plot(reg3$residuals)

# correlação serial 

dwtest(reg3)

' HO) DW=2
  H1) DW diferente de 2
 rejeita-se H0'

# Homocedasticidade (Breusch-Pagan)

bptest(reg3)

' HO) variancias iguais
  H1) há pelo menos uma diferente
p-value < 0,05'

# multicolinearidade

(vi<-vif(reg3))
mean(vi)

reduced_data <- subset(banco2, select = -CAD)
corr_matrix = round(cor(reduced_data), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

# # estimating the variance of y for different values of x
# variance = lm(abs(regStep$residuals) ~ regStep$fitted.values)$fitted.values^2
# # calculating the weights
# weights = ((1 / variance)/20)^2
# # weighted regression model
# weighted_model = reg3 <- lm(data = banco2, CAD ~ ALTITUDE + PROFUND + DENSIDADE + AREIA_GROS  + SILTE + ARGILA, weights = weights)
# summary(weighted_model)
# plot(weighted_model$fitted.values,regStep$residuals)
# plot(weighted_model$fitted.values,banco2$CAD)
# bptest(weighted_model)
