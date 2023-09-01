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
correlação<-as.data.frame(cor(banco1, method = "spearman"))

' CAD e CC -> 0,94
  CAD e PMP -> 0,84
  CC E PMP ->  0,97 '

' 1)A escala de medição deve ser uma escala ou relação de intervalo;
  2)As variáveis devem ser aproximadamente distribuídas;
  3)A associação deve ser linear; NÃO OK
  4)Não deve haver valores atípicos nos dados.

É necessário que as duas variáveis sejam medidas em um nível quantitativo contínuo.OK
A distribuição das variáveis deve ser semelhante à curva normal. NÃO OK'

' refazer para SPEARMAN '

plot(banco1)

combinacoes <- combn(names(banco1), 2, simplify = FALSE)  # Todas as combinações de variáveis

# Crie um layout de várias parcelas para o painel
n <- length(combinacoes)
par(mfrow = c(ceiling(sqrt(n)), ceiling(sqrt(n))))

# Crie gráficos de dispersão para todas as combinações
for (i in 1:n) {
  x_var <- combinacoes[[i]][1]
  y_var <- combinacoes[[i]][2]
  p <- ggplot(banco1) +
    aes(x = combinacoes[[i]][1], y = combinacoes[[i]][2]) +
    geom_point(colour = "#A11D21", size = 3) +
    labs(
      x =  x_var,
      y = y_var
    ) +
    theme_estat()
  print(p)
}



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

' nenhuma das variáveis possui uma distribuição normalizada '

# correlograma
# dados <- banco1 |> 
#   select(CAD, Altitude, `Areia Fina`,`Areia Grossa`, Argila,  CC, Densidade, Profundidade,PMP, Silte)
# res2 <- rcorr(as.matrix(dados), type = "spearman")
# # res2$r
# corrplot(res2$r, type="upper", order="hclust", 
#          p.mat = res2$P, sig.level = 0.05, insig = "blank")
# # correlações insignificantes (<0.05) ficam com um X 
# corrplot(res2$r, type="upper", order="hclust", 
#          p.mat = res2$P, sig.level = 0.05)

cor <- cor(banco1, method = "spearman")
cor_melt <- melt(round(cor, digits = 2))
ggplot(data = cor_melt, aes(x = Var1, y = Var2, fill=value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#003366", high = "#A11D21", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlação") +
  geom_text(aes(Var2, Var1, label = value), color = 'black', size = 1.8) +
  theme_minimal()+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=7),
        axis.text.y = element_text(size=7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_fixed()
#ggsave(filename = file.path(caminho,"correlograma.png"), width = 158, height = 93, units = "mm") 
#ggsave(filename = file.path(caminho,"correlograma.pdf"), width = 158, height = 93, units = "mm")

' O argumento order = "hclust" indica um agrupamento hierárquico para determinar a ordem das variáveis, ou seja,
as variáveis que têm correlações mais semelhantes entre si serão agrupadas e exibidas próximas umas das outras'

####################################################################################################################

############### divisão ###############

banco1<-cbind(banco1,banco$dominios_n, banco$MESO)
colnames(banco1)[11:12] <- c("Domínios", "Mesorregião")
banco1<-banco1[,-c(13:14)]

set.seed(123)  
indices <- sample(1:nrow(banco1), nrow(banco1) * 0.7)  # 70% para treinamento

# Crie conjuntos de treinamento e validação com base nos índices
treinamento <- banco1[indices, ]
validacao <- banco1[-indices, ]

############### variáveis qualitativas ###############

#table(banco$Municipio)
table(banco$regiao)
table(banco$MESO) # 7
table(banco$dominios_n) # 10

treinamento$Domínios <- factor(treinamento$Domínios)
treinamento$Mesorregião <- factor(treinamento$Mesorregião)
treinamento$Região <- factor(treinamento$Região)

# int1<-lm(data=treinamento, CAD ~ Mesorregião*Altitude)
# summary(int1)

############### modelo inicial ###############

reg1 <- lm(data = treinamento, CAD ~  Mesorregião + Domínios + CC  + Altitude + Profundidade  + Silte + Argila +`Areia Grossa` + `Areia Fina` + Densidade)

summary(reg1) 

' Adjusted R-squared:  0.9968 

variáveis significantes: CC, PMP, DomíniosPlanalto da Ibiapaba, Profundidade, considerando alpha=0,05
Manter as demais variáveis não agrega valor ao modelo.
aparecem 8 domínios (chapado do apodi e planície litorânea (1 observação)) e 6 mesorregiões(centro-sul cearaense)'

plot(reg1$fitted.values,treinamento$CAD)

' º consideranto todas as variáveis
  º uma outlier visível, reta perfeita'

############# Observações influentes #############

## observações com valor-p menor que 5% são suspeitas
outlierTest(reg1) # obs 843

medinflu1<-influence.measures(reg1)
indice<-c(1:846)

# Utilizando hii
plot(indice,hatvalues(reg1),type="l")


# Betas
dfbetaPlots(reg1)
ols_plot_dfbetas(reg1)


# Dffits
plot(indice,abs(dffits(reg1)), type = "l")


# Cook
plot(indice,cooks.distance(reg1), type = "l")
plot(reg1,which=4)
ols_plot_cooksd_chart(reg1)
' 543 '

' 389, 106 e 843 (principalmente)'

# treinamento<-treinamento[-c(),]

####################################################################################################################

############### diagnóstico inicial ###############

resíduos = reg1$residuals

# histograma dos resíduos
hist(resíduos)

# normalidade 

qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)
# avaliação da aderência da premissa de normalidade
qqPlot(reg1, main="QQ Plot")  


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
p-value =0.1094 > 0,05'

# multicolinearidade

(vi<-vif(reg1))
mean(vi)

' média do VIF muito superior a 1 (1379.761)'

reduced_data <- subset(banco1, select = -CAD)
corr_matrix = round(cor(reduced_data), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

' Podemos notar uma correlação forte  (valor é superior a 0,8) entre PMP e CC 
   Esse resultado faz sentido? se uma coisa for derivada da outra é possível tirar uma delas '

####################################################################################################################

############### eliminar a multicolinearidade ############### 

' a) Retirada das variáveis multicolineares do modelo
  b) Análise de componentes principais para criação de fatores ortogonais não correlacionados'

############### criar um novo modelo retirando PMP ou CC ###############

reg2 <- lm(data = treinamento, CAD ~  Mesorregião + CC + Altitude + Profundidade  + Silte + Argila + `Areia Grossa` + `Areia Fina` + Densidade)

summary(reg2)

'Adjusted R-squared:  0.9125
 variáveis significantes: CC, MesorregiãoSul Cearense, MesorregiãoNorte Cearense, 
 MesorregiãoNoroeste Cearense, MesorregiãoJaguaribe, DomíniosSerras Secas 
Manter as demais variáveis não agrega valor ao modelo.'

plot(reg2$fitted.values,treinamento$CAD)

resíduos = reg2$residuals

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

plot(reg2$fitted.values, reg2$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

' num modelo de regressão linear ideal, os pontos devem estar 
dispersos aleatoriamente em torno da linha horizontal em y = 0, pontos distantes indicam outliers'

# independencia

plot(reg2$residuals)

' media em torno de zero '

# correlação serial 

dwtest(reg2)

' HO) DW=2
  H1) DW diferente de 2'

# Homocedasticidade (Breusch-Pagan)

bptest(reg2)

' HO) variancias iguais
  H1) há pelo menos uma diferente'

# multicolinearidade

(vi<-vif(reg2))
mean(vi)

reduced_data <- subset(treinamento, select = -c(CAD,PMP))
corr_matrix = round(cor(reduced_data), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

############### transformação ###############

reg2_1 <- lm(data = treinamento, log(CAD) ~  CC + Altitude + Profundidade  + Silte + Argila +`Areia Grossa` + `Areia Fina` + Densidade)

summary(reg2_1)

plot(reg2_1$fitted.values,treinamento$CAD)

resíduos = reg2_1$residuals
hist(resíduos)
qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

plot(reg2_1$fitted.values, reg2_1$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

plot(reg2_1$residuals)

dwtest(reg2_1)

bptest(reg2_1)

(vi<-vif(reg2_1))
mean(vi)

' a homocedasticidade nao foi atendida e os residuos formam como se fosse uma parabola '

############ 

reg2_2 <- lm(data = treinamento, CAD*(-1) ~  CC + Altitude + Profundidade  + Silte + Argila +`Areia Grossa` + `Areia Fina` + Densidade)

summary(reg2_2)

plot(reg2_2$fitted.values,treinamento$CAD)

resíduos = reg2_2$residuals
hist(resíduos)
qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

plot(reg2_2$fitted.values, reg2_2$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

plot(reg2_2$residuals)

dwtest(reg2_2)

bptest(reg2_2)

(vi<-vif(reg2_2))
mean(vi)

' a homocedasticidade vira um problema '

############ 

reg2_3 <- lm(data = treinamento, sqrt(CAD) ~  CC + Altitude + Profundidade  + Silte + Argila +`Areia Grossa` + `Areia Fina` + Densidade)

summary(reg2_3)

plot(reg2_3$fitted.values,treinamento$CAD)

resíduos = reg2_3$residuals
hist(resíduos)
qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

plot(reg2_3$fitted.values, reg2_3$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

plot(reg2_3$residuals)

dwtest(reg2_3)

bptest(reg2_3)

' não é homocedástico '

(vi<-vif(reg2_3))
mean(vi)

' nenhuma transformação resolve o problema da normalidade, a terceira por kolmogorov aceita por bem pouco e
a distribuição lembra mais'

############### observações influentes ###############

medinflu1<-influence.measures(reg2)
indice<-c(1:846)

# Utilizando hii
plot(indice,hatvalues(reg2),type="l")

# Betas
dfbetaPlots(reg2)
ols_plot_dfbetas(reg2)

# Dffits
plot(indice,abs(dffits(reg2)), type = "l")

# Cook
plot(indice,cooks.distance(reg2), type = "l")
plot(reg2,which=4)
ols_plot_cooksd_chart(reg2)

' aumentou o número de observações influentes, mas a principal continua sendo 632'

############# seleção dos modelos (desconsiderando apenas o PMP) #############

dados <- treinamento %>%
  select(CAD, CC, Altitude, Profundidade,Silte, Argila,`Areia Grossa`, `Areia Fina`, Densidade)

sele <- regsubsets(CAD~.,data=dados,nbest = 5)
summary(sele)

cbind(summary(sele)$which, summary(sele)$rsq,summary(sele)$adjr2,summary(sele)$cp,summary(sele)$bic)
parametros <- as.numeric(rownames(summary(sele)$which))+1

plot(parametros,summary(sele)$cp, pch = 16)
plot(parametros,summary(sele)$rsq, pch = 16)
plot(parametros,summary(sele)$adjr2, pch = 16)
plot(parametros,summary(sele)$bic, pch = 16)

k <- ols_step_all_possible(reg2)
plot(k)

' pelo R^2 ajustado o melhor modelo é o 26 com 0.90757, assim como pelo cp 5.85
 considera todos menos as areias '

# seleção automatica

modmin<-lm(CAD ~ 1, data=dados)
step(modmin, direction='forward', scope=( ~ CC + Altitude + Profundidade  + Silte + Argila +`Areia Grossa` + `Areia Fina` + Densidade))

modmax<-lm(CAD ~ CC + Altitude + Profundidade  + Silte + Argila +`Areia Grossa` + `Areia Fina` + Densidade, data=dados)
step(modmax, direction = 'backward')
step(modmin, direction='both', scope=( ~ CC + Altitude + Profundidade  + Silte + Argila +`Areia Grossa` + `Areia Fina` + Densidade))

' forward e stepwise consideram a mesma coisa do modelo 26'

####################################################################################################################

############### modelo escolhido ############### 

modelo<- lm(data = treinamento, CAD ~  CC + Altitude + Profundidade  + Silte + Argila + Densidade)

summary(modelo)

plot(modelo$fitted.values,treinamento$CAD)

resíduos = modelo$residuals
hist(resíduos)
qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

plot(modelo$residuals)

dwtest(modelo)

bptest(modelo)

' deu ruim com a homocedasticidade '

(vi<-vif(modelo))
mean(vi) 

' media menor que 10 não tem multicoluinearidade '

####################################################################################################################

# ' comparando os modelos:
#  H0 ) a variável removida não têm significância
#  H1 ) a variável é significante' 
# 
# anova(reg1,modelo)
# 
# ' para ambas as retiradas, o valor p é muito pequeno (menor que 0,05), portanto rejeitamos a hipótese nula, 
#   significando que o segundo modelo não é uma melhoria do primeiro. '

####################################################################################################################

############### agrupamento de variaveis por PCA ###############

' agrupamento das variáveis multicolineares, por meio de técnicas de redução, como Análise de Componentes Principais '

dados<-select(treinamento, CC, PMP)
padronizados <- scale(dados)
pca <- prcomp(padronizados)
# componentes principais: pca$x
# pca$sdev^2 / sum(pca$sdev^2)
summary(pca)
PCA<- pca$x[, 1]

'0.988 0.012
uma componente é suficiente'

treinamento$PCA <- PCA

'a primeira componente principal foi extraída e representa uma combinação linear de CC e PMP
que captura a maior parte da variância dos dados, usada como uma nova variável não correlacionada'

reg3 <- lm(data = treinamento, CAD ~  Altitude + Profundidade  + Silte + Argila +`Areia Grossa` + `Areia Fina` + Densidade + PCA)

summary(reg3)

plot(reg3$fitted.values,treinamento$CAD)

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
p-value < 0,05

A existência de heterocedasticidade não causa viés nos estimadores,
embora ocasione viés nos estimadores da vairância do MQO, tornando
não válidos os testes F e t.

importante: fazer um estimador da variância robusto a heterocedasticidade será
consistente, mas é viesado'

# multicolinearidade

(vi<-vif(reg3))
mean(vi)

reduced_data <- subset(treinamento, select = -c(CAD, CC,PMP))
corr_matrix = round(cor(reduced_data), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

########## transformações ##########

reg3_1 <- lm(data = treinamento, log(CAD) ~  Altitude + Profundidade  + Silte + Argila +`Areia Grossa` + `Areia Fina` + Densidade + PCA)

summary(reg3_1)

plot(reg3_1$fitted.values,treinamento$CAD)

resíduos = reg3_1$residuals
hist(resíduos)
qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

plot(reg3_1$fitted.values, reg3_1$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

plot(reg3_1$residuals)

dwtest(reg3_1)

bptest(reg3_1)

(vi<-vif(reg3_1))
mean(vi)

##########

reg3_2 <- lm(data = treinamento, CAD*(-1) ~  Altitude + Profundidade  + Silte + Argila +`Areia Grossa` + `Areia Fina` + Densidade + PCA)

summary(reg3_2)

plot(reg3_2$fitted.values,treinamento$CAD)

resíduos = reg3_2$residuals
hist(resíduos)
qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

plot(reg3_2$fitted.values, reg3_2$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

plot(reg3_2$residuals)

dwtest(reg3_2)

bptest(reg3_2)

(vi<-vif(reg3_2))
mean(vi)

##########

reg3_3 <- lm(data = treinamento, sqrt(CAD) ~  Altitude + Profundidade  + Silte + Argila +`Areia Grossa` + `Areia Fina` + Densidade + PCA)

summary(reg3_3)

plot(reg3_3$fitted.values,treinamento$CAD)

resíduos = reg3_3$residuals
hist(resíduos)
qqnorm(resíduos)
qqline(resíduos)
shapiro.test(resíduos)
ols_test_normality(resíduos)

plot(reg3_3$fitted.values, reg3_3$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red")  # linha horizontal em y = 0 para auxiliar na visualização

plot(reg3_3$residuals)

dwtest(reg3_3)

bptest(reg3_3)

(vi<-vif(reg3_3))
mean(vi)

####################################################################################################################

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


