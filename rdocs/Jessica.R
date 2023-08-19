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
library("tidyverse")
library("dplyr")
library("openxlsx")
library("nortest")
library("FSA")

cores_estat <- c('#A11D21','#003366', '#CC9900', '#663333','#FF6600','#CC9966','#999966','#006606','#008091', '#041835','#666666')

theme_estat <- function (...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2:: element_text(colour = "black",
                                            size = 12),
      axis.title.x = ggplot2:: element_text(colour = "black",
                                            size = 12),
      axis.text = ggplot2:: element_text(colour = "black", size
                                         = 9.5),
      panel.border = ggplot2:: element_blank() ,
      axis.line = ggplot2:: element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return (
    list(
      theme,
      scale_fill_manual(values = cores_estat ),
      scale_colour_manual(values = cores_estat )
    )
  )
}

## Carregando o banco:

banco <- read.xlsx("banco/perfis_cad_analiseestatistica_19_07.xlsx")

############ Análises:

## Testando normalidade:

lillie.test(banco$CAD)

shapiro.test(banco$CAD)

####### Coeficiente de Determinação #######

#CLASSE

summary(lm(CAD ~ CLASSE, data = banco))$r.squared

banco %>% 
  ggplot(aes(x = as.factor(CLASSE), y = CAD)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Classe", y = "CAD") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_estat()
ggsave("box_classe.pdf", width = 158, height = 93, units = "mm")

#SUBCLASSE

summary(lm(CAD ~ SUBCLASSE, data = banco))$r.squared

banco %>%   ###ARRUMAR GRAFICO
  ggplot(aes(x = as.factor(SUBCLASSE), y = CAD)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Subclasse", y = "CAD") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_estat()
ggsave("box_subclasse.pdf", width = 158, height = 93, units = "mm")

kruskal.test(CAD ~ SUBCLASSE, data = banco) #Teste de comparação de médias

dunnTest(CAD ~ as.factor(SUBCLASSE), data = banco)$res %>%  #Post Hoc:
  filter(P.adj < 0.05)

#MUNICÍPIO

summary(lm(CAD ~ Municipio, data = banco))$r.squared

banco %>%   ###ARRUMAR GRAFICO
  ggplot(aes(x = as.factor(Municipio), y = CAD)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Municipio", y = "CAD") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_estat()
ggsave("box_municipio.pdf", width = 158, height = 93, units = "mm")


kruskal.test(CAD ~ Municipio, data = banco) #Teste de comparação de médias

dunnTest(CAD ~ as.factor(Municipio), data = banco)$res %>%  #Post Hoc:
  filter(P.adj < 0.05)

# REGIAO
kruskal.test(CAD ~ regiao, data = banco) #Teste de comparação de médias

dunnTest(CAD ~ as.factor(regiao), data = banco)$res %>%  #Post Hoc:
  filter(P.adj < 0.05)
