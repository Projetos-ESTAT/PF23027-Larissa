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

# PF23027-larissa-neris # 

# Diretório de trabalho

setwd("C:/Users/daviz/Downloads/ESTAT/Larissa-neris")

# Leitura de dados

pacman::p_load(readxl,tidyverse,dplyr,tidyr,plotly,readxl,ggplot2,factoextra,ggrepel,cluster)

# Tratamento de dados

dados =  read_excel(file.choose())
dados = dados[c(2,3,5:17,22:24)]

dados_quant = dados[3:14]
# Retirar PMP CC

dados_quant = dados_quant[c(1:5,8:12)]

# Padronização ESTAT


cores_estat <- c(
  "#A11D21", "#003366", "#CC9900", "#663333", "#FF6600",
  "#CC9966", "#999966", "#006606", "#008091", "#041835",
  "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 10),
      axis.title.x = ggplot2::element_text(colour = "black", size = 10),
      axis.text = ggplot2::element_text(colour = "black", size = 10),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      text = element_text(family = "sans", size = 12),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}

# Visualização banco
# Exploratoria

dim(dados)
names(dados)
summary(dados)


apply(dados_quant, 2, summary)
names(dados_quant)

# Boxplot


# dados originais 

plot_ly(y = dados_quant$ALTITUDE, name = "ALTITUDE", type = "box") %>%
  add_trace(y = dados_quant$DECLIVID, name = "DECLIVIDADE")%>%
  add_trace(y = dados_quant$DRENAGEM, name = "DRENAGEM")%>%
  add_trace(y = dados_quant$PROFUND, name = "PROFUNDIDADE")%>%
  add_trace(y = dados_quant$DENSIDADE, name = "DENSIDADE")%>%
  add_trace(y = dados_quant$CAD, name = "CAD")%>%
  add_trace(y = dados_quant$AREIA_GROS, name = "AREIA_GROS")%>%
  add_trace(y = dados_quant$AREIA_FINA, name = "AREIA_FINA")%>%
  add_trace(y = dados_quant$SILTE, name = "SILTE")%>%
  add_trace(y = dados_quant$ARGILA, name = "ARGILA")

# dados padronizados

dados_quant_padron <- scale(dados_quant)
class(dados_quant_padron)
dados_quant_padron = as.data.frame(dados_quant_padron)
class(dados_quant_padron)


plot_ly(y = dados_quant_padron$ALTITUDE, name = "ALTITUDE", type = "box") %>%
  add_trace(y = dados_quant_padron$DECLIVID, name = "DECLIVIDADE")%>%
  add_trace(y = dados_quant_padron$DRENAGEM, name = "DRENAGEM")%>%
  add_trace(y = dados_quant_padron$PROFUND, name = "PROFUNDIDADE")%>%
  add_trace(y = dados_quant_padron$DENSIDADE, name = "DENSIDADE")%>%
  add_trace(y = dados_quant_padron$CC, name = "CC")%>%
  add_trace(y = dados_quant_padron$PMP, name = "PMP")%>%
  add_trace(y = dados_quant_padron$CAD, name = "CAD")%>%
  add_trace(y = dados_quant_padron$AREIA_GROS, name = "AREIA_GROS")%>%
  add_trace(y = dados_quant_padron$AREIA_FINA, name = "AREIA_FINA")%>%
  add_trace(y = dados_quant_padron$SILTE, name = "SILTE")%>%
  add_trace(y = dados_quant_padron$ARGILA, name = "ARGILA")

# Multivariada

# Número ótimo de clusters
# Silhueta  

# Clusterização
# Num ótimo clusters

fviz_nbclust(dados_quant_padron, kmeans, 
             method = "wss", # soma de quadrados totais
             k.max = 10, # máximo de clusters
             nboot = 1000, # Máximo de bootstraps
             barfill = "#A11D21",
             barcolor = "#A11D21",
             linecolor = "#A11D21") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(title= "Número ótimo de clusters") + 
  xlab("Número de clusters") +
  ylab("Soma dos quadrados total") +
  theme_estat()
ggsave("elbow.pdf", width = 158, height = 93, units = "mm")

# Indicativo de 4 clusters

set.seed(2020178)
km.res <- kmeans(dados_quant_padron, 4, nstart=25)

aggregate(dados_quant_padron, by=list(cluster=km.res$cluster), mean)
matrix2 <- cbind(dados_quant_padron, cluster=km.res$cluster)


fviz_cluster(km.res, data=matrix2,
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             main = "Clusters",
             xlab = NULL, ylab = NULL,
             ggtheme=theme_estat(),
             geom = "point" )
ggsave("clust.pdf", width = 158, height = 93, units = "mm")

