source("rdocs/source/packages.R")

p_load(FactoMineR, factoextra, amap, ade4, openxlsx,
       ggrepel, HH, likert, janitor, reshape2,RColorBrewer,
       plyr,psych,lavaan, semPlot, conover.test, ca)


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


dados <- read_excel("banco/banco_clusters.xlsx")
str(dados)
colnames(dados)
boxplot(dados[,3:14])
plot(dados[,3:14])
dados_longos <- melt(round(cor(dados[,3:14]),digits = 2))
ggplot(data = dados_longos, aes(x = Var1, y = Var2, fill=value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#003366", high = "#A11D21", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlação") +
  geom_text(aes(Var2, Var1, label = value), color = 'black', size = 3) +
  theme_minimal()+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=7),
        axis.text.y = element_text(size=7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_fixed()
boxplot(dados$CAD ~ dados$MESO)

# CC = Capacidade de campo #
# PMP = Ponto de murcha permanente #
# CAD = CC - PMP #


intervals <- c(0,50, 100, 200,  Inf)
categories <- c("Muito Baixo","Baixo", "Médio", "Alto")

categoria_capacidade <- cut(dados$CAD, breaks = intervals, 
                            labels = categories, right = FALSE)
colnames(dados)[18] <- 'Domínios'
dados$CAD <- categoria_capacidade

ggplot(dados) +
  aes(
    x = CAD_cat,
    y = CAD
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Níveis de CAD", y = "CAD") +
  theme_estat()
ggsave("box_CAD.pdf", width = 158, height = 93, units = "mm")


a2 <- dados %>% dplyr::select(CAD, regiao)

a2[] <- lapply(a2, factor)
mca1_br1 <- ca::mjca(obj = a2, lambda = "Burt")
summary(mca1_br1)

mca2_br1 <- dudi.acm(a2, scannf = FALSE)
cats_br1 <- apply(a2, 2, function(x) nlevels(as.factor(x)))
mca2_br1_vars_df <- data.frame(mca2_br1$co,
                               Variable = rep(names(cats_br1), cats_br1)
)
dim1 <- str_c("Dimensão 1 (","",round(mca1_br1$inertia.e[1]*100,2),"", "%)")
dim2 <- str_c("Dimensão 2 (","",round(mca1_br1$inertia.e[2]*100,2),"", "%)")

ggplot(data = mca2_br1_vars_df,
       aes(x = Comp1, y = Comp2,
           label = rownames(mca2_br1_vars_df),
           col = Variable,
           size = 4)) +
  geom_hline(yintercept = 0, colour = "#666666") +
  geom_vline(xintercept = 0, colour = "#666666") +
  geom_point(show.legend = F)+
  labs(x = dim1, y =dim2) +
  geom_label_repel(show.legend = F) +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  scale_size(range = 2) +
  theme_estat()
ggsave("resultados/corresp_regiao.png", width = 158, height = 93, units = "mm")


a2 <- dados %>% dplyr::select(CAD, MESO)

a2[] <- lapply(a2, factor)
mca1_br1 <- ca::mjca(obj = a2, lambda = "Burt")
summary(mca1_br1)

mca2_br1 <- dudi.acm(a2, scannf = FALSE)
cats_br1 <- apply(a2, 2, function(x) nlevels(as.factor(x)))
mca2_br1_vars_df <- data.frame(mca2_br1$co,
                               Variable = rep(names(cats_br1), cats_br1)
)
dim1 <- str_c("Dimensão 1 (","",round(mca1_br1$inertia.e[1]*100,2),"", "%)")
dim2 <- str_c("Dimensão 2 (","",round(mca1_br1$inertia.e[2]*100,2),"", "%)")

ggplot(data = mca2_br1_vars_df,
       aes(x = Comp1, y = Comp2,
           label = rownames(mca2_br1_vars_df),
           col = Variable,
           size = 4)) +
  geom_hline(yintercept = 0, colour = "#666666") +
  geom_vline(xintercept = 0, colour = "#666666") +
  geom_point(show.legend = F)+
  labs(x = dim1, y =dim2) +
  geom_label_repel(show.legend = F) +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  scale_size(range = 2) +
  theme_estat()
ggsave("resultados/corresp_meso.png", width = 158, height = 93, units = "mm")


a2 <- dados %>% dplyr::select(CLASSE,CAD)

a2[] <- lapply(a2, factor)
mca1_br1 <- ca::mjca(obj = a2, lambda = "Burt")

mca2_br1 <- dudi.acm(a2, scannf = FALSE)
cats_br1 <- apply(a2, 2, function(x) nlevels(as.factor(x)))
mca2_br1_vars_df <- data.frame(mca2_br1$co,
                               Variable = rep(names(cats_br1), cats_br1)
)
dim1 <- str_c("Dimensão 1 (","",round(mca1_br1$inertia.e[1]*100,2),"", "%)")
dim2 <- str_c("Dimensão 2 (","",round(mca1_br1$inertia.e[2]*100,2),"", "%)")

ggplot(data = mca2_br1_vars_df,
       aes(x = Comp1, y = Comp2,
           label = rownames(mca2_br1_vars_df),
           col = Variable,
           size = 4)) +
  geom_hline(yintercept = 0, colour = "#666666") +
  geom_vline(xintercept = 0, colour = "#666666") +
  geom_point(show.legend = F)+
  labs(x = dim1, y =dim2) +
  geom_label_repel(show.legend = F) +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  scale_size(range = 2) +
  theme_estat()
ggsave("resultados/corresp_classe.png", width = 158, height = 93, units = "mm")


a2 <- dados %>% dplyr::select(CAD, Domínios)

a2[] <- lapply(a2, factor)
mca1_br1 <- ca::mjca(obj = a2, lambda = "Burt")
summary(mca1_br1)

mca2_br1 <- dudi.acm(a2, scannf = FALSE)
cats_br1 <- apply(a2, 2, function(x) nlevels(as.factor(x)))
mca2_br1_vars_df <- data.frame(mca2_br1$co,
                               Variable = rep(names(cats_br1), cats_br1)
)
dim1 <- str_c("Dimensão 1 (","",round(mca1_br1$inertia.e[1]*100,2),"", "%)")
dim2 <- str_c("Dimensão 2 (","",round(mca1_br1$inertia.e[2]*100,2),"", "%)")

ggplot(data = mca2_br1_vars_df,
       aes(x = Comp1, y = Comp2,
           label = rownames(mca2_br1_vars_df),
           col = Variable,
           size = 4)) +
  geom_hline(yintercept = 0, colour = "#666666") +
  geom_vline(xintercept = 0, colour = "#666666") +
  geom_point(show.legend = F)+
  labs(x = dim1, y =dim2) +
  geom_label_repel(show.legend = F) +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  scale_size(range = 2) +
  theme_estat()
ggsave("resultados/corresp_dominios.png", width = 158, height = 93, units = "mm")




