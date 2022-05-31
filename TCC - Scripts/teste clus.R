#bibliotecas importantes
library(tidyverse) #pacote para manipulacao de dados
library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange
library(readxl)
library(clValid)
#Importacao da tabela
FORMATADO <- read_excel("FORMATADO.xls", 
                        col_types = c("numeric", "text", "text", 
                                      "text", "text", "text", "text", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"), 
                        skip = 1)
View(FORMATADO)


#tira NA
clu <- na.omit(FORMATADO)
names(clu)

clu <- subset(clu, `B-LGB` != "NG" & `B-LGB`!= "AB" )


#tira acc<75
clu2 <- subset(clu, `acc.(%)` & `Acc.(%)...19`  >=75)

#separando nome ptag e pt%g
clu2 <- clu2[,c(4, 16, 18)]

view(clu2)

#nome dos animais na coluna 0
samp <- clu2

clu3 <- samp %>% remove_rownames %>% column_to_rownames(var="Nome")
view(clu3)


#PADRONIZA
clu3.pad <- scale(clu3)
view(clu3.pad)


#CALCULANDO MATRIZ DE DISTANCIAS
d <- dist(clu3.pad, method = "euclidean")
d



#define o cluster, desenha o dendograma
hc4 <- hclust(d, method = "ward.D" )
plot(hc4, cex = 0.6, hang = -1)

#ve 3 grupos
rect.hclust(hc4, k = 3)

#separa 3 grupos
grupos <- cutree(hc4, k = 3)
table(grupos)

#transforma em df
grupos_df <- data.frame(grupos)


#ARRUMANDO GENOTIPO para encaixar no df
AAA <- na.omit(FORMATADO)
names(AAA)
#tira acc<75
AAA <-  subset(clu, `acc.(%)` & `Acc.(%)...19`  >=75)
# pega nome e BLGB
AAA <- AAA[,c(4,7)]
AAA <- subset(AAA, `B-LGB` != "NG" & `B-LGB`!= "AB")
#AAA <- subset(AAA,`B-LGB` != "NG" )
#checa
view(AAA)


#juntando com a base original
base_final <- cbind(clu3, AAA, grupos_df)


names(base_final)[1:4] <- c("PTAG", "Porcent", "Nome","BLGB")

names(base_final)


view(base_final)

#ate aqui ok

table(base_final$BLGB)
table(base_final$grupos)
as.numeric(base_final$BLGB)










#substituir o BLGB por 0(AA) e 1(BB)
base_final$BLGB[base_final$BLGB == "AA"] <- "0" # substitutindo a letra B por A
base_final$BLGB[base_final$BLGB == "BB"] <- "1" # substitutindo a letra C por A

base_final$grupos 

testegraph <- base_final[,4:5]





################# n hier###########


#VERIFICANDO ELBOW 
fviz_nbclust(clu3.pad, FUN = hcut, method = "wss")

clu4.kmeans <- kmeans(clu3.pad, centers = 4)

#Visualizar os clusters
fviz_cluster(clu4.kmeans, geom = "point", data = clu3.pad, main = "Cluster K2")

clu4.kmeans$cluster

TESTEBB <- cbind(clu3, clu4.kmeans$cluster)
names(TESTEBB)



#ARRUMANDO GENOTIPO
AAA <- na.omit(FORMATADO)
names(AAA)

#tira acc<75
AAA <-  subset(clu, `acc.(%)` & `Acc.(%)...19`  >=75)
#AAA <- subset(AAA, `acc.(%)`  >=75)
#AAA <- subset(AAA,  `Acc.(%)...19`   >=75)
AAA <- AAA[,c(4,7)]

view(AAA)
AAA <- subset(AAA,`B-LGB` != "AB" )
AAA <- subset(AAA,`B-LGB` != "NG" )

TESTEBB2 <- cbind(TESTEBB,AAA[,2] )
view(TESTEBB2)





#Criar clusters
mcdonalds.k2 <- kmeans(clu3.pad, centers = 2)
mcdonalds.k3 <- kmeans(clu3.pad, centers = 3)
mcdonalds.k4 <- kmeans(clu3.pad, centers = 4)
mcdonalds.k5 <- kmeans(clu3.pad, centers = 5)

#Criar graficos
G1 <- fviz_cluster(mcdonalds.k2, geom = "point", data = clu3.pad) + ggtitle("k = 2")
G2 <- fviz_cluster(mcdonalds.k3, geom = "point",  data = clu3.pad) + ggtitle("k = 3")
G3 <- fviz_cluster(mcdonalds.k4, geom = "point",  data = clu3.pad) + ggtitle("k = 4")
G4 <- fviz_cluster(mcdonalds.k5, geom = "point",  data = clu3.pad) + ggtitle("k = 5")

#Imprimir graficos na mesma tela
grid.arrange(G1, G2, G3, G4, nrow = 2)





