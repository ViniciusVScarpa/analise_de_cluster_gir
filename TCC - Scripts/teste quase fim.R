#######Libraries######

library(tidyverse) #pacote para manipulacao de dados
library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange
library(readxl)
library(kableExtra)
library(clValid)
library(ggplot2)


# teste final viu

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
#View(FORMATADO)


#tira NA
clu <- na.omit(FORMATADO)
names(clu)


#separar caracteristicas de interesse

teste_sup <- clu[c(4,11,16,18,20,22,24,26)]



#nome dos animais na coluna 0
teste_sup_zerocol <- teste_sup

clu3 <- teste_sup_zerocol %>% remove_rownames %>% column_to_rownames(var="Nome")
#view(clu3)


#PADRONIZA
clu3.pad <- scale(clu3)
#view(clu3.pad)


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


##############VALIDACAO
testao <- clValid(clu3.pad, 3:5, 
                  clMethods = c("hierarchical","kmeans"),
                  validation="internal", 
                  method = "ward", 
                  metric = "euclidean"
                                         )


summary(testao)
optimalScores(testao)
plot(testao)




#estatisticas descritivas da base clu3
a <- summary(clu3)
a %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)

#scores otimos em tbaela
optimalScores(testao) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)

         
          

#separando os bois dos grupos 1, 2 e 3
gp_1 <- subset(grupos_df, grupos == 1)      
gp_2 <- subset(grupos_df, grupos == 2)      
gp_3 <- subset(grupos_df, grupos == 3)                 
                    
#####    tabela com nome de boi e grupo #####                
#gp_1 %>%
  #kable() %>%
 # kable_styling(bootstrap_options = "striped", 
 #               full_width = T, 
  #              font_size = 16)                  
                    
#gp_2 %>%
  #kable() %>%
 # kable_styling(bootstrap_options = "striped", 
   #             full_width = T, 
  #              font_size = 16)                   
                    
#gp_3 %>%
#  kable() %>%
#  kable_styling(bootstrap_options = "striped", 
 #               full_width = T, 
#                font_size = 16)                  

#####


FINAL <- cbind(clu3, grupos_df$grupos)


gp1_final <- subset(FINAL, `grupos_df$grupos` == 1)

gp2_final <- subset(FINAL, `grupos_df$grupos` == 2)

gp3_final <- subset(FINAL, `grupos_df$grupos` == 3)



summary (gp1_final)
summary (gp2_final)
summary (gp3_final)



summary (gp1_final) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)


summary (gp2_final) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)


summary (gp3_final) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)


abcde <- summary(gp1_final)

summary(gp1_final$`PTA%G`)
summary(gp2_final$`PTA%G`)
summary(gp3_final$`PTA%G`)

DATAO <- cbind(summary(gp1_final$`PTA%G`),summary(gp2_final$`PTA%G`),summary(gp3_final$`PTA%G`))

datao2 <- cbind(summary(gp1_final),summary(gp2_final),summary(gp3_final))

datao2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)
datao2


vinte <- FINAL[1:18,]

names(vinte)[8] <-"grupo"

vinte %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 16)
