#######Libraries######

library(tidyverse) #pacote para manipulacao de dados
library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange
library(readxl)
############importação da tabela 11########

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


#######teste inicial de tema######


Nome_genotipo_prod_gord <- FORMATADO[,c(4, 7, 11, 16:19)]

#tirando NA
Nome_genotipo_prod_gord_SNA <- na.omit(Nome_genotipo_prod_gord)
view(Nome_genotipo_prod_gord_SNA)

#checando os NG
table(Nome_genotipo_prod_gord_SNA$`B-LGB`)

#funcao para tirar o NG
subset(Nome_genotipo_prod_gord_SNA, `B-LGB` != "NG" )

#Salvando base nova sem NG e sem NA
df <- subset(Nome_genotipo_prod_gord_SNA, `B-LGB` != "NG" )

#checando
table(df$`B-LGB`)

#tirando as acuracia
df2 <- df[,-c(5,7)]

#para selecionar apenas bois com a acuracia maior que 75%
dfteste <- subset(Nome_genotipo_prod_gord_SNA, `Acc.(%)...19` >= 75)

#separando a tabela de Nome // B-LGB // Prod total // Prod gordura total // %G 
#df_AA <- subset(df2, `B-LGB` == "AA" )
#df_AB <- subset(df2, `B-LGB` == "AB" )
#df_BB <- subset(df2, `B-LGB` == "BB" )


############# dumizacao #########

#boi / genotipo blgb / prod % gord
boi_prod_genotipoblgb <- FORMATADO[,c(4, 7, 18)]

#tira NA
boi_prod_genotipoblgb <- na.omit(boi_prod_genotipoblgb)
view(boi_prod_genotipoblgb)

#tira AB
AABB <- subset(boi_prod_genotipoblgb,`B-LGB` != "AB")
#tira NG
AABBB <- subset(AABB,`B-LGB` != "NG")

#AABBB  São os 165 bois AA ou BB com informações válidas
glimpse(AABBB)

#duplica
AABBB2 <- AABBB

#transforma as observacoes
AABBB2$`B-LGB` <- as.factor(AABBB$`B-LGB` )
AABBB2$`PTA%G` <- as.numeric(AABBB2$`PTA%G`)
#checa
glimpse(AABBB2)

#checa e altera os nomes de colunas
names(AABBB2)
names(AABBB2)[1:3] <- c("Nome", "BLGB", "PorcentGord")

#Dummiza
abdum <- dummy_columns(.data = AABBB2,
                                   select_columns = "BLGB",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T)

#Visualizando a base de dados dummizada
abdum %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

glimpse(abdum)


#________________________ate aqui ok___________________________________________#

modelo_abdum <- lm(PorcentGord ~ . - Nome, abdum)
summary(modelo_abdum)





my_plot3 <- 
  abdum %>%
  mutate(rotulo = paste(Nome, PorcentGord)) %>%
  ggplot(aes(x = as.numeric(BLGB_AA), y = PorcentGord, label = rotulo)) +
  geom_point(color = "#FDE725FF") +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ bs(x, df = 4)) +
  labs(x = "Região",
       y = "Corruption Perception Index") +
  scale_x_discrete(labels = c("1" = "América do Sul", 
                              "2" = "Oceania", 
                              "3" = "Europa", 
                              "4" = "EUA e Canadá", 
                              "5" = "Ásia")) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw()
my_plot3
