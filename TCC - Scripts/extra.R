##### tabela sem class, tirei a classificação porque ja tem #####
sem_class = FORMATADO[,-1]

#separando a sem_class pelo nome
sem_nome = sem_class[,3]

#separando a sem_class pela prod total
ptal = sem_class[,10]


#juntando ambas para fazer uma tabela de nome pela produção total
nomeptal <- cbind(nome, ptal)
view(nomeptal)


#nome + dados
nome_dados = FORMATADO[,11:28]
view(nome_dados)

tudonomedados <- cbind(nome, nome_dados)

####### Nome dos animais em linha, Excluir NA, sessioninfo ######

#colocando o nome dos animais na linha
rownames(tudonomedados) <- tudonomedados[,1]
tudonomedados <- tudonomedados[,-1]

view(tudonomedados)

#excluindo as observações com NA
TND_sem_NA <- na.omit(tudonomedados)
view(TND_sem_NA)


sessionInfo()

#puxando os dados para apagar os NA
aaa = FORMATADO[,4:28]

#excluindo as observações com NA
aaa <- na.omit(aaa)

#criando nova tabela de nome e genotipo SEM os NA
nome_genotipo = aaa[,1:4]


view(nome_genotipo)
#######ver a quantidade de entradas diferentes, table / subset#####

table(nome_genotipo$`B-CN`)
table(nome_genotipo$`K-CN`)
table(nome_genotipo$`B-LGB`)


#SERIA BOM TIRAR ESSES NG, (USA SUBSET() )


Nome_producoes <- FORMATADO[,c(4, 11, 16, 20, 24)]
Nome_producoes

#excluindo as observações com NA
Nome_producoes_sNA <- na.omit(Nome_producoes)
view(Nome_producoes_sNA)

summary(Nome_producoes_sNA)




####TESTEEEEEEEEEEEEEEEE##########
#AABBB  São os 165 bois AA ou BB com informações válidas

dfteste <- subset(df, `Acc.(%)...19` >= 75)

dftested <- dfteste[,c(1,2,4,6)]

glimpse(dftested)

dftested2 <- dftested

dftested2$`B-LGB` <- as.factor(dftested2$`B-LGB` )

dftested2$`PTA%G` <- as.numeric(dftested2$`PTA%G`)
dftested2$`PTAG(kg)` <- as.numeric(dftested2$`PTAG(kg)`)

glimpse(dftested2)

names(dftested2)
names(dftested2)[1:4] <- c("Nome", "BLGB","PTAG", "%Gord")

dftested3 <- dftested2[,-2]

glimpse(dftested3)

testedum <- dummy_columns(.data = dftested3,
                       select_columns = "BLGB",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)
#testar assim, e testar com os nomes de animais em coluna

glimpse(testedum)

#Modelagem com todas as variáveis
testedum_model <- lm(`%Gord` ~ . - Nome, dftested3)

#Parâmetros do modelo_corrupcao_dummies
summary(testedum_model)




















xisde <- FORMATADO[,c(4,7,11)]
glimpse(xisde)


#AABBB  São os 165 bois AA ou BB com informações válidas
glimpse(xisde)

#duplica
xisde2 <- xisde

#transforma as observacoes
xisde2$`B-LGB` <- as.factor(xisde2$`B-LGB` )
xisde2$`PTAL(kg)` <- as.numeric(xisde2$`PTAL(kg)`)
#checa
glimpse(xisde2)

#checa e altera os nomes de colunas
names(xisde2)
names(xisde2)[1:3] <- c("Nome", "BLGB", "PTALKG")

#Dummiza
abxisdum <- dummy_columns(.data = xisde2,
                       select_columns = "BLGB",
                       remove_selected_columns = T,
                       remove_most_frequent_dummy = T)

#Visualizando a base de dados dummizada
abxisdum %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

glimpse(abxisdum)


#________________________ate aqui ok___________________________________________#

modelo_abxisdum <- lm(PTALKG ~ . - Nome, abxisdum)
summary(modelo_abxisdum)

my_plot3 <- 
  abxisdum %>%
  mutate(rotulo = paste(Nome, PTALKG)) %>%
  ggplot(aes(x = as.numeric(BLGB_AA), y = PTALKG, label = rotulo)) +
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




A <- FORMATADO[,c(4,7,16)]
glimpse(A)

B <- subset(A, `B-LGB` != 'AB')

C <- na.omit(B, `B-LGB` )
C <- subset(C, `B-LGB` != 'NG' )
glimpse(C)

C$`B-LGB` <- as.factor(C$`B-LGB` )

C$`PTAG(kg)` <- as.numeric(C$`PTAG(kg)`)

glimpse(C)

names(C)
names(C)[1:3] <- c("Nome", "BLGB","PTAG")


glimpse(C)

CD <- dummy_columns(.data = C,
                          select_columns = "BLGB",
                          remove_selected_columns = T,
                          remove_most_frequent_dummy = T)
glimpse(CD)

#Visualizando a base de dados dummizada
CD %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

CD$BLGB_AA <- as.factor(CD$BLGB_AA)


glimpse(CD)


modelo_CD <- lm(PTAG ~ BLGB_AA, CD)
summary(modelo_CD)


confint(modelo_CD, level = 0.95)











k2 <- kmeans(clu3.pad, centers = 4, nstart = 25)
str(k2)
k2$cluster


#calor
#distance <- get_dist(clu3.pad)
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))




a <- base_final[3:5]
a

`rownames<-`(a,NULL)

Base_final2 <- `rownames<-`(a,NULL)
Base_final2




