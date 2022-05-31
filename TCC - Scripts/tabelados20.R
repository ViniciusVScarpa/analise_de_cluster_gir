#nome dos animais na coluna 0
teste_sup_zerocol <- teste_sup

clu3 <- teste_sup_zerocol %>% remove_rownames %>% column_to_rownames(var="Nome")
#view(clu3)


final_t <- remove_rownames(FINAL)



vinte <- final_t[0:18,]

names(vinte)[8] <-"grupo"

vinte %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 16)
column_to_rownames(final_t)

vinte["Posição original"]<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18")



####

final_x <-  remove_rownames(FINAL)
names(final_x)[9] <- "1"



###

library(readxl)
X278 <- read_excel("278.xlsx", col_names = FALSE)
View(X278)


#final + 278

dend_fim <- cbind(FINAL, X278)


##TIRA NOME

dend_fim2 <- remove_rownames(dend_fim)

names(dend_fim2)
names(dend_fim2)[8] <- "Grupo"
names(dend_fim2)[9] <- "Rank"
dend_fim2[8]
