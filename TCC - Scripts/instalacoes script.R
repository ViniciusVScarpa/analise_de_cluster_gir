##################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



##################################################################################
#                               ESTUDO DAS CORRELAÇÕES                           #
##################################################################################
#Visualizando a base de dados
AABBB2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

#A função correlation do pacote correlation faz com que seja estruturado um
#diagrama interessante que mostra a inter-relação entre as variáveis e a
#magnitude das correlações entre elas
#Requer instalação e carregamento dos pacotes see e ggraph para a plotagem
abdum %>%
  correlation(method = "pearson") %>%
  plot()


chart.Correlation((abdum[2:3]), histogram = TRUE)






ggplotly(
  ggplot(abdum, aes(x = PorcentagemGord, y = BLGB_AA)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 2) +
    xlab("PorcentagemGord") +
    ylab("BLGB_AA") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)






##################################################################################
#                        ESTIMAÇÃO DO MODELO DE REGRESSÃO                        #
##################################################################################

#Modelagem com todas as variáveis
abdum_modelo <- lm(BLGB_AA ~ . BLGB_AA, abdum)

#Parâmetros do modelo_corrupcao_dummies
summary(abdum_modelo)


#Plotando o modelo_corrupcao_dummies de forma interpolada
my_plot3 <- 
  abdum_modelo %>%
  mutate(abdum = paste(Nome, PorcentagemGord)) %>%
  ggplot(aes(x = as.numeric(BLGB), y = PorcentagemGord, label = abdum)) +
  geom_point(color = "#FDE725FF") +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ bs(x, df = 4)) +
  labs(x = "BLGB_AA",
       y = "PorcentagemGord") +
  scale_x_discrete(labels = c("1" = "AA", 
                              "2" = "BB", 
  )) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw()
my_plot3
