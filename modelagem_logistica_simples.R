##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet", "magick",
             "cowplot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
# ler o dataset
df <- read.csv2('pre_processamento/df_teste_inadimplidos_combinados.csv', dec='.')
# Visualizar a base de dados
df %>% kable() %>%  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 25)
# Estatísticas descritivas univariadas da base de dados
summary(df)

# Tabela de frequências absolutas da variável atrasado
table(df$inadimplido_acima_0_dias_dummizado)
table(df$inadimplido_acima_90_dias_dummizado)
1+544/838
838/544
#Checar categorias das variáveis

str(df)

df <- as.data.frame(unclass(df), stringAsFactors=TRUE)

str(df)
# TODO: - Tentar identificar outliers 
#       - Criar uma variável de diferencial do PIB, pois só o PIB não é tão vantajoso como o seu crescimento ou queda 
#         também

modelo_atrasos_0_dias <- glm(
    formula = inadimplido_acima_0_dias_dummizado ~ cnae_secao + indexador + modalidade + porte + 
      ocupacao + uf + TAXAS_DESEMPREGO + PIB + IPCA_TAXA_VARIACAO + IPCA_INDICE_GERAL +
      INDICE_CONDICOES_ECONOMICAS_ATUAIS,
    data = df,
    family = "binomial"
  )

modelo_atrasos_90_dias <- glm(
  formula = inadimplido_acima_90_dias_dummizado ~ cnae_secao + indexador + modalidade + porte + 
    ocupacao + uf + TAXAS_DESEMPREGO + PIB + IPCA_TAXA_VARIACAO + IPCA_INDICE_GERAL +
    INDICE_CONDICOES_ECONOMICAS_ATUAIS,
  data = df,
  family = "binomial"
)
summary(modelo_atrasos_0_dias)
summary(modelo_atrasos_90_dias)
logLik(modelo_atrasos_0_dias)
logLik(modelo_atrasos_90_dias)

# Capturando as médias das variáveis utilizadas nos modelos
mean(df$INDICE_CONDICOES_ECONOMICAS_ATUAIS)
library(gridExtra)
hist(
     x=df$INDICE_CONDICOES_ECONOMICAS_ATUAIS,
     main="INDICE_CONDICOES_ECONOMICAS_ATUAIS",
     xlab="Valor",
     ylab="Observações"
)
barplot(table(df$uf))
# realizar pelo menos uma predição para cada modelo
