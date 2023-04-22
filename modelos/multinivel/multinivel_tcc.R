################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl","car",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools","glmmTMB")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Carregamento da base de dados
df <- read.csv2('pre_processamento/df_teste_inadimplidos_combinados.csv', dec='.')
df %>%
  mutate(inadimplido_acima_90_dias_factor = ifelse(inadimplido_acima_90_dias > 0,
                                                   yes = "sim",
                                                   no = "nao"),
         inadimplido_acima_90_dias_factor = factor(inadimplido_acima_90_dias_factor)) -> df
df <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
#Visualização da base de dados
df %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Estatísticas descritivas
summary(df)

#Estudo sobre o desbalanceamento dos dados
df %>% 
  group_by(uf) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F, 
                font_size = 25)

df_counts <- df %>%
  group_by(inadimplido_acima_90_dias_factor) %>%
  summarize(n = n())

ggplot(df_counts,
       aes(x = inadimplido_acima_90_dias_factor,
           y = n,
           fill = inadimplido_acima_90_dias_factor)) +
  geom_col() 

df_uf_counts <- df %>%
  group_by(uf, inadimplido_acima_90_dias_factor) %>%
  summarize(n = n())

ggplot(df_uf_counts,
       aes(x = n,
           y = uf,
           fill = inadimplido_acima_90_dias_factor)) +
  geom_col() 

# Modelos nulo

modelo_nulo_multinivel_1_nivel_1 <- glmmTMB(formula = inadimplido_acima_90_dias_factor ~ 1 + (1 | uf),
                                            data = df,
                                            family = binomial,
                                            REML = TRUE)
summary(modelo_nulo_multinivel_1_nivel_1) # agrupamento uf

# Modelo logístico nulo
modelo_nulo_logistico_1 <- glm(
  formula = inadimplido_acima_90_dias_factor ~ 1,
  data = df,
  family = "binomial"
)

summary(modelo_nulo_logistico_1)
logLik(modelo_nulo_multinivel_1_nivel_1)
logLik(modelo_nulo_logistico_1)

# Modelo com interceptos aleatórios
#Curvas Sigmóides para toda a base de dados

modelo_nulo_multinivel_1_nivel_2 <- glmmTMB(formula = inadimplido_acima_90_dias_factor ~ indexador + modalidade
                                             + (1 | uf),
                                             data = df,
                                             family = binomial,
                                             REML = TRUE)

logLik(modelo_nulo_multinivel_1_nivel_2)

modelo_logistico_2 <- glm(
  formula = inadimplido_acima_90_dias_factor ~ cnae_secao + indexador + modalidade + porte + 
    ocupacao + uf + TAXAS_DESEMPREGO + PIB + IPCA_TAXA_VARIACAO + IPCA_INDICE_GERAL +
    INDICE_CONDICOES_ECONOMICAS_ATUAIS,
  data = df,
  family = "binomial"
)
logLik(modelo_logistico_2)

#Procedimento Stepwise
step_atrasos_90_dias <- step(object = modelo_atrasos_90_dias,
                             k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

logLik(step_atrasos_90_dias)

cutoff <- .5
confusionMatrix(table(predict(modelo_nulo_multinivel_1_nivel_2,
                              type = "response") >= cutoff,
                      df$inadimplido_acima_90_dias_factor == "sim")[2:1, 2:1])

confusionMatrix(table(predict(step_atrasos_90_dias,
                              type = "response") >= cutoff,
                      df$inadimplido_acima_90_dias_factor == "sim")[2:1, 2:1])

#Curvas ROC

#GLMM
roc_modelo_nulo_multinivel_1_nivel_2 <- roc(response = df$inadimplido_acima_90_dias_factor,
                         predictor = predict(object = modelo_nulo_multinivel_1_nivel_2,
                                             type = "response"))

ggplotly(
  ggroc(roc_modelo_nulo_multinivel_1_nivel_2, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(roc_modelo_nulo_multinivel_1_nivel_2$auc, 3), 
                       "|",
                       "Coeficiente de Gini", 
                       round((roc_modelo_nulo_multinivel_1_nivel_2$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

roc_step_atrasos_90_dias <- roc(response = df$inadimplido_acima_90_dias_factor,
                                            predictor = predict(object = step_atrasos_90_dias,
                                                                type = "response"))

ggplotly(
  ggroc(roc_step_atrasos_90_dias, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(roc_step_atrasos_90_dias$auc, 3), 
                       "|",
                       "Coeficiente de Gini", 
                       round((roc_step_atrasos_90_dias$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)
