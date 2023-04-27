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
df_3 <- read.csv2('pre_processamento/dataset_fracionado_2020_0003.csv', dec='.')
str(df_3)
head(df_3$carteira_inadimplida_arrastada)
df_3$carteira_inadimplida_arrastada <- as.numeric(gsub(",", ".", df_3$carteira_inadimplida_arrastada))
head(df_3$carteira_inadimplida_arrastada)
df_3 %>%
  mutate(carteira_inadimplida_factor = ifelse(carteira_inadimplida_arrastada > 0,
                                              yes = "sim",
                                              no = "nao"),
         carteira_inadimplida_factor = factor(carteira_inadimplida_factor)) -> df_3

df_3 <- as.data.frame(unclass(df_3), stringsAsFactors = TRUE)
df_3 = subset(df_3, select=-c(
  CODE_INDICE_CONDICOES_ECONOMICAS_ATUAIS,
  CODE_IPCA_INDICE_GERAL,
  CODE_IPCA_TAXA_VARIACAO,
  CODE_PIB,
  CODE_TAXAS_DESEMPREGO,
  a_vencer_ate_90_dias,
  a_vencer_de_91_ate_360_dias,
  a_vencer_de_361_ate_1080_dias,
  a_vencer_de_1081_ate_1800_dias,
  a_vencer_de_1801_ate_5400_dias,
  vencido_acima_de_15_dias,
  a_vencer_acima_de_5400_dias,
  carteira_inadimplida_arrastada
  ))

head(df_3$carteira_ativa)
df_3$carteira_ativa <- as.numeric(gsub(",", ".", df_3$carteira_ativa))
head(df_3$carteira_ativa)
head(df_3$ativo_problematico)
df_3$ativo_problematico <- as.numeric(gsub(",", ".", df_3$ativo_problematico))
head(df_3$ativo_problematico)
head(df_3$numero_de_operacoes)
df_3$numero_de_operacoes <- as.numeric(gsub("<= 15", "8", df_3$numero_de_operacoes))
head(df_3$numero_de_operacoes)
str(df_3)

#Visualização da base de dados
df_3 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Estatísticas descritivas
summary(df_3)

#Estudo sobre o desbalanceamento dos dados
df_3 %>% 
  group_by(uf) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F, 
                font_size = 25)

df_counts <- df_3 %>%
  group_by(carteira_inadimplida_factor) %>%
  summarize(n = n())

ggplot(df_counts,
       aes(x = carteira_inadimplida_factor,
           y = n,
           fill = carteira_inadimplida_factor)) +
  geom_col() 

df_uf_counts <- df_3 %>%
  group_by(uf, carteira_inadimplida_factor) %>%
  summarize(n = n())

ggplot(df_uf_counts,
       aes(x = n,
           y = uf,
           fill = carteira_inadimplida_factor)) +
  geom_col() 

# Modelos nulo

modelo_nulo_3_multinivel <- glmmTMB(formula = carteira_inadimplida_factor ~ 1 + (1 | uf),
                                            data = df_3,
                                            family = binomial,
                                            REML = TRUE)
summary(modelo_nulo_3_multinivel) # agrupamento uf

# Modelo logístico nulo
modelo_nulo_3_logistico_binario <- glm(
  formula = carteira_inadimplida_factor ~ 1,
  data = df_3,
  family = "binomial"
)

summary(modelo_nulo_3_logistico_binario)
logLik(modelo_nulo_3_logistico_binario)
logLik(modelo_nulo_3_multinivel)

# Modelo com interceptos aleatórios
#Curvas Sigmóides para toda a base de dados

modelo_multinivel_3_interceptos_1 <- glmmTMB(formula = carteira_inadimplida_factor ~ indexador + modalidade
                                             + (1 | uf),
                                             data = df_3,
                                             family = binomial,
                                             REML = TRUE)

logLik(modelo_multinivel_3_interceptos_1)

modelo_multinivel_3_interceptos_2 <- glmmTMB(formula = carteira_inadimplida_factor ~ PIB+IPCA_INDICE_GERAL+indexador + modalidade+numero_de_operacoes
                                            + (1 | uf),
                                            data = df_3,
                                            family = binomial,
                                            REML = TRUE)

logLik(modelo_multinivel_3_interceptos_2)

modelo_multinivel_3_interceptos_3 <- glmmTMB(formula = carteira_inadimplida_factor ~ ocupacao + IPCA_INDICE_GERAL + TAXAS_DESEMPREGO + INDICE_CONDICOES_ECONOMICAS_ATUAIS+ IPCA_TAXA_VARIACAO + PIB + cnae_secao + porte + indexador + modalidade
                                            + (1 | uf),
                                            data = df_3,
                                            family = binomial,
                                            REML = TRUE)

logLik(modelo_multinivel_3_interceptos_3)


modelo_multinivel_3_interceptos_4 <- glmmTMB(formula = carteira_inadimplida_factor ~ IPCA_INDICE_GERAL + indexador + modalidade
                                            + (1 | uf),
                                            data = df_3,
                                            family = binomial,
                                            REML = TRUE)
logLik(modelo_multinivel_3_interceptos_4)


modelo_logistico_3 <- glm(
  formula = carteira_inadimplida_factor ~ cnae_secao + indexador + modalidade + porte + 
    ocupacao + uf + TAXAS_DESEMPREGO + PIB + IPCA_TAXA_VARIACAO + IPCA_INDICE_GERAL +
    INDICE_CONDICOES_ECONOMICAS_ATUAIS,
  data = df_3,
  family = "binomial"
)
logLik(modelo_logistico_3)

#Procedimento Stepwise
step_modelo_logistico_3 <- step(object = modelo_logistico_3,
                             k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

logLik(step_modelo_logistico_3)

cutoff <- .5
confusionMatrix(table(predict(modelo_nulo_multinivel_1_nivel_2,
                              type = "response") >= cutoff,
                      df_3$carteira_inadimplida_factor == "sim")[2:1, 2:1])
confusionMatrix(table(predict(modelo_nulo_multinivel_2_nivel_2,
                              type = "response") >= cutoff,
                      df_3$carteira_inadimplida_factor == "sim")[2:1, 2:1])

confusionMatrix(table(predict(step_atrasos_90_dias,
                              type = "response") >= cutoff,
                      df_3$carteira_inadimplida_factor == "sim")[2:1, 2:1])

#Curvas ROC

#GLMM
roc_modelo_nulo_multinivel_1_nivel_2 <- roc(response = df_3$carteira_inadimplida_factor,
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

roc_step_atrasos_90_dias <- roc(response = df_3$carteira_inadimplida_factor,
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
