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
df_1 <- read.csv2('pre_processamento/dataset_fracionado_2020_0001.csv', dec='.')
str(df_1)
head(df_1$carteira_inadimplida_arrastada)
df_1$carteira_inadimplida_arrastada <- as.numeric(gsub(",", ".", df_1$carteira_inadimplida_arrastada))
head(df_1$carteira_inadimplida_arrastada)
df_1 %>%
  mutate(carteira_inadimplida_factor = ifelse(carteira_inadimplida_arrastada > 0,
                                              yes = "sim",
                                              no = "nao"),
         carteira_inadimplida_factor = factor(carteira_inadimplida_factor)) -> df_1

df_1 <- as.data.frame(unclass(df_1), stringsAsFactors = TRUE)
df_1 = subset(df_1, select=-c(
  CODE_INDICE_CONDICOES_ECONOMICAS_ATUAIS,
  CODE_IPCA_INDICE_GERAL,
  CODE_IPCA_TAXA_VARIACAO,
  CODE_PIB,
  CODE_TAXAS_DESEMPREGO,
  ativo_problematico,
  a_vencer_ate_90_dias,
  a_vencer_de_91_ate_360_dias,
  a_vencer_de_361_ate_1080_dias,
  a_vencer_de_1081_ate_1800_dias,
  a_vencer_de_1801_ate_5400_dias,
  vencido_acima_de_15_dias,
  a_vencer_acima_de_5400_dias,
  cnae_subclasse,
  carteira_inadimplida_arrastada,
  origem,
  ))

head(df_1$carteira_ativa)
df_1$carteira_ativa <- as.numeric(gsub(",", ".", df_1$carteira_ativa))
head(df_1$carteira_ativa)
head(df_1$ativo_problematico)
df_1$ativo_problematico <- as.numeric(gsub(",", ".", df_1$ativo_problematico))
head(df_1$ativo_problematico)
head(df_1$numero_de_operacoes)
df_1$numero_de_operacoes <- as.numeric(gsub("<= 15", "8", df_1$numero_de_operacoes))
head(df_1$numero_de_operacoes)
str(df_1)

#Visualização da base de dados
df_1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Estatísticas descritivas
summary(df_1)

#Estudo sobre o desbalanceamento dos dados
df_1 %>% 
  group_by(uf) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F, 
                font_size = 25)

df_counts <- df_1 %>%
  group_by(carteira_inadimplida_factor) %>%
  summarize(n = n())

ggplot(df_counts,
       aes(x = carteira_inadimplida_factor,
           y = n,
           fill = carteira_inadimplida_factor)) +
  geom_col() 

df_uf_counts <- df_1 %>%
  group_by(uf, carteira_inadimplida_factor) %>%
  summarize(n = n())

ggplot(df_uf_counts,
       aes(x = n,
           y = uf,
           fill = carteira_inadimplida_factor)) +
  geom_col() 

# Modelos nulo

modelo_1_nulo_multinivel_1_nivel_1 <- glmmTMB(formula = carteira_inadimplida_factor ~ 1 + (1 | uf),
                                            data = df_1,
                                            family = binomial,
                                            REML = TRUE)
summary(modelo_1_nulo_multinivel_1_nivel_1) # agrupamento uf

# Modelo logístico nulo
modelo_nulo_logistico_1 <- glm(
  formula = carteira_inadimplida_factor ~ 1,
  data = df_1,
  family = "binomial"
)

summary(modelo_nulo_logistico_1)
logLik(modelo_1_nulo_multinivel_1_nivel_1)
logLik(modelo_nulo_logistico_1)

# Modelo com interceptos aleatórios
#Curvas Sigmóides para toda a base de dados

modelo_multinivel_1_interceptos_1 <- glmmTMB(formula = carteira_inadimplida_factor ~ indexador + modalidade
                                             + (1 | uf),
                                             data = df_1,
                                             family = binomial,
                                             REML = TRUE)

logLik(modelo_multinivel_1_interceptos_1)
str(df_1)
modelo_multinivel_1_interceptos_2 <- glmmTMB(formula = carteira_inadimplida_factor ~ PIB+IPCA_INDICE_GERAL+indexador + modalidade +numero_de_operacoes
                                             + (1 | uf),
                                             data = df_1,
                                             family = binomial,
                                             REML = TRUE)

logLik(modelo_multinivel_1_interceptos_2)

modelo_multinivel_1_interceptos_3 <- glmmTMB(formula = carteira_inadimplida_factor ~ PIB+IPCA_INDICE_GERAL+indexador + modalidade
                                            + (1 | uf),
                                            data = df_1,
                                            family = binomial,
                                            REML = TRUE)

logLik(modelo_multinivel_1_interceptos_3)
summary(modelo_multinivel_1_interceptos_2)

modelo_multinivel_1_interceptos_5 <- glmmTMB(formula = carteira_inadimplida_factor ~ ocupacao + IPCA_INDICE_GERAL + TAXAS_DESEMPREGO + INDICE_CONDICOES_ECONOMICAS_ATUAIS+ IPCA_TAXA_VARIACAO + PIB + cnae_secao + porte + indexador + modalidade
                                            + (1 | uf),
                                            data = df_1,
                                            family = binomial,
                                            REML = TRUE)

logLik(modelo_multinivel_1_interceptos_5)




modelo_multinivel_1_interceptos_4 <- glmmTMB(formula = carteira_inadimplida_factor ~ IPCA_INDICE_GERAL + indexador + modalidade
                                            + (1 | uf),
                                            data = df_1,
                                            family = binomial,
                                            REML = TRUE)
logLik(modelo_multinivel_1_interceptos_4)


modelo_logistico_1 <- glm(
  formula = carteira_inadimplida_factor ~ cnae_secao + indexador + modalidade + porte + 
    ocupacao + uf + TAXAS_DESEMPREGO + PIB + IPCA_TAXA_VARIACAO + IPCA_INDICE_GERAL +
    INDICE_CONDICOES_ECONOMICAS_ATUAIS,
  data = df_1,
  family = "binomial"
)
logLik(modelo_logistico_1)
summary(modelo_logistico_1)

#Procedimento Stepwise
step_modelo_logistico_1 <- step(object = modelo_logistico_1,
                             k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

logLik(step_modelo_logistico_1)
summary(step_modelo_logistico_1)
str(modelo_multinivel_1_interceptos_2)
str(df_1)
df_dummies <- dummy_columns(.data = df_1,
                                   select_columns = c("uf", "tcb", "sr", "cliente", "ocupacao", "cnae_secao",
                                                      "cnae_subclasse", "porte", "modalidade", "origem", "indexador"),
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T)

modelo_logistico_1_dummies <- glm(
  formula = carteira_inadimplida_factor ~ . - carteira_inadimplida_arrastada,
  data = df_1,
  family = "binomial"
)
logLik(modelo_logistico_1_dummies)
summary(modelo_logistico_1_dummies)

#Procedimento Stepwise
step_modelo_logistico_1_dummies <- step(object = modelo_logistico_1_dummies,
                                k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

logLik(step_modelo_logistico_1_dummies)
summary(step_modelo_logistico_1_dummies)




predicoes_multinivel_1_interceptos <- prediction(predictions = modelo_multinivel_1_interceptos$fit$parfull, 
                           labels = df$carteira_inadimplida_factor)
predicoes_binaria_1_step <- prediction(predictions = modelo_logistico_1$fitted.values, 
                                labels = df$carteira_inadimplida_factor) 
#a função prediction, do pacote ROCR, cria um objeto com os dados necessários
#para a futura plotagem da curva ROC.
dados_curva_roc_multinivel_1_interceptos <- performance(predicoes_90, measure = "sens")
dados_curva_roc_binaria_1_step <- performance(predicoes_binaria_1_step, measure = "sens") 
#Desejamos os dados da sensitividade e de especificidade. Então, devemos
#digitar os seguintes códigos::

sensitividade_multinivel_1_interceptos <- (performance(predicoes_multinivel_1_interceptos, measure = "sens"))@y.values[[1]] 
sensitividade_binaria_1_step <- (performance(predicoes_binaria_1_step, measure = "sens"))@y.values[[1]] 
especificidade_multinivel_1_interceptos <- (performance(predicoes_multinivel_1_interceptos, measure = "spec"))@y.values[[1]]
especificidade_binaria_1_step <- (performance(predicoes_binaria_1_step, measure = "spec"))@y.values[[1]]

#Extraindo os cutoffs:
cutoffs_multinivel_1_interceptos <- dados_curva_roc_multinivel_1_interceptos@x.values[[1]]
cutoffs_binaria_1_step <- dados_curva_roc_binaria_1_step@x.values[[1]] 

#Até o momento, foram extraídos 3 vetores: 'sensitividade', 'especificidade' 
#e 'cutoffs'. Poder-se-ia plotar normalmente a partir daqui com a linguagem 
#base do R, mas demos preferência à ferramenta ggplot2. Assim, criamos um data 
#frame que contém os vetores mencionados.

dados_plotagem_multinivel_1_interceptos <- cbind.data.frame(cutoffs_multinivel_1_interceptos, especificidade_multinivel_1_interceptos, sensitividade_multinivel_1_interceptos)
dados_plotagem_binaria_1_step <- cbind.data.frame(cutoffs_binaria_1_step, especificidade_binaria_1_step, sensitividade_binaria_1_step)

#Visualizando o novo data frame dados_plotagem
dados_plotagem_multinivel_1_interceptos %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)


#Plotando:
ggplotly(dados_plotagem_multinivel_1_interceptos %>%
           ggplot(aes(x = cutoffs_multinivel_1_interceptos, y = especificidade_multinivel_1_interceptos)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs_multinivel_1_interceptos, y = sensitividade_multinivel_1_interceptos, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs_multinivel_1_interceptos, y = sensitividade_multinivel_1_interceptos),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

dados_plotagem_binaria_1_step %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)


#Plotando:
ggplotly(dados_plotagem_binaria_1_step %>%
           ggplot(aes(x = cutoffs_binaria_1_step, y = especificidade_binaria_1_step)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs_binaria_1_step, y = sensitividade_binaria_1_step, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs_binaria_1_step, y = sensitividade_binaria_1_step),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

















































cutoff <- .2
confusionMatrix(table(predict(modelo_multinivel_1_interceptos_2,
                              type = "response") >= cutoff,
                      df_1$carteira_inadimplida_factor == "sim")[2:1, 2:1])
confusionMatrix(table(predict(modelo_nulo_multinivel_2_nivel_2,
                              type = "response") >= cutoff,
                      df_1$carteira_inadimplida_factor == "sim")[2:1, 2:1])

confusionMatrix(table(predict(step_modelo_logistico_1,
                              type = "response") >= cutoff,
                      df_1$carteira_inadimplida_factor == "sim")[2:1, 2:1])
step_modelo_logistico_1$terms
#Curvas ROC

#GLMM
roc_modelo_multinivel_1_interceptos_2 <- roc(response = df_1$carteira_inadimplida_factor,
                         predictor = predict(object = modelo_multinivel_1_interceptos_2,
                                             type = "response"))

ggplotly(
  ggroc(roc_modelo_multinivel_1_interceptos_2, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(roc_modelo_multinivel_1_interceptos_2$auc, 3), 
                       "|",
                       "Coeficiente de Gini", 
                       round((roc_modelo_multinivel_1_interceptos_2$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

roc_modelo_logistico_1 <- roc(response = df_1$carteira_inadimplida_factor,
                                            predictor = predict(object = modelo_logistico_1,
                                                                type = "response"))

ggplotly(
  ggroc(roc_modelo_logistico_1, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(roc_modelo_logistico_1$auc, 3),
                       "|",
                       "Coeficiente de Gini", 
                       round((roc_modelo_logistico_1$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)
