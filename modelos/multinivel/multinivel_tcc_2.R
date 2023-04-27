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
df_2 <- read.csv2('pre_processamento/dataset_fracionado_2020_0002.csv', dec='.')
str(df_2)
head(df_2$carteira_inadimplida_arrastada)
df_2$carteira_inadimplida_arrastada <- as.numeric(gsub(",", ".", df_2$carteira_inadimplida_arrastada))
head(df_2$carteira_inadimplida_arrastada)
df_2 %>%
  mutate(carteira_inadimplida_factor = ifelse(carteira_inadimplida_arrastada > 0,
                                              yes = "sim",
                                              no = "nao"),
         carteira_inadimplida_factor = factor(carteira_inadimplida_factor)) -> df_2

df_2 <- as.data.frame(unclass(df_2), stringsAsFactors = TRUE)
df_2 = subset(df_2, select=-c(
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

head(df_2$carteira_ativa)
df_2$carteira_ativa <- as.numeric(gsub(",", ".", df_2$carteira_ativa))
head(df_2$carteira_ativa)
head(df_2$ativo_problematico)
df_2$ativo_problematico <- as.numeric(gsub(",", ".", df_2$ativo_problematico))
head(df_2$ativo_problematico)
head(df_2$numero_de_operacoes)
df_2$numero_de_operacoes <- as.numeric(gsub("<= 15", "8", df_2$numero_de_operacoes))
head(df_2$numero_de_operacoes)
str(df_2)


#Visualização da base de dados
df_2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Estatísticas descritivas
summary(df_2)

#Estudo sobre o desbalanceamento dos dados
df_2 %>% 
  group_by(uf) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F, 
                font_size = 25)

df_counts <- df_2 %>%
  group_by(carteira_inadimplida_factor) %>%
  summarize(n = n())

ggplot(df_counts,
       aes(x = carteira_inadimplida_factor,
           y = n,
           fill = carteira_inadimplida_factor)) +
  geom_col() 

df_uf_counts <- df_2 %>%
  group_by(uf, carteira_inadimplida_factor) %>%
  summarize(n = n())

ggplot(df_uf_counts,
       aes(x = n,
           y = uf,
           fill = carteira_inadimplida_factor)) +
  geom_col() 

# Modelos nulo

modelo_nulo_2_multinivel <- glmmTMB(formula = carteira_inadimplida_factor ~ 1 + (1 | uf),
                                            data = df_2,
                                            family = binomial,
                                            REML = TRUE)
summary(modelo_nulo_2_multinivel) # agrupamento uf

# Modelo logístico nulo
modelo_nulo_2_logistico <- glm(
  formula = carteira_inadimplida_factor ~ 1,
  data = df_2,
  family = "binomial"
)

summary(modelo_nulo_2_logistico)
logLik(modelo_nulo_2_logistico)
logLik(modelo_nulo_2_multinivel)
lrtest(modelo_nulo_2_logistico, modelo_nulo_2_multinivel)
# Modelo com interceptos aleatórios
#Curvas Sigmóides para toda a base de dados

modelo_multinivel_2_interceptos_1 <- glmmTMB(formula = carteira_inadimplida_factor ~ indexador + modalidade
                                             + (1 | uf),
                                             data = df_2,
                                             family = binomial,
                                             REML = TRUE)

logLik(modelo_multinivel_2_interceptos_1)

modelo_multinivel_2_interceptos_2 <- glmmTMB(formula = carteira_inadimplida_factor ~ PIB+IPCA_INDICE_GERAL+indexador + modalidade+numero_de_operacoes
                                            + (1 | uf),
                                            data = df_2,
                                            family = binomial,
                                            REML = TRUE)
summary(modelo_multinivel_2_interceptos_2)
confint(modelo_multinivel_2_interceptos_2)
logLik(modelo_multinivel_2_interceptos_2)
ranef(modelo_multinivel_2_interceptos_2)[["cond"]][["uf"]] %>% 
  rownames_to_column("uf") %>% 
  rename(v0j = 2) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Para observarmos graficamente o comportamento dos valores de v0j
ranef(modelo_multinivel_2_interceptos_2)[["cond"]][["uf"]] %>% 
  rownames_to_column("uf") %>% 
  rename(v0j = 2) %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(uf) %>% 
  ggplot(aes(label = round(v0j, digits = 3), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(uf), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = uf, y = 0), size = 3.1, color = "black") +
  coord_flip() +
  labs(x = "Unidade Federativa",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("darkorchid","orange")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

#Curvas Sigmóides para toda a base de dados
#df_2 %>% 
#  mutate(fitted_probs_hnlm2 = predict(object = modelo_multinivel_2_interceptos_2, 
#                                      type = "response")) %>% 
#  ggplot(aes(x = filhos, y = fitted_probs_hnlm2)) +
#  geom_smooth(aes(group = uf, color = uf), 
#              method = "lm", formula = y ~ splines::bs(x), se = F) +
#  scale_colour_viridis_d() +
#  facet_wrap(~uf) +
#  labs(y = "Fitted Probs",
#       x = "?") +
#  theme(panel.background = element_rect("white"),
#        panel.border = element_rect(NA),
#        panel.grid = element_line("grey95"),
#        legend.position = "none")

modelo_multinivel_2_interceptos_3 <- glmmTMB(formula = carteira_inadimplida_factor ~ ocupacao + IPCA_INDICE_GERAL + TAXAS_DESEMPREGO + INDICE_CONDICOES_ECONOMICAS_ATUAIS+ IPCA_TAXA_VARIACAO + PIB + cnae_secao + porte + indexador + modalidade
                                            + (1 | uf),
                                            data = df_2,
                                            family = binomial,
                                            REML = TRUE)

logLik(modelo_multinivel_2_interceptos_3)

modelo_multinivel_2_interceptos_4 <- glmmTMB(formula = carteira_inadimplida_factor ~ IPCA_INDICE_GERAL + indexador + modalidade
                                            + (1 | uf),
                                            data = df_2,
                                            family = binomial,
                                            REML = TRUE)
logLik(modelo_multinivel_2_interceptos_4)


modelo_logistico_2 <- glm(
  formula = carteira_inadimplida_factor ~ cnae_secao + indexador + modalidade + porte + 
    ocupacao + uf + TAXAS_DESEMPREGO + PIB + IPCA_TAXA_VARIACAO + IPCA_INDICE_GERAL +
    INDICE_CONDICOES_ECONOMICAS_ATUAIS,
  data = df_2,
  family = "binomial"
)
logLik(modelo_logistico_2)

#Procedimento Stepwise
step_modelo_logistico_2 <- step(object = modelo_logistico_2,
                             k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

logLik(step_modelo_logistico_2)

cutoff <- .25
confusionMatrix(table(predict(modelo_multinivel_2_interceptos_1,
                              type = "response") >= cutoff,
                      df_2$carteira_inadimplida_factor == "sim")[2:1, 2:1])
confusionMatrix(table(predict(modelo_multinivel_2_interceptos_2,
                              type = "response") >= cutoff,
                      df_2$carteira_inadimplida_factor == "sim")[2:1, 2:1])

confusionMatrix(table(predict(step_modelo_logistico_2,
                              type = "response") >= cutoff,
                      df_2$carteira_inadimplida_factor == "sim")[2:1, 2:1])

#Curvas ROC

#GLMM
roc_modelo_multinivel_2_interceptos_2 <- roc(response = df_2$carteira_inadimplida_factor,
                         predictor = predict(object = modelo_multinivel_2_interceptos_2,
                                             type = "response"))

ggplotly(
  ggroc(roc_modelo_multinivel_2_interceptos_2, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(roc_modelo_multinivel_2_interceptos_2$auc, 3), 
                       "|",
                       "Coeficiente de Gini", 
                       round((roc_modelo_multinivel_2_interceptos_2$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

roc_step_modelo_logistico_2 <- roc(response = df_2$carteira_inadimplida_factor,
                                            predictor = predict(object = step_modelo_logistico_2,
                                                                type = "response"))

ggplotly(
  ggroc(roc_step_modelo_logistico_2, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(roc_step_modelo_logistico_2$auc, 3), 
                       "|",
                       "Coeficiente de Gini", 
                       round((roc_step_modelo_logistico_2$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

roc.test(roc_modelo_multinivel_2_interceptos_2, roc_step_modelo_logistico_2)
