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
# Transformar as variáveis y em factors
df %>%
  mutate(inadimplido_acima_90_dias_factor = ifelse(inadimplido_acima_90_dias > 0,
                                                  yes = "sim",
                                                  no = "nao"),
         inadimplido_acima_90_dias_factor = factor(inadimplido_acima_90_dias_factor)) -> df
df %>% kable() %>%  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 25)
# Estatísticas descritivas univariadas da base de dados
summary(df)

# Tabela de frequências absolutas da variável atrasado
table(df$inadimplido_acima_90_dias_factor)
# Proporção de inadimplentes com apenas 0 dias frente a taxa usual de inadimplentes de 90 dias
print(paste(round((838/544 - 1) * 100, 2), "%"))

#Checar categorias das variáveis

str(df)
df <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
str(df)
# TODO: - Tentar identificar outliers 
#       - Criar uma variável de diferencial do PIB, pois só o PIB não é tão vantajoso como o seu crescimento ou queda 
#         também

modelo_atrasos_90_dias <- glm(
  formula = inadimplido_acima_90_dias_factor ~ cnae_secao + indexador + modalidade + porte + 
    ocupacao + uf + TAXAS_DESEMPREGO + PIB + IPCA_TAXA_VARIACAO + IPCA_INDICE_GERAL +
    INDICE_CONDICOES_ECONOMICAS_ATUAIS,
  data = df,
  family = "binomial"
)

#Procedimento Stepwise
step_atrasos_90_dias <- step(object = modelo_atrasos_90_dias,
                            k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
step_atrasos_90_dias
summary(modelo_atrasos_90_dias)
summary(step_atrasos_90_dias)
logLik(modelo_atrasos_90_dias)
logLik(step_atrasos_90_dias)
summ(model = modelo_atrasos_90_dias, confint = T, digits = 4, ci.width = 0.95)
summ(model = step_atrasos_90_dias, confint = T, digits = 4, ci.width = 0.95)

export_summs(modelo_atrasos_90_dias, scale = F, digits = 4)
export_summs(step_atrasos_90_dias, scale = F, digits = 4)

df_predict <- data.frame(
  cnae_secao=factor("PJ - Atividades administrativas e serviços complementares", levels=c("-", "PJ - Administração pública, defesa e seguridade social","PJ - Agricultura, pecuária, produção florestal, pesca e aqüicultura","PJ - Água, esgoto, atividades de gestão de resíduos e descontaminação","PJ - Alojamento e alimentação","PJ - Artes, cultura, esporte e recreação","PJ - Atividades administrativas e serviços complementares","PJ - Atividades financeiras, de seguros e serviços relacionados","PJ - Atividades imobiliárias","PJ - Atividades profissionais, científicas e técnicas","PJ - Comércio; reparação de veículos automotores e motocicletas","PJ - Construção","PJ - Educação","PJ - Eletricidade e gás","PJ - Indústrias de transformação","PJ - Indústrias extrativas","PJ - Informação e comunicação","PJ - Outras atividades de serviços","PJ - Saúde humana e serviços sociais","PJ - Transporte, armazenagem e correio")),
  indexador=factor("Pós-fixado", levels=c("Flutuantes", "Índices de preços", "Outros indexadores", "Pós-fixado", "Prefixado", "TCR/TRFC")),
  modalidade=factor("PJ - Capital de giro rotativo", levels=c("PF - Cartão de crédito", "PF - Empréstimo com consignação em folha", "PF - Empréstimo sem consignação em folha", "PF - Habitacional", "PF - Outros créditos", "PF - Rural e agroindustrial", "PF - Veículos", "PJ - Capital de giro", "PJ - Capital de giro rotativo", "PJ - Comércio exterior", "PJ - Financiamento de infraestrutura/desenvolvimento/projeto e outros créditos", "PJ - Habitacional", "PJ - Investimento", "PJ - Operações com recebíveis", "PJ - Outros créditos", "PJ - Rural e agroindustrial")),
  porte=factor("PJ - Médio                                   ", levels=c("PF - Acima de 20 salários mínimos            ", "PF - Até 1 salário mínimo                    ", "PF - Indisponível                            ", "PF - Mais de 1 a 2 salários mínimos          ", "PF - Mais de 10 a 20 salários mínimos        ", "PF - Mais de 2 a 3 salários mínimos          ", "PF - Mais de 3 a 5 salários mínimos          ", "PF - Mais de 5 a 10 salários mínimos         ", "PF - Sem rendimento                          ", "PJ - Grande                                  ", "PJ - Indisponível                            ", "PJ - Médio                                   ", "PJ - Micro                                   ", "PJ - Pequeno                                 ")),
  ocupacao=factor("PF - Empregado de empresa privada", levels=c("-", "PF - Aposentado/pensionista", "PF - Autônomo", "PF - Empregado de empresa privada", "PF - Empregado de entidades sem fins lucrativos", "PF - Empresário", "PF - MEI", "PF - Outros", "PF - Servidor ou empregado público")),
  uf=factor("SP", levels=c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")),
  TAXAS_DESEMPREGO=4.4,
  IPCA_TAXA_VARIACAO=0.64,
  IPCA_INDICE_GERAL=5348.49,
  INDICE_CONDICOES_ECONOMICAS_ATUAIS=95.71,
  PIB=625136.3)
predict(object=modelo_atrasos_90_dias,
        df_predict,
        type = "response"
)
predict(object=step_atrasos_90_dias,
        df_predict,
        type = "response"
)
# Construção de uma matriz de confusão
df$phat_90 <- modelo_atrasos_90_dias$fitted.values
df$phat_step_90 <- step_atrasos_90_dias$fitted.values

# Visualizando a base de dados com a variável 'phat_90 dias'
df %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

# Matriz de confusão para cutoff = 0.5 (função confusionMatrix do pacote caret)
cutoff <- 0.5
cutoff_2 <- .6
confusionMatrix(table(predict(modelo_atrasos_90_dias, type="response") >= cutoff_2,
                      df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])

confusionMatrix(table(predict(step_atrasos_90_dias, type="response") >= cutoff_2,
                      df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])


# Visualizando os principais indicadores desta matriz de confusão
data.frame(Sensitividade = confusionMatrix(table(predict(modelo_atrasos_90_dias,
                                                         type = "response") >= 0.5,
                                                 df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(modelo_atrasos_90_dias,
                                                          type = "response") >= 0.5,
                                                  df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(modelo_atrasos_90_dias,
                                                    type = "response") >= 0.5,
                                            df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)

data.frame(Sensitividade = confusionMatrix(table(predict(step_atrasos_90_dias,
                                                         type = "response") >= 0.5,
                                                 df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(step_atrasos_90_dias,
                                                          type = "response") >= 0.5,
                                                  df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(step_atrasos_90_dias,
                                                    type = "response") >= 0.5,
                                            df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)



#Matriz de confusão para cutoff = 0.3
cutoff_03 <- 0.3
confusionMatrix(table(predict(modelo_atrasos_90_dias, type = "response") >= cutoff_03,
                      df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])
confusionMatrix(table(predict(step_atrasos_90_dias, type = "response") >= cutoff_03,
                      df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])


cutoff_07 <- 0.7
#Matriz de confusão para cutoff = 0.7
confusionMatrix(table(predict(modelo_atrasos_90_dias, type = "response") >= cutoff_07,
                      df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])
confusionMatrix(table(predict(step_atrasos_90_dias, type = "response") >= cutoff_07,
                      df$inadimplido_acima_90_dias_factor == 'sim')[2:1, 2:1])


predicoes_90 <- prediction(predictions = modelo_atrasos_90_dias$fitted.values, 
                          labels = df$inadimplido_acima_90_dias_factor)
predicoes_90_step <- prediction(predictions = modelo_atrasos_90_dias$fitted.values, 
                               labels = df$inadimplido_acima_90_dias_factor) 
#a função prediction, do pacote ROCR, cria um objeto com os dados necessários
#para a futura plotagem da curva ROC.

# função performance do pacote ROCR
dados_curva_roc_90 <- performance(predicoes_90, measure = "sens")
dados_curva_roc_90_step <- performance(predicoes_90_step, measure = "sens") 
#A função peformance(), do pacote ROCR, extrai do objeto 'predicoes' os 
#dados de sensitividade e de especificidade para a plotagem.

#Desejamos os dados da sensitividade e de especificidade. Então, devemos
#digitar os seguintes códigos::

sensitividade_90 <- (performance(predicoes_90, measure = "sens"))@y.values[[1]] 
sensitividade_90_step <- (performance(predicoes_90_step, measure = "sens"))@y.values[[1]] 
especificidade_90 <- (performance(predicoes_90, measure = "spec"))@y.values[[1]]
especificidade_90_step <- (performance(predicoes_90_step, measure = "spec"))@y.values[[1]]

#Extraindo os cutoffs:
cutoffs_90 <- dados_curva_roc_90@x.values[[1]]
cutoffs_90_step <- dados_curva_roc_90_step@x.values[[1]] 

#Até o momento, foram extraídos 3 vetores: 'sensitividade', 'especificidade' 
#e 'cutoffs'. Poder-se-ia plotar normalmente a partir daqui com a linguagem 
#base do R, mas demos preferência à ferramenta ggplot2. Assim, criamos um data 
#frame que contém os vetores mencionados.

dados_plotagem_90 <- cbind.data.frame(cutoffs_90, especificidade_90, sensitividade_90)
dados_plotagem_90_step <- cbind.data.frame(cutoffs_90_step, especificidade_90_step, sensitividade_90_step)

#Visualizando o novo data frame dados_plotagem
dados_plotagem_90 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)


#Plotando:
ggplotly(dados_plotagem_90 %>%
           ggplot(aes(x = cutoffs_90, y = especificidade_90)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs_90, y = sensitividade_90, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs_90, y = sensitividade_90),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

dados_plotagem_90_step %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)


#Plotando:
ggplotly(dados_plotagem_90_step %>%
           ggplot(aes(x = cutoffs_90_step, y = especificidade_90_step)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs_90_step, y = sensitividade_90_step, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs_90_step, y = sensitividade_90_step),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

##############################################################################
#                       CONSTRUÇÃO DA CURVA ROC                 #
##############################################################################
#função roc do pacote pROC
ROC_90 <- roc(response = df$inadimplido_acima_90_dias_factor, 
             predictor = modelo_atrasos_90_dias$fitted.values)

ggplotly(
  ggroc(ROC_90, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC_90$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC_90$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

ROC_90_step <- roc(response = df$inadimplido_acima_90_dias_factor, 
                  predictor = step_atrasos_90_dias$fitted.values)

ggplotly(
  ggroc(ROC_90, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC_90$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC_90$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)
