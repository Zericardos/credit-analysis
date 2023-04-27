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

#Algoritmo para determinação dos erros-padrão das variâncias no componente de
#efeitos aleatórios
#ATENÇÃO: A função abaixo é plenamente funcional para modelos do tipo HLM2
#e HLM3, desde que estimados pelo pacote nlme

stderr_nlme <- function(model){
  if(base::class(model) != "lme"){
    base::message("Use a lme object model from nlme package")
    stop()}
  resume <- base::summary(model)
  if(base::length(base::names(model$groups))==1){
    m.type <- "HLM2"
  } else if(base::length(base::names(model$groups))==2){
    m.type <- "HLM3"
  }
  if(m.type == "HLM2"){
    vcov_matrix <- model$apVar
    logs_sd_re <- base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re)==2){
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE Components`=base::c("Var(v0j)","Var(e)"),
                                  `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                                  base::exp(logs_sd_re[[2]])^2),
                                  `Std Err.`=base::c(stderr_tau00,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
    else{
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
                                  Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                       base::exp(logs_sd_re[[2]])^2,
                                                       base::exp(logs_sd_re[[4]])^2),
                                  Std_Err=base::c(stderr_tau00,
                                                  stderr_tau01,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                            base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                                                               base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
  if(m.type == "HLM3"){
    vcov_matrix <- model$apVar
    logs_sd_re <-  base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re) == 3){
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
                                  Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                                                      base::exp(logs_sd_re)[[1]]^2,
                                                      base::exp(logs_sd_re)[[3]]^2),
                                  Std_Err=base::c(stderr_tau_u000,
                                                  stderr_tau_r000,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    } 
    else{
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                                          "Var(v0jk)","Var(v1jk)",
                                                          "Var(e)"),
                                  `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                                                 base::exp(logs_sd_re)[[5]]^2,
                                                                 base::exp(logs_sd_re)[[1]]^2,
                                                                 base::exp(logs_sd_re)[[2]]^2,
                                                                 base::exp(logs_sd_re)[[7]]^2),
                                  `Std Err.`=base::c(stderr_tau_u000,
                                                     stderr_tau_u100,
                                                     stderr_tau_r000,
                                                     stderr_tau_r100,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                            base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                                                               base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
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

modelo_nulo_multinivel_1_nivel_1 <- glmmTMB(formula = inadimplido_acima_90_dias_factor ~ 1
                                            + (1 | uf),
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
#lrtest(modelo_nulo_multinivel_1, modelo_nulo_logistico_1)
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

modelo_nulo_multinivel_1_nivel_2_todas_variaveis <- glmmTMB(formula = inadimplido_acima_90_dias_factor ~ cnae_secao + indexador + modalidade + porte + 
                                              ocupacao + TAXAS_DESEMPREGO + PIB + IPCA_TAXA_VARIACAO + IPCA_INDICE_GERAL +
                                              INDICE_CONDICOES_ECONOMICAS_ATUAIS
                                            + (1 | uf),
                                            data = df,
                                            family = binomial,
                                            REML = TRUE)

logLik(modelo_nulo_multinivel_1_nivel_2_todas_variaveis)


modelo_nulo_multinivel_1_nivel_3 <- glmmTMB(formula = inadimplido_acima_90_dias_factor ~ indexador
                                             + (modalidade| uf),
                                             data = df,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_nulo_multinivel_1_nivel_3)
fixef(modelo_nulo_multinivel_1_nivel_3
df_teste <- drop_na(df, indexador)
table(df_teste$indexador)

modelo_nulo_multinivel_1_nivel_2 <- glmmTMB(
  formula = inadimplido_acima_90_dias_factor ~ cnae_secao + indexador + 
  modalidade + porte + ocupacao + TAXAS_DESEMPREGO + PIB + IPCA_TAXA_VARIACAO +
   IPCA_INDICE_GERAL + INDICE_CONDICOES_ECONOMICAS_ATUAIS + (1 | uf),
  data = df,
  family = binomial,
  REML = TRUE
  )

logLik(modelo_nulo_multinivel_1)


# Comparação entre os LLs dos modelos
# data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
#           HLM2_Nulo = logLik(modelo_nulo_hlm2)) %>%
#  rename(`OLS Nulo` = 1,
#         `HLM2 Nulo` = 2) %>%
#  melt() %>%
#  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
#  geom_bar(stat = "identity") +
#  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
#  labs(title = "Comparação do LL", 
#       y = "LogLik", 
#       x = "Modelo Proposto") +
#  coord_flip() +
#  scale_fill_manual("Legenda:",
#                    values = c("grey25","grey45")) +
#  theme(legend.title = element_blank(), 
#        panel.background = element_rect("white"),
#        legend.position = "none",
#        axis.line = element_line())


base_turismo %>% 
  mutate(fitted_probs_hnlm2 = predict(object = modelo_turismo_random_intercepts, 
                                      type = "response")) %>% 
  ggplot(aes(x = filhos, y = fitted_probs_hnlm2)) +
  geom_smooth(aes(group = país, color = país), 
              method = "lm", formula = y ~ splines::bs(x), se = F) +
  scale_colour_viridis_d() +
  facet_wrap(~país) +
  labs(y = "Fitted Probs",
       x = "Quantidade de Filhos") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")
