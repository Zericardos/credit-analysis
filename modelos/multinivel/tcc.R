dummyVars.undo = function(df, col_prefix) {
  if (!endsWith(col_prefix, '_')) {
    # If col_prefix doesn't end with a period, include one, but save the
    # "pretty name" as the one without a period
    pretty_col_prefix = col_prefix
    col_prefix = paste0(col_prefix, '_')
  } else {
    # Otherwise, strip the period for the pretty column name
    pretty_col_prefix = substr(col_prefix, 1, nchar(col_prefix)-1)
  }
  
  # Get all columns with that encoding prefix
  cols = names(df)[names(df) %>% startsWith(col_prefix)]
  
  # Find the rows where all values are zero. If this isn't the case
  # with your data there's no worry, it won't hurt anything.
  base_level.idx = rowSums(df[cols]) == 0
  
  # Set the column value to a base value of zero
  df[base_level.idx, pretty_col_prefix] = 0
  
  # Go through the remaining columns and find where the maximum value (1) occurs
  df[!base_level.idx, pretty_col_prefix] = cols[apply(df[!base_level.idx, cols], 1, which.max)] %>%
    strsplit('\\_') %>%
    sapply(tail, 1) 
  
  # Drop the encoded columns
  df[cols] = NULL
  
  return(df)  
}


df_1 <- read.csv2('pre_processamento/dataset_fracionado_2020_0001.csv', dec='.')
df_2 <- read.csv2('pre_processamento/dataset_fracionado_2020_0002.csv', dec='.')
df_3 <- read.csv2('pre_processamento/dataset_fracionado_2020_0003.csv', dec='.')
dim(df_1)
dim(df_2)
dim(df_3)
df <- rbind(df_1, df_2)
dim(df)
df <- rbind(df, df_3)
dim(df)
str(df)
head(df$carteira_inadimplida_arrastada)
df$carteira_inadimplida_arrastada <- as.numeric(gsub(",", ".", df$carteira_inadimplida_arrastada))
head(df$carteira_inadimplida_arrastada)
df %>%
  mutate(carteira_inadimplida_factor = ifelse(carteira_inadimplida_arrastada > 0,
                                              yes = "sim",
                                              no = "nao"),
         carteira_inadimplida_factor = factor(carteira_inadimplida_factor)) -> df

df <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
df = subset(df, select=-c(
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
  origem
))


# Modelos nulos

modelo_nulo_multinivel <- glmmTMB(formula = carteira_inadimplida_factor ~ 1 + (1 | uf),
                                              data = df,
                                              family = binomial,
                                              REML = TRUE)


modelo_nulo_logistico <- glm(
  formula = carteira_inadimplida_factor ~ 1,
  data = df,
  family = "binomial"
)
summary(modelo_nulo_multinivel) # agrupamento uf
summary(modelo_nulo_logistico)
logLik(modelo_nulo_multinivel)
logLik(modelo_nulo_logistico)

# Modelos com variáveis

modelo_logistico <- glm(
  formula = carteira_inadimplida_factor ~ cnae_secao + indexador + modalidade + porte + 
    ocupacao + uf + TAXAS_DESEMPREGO + PIB + IPCA_TAXA_VARIACAO + IPCA_INDICE_GERAL +
    INDICE_CONDICOES_ECONOMICAS_ATUAIS,
  data = df,
  family = "binomial"
)

logLik(modelo_logistico)
summary(modelo_logistico)
roc_logistico <- roc(response = df$carteira_inadimplida_factor,
                      predictor = predict(object = modelo_logistico,
                                          type = "response"))
roc_logistico
#Procedimento Stepwise
step_modelo_logistico <- step(object = modelo_logistico,
                                k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

# Obter colunas estatísticamente significantes
summary(step_modelo_logistico)
logLik(step_modelo_logistico)
roc_step_logistico <- roc(response = df$carteira_inadimplida_factor,
                     predictor = predict(object = step_modelo_logistico,
                                         type = "response"))
roc_step_logistico
# Dummização
df_dummies <- dummy_columns(.data = df,
                            select_columns = c("cnae_secao", "indexador", "modalidade", "porte", "ocupacao", "uf"),
                            remove_selected_columns = T,
                            remove_most_frequent_dummy = F)
colnames(df_dummies)
# Ajustar nomes de colunas
df_dummies_fixed_names <- clean_names(df_dummies)
colnames(df_dummies_fixed_names)
dim(df_dummies_fixed_names)
df_dummies_fixed_names_selected_columns <- subset(df_dummies_fixed_names, select=c(
    indice_condicoes_economicas_atuais,
    ipca_indice_geral,
    taxas_desemprego,
    carteira_inadimplida_factor,
    cnae_secao_pj_administracao_publica_defesa_e_seguridade_social,
    cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura,
    cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao,
    cnae_secao_pj_alojamento_e_alimentacao,
    cnae_secao_pj_artes_cultura_esporte_e_recreacao,
    cnae_secao_pj_atividades_administrativas_e_servicos_complementares,
    cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados,
    cnae_secao_pj_atividades_imobiliarias,
    cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas,
    cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas,
    cnae_secao_pj_construcao,
    cnae_secao_pj_educacao,
    cnae_secao_pj_eletricidade_e_gas,
    cnae_secao_pj_industrias_de_transformacao,
    cnae_secao_pj_industrias_extrativas,
    cnae_secao_pj_informacao_e_comunicacao,
    cnae_secao_pj_outras_atividades_de_servicos,
    cnae_secao_pj_saude_humana_e_servicos_sociais,
    cnae_secao_pj_transporte_armazenagem_e_correio,
    indexador_outros_indexadores,
    indexador_pos_fixado,
    indexador_prefixado,
    modalidade_pf_habitacional,
    modalidade_pf_outros_creditos,
    modalidade_pf_rural_e_agroindustrial,
    modalidade_pf_veiculos,
    modalidade_pj_capital_de_giro_rotativo,
    modalidade_pj_outros_creditos,
    porte_pf_ate_1_salario_minimo,
    porte_pf_indisponivel,
    porte_pf_mais_de_1_a_2_salarios_minimos,
    porte_pf_mais_de_2_a_3_salarios_minimos,
    porte_pf_mais_de_3_a_5_salarios_minimos,
    porte_pf_mais_de_5_a_10_salarios_minimos,
    porte_pj_grande,
    porte_pj_indisponivel,
    porte_pj_medio,
    ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos,
    ocupacao_pf_mei,
    uf_ba,
    uf_ce,
    uf_es,
    uf_go,
    uf_mg,
    uf_mt,
    uf_pe,
    uf_pi,
    uf_pr,
    uf_rj,
    uf_rr,
    uf_rs,
    uf_sp
    ))
modelo_logistico_dummies <- glm(
  formula = carteira_inadimplida_factor ~ .,
  data = df_a,
  family = "binomial"
)

logLik(modelo_logistico_dummies)
summary(modelo_logistico_dummies)

roc_step_logistico_dummies <- roc(response = df_a$carteira_inadimplida_factor,
                           predictor = predict(object = modelo_logistico_dummies,
                                               type = "response"))
roc_step_logistico_dummies


#step_modelo_logistico_dummies_2 <- step(object = modelo_logistico_dummies,
#                              k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

logLik(step_modelo_logistico_dummies_2)
summary(step_modelo_logistico_dummies_2)

dim(df_dummies_fixed_names_selected_columns)
df_a <- dummyVars.undo(df_dummies_fixed_names_selected_columns, 'uf')
colnames(df_a)
tail(df_a$uf)
df_a <- df_a[!(df_a$uf=="0"),]
dim(df_a)
head(df_a$uf)
unique(df_a$uf)
count(df, uf)
count(df_a, uf)
colnames(df_a)


modelo_multinivel_interceptos_b <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + indexador_pos_fixado + indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
modalidade_pf_rural_e_agroindustrial + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_b)
summary(modelo_multinivel_interceptos_b)
roc_modelo_multinivel_interceptos_b <- roc(response = df_a$carteira_inadimplida_factor,
                     predictor = predict(object = modelo_multinivel_interceptos_b,
                                         type = "response"))
roc_modelo_multinivel_interceptos_b

cutoff <- .25
confusionMatrix(table(predict(modelo_logistico_dummies,
                              type = "response") >= cutoff,
                      df_a$carteira_inadimplida_factor == "sim")[2:1, 2:1])
confusionMatrix(table(predict(modelo_multinivel_interceptos_b,
                              type = "response") >= cutoff,
                      df_a$carteira_inadimplida_factor == "sim")[2:1, 2:1])

modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)
modelo_multinivel_interceptos_a <- glmmTMB(formula = carteira_inadimplida_factor ~ indice_condicoes_economicas_atuais +
 ipca_indice_geral + taxas_desemprego + cnae_secao_pj_administracao_publica_defesa_e_seguridade_social +
  cnae_secao_pj_agricultura_pecuaria_producao_florestal_pesca_e_aquicultura +
  cnae_secao_pj_agua_esgoto_atividades_de_gestao_de_residuos_e_descontaminacao +
  cnae_secao_pj_alojamento_e_alimentacao + cnae_secao_pj_artes_cultura_esporte_e_recreacao +
  cnae_secao_pj_atividades_administrativas_e_servicos_complementares +
  cnae_secao_pj_atividades_financeiras_de_seguros_e_servicos_relacionados + cnae_secao_pj_atividades_imobiliarias +
  cnae_secao_pj_atividades_profissionais_cientificas_e_tecnicas +
  cnae_secao_pj_comercio_reparacao_de_veiculos_automotores_e_motocicletas + cnae_secao_pj_construcao +
  cnae_secao_pj_educacao + cnae_secao_pj_eletricidade_e_gas + cnae_secao_pj_industrias_de_transformacao +
  cnae_secao_pj_industrias_extrativas + cnae_secao_pj_informacao_e_comunicacao +
  cnae_secao_pj_outras_atividades_de_servicos + cnae_secao_pj_saude_humana_e_servicos_sociais +
  cnae_secao_pj_transporte_armazenagem_e_correio + indexador_outros_indexadores + indexador_pos_fixado +
  indexador_prefixado + modalidade_pf_habitacional + modalidade_pf_outros_creditos +
  modalidade_pf_rural_e_agroindustrial + modalidade_pf_veiculos +
  modalidade_pj_capital_de_giro_rotativo + modalidade_pj_outros_creditos +
   porte_pf_ate_1_salario_minimo + porte_pf_indisponivel + porte_pf_mais_de_1_a_2_salarios_minimos +
   porte_pf_mais_de_2_a_3_salarios_minimos + porte_pf_mais_de_3_a_5_salarios_minimos +
   porte_pf_mais_de_5_a_10_salarios_minimos + porte_pj_grande + porte_pj_indisponivel + porte_pj_medio +
   ocupacao_pf_empregado_de_entidades_sem_fins_lucrativos + ocupacao_pf_mei+
 (1 | uf),
                                             data = df_a,
                                             family = binomial,
                                             REML = TRUE)
logLik(modelo_multinivel_interceptos_a)
summary(modelo_multinivel_interceptos_a)




















































modelo_logistico_1_dummies <- glm(
  formula = carteira_inadimplida_factor ~ . ,
  data = df_a,
  family = "binomial"
)
logLik(modelo_logistico_1_dummies)
summary(modelo_logistico_1_dummies)
