predicoes_binaria_1_step <- prediction(predictions = modelo_logistico_dummies$fitted.values, 
                                       labels = df_a$carteira_inadimplida_factor) 
dados_curva_roc_binaria_1_step <- performance(predicoes_binaria_1_step, measure = "sens") 
#Desejamos os dados da sensitividade e de especificidade. Então, devemos
#digitar os seguintes códigos::

#sensitividade_multinivel_1_interceptos <- (performance(predicoes_multinivel_1_interceptos, measure = "sens"))@y.values[[1]] 
sensitividade_binaria_1_step <- (performance(predicoes_binaria_1_step, measure = "sens"))@y.values[[1]] 
#especificidade_multinivel_1_interceptos <- (performance(predicoes_multinivel_1_interceptos, measure = "spec"))@y.values[[1]]
especificidade_binaria_1_step <- (performance(predicoes_binaria_1_step, measure = "spec"))@y.values[[1]]
cutoffs_binaria_1_step <- dados_curva_roc_binaria_1_step@x.values[[1]] 
dados_plotagem_binaria_1_step <- cbind.data.frame(cutoffs_binaria_1_step, especificidade_binaria_1_step, sensitividade_binaria_1_step)
roc_curva_roc_binaria_1_step <- roc(response = df_a$carteira_inadimplida_factor,
                                             predictor = predict(object = modelo_logistico_dummies,
                                                                 type = "response"))
roc_multinivel <- roc(response = df_a$carteira_inadimplida_factor,
                                    predictor = predict(object = modelo_multinivel_interceptos_b,
                                                        type = "response"))
cutoff <- .1
confusionMatrix(table(predict(modelo_multinivel_interceptos_b,
                              type = "response") >= cutoff,
                      df_a$carteira_inadimplida_factor == "sim")[2:1, 2:1])
confusionMatrix(table(predict(modelo_multinivel_1_interceptos_1,
                              type = "response") >= cutoff,
                      df_z$carteira_inadimplida_factor == "sim")[2:1, 2:1])
