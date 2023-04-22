fixed       1 21.88582
escola      1 26.50136

#Fixed
-0.849 + 0.713*11 + 1.585*3.6 + 0.232*11*3.6

#Escola (predict de cada estudante)
-0.849 + 0.713*11 + 1.585*3.6 + 0.232*11*3.6 - 0.21 + 0.4388*11


#Modelo OLS com dummies 2
modelo_ols_dummies2 <- lm(formula = desempenho ~ horas + texp + texp:horas + escola_2 +
                           escola_3 + escola_4 + escola_5 + escola_6 +
                           escola_7 + escola_8 + escola_9 + escola_10,
                         data = estudante_escola_dummies)

#Parâmetros
summary(modelo_ols_dummies2)

#Procedimento stepwise
modelo_ols_dummies_step2 <- step(object = modelo_ols_dummies2,
                                step = qchisq(p = 0.05, df = 1,
                                              lower.tail = FALSE))

#Parâmetros do modelo OLS estimado com dummies por escola a partir do
#procedimento Stepwise 2
summary(modelo_ols_dummies_step2)

#Comparando os LL dos modelos HLM2 Final, OLs e OLS com Dummies e Stepwise 2
data.frame(OLS = logLik(modelo_ols),
           OLS_Dummies_Step2 = logLik(modelo_ols_dummies_step2),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS` = 1,
         `OLS com Dummies e Stepwise 2` = 2,
         `HLM2 Modelo Final` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","maroon1","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())
