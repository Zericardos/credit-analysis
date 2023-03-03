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
df <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
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
predict(object=modelo_atrasos_0_dias,
        df_predict,
        type = "response"
        )
predict(object=modelo_atrasos_90_dias,
        df_predict,
        type = "response"
)


