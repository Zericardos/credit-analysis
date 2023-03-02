# Frequência das variáveis
# Variáveis
#cnae_secao
dplyr::count(df, cnae_secao)
write.csv2(dplyr::count(df, cnae_secao), "frequencias/cnae_secao.csv")
#indexador
dplyr::count(df, indexador)
write.csv2(dplyr::count(df, indexador), "frequencias/indexador.csv")
#modalidade
dplyr::count(df, modalidade)
write.csv2(dplyr::count(df, modalidade), "frequencias/modalidade.csv")
#porte
dplyr::count(df, porte)
write.csv2(dplyr::count(df, porte), "frequencias/porte.csv")
#ocupacao
dplyr::count(df, ocupacao)
write.csv2(dplyr::count(df, ocupacao), "frequencias/ocupacao.csv")
#uf
dplyr::count(df, uf)
write.csv2(dplyr::count(df, uf), "frequencias/uf.csv")
#TAXAS_DESEMPREGO
dplyr::count(df, TAXAS_DESEMPREGO)
write.csv2(dplyr::count(df, TAXAS_DESEMPREGO), "frequencias/TAXAS_DESEMPREGO.csv")
#PIB
dplyr::count(df, PIB)
write.csv2(dplyr::count(df, PIB), "frequencias/PIB.csv")
#IPCA_TAXA_VARIACAO
dplyr::count(df, IPCA_TAXA_VARIACAO)
write.csv2(dplyr::count(df, IPCA_TAXA_VARIACAO), "frequencias/IPCA_TAXA_VARIACAO.csv")
#IPCA_INDICE_GERAL
dplyr::count(df, IPCA_INDICE_GERAL)
write.csv2(dplyr::count(df, IPCA_INDICE_GERAL), "frequencias/IPCA_INDICE_GERAL.csv")
#INDICE_CONDICOES_ECONOMICAS_ATUAIS
dplyr::count(df, INDICE_CONDICOES_ECONOMICAS_ATUAIS)
write.csv2(dplyr::count(df, INDICE_CONDICOES_ECONOMICAS_ATUAIS), "frequencias/INDICE_CONDICOES_ECONOMICAS_ATUAIS.csv")
