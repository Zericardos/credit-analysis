from copy import deepcopy
import glob
import os
from typing import List, Dict

import numpy as np
import pandas as pd
from pandas import DataFrame


lista_colunas_base = ['YEAR', 'MONTH', 'DAY']


def estimar_valores(df: DataFrame, valor: str):
    # indice_faltante = np.where(np.diff(df['MONTH']) != 1)[0][0]
    if len(df) == 12:
        return df
    else:
        df.loc[-1, 'MONTH'] = 1#SÓ funciona para um valor faltante, generalizar
        try:
            df['MONTH'] = np.arange(1, 13)
        except:
            print('trap')
        return df.interpolate().interpolate(method='pad')


def formatar_macro(df: DataFrame, valor: str) -> DataFrame:
    df.drop(df[df['YEAR'] != 2020].index, inplace=True)
    df.drop('RAW DATE', axis=1, inplace=True)
    lista_colunas_macro = deepcopy(lista_colunas_base)
    lista_colunas_macro.extend(df.columns[3:])
    valor_antigo = [coluna for coluna in df.columns if coluna.startswith('VALUE')][0]
    df.rename(columns={'CODE': f"CODE_{valor.upper()}",
                       valor_antigo: valor.upper()},
              inplace=True)
    df = estimar_valores(df, valor)
    return df


dir_micro = 'databases/microeconometricas/banco_central'
dir_macro = 'databases/macroeconomicas/ipeadata/time_series'
#carregar dataframes # formatar dataframes
# df_macro_2020_crescimento_geometrico = pd.read_csv(
#     f'{dir_macro}/time_series_ipeadata_taxa_crescimento_geometrico.csv', delimiter=';')
# df_macro_2020_crescimento_geometrico = formatar_macro(df_macro_2020_crescimento_geometrico, 'Crescimento Geometrico')
# df_macro_2020_renda_nacional_bruta = pd.read_csv(
#     f'{dir_macro}/time_series_ipeadata_renda_nacional_bruta.csv', delimiter=';')
# df_macro_2020_renda_nacional_bruta = formatar_macro(df_macro_2020_renda_nacional_bruta, 'RENDA NACIONAL BRUTA')
df_macro_2020_pib = pd.read_csv(f'{dir_macro}/time_series_ipeadata_pib.csv', delimiter=';')
df_macro_2020_pib = formatar_macro(df_macro_2020_pib, 'PIB')
df_macro_2020_desemprego = pd.read_csv(f'{dir_macro}/time_series_ipeadata_taxas_desemprego.csv', delimiter=';')
df_macro_2020_desemprego = formatar_macro(df_macro_2020_desemprego, 'DESEMPREGO')
df_macro_2020_ipca_taxa_variacao = pd.read_csv(
    f'{dir_macro}/time_series_ipeadata_ipca_taxa_variacao.csv', delimiter=';')
df_macro_2020_ipca_taxa_variacao = formatar_macro(df_macro_2020_ipca_taxa_variacao, 'IPCA TAXA VARIACAO')
df_macro_2020_ipca_indice_geral = pd.read_csv(f'{dir_macro}/time_series_ipeadata_ipca_indice_geral.csv', delimiter=';')
df_macro_2020_ipca_indice_geral = formatar_macro(df_macro_2020_ipca_indice_geral, 'IPCA INDICE GERAL')
df_macro_2020_indice_condicoes_economicas_atuais = pd.read_csv(
    f'{dir_macro}/time_series_ipeadata_indice_condicoes_economicas_atuais.csv', delimiter=';')
df_macro_2020_indice_condicoes_economicas_atuais = formatar_macro(
    df_macro_2020_indice_condicoes_economicas_atuais, 'Indice Condicoes Economicas Atuais')

# mergear dataframes
df_macros = pd.merge(left=df_macro_2020_pib, right=df_macro_2020_desemprego,
                                        left_on=lista_colunas_base, right_on=lista_colunas_base)
df_macros = pd.merge(left=df_macros, right=df_macro_2020_ipca_taxa_variacao,
                                        left_on=lista_colunas_base, right_on=lista_colunas_base)
df_macros = pd.merge(left=df_macros, right=df_macro_2020_ipca_indice_geral,
                                        left_on=lista_colunas_base, right_on=lista_colunas_base)
df_macros = pd.merge(left=df_macros, right=df_macro_2020_indice_condicoes_economicas_atuais,
                                        left_on=lista_colunas_base, right_on=lista_colunas_base)

df_micro_2020_01 = pd.read_csv(f'{dir_micro}/planilha_202001.csv', delimiter=';')
df_micro_2020_01['data_base'] = pd.to_datetime(df_micro_2020_01['data_base'], format='%Y-%m-%d')
df_micro_2020_01['YEAR'] = df_micro_2020_01['data_base'].dt.year
df_micro_2020_01['MONTH'] = df_micro_2020_01['data_base'].dt.month
df_micro_2020_01['DAY'] = df_micro_2020_01['data_base'].dt.day
df_micro_2020_01['DAY'] = 1
df_micro_2020_01.drop(labels='data_base', axis=1, inplace=True)
df_micro_2020_01 = pd.concat(
    (df_micro_2020_01[df_micro_2020_01.columns[-3:]], df_micro_2020_01[df_micro_2020_01.columns[:-3]]), axis=1)

df_2020 = pd.merge(left=df_micro_2020_01, right=df_macros, left_on=lista_colunas_base, right_on=lista_colunas_base)
print('trap')
