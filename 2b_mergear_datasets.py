from copy import deepcopy
from glob import glob
import os
from pathlib import Path
from typing import Union, List
import numpy as np
import pandas as pd
from pandas import DataFrame


lista_colunas_base = ['YEAR', 'MONTH', 'DAY']


def estimar_valores(df: DataFrame) -> DataFrame:
    if len(df) == 12:
        return df
    else:
        df.loc[-1, 'MONTH'] = 1
        df['MONTH'] = np.arange(1, 13)
        return df.interpolate().interpolate(method='pad')


def _formatar_macro(arquivo_csv: str, periodo: int) -> DataFrame:
    df = pd.read_csv(arquivo_csv, delimiter=';')
    df.drop(df[df['YEAR'] != periodo].index, inplace=True)
    df.drop('RAW DATE', axis=1, inplace=True)
    lista_colunas_macro = deepcopy(lista_colunas_base)
    lista_colunas_macro.extend(df.columns[3:])
    valor_antigo = [coluna for coluna in df.columns if coluna.startswith('VALUE')][0]
    nome_dataframe = os.path.basename(arquivo_csv)
    valor = nome_dataframe.replace('time_series_ipeadata_', '').replace('.csv', '')
    df.rename(columns={'CODE': f"CODE_{valor.upper()}", valor_antigo: valor.upper()}, inplace=True)
    df = estimar_valores(df)
    return df


def formatar_periodo(funcao):
    def wrapper(dir_macro, periodo: Union[str, int]):
        if isinstance(periodo, str):
            return funcao(dir_macro, int(periodo))
        elif isinstance(periodo, int):
            return funcao(dir_macro, periodo)
        else:
            print(f'O período informado, {periodo}, não é válido')
            #     log.error(f'O período informado, {periodo}, não é válido')
    return wrapper


def filtrar_macros(lista_arquivos_csv: List[str]) -> List[str]:
    """Função Provisória
    TODO: Deve ser descontinuada quando puder ser implementada os datasets filtrados aqui"""
    nova_lista_arquivos = []
    for arquivo_csv in lista_arquivos_csv:
        if 'renda_nacional_bruta' not in arquivo_csv and 'taxa_crescimento_geometrico' not in arquivo_csv:
            nova_lista_arquivos.append(arquivo_csv)
    return nova_lista_arquivos


@formatar_periodo
def _criar_df_macro_periodo(dir_macro, periodo: Union[str, int]):
    lista_arquivos_csv = filtrar_macros(glob(os.path.join(dir_macro, 'time_series_ipeadata_*.csv')))
    df_macro_periodo = _formatar_macro(lista_arquivos_csv[0], periodo)
    for arquivo_csv in lista_arquivos_csv[1:]:
        df = _formatar_macro(arquivo_csv, periodo)
        df_macro_periodo = pd.merge(left=df_macro_periodo, right=df,
                                    left_on=lista_colunas_base, right_on=lista_colunas_base)
    return df_macro_periodo


def _formatar_data_df(df: DataFrame) -> DataFrame:
    df['data_base'] = pd.to_datetime(df['data_base'], format='%Y-%m-%d')
    df['YEAR'], df['MONTH'], df['DAY'] = df['data_base'].dt.year, df['data_base'].dt.month, df['data_base'].dt.day
    df['DAY'] = 1
    df.drop(labels='data_base', axis=1, inplace=True)
    return pd.concat((df[df.columns[-3:]], df[df.columns[:-3]]), axis=1)


def _criar_arquivos_csv_mergeados(df_macro: DataFrame, dir_micro: str, dir_saida) -> None:
    lista_arquivos_csv = glob(f'{dir_micro}/*.csv')
    for arquivo_csv in lista_arquivos_csv:
        df = pd.read_csv(arquivo_csv, delimiter=';')
        df = _formatar_data_df(df)
        df = pd.merge(left=df_macro, right=df, left_on=lista_colunas_base, right_on=lista_colunas_base)
        df.to_csv(os.path.join(dir_saida, f'{os.path.basename(arquivo_csv)}'), sep=';', index=False)


if __name__ == '__main__':
    ano = 2020
    TAMANHO_MAXIMO_DATAFRAME = 5000
    DIR_MACRO = os.path.join('databases', 'macroeconomicas', 'ipeadata', 'time_series')
    df_macroeconomico = _criar_df_macro_periodo(DIR_MACRO, ano)
    DIR_MICRO = os.path.join(
        'databases', 'microeconometricas', 'banco_central', f'datasets_fracionados_{ano}_{TAMANHO_MAXIMO_DATAFRAME}')
    DIR_SAIDA = Path(os.path.join('databases', f'mergeados_periodo_ano_{TAMANHO_MAXIMO_DATAFRAME}'))
    DIR_SAIDA.mkdir(exist_ok=True)
    _criar_arquivos_csv_mergeados(df_macroeconomico, DIR_MICRO, DIR_SAIDA)
    print('feito')
