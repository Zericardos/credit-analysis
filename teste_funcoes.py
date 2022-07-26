import os
import re
from typing import Union, Tuple

import pandas as pd
from pandas import DataFrame

# os.chdir('databases/macroeconomicas/ipeadata')
# ipea_df = pd.read_csv('variaveis_macroeconomicas_ipeadata.csv', delimiter=';')


# def _remover_caracter_especial(string):
#     for caracter_especial in ('\\w', '?', '`', '*', '_', '{', '}', '[', ']', '(', ')', '>', '#', '+', '-', '.',
#                               '!', '$', '\''):
#         if caracter_especial in string:
#             string = string.replace(caracter_especial, '_')
#     return string


# def criar_arquivos_csv(nome_parte_name: Union[str, Tuple[str]] = '') -> 'csv file':
#     """Gera dois arquivos csv com a variável dada como descrição. Aceita Regex"""
#     descricao = '|'.join(nome_parte_name) if isinstance(nome_parte_name, tuple) else nome_parte_name
#     # arquivo = _remover_caracter_especial('ou'.join(nome_parte_name) if isinstance(nome_parte_name, (tuple, list, set))
#     #                                      else nome_parte_name)
#     arquivo = re.sub(r'[\\w\d{}]', '_', ('_ou_'.join(nome_parte_name) if isinstance(nome_parte_name, (tuple, list, set))
#                                          else nome_parte_name))
#     ipea_df[ipea_df['NAME'].str.contains(f'{descricao}', case=False, regex=True)].to_csv(
#         f'variaveis_macroeconomicas_ipeadata_name_{arquivo}.csv', sep=';', index=False)
#     ipea_df[ipea_df['NAME'].str.contains(f'{descricao}', case=False, regex=True)].to_csv(
#         f'variaveis_macroeconomicas_ipeadata_codigo_{arquivo}.csv', sep=';', index=False, columns=(
#             'INDEX', 'CODE', 'NAME'))


# criar_arquivos_csv(('infla\w{2}o', 'ipca'))

import ipeadatapy as ipea


def criar_arquivos_time_series(codigo: str, arquivo: str) -> None:
    local_dir = 'macroeconomicas/ipeadata/time_series'
    ipea.describe(codigo).to_csv(f'{local_dir}/descricao_time_series_ipeadata_{arquivo}', sep=';', index=False)
    ipea.timeseries(codigo, yearGreaterThan=2011).to_csv(f'{local_dir}/time_series_ipeadata_{arquivo}', sep=';', index=False)


dict_tuplas_codigo_arquivo = {'desemprego': ('ECONMI12_ALU12', 'taxas_desemprego'),
                              'inflacao': (
                                  ('PRECOS12_IPCA12', 'ipca_indice_geral'),
                                  ('PRECOS12_IPCAG12', 'ipca_taxa_variacao')),
                              'taxa_crescimento': ('DEPIS_TGCPOP', 'taxa_crescimento_geometrico'),
                              'renda': (
                                  ('FCESP12_IICA12', 'indice_condicoes_economicas_atuais'),
                                  ('SCN104_PNBN104', 'renda_nacional_bruta'))
                              }
