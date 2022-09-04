import os
from collections import defaultdict
from pathlib import Path

import numpy as np
import pandas as pd
from memory_profiler import profile
from glob import glob


def contar_linhas_arquivo(arquivo: str) -> int:
    with open(arquivo) as io_arquivo:
        return sum(1 for line in io_arquivo) - 1


def nomear_arquivo(arquivo: str) -> str:
    return os.path.basename(arquivo)


TAMANHO_MAXIMO_DATAFRAME = 50000

ano = 2020
SEMENTE_PRIMARIA = 0
total_registros = 0

lista_2020_datasets_primarios = (
    glob(os.path.join('databases', 'microeconometricas', 'banco_central', 'planilhas', 'planilha_2020??.csv'))
)
quantidade_datasets_primarios = len(lista_2020_datasets_primarios)
np.random.seed(SEMENTE_PRIMARIA)
array_sementes_secundarias = np.random.randint(0, 256, size=quantidade_datasets_primarios, dtype=np.uint8)
dict_arquivo_registros = {}
for planilha in lista_2020_datasets_primarios:
    dict_arquivo_registros[nomear_arquivo(planilha)] = contar_linhas_arquivo(planilha)
    total_registros += dict_arquivo_registros[nomear_arquivo(planilha)]

total_datasets = int(total_registros // TAMANHO_MAXIMO_DATAFRAME)

dict_arrays_indices = {}
lista_header = pd.read_csv(lista_2020_datasets_primarios[0], delimiter=';').columns
for (nome_planilha, total_registros_planilha), semente_secundaria in zip(
        dict_arquivo_registros.items(), array_sementes_secundarias):
    np.random.seed(semente_secundaria)
    dict_arrays_indices[nome_planilha] = np.array_split(
        np.random.permutation(total_registros_planilha).astype(np.uint32) + 1, total_datasets)
ano = 2020
dict_indices = defaultdict(list)
dir_fracionados = os.path.join('databases', 'microeconometricas', 'banco_central', f'datasets_fracionados_{ano}')
Path(dir_fracionados).mkdir(exist_ok=True)
lpad = len(str(total_datasets))
for indice_array in range(total_datasets):
    indice_arquivo = str(indice_array + 1).zfill(lpad)
    with open(
            os.path.join(dir_fracionados, f'dataset_fracionado_{ano}_{indice_arquivo}.csv'), 'w') as dataset_fracionado:
        dataset_fracionado.write(f"{';'.join(lista_header)}\n")
        for indice_dataset, arquivo_primario in enumerate(lista_2020_datasets_primarios):
            with open(arquivo_primario) as dataset_primario:
                next(dataset_primario)
                lista_indices_dataset_primario = list(
                    dict_arrays_indices.values())[indice_dataset][indice_array].tolist()
                for indice_registro, registro in enumerate(dataset_primario, 1):
                    if indice_registro in lista_indices_dataset_primario:
                        dataset_fracionado.write(registro)
