# Balance Line - Programa em COBOL

Este é um programa COBOL desenvolvido para realizar um Balance Line com base no CNPJ das empresas e de seus sócios.

## Detalhes do Projeto

O programa tem como propósito gerar um relatório de Balance Line utilizando informações dos CNPJs de empresas e de seus respectivos sócios.

## Estrutura do Projeto

O projeto é composto por um único programa COBOL, denominado `CBLZMW06`, que realiza a leitura de dois arquivos de entrada (`CNPJ.txt` e `SOCIOS.txt`) e gera um relatório de Balance Line (`BALANCEMW.txt`).

## Pré-requisitos

Certifique-se de ter os seguintes arquivos de entrada no diretório especificado no código:
- `CNPJ.txt`
- `SOCIOS.txt`

## Compilação e Execução

O programa pode ser compilado e executado em um ambiente COBOL. Certifique-se de fornecer os caminhos corretos para os arquivos de entrada e saída no código.

```bash
cobc -x -free CBLZMW06.cbl
./CBLZMW06
