       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLZMW06.
      *****************************************************************
      * Author: Marcelo Wzorek Filho
      * Date: 19/01/2024
      * Purpose: Programa que realiza um Balance Line com base no CNPJ das empresas e de seus sócios
      * Updates:
      * 190124 - Marcelo - Create Program
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT LISTA-CNPJ ASSIGN TO
           'C:\VOLVO_ESTAGIO\IDE_COBOL\Exercício06\CNPJ.txt'
           FILE STATUS IS WK-STATUS-E01.

       SELECT LISTA-SOCIOS ASSIGN TO
           'C:\VOLVO_ESTAGIO\IDE_COBOL\Exercício06\SOCIOS.txt'
           FILE STATUS IS WK-STATUS-E02.

       SELECT RELATORIO ASSIGN TO
           'C:\VOLVO_ESTAGIO\IDE_COBOL\Exercício06\BALANCEMW.txt'
           FILE STATUS IS WK-STATUS-S.

       DATA DIVISION.
       FILE SECTION.

       FD LISTA-CNPJ RECORDING MODE IS F.
       01 ARQ-LISTA PIC X(96).
       01 FILLER REDEFINES ARQ-LISTA.
           05 ARQ-L-CNPJ       PIC 9(14).
           05 ARQ-L-SIT        PIC X(06).
           05 ARQ-L-NOME       PIC X(59).
           05 ARQ-L-ATIVO      PIC X(05).
           05 ARQ-L-DATA-ABERT PIC X(10).
           05 ARQ-L-FIM        PIC X(02).

       FD LISTA-SOCIOS RECORDING MODE IS F.
       01 ARQ-SOCIOS PIC X(71).
       01 FILLER REDEFINES ARQ-SOCIOS.
           05 ARQ-S-CNPJ       PIC 9(14).
           05 ARQ-S-CNPJ-SOCIO PIC 9(14).
           05 ARQ-S-NOME       PIC X(36).
           05 ARQ-S-STATUS     PIC X(05).
           05 ARQ-S-FIM        PIC X(02).

       FD RELATORIO RECORDING MODE IS F.
       01 FL-RELATORIO-ARQ PIC X(60).

       WORKING-STORAGE SECTION.
      * Definicao das variaveis de apoio (STATUS e FIM)
       01 WK-STATUS-E01 PIC 9(02) VALUE 0.
       01 WK-STATUS-E02 PIC 9(02) VALUE 0.
       01 WK-STATUS-S   PIC 9(02) VALUE 0.
       01 WK-FIM-ARQ PIC X(01) VALUE 'N'.

      * Definicao dos Cabecalhos do arquivo de saida
       01 WK-CABEC-L          PIC X(60) VALUE ALL '='.

       01 WK-CABEC01.
           02 WK-CABEC01-PROG PIC X(20) VALUE 'CBLZMW03'.
           02 WK-CABEC01-IMPR PIC X(20) VALUE 'VOLVO S.A.'.
           02 WK-CABEC01-MASK PIC XXXXXXXXXX.

       01 WK-CABEC02.
           02 WK-CABEC02-HORA-MASK PIC XXXXXXXX.
           02 WK-CABEC02-SPAC      PIC X(12) VALUE SPACES.
           02 WK-CABEC02-DESC      PIC X(38) VALUE
               'RELATORIO BALANCE LINE'.

       01 WK-DATA-SYS.
           02 WK-YEAR-SYS  PIC 9(04) VALUE 0.
           02 WK-MONTH-SYS PIC 9(02) VALUE 0.
           02 WK-DAY-SYS   PIC 9(02) VALUE 0.

       01 WK-HORA-SYS.
           02 WK-HOUR-SYS   PIC 9(02) VALUE 0.
           02 WK-MINUTE-SYS PIC 9(02) VALUE 0.
           02 WK-SECOND-SYS PIC 9(02) VALUE 0.

       01 WK-LINDIT01.
           02 WK-LINDIT01-CNPJ PIC X(17) VALUE 'CNPJ'.
           02 WK-LINDIT01-SITU PIC X(20) VALUE 'NOME'.

       01 WK-LINDIT02.
           02 WK-LINDIT02-CNPJ PIC X(20) VALUE 'CNPJ'.
           02 WK-LINDIT02-SITU PIC X(20) VALUE '  NOME SOCIO'.

      * Variavel de controle para futuro print do CNPJ.txt
       01 WK-CNPJ-CONTROL PIC 9(14) VALUE ZEROS.

       PROCEDURE DIVISION.
           PERFORM 1000-INICIALIZAR

      * Loop para varrer as listas
           PERFORM UNTIL WK-FIM-ARQ = 'S'
               PERFORM 2000-PROCESSAR
           END-PERFORM

           PERFORM 3000-FINALIZAR
       .
      *****************************************************************
      * INICIALIZAR
      *****************************************************************
       1000-INICIALIZAR SECTION.
           PERFORM 1100-COLETAR-DATA-HORA.

           OPEN INPUT LISTA-CNPJ
           IF WK-STATUS-E01 NOT EQUAL 0
               DISPLAY 'ERRO DE ABERTURA DE CNPJ.txt' WK-STATUS-E01
           END-IF

           OPEN INPUT LISTA-SOCIOS
           IF WK-STATUS-E02 NOT EQUAL 0
               DISPLAY 'ERRO DE ABERTURA DE SOCIOS.txt' WK-STATUS-E02
           END-IF

           OPEN OUTPUT RELATORIO
           IF WK-STATUS-S NOT EQUAL 0
               DISPLAY 'ERRO DE ABERTURA DE RELATORIO.txt' WK-STATUS-S
           END-IF

           PERFORM 1200-INICIALIZAR-CABECALHO.

           PERFORM 2100-LER-LISTA-CNPJ
           PERFORM 2200-LER-LISTA-SOCIOS
       .
       1000-INICIALIZAR-FIM.
           EXIT.
      *****************************************************************
      * COLETAR DATA E HORA DO SISTEMA DO USUARIO
      *****************************************************************
       1100-COLETAR-DATA-HORA SECTION.
           ACCEPT WK-DATA-SYS FROM DATE YYYYMMDD.

           MOVE WK-DAY-SYS TO WK-CABEC01-MASK (1:2).
           MOVE WK-MONTH-SYS TO WK-CABEC01-MASK (4:2).
           MOVE WK-YEAR-SYS TO WK-CABEC01-MASK (7:4).
           MOVE '/' TO WK-CABEC01-MASK (3:1)
                       WK-CABEC01-MASK (6:1).

           ACCEPT WK-HORA-SYS FROM TIME.

           MOVE WK-HOUR-SYS TO WK-CABEC02-HORA-MASK (1:2).
           MOVE WK-MINUTE-SYS TO WK-CABEC02-HORA-MASK (4:2).
           MOVE WK-SECOND-SYS TO WK-CABEC02-HORA-MASK (7:2).
           MOVE ':' TO WK-CABEC02-HORA-MASK (3:1)
                       WK-CABEC02-HORA-MASK (6:1).

       1100-COLETAR-DATA-HORA-EXIT.
           EXIT.
      *****************************************************************
      * INICIALIZAR CABECALHO
      *****************************************************************
       1200-INICIALIZAR-CABECALHO SECTION.
           MOVE WK-CABEC-L TO FL-RELATORIO-ARQ
           WRITE FL-RELATORIO-ARQ

           MOVE WK-CABEC01 TO FL-RELATORIO-ARQ
           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE

           MOVE WK-CABEC02 TO FL-RELATORIO-ARQ
           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE

           MOVE WK-CABEC-L TO FL-RELATORIO-ARQ
           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE

           MOVE WK-LINDIT01 TO FL-RELATORIO-ARQ
           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE

           MOVE WK-LINDIT02 TO FL-RELATORIO-ARQ
           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE
       .
       1200-INICIALIZAR-CABECALHO-EXIT.
           EXIT.
      *****************************************************************
      * PROCESSAR
      *****************************************************************
       2000-PROCESSAR SECTION.
           IF ARQ-L-CNPJ > ARQ-S-CNPJ
               PERFORM 2200-LER-LISTA-SOCIOS
           ELSE
               IF ARQ-L-CNPJ < ARQ-S-CNPJ
                   PERFORM 2100-LER-LISTA-CNPJ
               ELSE
                   PERFORM 2300-GERAR-RELATORIO
               END-IF
           END-IF
       .
       2000-PROCESSAR-FIM.
       EXIT.
      *****************************************************************
      * LER LISTA DE CNPJ
      *****************************************************************
       2100-LER-LISTA-CNPJ SECTION.
           READ LISTA-CNPJ.

           IF WK-STATUS-E01 EQUAL 4
               MOVE 'S' TO WK-FIM-ARQ
           ELSE
               IF WK-STATUS-E01 NOT EQUAL 00
                   DISPLAY 'ERRO AO FECHAR O CNPJ.txt' WK-STATUS-E01
               END-IF
           END-IF

       .
       2100-LER-LISTA-CNPJ-FIM.
       EXIT.
      *****************************************************************
      * LER LISTA DE SOCIOS
      *****************************************************************
       2200-LER-LISTA-SOCIOS SECTION.
           READ LISTA-SOCIOS.

           IF WK-STATUS-E02 EQUAL 4
               MOVE 'S' TO WK-FIM-ARQ
           ELSE
               IF WK-STATUS-E01 NOT EQUAL 00
                   DISPLAY 'ERRO AO FECHAR O SOCIOS.txt' WK-STATUS-E02
               END-IF
           END-IF

           MOVE ARQ-L-CNPJ TO WK-CNPJ-CONTROL
       .
       2200-LER-LISTA-SOCIOS-FIM.
       EXIT.
      *****************************************************************
      * GERAR RELATORIO
      *****************************************************************
       2300-GERAR-RELATORIO SECTION.
           IF WK-CNPJ-CONTROL NOT EQUAL ARQ-L-CNPJ
               MOVE ARQ-L-CNPJ  TO FL-RELATORIO-ARQ (1:14)
               MOVE SPACES      TO FL-RELATORIO-ARQ (15:3)
               MOVE ARQ-L-NOME  TO FL-RELATORIO-ARQ (18:42)
               WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE
           END-IF

           MOVE ARQ-S-CNPJ  TO FL-RELATORIO-ARQ (1:14)
           MOVE SPACES      TO FL-RELATORIO-ARQ (15:8)
           MOVE ARQ-S-NOME  TO FL-RELATORIO-ARQ (23:37)

           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE

           IF WK-STATUS-S NOT EQUAL 0
               DISPLAY 'ERRO AO GRAVAR NO RELATORIO.txt' WK-STATUS-S
           END-IF

           PERFORM 2200-LER-LISTA-SOCIOS
       .
       2600-GERAR-RELATORIO-FIM.
       EXIT.
      *****************************************************************
      * FINALIZAR
      *****************************************************************
       3000-FINALIZAR SECTION.
           CLOSE LISTA-CNPJ
           IF WK-STATUS-E01 NOT EQUAL 0
               DISPLAY 'ERRO AO FECHAR O CNPJ.txt' WK-STATUS-E01
           END-IF

           CLOSE LISTA-SOCIOS
           IF WK-STATUS-E02 NOT EQUAL 0
               DISPLAY 'ERRO AO FECHAR O SOCIOS.txt' WK-STATUS-E02
           END-IF

           CLOSE RELATORIO
           IF WK-STATUS-S NOT EQUAL 0
               DISPLAY 'ERRO AO FECHAR O RELATORIO.txt' WK-STATUS-S
           END-IF

           DISPLAY 'Processo concluido.'

           STOP RUN
       .
       3000-FINALIZAR-FIM.
       EXIT.

       END PROGRAM CBLZMW06.
