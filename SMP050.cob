       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP050.
      *AUTHOR. LUCAS GUILHERME LIAS.
      **************************************************
      * CONSULTA DE CONVENIO *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".

       01 REGCONV.   
          03 CODIGO                   PIC 9(06).
          03 NOME                     PIC X(30).
          03 PLANO                    PIC 9(02).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL       PIC 9(01) VALUE ZEROS.
       01 W-CONT      PIC 9(04) VALUE ZEROS.
       01 W-OPCAO     PIC X(01) VALUE SPACES.
       01 ST-ERRO     PIC X(02) VALUE "00".
       01 W-ACT       PIC 9(02) VALUE ZEROS.
       01 MENS        PIC X(50) VALUE SPACES.
       01 LIMPA       PIC X(55) VALUE SPACES.
       01 SOLIC       PIC X(20) VALUE SPACES.
       01 CONLIN      PIC 9(03) VALUE 001.

       01 CEPENTR     PIC 9(09) VALUE ZEROS.
       01 LOGRENTR    PIC X(30) VALUE SPACES.

       01 IND         PIC 9(05) VALUE ZEROS.
       01 TABCONV.
          03 TBCONV    PIC 9(08) OCCURS 1000 TIMES.
       01 NUMREG      PIC 9(04) VALUE ZEROS.  
      *-----------------------------------------------------------------
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  SMT050.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             CONSULTA DE".
           05  LINE 02  COLUMN 41 
               VALUE  " CONVENIO".
           
           05  LINE 05  COLUMN 01 
               VALUE  "    CODIGO: ".

           05  LINE 07  COLUMN 01 
               VALUE  "    NOME: ".

           05  LINE 09  COLUMN 01 
               VALUE  "    PLANO: ".

           05  LINE 21  COLUMN 01 
               VALUE  "              OPCAO :   (E=ENCERRA     P".
           05  LINE 21  COLUMN 41 
               VALUE  "=PROXIMO     A=ANTERIOR )".
           05  LINE 22  COLUMN 41 
               VALUE  "   ==>            <==".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  LINE 23  COLUMN 41 
               VALUE  "                                 SMP050".
               
           05  TCODIGO
               LINE 05  COLUMN 20  PIC 9(06)
               USING  CODIGO
               HIGHLIGHT.

           05  TNOME
               LINE 07  COLUMN 20  PIC X(30)
               USING  NOME
               HIGHLIGHT.

           05  TPLANO
               LINE 09  COLUMN 20  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.

           05  TW-OPCAO
               LINE 21  COLUMN 23  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  CADCONV
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADCONV NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADCONV"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
           MOVE 1 TO IND
           MOVE ZEROS TO CODIGO NUMREG.
           START CADCONV KEY IS NOT LESS CODIGO INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.

       LER-CONV.           
           READ CADCONV NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE IND TO NUMREG
                 ADD -1 TO NUMREG
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCONV"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              MOVE CODIGO TO TBCONV(IND)
              ADD 1 TO IND              
              IF IND > 1000
                 MOVE "*** TABELA ESTOURADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 GO TO LER-CONV.

      *
       INC-001.
           MOVE ZEROS TO CODIGO PLANO
           MOVE SPACES TO NOME
           DISPLAY  SMT050.
       INC-001A.
           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADCONV
                      GO TO ROT-FIM.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM   
                      GO TO INC-001A.
           MOVE 1 TO IND.

       R111.
           IF TBCONV(IND) < CODIGO
              ADD 1 TO IND
              IF IND >1000
                 MOVE "*** CONVENIO NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001A
              ELSE
                 GO TO R111.
       R112.
           MOVE TBCONV(IND) TO CODIGO.

       INC-RD2.
           READ CADCONV 
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "*** REGISTRO NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCONV"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       ROT-MONTAR.
              DISPLAY SMT050.

       ROT-SOL.
           ACCEPT  TW-OPCAO  
           IF W-OPCAO = "E"
                  GO TO ROT-FIM 
           ELSE
             IF W-OPCAO = "P"
                 IF IND < NUMREG
                   ADD 1 TO IND
                   GO TO R112
                 ELSE
                   MOVE "*** ULTIMO REGISTRO ***" TO MENS
                           PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-SOL
             ELSE
                IF W-OPCAO = "A"
                    IF IND > 1
                       ADD -1 TO IND
                       GO TO R112
                    ELSE
                       MOVE "*** PRIMEIRO REGISTRO ***" TO MENS
                           PERFORM ROT-MENS THRU ROT-MENS-FIM
                       GO TO ROT-SOL
                ELSE
                
                     MOVE "*** OPCAO NAO DISPONIVEL ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO ROT-SOL.

      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.

           CLOSE CADCONV.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *

      *---------------------*** FIM DE PROGRAMA ***--------------------*