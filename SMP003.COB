       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP003.
      *AUTHOR. LUCAS GUILHERME LIAS.
      **************************************
      * CADASTRO DE CONVENIO  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 

       01 REGCONV.
              
          03 CODIGO                   PIC 9(06).

       	  03 NOME         	      PIC X(30).

          03 PLANO                    PIC 9(02).

      *
      *-----------------------------------------------------------------
	WORKING-STORAGE SECTION.

       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES.

       01 IND           PIC 9(02) VALUE ZEROS.

       01 W-SEL         PIC 9(01) VALUE ZEROS.

       01 ST-ERRO       PIC X(02) VALUE "00".

       01 TABPLANO.
          03 FILLER     PIC X(30) VALUE "ENFERMARIA REGINAL".
          03 FILLER     PIC X(30) VALUE "ENFERMARIA NACIONAL".
          03 FILLER     PIC X(30) VALUE "ENFERMARIA INTERNACIONAL".
          03 FILLER     PIC X(30) VALUE "APTO PADRÃO REGINAL".
          03 FILLER     PIC X(30) VALUE "APTO PADRAO NACIONAL".
          03 FILLER     PIC X(30) VALUE "APTO PADRAO INTERNACIONAL".
          03 FILLER     PIC X(30) VALUE "EMERGENCIA REGIONAL".
          03 FILLER     PIC X(30) VALUE "EMERGENCIA NACIONAL".
          03 FILLER     PIC X(30) VALUE "EMERCENCIA INTERNACIONAL".
          03 FILLER     PIC X(30) VALUE "PLANO GLOBAL".
          03 FILLER     PIC X(30) VALUE "PLANO VIP".

       01 TABEPLANO REDEFINES TABPLANO.
          05 TBEPLANO  PIC X(30) OCCURS 11 TIMES.

       01 TXTPLANO   PIC X(30) VALUE SPACES.


      *-----------------------------------------------------------------
	SCREEN SECTION.

       01  TELA1.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "Cadastro De Planos".

	   05  LINE 04  COLUMN 01 
               VALUE  " Codigo:".

           05  LINE 06  COLUMN 01 
               VALUE  " Nome:".

           05  LINE 08  COLUMN 01 
               VALUE  " Plano:".
          
           05  LINE 21  COLUMN 01 
               VALUE  " MENSAGEM:". 
        
           05  TCODIGO
               LINE 04  COLUMN 09  PIC 9(06)
               USING CODIGO
               HIGHLIGHT.

           05  TNOME
               LINE 06  COLUMN 07  PIC X(30)
               USING  NOME
               HIGHLIGHT.

           05  TPLANO
               LINE 08  COLUMN 7  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.

           05 TTXTPLANO
               LINE 08  COLUMN 10  PIC X(30)
               USING  TXTPLANO
               HIGHLIGHT.


       01  OPCTABELPLANO.
           05  LINE 08  COLUMN 41 VALUE " 01 ENFERMARIA REGINAL".
           05  LINE 09  COLUMN 41 VALUE " 02 ENFERMARIA NACIONAL".
           05  LINE 10  COLUMN 41 VALUE " 03 ENFERMARIA INTERNACIONAL".
           05  LINE 11  COLUMN 41 VALUE " 04 APTO PADRÃO REGINAL".
           05  LINE 12  COLUMN 41 VALUE " 05 APTO PADRAO NACIONAL".
           05  LINE 13  COLUMN 41 VALUE " 06 APTO PADRAO INTERNACIONAL".
           05  LINE 14  COLUMN 41 VALUE " 07 EMERGENCIA REGIONAL".
           05  LINE 15  COLUMN 41 VALUE " 08 EMERGENCIA NACIONAL".
           05  LINE 16  COLUMN 41 VALUE " 09 EMERCENCIA INTERNACIONAL".
           05  LINE 17  COLUMN 41 VALUE " 10 PLANO GLOBAL".
           05  LINE 18  COLUMN 41 VALUE " 11 PLANO VIP".



  
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

       INICIO.

       R0.
           OPEN I-O CADCONV.
           IF ST-ERRO NOT = "00"
             IF ST-ERRO = "30"
                 OPEN OUTPUT CADCONV
                 CLOSE CADCONV
                 MOVE "*** ARQUIVO CADCONV FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
             ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONV" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
       R1.
            MOVE SPACES TO TXTPLANO NOME
            MOVE ZEROS TO CODIGO PLANO 
            DISPLAY TELA1.
           
       R2.
           ACCEPT TCODIGO

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.

           IF CODIGO = ZEROS
                GO TO R2.

       LER-CADMED.
           READ CADCONV
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R4A
                DISPLAY TELA1
                MOVE "*** CONVENIO JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
 
       R3.
           ACCEPT TNOME

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.

           IF NOME = SPACES
                GO TO R3.
       
       R4.
           DISPLAY OPCTABELPLANO.
           ACCEPT TPLANO

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.

           IF PLANO = 00
                MOVE "DIGITE APENAS DE 01 ATE 09 " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R4.

           IF PLANO > 11
                MOVE "DIGITE APENAS DE 01 ATE 11" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R4.

       R4A.
            MOVE TBEPLANO(PLANO) TO TXTPLANO.
            DISPLAY TTXTPLANO.
            DISPLAY TELA1.
           
     

       INC-OPC.
           MOVE "S" TO W-OPCAO
           DISPLAY (23, 40) "DADOS OK (S/N) : ".
           ACCEPT (23, 57) W-OPCAO WITH UPDATE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 GO TO R4.
           IF W-OPCAO = "N" OR "n"
           MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1.
           IF W-OPCAO NOT = "S" AND "s"
           MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INC-OPC.
       INC-WR1.
           WRITE REGCONV
           IF ST-ERRO = "00" OR "02"
                MOVE "*** DADOS GRAVADOS *** " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1.
                
                IF ST-ERRO = "22"
                   MOVE "*JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1
                ELSE
                   MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.


      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCONV RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CONVENIO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R4.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO MEDICO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.



       ROT-FIM.
           CLOSE CADCONV.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------

       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    codigo dos erros
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO

