       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMPMEN.
      ****************************************
      * MENU DO SISTEMA DE CONSULTAS MEDICAS *
      ****************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC 9(02) VALUE ZEROS.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(50) VALUE SPACES.
       01 W-PROGR      PIC X(07) VALUE SPACES.

      *-----------------------------------------------------------------
       01 TAB-PROGR-R.
           03 FILLER PIC X(07) VALUE "SMP001".
           03 FILLER PIC X(07) VALUE "SMP003".
           03 FILLER PIC X(07) VALUE "SMP050".
           03 FILLER PIC X(07) VALUE "SMP060".
           03 FILLER PIC X(07) VALUE "SMP030R".
           03 FILLER PIC X(07) VALUE "SMP031R".
 
      *
       01 TAB-PROGR   REDEFINES TAB-PROGR-R.
          03 TAB-PROG  PIC X(07) OCCURS 6 TIMES.
      **********************
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  SMTMEN.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             MENU DE ".
           05  LINE 02  COLUMN 41 
               VALUE  "OPCOES".

           05  LINE 05  COLUMN 01 
               VALUE  "                          1 - CADASTRO D".
           05  LINE 05  COLUMN 41 
               VALUE  "E MEDICO".

           05  LINE 06  COLUMN 01 
               VALUE  "                          2 - CADASTRO D".
           05  LINE 06  COLUMN 41 
               VALUE  "E CONVENIO".

           05  LINE 07  COLUMN 01 
               VALUE  "                          3 - CONSULTA D".
           05  LINE 07  COLUMN 41 
               VALUE  "E CONVENIO".

           05  LINE 08  COLUMN 01 
               VALUE  "                          4 - CONSULTA D".
           05  LINE 08  COLUMN 41 
               VALUE  "E MEDICOS".

           05  LINE 09  COLUMN 01 
               VALUE  "                          5 - RELATORIO D".
           05  LINE 09  COLUMN 41 
               VALUE  "E MEDICO".

           05  LINE 10  COLUMN 01 
               VALUE  "                          6 - RELATORIO D".
           05  LINE 10  COLUMN 41 
               VALUE  "E CONVENIO".

          
           05  LINE 20  COLUMN 01 
               VALUE  "                             OPCAO :".
           05  LINE 20  COLUMN 41 
               VALUE  " ( 00 - ENCERRA )".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  T-OPCAO
               LINE 20  COLUMN 38  PIC 9(02)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

      ****************************
      * ROTINA DE SELECAO NIVEL1 *
      ****************************
      *
       NIVEL1.
           DISPLAY SMTMEN.
           
           ACCEPT T-OPCAO.
           IF W-OPCAO = 00
                      GO TO ROT-FIM.
           IF W-OPCAO > 6 
                 MOVE "*** FUNCAO NAO DISPONIVEL *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NIVEL1.
           MOVE TAB-PROG(W-OPCAO) TO W-PROGR
           IF W-PROGR = SPACES
                 MOVE "*** FUNCAO NAO IMPLEMENTADA *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NIVEL1.
      *
       ROT-EXEC.
           CALL W-PROGR ON OVERFLOW
              MOVE "* ERRO NA CHAMADA DO PROGRAMA SELECIONADO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ROT-EXEC1.
           CANCEL W-PROGR.
       ROT-EXEC1.
           GO TO NIVEL1.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           EXIT PROGRAM.
       ROT-FIM1.
           DISPLAY (01, 01) ERASE.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23 12) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23 12) LIMPA.
       ROT-MENS-FIM.
      *
      *---------------------*** FIM DE PROGRAMA ***--------------------*