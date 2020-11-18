       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP060.
      *AUTHOR. LUCAS GUILHERME LIAS.
      **************************************************
      * CONSULTA DE MEDICO *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT".

       01 REGMED.      
          03 CRM                   PIC 9(06).
          03 NOME                  PIC X(30).
          03 ESPECIALIDADE         PIC 9(02).
          03 SEXO                  PIC X(01).
          03 NASC.   
                05 DIA             PIC 9(02).
                05 MES             PIC 9(02).
                05 ANO             PIC 9(04).
          03 EMAIL                 PIC X(30).
          03 TELEFONE              PIC 9(09).
          03 DDD                   PIC 9(02).                 
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
       01 IND         PIC 9(05) VALUE ZEROS.

       01 TABMED.
          03 TBMED   PIC 9(06) OCCURS 1000 TIMES.
       01 NUMREG      PIC 9(04) VALUE ZEROS. 

      *-----------------------------------------------------------------
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  SMT060.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             CONSULTA DE".
           05  LINE 02  COLUMN 41 
               VALUE  " MEDICO".
           
           05  LINE 04  COLUMN 01 
               VALUE  " Crm:".
           05  LINE 06  COLUMN 01 
               VALUE  " Nome:".
           05  LINE 08  COLUMN 01 
               VALUE  " Especialidade:".
           05  LINE 10  COLUMN 01 
               VALUE  " Sexo:".
           05  LINE 12  COLUMN 01 
               VALUE  " Data Nascimento:".
           05  LINE 14  COLUMN 01 
               VALUE  " Email:".
           05  LINE 16  COLUMN 01 
               VALUE  " Telefone:".
           05  LINE 21  COLUMN 01 
               VALUE  " MENSAGEM:". 


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

           05  TCRM
               LINE 04  COLUMN 07  PIC X(06)
               USING CRM
               HIGHLIGHT.

           05  TNOME
               LINE 06  COLUMN 08  PIC X(30)
               USING  NOME
               HIGHLIGHT.

           05  TESPECIALIDADE
               LINE 08  COLUMN 18  PIC 9(02)
               USING  ESPECIALIDADE
               HIGHLIGHT.

           05  TSEXO
               LINE 10  COLUMN 07  PIC X(01)
               USING SEXO
               HIGHLIGHT.

           05  TDIA
               LINE 12  COLUMN 20  PIC 9(02)
               USING  DIA
               HIGHLIGHT.

           05  TMES
               LINE 12  COLUMN 23  PIC 9(02)
               USING  MES
               HIGHLIGHT.

           05  TANO
               LINE 12  COLUMN 26  PIC 9(04)
               USING  ANO
               HIGHLIGHT.

           05  TEMAIL
               LINE 14  COLUMN 9  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.

           05  TDDD
               LINE 16  COLUMN 12  PIC 9(02)
               USING DDD
               HIGHLIGHT.

           05  TTELEFONE
               LINE 16  COLUMN 15  PIC 9(09)
               USING  TELEFONE
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
           OPEN INPUT  CADMED
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADMED NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
           MOVE 1 TO IND
           MOVE ZEROS TO CRM NUMREG.
           START CADMED KEY IS NOT LESS CRM INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.

       LER-MED.           
           READ CADMED NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE IND TO NUMREG
                 ADD -1 TO NUMREG
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              MOVE CRM TO TBMED(IND)
              ADD 1 TO IND              
              IF IND > 1000
                 MOVE "*** TABELA ESTOURADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 GO TO LER-MED.

      *
       INC-001.
            MOVE SPACES TO NOME EMAIL SEXO
            MOVE ZEROS TO CRM ESPECIALIDADE DIA MES ANO TELEFONE DDD
            DISPLAY  SMT060.
       INC-001A.
           ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADMED
                      GO TO ROT-FIM.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM   
                      GO TO INC-001A.
           MOVE 1 TO IND.

       R111.
           IF TBMED(IND) < CRM
              ADD 1 TO IND
              IF IND >1000
                 MOVE "*** MEDICO NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001A
              ELSE
                 GO TO R111.
       R112.
           MOVE TBMED(IND) TO CRM.

       INC-RD2.
           READ CADMED 
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "*** REGISTRO NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       ROT-MONTAR.
              DISPLAY SMT060.

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

           CLOSE CADMED.
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