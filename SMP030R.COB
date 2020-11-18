       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP030R
       AUTHOR. LUCAS GUILHERME LIAS
      ************************
      * RELATORIO DE MEDICO   *
      ************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                   WITH DUPLICATES.
           SELECT CADMEDTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT".
       01 REGMED.          
          03 CRM                   PIC 9(06).
          03 NOME              PIC X(30).
          03 ESPECIALIDADE         PIC 9(02).
          03 SEXO            PIC X(01).
          03 NASC.   
                05 DIA             PIC 9(02).
                05 MES             PIC 9(02).
                05 ANO             PIC 9(04).
          03 EMAIL             PIC X(30).
          03 TELEFONE              PIC 9(09).
          03 DDD                   PIC 9(02).
      *
       FD CADMEDTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMEDRL.DOC".
       01 REGMEDTX    PIC X(150).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00". 
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.
       01 CONLIN     PIC 9(03) VALUE ZEROS.
       01 CONLIN1    PIC 9(04) VALUE ZEROS.
       01 CONPAG     PIC 9(03) VALUE ZEROS.
      *

       01  CAB1.
           05  FILLER                 PIC X(044) VALUE
           "﻿FATEC - ZONA LESTE  *** RELACAO DE MEDICO".

           05  FILLER                 PIC X(024) VALUE
           "S ***          PAG. : ".

           05  NUMPAG   VALUE ZEROS  PIC 999.

           05  FILLER                 PIC X(012) VALUE
           "    DATA : ".

           05  DATAREL  VALUE ZEROS  PIC 99.99.99.
           
         01  CAB2.
           05  FILLER                 PIC X(057) VALUE
           "CRR      NOME                   ESPECIALIDADE    SEXO    ".
           05  FILLER                 PIC X(040) VALUE
           "   NASC      EMAIL       DDD    TELEFONE".

       01  CAB3.
           05  FILLER                 PIC X(057) VALUE
           "---------    ------------------------------    ----------".
           05  FILLER                 PIC X(040) VALUE
           "----------    --------------------    --".

       01  DET.
           05  CRMTX  VALUE ZEROS  PIC 999999.
           05  FILLER                 PIC X(004) VALUE 
           "    ".
           05  NOMETX   VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(004) VALUE
           "    ".
           05  ESPECIALIDADETX  VALUE ZEROS  PIC 99.
           05  FILLER                 PIC X(004) VALUE
           "    ".
           05  SEXOTX  VALUE SPACES PIC X(002).
           05  FILLER                 PIC X(004) VALUE
           "    ".
           05  NASCTX  VALUE ZEROS  PIC 99.99.9999.
           05  FILLER                 PIC X(004) VALUE 
           "    ".
           05  EMAILTX   VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(004) VALUE
           "    ".
           05  DDDTX  VALUE ZEROS  PIC 99.
           05  FILLER                 PIC X(004) VALUE
           "    ".

           05  TELEFONETX  VALUE ZEROS  PIC 999999999.
           05  FILLER                 PIC X(004) VALUE
           "    ".

       01  LINTOT.
           05  FILLER                 PIC X(051) VALUE
           "                          *** TOTAL DE REGISTROS : ".
           05  TOTREG   VALUE ZEROS  PIC ZZZ9.
           05  FILLER                 PIC X(004) VALUE
           " ***".

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMT025.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE MED PARA TXT ***".
           05  LINE 12  COLUMN 01 
               VALUE  "                         CONFIRMA IMPRES".
           05  LINE 12  COLUMN 41 
               VALUE  "SAO (S ou N ) :".
           05  LINE 24  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TW-OPCAO
               LINE 12  COLUMN 57  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO. 
      *
       INC-001.
              MOVE SPACES TO NOME EMAIL SEXO
              MOVE ZEROS TO CRM ESPECIALIDADE DIA MES ANO TELEFONE DDD
                DISPLAY SMT025.
       INC-OPC.
                ACCEPT TW-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
      *
       INC-OP0.
           OPEN INPUT CADMED
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO MED NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO MED" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT CADMEDTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADMEDTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
       ROT-CABEC. 
           ADD 1 TO CONPAG
           MOVE CONPAG TO NUMPAG
           WRITE REGMEDTX FROM CAB1.
           MOVE SPACES TO REGMEDTX
           WRITE REGMEDTX.
           WRITE REGMEDTX FROM CAB2.
           WRITE REGMEDTX FROM CAB3.
      *
       LER-MED01.
                READ CADMED NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE SPACES TO REGMEDTX
                      WRITE REGMEDTX
                      MOVE CONLIN1 TO TOTREG
                      WRITE REGMEDTX FROM LINTOT                     
                      MOVE "*** FIM DO CADMED ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA CADMED"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
           MOVE CRM TO CRMTX.
           MOVE NOME TO NOMETX.
           MOVE ESPECIALIDADE TO ESPECIALIDADETX.
           MOVE SEXO TO SEXOTX.
           MOVE NASC TO NASCTX.
           MOVE EMAIL TO EMAILTX.
           MOVE DDD TO DDDTX.
           MOVE TELEFONE TO TELEFONETX.


       INC-WR1.
           WRITE REGMEDTX FROM DET
                IF ST-ERRO NOT = "00"
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADMEDTX"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
                MOVE "*** REGISTRO GRAVADOS *** " TO MENS
                ADD 1 TO CONLIN CONLIN1
                IF CONLIN < 34
                      GO TO LER-MED01
                ELSE
                      MOVE ZEROS TO CONLIN
                      GO TO ROT-CABEC.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                CLOSE CADMED CADMEDTX.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
