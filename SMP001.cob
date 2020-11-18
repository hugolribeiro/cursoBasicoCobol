       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP001.
      *AUTHOR. LUCAS GUILHERME LIAS.
      **************************************
      * CADASTRO DO MEDICO   *
      **************************************
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
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT". 

       01 REGMED.
              
          03 CRM                   PIC 9(06).

       	  03 NOME         	   PIC X(30).

          03 ESPECIALIDADE         PIC 9(02).

          03 SEXO	           PIC X(01).

          03 NASC.   
                05 DIA             PIC 9(02).
                05 MES             PIC 9(02).
                05 ANO             PIC 9(04).

       	  03 EMAIL         	   PIC X(30).

          03 TELEFONE              PIC 9(09).
          03 DDD                   PIC 9(02).


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

       01 TABESPECIALIDADE.
          03 FILLER     PIC X(30) VALUE "CLINICA MEDICA".
          03 FILLER     PIC X(30) VALUE "UROLOGIA".
          03 FILLER     PIC X(30) VALUE "GINICOLOGISTA".
          03 FILLER     PIC X(30) VALUE "PEDIATRIA".
          03 FILLER     PIC X(30) VALUE "CARDIOLOGISTA".
          03 FILLER     PIC X(30) VALUE "GERIATRA".
          03 FILLER     PIC X(30) VALUE "NEUROLOGISTA".
          03 FILLER     PIC X(30) VALUE "OTORRINOLARINGOLOGISTA".
          03 FILLER     PIC X(30) VALUE "OCULISTA".

       01 TABEESPECIALIDADE REDEFINES TABESPECIALIDADE.
          05 TBEESPECIALIDADE  PIC X(30) OCCURS 9 TIMES.
  
       01 TABSEXO.
          03 FILLER     PIC X(15) VALUE "MMASCULINO".
          03 FILLER     PIC X(15) VALUE "FFEMININO".

       01 TABELASEXO REDEFINES TABSEXO.
          05 TBESEXO   PIC X(15) OCCURS 2 TIMES.

       01 TXTESPECIALIDADE    PIC X(30) VALUE SPACES.

       01 TXTSEXO.
          03 TXTSEXO1 PIC X(01) VALUE SPACES.
          03 TXTSEXO2 PIC X(14) VALUE SPACES. 



      *-----------------------------------------------------------------
	SCREEN SECTION.

       01  TELA1.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "Cadastro De Medico".
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

           05 TTXTESPECIALIDADE
               LINE 08  COLUMN 21  PIC X(30)
               USING  TXTESPECIALIDADE
               HIGHLIGHT.

           05  TSEXO
               LINE 10  COLUMN 07  PIC X(01)
               USING SEXO
               HIGHLIGHT.

          05  TTXTSEXO
               LINE 10  COLUMN 09  PIC X(15)
               USING  TXTSEXO2
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

       01  OPCTABELESPECIALIDADE.
           05  LINE 08  COLUMN 41 VALUE "01- CLINICA MEDICA".
           05  LINE 09  COLUMN 41 VALUE "02- UROLOGIA".
           05  LINE 10  COLUMN 41 VALUE "03- GINICOLOGISTA".
           05  LINE 11  COLUMN 41 VALUE "04- PEDIATRIA".
           05  LINE 12  COLUMN 41 VALUE "05- CARDIOLOGISTA".
           05  LINE 13  COLUMN 41 VALUE "06- GERIATRA".
           05  LINE 14  COLUMN 41 VALUE "07- NEUROLOGISTA".
           05  LINE 15  COLUMN 41 VALUE "08- OTORRINOLARINGOLOGISTA".
           05  LINE 16  COLUMN 41 VALUE "09- OCULISTA".

       01  OPCTABELASEXO.
           05  LINE 08  COLUMN 41 VALUE "M - MASCULINO".
           05  LINE 09  COLUMN 41 VALUE "F - FEMININO".
           

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

       INICIO.

       R0.
           OPEN I-O CADMED.
           IF ST-ERRO NOT = "00"
             IF ST-ERRO = "30"
                 OPEN OUTPUT CADMED
                 CLOSE CADMED
                 MOVE "*** ARQUIVO CADMED FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
             ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMED" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
       R1.
            MOVE SPACES TO TXTESPECIALIDADE TXTSEXO NOME EMAIL SEXO
            MOVE ZEROS TO CRM ESPECIALIDADE DIA MES ANO TELEFONE DDD
            DISPLAY TELA1.
           
       R2.
           ACCEPT TCRM

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.

           IF CRM = ZEROS
                GO TO R2.

       LER-CADMED.
           READ CADMED
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R4A
                PERFORM R5A
                DISPLAY TELA1
                MOVE "*** CRM JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADMED" TO MENS
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
           DISPLAY OPCTABELESPECIALIDADE.
           ACCEPT TESPECIALIDADE

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.

           IF ESPECIALIDADE = 00
                MOVE "DIGITE APENAS DE 01 ATE 09 " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R4.

           IF ESPECIALIDADE > 09
                MOVE "DIGITE APENAS DE 01 ATE 09" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R4.

       R4A.
            MOVE TBEESPECIALIDADE(ESPECIALIDADE) TO TXTESPECIALIDADE.
            DISPLAY TTXTESPECIALIDADE.
            DISPLAY TELA1.
           
       R5.
           DISPLAY OPCTABELASEXO.
           MOVE 1 TO IND
           ACCEPT TSEXO.

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.

       R5A.
           MOVE TBESEXO(IND) TO TXTSEXO.

           IF TXTSEXO1 NOT = SEXO
             ADD 1 TO IND
             IF IND < 3
                 GO TO R5A
             ELSE
                MOVE "*** TIPO DE SEXO INCORRETO***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R5
           ELSE
                DISPLAY TTXTSEXO. 
                DISPLAY TELA1.

       R6.
           ACCEPT TDIA 

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.

           IF DIA = ZEROS
                GO TO R6.
           IF DIA > 31
                MOVE "DIA INVALIDO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R6.
       R6A.
           ACCEPT TMES 

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.

           IF MES = ZEROS
                GO TO R6A.
           IF MES > 12
                MOVE "MES INVALIDO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R6A.

       R6B.
           ACCEPT TANO 

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6A.

           IF ANO = ZEROS
                GO TO R6B.
           IF ANO > 2020
                MOVE "ANO INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R6B.

       R7.
           ACCEPT TEMAIL 

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6B.

           IF EMAIL = SPACES
                GO TO R7.
	      
       R8.
           ACCEPT TDDD

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.

           IF DDD = ZEROS
                GO TO R8.

       R8A.
           ACCEPT TTELEFONE 

           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.

           IF TELEFONE = ZEROS
                GO TO R8A.

       INC-OPC.
           MOVE "S" TO W-OPCAO
           DISPLAY (23, 40) "DADOS OK (S/N) : ".
           ACCEPT (23, 57) W-OPCAO WITH UPDATE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 GO TO R7.
           IF W-OPCAO = "N" OR "n"
           MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1.
           IF W-OPCAO NOT = "S" AND "s"
           MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INC-OPC.
       INC-WR1.
           WRITE REGMED
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
                DELETE CADMED RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO MEDICO EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO R8.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGMED
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO MEDICO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.



       ROT-FIM.
           CLOSE CADMED.
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

