       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HELLO-WORLD.
       AUTHOR.        GERRIT GIEHL.
       DATE-WRITTEN.  02/01/2014.
       DATE-COMPILED.

      *******************************************************************
      *                
      *
      *******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 
      * FILE-CONTROL.
      *   SELECT DATA-FILE-1
      *   ASSIGN TO "TEST.DAT"
      *   ORGANIZATION IS SEQUENTIAL
      *   ACCESS MODE IS SEQUENTIAL.
       
       DATA DIVISION.
      * FILE SECTION.
      * FD  DATA-FILE-1.
      *   01  RECORD-1.
      *    05  FIRST-NAME PIC X(25).
      *    05  LAST-NAME  PIC X(25).
       
       WORKING-STORAGE SECTION.
       01 BLACK   CONSTANT AS 0.
       01 BLUE    CONSTANT AS 1.
       01 GREEN   CONSTANT AS 2.
       01 CYAN    CONSTANT AS 3.
       01 RED     CONSTANT AS 4.
       01 MAGENTA CONSTANT AS 5.
       01 BROWN   CONSTANT AS 6.
       01 WHITE   CONSTANT AS 7.

       01 ENTRY-FIELD-COLOR CONSTANT AS RED.

       01  WS-STORED-AREAS.       
        05  FIRST-NAME PIC X(25).
        05  LAST-NAME  PIC X(25).
       
       01  WS-A-SATZ.
        05  SATZLAENGE                 PIC 9(4).
        05  SATZART                    PIC A(1)     VALUE "A".
        05  KENNZEICHEN                PIC A(2).
        05  BLZ-DATEIEMPFAENGER        PIC 9(8).
        05  BLZ-ABSENDERBANK           PIC 9(8).
        05  NAME-ABSENDER              PIC A(27).
        05  DATEIERSTELLUNGSDATUM      PIC 9(6).
        05  LEERZEICHEN                PIC A(4)     VALUE "    ".
        05  KONTONUMMER-ABSENDER       PIC X(10).
        05  SAMMEL-REF-NR              PIC 9(10)    VALUE 0000000000.
        05  LEERZEICHEN                PIC X(47).
      * WAEHRUNG VALUE "1" == EUR
        05  WAEHRUNG                   PIC X(1)     VALUE "1".

       01  WS-C-SATZ.
        05  SATZLAENGE                 PIC 9(4).
        05  SATZART                    PIC A(1)     VALUE "C".
        05  BLZ                        PIC 9(8)     VALUE 00000000.
        05  BLZ-BEGUENSTIGTER          PIC 9(8).
        05  KTO-BEGUENSTIGTER          PIC 9(10).
        05  INTERNE-KND-NR             PIC 9(13).
        05  TEXTSCHLUESSEL             PIC 9(2).
        05  TEXTSCHLUESSEL-ERGAENZUNG  PIC 9(3).
        05  LEERZEICHEN                PIC X(1)     VALUE " ".
        05  NULLEN                     PIC 9(11)    VALUE 00000000000.
        05  BLZ-AUFTRAGGEBER           PIC 9(8).
        05  KTO-AUFTRAGGEBER           PIC 9(10).
        05  BETRAG                     PIC 9(11).
        05  LEERZEICHEN                PIC A(3)     VALUE "   ".
        05  NAME-BEGUENSTIGTER         PIC A(27).
        05  LEERZEICHEN                PIC A(8)     VALUE "        ".
        05  NAME-AUFTRAGGEBER          PIC A(27).
        05  VERWENDUNGSZWECK           PIC X(27).
      * WAEHRUNG VALUE "1" == EUR
        05  WAEHRUNG                   PIC X(1)     VALUE "1".
        05  LEERZEICHEN                PIC A(2)     VALUE "  ".
        05  ZWEISTELLIGE-ANZAHL        PIC X(2).
        05  ERWEITERUNGSTEILE          PIC X(58).
        05  LEERZEICHEN                PIC A(11)    VALUE "           ".

       01  WS-E-SATZ.
        05  SATZLAENGE                 PIC 9(4).
        05  SATZART                    PIC A(1)     VALUE "E".
        05  LEERZEICHEN                PIC A(5)     VALUE "     ".
        05  ANZAHL-DATENSAETZE         PIC 9(7).
        05  EHEMALS-SUMME-DM           PIC 9(13)    VALUE 0000000000000.
        05  SUMME-KONTONUMMERN         PIC 9(17).
        05  SUMME-BLZ                  PIC 9(17).
        05  SUMME-EUR-BETRAEGE         PIC 9(13).
        05  LEERZEICHEN                PIC A(51).

       SCREEN SECTION.
       01  BLANK-SCREEN.
        05  FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.
    
       01  START-SCREEN.
        05  BLANK SCREEN BACKGROUND-COLOR BLACK FOREGROUND-COLOR WHITE.
        05  VALUE "DATA ENTRY SCREEN" BLANK SCREEN     LINE 1 COL 35.
        05  VALUE "ID #"                               LINE 3 COL 10.
        05  VALUE "C - TO CONTINUE"                    LINE 11 COL 30.
        05  VALUE "Q - TO QUIT"                        LINE 12 COL 30.
        05  VALUE "ENTER RESPONSE"                     LINE 14 COL 30.

       01  A-SATZ-SCREEN.
        05  VALUE "KENNZEICHEN:"                          LINE 2 COL 5.
        05  SCREEN-KENNZEICHEN PIC A(2)
                   USING KENNZEICHEN IN WS-A-SATZ         LINE 2 COL 28
                   FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "BLZ DATEIEMPFAENGER:"                  LINE 3 COL 5.
        05  SCREEN-BLZ-DATEIEMPFAENGER PIC 9(8) 
                   USING BLZ-DATEIEMPFAENGER IN WS-A-SATZ LINE 3 COL 28
                   FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "BLZ ABSENDERBANK:"                     LINE 4 COL 5.
        05  SCREEN-BLZ-ABSENDERBANK PIC 9(8)
                   USING BLZ-ABSENDERBANK IN WS-A-SATZ    LINE 4 COL 28
                   FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "NAME ABSENDER:"                        LINE 5 COL 5.
        05  SCREEN-NAME-ABSENDER PIC 9(8)
                   USING NAME-ABSENDER IN WS-A-SATZ       LINE 5 COL 28
                   FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "DATEIERSTELLUNGSDATUM:"                LINE 2 COL 45.
        05  SCREEN-DATEIERSTELLUNGSDATUM PIC 9(6)
                 USING DATEIERSTELLUNGSDATUM IN WS-A-SATZ LINE 2 COL 70
                 FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "KONTONUMMER ABSENDER:"                 LINE 3 COL 45.
        05  SCREEN-KONTONUMMER-ABSENDER PIC 9(10)
                  USING KONTONUMMER-ABSENDER IN WS-A-SATZ LINE 3 COL 70 
                  FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "SAMMELREFERENZNUMMER:"                 LINE 4 COL 45.
        05  SCREEN-SAMMEL-REF-NR PIC 9(10)
                 USING SAMMEL-REF-NR IN WS-A-SATZ         LINE 4 COL 70
                 FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "WAEHRUNG:"                             LINE 5 COL 45.
        05  SCREEN-WAEHRUNG PIC A(1)
                 USING WAEHRUNG IN WS-A-SATZ              LINE 5 COL 70
                 FOREGROUND-COLOR ENTRY-FIELD-COLOR.

       01  C-SATZ-SCREEN.
        05  VALUE "BLZ:"                                 LINE  8 COL  5.
        05  SCREEN-BLZ PIC 9(8)
                 USING BLZ IN WS-C-SATZ                  LINE  8 COL 35
                 FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "BLZ BEGUENSTIGTER:"                   LINE  9 COL 5.
        05  SCREEN-BLZ-BEGUENSTIGTER PIC 9(8)
                 USING BLZ-BEGUENSTIGTER IN WS-C-SATZ    LINE  9 COL 35
                 FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "KTO-NR. BEGUENSTIGER:"                LINE 10 COL 5.
        05  SCREEN-KTO-BEGUENSTIGTER PIC 9(10)
                 USING KTO-BEGUENSTIGTER IN WS-C-SATZ    LINE 10 COL 35
                 FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "INTERNE KUNDENNUMMER:"                LINE 11 COL 5.
        05  SCREEN-INTENRNE-KND-NR PIC 9(13)
                 USING INTERNE-KND-NR IN WS-C-SATZ       LINE 11 COL 35
                 FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "TEXTSCHLUESSEL:"                      LINE 12 COL 5.
        05  SCREEN-TEXTSCHLUESSEL PIC 9(2)
                 USING TEXTSCHLUESSEL IN WS-C-SATZ       LINE 12 COL 35
                 FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "TEXTSCHLUESSEL ERGAENZUNG:"           LINE 13 COL 5.
        05  SCREEN-TEXTSCHLUESSEL-ERG PIC X(3)
          USING TEXTSCHLUESSEL-ERGAENZUNG IN WS-C-SATZ   LINE 13 COL 35
          FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "BLZ AUFTRAGGEBER:"                    LINE 14 COL 5.
        05  SCREEN-BLZ-AUFTRAGGEBER PIC 9(8)
                  USING BLZ-AUFTRAGGEBER IN WS-C-SATZ    LINE 14 COL 35
                  FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "NAME BEGUENSTIGTER:"                  LINE 15 COL 5.
        05  SCREEN-NAME-BEGUENSTIGTER PIC A(27)
                  USING NAME-BEGUENSTIGTER IN WS-C-SATZ  LINE 15 COL 35
                  FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "NAME AUFTRAGGEBER:"                   LINE 16 COL 5.
        05  SCREEN-NAME-AUFTRAGGEBER PIC A(27)
                  USING NAME-AUFTRAGGEBER IN WS-C-SATZ   LINE 16 COL 35
                  FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "VERWENDUNGSZWECK:"                    LINE 17 COL 5.
        05  SCREEN-VERWENDUNGSZWECK PIC X(27)
                  USING VERWENDUNGSZWECK IN WS-C-SATZ    LINE 17 COL 35
                  FOREGROUND-COLOR ENTRY-FIELD-COLOR.
        05  VALUE "WAEHRUNG:"                            LINE 18 COL 5.
        05  SCREEN-WAEHRUNG PIC X(1)
                  USING WAEHRUNG IN WS-C-SATZ            LINE 18 COL 35
                  FOREGROUND-COLOR ENTRY-FIELD-COLOR.

       PROCEDURE DIVISION.
       DISPLAY BLANK-SCREEN.
       
      * PERFORM UNTIL START-SCREEN NOT EQUAL "Q"
      *   DISPLAY START-SCREEN
      *   ACCEPT START-SCREEN
      * END-PERFORM.
      * STOP RUN.
 
       SCREEN-LOOP.
         DISPLAY A-SATZ-SCREEN.
         DISPLAY C-SATZ-SCREEN.
         ACCEPT C-SATZ-SCREEN.

         IF FIRST-NAME IN WS-STORED-AREAS(1:1) NOT EQUAL "Q"
           GO TO SCREEN-LOOP.
         ELSE-IF.
    
       STOP RUN.
      * GOBACK.
      * END PROGRAM HELLO-WORLD.


       