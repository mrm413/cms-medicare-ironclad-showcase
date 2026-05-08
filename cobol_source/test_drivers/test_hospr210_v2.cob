       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_hospr210_v2.

      ******************************************************************
      * 2nd Hospice path: Continuous Home Care (TOB 32G), urban CBSA.
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  BILL-315-DATA.
           10  BILL-NPI                PIC X(10).
           10  BILL-PROV-NO            PIC X(06).
           10  BILL-FROM-DATE.
               15  BILL-FROM-CC        PIC 99.
               15  BILL-FROM-YY        PIC 99.
               15  BILL-FROM-MM        PIC 99.
               15  BILL-FROM-DD        PIC 99.
           10  BILL-ADMISSION-DATE.
               15  BILL-ADM-CC         PIC 99.
               15  BILL-ADM-YY         PIC 99.
               15  BILL-ADM-MM         PIC 99.
               15  BILL-ADM-DD         PIC 99.
           10  FILLER                  PIC X(10).
           10  BILL-PROV-MSA-LUGAR.
               15  BILL-PROV-MSA       PIC X(04).
               15  BILL-PROV-LUGAR     PIC X.
           10  BILL-BENE-MSA-LUGAR.
               15 BILL-BENE-MSA        PIC X(04).
               15 BILL-BENE-LUGAR      PIC X.
           10  BILL-PROV-WAGE-INDEX    PIC 9(02)V9(04).
           10  BILL-BENE-WAGE-INDEX    PIC 9(02)V9(04).
           10  BILL-SIA-ADD-ON-UNITS.
               15  BILL-NA-ADD-ON-DAY1-UNITS   PIC 99.
               15  BILL-NA-ADD-ON-DAY2-UNITS   PIC 99.
               15  BILL-EOL-ADD-ON-DAY1-UNITS  PIC 99.
               15  BILL-EOL-ADD-ON-DAY2-UNITS  PIC 99.
               15  BILL-EOL-ADD-ON-DAY3-UNITS  PIC 99.
               15  BILL-EOL-ADD-ON-DAY4-UNITS  PIC 99.
               15  BILL-EOL-ADD-ON-DAY5-UNITS  PIC 99.
               15  BILL-EOL-ADD-ON-DAY6-UNITS  PIC 99.
               15  BILL-EOL-ADD-ON-DAY7-UNITS  PIC 99.
           10  FILLER                  PIC X(10).
           10  BILL-QIP-IND            PIC X.
           10  BILL-GROUP1.
               15  BILL-REV1           PIC XXXX.
               15  BILL-HCPC1          PIC X(05).
               15  BILL-LINE-ITEM-DOS1.
                   20  BILL-LIDOS1-CC  PIC 99.
                   20  BILL-LIDOS1-YY  PIC 99.
                   20  BILL-LIDOS1-MM  PIC 99.
                   20  BILL-LIDOS1-DD  PIC 99.
               15  BILL-UNITS1         PIC 9(07).
               15  BILL-PAY-AMT1       PIC 9(06)V99.
           10  BILL-GROUP2.
               15  BILL-REV2           PIC XXXX.
               15  BILL-HCPC2          PIC X(05).
               15  BILL-LINE-ITEM-DOS2.
                   20  BILL-LIDOS2-CC  PIC 99.
                   20  BILL-LIDOS2-YY  PIC 99.
                   20  BILL-LIDOS2-MM  PIC 99.
                   20  BILL-LIDOS2-DD  PIC 99.
               15  BILL-UNITS2         PIC 9(07).
               15  BILL-PAY-AMT2       PIC 9(06)V99.
           10  BILL-GROUP3.
               15  BILL-REV3           PIC XXXX.
               15  BILL-HCPC3          PIC X(05).
               15  BILL-LINE-ITEM-DOS3.
                   20  BILL-LIDOS3-CC  PIC 99.
                   20  BILL-LIDOS3-YY  PIC 99.
                   20  BILL-LIDOS3-MM  PIC 99.
                   20  BILL-LIDOS3-DD  PIC 99.
               15  BILL-UNITS3         PIC 9(07).
               15  BILL-PAY-AMT3       PIC 9(06)V99.
           10  BILL-GROUP4.
               15  BILL-REV4           PIC XXXX.
               15  BILL-HCPC4          PIC X(05).
               15  BILL-LINE-ITEM-DOS4.
                   20  BILL-LIDOS4-CC  PIC 99.
                   20  BILL-LIDOS4-YY  PIC 99.
                   20  BILL-LIDOS4-MM  PIC 99.
                   20  BILL-LIDOS4-DD  PIC 99.
               15  BILL-UNITS4         PIC 9(07).
               15  BILL-PAY-AMT4       PIC 9(06)V99.
           10  BILL-SIA-ADD-ON-PYMTS.
               15  BILL-NA-ADD-ON-DAY1-PAY   PIC 9(06)V99.
               15  BILL-NA-ADD-ON-DAY2-PAY   PIC 9(06)V99.
               15  BILL-EOL-ADD-ON-DAY1-PAY  PIC 9(06)V99.
               15  BILL-EOL-ADD-ON-DAY2-PAY  PIC 9(06)V99.
               15  BILL-EOL-ADD-ON-DAY3-PAY  PIC 9(06)V99.
               15  BILL-EOL-ADD-ON-DAY4-PAY  PIC 9(06)V99.
               15  BILL-EOL-ADD-ON-DAY5-PAY  PIC 9(06)V99.
               15  BILL-EOL-ADD-ON-DAY6-PAY  PIC 9(06)V99.
               15  BILL-EOL-ADD-ON-DAY7-PAY  PIC 9(06)V99.
           10  BILL-RETURNED-DATA.
               15  BILL-PAY-AMT-TOTAL        PIC 9(06)V99.
               15  BILL-RTC                  PIC XX.
           10  BILL-RHC-DAYS-PAID.
               15  BILL-HIGH-RHC-DAYS        PIC 99.
               15  BILL-LOW-RHC-DAYS         PIC 99.
           10  FILLER                        PIC X(08).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           INITIALIZE BILL-315-DATA

      *    Continuous Home Care (TOB 32G) — different code path
           MOVE '9876543210' TO BILL-NPI
           MOVE '654321'     TO BILL-PROV-NO
           MOVE 20  TO BILL-FROM-CC
           MOVE 21  TO BILL-FROM-YY
           MOVE 03  TO BILL-FROM-MM
           MOVE 01  TO BILL-FROM-DD
           MOVE 20  TO BILL-ADM-CC
           MOVE 20  TO BILL-ADM-YY
           MOVE 12  TO BILL-ADM-MM
           MOVE 15  TO BILL-ADM-DD
           MOVE '14454' TO BILL-PROV-MSA-LUGAR
           MOVE '14454' TO BILL-BENE-MSA-LUGAR
           MOVE 1     TO BILL-PROV-WAGE-INDEX
           MOVE 1     TO BILL-BENE-WAGE-INDEX
           MOVE 'Y'   TO BILL-QIP-IND
           MOVE '0652' TO BILL-REV1
           MOVE '24'   TO BILL-UNITS1

           CALL 'HOSPR210' USING BILL-315-DATA

           DISPLAY 'BILL-RTC=[' BILL-RTC ']'
           DISPLAY 'BILL-PAY-AMT-TOTAL=' BILL-PAY-AMT-TOTAL
           DISPLAY 'BILL-PAY-AMT1=' BILL-PAY-AMT1
           DISPLAY 'BILL-HIGH-RHC-DAYS=' BILL-HIGH-RHC-DAYS
           DISPLAY 'BILL-LOW-RHC-DAYS=' BILL-LOW-RHC-DAYS
           STOP RUN.
