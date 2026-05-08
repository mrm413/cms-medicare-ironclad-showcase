       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_hhcal213.

      ******************************************************************
      * FY2021 Home Health PPS Pricer parity test driver.
      * Calls HHCAL213 directly with all 6 LINKAGE records embedded as
      * WORKING-STORAGE.
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  HHA-INPUT-DATA.
           05  HHA-DATA.
               10  HHA-NPI                 PIC X(10).
               10  HHA-HIC                 PIC X(12).
               10  HHA-PROV-NO             PIC X(06).
               10  HHA-INIT-PAY-QRP-IND    PIC X(01).
               10  HHA-PROV-VBP-ADJ-FAC    PIC 9V9(5).
               10  HHA-PROV-OUTLIER-PAY-TOTAL PIC 9(08)V9(02).
               10  HHA-PROV-PAYMENT-TOTAL  PIC 9(09)V9(02).
               10  HHA-TOB                 PIC X(03).
               10  HHA-CBSA                PIC X(05).
               10  HHA-COUNTY-CODE         PIC X(05).
               10  HHA-SERV-FROM-DATE.
                   15  HHA-FROM-CC         PIC XX.
                   15  HHA-FROM-YYMMDD.
                       25  HHA-FROM-YY     PIC XX.
                       25  HHA-FROM-MM     PIC XX.
                       25  HHA-FROM-DD     PIC XX.
               10  HHA-SERV-THRU-DATE.
                   15  HHA-THRU-CC         PIC XX.
                   15  HHA-THRU-YYMMDD.
                       25  HHA-THRU-YY     PIC XX.
                       25  HHA-THRU-MM     PIC XX.
                       25  HHA-THRU-DD     PIC XX.
               10  HHA-ADMIT-DATE.
                   15  HHA-ADMIT-CC        PIC XX.
                   15  HHA-ADMIT-YYMMDD.
                       25  HHA-ADMIT-YY    PIC XX.
                       25  HHA-ADMIT-MM    PIC XX.
                       25  HHA-ADMIT-DD    PIC XX.
               10  HHA-LUPA-SRC-ADM        PIC X.
               10  HHA-ADJ-IND             PIC X.
               10  HHA-PEP-IND             PIC X.
               10  HHA-HRG-INPUT-CODE      PIC X(05).
               10  HHA-HRG-NO-OF-DAYS      PIC 9(03).
               10  HHA-HRG-WGTS            PIC 9(02)V9(04).
               10  HHA-HRG-PAY             PIC 9(07)V9(02).
               10  HHA-REVENUE-DATA        OCCURS 6.
                   15  HHA-REVENUE-CODE              PIC X(04).
                   15  HHA-REVENUE-QTY-COV-VISITS    PIC 9(03).
                   15  HHA-REVENUE-QTY-OUTL-UNITS    PIC 9(05).
                   15  HHA-REVENUE-EARLIEST-DATE     PIC 9(08).
                   15  HHA-REVENUE-DOLL-RATE         PIC 9(07)V9(02).
                   15  HHA-REVENUE-COST              PIC 9(07)V9(02).
                   15  HHA-REVENUE-ADD-ON-VISIT-AMT  PIC 9(07)V9(02).
               10  HHA-PAY-RTC                PIC 99.
               10  HHA-REVENUE-SUM1-6-QTY-ALL PIC 9(05).
               10  HHA-OUTLIER-PAYMENT        PIC 9(07)V9(02).
               10  HHA-TOTAL-PAYMENT          PIC 9(07)V9(02).
               10  HHA-VBP-ADJ-AMT            PIC S9(7)V99.
               10  HHA-PPS-STD-VALUE          PIC 9(7)V99.
               10  HHA-RECEIPT-DATE           PIC X(8).
               10  HHA-OVERRIDE-IND           PIC X(1).
               10  HHA-LATE-SUB-PEN-AMT       PIC 9(7)V9(2).
               10  FILLER                     PIC X(188).

       01  HOLD-VARIABLES-DATA.
           02  HOLD-VAR-DATA.
               05  PRICER-OPTION-SW                   PIC X(01).
               05  HHOPN-VERSION                      PIC X(07).
               05  HHDRV-VERSION                      PIC X(07).
               05  HHCAL-VERSION                      PIC X(07).
               05  FILLER                             PIC X(20).

       01  CBSA-WAGE-INDEX-DATA.
           02  HOLD-WIR-DATA.
               05  WIR-CBSA                       PIC X(05).
               05  WIR-CBSA-EFFDATE               PIC X(08).
               05  WIR-CBSA-WAGEIND               PIC 9(02)V9(04).

       01  TB-REV-DOLL-RATE-UNITS.
           05 WK-REV-DOLL-RATE-UNITS  PIC 9(07)V9(02)
                                      OCCURS 6.

       01  TB-STDV-DATA.
           05 TB-STDV-TABLE           OCCURS 6.
              10  TB-STDV-REV-CODE           PIC X(04).
              10  TB-STDV-REV-DOLL-RATE      PIC 9(07)V9(02).

       01  L-HRG-THRESHOLD       PIC 9(01).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           INITIALIZE HHA-INPUT-DATA
           INITIALIZE HOLD-VARIABLES-DATA
           INITIALIZE CBSA-WAGE-INDEX-DATA
           INITIALIZE TB-REV-DOLL-RATE-UNITS
           INITIALIZE TB-STDV-DATA
           MOVE 0 TO L-HRG-THRESHOLD

      *    Realistic FY2021 Home Health claim
           MOVE '1234567890' TO HHA-NPI
           MOVE '123456789012' TO HHA-HIC
           MOVE '123456'     TO HHA-PROV-NO
           MOVE '0'          TO HHA-INIT-PAY-QRP-IND
           MOVE 1.00000      TO HHA-PROV-VBP-ADJ-FAC
           MOVE '329'        TO HHA-TOB
           MOVE '11260'      TO HHA-CBSA
           MOVE '11001'      TO HHA-COUNTY-CODE
           MOVE '20' TO HHA-FROM-CC
           MOVE '21' TO HHA-FROM-YY
           MOVE '06' TO HHA-FROM-MM
           MOVE '01' TO HHA-FROM-DD
           MOVE '20' TO HHA-THRU-CC
           MOVE '21' TO HHA-THRU-YY
           MOVE '06' TO HHA-THRU-MM
           MOVE '30' TO HHA-THRU-DD
           MOVE '20' TO HHA-ADMIT-CC
           MOVE '21' TO HHA-ADMIT-YY
           MOVE '06' TO HHA-ADMIT-MM
           MOVE '01' TO HHA-ADMIT-DD
           MOVE 'B' TO HHA-LUPA-SRC-ADM
           MOVE 'N' TO HHA-ADJ-IND
           MOVE 'N' TO HHA-PEP-IND
           MOVE '1AA11' TO HHA-HRG-INPUT-CODE

      *    CBSA wage index for 11260
           MOVE '11260'    TO WIR-CBSA
           MOVE '20210101' TO WIR-CBSA-EFFDATE
           MOVE 1.0000     TO WIR-CBSA-WAGEIND

      *    Standard rev table
           MOVE '0421'  TO TB-STDV-REV-CODE(1)
           MOVE 0166.83 TO TB-STDV-REV-DOLL-RATE(1)

           CALL 'HHCAL213' USING HHA-INPUT-DATA
                                  HOLD-VARIABLES-DATA
                                  CBSA-WAGE-INDEX-DATA
                                  TB-REV-DOLL-RATE-UNITS
                                  TB-STDV-DATA
                                  L-HRG-THRESHOLD

           DISPLAY 'HHA-PAY-RTC=[' HHA-PAY-RTC ']'
           DISPLAY 'HHA-TOTAL-PAYMENT=' HHA-TOTAL-PAYMENT
           DISPLAY 'HHA-OUTLIER-PAYMENT=' HHA-OUTLIER-PAYMENT
           DISPLAY 'HHA-VBP-ADJ-AMT=' HHA-VBP-ADJ-AMT
           DISPLAY 'HHA-PPS-STD-VALUE=' HHA-PPS-STD-VALUE
           DISPLAY 'HHCAL-VERSION=[' HHCAL-VERSION ']'
           STOP RUN.
