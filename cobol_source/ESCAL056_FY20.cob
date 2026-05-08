       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCAL056.
      *AUTHORS.    CMS.
      *       EFFECTIVE APRIL 1, 2005
      ******************************************************************
      * 4/06/05 - Allow provider type '05' for pediatric hosp          *
      *         - To be effective with the next release                *
      *         - Changed in 0100-INITIAL-ROUTINE with provider        *
      *           type '40'                                            *
      * 1/01/06 - NEW CBSA TABLE FOR CY2006                            *
      *         - UPDATE RATES WITH 1.6% INCREASE                      *
      * 1/18/07 - THIS PROGRAM WAS ENHANCED IN ORDER TO VERIFY MORE    *
      *           INFORMATION (VIA THE MASTER DRIVER).  VERY CLOSE     *
      *           ATTENTION HAS BEEN PAID TO ENSURE THAT ALL VARIABLES *
      *           THAT CHANGE FROM YEAR TO YEAR CONTINUE TO HAVE THE   *
      *           SAME VALUES AS IN THE ORIGINAL VERSION.  NO CHANGES  *
      *           WERE MADE IN THE CORE LOGIC OF THE PROCEDURE DIVISION*
      *           WHERE THE ACTUAL CALCULATIONS OCCUR.  THOROUGH       *
      *           TESTING USING THE ORIGINAL SET OF TEST CASES WAS MADE*
      *           AND THERE ARE ABSOLUTELY NO DIFFERENCES IN CALCULATED*
      *           AMOUNTS.  THE ORIGINAL CODE THAT WAS SENT OUT TO THE *
      *           FISCAL INTERMEDIARIES IN 2006 REMAINS INTACT UNDER   *
      *           THE MU00 ACCOUNT.                                    *
      * 1/18/07 - MSA-CBSA BLEND PERCENT SET AT 100% MSA 0% CBSA       *
      *         - ADDITIONAL VARIABLES WERE CREATED IN ORDER TO MAKE   *
      *           CHANGING VALUES EASIER (IN WORKING STORAGE RATHER    *
      *           THAN IN THE PROCEDURE DIVISION)                      *
      *         - THIS PROGRAM NOW REFLECTS ENHANCEMENTS MADE SO THAT  *
      *           TESTING OF THE CODE DOES NOT REQUIRE COMMENTING      *
      *           IN/OUT LINES OF CODE.  IN ADDITION CALCULATED        *
      *           VARIABLES ARE NOW PASSED BACK WHEN TEST CASES ARE    *
      *           ENCOUNTERED IN ORDER FOR THE MASTER DRIVER TO PRINT  *
      *           MORE INFORMATION ABOUT WHAT WENT ON IN THIS PROGRAM  *
      ******************************************************************
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.            IBM-Z990.
       OBJECT-COMPUTER.            ITTY-BITTY-MACHINE-CORPORATION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
004200
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.
       01  W-STORAGE-REF                  PIC X(46)  VALUE
           'ESCAL056      - W O R K I N G   S T O R A G E'.
004900
       01  CAL-VERSION                    PIC X(05)  VALUE 'C05.6'.
005100
       01  DISPLAY-LINE-MEASUREMENT.
           05  FILLER                     PIC X(50) VALUE
               '....:...10....:...20....:...30....:...40....:...50'.
           05  FILLER                     PIC X(50) VALUE
               '....:...60....:...70....:...80....:...90....:..100'.
           05  FILLER                     PIC X(20) VALUE
               '....:..110....:..120'.
005900
       01  PRINT-LINE-MEASUREMENT.
           05  FILLER                     PIC X(51) VALUE
               'X....:...10....:...20....:...30....:...40....:...50'.
           05  FILLER                     PIC X(50) VALUE
               '....:...60....:...70....:...80....:...90....:..100'.
           05  FILLER                     PIC X(32) VALUE
               '....:..110....:..120....:..130..'.
006700
       01  HOLD-PPS-COMPONENTS.
           05  H-PYMT-AMT                 PIC 9(07)V9(02).
           05  H-WAGE-ADJ-PYMT-AMT        PIC 9(07)V9(02).
           05  H-FIXED-LOSS-AMT           PIC 9(07)V9(02).
           05  H-ESRD-FAC-RATE            PIC 9(07)V9(02).
           05  H-PATIENT-AGE              PIC 9(03).
           05  H-AGE-FACTOR               PIC 9(01)V9(03).
           05  H-BSA-FACTOR               PIC 9(01)V9(04).
           05  H-BMI-FACTOR               PIC 9(01)V9(04).
           05  H-BSA                      PIC 9(03)V9(04).
           05  H-BMI                      PIC 9(03)V9(04).
           05  H-DRUG-ADDON               PIC 9(01)V9(04).
008000
      *   THE FOLLOWING THREE VARIABLES WILL CHANGE FROM YEAR TO YEAR
       01  DRUG-ADDON                     PIC 9(01)V9(04) VALUE 1.0870.
008300
      *   THE NEXT TWO PERCENTAGES MUST ADD UP TO 1 (I.E. 100%)
      *   THEY WILL CONTINUE TO CHANGE UNTIL 2009 WHEN CBSA WILL BE 1.00
       01  MSA-BLEND-PCT                  PIC 9(01)V9(02) VALUE 1.00.
       01  CBSA-BLEND-PCT                 PIC 9(01)V9(02) VALUE 0.00.
008800
      *  CONSTANTS AREA
      *   THE NEXT TWO PERCENTAGES MUST ADD UP TO 1 (I.E. 100%)
       01  NAT-LABOR-PCT                  PIC 9(01)V9(05) VALUE 0.53711.
       01  NAT-NONLABOR-PCT               PIC 9(01)V9(05) VALUE 0.46289.
009300
       01  HEMO-PERI-CCPD-AMT             PIC 9(02)       VALUE 20.
       01  CAPD-AMT                       PIC 9(02)       VALUE 12.
       01  CAPD-OR-CCPD-FACTOR            PIC 9(01)V9(06) VALUE
                                                             0.428571.
009800
      *  THE FOLLOWING NUMBER THAT IS LOADED INTO THE PAYMENT EQUATION
      *  IS MEANT TO BUDGET NEUTRALIZE CHANGES IN THE CASE MIX INDEX
      *  AND   --DOES NOT CHANGE--
       01  CASE-MIX-BDGT-NEUT-FACTOR      PIC 9(01)V9(04) VALUE 0.9116.
010300
010400
      ******************************************************************
      *                                                                *
      *   ******** POSSIBLE RETURN CODES FROM THIS PROGRAM ********    *
      *                                                                *
      *    ****  PPS-RTC 00-49 = BILL PAYMENT INFORMATION CODES        *
      *                                                                *
      *            00 = ESRD PPS PAYMENT CALCULATED                    *
      *                                                                *
      *    ****  PPS-RTC 50-99 = WHY THE BILL WAS NOT PAID             *
      *                                                                *
      *            52 = PROVIDER TYPE NOT = '40' OR '41'               *
      *            53 = SPECIAL PAYMENT INDICATOR NOT = '1' OR BLANK   *
      *            54 = DATE OF BIRTH  NOT NUMERIC OR = ZERO           *
      *            55 = PATIENT WEIGHT NOT NUMERIC OR = ZERO           *
      *            56 = PATIENT HEIGHT NOT NUMERIC OR = ZERO           *
      *            57 = REVENUE CENTER CODE NOT IN RANGE               *
      *            58 = CONDITION CODE NOT = '73' OR '74' OR BLANK     *
      *            71 = EXCEEDS MAXIMUM HEIGHT ALLOWANCE               *
      *            72 = EXCEEDS MAXIMUM WEIGHT ALLOWANCE               *
      ******************************************************************
      /
       LINKAGE SECTION.
       COPY BILLCPY.
      *COPY "BILLCPY.CPY".
      /
       COPY WAGECPY.
      *COPY "WAGECPY.CPY".
      /
       PROCEDURE DIVISION  USING BILL-NEW-DATA
                                 PPS-DATA-ALL
                                 WAGE-NEW-RATE-RECORD.
013600
      ******************************************************************
      * THERE ARE VARIOUS WAYS TO COMPUTE A FINAL DOLLAR AMOUNT.  THE  *
      * METHOD USED IN THIS PROGRAM IS TO USE ROUNDED INTERMEDIATE     *
      * VARIABLES.  THIS WAS DONE TO SIMPLIFY THE CALCULATIONS SO THAT *
      * WHEN SOMETHING GOES AWRY, ONE IS NOT LEFT WONDERING WHERE IN   *
      * A VAST COMPUTE STATEMENT, THINGS HAVE GONE AWRY.  THE METHOD   *
      * UTILIZED HERE HAS BEEN APPROVED BY WIL GEHNE AND JOEY BRYSON   *
      * BOTH OF WHOM WORK IN THE DIVISION OF INSTITUTIONAL CLAIMS      *
      * PROCESSING (DICP).                                             *
      *                                                                *
      *                                                                *
      *    PROCESSING:                                                 *
      *        A. WILL PROCESS CLAIMS BASED ON AGE/HEIGHT/WEIGHT       *
      *        B. INITIALIZE ESCAL HOLD VARIABLES.                     *
      *        C. EDIT THE DATA PASSED FROM THE CLAIM BEFORE           *
      *           ATTEMPTING TO CALCULATE PPS. IF THIS CLAIM           *
      *           CANNOT BE PROCESSED, SET A RETURN CODE AND           *
      *           GOBACK.                                              *
      *        D. ASSEMBLE PRICING COMPONENTS.                         *
      *        E. CALCULATE THE PRICE.                                 *
      ******************************************************************
015800
       0000-MAINLINE-CONTROL.
016000
           PERFORM 0100-INITIAL-ROUTINE.
016200
           IF PPS-RTC = 00
              PERFORM 1000-EDIT-THE-BILL-INFO
           END-IF.
016600
           IF PPS-RTC = 00
              PERFORM 2000-ASSEMBLE-PPS-VARIABLES
              PERFORM 3000-CALC-PAYMENT
           END-IF.
017100
           PERFORM 9000-MOVE-RESULTS.
017300
           GOBACK.
      /
       0100-INITIAL-ROUTINE.
017700
      ******************************************************************
      **   NEW PAYMENT RATES FOR NEW LEGISLATION                      **
      ******************************************************************
           INITIALIZE PPS-DATA-ALL.
           INITIALIZE BILL-DATA-TEST.
           INITIALIZE HOLD-PPS-COMPONENTS.
           MOVE ZEROS                        TO PPS-RTC.
           MOVE CAL-VERSION                  TO PPS-CALC-VERS-CD.
018600
      * PROVIDER TYPE '40' IS A HOSPITAL BASED ESRD FACILITY
           IF P-PROV-TYPE = '40'
              MOVE ZERO                      TO PPS-NAT-LABOR-PCT
              MOVE ZERO                      TO PPS-NAT-NONLABOR-PCT
              MOVE W-NEW-RATE1-RECORD        TO PPS-WAGE-ADJ-RATE
              MOVE W-NEW-RATE1-RECORD        TO MSA-WAGE-AMT
           ELSE
      * PROVIDER TYPE '41' IS AN INDEPENDENT ESRD FACILITY
              IF P-PROV-TYPE = '41'
                 MOVE ZERO                   TO PPS-NAT-LABOR-PCT
                 MOVE ZERO                   TO PPS-NAT-NONLABOR-PCT
                 MOVE W-NEW-RATE2-RECORD     TO PPS-WAGE-ADJ-RATE
                 MOVE W-NEW-RATE2-RECORD     TO MSA-WAGE-AMT
              ELSE
                 MOVE 52                     TO PPS-RTC
                 MOVE ZERO                   TO PPS-WAGE-ADJ-RATE
              END-IF
           END-IF.
020500
           MOVE PPS-WAGE-ADJ-RATE            TO MSA-ADJ-YEAR-AMT.
020700
      ******************************************************************
      **  NEW DRUG ADD-ON FOR NEW LEGISLATION                         **
      ******************************************************************
021100
           MOVE CASE-MIX-BDGT-NEUT-FACTOR    TO PPS-BDGT-NEUT-RATE.
           MOVE DRUG-ADDON                   TO H-DRUG-ADDON.
      /
      ******************************************************************
      ***  BILL DATA EDITS IF ANY FAIL SET PPS-RTC                   ***
      ***  AND DO NOT ATTEMPT TO PRICE.                              ***
      ******************************************************************
       1000-EDIT-THE-BILL-INFO.
022000
           IF PPS-RTC = 00
              IF P-SPEC-PYMT-IND NOT = '1' AND ' '
                 MOVE 53                     TO PPS-RTC
              END-IF
           END-IF.
022600
           IF PPS-RTC = 00
              IF (B-DOB-DATE = ZERO) OR (B-DOB-DATE NOT NUMERIC)
                 MOVE 54                     TO PPS-RTC
              END-IF
           END-IF.
023200
           IF PPS-RTC = 00
              IF (B-PATIENT-WGT = 0) OR (B-PATIENT-WGT NOT NUMERIC)
                 MOVE 55                     TO PPS-RTC
              END-IF
           END-IF.
023800
           IF PPS-RTC = 00
              IF (B-PATIENT-HGT = 0) OR (B-PATIENT-HGT NOT NUMERIC)
                 MOVE 56                     TO PPS-RTC
              END-IF
           END-IF.
024400
           IF PPS-RTC = 00
              IF B-REV-CODE  = '0821' OR '0831' OR '0841' OR '0851'
                                      OR '0880' OR '0881'
                 NEXT SENTENCE
              ELSE
                 MOVE 57                     TO PPS-RTC
              END-IF
           END-IF.
025300
           IF PPS-RTC = 00
              IF B-COND-CODE NOT = '73' AND '74' AND '  '
                 MOVE 58                     TO PPS-RTC
              END-IF
           END-IF.
025900
           IF PPS-RTC = 00
              IF B-PATIENT-HGT > 300.00
                 MOVE 71                     TO PPS-RTC
              END-IF
           END-IF.
026500
           IF PPS-RTC = 00
              IF B-PATIENT-WGT > 500.00
                 MOVE 72                     TO PPS-RTC
              END-IF
           END-IF.
027100
           IF PPS-RTC = 00
              PERFORM 1200-CALC-AGE
           END-IF.
027500
027600
       1200-CALC-AGE.
      ******************************************************************
      ***  CALCULATE PATIENT AGE                                     ***
      ******************************************************************
028100
           COMPUTE H-PATIENT-AGE = B-THRU-CCYY - B-DOB-CCYY.
028300
           IF B-DOB-MM > B-THRU-MM
              COMPUTE H-PATIENT-AGE = H-PATIENT-AGE - 1
           END-IF.
028700
      ******************************************************************
      ***  SET AGE ADJUSTMENT FACTOR                                 ***
      ******************************************************************
029100
           IF H-PATIENT-AGE < 18
              MOVE 1.620                     TO H-AGE-FACTOR
           ELSE
              IF H-PATIENT-AGE > 17 AND H-PATIENT-AGE < 45
                 MOVE 1.223                  TO H-AGE-FACTOR
              ELSE
                 IF H-PATIENT-AGE > 44 AND H-PATIENT-AGE < 60
                    MOVE 1.055               TO H-AGE-FACTOR
                 ELSE
                    IF H-PATIENT-AGE > 59 AND H-PATIENT-AGE < 70
                       MOVE 1.000            TO H-AGE-FACTOR
                    ELSE
                       IF H-PATIENT-AGE > 69 AND H-PATIENT-AGE < 80
                          MOVE 1.094         TO H-AGE-FACTOR
                       ELSE
                          IF H-PATIENT-AGE > 79
                             MOVE 1.174      TO H-AGE-FACTOR
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
031500
      /
       2000-ASSEMBLE-PPS-VARIABLES.
      ******************************************************************
      ***  CALCULATE PPS PRICING VARIABLES                           ***
      ******************************************************************
032100
           MOVE PPS-WAGE-ADJ-RATE            TO H-WAGE-ADJ-PYMT-AMT.
032300
           COMPUTE H-BSA ROUNDED = (.007184 *
               (B-PATIENT-HGT ** .725) * (B-PATIENT-WGT ** .425))
032600
           COMPUTE H-BMI ROUNDED = (B-PATIENT-WGT /
               (B-PATIENT-HGT ** 2)) * 10000.
032900
           IF H-PATIENT-AGE > 17
              COMPUTE H-BSA-FACTOR ROUNDED =
                   1.037 ** ((H-BSA - 1.84) / .1)
           ELSE
              MOVE 1.000                     TO H-BSA-FACTOR
           END-IF.
033600
           IF (H-PATIENT-AGE > 17) AND (H-BMI < 18.5)
              MOVE 1.112                     TO H-BMI-FACTOR
           ELSE
              MOVE 1.000                     TO H-BMI-FACTOR
           END-IF.
034200
      /
      ******************************************************************
      ***  IF THE BILL DATA HAS PASSED ALL EDITS (RTC=00)            ***
      ***      CALCULATE THE STANDARD PAYMENT AMOUNT.                ***
      ***    - BLEND 50% OLD RATE (MSA) WITH 50% NEW RATE (CBSA).    ***
      ******************************************************************
       3000-CALC-PAYMENT.
035000
           COMPUTE H-PYMT-AMT ROUNDED = H-WAGE-ADJ-PYMT-AMT *
                H-BMI-FACTOR * H-BSA-FACTOR * PPS-BDGT-NEUT-RATE *
                H-AGE-FACTOR * H-DRUG-ADDON.
035400
           MOVE H-PYMT-AMT                   TO CASE-MIX-FCTR-ADJ-RATE.
           MOVE SPACES                       TO COND-CD-73.
035700
           IF (B-COND-CODE = '73') AND (B-REV-CODE = '0821' OR '0831'
                                                            OR '0851')
              COMPUTE H-PYMT-AMT = H-PYMT-AMT + HEMO-PERI-CCPD-AMT
              MOVE 'A'                       TO AMT-INDIC
              MOVE HEMO-PERI-CCPD-AMT        TO BLOOD-DOLLAR
           ELSE
              IF (B-COND-CODE = '73')  AND  (B-REV-CODE = '0841')  THEN
                 COMPUTE H-PYMT-AMT = H-PYMT-AMT + CAPD-AMT
                 MOVE 'A'                    TO AMT-INDIC
                 MOVE CAPD-AMT               TO BLOOD-DOLLAR
              ELSE
                 IF (B-COND-CODE = '74')  AND
                    (B-REV-CODE = '0841' OR '0851')  THEN
                    COMPUTE H-PYMT-AMT ROUNDED = H-PYMT-AMT *
                                                 CAPD-OR-CCPD-FACTOR
                    MOVE CAPD-OR-CCPD-FACTOR TO HEMO-CCPD-CAPD
                 ELSE
                    MOVE 'A'                 TO AMT-INDIC
                    MOVE ZERO                TO BLOOD-DOLLAR
                 END-IF
              END-IF
           END-IF.
038000
           MOVE H-PYMT-AMT                   TO PPS-FINAL-PAY-AMT.
           MOVE ZERO                         TO PPS-WAGE-ADJ-RATE.
      /
       9000-MOVE-RESULTS.
038500
           IF PPS-RTC < 50  THEN
              MOVE B-COND-CODE               TO PPS-COND-CODE
              MOVE B-REV-CODE                TO PPS-REV-CODE
              MOVE P-GEO-MSA                 TO PPS-MSA
              MOVE P-GEO-CBSA                TO PPS-CBSA
              MOVE H-AGE-FACTOR              TO PPS-AGE-FACTOR
              MOVE H-BSA-FACTOR              TO PPS-BSA-FACTOR
              MOVE H-BMI-FACTOR              TO PPS-BMI-FACTOR
              IF OLD-TEST-CASE  THEN
                 MOVE H-DRUG-ADDON           TO DRUG-ADD-ON-RETURN
                 IF P-PROV-TYPE = '40' OR '05'  THEN
                    MOVE W-NEW-RATE1-RECORD  TO PPS-WAGE-ADJ-RATE
                    MOVE W-NEW-RATE1-RECORD  TO MSA-WAGE-AMT
                    MOVE W-NEW-RATE1-RECORD  TO MSA-WAGE-ADJ
                 ELSE
                    MOVE W-NEW-RATE2-RECORD  TO PPS-WAGE-ADJ-RATE
                    MOVE W-NEW-RATE2-RECORD  TO MSA-WAGE-AMT
                    MOVE W-NEW-RATE2-RECORD  TO MSA-WAGE-ADJ
                 END-IF
                 MOVE ZERO                   TO CBSA-WAGE-ADJ
                 MOVE CBSA-BLEND-PCT         TO CBSA-PCT
                 MOVE MSA-BLEND-PCT          TO MSA-PCT
                 MOVE ZERO                   TO CBSA-WAGE-PMT-RATE
                 MOVE H-PATIENT-AGE          TO AGE-RETURN
                 MOVE ZERO                   TO CBSA-WAGE-INDEX
                 MOVE NAT-LABOR-PCT          TO LABOR-PCT
              END-IF
           ELSE
              IF OLD-TEST-CASE  THEN
                 INITIALIZE PPS-COND-CODE
                 INITIALIZE PPS-REV-CODE
                 INITIALIZE PPS-MSA
                 INITIALIZE PPS-CBSA
                 INITIALIZE PPS-AGE-FACTOR
                 INITIALIZE PPS-BSA-FACTOR
                 INITIALIZE PPS-BMI-FACTOR
                 INITIALIZE DRUG-ADD-ON-RETURN
                 INITIALIZE MSA-WAGE-ADJ
                 INITIALIZE CBSA-WAGE-ADJ
                 INITIALIZE CBSA-PCT
                 INITIALIZE MSA-PCT
                 INITIALIZE CASE-MIX-FCTR-ADJ-RATE
                 INITIALIZE CBSA-WAGE-PMT-RATE
                 INITIALIZE HEMO-CCPD-CAPD
                 INITIALIZE AGE-RETURN
              END-IF
           END-IF.
043300
      ******        L A S T   S O U R C E   S T A T E M E N T      *****
