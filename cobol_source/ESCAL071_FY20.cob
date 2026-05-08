       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCAL071.
      *AUTHORS.    CMS.
      *       EFFECTIVE APRIL 1, 2007
      ******************************************************************
      * 4/06/05 - ALLOW PROVIDER TYPE '05' FOR PEDIATRIC HOSP          *
      *         - TO BE EFFECTIVE WITH THE NEXT RELEASE                *
      *         - CHANGED IN 0100-INITIAL-ROUTINE WITH PROVIDER        *
      *           TYPE '40'                                            *
      * 1/01/06 - NEW CBSA TABLE FOR CY2006                            *
      *         - UPDATE 2005 MSA COMPOSITE RATES WITH 1.6% INCREASE   *
      * 1/18/07 - THE MSA-WAGE-FACTOR-2007 WAS NOT IMPLEMENTED DURING  *
      *           THE FIRST THREE MONTHS OF 2007                       *
      *         - MSA-CBSA BLEND PERCENT NOW SET AT 50% MSA 50% CBSA   *
      *         - ADDITIONAL VARIABLES WERE CREATED IN ORDER TO MAKE   *
      *           CHANGING VALUES EASIER (IN WORKING STORAGE RATHER    *
      *           THAN IN THE PROCEDURE DIVISION)                      *
      *         - THIS PROGRAM NOW REFLECTS ENHANCEMENTS MADE SO THAT  *
      *           TESTING OF THE CODE DOES NOT REQUIRE COMMENTING      *
      *           IN/OUT LINES OF CODE.  IN ADDITION CALCULATED        *
      *           VARIABLES ARE NOW PASSED BACK WHEN TEST CASES ARE    *
      *           ENCOUNTERED IN ORDER FOR THE MASTER DRIVER TO PRINT  *
      *           MORE INFORMATION ABOUT WHAT WENT ON IN THIS PROGRAM  *
      * 1/19/07 - INDEPENDENT ESRD FACILITY WAGE NOW    $132.49        *
      *           HOSPITAL BASED ESRD FACILTIY WAGE NOW $136.68        *
      *           DRUG ADD-ON ADJUSTMENT AFTER 4/1/07   1.1490         *
      * 1/26/07 - MSA composite payment rates increased 1.6% above the *
      *           2006 rates.  This means that the rates passed from   *
      *           the driver, which are 2005 rates, need to be         *
      *           multiplied by 1.016 and then rounded to get the 2006 *
      *           rate and then that result multiplied by another 1.016*
      *           and rounded again to get the 4/1/2007 rate.  This    *
      *           necessary rounding makes the results agree with those*
      *           published in the Federal Register.  This method was  *
      *           verified via tedious Excel spreadsheet calculations  *
      ******************************************************************
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.            IBM-Z990.
       OBJECT-COMPUTER.            ITTY-BITTY-MACHINE-CORPORATION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
004400
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.
       01  W-STORAGE-REF                  PIC X(46)  VALUE
           'ESCAL071      - W O R K I N G   S T O R A G E'.
       01  CAL-VERSION                    PIC X(05)  VALUE 'C07.1'.
005200
       01  DISPLAY-LINE-MEASUREMENT.
           05  FILLER                     PIC X(50) VALUE
               '....:...10....:...20....:...30....:...40....:...50'.
           05  FILLER                     PIC X(50) VALUE
               '....:...60....:...70....:...80....:...90....:..100'.
           05  FILLER                     PIC X(20) VALUE
               '....:..110....:..120'.
006000
       01  PRINT-LINE-MEASUREMENT.
           05  FILLER                     PIC X(51) VALUE
               'X....:...10....:...20....:...30....:...40....:...50'.
           05  FILLER                     PIC X(50) VALUE
               '....:...60....:...70....:...80....:...90....:..100'.
           05  FILLER                     PIC X(32) VALUE
               '....:..110....:..120....:..130..'.
006800
       01  HOLD-PPS-COMPONENTS.
           05  H-PYMT-AMT                 PIC 9(07)V9(02).
           05  H-WAGE-ADJ-PYMT-AMT        PIC 9(07)V9(02).
           05  H-2006-WAGE-ADJ-PYMT       PIC 9(07)V9(02).
           05  H-2007-WAGE-ADJ-PYMT       PIC 9(07)V9(02).
           05  H-MSA-COMPOSITE-PYMT       PIC 9(07)V9(02).
           05  H-WAGE-ADJ-PYMT-OLD        PIC 9(07)V9(02).
           05  H-WAGE-ADJ-PYMT-NEW        PIC 9(07)V9(02).
           05  H-WAGE-ADJ                 PIC 9(02)V9(04).
           05  H-PYMT-RATE                PIC 9(04)V9(02).
           05  H-FIXED-LOSS-AMT           PIC 9(07)V9(02).
           05  H-ESRD-FAC-RATE            PIC 9(07)V9(02).
           05  H-PATIENT-AGE              PIC 9(03).
           05  H-AGE-FACTOR               PIC 9(01)V9(03).
           05  H-BSA-FACTOR               PIC 9(01)V9(04).
           05  H-BMI-FACTOR               PIC 9(01)V9(04).
           05  H-BSA                      PIC 9(03)V9(04).
           05  H-BMI                      PIC 9(03)V9(04).
           05  H-DRUG-ADDON               PIC 9(01)V9(04).
008800
      *   THE FOLLOWING THREE VARIABLES WILL CHANGE FROM YEAR TO YEAR
       01  DRUG-ADDON                     PIC 9(01)V9(04) VALUE 1.1490.
       01  HOSP-BASED-PMT-RATE            PIC 9(04)V9(02) VALUE 136.68.
       01  INDP-ESRD-FAC-PMT-RATE         PIC 9(04)V9(02) VALUE 132.49.
009300
      *   THE NEXT TWO PERCENTAGES MUST ADD UP TO 1 (I.E. 100%)
      *   THEY WILL CONTINUE TO CHANGE UNTIL 2009 (AND THEN BE FIXED)
       01  MSA-BLEND-PCT                  PIC 9(01)V9(02) VALUE 0.50.
       01  CBSA-BLEND-PCT                 PIC 9(01)V9(02) VALUE 0.50.
009800
009900
      *  CONSTANTS AREA
      *   THE NEXT TWO PERCENTAGES MUST ADD UP TO 1 (I.E. 100%)
       01  NAT-LABOR-PCT                  PIC 9(01)V9(05) VALUE 0.53711.
       01  NAT-NONLABOR-PCT               PIC 9(01)V9(05) VALUE 0.46289.
010400
       01  HEMO-PERI-CCPD-AMT             PIC 9(02)       VALUE 20.
       01  CAPD-AMT                       PIC 9(02)       VALUE 12.
       01  CAPD-OR-CCPD-FACTOR            PIC 9(01)V9(06) VALUE
                                                             0.428571.
       01  MSA-WAGE-FACTOR-2006           PIC 9(01)V9(03) VALUE 1.016.
       01  MSA-WAGE-FACTOR-2007           PIC 9(01)V9(03) VALUE 1.016.
011100
      *  THE FOLLOWING NUMBER THAT IS LOADED INTO THE PAYMENT EQUATION
      *  IS MEANT TO BUDGET NEUTRALIZE CHANGES IN THE CASE MIX INDEX
      *  AND   --DOES NOT CHANGE--
       01  CASE-MIX-BDGT-NEUT-FACTOR      PIC 9(01)V9(04) VALUE 0.9116.
011600
011700
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
      *            52 = PROVIDER TYPE NOT = '40' OR '41' OR '05'       *
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
                                 WAGE-NEW-RATE-RECORD
                                 COM-CBSA-WAGE-RECORD.
015000
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
017200
       0000-MAINLINE-CONTROL.
017400
           PERFORM 0100-INITIAL-ROUTINE.
017600
           IF PPS-RTC = 00
              PERFORM 1000-EDIT-THE-BILL-INFO
           END-IF.
018000
           IF PPS-RTC = 00
              PERFORM 2000-ASSEMBLE-PPS-VARIABLES
              PERFORM 3000-CALC-PAYMENT
           END-IF.
018500
           PERFORM 9000-MOVE-RESULTS.
018700
           GOBACK.
      /
       0100-INITIAL-ROUTINE.
019100
      ******************************************************************
      **   NEW PAYMENT RATES FOR NEW LEGISLATION                      **
      ******************************************************************
           INITIALIZE PPS-DATA-ALL.
           INITIALIZE BILL-DATA-TEST.
           INITIALIZE HOLD-PPS-COMPONENTS.
           MOVE ZEROS                        TO PPS-RTC.
           MOVE CAL-VERSION                  TO PPS-CALC-VERS-CD.
020000
      * PROVIDER TYPE '40' AND '05' ARE HOSPITAL BASED ESRD FACILITIES
           IF P-PROV-TYPE = '40' OR '05'
              MOVE NAT-LABOR-PCT             TO PPS-NAT-LABOR-PCT
              MOVE NAT-NONLABOR-PCT          TO PPS-NAT-NONLABOR-PCT
              MOVE HOSP-BASED-PMT-RATE       TO H-PYMT-RATE
              COMPUTE H-2006-WAGE-ADJ-PYMT ROUNDED =
                      W-NEW-RATE1-RECORD    *  MSA-WAGE-FACTOR-2006
              COMPUTE H-2007-WAGE-ADJ-PYMT ROUNDED =
                      H-2006-WAGE-ADJ-PYMT  *  MSA-WAGE-FACTOR-2007
              MOVE H-2007-WAGE-ADJ-PYMT      TO H-WAGE-ADJ-PYMT-OLD
              MOVE W-NEW-RATE1-RECORD        TO MSA-WAGE-AMT
              MOVE COM-CBSA-W-INDEX          TO H-WAGE-ADJ
           ELSE
      * PROVIDER TYPE '41' IS AN INDEPENDENT ESRD FACILITY
              IF P-PROV-TYPE = '41'
                 MOVE NAT-LABOR-PCT          TO PPS-NAT-LABOR-PCT
                 MOVE NAT-NONLABOR-PCT       TO PPS-NAT-NONLABOR-PCT
                 MOVE INDP-ESRD-FAC-PMT-RATE TO H-PYMT-RATE
                 COMPUTE H-2006-WAGE-ADJ-PYMT ROUNDED =
                         W-NEW-RATE2-RECORD    *  MSA-WAGE-FACTOR-2006
                 COMPUTE H-2007-WAGE-ADJ-PYMT ROUNDED =
                         H-2006-WAGE-ADJ-PYMT  *  MSA-WAGE-FACTOR-2007
                 MOVE H-2007-WAGE-ADJ-PYMT   TO H-WAGE-ADJ-PYMT-OLD
                 MOVE W-NEW-RATE2-RECORD     TO MSA-WAGE-AMT
                 MOVE COM-CBSA-W-INDEX       TO H-WAGE-ADJ
              ELSE
                 MOVE 52                     TO PPS-RTC
                 MOVE ZERO                   TO PPS-WAGE-ADJ-RATE
              END-IF
           END-IF.
023100
           MOVE H-WAGE-ADJ-PYMT-OLD          TO MSA-ADJ-YEAR-AMT.
023300
      ******************************************************************
      **  NEW DRUG ADD-ON FOR NEW LEGISLATION                         **
      ******************************************************************
023700
           MOVE CASE-MIX-BDGT-NEUT-FACTOR    TO PPS-BDGT-NEUT-RATE.
           MOVE DRUG-ADDON                   TO H-DRUG-ADDON.
      /
      ******************************************************************
      ***  BILL DATA EDITS IF ANY FAIL SET PPS-RTC                   ***
      ***  AND DO NOT ATTEMPT TO PRICE.                              ***
      ******************************************************************
       1000-EDIT-THE-BILL-INFO.
024600
           IF PPS-RTC = 00
              IF P-SPEC-PYMT-IND NOT = '1' AND ' '
                 MOVE 53                     TO PPS-RTC
              END-IF
           END-IF.
025200
           IF PPS-RTC = 00
              IF (B-DOB-DATE = ZERO) OR (B-DOB-DATE NOT NUMERIC)
                 MOVE 54                     TO PPS-RTC
              END-IF
           END-IF.
025800
           IF PPS-RTC = 00
              IF (B-PATIENT-WGT = 0) OR (B-PATIENT-WGT NOT NUMERIC)
                 MOVE 55                     TO PPS-RTC
              END-IF
           END-IF.
026400
           IF PPS-RTC = 00
              IF (B-PATIENT-HGT = 0) OR (B-PATIENT-HGT NOT NUMERIC)
                 MOVE 56                     TO PPS-RTC
              END-IF
           END-IF.
027000
           IF PPS-RTC = 00
              IF B-REV-CODE  = '0821' OR '0831' OR '0841' OR '0851'
                                      OR '0880' OR '0881'
                 NEXT SENTENCE
              ELSE
                 MOVE 57                     TO PPS-RTC
              END-IF
           END-IF.
027900
           IF PPS-RTC = 00
              IF B-COND-CODE NOT = '73' AND '74' AND '  '
                 MOVE 58                     TO PPS-RTC
              END-IF
           END-IF.
028500
           IF PPS-RTC = 00
              IF B-PATIENT-HGT > 300.00
                 MOVE 71                     TO PPS-RTC
              END-IF
           END-IF.
029100
           IF PPS-RTC = 00
              IF B-PATIENT-WGT > 500.00
                 MOVE 72                     TO PPS-RTC
              END-IF
           END-IF.
029700
           IF PPS-RTC = 00
              PERFORM 1200-CALC-AGE
           END-IF.
030100
030200
       1200-CALC-AGE.
      ******************************************************************
      ***  CALCULATE PATIENT AGE                                     ***
      ******************************************************************
030700
           COMPUTE H-PATIENT-AGE = B-THRU-CCYY - B-DOB-CCYY.
030900
           IF B-DOB-MM > B-THRU-MM
              COMPUTE H-PATIENT-AGE = H-PATIENT-AGE - 1
           END-IF.
031300
      ******************************************************************
      ***  SET AGE ADJUSTMENT FACTOR                                 ***
      ******************************************************************
031700
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
034100
      /
       2000-ASSEMBLE-PPS-VARIABLES.
      ******************************************************************
      ***  CALCULATE PPS PRICING VARIABLES                           ***
      ******************************************************************
034700
           COMPUTE H-BSA ROUNDED = (.007184 *
               (B-PATIENT-HGT ** .725) * (B-PATIENT-WGT ** .425))
035000
           COMPUTE H-BMI ROUNDED = (B-PATIENT-WGT /
               (B-PATIENT-HGT ** 2)) * 10000.
035300
           IF H-PATIENT-AGE > 17
              COMPUTE H-BSA-FACTOR ROUNDED =
                   1.037 ** ((H-BSA - 1.84) / .1)
           ELSE
              MOVE 1.000                     TO H-BSA-FACTOR
           END-IF.
036000
           IF (H-PATIENT-AGE > 17) AND (H-BMI < 18.5)
              MOVE 1.112                     TO H-BMI-FACTOR
           ELSE
              MOVE 1.000                     TO H-BMI-FACTOR
           END-IF.
036600
      /
      ******************************************************************
      ***  IF THE BILL DATA HAS PASSED ALL EDITS (RTC=00)            ***
      ***      CALCULATE THE STANDARD PAYMENT AMOUNT.                ***
      ***    - BLEND 50% OLD RATE (MSA) WITH 50% NEW RATE (CBSA).    ***
      ******************************************************************
       3000-CALC-PAYMENT.
037400
      * BEGINNING 01/01/2007 THE BLEND RATE WILL BE 50% MSA 50% CBSA
           COMPUTE H-WAGE-ADJ-PYMT-OLD ROUNDED =
                  (H-WAGE-ADJ-PYMT-OLD * MSA-BLEND-PCT).
037800
           COMPUTE H-WAGE-ADJ-PYMT-NEW ROUNDED =
               (((H-PYMT-RATE * PPS-NAT-LABOR-PCT) * H-WAGE-ADJ) +
                 (H-PYMT-RATE * PPS-NAT-NONLABOR-PCT)) * CBSA-BLEND-PCT.
038200
           COMPUTE H-WAGE-ADJ-PYMT-AMT =
                   H-WAGE-ADJ-PYMT-NEW + H-WAGE-ADJ-PYMT-OLD.
038500
           COMPUTE H-PYMT-AMT ROUNDED = H-WAGE-ADJ-PYMT-AMT *
                H-BMI-FACTOR * H-BSA-FACTOR * PPS-BDGT-NEUT-RATE *
                H-AGE-FACTOR * H-DRUG-ADDON.
038900
           MOVE H-PYMT-AMT                   TO CASE-MIX-FCTR-ADJ-RATE.
           MOVE SPACES                       TO COND-CD-73.
039200
           IF (B-COND-CODE = '73') AND (B-REV-CODE = '0821' OR '0831'
                                                            OR '0851')
              COMPUTE H-PYMT-AMT = H-PYMT-AMT + HEMO-PERI-CCPD-AMT
              MOVE 'A'                       TO AMT-INDIC
              MOVE HEMO-PERI-CCPD-AMT        TO BLOOD-DOLLAR
           ELSE
              IF (B-COND-CODE = '73') AND (B-REV-CODE = '0841')
                 COMPUTE H-PYMT-AMT = H-PYMT-AMT + CAPD-AMT
                 MOVE 'A'                    TO AMT-INDIC
                 MOVE CAPD-AMT               TO BLOOD-DOLLAR
              ELSE
                 IF (B-COND-CODE = '74') AND
                    (B-REV-CODE = '0841' OR '0851')
                    COMPUTE H-PYMT-AMT ROUNDED = H-PYMT-AMT *
                                                 CAPD-OR-CCPD-FACTOR
                    MOVE CAPD-OR-CCPD-FACTOR TO HEMO-CCPD-CAPD
                 ELSE
                    MOVE 'A'                 TO AMT-INDIC
                    MOVE ZERO                TO BLOOD-DOLLAR
                 END-IF
              END-IF
           END-IF.
041500
           MOVE H-PYMT-AMT                   TO PPS-FINAL-PAY-AMT.
           MOVE H-WAGE-ADJ-PYMT-AMT          TO PPS-WAGE-ADJ-RATE.
      /
       9000-MOVE-RESULTS.
042000
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
                 MOVE H-WAGE-ADJ-PYMT-OLD    TO MSA-WAGE-ADJ
                 MOVE H-WAGE-ADJ-PYMT-NEW    TO CBSA-WAGE-ADJ
                 MOVE CBSA-BLEND-PCT         TO CBSA-PCT
                 MOVE MSA-BLEND-PCT          TO MSA-PCT
                 MOVE H-PYMT-RATE            TO CBSA-WAGE-PMT-RATE
                 MOVE H-PATIENT-AGE          TO AGE-RETURN
                 MOVE H-WAGE-ADJ             TO CBSA-WAGE-INDEX
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
046000
      ******        L A S T   S O U R C E   S T A T E M E N T      *****
