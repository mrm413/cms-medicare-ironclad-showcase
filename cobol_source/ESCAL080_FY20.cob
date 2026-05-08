       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCAL080.
      *AUTHORS.    CMS.
      *       EFFECTIVE JANUARY 1, 2008
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
      * 1/26/07 - MSA COMPOSITE PAYMENT RATES INCREASED 1.6% ABOVE THE *
      *           2006 RATES.  THIS MEANS THAT THE RATES PASSED FROM   *
      *           THE DRIVER, WHICH ARE 2005 RATES, NEED TO BE         *
      *           MULTIPLIED BY 1.016 AND THEN ROUNDED TO GET THE 2006 *
      *           RATE AND THEN THAT RESULT MULTIPLIED BY ANOTHER 1.016*
      *           AND ROUNDED AGAIN TO GET THE 4/1/2007 RATE.  THIS    *
      *           NECESSARY ROUNDING MAKES THE RESULTS AGREE WITH THOSE*
      *           PUBLISHED IN THE FEDERAL REGISTER.  THIS METHOD WAS  *
      *           VERIFIED VIA TEDIOUS EXCEL SPREADSHEET CALCULATIONS  *
      * 10/30/07- MSA COMPOSITE PAYMENT RATES DID NOT INCREASE FROM THE*
      *           2007/04/01 RATES.  THE COMPOSITE BASE RATES DID NOT  *
      *           CHANGE FROM THE 07/04/01 FACILITY RATES.             *
      *           THE BLEND OF MSA TO CBSA WAS CHANGED TO 25% MSA AND  *
      *           75% CBSA.                                            *
      *           THE DRUG ADDON FACTOR WAS INCREASED TO 1.1550        *
      *           ALL OTHER FIGURES REMAINED THE SAME.                 *
      * 11/21/07- CHANGES WERE MADE TO ALL THE CALCULATION SUBROUTINES *
      *           BEGINNING IN 2005 IN ORDER TO ENSURE THAT THE        *
      *           PC-PRICER CAN USE THE **EXACT** SAME CODE THAT EXISTS*
      *           ON THE MAINFRAME.  IN ORDER TO ENSURE THAT THE       *
      *           LINKAGE SECTION REMAINS THE SAME, THE FILLER AREAS   *
      *           LOCATED AT THE END OF EACH '05' LEVEL HAVE BEEN      *
      *           MODIFIED TO INCLUDE VARIABLES NEEDED FOR PROOFING OF *
      *           THE MAINFRAME CODE AS WELL AS DISPLAYING ON THE PC-  *
      *           PRICER.                                              *
      *                THE VARIABLE LABELED 'P-ESRD-RATE' IS NEVER     *
      *           USED IN CALCUALTIONS SINCE THE DRIVER IS THE ONLY ONE*
      *           THAT USES IT IN SPECIAL CIRCUMSTANCES AT THE FISCAL  *
      *           INTERMEDIARIES.  THEREFORE A DUAL USE WAS MADE OF IT *
      *           SO THAT THE PC-PRICER CAN MAKE USE OF THE VERY       *
      *           LIMITED SPACE LEFT.  THE VARIABLE NAME WAS REDEFINED *
      *           TO 'CASE-MIX-FCTR-ADJ-RATE' TO REFLECT IT'S USAGE ON *
      *           THE PC-PRICER.                                       *
      *                  USAGE OF THE THESE FILLER AREAS AND THE       *
      *           'CASE-MIX-FCTR-ADJ-RATE' VARIABLE WILL ONLY OCCUR    *
      *           WHEN A 'T' (FOR TESTING) IS IN THE LAST COLUMN OF THE*
      *           INPUT RECORD.  OTHERWISE THESE VARIABLES WILL CONTAIN*
      *           SPACES WHEN NOT IN TEST MODE.  MOST OF THE MOVES TO  *
      *           THESE VARIABLES OCCURS IN THE 9000-MOVE-RESULTS PARA-*
      *           GRAPH.  A FEW MOVES MUST OCCUR IN OTHER AREAS OF THE *
      *           PROGRAM.                                             *
      * FUTURE    This program is intended to be used in a running four*
      *           year (current year and three prior years) mode since *
      *           that is the only manner in which bills can be submit-*
      *           ted and corrected.  Therefore the code for ths 2005  *
      *           MSA will be phased out beginning in 2009 and complet-*
      *           ly gone in 2013 when the MSA-CBSA blend will no      *
      *           longer be needed.                                    *
      ******************************************************************
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.            IBM-Z990.
       OBJECT-COMPUTER.            ITTY-BITTY-MACHINE-CORPORATION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
008300
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.
       01  W-STORAGE-REF                  PIC X(46)  VALUE
           'ESCAL080      - W O R K I N G   S T O R A G E'.
       01  CAL-VERSION                    PIC X(05)  VALUE 'C08.0'.
009100
       01  DISPLAY-LINE-MEASUREMENT.
           05  FILLER                     PIC X(50) VALUE
               '....:...10....:...20....:...30....:...40....:...50'.
           05  FILLER                     PIC X(50) VALUE
               '....:...60....:...70....:...80....:...90....:..100'.
           05  FILLER                     PIC X(20) VALUE
               '....:..110....:..120'.
009900
       01  PRINT-LINE-MEASUREMENT.
           05  FILLER                     PIC X(51) VALUE
               'X....:...10....:...20....:...30....:...40....:...50'.
           05  FILLER                     PIC X(50) VALUE
               '....:...60....:...70....:...80....:...90....:..100'.
           05  FILLER                     PIC X(32) VALUE
               '....:..110....:..120....:..130..'.
010700
       01  HOLD-PPS-COMPONENTS.
           05  H-PYMT-AMT                 PIC 9(07)V9(02).
           05  H-WAGE-ADJ-PYMT-AMT        PIC 9(07)V9(02).
           05  H-2006-WAGE-ADJ-PYMT       PIC 9(07)V9(02).
           05  H-2007-WAGE-ADJ-PYMT       PIC 9(07)V9(02).
           05  H-2008-WAGE-ADJ-PYMT       PIC 9(07)V9(02).
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
012800
      *   THE FOLLOWING THREE VARIABLES WILL CHANGE FROM YEAR TO YEAR
       01  DRUG-ADDON                     PIC 9(01)V9(04) VALUE 1.1550.
       01  HOSP-BASED-PMT-RATE            PIC 9(04)V9(02) VALUE 136.68.
       01  INDP-ESRD-FAC-PMT-RATE         PIC 9(04)V9(02) VALUE 132.49.
013300
      *   THE NEXT TWO PERCENTAGES MUST ADD UP TO 1 (I.E. 100%)
      *   THEY WILL CONTINUE TO CHANGE UNTIL 2009 WHEN CBSA WILL BE 1.00
       01  MSA-BLEND-PCT                  PIC 9(01)V9(02) VALUE 0.25.
       01  CBSA-BLEND-PCT                 PIC 9(01)V9(02) VALUE 0.75.
013800
013900
      *  CONSTANTS AREA
      *   THE NEXT TWO PERCENTAGES MUST ADD UP TO 1 (I.E. 100%)
       01  NAT-LABOR-PCT                  PIC 9(01)V9(05) VALUE 0.53711.
       01  NAT-NONLABOR-PCT               PIC 9(01)V9(05) VALUE 0.46289.
014400
       01  HEMO-PERI-CCPD-AMT             PIC 9(02)       VALUE 20.
       01  CAPD-AMT                       PIC 9(02)       VALUE 12.
       01  CAPD-OR-CCPD-FACTOR            PIC 9(01)V9(06) VALUE
                                                             0.428571.
      *    THE ABOVE NUMBER TECHNICALLY REPRESENTS THE FRACTIONAL
      *    NUMBER 3/7 WHICH IS THREE DAYS PER WEEK THAT A PERSON CAN
      *    RECEIVE DIALYSIS.
015200
       01  MSA-WAGE-FACTOR-2006           PIC 9(01)V9(03) VALUE 1.016.
       01  MSA-WAGE-FACTOR-2007           PIC 9(01)V9(03) VALUE 1.016.
       01  MSA-WAGE-FACTOR-2008           PIC 9(01)V9(03) VALUE 1.000.
015600
      *  THE FOLLOWING NUMBER THAT IS LOADED INTO THE PAYMENT EQUATION
      *  IS MEANT TO BUDGET NEUTRALIZE CHANGES IN THE CASE MIX INDEX
      *  AND   --DOES NOT CHANGE--
       01  CASE-MIX-BDGT-NEUT-FACTOR      PIC 9(01)V9(04) VALUE 0.9116.
016100
016200
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
019500
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
021700
       0000-MAINLINE-CONTROL.
021900
           PERFORM 0100-INITIAL-ROUTINE.
022100
           IF PPS-RTC = 00  THEN
              PERFORM 1000-EDIT-THE-BILL-INFO
           END-IF.
022500
           IF PPS-RTC = 00  THEN
              PERFORM 2000-ASSEMBLE-PPS-VARIABLES
              PERFORM 3000-CALC-PAYMENT
           END-IF.
023000
           PERFORM 9000-MOVE-RESULTS.
023200
           GOBACK.
      /
       0100-INITIAL-ROUTINE.
023600
      ******************************************************************
      **   NEW PAYMENT RATES FOR NEW LEGISLATION                      **
      ******************************************************************
           INITIALIZE PPS-DATA-ALL.
           INITIALIZE BILL-DATA-TEST.
           INITIALIZE HOLD-PPS-COMPONENTS.
           MOVE ZEROS                        TO PPS-RTC.
           MOVE CAL-VERSION                  TO PPS-CALC-VERS-CD.
024500
      * PROVIDER TYPE '40' AND '05' ARE HOSPITAL BASED ESRD FACILITIES
           IF P-PROV-TYPE = '40' OR '05'  THEN
              MOVE NAT-LABOR-PCT             TO PPS-NAT-LABOR-PCT
              MOVE NAT-NONLABOR-PCT          TO PPS-NAT-NONLABOR-PCT
              MOVE HOSP-BASED-PMT-RATE       TO H-PYMT-RATE
              COMPUTE H-2006-WAGE-ADJ-PYMT ROUNDED =
                      W-NEW-RATE1-RECORD    *  MSA-WAGE-FACTOR-2006
              COMPUTE H-2007-WAGE-ADJ-PYMT ROUNDED =
                      H-2006-WAGE-ADJ-PYMT  *  MSA-WAGE-FACTOR-2007
              COMPUTE H-2008-WAGE-ADJ-PYMT ROUNDED =
                      H-2007-WAGE-ADJ-PYMT  *  MSA-WAGE-FACTOR-2008
              MOVE H-2008-WAGE-ADJ-PYMT      TO H-WAGE-ADJ-PYMT-OLD
              MOVE W-NEW-RATE1-RECORD        TO MSA-WAGE-AMT
              MOVE COM-CBSA-W-INDEX          TO H-WAGE-ADJ
           ELSE
      * PROVIDER TYPE '41' IS AN INDEPENDENT ESRD FACILITY
              IF P-PROV-TYPE = '41'  THEN
                 MOVE NAT-LABOR-PCT          TO PPS-NAT-LABOR-PCT
                 MOVE NAT-NONLABOR-PCT       TO PPS-NAT-NONLABOR-PCT
                 MOVE INDP-ESRD-FAC-PMT-RATE TO H-PYMT-RATE
                 COMPUTE H-2006-WAGE-ADJ-PYMT ROUNDED =
                         W-NEW-RATE2-RECORD    *  MSA-WAGE-FACTOR-2006
                 COMPUTE H-2007-WAGE-ADJ-PYMT ROUNDED =
                         H-2006-WAGE-ADJ-PYMT  *  MSA-WAGE-FACTOR-2007
                 COMPUTE H-2008-WAGE-ADJ-PYMT ROUNDED =
                         H-2007-WAGE-ADJ-PYMT  *  MSA-WAGE-FACTOR-2008
                 MOVE H-2008-WAGE-ADJ-PYMT   TO H-WAGE-ADJ-PYMT-OLD
                 MOVE W-NEW-RATE2-RECORD     TO MSA-WAGE-AMT
                 MOVE COM-CBSA-W-INDEX       TO H-WAGE-ADJ
              ELSE
                 MOVE 52                     TO PPS-RTC
                 MOVE ZERO                   TO PPS-WAGE-ADJ-RATE
              END-IF
           END-IF.
028000
           MOVE H-WAGE-ADJ-PYMT-OLD          TO MSA-ADJ-YEAR-AMT.
028200
      ******************************************************************
      **  NEW DRUG ADD-ON FOR NEW LEGISLATION                         **
      ******************************************************************
028600
           MOVE CASE-MIX-BDGT-NEUT-FACTOR    TO PPS-BDGT-NEUT-RATE.
           MOVE DRUG-ADDON                   TO H-DRUG-ADDON.
      /
      ******************************************************************
      ***  BILL DATA EDITS IF ANY FAIL SET PPS-RTC                   ***
      ***  AND DO NOT ATTEMPT TO PRICE.                              ***
      ******************************************************************
       1000-EDIT-THE-BILL-INFO.
029500
           IF PPS-RTC = 00  THEN
              IF P-SPEC-PYMT-IND NOT = '1' AND ' '  THEN
                 MOVE 53                     TO PPS-RTC
              END-IF
           END-IF.
030100
           IF PPS-RTC = 00  THEN
              IF (B-DOB-DATE = ZERO)  OR  (B-DOB-DATE NOT NUMERIC)  THEN
                 MOVE 54                     TO PPS-RTC
              END-IF
           END-IF.
030700
           IF PPS-RTC = 00  THEN
              IF (B-PATIENT-WGT = 0)  OR  (B-PATIENT-WGT NOT NUMERIC)
                 MOVE 55                     TO PPS-RTC
              END-IF
           END-IF.
031300
           IF PPS-RTC = 00  THEN
              IF (B-PATIENT-HGT = 0)  OR  (B-PATIENT-HGT NOT NUMERIC)
                 MOVE 56                     TO PPS-RTC
              END-IF
           END-IF.
031900
           IF PPS-RTC = 00  THEN
              IF B-REV-CODE  = '0821' OR '0831' OR '0841' OR '0851'
                                      OR '0880' OR '0881'
                 NEXT SENTENCE
              ELSE
                 MOVE 57                     TO PPS-RTC
              END-IF
           END-IF.
032800
           IF PPS-RTC = 00  THEN
              IF B-COND-CODE NOT = '73' AND '74' AND '  '
                 MOVE 58                     TO PPS-RTC
              END-IF
           END-IF.
033400
           IF PPS-RTC = 00  THEN
              IF B-PATIENT-HGT > 300.00
                 MOVE 71                     TO PPS-RTC
              END-IF
           END-IF.
034000
           IF PPS-RTC = 00  THEN
              IF B-PATIENT-WGT > 500.00  THEN
                 MOVE 72                     TO PPS-RTC
              END-IF
           END-IF.
034600
           IF PPS-RTC = 00  THEN
              PERFORM 1200-CALC-AGE
           END-IF.
035000
035100
       1200-CALC-AGE.
      ******************************************************************
      ***  CALCULATE PATIENT AGE                                     ***
      ******************************************************************
035600
           COMPUTE H-PATIENT-AGE = B-THRU-CCYY - B-DOB-CCYY.
035800
           IF B-DOB-MM > B-THRU-MM  THEN
              COMPUTE H-PATIENT-AGE = H-PATIENT-AGE - 1
           END-IF.
036200
      ******************************************************************
      ***  SET AGE ADJUSTMENT FACTOR                                 ***
      ******************************************************************
036600
           IF H-PATIENT-AGE < 18  THEN
              MOVE 1.620                     TO H-AGE-FACTOR
           ELSE
              IF H-PATIENT-AGE > 17 AND H-PATIENT-AGE < 45  THEN
                 MOVE 1.223                  TO H-AGE-FACTOR
              ELSE
                 IF H-PATIENT-AGE > 44 AND H-PATIENT-AGE < 60  THEN
                    MOVE 1.055               TO H-AGE-FACTOR
                 ELSE
                    IF H-PATIENT-AGE > 59 AND H-PATIENT-AGE < 70  THEN
                       MOVE 1.000            TO H-AGE-FACTOR
                    ELSE
                       IF H-PATIENT-AGE > 69 AND H-PATIENT-AGE < 80
                          MOVE 1.094         TO H-AGE-FACTOR
                       ELSE
                          IF H-PATIENT-AGE > 79  THEN
                             MOVE 1.174      TO H-AGE-FACTOR
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
039000
      /
       2000-ASSEMBLE-PPS-VARIABLES.
      ******************************************************************
      ***  CALCULATE PPS PRICING VARIABLES                           ***
      ******************************************************************
039600
           COMPUTE H-BSA ROUNDED = (.007184 *
               (B-PATIENT-HGT ** .725) * (B-PATIENT-WGT ** .425))
039900
           COMPUTE H-BMI ROUNDED = (B-PATIENT-WGT /
               (B-PATIENT-HGT ** 2)) * 10000.
040200
           IF H-PATIENT-AGE > 17  THEN
              COMPUTE H-BSA-FACTOR ROUNDED =
                   1.037 ** ((H-BSA - 1.84) / .1)
           ELSE
              MOVE 1.000                     TO H-BSA-FACTOR
           END-IF.
040900
           IF (H-PATIENT-AGE > 17) AND (H-BMI < 18.5)  THEN
              MOVE 1.112                     TO H-BMI-FACTOR
           ELSE
              MOVE 1.000                     TO H-BMI-FACTOR
           END-IF.
041500
      /
      ******************************************************************
      ***  IF THE BILL DATA HAS PASSED ALL EDITS (RTC=00)            ***
      ***      CALCULATE THE STANDARD PAYMENT AMOUNT.                ***
      ***    - BLEND 25% OLD RATE (MSA) WITH 75% NEW RATE (CBSA).    ***
      ******************************************************************
       3000-CALC-PAYMENT.
042300
      * BEGINNING 01/01/2008 THE BLEND RATE WILL BE 25% MSA 75% CBSA
           COMPUTE H-WAGE-ADJ-PYMT-OLD ROUNDED =
                  (H-WAGE-ADJ-PYMT-OLD * MSA-BLEND-PCT).
042700
           COMPUTE H-WAGE-ADJ-PYMT-NEW ROUNDED =
               (((H-PYMT-RATE * PPS-NAT-LABOR-PCT) * H-WAGE-ADJ) +
                 (H-PYMT-RATE * PPS-NAT-NONLABOR-PCT)) * CBSA-BLEND-PCT.
043100
           COMPUTE H-WAGE-ADJ-PYMT-AMT =
                   H-WAGE-ADJ-PYMT-NEW + H-WAGE-ADJ-PYMT-OLD.
043400
           COMPUTE H-PYMT-AMT ROUNDED = H-WAGE-ADJ-PYMT-AMT *
                H-BMI-FACTOR * H-BSA-FACTOR * PPS-BDGT-NEUT-RATE *
                H-AGE-FACTOR * H-DRUG-ADDON.
043800
           MOVE H-PYMT-AMT                   TO CASE-MIX-FCTR-ADJ-RATE.
           MOVE SPACES                       TO COND-CD-73.
044100
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
046400
           MOVE H-PYMT-AMT                   TO PPS-FINAL-PAY-AMT.
           MOVE H-WAGE-ADJ-PYMT-AMT          TO PPS-WAGE-ADJ-RATE.
      /
       9000-MOVE-RESULTS.
046900
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
                 INITIALIZE CBSA-PCT
                 INITIALIZE MSA-PCT
                 INITIALIZE CASE-MIX-FCTR-ADJ-RATE
                 INITIALIZE CBSA-WAGE-PMT-RATE
                 INITIALIZE HEMO-CCPD-CAPD
                 INITIALIZE AGE-RETURN
              END-IF
           END-IF.
050800
      ******        L A S T   S O U R C E   S T A T E M E N T      *****
