       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCAL151.
      *AUTHOR.     CMS
      *       EFFECTIVE JANUARY 1, 2015
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
      *         -      THE VARIABLE LABELED 'P-ESRD-RATE' IS NEVER     *
      *           USED IN CALCUALTIONS SINCE THE DRIVER IS THE ONLY ONE*
      *           THAT USES IT IN SPECIAL CIRCUMSTANCES AT THE FISCAL  *
      *           INTERMEDIARIES.  THEREFORE A DUAL USE WAS MADE OF IT *
      *           SO THAT THE PC-PRICER CAN MAKE USE OF THE VERY       *
      *           LIMITED SPACE LEFT.  THE VARIABLE NAME WAS REDEFINED *
      *           TO 'CASE-MIX-FCTR-ADJ-RATE' TO REFLECT IT'S USAGE ON *
      *           THE PC-PRICER.                                       *
      *         -        USAGE OF THE THESE FILLER AREAS AND THE       *
      *           'CASE-MIX-FCTR-ADJ-RATE' VARIABLE WILL ONLY OCCUR    *
      *           WHEN A 'T' (FOR TESTING) IS IN THE LAST COLUMN OF THE*
      *           INPUT RECORD.  OTHERWISE THESE VARIABLES WILL CONTAIN*
      *           SPACES WHEN NOT IN TEST MODE.  MOST OF THE MOVES TO  *
      *           THESE VARIABLES OCCURS IN THE 9000-MOVE-RESULTS PARA-*
      *           GRAPH.  A FEW MOVES MUST OCCUR IN OTHER AREAS OF THE *
      *           PROGRAM.                                             *
      * 11/07/08- ALL CODE FOR 2006-2008 WAGE ADJUSTED PAYMENTS WERE   *
      *           REMOVED BECAUSE THERE IS NO LONGER A BLEND BETWEEN   *
      *           MSA AND CBSA STARTING 1/1/2009.                      *
      *           THE DRUG ADDON FACTOR WAS CHANGED TO 1.1520          *
      *           THE HOSPITAL AND INDEPENDENT ESRD FACILITY PAYMENT   *
      *           RATES ARE NOW THE SAME.  THE TWO VARIABLES WERE LEFT *
      *           IN THE PROGRAM IN CASE THEY DECIDE TO MAKE THEM      *
      *           DIFFERENT IN THE FUTURE.                             *
      * 12/03/08- RENAMED THIS SUBROUTINE ESCAL091 AND CHANGED THE     *
      *           APPROPRIATE VERSION INFORMATION.  THE 9.0 VERSION OF *
      *           THE PRICER WAS SENT OUT IN NOVEMBER.  AFTERWARDS THE *
      *           POLICY PEOPLE WHO SET THE WAGE INDEXES, CHANGED THEIR*
      *           MINDS ABOUT CBSA 16700 AND RESCINDED THE WAGE INDEX. *
      *           THIS NECESSITATED A RE-RELEASE OF THE ESRD PRICER IN *
      *           ORDER TO MAKE SURE THAT THE FI'S ARE USING THE LATEST*
      *           VERSION AT THE START OF CY2009.                      *
      * 11/02/09- Renamed this subroutine ESCAL100 and changed the     *
      *           appropriate version information.  Changed the        *
      *           composite rate (Hosp-Based-Pmt-Rate and Indp-ESRD-Fac*
      *           -Pmt-Rate) to 135.15.  Also changed the Drug-Addon to*
      *           1.1500.                                              *
      *         - Began reorganization of program so that it can be    *
      *           used with the Bundled rate payment system as well as *
      *           the Composite rate payment system.  In that light,   *
      *           changed WAGE-NEW-CBSA-RECORD to COM-CBSA-WAGE-RECORD *
      *           and added BUN-CBSA-WAGE-RECORD which is not used in  *
      *           this version of the program.  Changed corresponding  *
      *           variable names to agree with the new name.           *
      * 10/29/10- Renamed this subroutine ESCAL110.  This pricer       *
      *           changed from being only a Composite Rate pricer to   *
      *           processing line-items using both the Composite Rate  *
      *           methodology AND a Bundled (now called PPS) Rate      *
      *           methodology during a blend period that lasts from    *
      *           2011 to 2013.  The original composite rate code has  *
      *           been completely rewritten to conform to the new      *
      *           manner of pricing as well as being more efficient.   *
      *           There are too many changes to list from the bundled  *
      *           methodology to put here.                             *
      *           On the composite-rate side of the program, the drug  *
      *           add-on has been changed to 14.7 percent and the      *
      *           the composite rate changed to $138.53.               *
      *           Also added new variable A-49-CENT-PART-D-DRUG-ADJ    *
      *           to the composite-rate side of the calculation which  *
      *           is an adjustment for ESRD drugs and biologicals      *
      *           currently paid under Part-D.                         *
      * 03/07/11- Renamed this subroutine ESCAL116.  This was due to   *
      *           problems found in the initial version which was re-  *
      *           leased to FISS for their initial work in bringing up *
      *           the new pricer.  Future revisions may be needed.     *
      * 10/28/11- Renamed this subroutine ESCAL120.  Four changes are: *
      *           (1) fixed the onstart calculation to count from day  *
      *           one and therefore use 120 days vs the 121 days prior *
      *           calculation (which counted from the second day).     *
      *           (2) added an output variable which is the amount of  *
      *           the low volume that is added to a line-item payment  *
      *           when the provider is a low-volume provider.  This is *
      *           for recovery purposes.                               *
      *           (3) added an input variable for the Quality Incentive*
      *           Program (QIP).  The specific number in this field    *
      *           comes from FISS which maintaines the Provider        *
      *           Specific File and the number ultimately is provided  *
      *           by the Quality people at CMS.                        *
      *           (4) made necessary changes for CY 2012.  A noteworthy*
      *           change is how blended payments are calculated.  In   *
      *           2011 there was a 49 cent Part D drug adjustment.  In *
      *           2012 this 49 cent is added to the Base-Payment-Rate  *
      *           and then the 2.1 % productivity is applied.  This    *
      *           means that the base rate starts at $139.02 and after *
      *           the 2.1% increase the base rate is $141.94.          *
      *           Another change is in calculating the BSA.  The super-*
      *           script is now the same in the Composite Rate portion *
      *           of the code as it is in the PPS portion of the code. *
      * FUTURE    This program is intended to be used in a running TEN *
      *           year (current year and nine prior years) mode.       *
      *           As of 10/10, there are no plans on which variables to*
      *           remove since plans may change yet again.             *
      * ESCAL121  ESCAL120...Note that there is NO ESCAL120 calculating
      *           subroutine due to a reversing of CBSA 41980 composite
      *           & PPS WAGE INDEX which was sent out with the ESDRV120
      *           subroutine prior to the start of the calendar year.
      *           There is NO difference between the original ESCAL120
      *           and ESCAL121 except the notational difference in the
      *           version number.
      * 11/14/12- ESCAL130 - Made annual updates for CY 2013 to match
      *           specifications from the Chronic Care Policy Group.
      *           In addition, code was added to implement the
      *           assignment of the pediatric hemodialysis rate to
      *           revenue code 0881 (ultrafiltration) when reported
      *           on a pediatric claim (CR 7064 - Requirement #11).
      *           List of changes for CY 2013 -->
      *           - Composite payment rate for blended payments:
      *              changed BASE-PAYMENT-RATE to 145.20.
      *           - Drug add-on to the composite payment system for
      *             blended payments:
      *              changed DRUG-ADDON to 1.1400.
      *           - ESRD PPS base rate:
      *              changed BUNDLED-BASE-PMT-RATE to 240.36
      *              (after taking into account both -->
      *                    - the market basket update, and
      *                    - the wage index budget neutrality adjustment
      *                      factor).
      *           - ESRD PPS outlier per treatment MAP amount for
      *             adult patients:
      *              changed ADJ-AVG-MAP-AMT-GT-17 to 59.42.
      *           - ESRD PPS outlier per treatment MAP amount for
      *             pediatric patients:
      *              changed ADJ-AVG-MAP-AMT-LT-18 to 41.39.
      *           - ESRD PPS outlier threshold fixed dollar loss amount
      *             for adult patients:
      *              changed FIX-DOLLAR-LOSS-GT-17 to 110.22.
      *           - ESRD PPS outlier threshold fixed dollar loss amount
      *             for pediatric patients:
      *              changed FIX-DOLLAR-LOSS-LT-18 to 47.32.
      *           - 25 percent of the basic case-mix adjusted composite
      *             payment amount:
      *              changed COM-CBSA-BLEND-PCT to 0.25.
      *           - 75 percent of the ESRD PPS payment amount:
      *              changed BUN-CBSA-BLEND-PCT to 0.75.
      * 11-15-13 - ESCAL14B - BETA PRICER FOR TESTING ONLY
      * We have to wait until November 29th, when the ESRD Final Rule
      * is published, to release the CY 2014 ESRD Pricer to FISS
      * because the rates are sensitive.  However, we will release a
      * BETA version to FISS by November 15th so they can test the
      * logic changes.
      * The BETA release should have the following characteristics:
      * - Contain a "B" in the file name(s) to indicate it's the 2014
      *   BETA version
      * - Contain the CY 2013 rates
      * - Contain a modified CY 2013 wage index table
      *       o Add the three Pacific Rim CBSAs to the table
      *         (CBSAs 64, 65, and 66)
      *       o Assign the three Pacific Rim CBSAs the CY 2013
      *         Puerto Rico (CBSA 40) wage index value
      * - Include new logic that forces all providers to receive the
      *   100% PPS payment (blend logic removed/disabled)
      *      TO MAKE SURE THAT ALL BILLS ARE 100% PPS
      *      ADDED LINE TO MOVE 'Y' TO P-PROV-WAIVE-BLEND-PAY-INDIC.
      * - Include new logic that directs Pacific Rim providers to
      *   flow through the pricing calculation as all other providers
      * The BETA Pricer release memo/email should emphasize the fact
      * that this version and the rates therein are for testing only.
      *
      * 11/15/13- ESCAL140 - Made annual updates for CY 2014 to match
      * specifications from the Chronic Care Policy Group.
      * In addition, code was changed in the following two ways:
      * - Included new log that forces all providers to
      *   receive the 100% PPS payment (blend logic is
      *   removed/disabled)
      * - Included new logic that directs Pacific Rim providers
      *   to flow through the pricing calculation as all other
      *   providers
      * List of changes for CY 2014 -->
      *      - Changed BUNDLED-BASE-PMT-RATE to 239.02.
      *      - Changed ADJ-AVG-MAP-AMT-GT-17 to 50.25.
      *      - Changed ADJ-AVG-MAP-AMT-LT-18 to 40.49.
      *      - Changed FIX-DOLLAR-LOSS-GT-17 to 98.67.
      *      - Changed FIX-DOLLAR-LOSS-LT-18 to 47.32.
      *      - Changed TRAINING-ADD-ON-PMT-AMT to 50.16.
      *      - Kept the following changes from the CY 2014 BETA
      *              - COM-CBSA-BLEND-PCT = 0.00.
      *              - BUN-CBSA-BLEND-PCT = 1.00.
      *
      * 11/15/14- ESCAL150 - Made annual updates for CY 2015 to match
      * specifications from the Chronic Care Policy Group.
      * List of changes for CY 2015 -->
      *      - ESRD PPS base rate
      *          Changed BUNDLED-BASE-PMT-RATE to 239.43.
      *           includes Wage Index Budget Neutrality Adjustment
      *           Factor of 1.001729
      *      - Labor-related share
      *          Changed BUN-NAT-LABOR-PCT to 0.46205
      *           Implementation of the revised
      *           labor-related share with a 50/50
      *           blend under a 2-year transition
      *           results in a labor-related share
      *           value of 46.205 percent for CY 2015.
      *      - Non-labor-related share
      *          Changed BUN-NAT-NONLABOR-PCT to 0.53795
      *           Non-labor-related share =
      *            1 - Labor-related share
      *      - Changed ADJ-AVG-MAP-AMT-GT-17 to 51.29.
      *          For adult patients, the adjusted
      *          average outlier service MAP
      *          amount per treatment
      *      - Changed ADJ-AVG-MAP-AMT-LT-18 to 43.57.
      *          For pediatric patients, the
      *          adjusted average outlier service
      *          MAP amount per treatment
      *      - Changed FIX-DOLLAR-LOSS-GT-17 to 86.19.
      *          The fixed dollar loss amount for adult patients
      *      - Changed FIX-DOLLAR-LOSS-LT-18 to 54.35.
      *          The fixed dollar loss amount for pediatric patients
      * 12/23/14 ESCAL151 WASN'T CHANGED EXCEPT TO ALLOW FOR NEW VERSION
      * DUE TO CHANGES TO ESDRV151 TO IMPLEMENT SEARCH FOR SPECIAL WAGE
      * INDEXES FOR CHILDREN'S HOSPITALS
      ******************************************************************
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.            IBM-Z990.
       OBJECT-COMPUTER.            IBM-Z990.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
027300
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.
       01  W-STORAGE-REF                  PIC X(46) VALUE
           'ESCAL151      - W O R K I N G   S T O R A G E'.
       01  CAL-VERSION                    PIC X(05) VALUE 'C15.1'.
028100
       01  DISPLAY-LINE-MEASUREMENT.
           05  FILLER                     PIC X(50) VALUE
               '....:...10....:...20....:...30....:...40....:...50'.
           05  FILLER                     PIC X(50) VALUE
               '....:...60....:...70....:...80....:...90....:..100'.
           05  FILLER                     PIC X(20) VALUE
               '....:..110....:..120'.
028900
       01  PRINT-LINE-MEASUREMENT.
           05  FILLER                     PIC X(51) VALUE
               'X....:...10....:...20....:...30....:...40....:...50'.
           05  FILLER                     PIC X(50) VALUE
               '....:...60....:...70....:...80....:...90....:..100'.
           05  FILLER                     PIC X(32) VALUE
               '....:..110....:..120....:..130..'.
      /
      ******************************************************************
      *  This area contains all of the old Composite Rate variables.   *
      * They will be eliminated when the transition period ends - 2014 *
      ******************************************************************
       01  HOLD-COMP-RATE-PPS-COMPONENTS.
           05  H-PAYMENT-RATE             PIC 9(04)V9(02).
           05  H-PYMT-AMT                 PIC 9(04)V9(02).
           05  H-WAGE-ADJ-PYMT-AMT        PIC 9(04)V9(02).
           05  H-PATIENT-AGE              PIC 9(03).
           05  H-AGE-FACTOR               PIC 9(01)V9(03).
           05  H-BSA-FACTOR               PIC 9(01)V9(04).
           05  H-BMI-FACTOR               PIC 9(01)V9(04).
           05  H-BSA                      PIC 9(03)V9(04).
           05  H-BMI                      PIC 9(03)V9(04).
           05  HGT-PART                   PIC 9(04)V9(08).
           05  WGT-PART                   PIC 9(04)V9(08).
           05  COMBINED-PART              PIC 9(04)V9(08).
           05  CALC-BSA                   PIC 9(04)V9(08).
031600
031700
      * The following two variables will change from year to year
      * and are used for the COMPOSITE part of the Bundled Pricer.
       01  DRUG-ADDON                     PIC 9(01)V9(04) VALUE 1.1400.
       01  BASE-PAYMENT-RATE              PIC 9(04)V9(02) VALUE 145.20.
032200
      * The next two percentages MUST add up to 1 (i.e. 100%)
      * They will continue to change until CY2009 when CBSA will be 1.00
       01  MSA-BLEND-PCT                  PIC 9(01)V9(02) VALUE 0.00.
       01  CBSA-BLEND-PCT                 PIC 9(01)V9(02) VALUE 1.00.
032700
      * CONSTANTS AREA
      * The next two percentages MUST add up TO 1 (i.e. 100%)
       01  NAT-LABOR-PCT                  PIC 9(01)V9(05) VALUE 0.53711.
       01  NAT-NONLABOR-PCT               PIC 9(01)V9(05) VALUE 0.46289.
033200
      * The next variable is only applicapable for the 2011 Pricer.
       01  A-49-CENT-PART-D-DRUG-ADJ      PIC 9(01)V9(02) VALUE 0.49.
033500
       01  HEMO-PERI-CCPD-AMT             PIC 9(02)       VALUE 20.
       01  CAPD-AMT                       PIC 9(02)       VALUE 12.
       01  CAPD-OR-CCPD-FACTOR            PIC 9(01)V9(06) VALUE
                                                               0.428571.
      * The above number technically represents the fractional
      * number 3/7 which is three days per week that a person can
      * receive dialysis.  It will remain this value ONLY for the
      * COMPOSITe side of the Bundled Pricer.  The Bundled portion will
      * use the calculation method which is more understandable and
      * follows the method used by the Policy folks.
034600
      *  The following number that is loaded into the payment equation
      *  is meant to BUDGET NEUTRALIZE changes in THE CASE MIX INDEX
      *  and   --DOES NOT CHANGE--
035000
       01  CASE-MIX-BDGT-NEUT-FACTOR      PIC 9(01)V9(04) VALUE 0.9116.
035200
       01  COMPOSITE-RATE-MULTIPLIERS.
      *Composite rate payment multiplier (used for blended providers)
           05  CR-AGE-LT-18           PIC 9(01)V9(03) VALUE 1.620.
           05  CR-AGE-18-44           PIC 9(01)V9(03) VALUE 1.223.
           05  CR-AGE-45-59           PIC 9(01)V9(03) VALUE 1.055.
           05  CR-AGE-60-69           PIC 9(01)V9(03) VALUE 1.000.
           05  CR-AGE-70-79           PIC 9(01)V9(03) VALUE 1.094.
           05  CR-AGE-80-PLUS         PIC 9(01)V9(03) VALUE 1.174.
036100
           05  CR-BSA                 PIC 9(01)V9(03) VALUE 1.037.
           05  CR-BMI-LT-18-5         PIC 9(01)V9(03) VALUE 1.112.
      /
      ******************************************************************
      *    This area contains all of the NEW Bundled Rate variables.   *
      ******************************************************************
       01  HOLD-BUNDLED-PPS-COMPONENTS.
           05  H-BUN-NAT-LABOR-AMT        PIC 9(04)V9(02).
           05  H-BUN-NAT-NONLABOR-AMT     PIC 9(04)V9(02).
           05  H-BUN-BASE-WAGE-AMT        PIC 9(04)V9(04).
           05  H-BUN-AGE-FACTOR           PIC 9(01)V9(03).
           05  H-BUN-BSA                  PIC 9(03)V9(04).
           05  H-BUN-BSA-FACTOR           PIC 9(01)V9(04).
           05  H-BUN-BMI                  PIC 9(03)V9(04).
           05  H-BUN-BMI-FACTOR           PIC 9(01)V9(04).
           05  H-BUN-ONSET-FACTOR         PIC 9(01)V9(04).
           05  H-BUN-COMORBID-MULTIPLIER  PIC 9(01)V9(03).
           05  H-BUN-ADJUSTED-BASE-WAGE-AMT
                                          PIC 9(07)V9(04).
           05  H-BUN-WAGE-ADJ-TRAINING-AMT
                                          PIC 9(07)V9(04).
           05  H-CC-74-PER-DIEM-AMT       PIC 9(07)V9(04).
           05  H-HEMO-EQUIV-DIAL-SESSIONS PIC 9(07)V9(04).
           05  H-PPS-FINAL-PAY-AMT        PIC 9(07)V9(02).
           05  H-FULL-CLAIM-AMT           PIC 9(07)V9(02).
           05  H-LV-BUN-ADJUST-BASE-WAGE-AMT
                                          PIC 9(07)V9(04).
           05  H-LV-PPS-FINAL-PAY-AMT     PIC 9(07)V9(04).
           05  H-LV-OUT-PREDICT-SERVICES-MAP
                                          PIC 9(07)V9(04).
           05  H-LV-OUT-CM-ADJ-PREDICT-M-TRT
                                          PIC 9(07)V9(04).
           05  H-LV-OUT-PREDICTED-MAP
                                          PIC 9(07)V9(04).
           05  H-LV-OUT-PAYMENT           PIC 9(07)V9(04).
039700
           05  H-COMORBID-MULTIPLIER      PIC 9(01)V9(03).
           05  IS-HIGH-COMORBID-FOUND     PIC X(01).
               88  HIGH-COMORBID-FOUND               VALUE 'Y'.
040100
           05  H-COMORBID-DATA  OCCURS 6 TIMES
                  INDEXED BY H-COMORBID-INDEX
                                          PIC X(02).
           05  H-COMORBID-CWF-CODE        PIC X(02).
040600
           05  H-BUN-LOW-VOL-MULTIPLIER   PIC 9(01)V9(03).
040800
           05  QIP-REDUCTION              PIC 9(01)V9(03).
           05  SUB                        PIC 9(04).
041100
           05  THE-DATE                   PIC 9(08).
           05  INTEGER-LINE-ITEM-DATE     PIC S9(09).
           05  INTEGER-DIALYSIS-DATE      PIC S9(09).
           05  ONSET-DATE                 PIC 9(08).
           05  MOVED-CORMORBIDS           PIC X(01).
041700
       01  HOLD-OUTLIER-PPS-COMPONENTS.
           05  H-OUT-AGE-FACTOR           PIC 9(01)V9(03).
           05  H-OUT-BSA                  PIC 9(03)V9(04).
           05  H-OUT-BSA-FACTOR           PIC 9(01)V9(04).
           05  H-OUT-BMI                  PIC 9(03)V9(04).
           05  H-OUT-BMI-FACTOR           PIC 9(01)V9(04).
           05  H-OUT-ONSET-FACTOR         PIC 9(01)V9(04).
           05  H-OUT-COMORBID-MULTIPLIER  PIC 9(01)V9(03).
           05  H-OUT-LOW-VOL-MULTIPLIER   PIC 9(01)V9(03).
           05  H-OUT-ADJ-AVG-MAP-AMT      PIC 9(03)V9(02).
           05  H-OUT-FIX-DOLLAR-LOSS      PIC 9(04)V9(02).
           05  H-OUT-LOSS-SHARING-PCT     PIC 9(01)V9(02).
           05  H-OUT-PREDICTED-SERVICES-MAP
                                          PIC 9(07)V9(04).
           05  H-OUT-IMPUTED-MAP          PIC 9(07)V9(04).
           05  H-OUT-CM-ADJ-PREDICT-MAP-TRT
                                          PIC 9(07)V9(04).
           05  H-OUT-PREDICTED-MAP        PIC 9(07)V9(04).
           05  H-OUT-PAYMENT              PIC 9(07)V9(04).
           05  H-OUT-HEMO-EQUIV-PAYMENT   PIC 9(07)V9(04).
043800
043900
      * The following variable will change from year to year and is
      * used for the BUNDLED part of the Bundled Pricer.
       01  BUNDLED-BASE-PMT-RATE          PIC 9(04)V9(02) VALUE 239.43.
044300
      * The next two percentages MUST add up to 1 (i.e. 100%)
      * They start in 2011 and will continue to change until CY2014 when
      * BUN-CBSA-BLEND-PCT will be 1.00
      * The third blend percent is for those providers that waived the
      * blended percent and went to full PPS.  This variable will be
      * eliminated in 2014 when it is no longer needed.
       01  COM-CBSA-BLEND-PCT             PIC 9(01)V9(02) VALUE 0.00.
       01  BUN-CBSA-BLEND-PCT             PIC 9(01)V9(02) VALUE 1.00.
       01  WAIVE-CBSA-BLEND-PCT           PIC 9(01)V9(02) VALUE 1.00.
045300
      * CONSTANTS AREA
      * The next two percentages MUST add up TO 1 (i.e. 100%)
       01  BUN-NAT-LABOR-PCT              PIC 9(01)V9(05) VALUE 0.46205.
       01  BUN-NAT-NONLABOR-PCT           PIC 9(01)V9(05) VALUE 0.53795.
       01  TRAINING-ADD-ON-PMT-AMT        PIC 9(02)V9(02) VALUE 50.16.
045900
      *  The following number that is loaded into the payment equation
      *  is meant to BUDGET NEUTRALIZE changes in the bundled case-mix
      *  and   --DOES NOT CHANGE--
046300
       01  TRANSITION-BDGT-NEUT-FACTOR    PIC 9(01)V9(04) VALUE 0.9690.
046500
       01  PEDIATRIC-MULTIPLIERS.
      *Separately billable payment multiplier (used for outliers)
           05  PED-SEP-BILL-PAY-MULTI.
               10  SB-AGE-LT-13-PD-MODE   PIC 9(01)V9(03) VALUE 0.319.
               10  SB-AGE-LT-13-HEMO-MODE PIC 9(01)V9(03) VALUE 1.185.
               10  SB-AGE-13-17-PD-MODE   PIC 9(01)V9(03) VALUE 0.476.
               10  SB-AGE-13-17-HEMO-MODE PIC 9(01)V9(03) VALUE 1.459.
           05  PED-EXPAND-BUNDLE-PAY-MULTI.
      *Expanded bundle payment multiplier (used for normal billing)
               10  EB-AGE-LT-13-PD-MODE   PIC 9(01)V9(03) VALUE 1.033.
               10  EB-AGE-LT-13-HEMO-MODE PIC 9(01)V9(03) VALUE 1.219.
               10  EB-AGE-13-17-PD-MODE   PIC 9(01)V9(03) VALUE 1.067.
               10  EB-AGE-13-17-HEMO-MODE PIC 9(01)V9(03) VALUE 1.277.
047900
       01  ADULT-MULTIPLIERS.
      *Separately billable payment multiplier (used for outliers)
           05  SEP-BILLABLE-PAYMANT-MULTI.
               10  SB-AGE-18-44           PIC 9(01)V9(03) VALUE 0.996.
               10  SB-AGE-45-59           PIC 9(01)V9(03) VALUE 0.992.
               10  SB-AGE-60-69           PIC 9(01)V9(03) VALUE 1.000.
               10  SB-AGE-70-79           PIC 9(01)V9(03) VALUE 0.963.
               10  SB-AGE-80-PLUS         PIC 9(01)V9(03) VALUE 0.915.
               10  SB-BSA                 PIC 9(01)V9(03) VALUE 1.014.
               10  SB-BMI-LT-18-5         PIC 9(01)V9(03) VALUE 1.078.
               10  SB-ONSET-LE-120        PIC 9(01)V9(03) VALUE 1.450.
               10  SB-PERICARDITIS        PIC 9(01)V9(03) VALUE 1.354.
               10  SB-PNEUMONIA           PIC 9(01)V9(03) VALUE 1.422.
               10  SB-GI-BLEED            PIC 9(01)V9(03) VALUE 1.571.
               10  SB-SICKEL-CELL         PIC 9(01)V9(03) VALUE 1.225.
               10  SB-MYELODYSPLASTIC     PIC 9(01)V9(03) VALUE 1.309.
               10  SB-MONOCLONAL-GAMM     PIC 9(01)V9(03) VALUE 1.074.
               10  SB-LOW-VOL-ADJ-LT-4000 PIC 9(01)V9(03) VALUE 0.975.
      *Case-Mix adjusted payment multiplier (used for normal billing)
           05  CASE-MIX-PAYMENT-MULTI.
               10  CM-AGE-18-44           PIC 9(01)V9(03) VALUE 1.171.
               10  CM-AGE-45-59           PIC 9(01)V9(03) VALUE 1.013.
               10  CM-AGE-60-69           PIC 9(01)V9(03) VALUE 1.000.
               10  CM-AGE-70-79           PIC 9(01)V9(03) VALUE 1.011.
               10  CM-AGE-80-PLUS         PIC 9(01)V9(03) VALUE 1.016.
               10  CM-BSA                 PIC 9(01)V9(03) VALUE 1.020.
               10  CM-BMI-LT-18-5         PIC 9(01)V9(03) VALUE 1.025.
               10  CM-ONSET-LE-120        PIC 9(01)V9(03) VALUE 1.510.
               10  CM-PERICARDITIS        PIC 9(01)V9(03) VALUE 1.114.
               10  CM-PNEUMONIA           PIC 9(01)V9(03) VALUE 1.135.
               10  CM-GI-BLEED            PIC 9(01)V9(03) VALUE 1.183.
               10  CM-SICKEL-CELL         PIC 9(01)V9(03) VALUE 1.072.
               10  CM-MYELODYSPLASTIC     PIC 9(01)V9(03) VALUE 1.099.
               10  CM-MONOCLONAL-GAMM     PIC 9(01)V9(03) VALUE 1.024.
               10  CM-LOW-VOL-ADJ-LT-4000 PIC 9(01)V9(03) VALUE 1.189.
051500
       01  OUTLIER-SB-CALC-AMOUNTS.
           05  ADJ-AVG-MAP-AMT-LT-18      PIC 9(04)V9(02) VALUE 43.57.
           05  ADJ-AVG-MAP-AMT-GT-17      PIC 9(04)V9(02) VALUE 51.29.
           05  FIX-DOLLAR-LOSS-LT-18      PIC 9(04)V9(02) VALUE 54.35.
           05  FIX-DOLLAR-LOSS-GT-17      PIC 9(04)V9(02) VALUE 86.19.
           05  LOSS-SHARING-PCT-LT-18     PIC 9(03)V9(02) VALUE 0.80.
           05  LOSS-SHARING-PCT-GT-17     PIC 9(03)V9(02) VALUE 0.80.
      /
      ******************************************************************
      *    This area contains return code variables and their codes.   *
      ******************************************************************
       01 PAID-RETURN-CODE-TRACKERS.
           05  OUTLIER-TRACK              PIC X(01).
           05  ACUTE-COMORBID-TRACK       PIC X(01).
           05  CHRONIC-COMORBID-TRACK     PIC X(01).
           05  ONSET-TRACK                PIC X(01).
           05  LOW-VOLUME-TRACK           PIC X(01).
           05  TRAINING-TRACK             PIC X(01).
           05  PEDIATRIC-TRACK            PIC X(01).
           05  LOW-BMI-TRACK              PIC X(01).
       COPY RTCCPY.
      *COPY "RTCCPY.CPY".
      *                                                                *
      *  Legal combinations of adjustments for ADULTS are:             *
      *     if NO ONSET applies, then they can have any combination of:*
      *       acute OR chronic comorbid, & outlier, low vol., training.*
      *     if ONSET applies, then they can have:                      *
      *           outlier and/or low volume.                           *
      *  Legal combinations of adjustments for PEDIATRIC are:          *
      *     outlier and/or training.                                   *
      *                                                                *
      *  Illegal combinations of adjustments for PEDIATRIC are:        *
      *     pediatric with comorbid, onset, low volume, BSA, or BMI.   *
      *     onset     with comorbid or training.                       *
      *  Illegal combinations of adjustments for ANYONE are:           *
      *     acute comorbid AND chronic comorbid.                       *
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
                                 COM-CBSA-WAGE-RECORD
                                 BUN-CBSA-WAGE-RECORD.
056500
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
058700
       0000-START-TO-FINISH.
           INITIALIZE PPS-DATA-ALL.
059000
      * TO MAKE SURE THAT ALL BILLS ARE 100% PPS
           MOVE 'Y' TO P-PROV-WAIVE-BLEND-PAY-INDIC.
059300
           IF BUNDLED-TEST THEN
              INITIALIZE BILL-DATA-TEST
              INITIALIZE COND-CD-73
           END-IF.
           MOVE CAL-VERSION                  TO PPS-CALC-VERS-CD.
           MOVE ZEROS                        TO PPS-RTC.
060000
           PERFORM 1000-VALIDATE-BILL-ELEMENTS.
060200
           IF PPS-RTC = 00  THEN
              PERFORM 1200-INITIALIZATION
      **Calculate patient age
              COMPUTE H-PATIENT-AGE = B-THRU-CCYY - B-DOB-CCYY
              IF B-DOB-MM > B-THRU-MM  THEN
                 COMPUTE H-PATIENT-AGE = H-PATIENT-AGE - 1
              END-IF
              IF H-PATIENT-AGE < 18  THEN
                 MOVE "Y"                    TO PEDIATRIC-TRACK
              END-IF
              PERFORM 2000-CALCULATE-BUNDLED-FACTORS
              IF P-PROV-WAIVE-BLEND-PAY-INDIC = 'N'  THEN
                 PERFORM 5000-CALC-COMP-RATE-FACTORS
              END-IF
              PERFORM 9000-SET-RETURN-CODE
              PERFORM 9100-MOVE-RESULTS
           END-IF.
062000
           GOBACK.
      /
       1000-VALIDATE-BILL-ELEMENTS.
           IF P-PROV-TYPE = '40'  OR  '41' OR '05'  THEN
              NEXT SENTENCE
           ELSE
              MOVE 52                        TO PPS-RTC
           END-IF.
062900
           IF PPS-RTC = 00  THEN
              IF P-SPEC-PYMT-IND NOT = '1' AND ' '  THEN
                 MOVE 53                     TO PPS-RTC
              END-IF
           END-IF.
063500
           IF PPS-RTC = 00  THEN
              IF (B-DOB-DATE = ZERO)  OR  (B-DOB-DATE NOT NUMERIC)  THEN
                 MOVE 54                     TO PPS-RTC
              END-IF
           END-IF.
064100
           IF PPS-RTC = 00  THEN
              IF (B-PATIENT-WGT = 0)  OR  (B-PATIENT-WGT NOT NUMERIC)
                 MOVE 55                     TO PPS-RTC
              END-IF
           END-IF.
064700
           IF PPS-RTC = 00  THEN
              IF (B-PATIENT-HGT = 0)  OR  (B-PATIENT-HGT NOT NUMERIC)
                 MOVE 56                     TO PPS-RTC
              END-IF
           END-IF.
065300
           IF PPS-RTC = 00  THEN
              IF B-REV-CODE  = '0821' OR '0831' OR '0841' OR '0851'
                                      OR '0881'
                 NEXT SENTENCE
              ELSE
                 MOVE 57                     TO PPS-RTC
              END-IF
           END-IF.
066200
           IF PPS-RTC = 00  THEN
              IF B-COND-CODE NOT = '73' AND '74' AND '  '
                 MOVE 58                     TO PPS-RTC
              END-IF
           END-IF.
066800
           IF PPS-RTC = 00  THEN
              IF P-QIP-REDUCTION NOT = '1' AND '2' AND '3' AND '4' AND
                                       ' '  THEN
                 MOVE 53                     TO PPS-RTC
      *  This RTC is for the Special Payment Indicator not = '1' or
      *  blank, which closely approximates the intent of the edit check.
      *  I propose to make this a PPS-RTC = 59 in 2013 version of Pricer
              END-IF
           END-IF.
067800
           IF PPS-RTC = 00  THEN
              IF B-PATIENT-HGT > 300.00
                 MOVE 71                     TO PPS-RTC
              END-IF
           END-IF.
068400
           IF PPS-RTC = 00  THEN
              IF B-PATIENT-WGT > 500.00  THEN
                 MOVE 72                     TO PPS-RTC
              END-IF
           END-IF.
069000
      * Before 2012 pricer, put in edit check to make sure that the
      * # of sesions does not exceed the # of days in a month.  Maybe
      * the # of cays in a month minus one when patient goes into a
      * dialysis center for dialysis (i.e. CC = 74 and rev-cd = (0841
      * or 0851)).  If done, then will need extra RTC.
           IF PPS-RTC = 00  THEN
              IF (B-CLAIM-NUM-DIALYSIS-SESSIONS = ZERO) OR
                 (B-CLAIM-NUM-DIALYSIS-SESSIONS NOT NUMERIC)  THEN
                 MOVE 73                     TO PPS-RTC
              END-IF
           END-IF.
070200
           IF PPS-RTC = 00  THEN
              IF (B-LINE-ITEM-DATE-SERVICE = ZERO) OR
                 (B-LINE-ITEM-DATE-SERVICE NOT NUMERIC)  THEN
                 MOVE 74                     TO PPS-RTC
              END-IF
           END-IF.
070900
           IF PPS-RTC = 00  THEN
              IF (B-DIALYSIS-START-DATE NOT NUMERIC)  THEN
                 MOVE 75                     TO PPS-RTC
              END-IF
           END-IF.
071500
           IF PPS-RTC = 00  THEN
              IF (B-TOT-PRICE-SB-OUTLIER NOT NUMERIC) THEN
                 MOVE 76                     TO PPS-RTC
              END-IF
           END-IF.
072100
           IF PPS-RTC = 00  THEN
              IF (COMORBID-CWF-RETURN-CODE = SPACES) OR
                  VALID-COMORBID-CWF-RETURN-CD       THEN
                 NEXT SENTENCE
              ELSE
                 MOVE 81                     TO PPS-RTC
              END-IF
           END-IF.
      /
       1200-INITIALIZATION.
           INITIALIZE HOLD-COMP-RATE-PPS-COMPONENTS.
           INITIALIZE HOLD-BUNDLED-PPS-COMPONENTS.
           INITIALIZE HOLD-OUTLIER-PPS-COMPONENTS.
           INITIALIZE PAID-RETURN-CODE-TRACKERS.
073600
           MOVE SPACES                       TO MOVED-CORMORBIDS.
073800
           IF P-QIP-REDUCTION = ' '  THEN
      * no reduction
              MOVE 1.000 TO QIP-REDUCTION
           ELSE
              IF P-QIP-REDUCTION = '1'  THEN
      * one-half percent reduction
                 MOVE 0.995 TO QIP-REDUCTION
              ELSE
                 IF P-QIP-REDUCTION = '2'  THEN
      * one percent reduction
                    MOVE 0.990 TO QIP-REDUCTION
                 ELSE
                    IF P-QIP-REDUCTION = '3'  THEN
      * one and one-half percent reduction
                       MOVE 0.985 TO QIP-REDUCTION
                    ELSE
      * two percent reduction
                       MOVE 0.980 TO QIP-REDUCTION
                    END-IF
                 END-IF
              END-IF
           END-IF.
076100
      *    Since pricer has to pay a comorbid condition according to the
      * return code that CWF passes back, it is cleaner if the pricer
      * sets aside whatever comorbid data exists on the line-item when
      * it comes into the pricer and then transferrs the CWF code to
      * the appropriate place in the comorbid data.  This avoids
      * making convoluted changes in the other parts of the program
      * which has to look at both original comorbid data AND CWF return
      * codes to handle comorbids.  Near the end of the program where
      * variables are transferred to the output, the original comorbid
      * data is put back into its original place as though nothing
      * occurred.
           IF COMORBID-CWF-RETURN-CODE = SPACES  THEN
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                       TO MOVED-CORMORBIDS
              MOVE COMORBID-DATA (1)         TO H-COMORBID-DATA (1)
              MOVE COMORBID-DATA (2)         TO H-COMORBID-DATA (2)
              MOVE COMORBID-DATA (3)         TO H-COMORBID-DATA (3)
              MOVE COMORBID-DATA (4)         TO H-COMORBID-DATA (4)
              MOVE COMORBID-DATA (5)         TO H-COMORBID-DATA (5)
              MOVE COMORBID-DATA (6)         TO H-COMORBID-DATA (6)
              MOVE COMORBID-CWF-RETURN-CODE  TO H-COMORBID-CWF-CODE
              IF COMORBID-CWF-RETURN-CODE = '10'  THEN
                 MOVE SPACES                 TO COMORBID-DATA (1)
                                                COMORBID-DATA (2)
                                                COMORBID-DATA (3)
                                                COMORBID-DATA (4)
                                                COMORBID-DATA (5)
                                                COMORBID-DATA (6)
                                                COMORBID-CWF-RETURN-CODE
              ELSE
                 IF COMORBID-CWF-RETURN-CODE = '20'  THEN
                    MOVE 'MA'                TO COMORBID-DATA (1)
                    MOVE SPACES              TO COMORBID-DATA (2)
                                                COMORBID-DATA (3)
                                                COMORBID-DATA (4)
                                                COMORBID-DATA (5)
                                                COMORBID-DATA (6)
                                                COMORBID-CWF-RETURN-CODE
                 ELSE
                    IF COMORBID-CWF-RETURN-CODE = '30'  THEN
                       MOVE SPACES           TO COMORBID-DATA (1)
                       MOVE 'MB'             TO COMORBID-DATA (2)
                       MOVE SPACES           TO COMORBID-DATA (3)
                       MOVE SPACES           TO COMORBID-DATA (4)
                       MOVE SPACES           TO COMORBID-DATA (5)
                       MOVE SPACES           TO COMORBID-DATA (6)
                                                COMORBID-CWF-RETURN-CODE
                    ELSE
                       IF COMORBID-CWF-RETURN-CODE = '40'  THEN
                          MOVE SPACES        TO COMORBID-DATA (1)
                          MOVE SPACES        TO COMORBID-DATA (2)
                          MOVE 'MC'          TO COMORBID-DATA (3)
                          MOVE SPACES        TO COMORBID-DATA (4)
                          MOVE SPACES        TO COMORBID-DATA (5)
                          MOVE SPACES        TO COMORBID-DATA (6)
                                                COMORBID-CWF-RETURN-CODE
                       ELSE
                          IF COMORBID-CWF-RETURN-CODE = '50'  THEN
                             MOVE SPACES     TO COMORBID-DATA (1)
                             MOVE SPACES     TO COMORBID-DATA (2)
                             MOVE SPACES     TO COMORBID-DATA (3)
                             MOVE 'MD'       TO COMORBID-DATA (4)
                             MOVE SPACES     TO COMORBID-DATA (5)
                             MOVE SPACES     TO COMORBID-DATA (6)
                                                COMORBID-CWF-RETURN-CODE
                          ELSE
                             IF COMORBID-CWF-RETURN-CODE = '60'  THEN
                                MOVE SPACES  TO COMORBID-DATA (1)
                                MOVE SPACES  TO COMORBID-DATA (2)
                                MOVE SPACES  TO COMORBID-DATA (3)
                                MOVE SPACES  TO COMORBID-DATA (4)
                                MOVE 'ME'    TO COMORBID-DATA (5)
                                MOVE SPACES  TO COMORBID-DATA (6)
                                                COMORBID-CWF-RETURN-CODE
                             ELSE
                                MOVE SPACES  TO COMORBID-DATA (1)
                                                COMORBID-DATA (2)
                                                COMORBID-DATA (3)
                                                COMORBID-DATA (4)
                                                COMORBID-DATA (5)
                                                COMORBID-CWF-RETURN-CODE
                                MOVE 'MF'    TO COMORBID-DATA (6)
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
085200
      ******************************************************************
      ***Calculate BUNDLED Wage Adjusted Rate (note different method)***
      ******************************************************************
           COMPUTE H-BUN-NAT-LABOR-AMT ROUNDED =
              (BUNDLED-BASE-PMT-RATE * BUN-NAT-LABOR-PCT) *
               BUN-CBSA-W-INDEX.
085900
           COMPUTE H-BUN-NAT-NONLABOR-AMT ROUNDED =
              BUNDLED-BASE-PMT-RATE * BUN-NAT-NONLABOR-PCT
086200
           COMPUTE H-BUN-BASE-WAGE-AMT ROUNDED =
              H-BUN-NAT-LABOR-AMT + H-BUN-NAT-NONLABOR-AMT.
      /
       2000-CALCULATE-BUNDLED-FACTORS.
      ******************************************************************
      ***  Set BUNDLED age adjustment factor                         ***
      ******************************************************************
           IF H-PATIENT-AGE < 13  THEN
              IF B-REV-CODE = '0821' OR '0881' THEN
                 MOVE EB-AGE-LT-13-HEMO-MODE TO H-BUN-AGE-FACTOR
              ELSE
                 MOVE EB-AGE-LT-13-PD-MODE   TO H-BUN-AGE-FACTOR
              END-IF
           ELSE
              IF H-PATIENT-AGE < 18 THEN
                 IF B-REV-CODE = '0821' OR '0881' THEN
                    MOVE EB-AGE-13-17-HEMO-MODE
                                             TO H-BUN-AGE-FACTOR
                 ELSE
                    MOVE EB-AGE-13-17-PD-MODE
                                             TO H-BUN-AGE-FACTOR
                 END-IF
              ELSE
                 IF H-PATIENT-AGE < 45  THEN
                    MOVE CM-AGE-18-44        TO H-BUN-AGE-FACTOR
                 ELSE
                    IF H-PATIENT-AGE < 60  THEN
                       MOVE CM-AGE-45-59     TO H-BUN-AGE-FACTOR
                    ELSE
                       IF H-PATIENT-AGE < 70  THEN
                          MOVE CM-AGE-60-69  TO H-BUN-AGE-FACTOR
                       ELSE
                          IF H-PATIENT-AGE < 80  THEN
                             MOVE CM-AGE-70-79
                                             TO H-BUN-AGE-FACTOR
                          ELSE
                             MOVE CM-AGE-80-PLUS
                                             TO H-BUN-AGE-FACTOR
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
090700
      ******************************************************************
      ***  Calculate BUNDLED BSA factor (note NEW formula)           ***
      ******************************************************************
           COMPUTE H-BUN-BSA  ROUNDED = (.007184 *
               (B-PATIENT-HGT ** .725) * (B-PATIENT-WGT ** .425))
091300
           IF H-PATIENT-AGE > 17  THEN
              COMPUTE H-BUN-BSA-FACTOR  ROUNDED =
                   CM-BSA ** ((H-BUN-BSA - 1.87) / .1)
           ELSE
              MOVE 1.000                     TO H-BUN-BSA-FACTOR
           END-IF.
092000
      ******************************************************************
      ***  Calculate BUNDLED BMI factor                              ***
      ******************************************************************
           COMPUTE H-BUN-BMI  ROUNDED = (B-PATIENT-WGT /
               (B-PATIENT-HGT ** 2)) * 10000.
092600
           IF (H-PATIENT-AGE > 17) AND (H-BUN-BMI < 18.5)  THEN
              MOVE CM-BMI-LT-18-5            TO H-BUN-BMI-FACTOR
              MOVE "Y"                       TO LOW-BMI-TRACK
           ELSE
              MOVE 1.000                     TO H-BUN-BMI-FACTOR
           END-IF.
093300
      ******************************************************************
      ***  Calculate BUNDLED ONSET factor                            ***
      ******************************************************************
           IF B-DIALYSIS-START-DATE > ZERO  THEN
              MOVE B-LINE-ITEM-DATE-SERVICE  TO THE-DATE
              COMPUTE INTEGER-LINE-ITEM-DATE =
                  FUNCTION INTEGER-OF-DATE(THE-DATE)
              MOVE B-DIALYSIS-START-DATE     TO THE-DATE
              COMPUTE INTEGER-DIALYSIS-DATE  =
                  FUNCTION INTEGER-OF-DATE(THE-DATE)
      * Need to add one to onset-date because the start date should
      * be included in the count of days.  fix made 9/6/2011
              COMPUTE ONSET-DATE = (INTEGER-LINE-ITEM-DATE -
                                    INTEGER-DIALYSIS-DATE) + 1
              IF H-PATIENT-AGE > 17  THEN
                 IF ONSET-DATE > 120  THEN
                    MOVE 1                   TO H-BUN-ONSET-FACTOR
                 ELSE
                    MOVE CM-ONSET-LE-120     TO H-BUN-ONSET-FACTOR
                    MOVE "Y"                 TO ONSET-TRACK
                 END-IF
              ELSE
                 MOVE 1                      TO H-BUN-ONSET-FACTOR
              END-IF
           ELSE
              MOVE 1.000                     TO H-BUN-ONSET-FACTOR
           END-IF.
096100
      ******************************************************************
      ***  Set BUNDLED Co-morbidities adjustment                     ***
      ******************************************************************
           IF COMORBID-CWF-RETURN-CODE = SPACES  THEN
              IF H-PATIENT-AGE  <  18  THEN
                 MOVE 1.000                  TO
                                             H-BUN-COMORBID-MULTIPLIER
                 MOVE '10'                   TO PPS-2011-COMORBID-PAY
              ELSE
                 IF H-BUN-ONSET-FACTOR  =  CM-ONSET-LE-120  THEN
                    MOVE 1.000               TO
                                             H-BUN-COMORBID-MULTIPLIER
                    MOVE '10'                TO PPS-2011-COMORBID-PAY
                 ELSE
                    PERFORM 2100-CALC-COMORBID-ADJUST
                    MOVE H-COMORBID-MULTIPLIER TO
                                             H-BUN-COMORBID-MULTIPLIER
                 END-IF
              END-IF
           ELSE
              IF COMORBID-CWF-RETURN-CODE  =  '10'  THEN
                 MOVE 1.000                  TO
                                             H-BUN-COMORBID-MULTIPLIER
                 MOVE '10'                   TO PPS-2011-COMORBID-PAY
              ELSE
                 IF COMORBID-CWF-RETURN-CODE  =  '20'  THEN
                    MOVE CM-GI-BLEED         TO
                                             H-BUN-COMORBID-MULTIPLIER
                    MOVE '20'                TO PPS-2011-COMORBID-PAY
                 ELSE
                    IF COMORBID-CWF-RETURN-CODE  =  '30'  THEN
                       MOVE CM-PNEUMONIA     TO
                                             H-BUN-COMORBID-MULTIPLIER
                       MOVE '30'             TO PPS-2011-COMORBID-PAY
                    ELSE
                       IF COMORBID-CWF-RETURN-CODE  =  '40'  THEN
                          MOVE CM-PERICARDITIS TO
                                             H-BUN-COMORBID-MULTIPLIER
                          MOVE '40'          TO PPS-2011-COMORBID-PAY
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
100600
      ******************************************************************
      ***  Calculate BUNDLED Low Volume adjustment                   ***
      ******************************************************************
           IF P-PROV-LOW-VOLUME-INDIC = 'Y'  THEN
              IF H-PATIENT-AGE > 17  THEN
                 MOVE CM-LOW-VOL-ADJ-LT-4000 TO
                                             H-BUN-LOW-VOL-MULTIPLIER
                 MOVE "Y"                    TO  LOW-VOLUME-TRACK
              ELSE
                 MOVE 1.000                  TO
                                             H-BUN-LOW-VOL-MULTIPLIER
              END-IF
           ELSE
              MOVE 1.000                     TO
                                             H-BUN-LOW-VOL-MULTIPLIER
           END-IF.
102300
      ******************************************************************
      ***  Calculate BUNDLED Adjusted PPS Base Rate                  ***
      ******************************************************************
           COMPUTE H-BUN-ADJUSTED-BASE-WAGE-AMT  ROUNDED  =
              (H-BUN-BASE-WAGE-AMT * H-BUN-AGE-FACTOR)    *
              (H-BUN-BSA-FACTOR    * H-BUN-BMI-FACTOR)    *
              (H-BUN-ONSET-FACTOR  * H-BUN-COMORBID-MULTIPLIER) *
              (H-BUN-LOW-VOL-MULTIPLIER).
103200
      ******************************************************************
      ***  Calculate BUNDLED Condition Code payment                  ***
      ******************************************************************
      * Self-care in Training add-on
           IF B-COND-CODE = '73'  THEN
      * no add-on when onset is present
              IF H-BUN-ONSET-FACTOR  =  CM-ONSET-LE-120  THEN
                 MOVE ZERO                   TO
                                          H-BUN-WAGE-ADJ-TRAINING-AMT
              ELSE
      * use new PPS training add-on amount times wage-index
                 COMPUTE H-BUN-WAGE-ADJ-TRAINING-AMT  ROUNDED  =
                   TRAINING-ADD-ON-PMT-AMT * BUN-CBSA-W-INDEX
                 MOVE "Y"                    TO TRAINING-TRACK
              END-IF
           ELSE
      * Dialysis in Home and (CAPD or CCPD) Per-Diem calculation
              IF (B-COND-CODE = '74')  AND
                 (B-REV-CODE = '0841' OR '0851')  THEN
                    COMPUTE H-CC-74-PER-DIEM-AMT  ROUNDED =
                       (H-BUN-ADJUSTED-BASE-WAGE-AMT * 3) / 7
              ELSE
                 MOVE ZERO                   TO
                                          H-BUN-WAGE-ADJ-TRAINING-AMT
                                          H-CC-74-PER-DIEM-AMT
              END-IF
           END-IF.
106000
      ******************************************************************
      ***  Calculate BUNDLED ESRD PPS Final Payment Rate             ***
      ******************************************************************
           IF (B-COND-CODE = '74')  AND
              (B-REV-CODE = '0841' OR '0851')  THEN
                 COMPUTE H-PPS-FINAL-PAY-AMT  ROUNDED  =
                                 H-CC-74-PER-DIEM-AMT
                 COMPUTE H-FULL-CLAIM-AMT  ROUNDED  =
                    (H-BUN-ADJUSTED-BASE-WAGE-AMT *
                    ((B-CLAIM-NUM-DIALYSIS-SESSIONS) * 3) / 7)
           ELSE
              COMPUTE H-PPS-FINAL-PAY-AMT  ROUNDED  =
                      H-BUN-ADJUSTED-BASE-WAGE-AMT  +
                      H-BUN-WAGE-ADJ-TRAINING-AMT
           END-IF.
107600
      ******************************************************************
      ***  Calculate BUNDLED Outlier                                 ***
      ******************************************************************
           PERFORM 2500-CALC-OUTLIER-FACTORS.
108100
      ******************************************************************
      ***  Calculate Low Volume payment for recovery purposes        ***
      ******************************************************************
           IF LOW-VOLUME-TRACK = "Y"  THEN
              PERFORM 3000-LOW-VOL-FULL-PPS-PAYMENT
              PERFORM 3100-LOW-VOL-OUT-PPS-PAYMENT
108800
              COMPUTE H-LV-PPS-FINAL-PAY-AMT = H-LV-PPS-FINAL-PAY-AMT -
                 H-PPS-FINAL-PAY-AMT
109100
              COMPUTE H-LV-OUT-PAYMENT       = H-LV-OUT-PAYMENT       -
                 H-OUT-PAYMENT
109400
              COMPUTE H-LV-PPS-FINAL-PAY-AMT = H-LV-PPS-FINAL-PAY-AMT +
                 H-LV-OUT-PAYMENT
109700
              IF P-PROV-WAIVE-BLEND-PAY-INDIC = 'N'  THEN
                 COMPUTE PPS-LOW-VOL-AMT  ROUNDED =
                    H-LV-PPS-FINAL-PAY-AMT  *  BUN-CBSA-BLEND-PCT
              ELSE
                 MOVE H-LV-PPS-FINAL-PAY-AMT TO PPS-LOW-VOL-AMT
              END-IF
           END-IF.
110500
110600
      /
       2100-CALC-COMORBID-ADJUST.
      ******************************************************************
      ***  Calculate Co-morbidities adjustment                       ***
      ******************************************************************
      *  This logic assumes that the comorbids are randomly assigned   *
      *to the comorbid table.  It will select the highest comorbid for *
      *payment if one is found.                                        *
      ******************************************************************
           MOVE 'N'                          TO IS-HIGH-COMORBID-FOUND.
           MOVE 1.000                        TO H-COMORBID-MULTIPLIER.
           MOVE '10'                         TO PPS-2011-COMORBID-PAY.
111900
           PERFORM VARYING  SUB  FROM  1 BY 1
             UNTIL SUB   >  6   OR   HIGH-COMORBID-FOUND
               IF COMORBID-DATA (SUB) = 'MA'  THEN
                 MOVE CM-GI-BLEED            TO H-COMORBID-MULTIPLIER
                 MOVE "Y"                    TO IS-HIGH-COMORBID-FOUND
                 MOVE "Y"                    TO ACUTE-COMORBID-TRACK
                 MOVE '20'                   TO PPS-2011-COMORBID-PAY
               ELSE
                 IF COMORBID-DATA (SUB) = 'MB'  THEN
                   IF CM-PNEUMONIA  >  H-COMORBID-MULTIPLIER  THEN
                     MOVE CM-PNEUMONIA       TO H-COMORBID-MULTIPLIER
                     MOVE "Y"                TO ACUTE-COMORBID-TRACK
                     MOVE '30'               TO PPS-2011-COMORBID-PAY
                   END-IF
                 ELSE
                   IF COMORBID-DATA (SUB) = 'MC'  THEN
                      IF CM-PERICARDITIS  >
                                            H-COMORBID-MULTIPLIER  THEN
                        MOVE CM-PERICARDITIS TO H-COMORBID-MULTIPLIER
                        MOVE "Y"             TO ACUTE-COMORBID-TRACK
                        MOVE '40'            TO PPS-2011-COMORBID-PAY
                      END-IF
                   ELSE
                     IF COMORBID-DATA (SUB) = 'MD'  THEN
                       IF CM-MYELODYSPLASTIC  >
                                            H-COMORBID-MULTIPLIER  THEN
                         MOVE CM-MYELODYSPLASTIC  TO
                                            H-COMORBID-MULTIPLIER
                         MOVE "Y"            TO CHRONIC-COMORBID-TRACK
                         MOVE '50'           TO PPS-2011-COMORBID-PAY
                       END-IF
                     ELSE
                       IF COMORBID-DATA (SUB) = 'ME'  THEN
                         IF CM-SICKEL-CELL  >
                                            H-COMORBID-MULTIPLIER  THEN
                           MOVE CM-SICKEL-CELL  TO
                                            H-COMORBID-MULTIPLIER
                           MOVE "Y"          TO CHRONIC-COMORBID-TRACK
                           MOVE '60'         TO PPS-2011-COMORBID-PAY
                         END-IF
                       ELSE
                         IF COMORBID-DATA (SUB) = 'MF'  THEN
                           IF CM-MONOCLONAL-GAMM  >
                                            H-COMORBID-MULTIPLIER  THEN
                             MOVE CM-MONOCLONAL-GAMM TO
                                            H-COMORBID-MULTIPLIER
                             MOVE "Y"        TO CHRONIC-COMORBID-TRACK
                             MOVE '70'       TO PPS-2011-COMORBID-PAY
                           END-IF
                         END-IF
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               END-IF
           END-PERFORM.
      /
       2500-CALC-OUTLIER-FACTORS.
      ******************************************************************
      ***  Set separately billable OUTLIER age adjustment factor     ***
      ******************************************************************
           IF H-PATIENT-AGE < 13  THEN
              IF B-REV-CODE = '0821' OR '0881' THEN
                 MOVE SB-AGE-LT-13-HEMO-MODE TO H-OUT-AGE-FACTOR
              ELSE
                 MOVE SB-AGE-LT-13-PD-MODE   TO H-OUT-AGE-FACTOR
              END-IF
           ELSE
              IF H-PATIENT-AGE < 18 THEN
                 IF B-REV-CODE = '0821' OR '0881'  THEN
                    MOVE SB-AGE-13-17-HEMO-MODE
                                             TO H-OUT-AGE-FACTOR
                 ELSE
                    MOVE SB-AGE-13-17-PD-MODE
                                             TO H-OUT-AGE-FACTOR
                 END-IF
              ELSE
                 IF H-PATIENT-AGE < 45  THEN
                    MOVE SB-AGE-18-44        TO H-OUT-AGE-FACTOR
                 ELSE
                    IF H-PATIENT-AGE < 60  THEN
                       MOVE SB-AGE-45-59     TO H-OUT-AGE-FACTOR
                    ELSE
                       IF H-PATIENT-AGE < 70  THEN
                          MOVE SB-AGE-60-69  TO H-OUT-AGE-FACTOR
                       ELSE
                          IF H-PATIENT-AGE < 80  THEN
                             MOVE SB-AGE-70-79
                                             TO H-OUT-AGE-FACTOR
                          ELSE
                             MOVE SB-AGE-80-PLUS
                                             TO H-OUT-AGE-FACTOR
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
121800
      ******************************************************************
      **Calculate separately billable OUTLIER BSA factor (superscript)**
      ******************************************************************
           COMPUTE H-OUT-BSA  ROUNDED = (.007184 *
               (B-PATIENT-HGT ** .725) * (B-PATIENT-WGT ** .425))
122400
           IF H-PATIENT-AGE > 17  THEN
              COMPUTE H-OUT-BSA-FACTOR  ROUNDED =
                   SB-BSA ** ((H-OUT-BSA - 1.87) / .1)
           ELSE
              MOVE 1.000                     TO H-OUT-BSA-FACTOR
           END-IF.
123100
      ******************************************************************
      ***  Calculate separately billable OUTLIER BMI factor          ***
      ******************************************************************
           COMPUTE H-OUT-BMI  ROUNDED = (B-PATIENT-WGT /
               (B-PATIENT-HGT ** 2)) * 10000.
123700
           IF (H-PATIENT-AGE > 17) AND (H-OUT-BMI < 18.5)  THEN
              MOVE SB-BMI-LT-18-5            TO H-OUT-BMI-FACTOR
           ELSE
              MOVE 1.000                     TO H-OUT-BMI-FACTOR
           END-IF.
124300
      ******************************************************************
      ***  Calculate separately billable OUTLIER ONSET factor        ***
      ******************************************************************
           IF B-DIALYSIS-START-DATE > ZERO  THEN
              IF H-PATIENT-AGE > 17  THEN
                 IF ONSET-DATE > 120  THEN
                    MOVE 1                   TO H-OUT-ONSET-FACTOR
                 ELSE
                    MOVE SB-ONSET-LE-120     TO H-OUT-ONSET-FACTOR
                 END-IF
              ELSE
                 MOVE 1                      TO H-OUT-ONSET-FACTOR
              END-IF
           ELSE
              MOVE 1.000                     TO H-OUT-ONSET-FACTOR
           END-IF.
126000
      ******************************************************************
      ***  Set separately billable OUTLIER Co-morbidities adjustment ***
      ******************************************************************
           IF COMORBID-CWF-RETURN-CODE = SPACES  THEN
              IF H-PATIENT-AGE  <  18  THEN
                 MOVE 1.000                  TO
                                             H-OUT-COMORBID-MULTIPLIER
                 MOVE '10'                   TO PPS-2011-COMORBID-PAY
              ELSE
                 IF H-BUN-ONSET-FACTOR  =  CM-ONSET-LE-120  THEN
                    MOVE 1.000               TO
                                             H-OUT-COMORBID-MULTIPLIER
                    MOVE '10'                TO PPS-2011-COMORBID-PAY
                 ELSE
                    PERFORM 2600-CALC-COMORBID-OUT-ADJUST
                 END-IF
              END-IF
           ELSE
              IF COMORBID-CWF-RETURN-CODE  =  '10'  THEN
                 MOVE 1.000                  TO
                                             H-OUT-COMORBID-MULTIPLIER
              ELSE
                 IF COMORBID-CWF-RETURN-CODE  =  '20'  THEN
                    MOVE SB-GI-BLEED         TO
                                             H-OUT-COMORBID-MULTIPLIER
                 ELSE
                    IF COMORBID-CWF-RETURN-CODE  =  '30'  THEN
                       MOVE SB-PNEUMONIA     TO
                                             H-OUT-COMORBID-MULTIPLIER
                    ELSE
                       IF COMORBID-CWF-RETURN-CODE  =  '40'  THEN
                          MOVE SB-PERICARDITIS TO
                                             H-OUT-COMORBID-MULTIPLIER
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
129900
      ******************************************************************
      ***  Set OUTLIER low-volume-multiplier                         ***
      ******************************************************************
           IF P-PROV-LOW-VOLUME-INDIC = "N"  THEN
              MOVE 1                         TO H-OUT-LOW-VOL-MULTIPLIER
           ELSE
              IF H-PATIENT-AGE < 18  THEN
                 MOVE 1                      TO H-OUT-LOW-VOL-MULTIPLIER
              ELSE
                 MOVE SB-LOW-VOL-ADJ-LT-4000 TO H-OUT-LOW-VOL-MULTIPLIER
                 MOVE "Y"                    TO LOW-VOLUME-TRACK
              END-IF
           END-IF.
131300
      ******************************************************************
      ***  Calculate predicted OUTLIER services MAP per treatment    ***
      ******************************************************************
           COMPUTE H-OUT-PREDICTED-SERVICES-MAP  ROUNDED =
              (H-OUT-AGE-FACTOR             *
               H-OUT-BSA-FACTOR             *
               H-OUT-BMI-FACTOR             *
               H-OUT-ONSET-FACTOR           *
               H-OUT-COMORBID-MULTIPLIER    *
               H-OUT-LOW-VOL-MULTIPLIER).
132400
      ******************************************************************
      ***  Calculate case mix adjusted predicted OUTLIER serv MAP/trt***
      ******************************************************************
           IF H-PATIENT-AGE < 18  THEN
              COMPUTE H-OUT-CM-ADJ-PREDICT-MAP-TRT  ROUNDED  =
                 (H-OUT-PREDICTED-SERVICES-MAP * ADJ-AVG-MAP-AMT-LT-18)
              MOVE ADJ-AVG-MAP-AMT-LT-18     TO  H-OUT-ADJ-AVG-MAP-AMT
           ELSE
133300
              COMPUTE H-OUT-CM-ADJ-PREDICT-MAP-TRT  ROUNDED  =
                 (H-OUT-PREDICTED-SERVICES-MAP * ADJ-AVG-MAP-AMT-GT-17)
              MOVE ADJ-AVG-MAP-AMT-GT-17     TO  H-OUT-ADJ-AVG-MAP-AMT
           END-IF.
133800
      ******************************************************************
      *** Calculate imputed OUTLIER services MAP amount per treatment***
      ******************************************************************
           IF (B-COND-CODE = '74')  AND
              (B-REV-CODE = '0841' OR '0851')  THEN
               COMPUTE H-HEMO-EQUIV-DIAL-SESSIONS  ROUNDED  =
                  ((B-CLAIM-NUM-DIALYSIS-SESSIONS * 3) / 7)
               COMPUTE H-OUT-IMPUTED-MAP  ROUNDED =
               (B-TOT-PRICE-SB-OUTLIER / H-HEMO-EQUIV-DIAL-SESSIONS)
           ELSE
              COMPUTE H-OUT-IMPUTED-MAP  ROUNDED =
              (B-TOT-PRICE-SB-OUTLIER / B-CLAIM-NUM-DIALYSIS-SESSIONS)
           END-IF.
135200
      ******************************************************************
      *** Comparison of predicted to the imputed OUTLIER svc MAP/trt ***
      ******************************************************************
           IF H-PATIENT-AGE < 18   THEN
              COMPUTE H-OUT-PREDICTED-MAP  ROUNDED  =
                 H-OUT-CM-ADJ-PREDICT-MAP-TRT + FIX-DOLLAR-LOSS-LT-18
              MOVE FIX-DOLLAR-LOSS-LT-18     TO H-OUT-FIX-DOLLAR-LOSS
              IF H-OUT-IMPUTED-MAP  >  H-OUT-PREDICTED-MAP  THEN
                 COMPUTE H-OUT-PAYMENT  ROUNDED  =
                  (H-OUT-IMPUTED-MAP  -  H-OUT-PREDICTED-MAP)  *
                                               LOSS-SHARING-PCT-LT-18
                 MOVE LOSS-SHARING-PCT-LT-18 TO H-OUT-LOSS-SHARING-PCT
                 MOVE "Y"                    TO OUTLIER-TRACK
              ELSE
                 MOVE ZERO                   TO H-OUT-PAYMENT
                 MOVE ZERO                   TO H-OUT-LOSS-SHARING-PCT
              END-IF
           ELSE
              COMPUTE H-OUT-PREDICTED-MAP  ROUNDED =
                 H-OUT-CM-ADJ-PREDICT-MAP-TRT + FIX-DOLLAR-LOSS-GT-17
                 MOVE FIX-DOLLAR-LOSS-GT-17  TO H-OUT-FIX-DOLLAR-LOSS
              IF H-OUT-IMPUTED-MAP  >  H-OUT-PREDICTED-MAP  THEN
                 COMPUTE H-OUT-PAYMENT  ROUNDED  =
                  (H-OUT-IMPUTED-MAP  -  H-OUT-PREDICTED-MAP)  *
                                               LOSS-SHARING-PCT-GT-17
                 MOVE LOSS-SHARING-PCT-GT-17 TO H-OUT-LOSS-SHARING-PCT
                 MOVE "Y"                    TO OUTLIER-TRACK
              ELSE
                 MOVE ZERO                   TO H-OUT-PAYMENT
              END-IF
           END-IF.
138400
           MOVE H-OUT-PAYMENT                TO OUT-NON-PER-DIEM-PAYMENT
138600
      * Dialysis in Home and (CAPD or CCPD) Per-Diem calculation
           IF (B-COND-CODE = '74')  AND
              (B-REV-CODE = '0841' OR '0851')  THEN
                 COMPUTE H-OUT-PAYMENT ROUNDED = H-OUT-PAYMENT *
                   (((B-CLAIM-NUM-DIALYSIS-SESSIONS) * 3) / 7)
           END-IF.
      /
       2600-CALC-COMORBID-OUT-ADJUST.
      ******************************************************************
      ***  Calculate OUTLIER Co-morbidities adjustment               ***
      ******************************************************************
      *  This logic assumes that the comorbids are randomly assigned   *
      *to the comorbid table.  It will select the highest comorbid for *
      *payment if one is found.                                        *
      ******************************************************************
140200
           MOVE 'N'                          TO IS-HIGH-COMORBID-FOUND.
           MOVE 1.000                        TO
                                        H-OUT-COMORBID-MULTIPLIER.
140600
           PERFORM VARYING  SUB  FROM  1 BY 1
             UNTIL SUB   >  6   OR   HIGH-COMORBID-FOUND
               IF COMORBID-DATA (SUB) = 'MA'  THEN
                 MOVE SB-GI-BLEED            TO
                                        H-OUT-COMORBID-MULTIPLIER
                 MOVE "Y"                    TO IS-HIGH-COMORBID-FOUND
                 MOVE "Y"                    TO ACUTE-COMORBID-TRACK
               ELSE
                 IF COMORBID-DATA (SUB) = 'MB'  THEN
                   IF SB-PNEUMONIA  >  H-OUT-COMORBID-MULTIPLIER  THEN
                     MOVE SB-PNEUMONIA       TO
                                        H-OUT-COMORBID-MULTIPLIER
                     MOVE "Y"                TO ACUTE-COMORBID-TRACK
                   END-IF
                 ELSE
                   IF COMORBID-DATA (SUB) = 'MC'  THEN
                      IF SB-PERICARDITIS  >
                                        H-OUT-COMORBID-MULTIPLIER  THEN
                        MOVE SB-PERICARDITIS TO
                                        H-OUT-COMORBID-MULTIPLIER
                        MOVE "Y"             TO ACUTE-COMORBID-TRACK
                      END-IF
                   ELSE
                     IF COMORBID-DATA (SUB) = 'MD'  THEN
                       IF SB-MYELODYSPLASTIC  >
                                        H-OUT-COMORBID-MULTIPLIER  THEN
                         MOVE SB-MYELODYSPLASTIC  TO
                                        H-OUT-COMORBID-MULTIPLIER
                         MOVE "Y"            TO CHRONIC-COMORBID-TRACK
                       END-IF
                     ELSE
                       IF COMORBID-DATA (SUB) = 'ME'  THEN
                         IF SB-SICKEL-CELL  >
                                        H-OUT-COMORBID-MULTIPLIER  THEN
                           MOVE SB-SICKEL-CELL  TO
                                        H-OUT-COMORBID-MULTIPLIER
                            MOVE "Y"          TO CHRONIC-COMORBID-TRACK
                         END-IF
                       ELSE
                         IF COMORBID-DATA (SUB) = 'MF'  THEN
                           IF SB-MONOCLONAL-GAMM  >
                                        H-OUT-COMORBID-MULTIPLIER  THEN
                             MOVE SB-MONOCLONAL-GAMM  TO
                                        H-OUT-COMORBID-MULTIPLIER
                             MOVE "Y"        TO CHRONIC-COMORBID-TRACK
                           END-IF
                         END-IF
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               END-IF
           END-PERFORM.
      /
      ******************************************************************
      *** Calculate Low Volume Full PPS payment for recovery purposes***
      ******************************************************************
       3000-LOW-VOL-FULL-PPS-PAYMENT.
      ******************************************************************
      ** Modified code from 'Calc BUNDLED Adjust PPS Base Rate' para. **
           COMPUTE H-LV-BUN-ADJUST-BASE-WAGE-AMT  ROUNDED  =
              (H-BUN-BASE-WAGE-AMT * H-BUN-AGE-FACTOR)     *
              (H-BUN-BSA-FACTOR    * H-BUN-BMI-FACTOR)     *
              (H-BUN-ONSET-FACTOR  * H-BUN-COMORBID-MULTIPLIER).
147100
      ******************************************************************
      **Modified code from 'Calc BUNDLED Condition Code pay' paragraph**
      * Self-care in Training add-on
           IF B-COND-CODE = '73'  THEN
      * no add-on when onset is present
              IF H-BUN-ONSET-FACTOR  =  CM-ONSET-LE-120  THEN
                 MOVE ZERO                   TO
                                          H-BUN-WAGE-ADJ-TRAINING-AMT
              ELSE
      * use new PPS training add-on amount times wage-index
                 COMPUTE H-BUN-WAGE-ADJ-TRAINING-AMT  ROUNDED  =
                   TRAINING-ADD-ON-PMT-AMT * BUN-CBSA-W-INDEX
                 MOVE "Y"                    TO TRAINING-TRACK
              END-IF
           ELSE
      * Dialysis in Home and (CAPD or CCPD) Per-Diem calculation
              IF (B-COND-CODE = '74')  AND
                 (B-REV-CODE = '0841' OR '0851')  THEN
                    COMPUTE H-CC-74-PER-DIEM-AMT  ROUNDED =
                       (H-LV-BUN-ADJUST-BASE-WAGE-AMT * 3) / 7
              ELSE
                 MOVE ZERO                   TO
                                          H-BUN-WAGE-ADJ-TRAINING-AMT
                                          H-CC-74-PER-DIEM-AMT
              END-IF
           END-IF.
149800
      ******************************************************************
      **Modified code from 'Calc BUNDLED ESRD PPS Final Pay Rate para.**
           IF (B-COND-CODE = '74')  AND
              (B-REV-CODE = '0841' OR '0851')  THEN
                 COMPUTE H-LV-PPS-FINAL-PAY-AMT  ROUNDED  =
                                 H-CC-74-PER-DIEM-AMT
           ELSE
              COMPUTE H-LV-PPS-FINAL-PAY-AMT  ROUNDED  =
                      H-LV-BUN-ADJUST-BASE-WAGE-AMT +
                      H-BUN-WAGE-ADJ-TRAINING-AMT
           END-IF.
151000
      /
      ******************************************************************
      *** Calculate Low Volume OUT PPS payment for recovery purposes ***
      ******************************************************************
       3100-LOW-VOL-OUT-PPS-PAYMENT.
      ******************************************************************
      **Modified code from 'Calc predict OUT serv MAP per treat' para.**
           COMPUTE H-LV-OUT-PREDICT-SERVICES-MAP  ROUNDED =
              (H-OUT-AGE-FACTOR             *
               H-OUT-BSA-FACTOR             *
               H-OUT-BMI-FACTOR             *
               H-OUT-ONSET-FACTOR           *
               H-OUT-COMORBID-MULTIPLIER).
152400
      ******************************************************************
      **modifi code 'Calc case mix adj predict OUT serv MAP/trt' para.**
           IF H-PATIENT-AGE < 18  THEN
              COMPUTE H-LV-OUT-CM-ADJ-PREDICT-M-TRT  ROUNDED  =
                 (H-LV-OUT-PREDICT-SERVICES-MAP * ADJ-AVG-MAP-AMT-LT-18)
              MOVE ADJ-AVG-MAP-AMT-LT-18     TO  H-OUT-ADJ-AVG-MAP-AMT
           ELSE
              COMPUTE H-LV-OUT-CM-ADJ-PREDICT-M-TRT  ROUNDED  =
                 (H-LV-OUT-PREDICT-SERVICES-MAP * ADJ-AVG-MAP-AMT-GT-17)
              MOVE ADJ-AVG-MAP-AMT-GT-17     TO  H-OUT-ADJ-AVG-MAP-AMT
           END-IF.
153600
      ******************************************************************
      ** 'Calculate imput OUT services MAP amount per treatment' para **
      ** It is not necessary to modify or insert this paragraph here. **
154000
      ******************************************************************
      **Modified 'Compare of predict to imputed OUT svc MAP/trt' para.**
           IF H-PATIENT-AGE < 18   THEN
              COMPUTE H-LV-OUT-PREDICTED-MAP  ROUNDED  =
                 H-LV-OUT-CM-ADJ-PREDICT-M-TRT + FIX-DOLLAR-LOSS-LT-18
              MOVE FIX-DOLLAR-LOSS-LT-18     TO H-OUT-FIX-DOLLAR-LOSS
              IF H-OUT-IMPUTED-MAP  >  H-LV-OUT-PREDICTED-MAP  THEN
                 COMPUTE H-LV-OUT-PAYMENT  ROUNDED  =
                  (H-OUT-IMPUTED-MAP  -  H-LV-OUT-PREDICTED-MAP)  *
                                               LOSS-SHARING-PCT-LT-18
                 MOVE LOSS-SHARING-PCT-LT-18 TO H-OUT-LOSS-SHARING-PCT
              ELSE
                 MOVE ZERO                   TO H-LV-OUT-PAYMENT
                 MOVE ZERO                   TO H-OUT-LOSS-SHARING-PCT
              END-IF
           ELSE
              COMPUTE H-LV-OUT-PREDICTED-MAP  ROUNDED =
                 H-LV-OUT-CM-ADJ-PREDICT-M-TRT + FIX-DOLLAR-LOSS-GT-17
                 MOVE FIX-DOLLAR-LOSS-GT-17  TO H-OUT-FIX-DOLLAR-LOSS
              IF H-OUT-IMPUTED-MAP  >  H-LV-OUT-PREDICTED-MAP  THEN
                 COMPUTE H-LV-OUT-PAYMENT  ROUNDED  =
                  (H-OUT-IMPUTED-MAP  -  H-LV-OUT-PREDICTED-MAP)  *
                                               LOSS-SHARING-PCT-GT-17
                 MOVE LOSS-SHARING-PCT-GT-17 TO H-OUT-LOSS-SHARING-PCT
              ELSE
                 MOVE ZERO                   TO H-LV-OUT-PAYMENT
              END-IF
           END-IF.
156900
           MOVE H-LV-OUT-PAYMENT             TO OUT-NON-PER-DIEM-PAYMENT
157100
      * Dialysis in Home and (CAPD or CCPD) Per-Diem calculation
           IF (B-COND-CODE = '74')  AND
              (B-REV-CODE = '0841' OR '0851')  THEN
                 COMPUTE H-LV-OUT-PAYMENT ROUNDED = H-LV-OUT-PAYMENT *
                   (((B-CLAIM-NUM-DIALYSIS-SESSIONS) * 3) / 7)
           END-IF.
      /
       5000-CALC-COMP-RATE-FACTORS.
      ******************************************************************
      ***  Set Composite Rate age adjustment factor                  ***
      ******************************************************************
           IF H-PATIENT-AGE < 18  THEN
              MOVE CR-AGE-LT-18              TO H-AGE-FACTOR
           ELSE
              IF H-PATIENT-AGE < 45  THEN
                 MOVE CR-AGE-18-44           TO H-AGE-FACTOR
              ELSE
                 IF H-PATIENT-AGE < 60  THEN
                    MOVE CR-AGE-45-59        TO H-AGE-FACTOR
                 ELSE
                    IF H-PATIENT-AGE < 70  THEN
                       MOVE CR-AGE-60-69     TO H-AGE-FACTOR
                    ELSE
                       IF H-PATIENT-AGE < 80  THEN
                          MOVE CR-AGE-70-79  TO H-AGE-FACTOR
                       ELSE
                          MOVE CR-AGE-80-PLUS
                                             TO H-AGE-FACTOR
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
160500
      ******************************************************************
      **Calculate Composite Rate BSA factor (2012 superscript now same)*
      ******************************************************************
           COMPUTE H-BSA  ROUNDED = (.007184 *
               (B-PATIENT-HGT ** .725) * (B-PATIENT-WGT ** .425))
161100
           IF H-PATIENT-AGE > 17  THEN
              COMPUTE H-BSA-FACTOR  ROUNDED =
                   CR-BSA ** ((H-BSA - 1.87) / .1)
           ELSE
              MOVE 1.000                     TO H-BSA-FACTOR
           END-IF.
161800
      ******************************************************************
      *** Calculate Composite Rate BMI factor (different BMI < 18.5) ***
      ******************************************************************
           COMPUTE H-BMI  ROUNDED = (B-PATIENT-WGT /
               (B-PATIENT-HGT ** 2)) * 10000.
162400
           IF (H-PATIENT-AGE > 17) AND (H-BMI < 18.5)  THEN
              MOVE CR-BMI-LT-18-5            TO H-BMI-FACTOR
           ELSE
              MOVE 1.000                     TO H-BMI-FACTOR
           END-IF.
163000
      ******************************************************************
      ***  Calculate Composite Rate Payment Amount                   ***
      ******************************************************************
      *P-ESRD-RATE, also called the Exception Rate, will not be granted*
      *in full beginning in 2011 (the beginning of the Bundled method) *
      *and will be eliminated entirely beginning in 2014 which is the  *
      *end of the blending period.  For 2011, those providers who elect*
      *to be in the blend, will get only 75% of the exception rate.    *
      *This apparently is for the pediatric providers who originally   *
      *had the exception rate.                                         *
164100
           IF P-ESRD-RATE  =  ZERO  THEN
              MOVE BASE-PAYMENT-RATE         TO  H-PAYMENT-RATE
           ELSE
              MOVE P-ESRD-RATE               TO  H-PAYMENT-RATE
           END-IF.
164700
           COMPUTE H-WAGE-ADJ-PYMT-AMT ROUNDED =
           (((H-PAYMENT-RATE * NAT-LABOR-PCT) * COM-CBSA-W-INDEX) +
             (H-PAYMENT-RATE * NAT-NONLABOR-PCT)) *
                  CBSA-BLEND-PCT.
165200
           COMPUTE H-PYMT-AMT ROUNDED = (H-WAGE-ADJ-PYMT-AMT *
              H-BMI-FACTOR * H-BSA-FACTOR * CASE-MIX-BDGT-NEUT-FACTOR *
              H-AGE-FACTOR * DRUG-ADDON).
165600
           MOVE H-PYMT-AMT                   TO CASE-MIX-FCTR-ADJ-RATE.
165800
      ******************************************************************
      ***  Calculate condition code payment                          ***
      ******************************************************************
           MOVE SPACES                       TO COND-CD-73.
166300
      * Hemo, peritoneal, or CCPD training add-on
           IF (B-COND-CODE = '73') AND (B-REV-CODE = '0821' OR '0831'
                                                            OR '0851')
              COMPUTE H-PYMT-AMT = H-PYMT-AMT + HEMO-PERI-CCPD-AMT
              MOVE 'A'                       TO AMT-INDIC
              MOVE HEMO-PERI-CCPD-AMT        TO BLOOD-DOLLAR
           ELSE
      * CAPD training add-on
              IF (B-COND-CODE = '73')  AND  (B-REV-CODE = '0841')  THEN
                 COMPUTE H-PYMT-AMT = H-PYMT-AMT + CAPD-AMT
                 MOVE 'A'                    TO AMT-INDIC
                 MOVE CAPD-AMT               TO BLOOD-DOLLAR
              ELSE
      * Dialysis in Home and (CAPD or CCPD) Per-Diem calculation
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
168900
      /
       9000-SET-RETURN-CODE.
      ******************************************************************
      ***  Set the return code                                       ***
      ******************************************************************
      *   The following 'table' helps in understanding and in making   *
      *changes to the rather large and complex "IF" statement that     *
      *follows.  This 'table' just reorders and rewords the comments   *
      *contained in the working storage area concerning the paid       *
      *return-codes.                                                   *
      *                                                                *
      *  17 = pediatric, outlier, training                             *
      *  16 = pediatric, outlier                                       *
      *  15 = pediatric, training                                      *
      *  14 = pediatric                                                *
      *                                                                *
      *  24 = outlier, low volume, training, chronic comorbid          *
      *  19 = outlier, low volume, training, acute comorbid            *
      *  29 = outlier, low volume, training                            *
      *  23 = outlier, low volume, chronic comorbid                    *
      *  18 = outlier, low volume, acute comorbid                      *
      *  30 = outlier, low volume, onset                               *
      *  28 = outlier, low volume                                      *
      *  34 = outlier, training, chronic comorbid                      *
      *  35 = outlier, training, acute comorbid                        *
      *  33 = outlier, training                                        *
      *  07 = outlier, chronic comorbid                                *
      *  06 = outlier, acute comorbid                                  *
      *  09 = outlier, onset                                           *
      *  03 = outlier                                                  *
      *                                                                *
      *  26 = low volume, training, chronic comorbid                   *
      *  21 = low volume, training, acute comorbid                     *
      *  12 = low volume, training                                     *
      *  25 = low volume, chronic comorbid                             *
      *  20 = low volume, acute comorbid                               *
      *  32 = low volume, onset                                        *
      *  10 = low volume                                               *
      *                                                                *
      *  27 = training, chronic comorbid                               *
      *  22 = training, acute comorbid                                 *
      *  11 = training                                                 *
      *                                                                *
      *  08 = onset                                                    *
      *  04 = acute comorbid                                           *
      *  05 = chronic comorbid                                         *
      *  31 = low BMI                                                  *
      *  02 = no adjustments                                           *
      *                                                                *
      *  13 = w/multiple adjustments....reserved for future use        *
      ******************************************************************
      /
           IF PEDIATRIC-TRACK                       = "Y"  THEN
              IF OUTLIER-TRACK                      = "Y"  THEN
                 IF TRAINING-TRACK                  = "Y"  THEN
                    MOVE 17                  TO PPS-RTC
                 ELSE
                    MOVE 16                  TO PPS-RTC
                 END-IF
              ELSE
                 IF TRAINING-TRACK                  = "Y"  THEN
                    MOVE 15                  TO PPS-RTC
                 ELSE
                    MOVE 14                  TO PPS-RTC
                 END-IF
              END-IF
           ELSE
              IF OUTLIER-TRACK                      = "Y"  THEN
                 IF LOW-VOLUME-TRACK                = "Y"  THEN
                    IF TRAINING-TRACK               = "Y"  THEN
                       IF CHRONIC-COMORBID-TRACK    = "Y"  THEN
                          MOVE 24            TO PPS-RTC
                       ELSE
                          IF ACUTE-COMORBID-TRACK   = "Y"  THEN
                             MOVE 19         TO PPS-RTC
                          ELSE
                             MOVE 29         TO PPS-RTC
                          END-IF
                       END-IF
                    ELSE
                       IF CHRONIC-COMORBID-TRACK    = "Y"  THEN
                          MOVE 23            TO PPS-RTC
                       ELSE
                          IF ACUTE-COMORBID-TRACK   = "Y"  THEN
                             MOVE 18         TO PPS-RTC
                          ELSE
                             IF ONSET-TRACK         = "Y"  THEN
                                MOVE 30      TO PPS-RTC
                             ELSE
                                MOVE 28      TO PPS-RTC
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 ELSE
                    IF TRAINING-TRACK               = "Y"  THEN
                       IF CHRONIC-COMORBID-TRACK    = "Y"  THEN
                          MOVE 34            TO PPS-RTC
                       ELSE
                          IF ACUTE-COMORBID-TRACK   = "Y"  THEN
                             MOVE 35         TO PPS-RTC
                          ELSE
                             MOVE 33         TO PPS-RTC
                          END-IF
                       END-IF
                    ELSE
                       IF CHRONIC-COMORBID-TRACK    = "Y"  THEN
                          MOVE 07            TO PPS-RTC
                       ELSE
                          IF ACUTE-COMORBID-TRACK   = "Y"  THEN
                             MOVE 06         TO PPS-RTC
                          ELSE
                             IF ONSET-TRACK         = "Y"  THEN
                                MOVE 09      TO PPS-RTC
                             ELSE
                                MOVE 03      TO PPS-RTC
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 IF LOW-VOLUME-TRACK                = "Y"
                    IF TRAINING-TRACK               = "Y"  THEN
                       IF CHRONIC-COMORBID-TRACK    = "Y"  THEN
                          MOVE 26            TO PPS-RTC
                       ELSE
                          IF ACUTE-COMORBID-TRACK   = "Y"  THEN
                             MOVE 21         TO PPS-RTC
                          ELSE
                             MOVE 12         TO PPS-RTC
                          END-IF
                       END-IF
                    ELSE
                       IF CHRONIC-COMORBID-TRACK    = "Y"  THEN
                          MOVE 25            TO PPS-RTC
                       ELSE
                          IF ACUTE-COMORBID-TRACK   = "Y"  THEN
                             MOVE 20         TO PPS-RTC
                          ELSE
                             IF ONSET-TRACK         = "Y"  THEN
                                MOVE 32      TO PPS-RTC
                             ELSE
                                MOVE 10      TO PPS-RTC
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 ELSE
                    IF TRAINING-TRACK               = "Y"  THEN
                       IF CHRONIC-COMORBID-TRACK    = "Y"  THEN
                          MOVE 27            TO PPS-RTC
                       ELSE
                          IF ACUTE-COMORBID-TRACK   = "Y"  THEN
                             MOVE 22         TO PPS-RTC
                          ELSE
                             MOVE 11         TO PPS-RTC
                          END-IF
                       END-IF
                    ELSE
                       IF ONSET-TRACK               = "Y"  THEN
                          MOVE 08            TO PPS-RTC
                       ELSE
                          IF ACUTE-COMORBID-TRACK   = "Y"  THEN
                             MOVE 04         TO PPS-RTC
                          ELSE
                             IF CHRONIC-COMORBID-TRACK = "Y"  THEN
                                MOVE 05      TO PPS-RTC
                             ELSE
                                IF LOW-BMI-TRACK = "Y"  THEN
                                   MOVE 31 TO PPS-RTC
                                ELSE
                                   MOVE 02 TO PPS-RTC
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
      /
       9100-MOVE-RESULTS.
           IF MOVED-CORMORBIDS = SPACES  THEN
              NEXT SENTENCE
           ELSE
              MOVE H-COMORBID-DATA (1)       TO COMORBID-DATA (1)
              MOVE H-COMORBID-DATA (2)       TO COMORBID-DATA (2)
              MOVE H-COMORBID-DATA (3)       TO COMORBID-DATA (3)
              MOVE H-COMORBID-DATA (4)       TO COMORBID-DATA (4)
              MOVE H-COMORBID-DATA (5)       TO COMORBID-DATA (5)
              MOVE H-COMORBID-DATA (6)       TO COMORBID-DATA (6)
              MOVE H-COMORBID-CWF-CODE       TO
                                          COMORBID-CWF-RETURN-CODE
           END-IF.
188500
           MOVE P-GEO-MSA                    TO PPS-MSA.
           MOVE P-GEO-CBSA                   TO PPS-CBSA.
           MOVE H-WAGE-ADJ-PYMT-AMT          TO PPS-WAGE-ADJ-RATE.
           MOVE B-COND-CODE                  TO PPS-COND-CODE.
           MOVE B-REV-CODE                   TO PPS-REV-CODE.
           MOVE H-BUN-BASE-WAGE-AMT          TO PPS-2011-WAGE-ADJ-RATE.
           MOVE BUN-NAT-LABOR-PCT            TO PPS-2011-NAT-LABOR-PCT.
           MOVE BUN-NAT-NONLABOR-PCT         TO
                                          PPS-2011-NAT-NONLABOR-PCT.
           MOVE NAT-LABOR-PCT                TO PPS-NAT-LABOR-PCT.
           MOVE NAT-NONLABOR-PCT             TO PPS-NAT-NONLABOR-PCT.
           MOVE H-AGE-FACTOR                 TO PPS-AGE-FACTOR.
           MOVE H-BSA-FACTOR                 TO PPS-BSA-FACTOR.
           MOVE H-BMI-FACTOR                 TO PPS-BMI-FACTOR.
           MOVE CASE-MIX-BDGT-NEUT-FACTOR    TO PPS-BDGT-NEUT-RATE.
           MOVE H-BUN-AGE-FACTOR             TO PPS-2011-AGE-FACTOR.
           MOVE H-BUN-BSA-FACTOR             TO PPS-2011-BSA-FACTOR.
           MOVE H-BUN-BMI-FACTOR             TO PPS-2011-BMI-FACTOR.
           MOVE TRANSITION-BDGT-NEUT-FACTOR  TO
                                          PPS-2011-BDGT-NEUT-RATE.
           MOVE SPACES                       TO PPS-2011-COMORBID-MA.
           MOVE SPACES                       TO
                                          PPS-2011-COMORBID-MA-CC.
190900
           IF (B-COND-CODE = '74')  AND
              (B-REV-CODE = '0841' OR '0851')  THEN
               COMPUTE H-OUT-PAYMENT ROUNDED = H-OUT-PAYMENT /
                                           B-CLAIM-NUM-DIALYSIS-SESSIONS
           END-IF.
191500
           IF P-PROV-WAIVE-BLEND-PAY-INDIC        = 'N'  THEN
                 COMPUTE PPS-2011-BLEND-COMP-RATE    ROUNDED =
                    H-PYMT-AMT              *  COM-CBSA-BLEND-PCT
                 COMPUTE PPS-2011-BLEND-PPS-RATE     ROUNDED =
                    H-PPS-FINAL-PAY-AMT     *  BUN-CBSA-BLEND-PCT
                 COMPUTE PPS-2011-BLEND-OUTLIER-RATE ROUNDED =
                    H-OUT-PAYMENT           *  BUN-CBSA-BLEND-PCT
           ELSE
              MOVE ZERO                      TO
                                          PPS-2011-BLEND-COMP-RATE
              MOVE ZERO                      TO
                                          PPS-2011-BLEND-PPS-RATE
              MOVE ZERO                      TO
                                          PPS-2011-BLEND-OUTLIER-RATE
           END-IF.
193100
           MOVE H-PYMT-AMT                   TO
                                          PPS-2011-FULL-COMP-RATE.
           MOVE H-PPS-FINAL-PAY-AMT          TO PPS-2011-FULL-PPS-RATE
                                                PPS-FINAL-PAY-AMT.
           MOVE H-OUT-PAYMENT                TO
                                          PPS-2011-FULL-OUTLIER-RATE.
193800
193900
           IF P-QIP-REDUCTION = ' ' THEN
              NEXT SENTENCE
           ELSE
              COMPUTE PPS-2011-BLEND-COMP-RATE    ROUNDED =
                      PPS-2011-BLEND-COMP-RATE    *  QIP-REDUCTION
              COMPUTE PPS-2011-FULL-COMP-RATE     ROUNDED =
                      PPS-2011-FULL-COMP-RATE     *  QIP-REDUCTION
              COMPUTE PPS-2011-BLEND-PPS-RATE     ROUNDED =
                      PPS-2011-BLEND-PPS-RATE     *  QIP-REDUCTION
              COMPUTE PPS-2011-FULL-PPS-RATE      ROUNDED =
                      PPS-2011-FULL-PPS-RATE      *  QIP-REDUCTION
              COMPUTE PPS-2011-BLEND-OUTLIER-RATE ROUNDED =
                      PPS-2011-BLEND-OUTLIER-RATE *  QIP-REDUCTION
              COMPUTE PPS-2011-FULL-OUTLIER-RATE  ROUNDED =
                      PPS-2011-FULL-OUTLIER-RATE  *  QIP-REDUCTION
           END-IF.
195600
           IF BUNDLED-TEST   THEN
              MOVE DRUG-ADDON                TO DRUG-ADD-ON-RETURN
              MOVE 0.0                       TO MSA-WAGE-ADJ
              MOVE H-WAGE-ADJ-PYMT-AMT       TO CBSA-WAGE-ADJ
              MOVE BASE-PAYMENT-RATE         TO CBSA-WAGE-PMT-RATE
              MOVE H-PATIENT-AGE             TO AGE-RETURN
              MOVE 0.0                       TO MSA-WAGE-AMT
              MOVE COM-CBSA-W-INDEX          TO CBSA-WAGE-INDEX
              MOVE H-BMI                     TO PPS-BMI
              MOVE H-BSA                     TO PPS-BSA
              MOVE MSA-BLEND-PCT             TO MSA-PCT
              MOVE CBSA-BLEND-PCT            TO CBSA-PCT
196900
              IF P-PROV-WAIVE-BLEND-PAY-INDIC        = 'N'  THEN
                 MOVE COM-CBSA-BLEND-PCT     TO COM-CBSA-PCT-BLEND
                 MOVE BUN-CBSA-BLEND-PCT     TO BUN-CBSA-PCT-BLEND
              ELSE
                 MOVE ZERO                   TO COM-CBSA-PCT-BLEND
                 MOVE WAIVE-CBSA-BLEND-PCT   TO BUN-CBSA-PCT-BLEND
              END-IF
197700
              MOVE H-BUN-BSA                 TO BUN-BSA
              MOVE H-BUN-BMI                 TO BUN-BMI
              MOVE H-BUN-ONSET-FACTOR        TO BUN-ONSET-FACTOR
              MOVE H-BUN-COMORBID-MULTIPLIER TO BUN-COMORBID-MULTIPLIER
              MOVE H-BUN-LOW-VOL-MULTIPLIER  TO BUN-LOW-VOL-MULTIPLIER
              MOVE H-OUT-AGE-FACTOR          TO OUT-AGE-FACTOR
              MOVE H-OUT-BSA                 TO OUT-BSA
              MOVE SB-BSA                    TO OUT-SB-BSA
              MOVE H-OUT-BSA-FACTOR          TO OUT-BSA-FACTOR
              MOVE H-OUT-BMI                 TO OUT-BMI
              MOVE H-OUT-BMI-FACTOR          TO OUT-BMI-FACTOR
              MOVE H-OUT-ONSET-FACTOR        TO OUT-ONSET-FACTOR
              MOVE H-OUT-COMORBID-MULTIPLIER TO
                                          OUT-COMORBID-MULTIPLIER
              MOVE H-OUT-PREDICTED-SERVICES-MAP  TO
                                          OUT-PREDICTED-SERVICES-MAP
              MOVE H-OUT-CM-ADJ-PREDICT-MAP-TRT  TO
                                          OUT-CASE-MIX-PREDICTED-MAP
              MOVE H-HEMO-EQUIV-DIAL-SESSIONS    TO
                                          OUT-HEMO-EQUIV-DIAL-SESSIONS
              MOVE H-OUT-LOW-VOL-MULTIPLIER  TO OUT-LOW-VOL-MULTIPLIER
              MOVE H-OUT-ADJ-AVG-MAP-AMT     TO OUT-ADJ-AVG-MAP-AMT
              MOVE H-OUT-IMPUTED-MAP         TO OUT-IMPUTED-MAP
              MOVE H-OUT-FIX-DOLLAR-LOSS     TO OUT-FIX-DOLLAR-LOSS
              MOVE H-OUT-LOSS-SHARING-PCT    TO OUT-LOSS-SHARING-PCT
              MOVE H-OUT-PREDICTED-MAP       TO OUT-PREDICTED-MAP
              MOVE CR-BSA                    TO CR-BSA-MULTIPLIER
              MOVE CR-BMI-LT-18-5            TO CR-BMI-MULTIPLIER
              MOVE A-49-CENT-PART-D-DRUG-ADJ TO A-49-CENT-DRUG-ADJ
              MOVE CM-BSA                    TO PPS-CM-BSA
              MOVE CM-BMI-LT-18-5            TO PPS-CM-BMI-LT-18-5
              MOVE BUNDLED-BASE-PMT-RATE     TO PPS-BUN-BASE-PMT-RATE
              MOVE BUN-CBSA-W-INDEX          TO PPS-BUN-CBSA-W-INDEX
              MOVE H-BUN-ADJUSTED-BASE-WAGE-AMT  TO
                                          BUN-ADJUSTED-BASE-WAGE-AMT
              MOVE H-BUN-WAGE-ADJ-TRAINING-AMT   TO
                                          PPS-BUN-WAGE-ADJ-TRAIN-AMT
              MOVE TRAINING-ADD-ON-PMT-AMT   TO
                                          PPS-TRAINING-ADD-ON-PMT-AMT
              MOVE H-PAYMENT-RATE            TO COM-PAYMENT-RATE
           END-IF.
      ******        L A S T   S O U R C E   S T A T E M E N T      *****
