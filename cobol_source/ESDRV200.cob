       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESDRV200.
      *AUTHOR.     CMS.
      *       EFFECTIVE JANUARY 1, 2020
      * This subroutine CALLs the appropriate Calculation Subprogram
      * according to the bill's date
      ******************************************************************
      *   This subroutine is furnished by the Centers for Medicare     *
      *   and Medicaid Services.                                       *
      *   It is to be used as an AID in implementing the Prospective   *
      *   Payment System for ESRD claims.                              *
      *                  *  *  *  *  *  *  *  *                        *
      *   THE PROGRAM WILL:                                            *
      *       1. Include the MSA Wage Adjusted Rate table.   (ESWRT151)*
      *       2. Include the COMPOSITE CBSA Wage Index table.(ESCOM151)*
      *       3. Include the BUNDLED CBSA Wage Index table.  (ESBUN191)*
      *       4. Include a special Copylib used to switch between the  *
      *          mainframe and PC portions of the code.                *
      *       5. Do some edits on the bill information.                *
      *       6. Pass back return codes.                               *
      *       7. Create a final payment WHEN that info is on the claim.*
      *       8. CALL a calculate subroutine when applicable based on  *
      *          claim date.                                           *
      *                                                                *
      *                  *  *  *  *  *  *  *  *                        *
      *   CHANGE LOG.                                                  *
      *                                                                *
      *   The following changes made by DDS                            *
      * 10/20/06 - NEW CBSA TABLE FOR CY2007 (I.E. COPY EXCBS070)      *
      *          - ADDED DATE CHECKS FOR CY2007 AND CALL ESCAL070      *
      * 12/28/06 - ADDED DATE CHECKS FOR CY2007 APRIL AND CALL ESCAL071*
      *          - CREATED ENTER AND EXIT PARAGRAPHS FOR CLARITY       *
      *          - STREAMLINED THE PROCESSING SO THAT IT IS MORE       *
      *            EFFICIENT                                           *
      *          - PROVIDED A DRIVER VERSION NUMBER WHICH WILL BE      *
      *            PASSED BACK (IN THE PPS-CALC-VERS-CD) INDICATING AN *
      *            INTERNAL ERROR THAT IS FOUND AND THUS THE CALCULATE *
      *            SUBROUTINE IS NOT CALLED                            *
      * 01/30/07 - NEW MSA AND CBSA TABLES WHICH INCLUDE THE LATEST    *
      *            ADDITIONS TO THE STATE CODES PER CR#5490 AND CMSO   *
      *            DATA ON WEB SITE                                    *
      * 11/27/07 - ADDED SEVERAL 'IF' TESTS TO CHECK FOR WHEN CBSAS ARE*
      *            DELETED OR ADDED IN VARIOUS CLAIM YEARS.  THIS WAS  *
      *            NECESSARY SINCE I DISCOVERED BY ACCIDENT THAT BOB   *
      *            CHRISTY NEGLECTED TO ACCOUNT FOR CBSAS CHANGING OVER*
      *            TIME.  THESE TESTS ARE TEMPORARY SO THAT THE PRICER *
      *            CAN BE DELIVERED TO THE FISCAL INTERMEDIARIES ON    *
      *            TIME RATHER THAN TRYING TO TAKE THE TIME TO DEVISE A*
      *            SEARCH-ALL TABLE WHICH WOULD TAKE ACCOUNT OF CBSAS  *
      *            CHANGING OVER TIME.                                 *
      * 06/04/08 - Added Copylib which enables this subprogram to act  *
      *            the same on the IBM mainframe as well as on the PC  *
      *            under MicroFocus COBOL.  The Copylib is technically *
      *            slightly different on both systems which enables the*
      *            test switch to operate correctly without any        *
      *            standard COBOL changes between the systems.  The    *
      *            EXACT same code shown in this listing (minus the    *
      *            Copylib) WILL operate on both systems.  Therefore   *
      *            there are no problems with synchronizing two        *
      *            different source codes - - which, from experience,  *
      *            will get out of sync over time.                     *
      *          - Added different return codes to differentiate the   *
      *            errors encountered.  These additional return codes  *
      *            affect only the manager main-program which runs on  *
      *            the PC.  They do not affect the running of this     *
      *            subroutine on the IBM mainframe and will NOT impact *
      *            'FISS' because only those codes contained in the    *
      *            manual (IOM) are returned to 'FISS'.                *
      *          - New CBSA Table for CY2009 (I.E. Copy EXCBS091       *
      *          - Added date checks for CY2009 and CALL ESCAL091      *
      * 12/03/08 - Renamed this subroutine ESDRV091 and changed the    *
      *            appropriate version information.  The 9.0 version of*
      *            the driver was sent out in November.  Afterwards the*
      *            policy people who set the wage indexes, changed     *
      *            their minds about CBSA 16700 and rescinded the new  *
      *            wage index. This necessitated a re-release of the   *
      *            ESRD dirver in order to make sure that the FI'S are *
      *            using the latest version at the start of CY2009.    *
      * 11/01/09 - Renamed this subroutine ESDRV100 and changed the    *
      *            appropriate version information.  The 10.0 version  *
      *            of the driver was sent out in November.             *
      *          - Removed the ability to process 2005 claims.         *
      *          - Added features which will make the driver able to   *
      *            handle the new Bundled wage-index table and process *
      *            those claims.  However, for this release, these     *
      *            added features were commented out to speed up       *
      *            processing.  There are other additions that need    *
      *            to be made such as the length of the input record   *
      *            in order for the 2011 driver and pricer to work.    *
      * 01/08/10 - Renamed this subroutine ESDRV101 and changed the    *
      *            appropriate version information.  The 10.1 version  *
      *            of the driver was sent out in mid January.          *
      *          - Because of a request by FISS and the MACs due to    *
      *            requirements mandated by the OIG and CWF, the       *
      *            ability to process 2005 claims was restored.  In    *
      *            addition, to make this pricer conform to the rules  *
      *            established with the IPPS Pricer, the ESRD Pricer   *
      *            will be a 10 year rolling pricer.                   *
      * 01/21/10 - Renamed this subroutine ESDRV102 and changed the    *
      *            appropriate version information.  The 10.2 version  *
      *            of the driver was sent out in late January.         *
      *          - Fixed a slip-up in restoring the capability to      *
      *            process 2005 claims.  My test file had real CBSA    *
      *            values when it should have had blank values in that *
      *            field, which is the case for 2005 claims.           *
      * 08/04/10 - Renamed this subroutine ESRDRV110 and changed the   *
      *            input and output file layout.  Installed the new    *
      *            bundled (now called just PPS - the old methodology  *
      *            will now be called composite rate) wage index table.*
      * 11/09/10   Corrected the problem with CBSA 16700 for 2009 in   *
      *            the composite rate table.  The latest wage indexes  *
      *            from Suzanne Asplen's spreadsheet dated 10/27/2010  *
      *            installed for both Bundled (PPS) and Composite Rate *
      *            tables.  Updated the 0400-CHECK-CBSA-ADDS-DELETES   *
      *            paragraph with 2011 CBSA changes.                   *
      * 03/09/11   Renamed this subroutine ESDRV116 due to conformitity*
      *            with the Calculate subroutine and also due to the   *
      *            reworking of the copylib so that it used the same   *
      *            ones that the calculate subroutines use.            *
      * 10/28/11   Renamed this subroutine ESDRV120 due to conformitity*
      *            with the Calculate subroutine.                      *
      * 12/02/11   Renamed this subroutine ESDRV121 due to conformitity*
      *            with the Calculate subroutine.                      *
      * 10/19/12   ESDRV130 created for the CY 2013 ESRD Pricer.
      *            - Added comments concerning this release
      *            - Changed W-STORAGE-REF to ESRD D13.0
      *            - Added  the following line to the WORKING-STORAGE
      *              SECTION to add the name of the new Calculation
      *              subprogram -->
      *                 01  ESCAL130       PIC X(08) VALUE 'ESCAL130'.
      *            - Changed COPY ESWRT121. to COPY ESWRT130. For MSA
      *              Wage Index, which is no longer updated, just
      *              renamed.
      *            - Changed COPY ESCOM121. to COPY ESCOM130. to use
      *              the latest Composite Wage Index.
      *            - Changed COPY ESBUN121. to COPY ESBUN130. To use
      *              the latest Bundled (PPS) Wage Index.
      *            - Added section to code to CALL the new Calculation
      *              Subprogram (ESCAL130)
      *            - Since the CY 2013 ESRD Pricer update included
      *              changes to the Calculation subprograms for
      *              both 2012 and 2011, replaced
      *              ESCAL116 with ESCAL117, and ESCAL121 with
      *              ESCAL122 where needed.
      * 11/15/13 ESDRV14B - TEST ONLY BETA VERSION
      *          CREATED TO GIVE FISS SOMETHING TO TEST NOW BECAUSE
      *          THE FINAL RULE WILL NOT BE AVAILABLE FOR ANOTHER
      *          MONTH
      *          MADE CHANGES TO ENSURE THAT PACIFIC RIM FACILITIES
      *          ARE NOW PAID USING THE SAME CALCULATIONS AS
      *          FACILITIES FROM OTHER AREAS
      * 11/18/13 ESDRV140 - normal yearly release
      *       This Driver module includes new logic
      *       that directs Pacific Rim providers to flow through the
      *       same pricing calculation as all other providers. This
      *       logic was tested in the Beta version (ESDRV14B).
      * 11/15/14 ESDRV150 - normal yearly release
      *       added code to the 0800-FIND-BUNDLED-CBSA-WI to check to
      *       make sure that the Wage Index being used to price the
      *       claim is equal to the year of the claim in B-THRU-DATE
      * 12/23/14 ESDRV151 - implement use of Special Wage Indexes for
      *       certain Children's Hospitals
      * 11/16/15 UPDATED FOR CY 2016 VERSION 0
      * Stop using 0400-CHECK-CBSA-ADDS-DELETES.
      * 09/07/16 - VERSION 17.B FOR TESTING ONLY
      * 10/19/16 - VERSION 17.0 - CR9807 - CY 2017 ANNUAL UPDATE
      * 03/16/17 - VERSION 17.1 - CR9609 - ADD RETRAINING - JULY 1, 2017
      * 06/15/17 - VERSION 18.B - BETA VERSION TO TEST TDAPA
      * 10/02/18 - VERSION 18.C - 2ND BETA VERSION TO TEST TDAPA
      * 10/24/18 - VERSION 18.0 - CR10312 - CY2018 ANNUAL UPDATE
      * 10/18/18 - VERSION 19.0 - CR11021 - CY2019 ANNUAL UPDATE
      * 02/04/19 - VERSION 19.1 - CORRECTED WAGE INDEX
      * 10/07/19 - VERSION 20.0 - CR11506 - CY2020 ANNUAL UPDATE
      *
      ******************************************************************
017600
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.            IBM-Z990.
       OBJECT-COMPUTER.            IBM.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
018300
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.
       01  W-STORAGE-REF                  PIC X(48)  VALUE
           'ESRD D20.1    -    W O R K I N G   S T O R A G E'.
019000
       01  DRIVER-VERSION                 PIC X(05) VALUE 'D20.1'.
019200
       01  ESCAL056                       PIC X(08) VALUE 'ESCAL056'.
       01  ESCAL062                       PIC X(08) VALUE 'ESCAL062'.
       01  ESCAL070                       PIC X(08) VALUE 'ESCAL070'.
       01  ESCAL071                       PIC X(08) VALUE 'ESCAL071'.
       01  ESCAL080                       PIC X(08) VALUE 'ESCAL080'.
      *01  ESCAL090 does not exist        PIC X(08) VALUE 'ESCAL090'.
       01  ESCAL091                       PIC X(08) VALUE 'ESCAL091'.
       01  ESCAL100                       PIC X(08) VALUE 'ESCAL100'.
      *01  ESCAL110 only for FISS testing PIC X(08) VALUE 'ESCAL110'.
      *01  ESCAL111 only for FISS testing PIC X(08) VALUE 'ESCAL111'.
      *01  ESCAL112 only for FISS testing PIC X(08) VALUE 'ESCAL112'.
      *01  ESCAL113 only for FISS testing PIC X(08) VALUE 'ESCAL113'.
      *01  ESCAL114 only for FISS testing PIC X(08) VALUE 'ESCAL114'.
      *01  ESCAL115 still under CR7064... PIC X(08) VALUE 'ESCAL115'.
       01  ESCAL117                       PIC X(08) VALUE 'ESCAL117'.
      *01  ESCAL120 does not exist        PIC X(08) VALUE 'ESCAL120'.
       01  ESCAL122                       PIC X(08) VALUE 'ESCAL122'.
       01  ESCAL130                       PIC X(08) VALUE 'ESCAL130'.
       01  ESCAL140                       PIC X(08) VALUE 'ESCAL140'.
       01  ESCAL151                       PIC X(08) VALUE 'ESCAL151'.
       01  ESCAL160                       PIC X(08) VALUE 'ESCAL160'.
       01  ESCAL170                       PIC X(08) VALUE 'ESCAL170'.
       01  ESCAL171                       PIC X(08) VALUE 'ESCAL171'.
       01  ESCAL180                       PIC X(08) VALUE 'ESCAL180'.
       01  ESCAL191                       PIC X(08) VALUE 'ESCAL191'.
       01  ESCAL200                       PIC X(08) VALUE 'ESCAL200'.
021900
       01  DISPLAY-LINE-MEASUREMENT.
           05  FILLER                     PIC X(50) VALUE
               '....:...10....:...20....:...30....:...40....:...50'.
           05  FILLER                     PIC X(50) VALUE
               '....:...60....:...70....:...80....:...90....:..100'.
           05  FILLER                     PIC X(20) VALUE
               '....:..110....:..120'.
022700
       01  PRINT-LINE-MEASUREMENT.
           05  FILLER                     PIC X(51) VALUE
               'X....:...10....:...20....:...30....:...40....:...50'.
           05  FILLER                     PIC X(50) VALUE
               '....:...60....:...70....:...80....:...90....:..100'.
           05  FILLER                     PIC X(32) VALUE
               '....:..110....:..120....:..130..'.
023500
       01  WORK-AREA.
           05  W-SUB1                     PIC S9(07) COMP-3 VALUE ZERO.
           05  W-SUB2                     PIC S9(07) COMP-3 VALUE ZERO.
           05  W-SUB3                     PIC S9(07) COMP-3 VALUE ZERO.
024000
      *ADDED B-THRU-YEAR-CODE TO COMPARE COMPARE BILL YEAR TO WI YEAR
       01  B-THRU-YEAR-CODE                 PIC  9(03) VALUE 0.
024300
      /
       COPY DSCNTRL.
      *COPY "DSCNTRL.CPY".
      /
      *The Pricer is required to handle claims for the current year and
      *three prior years in order for claims to be processed in a timely
      *manner.  However, because claims can be reopened due to requireme
      *by the OIG or CWF, the pricer needs to be able to process claims
      *for a total of TEN years running.  Therefore, a diagram is
      *necessary to figure out which tables need to be kept and which re
      *from the driver in order to save space and increase efficiency.
      *An asterisk beside the word 'yes' (below) indicates that the tabl
      *does not need to be updated (even though it is still needed for
      *for prior year claims).  Example:  A claim dated Dec. 31, 2008 ne
      *the MSA table to price the claim items using a blend of 25% MSA 7
      *CBSA.  Therefore the 2018 pricer needs the MSA table to process a
      *year old claim.  Beginning in 2019 the MSA table can then be remo
      *
      *
      *                                                        Call
      *                 Wage index tables needed            Calculate
      *                                                     Subroutine
      *
      *                 Need    Need      Need             2222222222222
      *20xx                   Composite  Bundled           0000000000000
      *Year blend      MSA-tbl CBSA-tbl  CBSA-tbl Pricer   1111111111222
      *                                           Version  0123456789012
      *
      *05 100%MSA       yes                        05.6    XXXXXX
      *06  25%Composite yes*    yes                06.2    XXXXXXX
      *07  50%  "       yes*    yes                07.0    XXXXXXXX
      *07  50%  "       yes*    yes                07.1    XXXXXXXX
      *08  75%  "       yes*    yes                08.0    XXXXXXXXX
      *09 100%  "       yes*    yes                09.1    XXXXXXXXXX
      *10 100%  "       yes*    yes                10.0    XXXXXXXXXXX
      *11  25%Bundled   yes*    yes        yes     11.0    XXXXXXXXXXXX
      *12  50%  "       yes*    yes        yes     12.0    XXXXXXXXXXXXX
      *13  75%  "       yes*    yes        yes     13.0    XXXXXXXXXXXXX
      *14 100%  "       yes*    yes*       yes     14.0    XXXXXXXXXXXXX
      *15 100%  "       yes*    yes*       yes     15.0    XXXXXXXXXXXXX
      *16 100%  "       yes*    yes*       yes     16.0     XXXXXXXXXXXX
      *17 100%  "       yes*    yes*       yes     17.0      XXXXXXXXXXX
      *18 100%  "       yes*    yes*       yes     18.0       XXXXXXXXXX
      *19 100%  "        no     yes*       yes     19.0        XXXXXXXXX
      *20 100%  "        no     yes*       yes     20.0         XXXXXXXX
      *21 100%  "        no     yes*       yes     21.0          XXXXXXX
      *22 100%  "        no     yes*       yes     22.0           XXXXXX
      *23 100%  "        no     yes*       yes     23.0            XXXXX
      *24 100%  "        no      no        yes     24.0             XXXX
      *25 100%  "        no      no        yes     25.0              XXX
      *26 100%  "        no      no        yes     26.0               XX
      *27 100%  "        no      no        yes     27.0                X
      *28 100%  "        no      no        yes     28.0
      *29 100%  "        no      no        yes     29.0
      /
      *  The following COPYLIB will NOT change (it's for MSA in 2005). *
      *  Any wage adjustments are made in the calculation sub-programs.*
      *  This COPYLIB will be removed in 2019 since it will no longer  *
      *  be used.                                                      *
       COPY ESWRT151.
      *COPY "ESWRT121.CPY".
      /
      *  The following COPYLIB is for the Composite Rate payment       *
      *  system.  It will cease to be updated beginning in 2014,       *
      *  although it will still be used through calendar year 2024.    *
      *  Unless policies change, this table will be limited to ten     *
      *  calendar years worth of data in order to cut down on the      *
      *  amount of memory space used by the program.                   *
      *  This COPYLIB will be removed in 2024 since it will no longer  *
      *  be used.                                                      *
       COPY ESCOM151.
      *COPY "ESCOM121.CPY".
      /
      *  The following COPYLIB is for the new Bundled payment system.  *
      *  It is effective January 1, 2011 and will be changed on a      *
      *  yearly basis from then on.  Although it looks similiar to the *
      *  CBSA COPYLIB below it and also uses CBSAs, the wage indices   *
      *  are derived in a different manner and while in the blend      *
      *  period, they have to remain distinct because of the need to   *
      *  process prior year's bills which use the old single CBSA index*
      *  This table will superceed the old CBSA table below starting in*
      *  calendar year 2014.  Unless policies change, this table will  *
      *  be limited to TEN calendar years worth of data in order to    *
      *  cut down on the amount of memory space used by the program.   *
       COPY ESBUN200.
      *COPY "ESBUN121.CPY".
      /
       COPY ESCHI151.
      /
       COPY WAGECPY.
      *COPY "WAGECPY.CPY".
      /
       COPY RTCCPY.
      *COPY "RTCCPY.CPY".
      /
       LINKAGE SECTION.
       COPY BILLCPY.
      *COPY "BILLCPY.CPY".
      /
       PROCEDURE DIVISION  USING BILL-NEW-DATA
                                 PPS-DATA-ALL.
034500
      ******************************************************************
      *    THIS SUBROUTINE WILL...                                     *
      *        A. Validate BILL-THRU-DATE and P-ESRD-RATE.             *
      *        B. When P-ESRD-RATE > ZERO, then Rate on the bill is put*
      *           in the PPS-FINAL-PAY-AMT area and then the record is *
      *           passed back to the calling program without calling   *
      *           any Calculate subroutine.                            *
      *        C. Get MSA WAGE-RATE and/or CBSA WAGE-INDEX from the    *
      *           appropriate internal Indexed Tables.                 *
      *        D. Call ONE of various calculate subroutines based on   *
      *           the date shown on the bill record.  Will not call any*
      *           calculate subroutines if BILL-THRU-DATE exceeds the  *
      *           driver design date.  Return code will be "00" and the*
      *           Driver Version number will be present after 2010.    *
      *        E. Pass back the Driver Version if no MSA/CBSA and date *
      *           comtination are found in the Indexed Tables.         *
      *           Otherwise the calculate subroutine supplies the      *
      *           Version Number since no further processing in this   *
      *           program is done after the call to the calculate      *
      *           subroutine.                                          *
      ******************************************************************
036700
       0100-ENTER-DRIVER.
           INITIALIZE PPS-DATA-ALL.
           MOVE ZEROS TO COM-CBSA-WAGE-RECORD.
           MOVE DRIVER-VERSION          TO PPS-CALC-VERS-CD.
           MOVE 00                      TO PPS-RTC.
      *
      * Propose moving 99 to PPS-RTC so that when claims with a date
      * greater than the year that the DRIVER is designed for, will
      * return a RTC=99 rather than RTC=00 which is does currently.
      * i.e. B-THRU-DATE > 2011 for the current DRIVER will get RTC=99.
      *
      *    MOVE 99                      TO PPS-RTC.
      * You may uncomment the above line to test it out it you wish.
      *
      *    DISPLAY '***Entering DRIVER, Bill-New-Data follows'.
      *    DISPLAY BILL-NEW-DATA.
038400
           IF (B-THRU-DATE < 20050401) OR (B-THRU-DATE NOT NUMERIC)
              MOVE 98                   TO PPS-RTC
              GO TO 0100-EXIT-DRIVER
           END-IF.
038900
           IF P-ESRD-RATE NOT NUMERIC
              MOVE 50                   TO PPS-RTC
              GO TO 0100-EXIT-DRIVER
           END-IF.
039400
      *P-ESRD-RATE, also called the Exception Rate, will not be granted*
      *in full beginning in 2011 (the beginning of the Bundled method) *
      *and will be eliminated entirely beginning in 2014 which is the  *
      *end of the blending period.  For 2011, those providers who elect*
      *to be in the blend, will get only 75% of the exception rate.    *
      *The exception to this 'Exception Rate' is for those providers   *
      *located within one of the Pacific Island Trust Territories.     *
      *These providers are paid at cost and so a new variable is needed*
      *to allow them to be paid the same way as before, but still allow*
      *the pediatric providers who had the Exception Rate to be        *
      *processed under the blended PPS if they chose to elect to be in *
      *the blend.                                                      *
040700
           IF (B-THRU-DATE < 20110101)  AND  (P-ESRD-RATE > ZERO)
              MOVE P-ESRD-RATE          TO PPS-FINAL-PAY-AMT
              MOVE 01                   TO PPS-RTC
              GO TO 0100-EXIT-DRIVER
           END-IF.
041300
           IF (B-THRU-DATE > 20101231)        AND
              (B-THRU-DATE < 20140101)        AND
              (P-PACIFIC-IS-TRUST-TERR = '2') AND
              (P-ESRD-RATE > ZERO)
              MOVE P-ESRD-RATE                TO PPS-FINAL-PAY-AMT
              MOVE 01                         TO PPS-RTC
              GO TO 0100-EXIT-DRIVER
           END-IF.
042200
      ******************************************************************
      * Check for additions and deletions in CBSAs for each year.      *
      *CY 2016 COMMENTED OUT
      ******************************************************************
042700
      *    PERFORM 0400-CHECK-CBSA-ADDS-DELETES
      *       THRU 0400-ADD-DELETE-EXIT.
043000
      *    IF PPS-RTC > 00  THEN
      *       GO TO 0100-EXIT-DRIVER
      *    END-IF.
043400
      /
      ******************************************************************
      * Get the Wage Adjusted Rate as well as                          *
      *     the COMposite and BUNdled budget neutralized Wage Indexes. *
      ******************************************************************
044000
      * This driver will NOT CALL any calculate subroutine beyond the
      * year for which the the driver is designed, and therefore a claim
      * will not be paid correctly despite the RTC having a value of"00"
      * A return code of "00" was only valid from 2005 thru 2010.
      * Beginning in 2011 the return code
      * should be greater than "01" for paid and unpaid claims.
      *    FISS has the responsibility to insure that claims beyond the
      * driver design date are not permitted.  Therefore for 2011, no
      * claims should have a date beyond 2011.
      *    FISS also has the responsibility of insuring that the claims
      * do not have future dates beyond the "TODAYS-DATE"
      * (which can be accepted from the IBM computer).
045300
           IF (B-THRU-DATE > 20131231) THEN
      * Process 2014 and later claims.
              MOVE ZERO                TO W-NEW-RATE1-RECORD
                                          W-NEW-RATE2-RECORD
              PERFORM 0800-FIND-BUNDLED-CBSA-WI
                 THRU 0800-FIND-EXIT
           ELSE
              IF (B-THRU-DATE > 20101231) THEN
      * Process 2011 - 2013 claims.
                  MOVE ZERO                TO W-NEW-RATE1-RECORD
                                              W-NEW-RATE2-RECORD
                  PERFORM 0700-FIND-COMPOSITE-CBSA-WI
                     THRU 0700-FIND-EXIT
                  PERFORM 0800-FIND-BUNDLED-CBSA-WI
                     THRU 0800-FIND-EXIT
              ELSE
                 IF (B-THRU-DATE > 20081231)  THEN
      * Process 2009 - 2010 claims.
                    MOVE ZERO              TO W-NEW-RATE1-RECORD
                                              W-NEW-RATE2-RECORD
                    PERFORM 0700-FIND-COMPOSITE-CBSA-WI
                       THRU 0700-FIND-EXIT
                 ELSE
                    IF (B-THRU-DATE > 20051231 AND
                        B-THRU-DATE < 20090101)  THEN
      * Process 2006 - 08 claims.
                          PERFORM 0500-FIND-MSA-WAGE-ADJ-RATE
                             THRU 0500-FIND-EXIT
                          PERFORM 0700-FIND-COMPOSITE-CBSA-WI
                             THRU 0700-FIND-EXIT
                    ELSE
                       IF (B-THRU-DATE > 20050331 AND
                           B-THRU-DATE < 20060101)  THEN
      * Process 2005 claims.
                           PERFORM 0500-FIND-MSA-WAGE-ADJ-RATE
                              THRU 0500-FIND-EXIT
                       ELSE
                           MOVE 98         TO PPS-RTC
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
      *RTC > 00  --  WAGE ADJUSTED RATE NOT FOUND.
           IF PPS-RTC > 00  THEN
              GO TO 0100-EXIT-DRIVER
           END-IF.
050100
050200
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20200101 AND 20201231              *
      ******************************************************************
050700
           IF (B-THRU-DATE > 20191231  AND
               B-THRU-DATE < 20210101)  THEN
              CALL ESCAL200 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
                                  BUN-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
051700
051800
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20190101 AND 20191231              *
      ******************************************************************
052300
           IF (B-THRU-DATE > 20181231  AND
               B-THRU-DATE < 20200101)  THEN
              CALL ESCAL191 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
                                  BUN-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
053300
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20180101 AND 20181231              *
      ******************************************************************
053800
           IF (B-THRU-DATE > 20171231  AND
               B-THRU-DATE < 20190101)  THEN
              CALL ESCAL180 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
                                  BUN-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
054800
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20170701 AND 20171231              *
      ******************************************************************
055300
           IF (B-THRU-DATE > 20170630  AND
               B-THRU-DATE < 20180101)  THEN
              CALL ESCAL171 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
                                  BUN-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
056300
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20170101 AND 20170630              *
      ******************************************************************
056800
           IF (B-THRU-DATE > 20161231  AND
               B-THRU-DATE < 20170701)  THEN
              CALL ESCAL170 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
                                  BUN-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
057800
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20160101 AND 20161231              *
      ******************************************************************
058300
           IF (B-THRU-DATE > 20151231  AND
               B-THRU-DATE < 20170101)  THEN
              CALL ESCAL160 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
                                  BUN-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
059300
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20150101 AND 20151231              *
      ******************************************************************
059800
           IF (B-THRU-DATE > 20141231  AND
               B-THRU-DATE < 20160101)  THEN
              CALL ESCAL151 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
                                  BUN-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
060800
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20140101 AND 20141231              *
      ******************************************************************
061300
           IF (B-THRU-DATE > 20131231  AND
               B-THRU-DATE < 20150101)  THEN
              CALL ESCAL140 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
                                  BUN-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
062300
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20130101 AND 20131231              *
      ******************************************************************
062800
           IF (B-THRU-DATE > 20121231  AND
               B-THRU-DATE < 20140101)  THEN
              CALL ESCAL130 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
                                  BUN-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
063800
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20120101 AND 20121231              *
      *                  Remove this CALL in 2022.                     *
      ******************************************************************
064400
           IF (B-THRU-DATE > 20111231  AND
               B-THRU-DATE < 20130101)  THEN
              CALL ESCAL122 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
                                  BUN-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
065400
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20110101 AND 20111231              *
      *                  Remove this CALL in 2022.                     *
      ******************************************************************
066000
           IF (B-THRU-DATE > 20101231  AND
               B-THRU-DATE < 20120101)  THEN
              CALL ESCAL117 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
                                  BUN-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
067000
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20100101 AND 20101231              *
      *                  Remove this CALL in 2021.                     *
      ******************************************************************
067600
           IF (B-THRU-DATE > 20091231  AND
               B-THRU-DATE < 20110101)  THEN
              CALL ESCAL100 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
068500
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20090101 AND 20091231              *
      * NOTE:  THERE IS NO ESCAL090 DUE TO THIS VERSION BEING RELEASED *
      *        BEFORE JANUARY 2009 (AND AFTER THE INITIAL 9.0 RELEASE) *
      *                  Remove this CALL in 2020.                     *
      ******************************************************************
069300
           IF (B-THRU-DATE > 20081231  AND
               B-THRU-DATE < 20100101)  THEN
              CALL ESCAL091 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
070200
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20080101 AND 20081231              *
      *                  Remove this CALL in 2019.                     *
      ******************************************************************
070800
           IF (B-THRU-DATE > 20071231  AND
               B-THRU-DATE < 20090101)  THEN
              CALL ESCAL080 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
071700
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH  A             *
      *           THRU DATE BETWEEN 20070401 AND 20071231              *
      *                  Remove this CALL in 2018.                     *
      ******************************************************************
072300
           IF (B-THRU-DATE > 20070331  AND
               B-THRU-DATE < 20080101)  THEN
              CALL ESCAL071 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
073200
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20070101 AND 20070331              *
      *                  Remove this CALL in 2018.                     *
      ******************************************************************
073800
           IF (B-THRU-DATE > 20061231  AND
               B-THRU-DATE < 20070401)  THEN
              CALL ESCAL070 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
074700
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20060101 AND 20061231              *
      *                  Remove this CALL in 2017.                     *
      ******************************************************************
075300
           IF (B-THRU-DATE > 20051231  AND
               B-THRU-DATE < 20070101)  THEN
              CALL ESCAL062 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
                                  COM-CBSA-WAGE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
076200
      ******************************************************************
      *           THE NEXT CALL WILL PROCESS BILLS WITH A              *
      *           THRU DATE BETWEEN 20050401 AND 20051231              *
      *                  Remove this CALL in 2016.                     *
      ******************************************************************
076800
           IF (B-THRU-DATE > 20050331  AND
               B-THRU-DATE < 20060101)  THEN
              CALL ESCAL056 USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  WAGE-NEW-RATE-RECORD
              GO TO 0100-EXIT-DRIVER
           END-IF.
077600
077700
077800
       0100-EXIT-DRIVER.
      *    DISPLAY 'GOING BACK'.
           GOBACK.
      ******************************************************************
      /
      *0400-CHECK-CBSA-ADDS-DELETES.
      *
      *    IF B-THRU-CCYY > 2010  THEN
      ** These CBSAs Deleted starting in 2011                          *
      ** Note that '14600' also appears when   < 2009                  *
      ** There is no CBSA that was deleted starting in 2010            *
      *       IF P-GEO-CBSA = '14600' OR  '23020'  OR  '48260'  THEN
      *          IF MAINFRAME-PC-SWITCH = DS-ERROR-CODE  THEN
      *             MOVE 60             TO PPS-RTC
      *             GO TO 0400-ADD-DELETE-EXIT
      *          ELSE
      *             MOVE 62             TO PPS-RTC
      *             GO TO 0400-ADD-DELETE-EXIT
      *          END-IF
      *       END-IF
      *    END-IF.
      *
      *    IF B-THRU-CCYY > 2008  THEN
      ** This CBSA Deleted starting in 2009                            *
      *       IF P-GEO-CBSA = '42260'  THEN
      *          IF MAINFRAME-PC-SWITCH = DS-ERROR-CODE  THEN
      *             MOVE 60             TO PPS-RTC
      *             GO TO 0400-ADD-DELETE-EXIT
      *          ELSE
      *             MOVE 62             TO PPS-RTC
      *             GO TO 0400-ADD-DELETE-EXIT
      *          END-IF
      *       END-IF
      *    END-IF.
      *
      *    IF B-THRU-CCYY > 2007  THEN
      ** This CBSA Deleted starting in 2008                            *
      *       IF P-GEO-CBSA = '21604'  THEN
      *          IF MAINFRAME-PC-SWITCH = DS-ERROR-CODE  THEN
      *             MOVE 60             TO PPS-RTC
      *             GO TO 0400-ADD-DELETE-EXIT
      *          ELSE
      *             MOVE 62             TO PPS-RTC
      *             GO TO 0400-ADD-DELETE-EXIT
      *          END-IF
      *       END-IF
      *    END-IF.
      *
      *    IF B-THRU-CCYY > 2006  THEN
      ** This CBSA Deleted starting in 2007                            *
      *       IF P-GEO-CBSA = '46940'  THEN
      *          IF MAINFRAME-PC-SWITCH = DS-ERROR-CODE  THEN
      *             MOVE 60             TO PPS-RTC
      *             GO TO 0400-ADD-DELETE-EXIT
      *          ELSE
      *             MOVE 62             TO PPS-RTC
      *             GO TO 0400-ADD-DELETE-EXIT
      *          END-IF
      *       END-IF
      *    END-IF.
      *
      *    IF B-THRU-CCYY < 2011  THEN
      ** These CBSAs Added starting in 2011                            *
      *      IF P-GEO-CBSA = '18880'  OR
      *                      '35840'  OR
      *                      '44600'  THEN
      *        IF MAINFRAME-PC-SWITCH = DS-ERROR-CODE  THEN
      *          MOVE 60                TO PPS-RTC
      *          GO TO 0400-ADD-DELETE-EXIT
      *        ELSE
      *          MOVE 62                TO PPS-RTC
      *          GO TO 0400-ADD-DELETE-EXIT
      *        END-IF
      *      ELSE
      *        IF B-THRU-CCYY < 2010  THEN
      ** These CBSAs Added starting in 2010                            *
      *          IF P-GEO-CBSA = '31740'  OR
      *                          '31860'  THEN
      *            IF MAINFRAME-PC-SWITCH = DS-ERROR-CODE  THEN
      *              MOVE 60            TO PPS-RTC
      *              GO TO 0400-ADD-DELETE-EXIT
      *            ELSE
      *              MOVE 62            TO PPS-RTC
      *              GO TO 0400-ADD-DELETE-EXIT
      *            END-IF
      *          ELSE
      *            IF B-THRU-CCYY < 2009  THEN
      ** This CBSA Added starting in 2009                            *
      *              IF P-GEO-CBSA = '14600'  THEN
      *                IF MAINFRAME-PC-SWITCH = DS-ERROR-CODE  THEN
      *                  MOVE 60        TO PPS-RTC
      *                  GO TO 0400-ADD-DELETE-EXIT
      *                ELSE
      *                  MOVE 62        TO PPS-RTC
      *                  GO TO 0400-ADD-DELETE-EXIT
      *                END-IF
      *              ELSE
      *                IF B-THRU-CCYY < 2008  THEN
      ** These CBSAs Added in 2008                                     *
      *                  IF P-GEO-CBSA = '29420' OR
      *                                  '37380' OR
      *                                  '37764' THEN
      *                    IF MAINFRAME-PC-SWITCH = DS-ERROR-CODE  THEN
      *                      MOVE 60    TO PPS-RTC
      *                      GO TO 0400-ADD-DELETE-EXIT
      *                    ELSE
      *                      MOVE 62    TO PPS-RTC
      *                      GO TO 0400-ADD-DELETE-EXIT
      *                    END-IF
      *                  ELSE
      *                    IF B-THRU-CCYY < 2007  THEN
      ** This CBSA Added in 2007                                       *
      *                      IF P-GEO-CBSA = '42680'  THEN
      *                      IF MAINFRAME-PC-SWITCH = DS-ERROR-CODE THEN
      *                          MOVE 60 TO PPS-RTC
      *                          GO TO 0400-ADD-DELETE-EXIT
      *                        ELSE
      *                          MOVE 62 TO PPS-RTC
      *                          GO TO 0400-ADD-DELETE-EXIT
      *                        END-IF
      *                      END-IF
      *                    END-IF
      *                  END-IF
      *                END-IF
      *              END-IF
      *            END-IF
      *          END-IF
      *        END-IF
      *      END-IF
      *    END-IF.
      *
      *0400-ADD-DELETE-EXIT.
      *    EXIT.
      /
       0500-FIND-MSA-WAGE-ADJ-RATE.
           MOVE WWD-MAX                 TO WWD-SUB.
091500
           PERFORM UNTIL B-THRU-DATE NOT < WWD-DATE (WWD-SUB)
               SUBTRACT 1 FROM WWD-SUB
           END-PERFORM.
091900
           SEARCH ALL WWM-ENTRY
             AT END
                MOVE 60                 TO PPS-RTC
                GO TO 0500-FIND-EXIT
             WHEN WWM-MSA (WWM-INDX) = P-GEO-MSA
                MOVE WWM-PTR (WWM-INDX) TO W-SUB1
                PERFORM 0550-N-GET-WAGE-RATE
                   THRU 0550-N-EXIT
           END-SEARCH.
092900
       0500-FIND-EXIT.
            EXIT.
093200
       0550-N-GET-WAGE-RATE.
           IF WWW-DTCD (W-SUB1) NOT > WWD-DTCD (WWD-SUB)  THEN
              MOVE WWD-DATE (WWD-SUB)   TO W-NEW-EFF-DATE
              MOVE WWW-WART1 (W-SUB1)   TO W-NEW-RATE1-RECORD
              MOVE WWW-WART2 (W-SUB1)   TO W-NEW-RATE2-RECORD
           ELSE
              SUBTRACT 1 FROM W-SUB1
              IF W-SUB1 > WWM-PTR (WWM-INDX - 1)  THEN
                 GO TO 0550-N-GET-WAGE-RATE
              ELSE
                MOVE 0                  TO W-NEW-RATE1-RECORD
                                           W-NEW-RATE2-RECORD
              END-IF
           END-IF.
094700
       0550-N-EXIT.
           EXIT.
      /
       0700-FIND-COMPOSITE-CBSA-WI.
           IF P-SPEC-PYMT-IND = '1'  THEN
              MOVE P-SPEC-WAGE-INDX     TO COM-CBSA-W-INDEX
              GO TO 0700-FIND-EXIT
           END-IF.
095600
           MOVE COM-MAX-DATE            TO COM-SUB.
095800
           PERFORM UNTIL B-THRU-DATE NOT < COM-DATE (COM-SUB)
               SUBTRACT 1 FROM COM-SUB
           END-PERFORM.
096200
           SEARCH ALL COM-CBSA-ENTRY
             AT END
                IF MAINFRAME-PC-SWITCH = DS-ERROR-CODE  THEN
                   MOVE 60              TO PPS-RTC
                   GO TO 0700-FIND-EXIT
                ELSE
                   MOVE 61              TO PPS-RTC
                   GO TO 0700-FIND-EXIT
                END-IF
             WHEN COM-CBSA-VALUE (COM-INDX) = P-GEO-CBSA
                MOVE COM-PTR (COM-INDX) TO W-SUB2
                PERFORM 0750-GET-COMP-CBSA-RATE
                   THRU 0750-COMP-EXIT
           END-SEARCH.
097700
       0700-FIND-EXIT.
            EXIT.
098000
       0750-GET-COMP-CBSA-RATE.
           IF COM-WI-DATE-CODE (W-SUB2) NOT > COM-DATE-CODE (COM-SUB)
                                                         THEN
              MOVE COM-DATE (COM-SUB)   TO COM-CBSA-DATE
              MOVE COM-WAGE-INDEX (W-SUB2)
                                        TO COM-CBSA-W-INDEX
           ELSE
              SUBTRACT 1 FROM W-SUB2
              IF W-SUB2 > COM-PTR (COM-INDX - 1)  THEN
                 GO TO 0750-GET-COMP-CBSA-RATE
              ELSE
                 MOVE 0                 TO COM-CBSA-W-INDEX
              END-IF
           END-IF.
099500
       0750-COMP-EXIT.
           EXIT.
      ******************************************************************
      /
       0800-FIND-BUNDLED-CBSA-WI.
           IF P-SPEC-PYMT-IND = '1'  THEN
              MOVE P-SPEC-WAGE-INDX     TO BUN-CBSA-W-INDEX
              GO TO 0800-FIND-EXIT
           END-IF.
100500
           IF B-THRU-DATE > 20141231  AND  B-THRU-DATE < 20160101
            MOVE "N" TO CHILD-HOSP-SWI-FOUND-SWITCH
            PERFORM 0820-SEARCH-CHILD-HOSP-TABLE
                WITH TEST AFTER
                VARYING CHILD-HOSP-TABLE-SUB FROM 1 BY 1
                UNTIL CHILD-HOSP-SWI-FOUND
                   OR CHILD-HOSP-TABLE-SUB = TOTAL-NUM-OF-CHILD-HOSP
            IF CHILD-HOSP-SWI-FOUND
                MOVE CHILD-HOSP-SWI (CHILD-HOSP-TABLE-SUB) TO
                   BUN-CBSA-W-INDEX
                GO TO 0800-FIND-EXIT.
101700
           MOVE BUN-MAX-DATE            TO BUN-SUB.
101900
      * FOR CY 2015 VERSION 0 ADDED NEXT LINE TO HOLD THE YEAR CODE
      * THAT WILL BE USED TO CHECK THAT THE YEAR OF THE WAGE INDEX
      * THAT'S BEING USED TO PRICE THE CLAIM IS THE SAME AS THE YEAR
      * OF THE CLAIM
      *    MOVE B-THRU-DATE (4:1)   TO B-THRU-YEAR-CODE.
      * CY2020 CHANGED TO ALLOW PRICING OF 2020 AND LATER CLAIMS
           MOVE B-THRU-CCYY (3:2)   TO B-THRU-YEAR-CODE.
           COMPUTE B-THRU-YEAR-CODE = B-THRU-YEAR-CODE - 10.
102600
           PERFORM UNTIL B-THRU-DATE NOT < BUN-DATE (BUN-SUB)
               SUBTRACT 1 FROM BUN-SUB
           END-PERFORM.
103000
           SEARCH ALL BUN-CBSA-ENTRY
             AT END
                IF MAINFRAME-PC-SWITCH = DS-ERROR-CODE  THEN
                   MOVE 60              TO PPS-RTC
                   GO TO 0800-FIND-EXIT
                ELSE
                   MOVE 61              TO PPS-RTC
                   GO TO 0800-FIND-EXIT
                END-IF
             WHEN BUN-CBSA-VALUE (BUN-INDX) = P-GEO-CBSA
                MOVE BUN-PTR (BUN-INDX) TO W-SUB3
                PERFORM 0850-GET-BUNDLED-CBSA-RATE
                   THRU 0850-BUNDLED-EXIT
           END-SEARCH.
104500
       0800-FIND-EXIT.
            EXIT.
104800
       0820-SEARCH-CHILD-HOSP-TABLE.
           IF CHILD-HOSP-PROV (CHILD-HOSP-TABLE-SUB) = P-PROV-OSCAR
              SET CHILD-HOSP-SWI-FOUND TO TRUE.
      ****** Replaced the 850 paragraph from 2014
      ****** to fix a problem with the CBSA lookup that would
      ****** cause it to price with wage indexes from previous years if
      ****** it couldn't find a wage index to match the year of the clai
       0850-GET-BUNDLED-CBSA-RATE.
      * CY 2015 ADD CHECK TO MAKE SURE THAT THE YEAR OF THE
      * WAGE INDEX RECORD IS THE SAME AS THE YEAR OF THE BILL
           IF
              (BUN-WI-DATE-CODE (W-SUB3) =
               B-THRU-YEAR-CODE)
           THEN
              MOVE BUN-DATE (BUN-SUB)   TO BUN-CBSA-DATE
              MOVE BUN-WAGE-INDEX (W-SUB3)
                                        TO BUN-CBSA-W-INDEX
           ELSE
              SUBTRACT 1 FROM W-SUB3
              IF W-SUB3 > BUN-PTR (BUN-INDX - 1)
              THEN GO TO 0850-GET-BUNDLED-CBSA-RATE
              ELSE
                MOVE 0                 TO BUN-CBSA-W-INDEX
      * FOR CY 2015 VERSION 0 ADDED ASSIGNMENT OF RETURN CODE
      * WHEN THERE IS NO CBSA FOUND FOR THE YEAR OF THE CLAIM
                 MOVE 60 TO PPS-RTC
              END-IF
           END-IF.
      * the following code is the old way to search the Wage Index
      * Table that was dropped because it would price claims using
      * a CBSA with a previous year's Wage Index even though the
      * CBSA had been dropped for the year of the claim
      *0850-GET-BUNDLED-CBSA-RATE.
      *    IF BUN-WI-DATE-CODE (W-SUB3) NOT > BUN-DATE-CODE (BUN-SUB)
      *                                                  THEN
      *       MOVE BUN-DATE (BUN-SUB)   TO BUN-CBSA-DATE
      *       MOVE BUN-WAGE-INDEX (W-SUB3)
      *                                 TO BUN-CBSA-W-INDEX
      *    ELSE
      *       SUBTRACT 1 FROM W-SUB3
      *       IF W-SUB3 > BUN-PTR (BUN-INDX - 1)  THEN
      *          GO TO 0850-GET-BUNDLED-CBSA-RATE
      *       ELSE
      *          MOVE 0                 TO BUN-CBSA-W-INDEX
      *       END-IF
      *    END-IF.
       0850-BUNDLED-EXIT.
           EXIT.
