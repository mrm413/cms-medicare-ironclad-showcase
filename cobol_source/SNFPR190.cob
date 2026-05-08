       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.      SNFPR190.                                       
      *AUTHOR.                 CMS.                                     
      *                                                                 
      *                                                                 
      *REMARKS. (CENTERS FOR MEDICARE AND MEDICAID SERVICES)            
      ***         - CICS VERSION RELEASED FOR JULY 1 2006               
      ***         - NATIONAL SNF PRICER FOR ALL YEARS                   
      ***         - NATIONAL SNF PRICER EFFECTIVE JULY 1, 1998          
      ***         - SNF PRICER REFERS TO A PROGRAM WHICH WILL           
      ***           CALCULATE THE DAILY MEDICARE RATE UPON WHICH THE    
      ***           MEDICARE SNF PPS PAYMENT IS MADE. PRICER USES THE   
      ***           FACILITY'S PSF RATE, MSA AND BLEND INDICATOR WITH   
      ***           THE 'STATEMENT COVERS THROUGH DATE' ON THE CLAIM AND
      ***           HCPPS RATE CODE TO CALCULATE THE HCPPS RATES.       
      ***************************************************************   
      ******************************************************************
      *     SNFPR16A   EFFECTIVE OCT 1, 2015                            
      *                KEEP CBSA FILE FOR FY2016                        
      *                ICD10 TESTING SNF CALC MODULE  - SNFPR160        
      ******************************************************************
      ******************************************************************
      *     SNFPR160   EFFECTIVE OCT 1, 2015                            
      *                KEEP CBSA FILE FOR FY2016                        
      *                ICD10 TESTING SNF CALC MODULE  - SNFPR160        
      ******************************************************************
      ******************************************************************
      *     SNFPR170   EFFECTIVE OCT 1, 2016                            
      *                KEEP CBSA FILE FOR FY2017                        
      *                SNF CALC MODULE  - SNFPR170                      
      ******************************************************************
      ******************************************************************
      *     SNFPR180   EFFECTIVE OCT 1, 2017                            
      *                KEEP CBSA FILE FOR FY2017                        
      *                SNF CALC MODULE  - SNFPR180                      
      ******************************************************************
      ***************************************************************   
      *     SNFPR181   EFFECTIVE OCT 1, 2017                            
      *                KEEP CBSA FILE FOR FY2017                        
      *                SNF CALC MODULE  - SNFPR181                      
      *                ADD QRP PENALTY LOGIC - SNFPR181                 
      ******************************************************************
      ***--------------------------------------------------------****   
      *     SNFPR19B   EFFECTIVE OCT 1, 2018                            
      *                BETA VERSION USED FOR TESTING ONLY               
      *                KEEP CBSA FILE FOR FY2018                        
      *                KEEP PAYMENT RATES FOR FY2018                    
      *                SNF DRIVER - SNFDR19B                            
      *                VALUE BASED PAYMENT LOGIC ADDED                  
      ******************************************************************
      ******************************************************************
      ***--------------------------------------------------------****   
      *     SNFPR190   EFFECTIVE OCT 1, 2018                            
      *                USE CBSA FILE FOR FY2019                         
      *                USE PAYMENT RATES FOR FY2019                     
      *                SNF DRIVER - SNFDR190                            
      *                VALUE BASED PAYMENT LOGIC ADDED                  
      ******************************************************************
       DATE-COMPILED.                                                   
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SOURCE-COMPUTER.            IBM-370.                             
       OBJECT-COMPUTER.            IBM-370.                             
       INPUT-OUTPUT  SECTION.                                           
       FILE-CONTROL.                                                    
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       WORKING-STORAGE SECTION.                                         
       01  W-STORAGE-REF                  PIC X(46)  VALUE              
           'SNFPR19.0   - W O R K I N G   S T O R A G E'.               
       01  CAL-VERSION                    PIC X(09)  VALUE 'SNFPR19.0'. 
       01  R1                             PIC S9(04) COMP SYNC.         
       01  R2                             PIC S9(04) COMP SYNC.         
       01  R3                             PIC S9(04) COMP SYNC.         
       01  LABOR-NLABOR-PERCENT.                                        
           05 PERCENT-1999-LABOR     PIC 9V99999 VALUE 0.75888.         
           05 PERCENT-1999-NLABOR    PIC 9V99999 VALUE 0.24112.         
           05 PERCENT-2000-LABOR     PIC 9V99999 VALUE 0.77545.         
           05 PERCENT-2000-NLABOR    PIC 9V99999 VALUE 0.22455.         
           05 PERCENT-2001-LABOR     PIC 9V99999 VALUE 0.77870.         
           05 PERCENT-2001-NLABOR    PIC 9V99999 VALUE 0.22130.         
           05 PERCENT-2002-LABOR     PIC 9V99999 VALUE 0.75379.         
           05 PERCENT-2002-NLABOR    PIC 9V99999 VALUE 0.24621.         
           05 PERCENT-2003-LABOR     PIC 9V99999 VALUE 0.76128.         
           05 PERCENT-2003-NLABOR    PIC 9V99999 VALUE 0.23872.         
           05 PERCENT-2004-LABOR     PIC 9V99999 VALUE 0.76372.         
           05 PERCENT-2004-NLABOR    PIC 9V99999 VALUE 0.23628.         
           05 PERCENT-2005-LABOR     PIC 9V99999 VALUE 0.76222.         
           05 PERCENT-2005-NLABOR    PIC 9V99999 VALUE 0.23778.         
           05 PERCENT-2006-LABOR     PIC 9V99999 VALUE 0.75922.         
           05 PERCENT-2006-NLABOR    PIC 9V99999 VALUE 0.24078.         
           05 PERCENT-2007-LABOR     PIC 9V99999 VALUE 0.75839.         
           05 PERCENT-2007-NLABOR    PIC 9V99999 VALUE 0.24161.         
           05 PERCENT-2008-LABOR     PIC 9V99999 VALUE 0.70249.         
           05 PERCENT-2008-NLABOR    PIC 9V99999 VALUE 0.29751.         
      **************YEARCHANGE 2009.0 *******************************   
           05 PERCENT-2009-LABOR     PIC 9V99999 VALUE 0.69783.         
           05 PERCENT-2009-NLABOR    PIC 9V99999 VALUE 0.30217.         
      **************YEARCHANGE 2009.0 *******************************   
      **************YEARCHANGE 2010.0 *******************************   
           05 PERCENT-2010-LABOR     PIC 9V99999 VALUE 0.69840.         
           05 PERCENT-2010-NLABOR    PIC 9V99999 VALUE 0.30160.         
      **************YEARCHANGE 2010.0 *******************************   
      **************YEARCHANGE 2011.2 *******************************   
           05 PERCENT-2011-LABOR     PIC 9V99999 VALUE 0.69311.         
           05 PERCENT-2011-NLABOR    PIC 9V99999 VALUE 0.30689.         
      **************YEARCHANGE 2011.2 *******************************   
      **************YEARCHANGE 2012.0 *******************************   
           05 PERCENT-2012-LABOR     PIC 9V99999 VALUE 0.68693.         
           05 PERCENT-2012-NLABOR    PIC 9V99999 VALUE 0.31307.         
      **************YEARCHANGE 2012.0 *******************************   
      **************YEARCHANGE 2013.0 *******************************   
           05 PERCENT-2013-LABOR     PIC 9V99999 VALUE 0.68383.         
           05 PERCENT-2013-NLABOR    PIC 9V99999 VALUE 0.31617.         
      **************YEARCHANGE 2013.0 *******************************   
      **************YEARCHANGE 2014.0 *******************************   
           05 PERCENT-2014-LABOR     PIC 9V99999 VALUE 0.69545.         
           05 PERCENT-2014-NLABOR    PIC 9V99999 VALUE 0.30455.         
      **************YEARCHANGE 2014.0 *******************************   
      **************YEARCHANGE 2015.0 *******************************   
           05 PERCENT-2015-LABOR     PIC 9V99999 VALUE 0.69180.         
           05 PERCENT-2015-NLABOR    PIC 9V99999 VALUE 0.30820.         
      **************YEARCHANGE 2015.0 *******************************   
      **************YEARCHANGE 2016.0 *******************************   
           05 PERCENT-2016-LABOR     PIC 9V99999 VALUE 0.69100.         
           05 PERCENT-2016-NLABOR    PIC 9V99999 VALUE 0.30900.         
      **************YEARCHANGE 2016.0 *******************************   
      **************YEARCHANGE 2017.0 *******************************   
           05 PERCENT-2017-LABOR     PIC 9V99999 VALUE 0.68800.         
           05 PERCENT-2017-NLABOR    PIC 9V99999 VALUE 0.31200.         
      **************YEARCHANGE 2017.0 *******************************   
      **************YEARCHANGE 2018.0 *******************************   
           05 PERCENT-2018-LABOR     PIC 9V99999 VALUE 0.70800.         
           05 PERCENT-2018-NLABOR    PIC 9V99999 VALUE 0.29200.         
      **************YEARCHANGE 2018.0 *******************************   
                                                                        
      *****-------  YEARCHANGE 2019.0   ------***********************   
           05 PERCENT-2019-LABOR     PIC 9V99999 VALUE 0.70500.         
           05 PERCENT-2019-NLABOR    PIC 9V99999 VALUE 0.29500.         
      **************YEARCHANGE 2019.0 *******************************   
                                                                        
       01  WK-PAYMENT-RATE           PIC 9(06)V99 VALUE 0.              
       01  WK-FACILITY-RATE          PIC 9(06)V99 VALUE 0.              
       01  AIDS-ADD-ON-MULTIPLIER    PIC 9(02)V99 VALUE 02.28.          
       01  VBP-ADJ-PYMT-RATE         PIC 9(06)V99 VALUE 0.              
       01  INITIAL-PAYMENT-RATE      PIC 9(06)V99 VALUE 0.              
                                                                        
       01  HLD-PRICE-FROM-DATE.                                         
           10  HLD-PRICE-FROM-CC     PIC XX.                            
           10  HLD-PRICE-FROM-YY     PIC XX.                            
           10  HLD-PRICE-FROM-MMDD   PIC XXXX.                          
                                                                        
       01  HLD-PRICE-THRU-DATE.                                         
           10  HLD-PRICE-THRU-CC     PIC XX.                            
           10  HLD-PRICE-THRU-YY     PIC XX.                            
           10  HLD-PRICE-THRU-MMDD   PIC XXXX.                          
                                                                        
       01  HOLD-SNF-DATA.                                               
           10  HLD-MSA.                                                 
               15  HLD-MSA-1ST       PIC XX.                            
                   88  HLD-MSA-RURAL-CHECK VALUE '  '.                  
               15  HLD-MSA-2ND       PIC XX.                            
           10  HLD-CBSA.                                                
               15  HLD-CBSA-1ST      PIC XXX.                           
                   88  HLD-CBSA-RURAL-CHECK     VALUE '   ' '999'.      
               15  HLD-CBSA-2ND      PIC XX.                            
           10  HLD-SPEC-WI-IND       PIC X.                             
           10  HLD-SPEC-WI           PIC 9(02)V9(04).                   
           10  HLD-RUG-CODE.                                            
               15  HLD-RUG           PIC XXX.                           
               15  HLD2-RUG REDEFINES HLD-RUG.                          
                   20  HLD-RUG-POS12 PIC XX.                            
                   20  HLD-RUG-POS3  PIC 9.                             
               15  FILLER            PIC XX.                            
           10  HLD-FROM-DATE.                                           
               15  HLD-FROM-CC       PIC XX.                            
               15  HLD-FROM-YY       PIC XX.                            
               15  HLD-FROM-MMDD     PIC XXXX.                          
           10  HLD-THRU-DATE.                                           
               15  HLD-THRU-CC       PIC XX.                            
               15  HLD-THRU-YY       PIC XX.                            
               15  HLD-THRU-MMDD     PIC XXXX.                          
           10  HLD-FED-BLEND         PIC X.                             
               88  HLD-ALL-FACILITY        VALUE '0'.                   
               88  HLD-FED-BLEND-QRP-FAIL  VALUE '1'.                   
               88  HLD-FED-BLEND-5050PER   VALUE '2'.                   
               88  HLD-FED-BLEND-7525PER   VALUE '3'.                   
               88  HLD-FED-BLEND-100PER    VALUE '4'.                   
                                                                        
           10  HLD-FACILITY-RATE     PIC 99999V99.                      
                                                                        
           10  HLD-DIAGNOSIS-CODES.                                     
               15  HLD-PRIN-DIAG-CODE     PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE2   PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE3   PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE4   PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE5   PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE6   PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE7   PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE8   PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE9   PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE10  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE11  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE12  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE13  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE14  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE15  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE16  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE17  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE18  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE19  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE20  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE21  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE22  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE23  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE24  PIC X(07).                    
               15  HLD-OTHER-DIAG-CODE25                                
                                     PIC X(07).                         
           10  HLD-RATE-RTC.                                            
               15  HLD-PAYMENT-RATE  PIC 9(06)V99.                      
               15  HLD-RTC           PIC 99.                            
           10  HLD-VBP-DATA.                                            
               15  HLD-VBP-MULTIPLIER          PIC S9V9(11).            
               15  HLD-VBP-PAY-DIFF            PIC S9(06)V9(02).        
           10  FILLER                      PIC X(50).                   
                                                                        
                                                                        
      ***************************************************************   
      *    LAYUP TABLE AREA FOR REGIONS                             *   
      *  FORMAT FOR RATE TABLE 1998 AND 1999 FOR JULY 1 1998        *   
      *  FORMAT FOR RATE TABLE 2000 FOR OCT 1, 1999                 *   
      *  FORMAT FOR RATE TABLE 2001 FOR OCT 1, 2000                 *   
      *  FORMAT FOR RATE TABLE 2001 FOR APR 1, 2001                 *   
      *  FORMAT FOR RATE TABLE 2002 FOR OCT 1, 2001                 *   
      *  FORMAT FOR RATE TABLE 2003 FOR OCT 1, 2002                 *   
      *  FORMAT FOR RATE TABLE 2004 FOR OCT 1, 2003                 *   
      *  FORMAT FOR RATE TABLE 2005 FOR OCT 1, 2004                 *   
      *  FORMAT FOR RATE TABLE 2006 FOR OCT 1, 2005                 *   
      *  FORMAT FOR RATE TABLE 2007 FOR OCT 1, 2006                 *   
      *  FORMAT FOR RATE TABLE 2008 FOR OCT 1, 2007                 *   
      *  FORMAT FOR RATE TABLE 2009 FOR OCT 1, 2008                 *   
      *  FORMAT FOR RATE TABLE 2010 FOR OCT 1, 2009                 *   
      *  FORMAT FOR RATE TABLE 2011 FOR OCT 1, 2010                 *   
      *  FORMAT FOR RATE TABLE 2012 FOR OCT 1, 2011                 *   
      *  FORMAT FOR RATE TABLE 2013 FOR OCT 1, 2012                 *   
      *  FORMAT FOR RATE TABLE 2014 FOR OCT 1, 2013                 *   
      *  FORMAT FOR RATE TABLE 2015 FOR OCT 1, 2014                 *   
      *  FORMAT FOR RATE TABLE 2016 FOR OCT 1, 2015                 *   
      *  FORMAT FOR RATE TABLE 2017 FOR OCT 1, 2016                 *   
      *  REGION = ( 1 = RURAL  2 = URBAN ) RATES                    *   
      ***************************************************************   
       01  REGION-TABLE.                                                
           02  REGION-DATA.                                             
      *  TABLE RATE FOR FY1998 AND 1999                                 
      *  19980701/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '19980701'.                    
           05  FILLER    PIC X(25) VALUE '1 10488 05695 01166 09551'.   
           05  FILLER    PIC X(25) VALUE '2 10948 05588 01091 08267'.   
      *  TABLE RATE FOR FY2000                                          
      *  19991001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '19991001'.                    
           05  FILLER    PIC X(25) VALUE '1 10712 05825 01188 09733'.   
           05  FILLER    PIC X(25) VALUE '2 11189 05720 01112 08425'.   
      *  TABLE RATE FOR FY2001                                          
      *  20001001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20001001'.                    
           05  FILLER    PIC X(25) VALUE '1 10929 05945 01213 09934'.   
           05  FILLER    PIC X(25) VALUE '2 11438 05838 01135 08616'.   
      *  TABLE RATE FOR FY2001 EFFECTIVE APR 1, 2001                    
      *  20010401/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20010401'.                    
           05  FILLER    PIC X(25) VALUE '1 12999 06062 01236 10129'.   
           05  FILLER    PIC X(25) VALUE '2 13605 05953 01157 08784'.   
      *  TABLE RATE FOR FY2002 EFFECTIVE OCT 1, 2001                    
      *  20011001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20011001'.                    
           05  FILLER    PIC X(25) VALUE '1 13213 06162 01256 10296'.   
           05  FILLER    PIC X(25) VALUE '2 13829 06050 01176 08929'.   
      *  TABLE RATE FOR FY2003 EFFECTIVE OCT 1, 2002                    
      *  20021001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20021001'.                    
           05  FILLER    PIC X(25) VALUE '1 11617 06320 01288 10561'.   
           05  FILLER    PIC X(25) VALUE '2 12159 06205 01206 09158'.   
      **************YEARCHANGE 2004.0 *******************************   
      *  TABLE RATE FOR FY2004 EFFECTIVE OCT 1, 2003                    
      *  20031001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20031001'.                    
           05  FILLER    PIC X(25) VALUE '1 12374 06733 01372 11250'.   
           05  FILLER    PIC X(25) VALUE '2 12952 06610 01285 09756'.   
      **************YEARCHANGE 2005.0 *******************************   
      *  TABLE RATE FOR FY2005 EFFECTIVE OCT 1, 2004                    
      *  20041001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20041001'.                    
           05  FILLER    PIC X(25) VALUE '1 12734 06929 01412 11578'.   
           05  FILLER    PIC X(25) VALUE '2 13329 06803 01322 10040'.   
      **************YEARCHANGE 2006.0 *******************************   
      *  TABLE RATE FOR FY2006 EFFECTIVE OCT 1, 2005                    
      *  20051001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20051001'.                    
           05  FILLER    PIC X(25) VALUE '1 13145 07152 01458 11951'.   
           05  FILLER    PIC X(25) VALUE '2 13759 07022 01365 10364'.   
      **************YEARCHANGE 2007.0 *******************************   
      *  TABLE RATE FOR FY2007 EFFECTIVE OCT 1, 2006                    
      *  20061001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20061001'.                    
           05  FILLER    PIC X(25) VALUE '1 13570 07383 01505 12337'.   
           05  FILLER    PIC X(25) VALUE '2 14204 07249 01409 10699'.   
      **************YEARCHANGE 2008.0 *******************************   
      *  TABLE RATE FOR FY2008 EFFECTIVE OCT 2, 2007                    
      *  20071001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20071001'.                    
           05  FILLER    PIC X(25) VALUE '1 14008 07621 01554 12735'.   
           05  FILLER    PIC X(25) VALUE '2 14662 07483 01454 11044'.   
      **************YEARCHANGE 2009.0 *******************************   
      *  TABLE RATE FOR FY2009 EFFECTIVE OCT 1, 2008                    
      *  20081001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20081001'.                    
           05  FILLER    PIC X(25) VALUE '1 14497 07887 01608 13180'.   
           05  FILLER    PIC X(25) VALUE '2 15174 07744 01505 11430'.   
      **************YEARCHANGE 2009.0 *******************************   
      **************YEARCHANGE 2010.0 *******************************   
      *  TABLE RATE FOR FY2009 EFFECTIVE OCT 2, 2009                    
      *  20091001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20091001'.                    
           05  FILLER    PIC X(25) VALUE '1 14831 08069 01645 13483'.   
           05  FILLER    PIC X(25) VALUE '2 15523 07922 01540 11693'.   
      **************YEARCHANGE 2010.0 *******************************   
      **************YEARCHANGE 2011.0 *******************************   
      *  TABLE RATE FOR FY2009 EFFECTIVE OCT 2, 2009                    
      *  20091001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20101001'.                    
           05  FILLER    PIC X(25) VALUE '1 15079 08204 01672 13708'.   
           05  FILLER    PIC X(25) VALUE '2 15782 08054 01566 11888'.   
      **************YEARCHANGE 2011.0 *******************************   
      **************YEARCHANGE 2012.0 *******************************   
      *  TABLE RATE FOR FY2012 EFFECTIVE OCT 1, 2011                    
      *  20111001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20111001'.                    
           05  FILLER    PIC X(25) VALUE '1 15346 08349 01702 13951'.   
           05  FILLER    PIC X(25) VALUE '2 16062 08197 01594 12099'.   
      **************YEARCHANGE 2012.0 *******************************   
      **************YEARCHANGE 2013.0 *******************************   
      *  TABLE RATE FOR FY2013 EFFECTIVE OCT 1, 2012                    
      *  20121001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20121001'.                    
           05  FILLER    PIC X(25) VALUE '1 15628 08503 01733 14208'.   
           05  FILLER    PIC X(25) VALUE '2 16358 08348 01623 12322'.   
      **************YEARCHANGE 2013.0 *******************************   
      **************YEARCHANGE 2014.0 *******************************   
      *  TABLE RATE FOR FY2014 EFFECTIVE OCT 1, 2013                    
      *  20131001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20131001'.                    
           05  FILLER    PIC X(25) VALUE '1 15841 08619 01757 14401'.   
           05  FILLER    PIC X(25) VALUE '2 16581 08462 01645 12490'.   
      **************YEARCHANGE 2014.0 *******************************   
      **************YEARCHANGE 2015.0 *******************************   
      *  TABLE RATE FOR FY2015 EFFECTIVE OCT 1, 2014                    
      *  20141001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20141001'.                    
           05  FILLER    PIC X(25) VALUE '1 16172 08799 01794 14702'.   
           05  FILLER    PIC X(25) VALUE '2 16928 08639 01679 12751'.   
      **************YEARCHANGE 2015.0 *******************************   
      **************YEARCHANGE 2016.0 *******************************   
      *  TABLE RATE FOR FY2016 EFFECTIVE OCT 1, 2015                    
      *  20151001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20151001'.                    
           05  FILLER    PIC X(25) VALUE '1 16353 08897 01814 14867'.   
           05  FILLER    PIC X(25) VALUE '2 17117 08736 01698 12894'.   
      **************YEARCHANGE 2016.0 *******************************   
      **************YEARCHANGE 2017.0 *******************************   
      *  TABLE RATE FOR FY2017 EFFECTIVE OCT 1, 2016                    
      *  20161001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20161001'.                    
           05  FILLER    PIC X(25) VALUE '1 16745 09111 01858 15224'.   
           05  FILLER    PIC X(25) VALUE '2 17528 08946 01739 13203'.   
      **************YEARCHANGE 2017.0 *******************************   
                                                                        
      **************YEARCHANGE 2017.0 *******************************   
           02  REGION-DATA2 REDEFINES REGION-DATA.                      
               04  REG-PERIOD        OCCURS 20.                         
      **************YEARCHANGE 2017.0 *******************************   
                   06  REG-EFF-DATE  PIC X(08).                         
                   06  REGS          OCCURS 2.                          
                       08  REG-DATA  OCCURS 1.                          
                           10  REGION        PIC X.                     
                           10  FILLER        PIC X.                     
                           10  R-IP-RATE     PIC 999V99.                
                           10  FILLER        PIC X.                     
                           10  R-GS-RATE     PIC 999V99.                
                           10  FILLER        PIC X.                     
                           10  R-TH-RATE     PIC 999V99.                
                           10  FILLER        PIC X.                     
                           10  R-REHAB-RATE  PIC 999V99.                
                                                                        
                                                                        
      ***************************************************************   
      *  LAYUP TABLE AREA FOR RUG  44 CODES                         *   
      *  EFFECTIVE JULY 1, 1998  TIL JANUARY 1, 2006                *   
      *  FORMAT FOR RUG TABLE 44                                    *   
      *    N = NURSING   T = THERAPY  F = THERAPY COMPONENT FACTOR  *   
      ***************************************************************   
       01  RUG-TABLE.                                                   
           02  RUG-DATA1.                                               
      *                               RUG  REV                          
      *                               CODE CODE N - T - F               
               03  FILLER  PIC X(19)                                    
                               VALUE 'AAA  9000 046 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PA1  9001 046 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PA2  9002 049 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PB1  9003 050 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PB2  9004 051 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PC1  9005 064 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PC2  9006 065 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PD1  9007 070 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PD2  9008 072 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PE1  9009 077 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PE2  9010 079 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BA1  9011 048 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BA2  9012 056 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BB1  9013 065 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BB2  9014 068 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IA1  9015 053 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IA2  9016 057 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IB1  9017 067 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IB2  9018 069 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CA1  9019 075 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CA2  9020 083 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CB1  9021 084 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CB2  9022 091 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CC1  9023 099 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CC2  9024 112 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SSA  9025 101 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SSB  9026 105 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SSC  9027 113 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SE1  9028 117 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SE2  9029 139 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SE3  9030 170 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RLA  9031 080 043 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RLB  9032 111 043 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RMA  9033 096 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RMB  9034 109 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RMC  9035 135 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHA  9036 087 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHB  9037 106 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHC  9038 126 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVA  9039 081 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVB  9040 104 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVC  9041 113 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUA  9042 078 225 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUB  9043 095 225 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUC  9044 130 225 0'.             
           02  RUG-DATA2 REDEFINES RUG-DATA1 OCCURS 45                  
                   ASCENDING KEY IS RUG-CODE                            
                   INDEXED BY RUG01.                                    
               03  RUG-CODE          PIC X(03).                         
               03  RUG-REGS             OCCURS 1.                       
                   04  FILLER        PIC XX.                            
                   04  REV-CODE      PIC XXXX.                          
                   04  FILLER        PIC X.                             
                   04  RUG-NURSING   PIC 9V99.                          
                   04  FILLER        PIC X.                             
                   04  RUG-THERAPY   PIC 9V99.                          
                   04  FILLER        PIC X.                             
                   04  RUG-FACTOR    PIC 9.                             
                                                                        
                                                                        
      ***************************************************************   
      *  LAYUP TABLE AREA FOR RUG 53 CODES                          *   
      *  EFFECTIVE JAN  1, 2006                                     *   
      *  FORMAT FOR RUG TABLE 53                                    *   
      *    N = NURSING   T = THERAPY  F = THERAPY COMPONENT FACTOR  *   
      ***************************************************************   
       01  RUG53-TABLE.                                                 
           02  RUG53-DATA1.                                             
      *                               RUG  REV                          
      *                               CODE CODE N - T - F               
               03  FILLER  PIC X(19)                                    
                               VALUE 'AAA  9000 050 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PA1  9001 050 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PA2  9002 053 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PB1  9003 054 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PB2  9004 055 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PC1  9005 069 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PC2  9006 071 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PD1  9007 076 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PD2  9008 078 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PE1  9009 082 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PE2  9010 085 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BA1  9011 052 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BA2  9012 060 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BB1  9013 069 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BB2  9014 073 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IA1  9015 056 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IA2  9016 061 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IB1  9017 072 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IB2  9018 074 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CA1  9019 080 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CA2  9020 090 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CB1  9021 091 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CB2  9022 098 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CC1  9023 106 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CC2  9024 122 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SSA  9025 110 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SSB  9026 113 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SSC  9027 123 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SE1  9028 126 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SE2  9029 149 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SE3  9030 186 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RLA  9031 085 043 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RLB  9032 114 043 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RMA  9033 104 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RMB  9034 109 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RMC  9035 115 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHA  9036 094 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHB  9037 111 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHC  9038 122 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVA  9039 082 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVB  9040 109 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVC  9041 123 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUA  9042 084 225 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUB  9043 099 225 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUC  9044 128 225 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RLX  9045 131 043 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RML  9046 168 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RMX  9047 193 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHL  9048 137 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHX  9049 142 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVL  9050 133 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVX  9051 154 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUL  9052 140 225 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUX  9053 190 225 0'.             
           02  RUG53-DATA2 REDEFINES RUG53-DATA1 OCCURS 54              
                   ASCENDING KEY IS RUG53-CODE                          
                   INDEXED BY RUG53-01.                                 
               03  RUG53-CODE          PIC X(03).                       
               03  RUG53-REGS             OCCURS 1.                     
                   04  FILLER        PIC XX.                            
                   04  REV53-CODE      PIC XXXX.                        
                   04  FILLER        PIC X.                             
                   04  RUG53-NURSING   PIC 9V99.                        
                   04  FILLER        PIC X.                             
                   04  RUG53-THERAPY   PIC 9V99.                        
                   04  FILLER        PIC X.                             
                   04  RUG53-FACTOR    PIC 9.                           
                                                                        
                                                                        
      ***************************************************************   
      *  LAYUP TABLE AREA FOR RUG 53 CODES FOR FY2010               *   
      *  EFFECTIVE OCT  1, 2009                                     *   
      *  FORMAT FOR RUG TABLE 53                                    *   
      *    N = NURSING   T = THERAPY  F = THERAPY COMPONENT FACTOR  *   
      ***************************************************************   
       01  RUG53-2010-TABLE.                                            
           02  RUG53-2010-DATA1.                                        
      *                               RUG  REV                          
      *                               CODE CODE N - T - F               
               03  FILLER  PIC X(19)                                    
                               VALUE 'AAA  9000 046 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PA1  9001 046 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PA2  9002 049 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PB1  9003 050 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PB2  9004 052 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PC1  9005 065 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PC2  9006 066 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PD1  9007 070 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PD2  9008 072 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PE1  9009 077 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'PE2  9010 079 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BA1  9011 048 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BA2  9012 056 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BB1  9013 065 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'BB2  9014 068 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IA1  9015 053 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IA2  9016 057 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IB1  9017 067 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'IB2  9018 069 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CA1  9019 075 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CA2  9020 083 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CB1  9021 084 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CB2  9022 091 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CC1  9023 099 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'CC2  9024 113 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SSA  9025 102 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SSB  9026 105 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SSC  9027 114 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SE1  9028 117 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SE2  9029 138 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'SE3  9030 172 000 1'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RLA  9031 079 043 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RLB  9032 106 043 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RMA  9033 097 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RMB  9034 101 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RMC  9035 107 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHA  9036 088 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHB  9037 103 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHC  9038 113 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVA  9039 077 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVB  9040 101 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVC  9041 114 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUA  9042 078 225 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUB  9043 092 225 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUC  9044 120 225 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RLX  9045 122 043 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RML  9046 157 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RMX  9047 180 077 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHL  9048 127 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RHX  9049 133 094 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVL  9050 124 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RVX  9051 144 141 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUL  9052 131 225 0'.             
               03  FILLER  PIC X(19)                                    
                               VALUE 'RUX  9053 177 225 0'.             
           02  RUG53-2010-DATA2 REDEFINES RUG53-2010-DATA1 OCCURS 54    
                   ASCENDING KEY IS RUG53-2010-CODE                     
                   INDEXED BY RUG53-2010-01.                            
               03  RUG53-2010-CODE     PIC X(03).                       
               03  RUG53-2010-REGS        OCCURS 1.                     
                   04  FILLER        PIC XX.                            
                   04  REV53-2010-CODE      PIC XXXX.                   
                   04  FILLER        PIC X.                             
                   04  RUG53-2010-NURSING PIC 9V99.                     
                   04  FILLER        PIC X.                             
                   04  RUG53-2010-THERAPY PIC 9V99.                     
                   04  FILLER        PIC X.                             
                   04  RUG53-2010-FACTOR PIC 9.                         
                                                                        
      ***************************************************************   
      *  LAYUP TABLE AREA FOR RUG 53 CODES FOR FY2011               *   
      *  EFFECTIVE OCT  2, 2010                                     *   
      *  FORMAT FOR RUG TABLE 53                                    *   
      *    N = NURSING   T = THERAPY  F = THERAPY COMPONENT FACTOR  *   
      ***************************************************************   
       01  RUG67-2011-TABLE.                                            
           02  RUG67-2011-DATA1.                                        
      *                                      RUG  REV                   
      *                                      CODE CODE N - T - F        
               03  FILLER  PIC X(19)  VALUE 'AAA  9000 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA1  9011 064 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA2  9012 070 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB1  9013 090 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB2  9014 097 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA1  9019 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA2  9020 088 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB1  9021 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB2  9022 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC1  9023 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC2  9024 129 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD1  9024 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD2  9024 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE1  9024 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE2  9028 168 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES1  9028 232 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES2  9029 267 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES3  9030 358 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB1  9015 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB2  9016 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC1  9017 148 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC2  9018 189 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD1  9018 160 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD2  9018 204 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE1  9018 174 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE2  9018 222 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB1  9018 114 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB2  9018 145 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC1  9018 122 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC2  9018 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD1  9018 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD2  9018 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE1  9018 154 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE2  9018 196 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA1  9001 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA2  9002 059 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB1  9003 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB2  9004 084 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC1  9005 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC2  9006 110 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD1  9007 128 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD2  9008 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE1  9009 140 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE2  9010 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'RHA  9036 123 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHB  9037 160 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHC  9038 194 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHL  9048 289 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHX  9049 343 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLA  9031 095 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLB  9032 201 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLX  9045 304 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMA  9033 113 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMB  9034 163 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMC  9035 183 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RML  9046 295 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMX  9047 331 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUA  9042 133 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUB  9043 210 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUC  9044 210 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUL  9052 345 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUX  9053 359 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVA  9039 148 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVB  9040 149 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVC  9041 202 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVL  9050 295 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVX  9051 351 128 0'.      
           02  RUG67-2011-DATA2 REDEFINES RUG67-2011-DATA1 OCCURS 67    
                   ASCENDING KEY IS RUG67-2011-CODE                     
                   INDEXED BY RUG67-2011-01.                            
               03  RUG67-2011-CODE     PIC X(03).                       
               03  RUG67-2011-REGS        OCCURS 1.                     
                   04  FILLER        PIC XX.                            
                   04  REV67-2011-CODE      PIC XXXX.                   
                   04  FILLER        PIC X.                             
                   04  RUG67-2011-NURSING PIC 9V99.                     
                   04  FILLER        PIC X.                             
                   04  RUG67-2011-THERAPY PIC 9V99.                     
                   04  FILLER        PIC X.                             
                   04  RUG67-2011-FACTOR PIC 9.                         
                                                                        
      ***************************************************************   
      *  LAYUP TABLE AREA FOR RUG 67 CODES FOR FY2012               *   
      *  EFFECTIVE OCT  1, 2011                                     *   
      *  FORMAT FOR RUG TABLE 67                                    *   
      *    N = NURSING   T = THERAPY  F = THERAPY COMPONENT FACTOR  *   
      ***************************************************************   
       01  RUG67-2012-TABLE.                                            
           02  RUG67-2012-DATA1.                                        
      *                                      RUG  REV                   
      *                                      CODE CODE N - T - F        
               03  FILLER  PIC X(19)  VALUE 'AAA  9000 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA1  9011 064 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA2  9012 070 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB1  9013 090 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB2  9014 097 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA1  9019 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA2  9020 088 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB1  9021 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB2  9022 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC1  9023 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC2  9024 129 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD1  9024 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD2  9024 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE1  9024 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE2  9028 168 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES1  9028 232 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES2  9029 267 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES3  9030 358 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB1  9015 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB2  9016 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC1  9017 148 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC2  9018 189 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD1  9018 160 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD2  9018 204 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE1  9018 174 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE2  9018 222 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB1  9018 114 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB2  9018 145 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC1  9018 122 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC2  9018 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD1  9018 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD2  9018 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE1  9018 154 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE2  9018 196 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA1  9001 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA2  9002 059 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB1  9003 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB2  9004 084 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC1  9005 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC2  9006 110 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD1  9007 128 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD2  9008 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE1  9009 140 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE2  9010 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'RHA  9036 091 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHB  9037 119 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHC  9038 145 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHL  9048 215 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHX  9049 255 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLA  9031 071 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLB  9032 150 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLX  9045 226 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMA  9033 084 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMB  9034 122 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMC  9035 136 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RML  9046 219 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMX  9047 247 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUA  9042 099 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUB  9043 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUC  9044 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUL  9052 257 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUX  9053 267 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVA  9039 110 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVB  9040 111 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVC  9041 151 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVL  9050 219 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVX  9051 261 128 0'.      
           02  RUG67-2012-DATA2 REDEFINES RUG67-2012-DATA1 OCCURS 67    
                   ASCENDING KEY IS RUG67-2012-CODE                     
                   INDEXED BY RUG67-2012-01.                            
               03  RUG67-2012-CODE     PIC X(03).                       
               03  RUG67-2012-REGS        OCCURS 1.                     
                   04  FILLER        PIC XX.                            
                   04  REV67-2012-CODE      PIC XXXX.                   
                   04  FILLER        PIC X.                             
                   04  RUG67-2012-NURSING PIC 9V99.                     
                   04  FILLER        PIC X.                             
                   04  RUG67-2012-THERAPY PIC 9V99.                     
                   04  FILLER        PIC X.                             
                   04  RUG67-2012-FACTOR PIC 9.                         
                                                                        
      *********************>>  2013  <<**********************           
                                                                        
                                                                        
      ***************************************************************   
      *  LAYUP TABLE AREA FOR RUG 67 CODES FOR FY2013               *   
      *  EFFECTIVE OCT  1, 2012                                     *   
      *  FORMAT FOR RUG TABLE 67                                    *   
      *    N = NURSING   T = THERAPY  F = THERAPY COMPONENT FACTOR  *   
      ***************************************************************   
       01  RUG67-2013-TABLE.                                            
           02  RUG67-2013-DATA1.                                        
      *                                      RUG  REV                   
      *                                      CODE CODE N - T - F        
               03  FILLER  PIC X(19)  VALUE 'AAA  9000 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA1  9011 064 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA2  9012 070 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB1  9013 090 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB2  9014 097 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA1  9019 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA2  9020 088 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB1  9021 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB2  9022 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC1  9023 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC2  9024 129 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD1  9024 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD2  9024 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE1  9024 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE2  9028 168 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES1  9028 232 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES2  9029 267 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES3  9030 358 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB1  9015 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB2  9016 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC1  9017 148 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC2  9018 189 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD1  9018 160 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD2  9018 204 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE1  9018 174 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE2  9018 222 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB1  9018 114 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB2  9018 145 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC1  9018 122 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC2  9018 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD1  9018 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD2  9018 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE1  9018 154 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE2  9018 196 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA1  9001 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA2  9002 059 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB1  9003 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB2  9004 084 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC1  9005 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC2  9006 110 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD1  9007 128 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD2  9008 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE1  9009 140 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE2  9010 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'RHA  9036 091 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHB  9037 119 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHC  9038 145 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHL  9048 215 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHX  9049 255 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLA  9031 071 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLB  9032 150 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLX  9045 226 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMA  9033 084 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMB  9034 122 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMC  9035 136 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RML  9046 219 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMX  9047 247 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUA  9042 099 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUB  9043 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUC  9044 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUL  9052 257 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUX  9053 267 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVA  9039 110 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVB  9040 111 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVC  9041 151 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVL  9050 219 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVX  9051 261 128 0'.      
           02  RUG67-2013-DATA2 REDEFINES RUG67-2013-DATA1 OCCURS 67    
                   ASCENDING KEY IS RUG67-2013-CODE                     
                   INDEXED BY RUG67-2013-01.                            
               03  RUG67-2013-CODE     PIC X(03).                       
               03  RUG67-2013-REGS        OCCURS 1.                     
                   04  FILLER        PIC XX.                            
                   04  REV67-2013-CODE      PIC XXXX.                   
                   04  FILLER        PIC X.                             
                   04  RUG67-2013-NURSING PIC 9V99.                     
                   04  FILLER        PIC X.                             
                   04  RUG67-2013-THERAPY PIC 9V99.                     
                   04  FILLER        PIC X.                             
                   04  RUG67-2013-FACTOR PIC 9.                         
                                                                        
                                                                        
                                                                        
      *********************>>  2013  <<**********************           
      *********************>>  2014  <<**********************           
                                                                        
                                                                        
      ***************************************************************   
      *  LAYUP TABLE AREA FOR RUG 67 CODES FOR FY2014               *   
      *  EFFECTIVE OCT  1, 2013                                     *   
      *  FORMAT FOR RUG TABLE 67                                    *   
      *    N = NURSING   T = THERAPY  F = THERAPY COMPONENT FACTOR  *   
      ***************************************************************   
       01  RUG67-2014-TABLE.                                            
           02  RUG67-2014-DATA1.                                        
      *                                      RUG  REV                   
      *                                      CODE CODE N - T - F        
               03  FILLER  PIC X(19)  VALUE 'AAA  9000 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA1  9011 064 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA2  9012 070 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB1  9013 090 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB2  9014 097 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA1  9019 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA2  9020 088 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB1  9021 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB2  9022 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC1  9023 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC2  9024 129 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD1  9024 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD2  9024 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE1  9024 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE2  9028 168 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES1  9028 232 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES2  9029 267 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES3  9030 358 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB1  9015 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB2  9016 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC1  9017 148 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC2  9018 189 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD1  9018 160 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD2  9018 204 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE1  9018 174 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE2  9018 222 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB1  9018 114 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB2  9018 145 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC1  9018 122 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC2  9018 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD1  9018 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD2  9018 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE1  9018 154 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE2  9018 196 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA1  9001 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA2  9002 059 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB1  9003 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB2  9004 084 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC1  9005 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC2  9006 110 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD1  9007 128 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD2  9008 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE1  9009 140 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE2  9010 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'RHA  9036 091 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHB  9037 119 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHC  9038 145 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHL  9048 215 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHX  9049 255 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLA  9031 071 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLB  9032 150 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLX  9045 226 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMA  9033 084 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMB  9034 122 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMC  9035 136 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RML  9046 219 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMX  9047 247 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUA  9042 099 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUB  9043 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUC  9044 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUL  9052 257 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUX  9053 267 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVA  9039 110 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVB  9040 111 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVC  9041 151 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVL  9050 219 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVX  9051 261 128 0'.      
           02  RUG67-2014-DATA2 REDEFINES RUG67-2014-DATA1 OCCURS 67    
                   ASCENDING KEY IS RUG67-2014-CODE                     
                   INDEXED BY RUG67-2014-01.                            
               03  RUG67-2014-CODE     PIC X(03).                       
               03  RUG67-2014-REGS        OCCURS 1.                     
                   04  FILLER               PIC XX.                     
                   04  REV67-2014-CODE      PIC XXXX.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2014-NURSING   PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2014-THERAPY   PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2014-FACTOR    PIC 9.                      
                                                                        
                                                                        
                                                                        
      *********************>>  2014  <<**********************           
      *********************>>  2015  <<**********************           
                                                                        
                                                                        
      ***************************************************************   
      *  LAYUP TABLE AREA FOR RUG 67 CODES FOR FY2015               *   
      *  EFFECTIVE OCT  1, 2014                                     *   
      *  FORMAT FOR RUG TABLE 67                                    *   
      *    N = NURSING   T = THERAPY  F = THERAPY COMPONENT FACTOR  *   
      ***************************************************************   
       01  RUG67-2015-TABLE.                                            
           02  RUG67-2015-DATA1.                                        
      *                                      RUG  REV                   
      *                                      CODE CODE N - T - F        
               03  FILLER  PIC X(19)  VALUE 'AAA  9000 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA1  9011 064 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA2  9012 070 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB1  9013 090 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB2  9014 097 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA1  9019 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA2  9020 088 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB1  9021 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB2  9022 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC1  9023 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC2  9024 129 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD1  9024 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD2  9024 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE1  9024 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE2  9028 168 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES1  9028 232 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES2  9029 267 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES3  9030 358 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB1  9015 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB2  9016 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC1  9017 148 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC2  9018 189 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD1  9018 160 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD2  9018 204 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE1  9018 174 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE2  9018 222 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB1  9018 114 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB2  9018 145 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC1  9018 122 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC2  9018 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD1  9018 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD2  9018 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE1  9018 154 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE2  9018 196 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA1  9001 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA2  9002 059 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB1  9003 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB2  9004 084 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC1  9005 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC2  9006 110 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD1  9007 128 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD2  9008 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE1  9009 140 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE2  9010 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'RHA  9036 091 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHB  9037 119 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHC  9038 145 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHL  9048 215 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHX  9049 255 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLA  9031 071 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLB  9032 150 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLX  9045 226 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMA  9033 084 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMB  9034 122 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMC  9035 136 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RML  9046 219 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMX  9047 247 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUA  9042 099 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUB  9043 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUC  9044 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUL  9052 257 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUX  9053 267 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVA  9039 110 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVB  9040 111 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVC  9041 151 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVL  9050 219 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVX  9051 261 128 0'.      
           02  RUG67-2015-DATA2 REDEFINES RUG67-2015-DATA1 OCCURS 67    
                   ASCENDING KEY IS RUG67-2015-CODE                     
                   INDEXED BY RUG67-2015-01.                            
               03  RUG67-2015-CODE     PIC X(03).                       
               03  RUG67-2015-REGS        OCCURS 1.                     
                   04  FILLER               PIC XX.                     
                   04  REV67-2015-CODE      PIC XXXX.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2015-NURSING   PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2015-THERAPY   PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2015-FACTOR    PIC 9.                      
                                                                        
                                                                        
                                                                        
      *********************>>  2015  <<**********************           
      *********************>>  2016  <<**********************           
                                                                        
                                                                        
      ***************************************************************   
      *  LAYUP TABLE AREA FOR RUG 67 CODES FOR FY2016               *   
      *  EFFECTIVE OCT  1, 2015                                     *   
      *  FORMAT FOR RUG TABLE 67                                    *   
      *    N = NURSING   T = THERAPY  F = THERAPY COMPONENT FACTOR  *   
      ***************************************************************   
       01  RUG67-2016-TABLE.                                            
           02  RUG67-2016-DATA1.                                        
      *                                      RUG  REV                   
      *                                      CODE CODE N - T - F        
               03  FILLER  PIC X(19)  VALUE 'AAA  9000 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA1  9011 064 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA2  9012 070 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB1  9013 090 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB2  9014 097 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA1  9019 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA2  9020 088 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB1  9021 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB2  9022 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC1  9023 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC2  9024 129 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD1  9024 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD2  9024 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE1  9024 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE2  9028 168 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES1  9028 232 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES2  9029 267 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES3  9030 358 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB1  9015 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB2  9016 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC1  9017 148 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC2  9018 189 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD1  9018 160 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD2  9018 204 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE1  9018 174 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE2  9018 222 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB1  9018 114 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB2  9018 145 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC1  9018 122 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC2  9018 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD1  9018 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD2  9018 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE1  9018 154 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE2  9018 196 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA1  9001 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA2  9002 059 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB1  9003 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB2  9004 084 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC1  9005 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC2  9006 110 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD1  9007 128 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD2  9008 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE1  9009 140 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE2  9010 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'RHA  9036 091 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHB  9037 119 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHC  9038 145 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHL  9048 215 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHX  9049 255 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLA  9031 071 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLB  9032 150 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLX  9045 226 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMA  9033 084 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMB  9034 122 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMC  9035 136 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RML  9046 219 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMX  9047 247 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUA  9042 099 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUB  9043 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUC  9044 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUL  9052 257 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUX  9053 267 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVA  9039 110 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVB  9040 111 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVC  9041 151 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVL  9050 219 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVX  9051 261 128 0'.      
           02  RUG67-2016-DATA2 REDEFINES RUG67-2016-DATA1 OCCURS 67    
                   ASCENDING KEY IS RUG67-2016-CODE                     
                   INDEXED BY RUG67-2016-01.                            
               03  RUG67-2016-CODE     PIC X(03).                       
               03  RUG67-2016-REGS        OCCURS 1.                     
                   04  FILLER               PIC XX.                     
                   04  REV67-2016-CODE      PIC XXXX.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2016-NURSING   PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2016-THERAPY   PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2016-FACTOR    PIC 9.                      
                                                                        
                                                                        
                                                                        
      *********************>>  2016  <<**********************           
      *********************>>  2017  <<**********************           
                                                                        
                                                                        
      ***************************************************************   
      *  LAYUP TABLE AREA FOR RUG 67 CODES FOR FY2017               *   
      *  EFFECTIVE OCT  1, 2016                                     *   
      *  FORMAT FOR RUG TABLE 67                                    *   
      *    N = NURSING   T = THERAPY  F = THERAPY COMPONENT FACTOR  *   
      ***************************************************************   
       01  RUG67-2017-TABLE.                                            
           02  RUG67-2017-DATA1.                                        
      *                                      RUG  REV                   
      *                                      CODE CODE N - T - F        
               03  FILLER  PIC X(19)  VALUE 'AAA  9000 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA1  9011 064 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA2  9012 070 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB1  9013 090 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB2  9014 097 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA1  9019 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA2  9020 088 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB1  9021 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB2  9022 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC1  9023 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC2  9024 129 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD1  9024 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD2  9024 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE1  9024 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE2  9028 168 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES1  9028 232 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES2  9029 267 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES3  9030 358 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB1  9015 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB2  9016 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC1  9017 148 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC2  9018 189 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD1  9018 160 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD2  9018 204 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE1  9018 174 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE2  9018 222 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB1  9018 114 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB2  9018 145 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC1  9018 122 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC2  9018 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD1  9018 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD2  9018 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE1  9018 154 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE2  9018 196 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA1  9001 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA2  9002 059 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB1  9003 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB2  9004 084 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC1  9005 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC2  9006 110 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD1  9007 128 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD2  9008 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE1  9009 140 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE2  9010 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'RHA  9036 091 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHB  9037 119 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHC  9038 145 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHL  9048 215 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHX  9049 255 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLA  9031 071 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLB  9032 150 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLX  9045 226 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMA  9033 084 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMB  9034 122 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMC  9035 136 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RML  9046 219 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMX  9047 247 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUA  9042 099 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUB  9043 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUC  9044 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUL  9052 257 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUX  9053 267 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVA  9039 110 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVB  9040 111 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVC  9041 151 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVL  9050 219 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVX  9051 261 128 0'.      
           02  RUG67-2017-DATA2 REDEFINES RUG67-2017-DATA1 OCCURS 67    
                   ASCENDING KEY IS RUG67-2017-CODE                     
                   INDEXED BY RUG67-2017-01.                            
               03  RUG67-2017-CODE     PIC X(03).                       
               03  RUG67-2017-REGS        OCCURS 1.                     
                   04  FILLER               PIC XX.                     
                   04  REV67-2017-CODE      PIC XXXX.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2017-NURSING   PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2017-THERAPY   PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2017-FACTOR    PIC 9.                      
                                                                        
      *********************>>  2017  <<**********************           
                                                                        
                                                                        
      *********************>>  2018  <<**********************           
      *********************>>  2018  <<**********************           
      ****>>  MAJOR TABLE FORMAT CHANGES FOR 2018     <<******          
      ****>>       ADD QRP PENALTY LOGIC              <<******          
      ****>>  NEW RUG COMPONENT TABLE WITH 4 REGIONS  <<******          
      *********************>>  2018  <<**********************           
      *********************>>  2018  <<**********************           
      *                                                                 
                                                                        
      **************YEARCHANGE 2018.0 *******************************   
      ***************************************************************   
      *    RATE COMPONENT TABLE AREA FOR REGIONS - QRP              *   
      *         STARTING IN 2018 & ALL FUTURE YEARS                 *   
      *  FORMAT FOR RATE TABLE 2018 FOR OCT 1, 2017                 *   
0R9300*  REGION = ( 1 = RURAL  RATES )                              *   
0R9300*           ( 2 = URBAN  RATES )                              *   
R19300*           ( 3 = RURAL-QRP-FAIL  RATES )                     *   
R19300*           ( 4 = URBAN-QRP-FAIL  RATES )                     *   
      ***************************************************************   
       01  REGION-TABLE-Q.                                              
           02  REGION-DATA-Q.                                           
      **************YEARCHANGE 2018.0 *******************************   
      *  RATE COMPONENT TABLE FOR FY2018 EFFECTIVE OCT 1, 2017          
      *  20171001/               REGION/ INP / GEN SER / THER / REHAB   
           05  FILLER    PIC X(08) VALUE '20171001'.                    
           05  FILLER    PIC X(25) VALUE '1 16929 09211 01878 15392'.   
           05  FILLER    PIC X(25) VALUE '2 17721 09044 01758 13348'.   
           05  FILLER    PIC X(25) VALUE '3 16594 09029 01841 15087'.   
           05  FILLER    PIC X(25) VALUE '4 17370 08865 01723 13084'.   
      **************YEARCHANGE 2018.0 *******************************   
      **************YEARCHANGE 2019.0 *******************************   
      *  RATE COMPONENT TABLE FOR FY2019 EFFECTIVE OCT 1, 2018          
      *  20181001/               REGION/ INP / GEN SER / THER /REHAB    
           05  FILLER    PIC X(08) VALUE '20181001'.                    
           05  FILLER    PIC X(25) VALUE '1 17334 09431 01923 15760'.   
           05  FILLER    PIC X(25) VALUE '2 18144 09260 01800 13667'.   
           05  FILLER    PIC X(25) VALUE '3 16995 09247 01885 15452'.   
           05  FILLER    PIC X(25) VALUE '4 17790 09079 01765 13400'.   
      **************YEARCHANGE 2019.0 ******************************    
           02  REGION-DATA-Q2         REDEFINES REGION-DATA-Q.          
               04  REG-PERIOD-Q          OCCURS 2.                      
      **************YEARCHANGE 2019.0 *******************************   
                   06  REG-EFF-DATE-Q           PIC X(08).              
                   06  REGS-Q                OCCURS 4.                  
                       08  REG-DATA-Q        OCCURS 1.                  
                           10  REGION-Q         PIC X.                  
                           10  FILLER           PIC X.                  
                           10  R-IP-RATE-Q      PIC 999V99.             
                           10  FILLER           PIC X.                  
                           10  R-GS-RATE-Q      PIC 999V99.             
                           10  FILLER           PIC X.                  
                           10  R-TH-RATE-Q      PIC 999V99.             
                           10  FILLER           PIC X.                  
                           10  R-REHAB-RATE-Q   PIC 999V99.             
                                                                        
      **************YEARCHANGE 2019.0 *******************************   
      **************YEARCHANGE 2018.0 *******************************   
                                                                        
      **************YEARCHANGE 2018.0 *******************************   
      ***************************************************************   
      *     RATE COMPONENT TABLE AREA OF RUG 67 CODES FOR FY2018    *   
      *     EFFECTIVE OCT  1, 2017                                  *   
      *     FORMAT FOR RUG TABLE 67                                 *   
      *     N = NURSING                                             *   
      *     T = THERAPY                                             *   
      *     F = THERAPY COMPONENT FACTOR                            *   
      ***************************************************************   
       01  RUG67-2018-TABLE-Q.                                          
           02  RUG67-2018-DATA1-Q.                                      
      *                                      RUG  REV                   
      *                                      CODE CODE N - T - F        
               03  FILLER  PIC X(19)  VALUE 'AAA  9000 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA1  9011 064 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA2  9012 070 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB1  9013 090 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB2  9014 097 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA1  9019 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA2  9020 088 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB1  9021 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB2  9022 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC1  9023 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC2  9024 129 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD1  9024 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD2  9024 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE1  9024 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE2  9028 168 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES1  9028 232 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES2  9029 267 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES3  9030 358 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB1  9015 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB2  9016 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC1  9017 148 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC2  9018 189 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD1  9018 160 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD2  9018 204 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE1  9018 174 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE2  9018 222 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB1  9018 114 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB2  9018 145 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC1  9018 122 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC2  9018 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD1  9018 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD2  9018 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE1  9018 154 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE2  9018 196 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA1  9001 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA2  9002 059 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB1  9003 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB2  9004 084 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC1  9005 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC2  9006 110 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD1  9007 128 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD2  9008 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE1  9009 140 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE2  9010 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'RHA  9036 091 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHB  9037 119 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHC  9038 145 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHL  9048 215 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHX  9049 255 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLA  9031 071 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLB  9032 150 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLX  9045 226 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMA  9033 084 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMB  9034 122 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMC  9035 136 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RML  9046 219 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMX  9047 247 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUA  9042 099 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUB  9043 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUC  9044 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUL  9052 257 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUX  9053 267 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVA  9039 110 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVB  9040 111 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVC  9041 151 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVL  9050 219 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVX  9051 261 128 0'.      
                                                                        
           02  RUG67-2018-DATA2-Q                                       
                   REDEFINES RUG67-2018-DATA1-Q                         
                      OCCURS 67                                         
                         ASCENDING KEY IS RUG67-2018-CODE-Q             
                            INDEXED BY RUG67-2018-01-Q.                 
                                                                        
               03  RUG67-2018-CODE-Q        PIC X(03).                  
               03  RUG67-2018-REGS-Q        OCCURS 1.                   
      **************YEARCHANGE 2018.0 *************************         
                   04  FILLER               PIC XX.                     
                   04  REV67-2018-CODE-Q    PIC XXXX.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2018-NURSING-Q PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2018-THERAPY-Q PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2018-FACTOR-Q  PIC 9.                      
                                                                        
      *-------------------------------------------------------*         
      *****>>  END OF RATE COMPONENT TABLE FOR FY2018  <<******         
      *-------------------------------------------------------*         
                                                                        
      *-------------------------------------------------------*         
      ***>>  START OF RATE COMPONENT TABLE FOR FY2019 <<******          
      *-------------------------------------------------------*         
                                                                        
                                                                        
      **************YEARCHANGE 2019.0 *******************************   
      ***************************************************************   
      *     RATE COMPONENT TABLE AREA OF RUG 67 CODES FOR FY2019.0  *   
      *     EFFECTIVE OCT  1, 2018                                  *   
      *     FORMAT FOR RUG TABLE 67                                 *   
      *     N = NURSING                                             *   
      *     T = THERAPY                                             *   
      *     F = THERAPY COMPONENT FACTOR                            *   
      ***************************************************************   
       01  RUG67-2019-TABLE-Q.                                          
           02  RUG67-2019-DATA1-Q.                                      
      *                                      RUG  REV                   
      *                                      CODE CODE N - T - F        
               03  FILLER  PIC X(19)  VALUE 'AAA  9000 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA1  9011 064 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BA2  9012 070 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB1  9013 090 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'BB2  9014 097 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA1  9019 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CA2  9020 088 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB1  9021 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CB2  9022 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC1  9023 115 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CC2  9024 129 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD1  9024 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CD2  9024 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE1  9024 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'CE2  9028 168 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES1  9028 232 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES2  9029 267 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'ES3  9030 358 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB1  9015 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HB2  9016 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC1  9017 148 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HC2  9018 189 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD1  9018 160 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HD2  9018 204 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE1  9018 174 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'HE2  9018 222 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB1  9018 114 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LB2  9018 145 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC1  9018 122 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LC2  9018 156 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD1  9018 146 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LD2  9018 186 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE1  9018 154 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'LE2  9018 196 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA1  9001 054 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PA2  9002 059 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB1  9003 078 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PB2  9004 084 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC1  9005 102 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PC2  9006 110 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD1  9007 128 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PD2  9008 138 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE1  9009 140 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'PE2  9010 150 000 1'.      
               03  FILLER  PIC X(19)  VALUE 'RHA  9036 091 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHB  9037 119 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHC  9038 145 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHL  9048 215 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RHX  9049 255 085 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLA  9031 071 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLB  9032 150 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RLX  9045 226 028 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMA  9033 084 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMB  9034 122 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMC  9035 136 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RML  9046 219 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RMX  9047 247 055 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUA  9042 099 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUB  9043 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUC  9044 156 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUL  9052 257 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RUX  9053 267 187 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVA  9039 110 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVB  9040 111 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVC  9041 151 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVL  9050 219 128 0'.      
               03  FILLER  PIC X(19)  VALUE 'RVX  9051 261 128 0'.      
                                                                        
           02  RUG67-2019-DATA2-Q                                       
                   REDEFINES RUG67-2019-DATA1-Q                         
                      OCCURS 67                                         
                         ASCENDING KEY IS RUG67-2019-CODE-Q             
                            INDEXED BY RUG67-2019-01-Q.                 
                                                                        
               03  RUG67-2019-CODE-Q       PIC X(03).                   
               03  RUG67-2019-REGS-Q       OCCURS 1.                    
      **************YEARCHANGE 2019.0 *************************         
                   04  FILLER               PIC XX.                     
                   04  REV67-2019-CODE-Q    PIC XXXX.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2019-NURSING-Q PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2019-THERAPY-Q PIC 9V99.                   
                   04  FILLER               PIC X.                      
                   04  RUG67-2019-FACTOR-Q  PIC 9.                      
                                                                        
      *-------------------------------------------------------*         
      ****>>  END OF RATE COMPONENT TABLE FOR FY2019.0 <<******         
      *-------------------------------------------------------*         
                                                                        
      *-------------------------------------------------------*         
      *------------>>  END OF WORKING STORGE  <<--------------*         
      *-----------------------------------------  ------------*         
                                                                        
       LINKAGE SECTION.                                                 
      ***************************************************************   
      *                 * * * * * * * * *                           *   
      ***************************************************************   
      ***************************************************************   
      *    THIS DATA IS CALCULATED BY THIS SNFPR  SUBROUTINE        *   
      *    AND PASSED BACK TO THE CALLING PROGRAM                   *   
      *            RETURN CODE VALUES (SNF-RTC)                     *   
      *                                                             *   
      *            SNF-RTC                                          *   
      *              00 = RUG III GROUP RATE RETURNED               *   
      *                                                             *   
      *            SNF-RTC   NO RATE RETURNED                       *   
      *                                                             *   
      *              20 = INVALID RUG CODE                          *   
      *                                                             *   
      *              30 = INVALID MSA OR CBSA OR WAGE INDEX         *   
      *                                                             *   
      *              40 = THRU DATE < JULY 1 1998 OR INVALID        *   
      *                                                             *   
      *              50 = INVALID FEDERAL BLEND FOR THAT YEAR       *   
      *                                                             *   
      *              60 = INVALID FEDERAL BLEND                     *   
      *                                                             *   
      *              61 = FEDERAL BLEND = 0 AND                     *   
      *                   SNF THRU DATE < JAN 1, 2000               *   
      *                                                             *   
      *              70 = VBP-MULTIPLIER = ZEROES                   *   
      ***************************************************************   
      *     IP-RATE, GS-RATE, MDS-RATE, THERAPY, REHAB              *   
      ***************************************************************   
       01  SNF-DATA.                                                    
           10  SNF-MSA               PIC XXXX.                          
           10  SNF-CBSA              PIC XXXXX.                         
           10  SNF-SPEC-WI-IND       PIC X.                             
           10  SNF-SPEC-WI           PIC 9(02)V9(04).                   
           10  SNF-RUG-CODE.                                            
               15  SNF-RUG           PIC XXX.                           
               15  FILLER            PIC XX.                            
                                                                        
           10  SNF-FROM-DATE.                                           
               15  SNF-FROM-CC        PIC XX.                           
               15  SNF-FROM-YY        PIC XX.                           
               15  SNF-FROM-MM        PIC XX.                           
               15  SNF-FROM-DD        PIC XX.                           
                                                                        
           10  SNF-THRU-DATE.                                           
               15  SNF-THRU-CC       PIC XX.                            
               15  SNF-THRU-YY       PIC XX.                            
               15  SNF-THRU-MM       PIC XX.                            
               15  SNF-THRU-DD       PIC XX.                            
                                                                        
           10  SNF-FED-BLEND         PIC X.                             
               88  SNF-ALL-FACILITY      VALUE '0'.                     
               88  SNF-FED-BLEND-2575PER VALUE '1'.                     
               88  SNF-FED-BLEND-5050PER VALUE '2'.                     
               88  SNF-FED-BLEND-7525PER VALUE '3'.                     
               88  SNF-FED-BLEND-100PER  VALUE '4'.                     
           10  SNF-FACILITY-RATE     PIC 9(05)V99.                      
           10  SNF-DIAGNOSIS-CODES.                                     
               15  SNF-PRIN-DIAG-CODE    PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE2  PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE3  PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE4  PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE5  PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE6  PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE7  PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE8  PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE9  PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE10 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE11 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE12 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE13 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE14 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE15 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE16 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE17 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE18 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE19 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE20 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE21 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE22 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE23 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE24 PIC X(07).                     
               15  SNF-OTHER-DIAG-CODE25 PIC X(07).                     
           10  SNF-RATE-RTC.                                            
               15  SNF-PAYMENT-RATE      PIC 9(06)V99.                  
               15  SNF-RTC               PIC XX.                        
           10  VBP-DATA.                                                
               15  VBP-MULTIPLIER        PIC S9V9(11).                  
               15  VBP-PAY-DIFF          PIC S9(06)V9(02).              
           10  FILLER                      PIC X(50).                   
                                                                        
                                                                        
                                                                        
       01  HOLD-VARIABLES.                                              
           02  HOLD-VAR-DATA.                                           
               05  FACTOR                         PIC 9.                
               05  NUR-INDEX                      PIC 9V99.             
               05  THR-INDEX                      PIC 9V99.             
               05  AREA-WAGE-INDEX                PIC 9(01)V9(04).      
               05  IP-RATE                        PIC 9(03)V9(02).      
               05  GS-RATE                        PIC 9(02)V9(02).      
               05  TH-RATE                        PIC 9(02)V9(02).      
               05  REHAB-RATE                     PIC 9(03)V9(02).      
               05  NURSING-COMPONENT              PIC 999V99.           
               05  THERAPY-COMPONENT              PIC 999V99.           
               05  NCM-THR-COMPONENT              PIC 999V99.           
               05  NCM-COMPONENT                  PIC 999V99.           
               05  PAYMENT-RATE-ADJ               PIC 9(06)V99.         
               05  FED-PAYMENT-PORTION            PIC 9(06)V99.         
           02  SNFPR-VERSION                      PIC X(09).            
                                                                        
       01  MSA-WAGE-INDEX-RECORD.                                       
           02  HOLD-MSA-WIR-DATA.                                       
               05  MSA-WIR-MSA             PIC X(04).                   
               05  MSA-WIR-EFFDATE         PIC X(08).                   
               05  MSA-WIR-AREA-WAGEIND    PIC 9(02)V9(04).             
                                                                        
       01  CBSA-WAGE-INDEX-RECORD.                                      
           02  HOLD-CBSA-WIR-DATA.                                      
               05  CBSA-WIR-CBSA            PIC X(05).                  
               05  CBSA-WIR-EFFDATE         PIC X(08).                  
               05  CBSA-WIR-AREA-WAGEIND    PIC 9(02)V9(04).            
                                                                        
       PROCEDURE DIVISION            USING SNF-DATA                     
                                           HOLD-VARIABLES               
                                           CBSA-WAGE-INDEX-RECORD       
                                           MSA-WAGE-INDEX-RECORD.       
                                                                        
      ***************************************************************   
      *    PROCESSING:                                              *   
      *        -  WILL PROCESS ALL NATIONAL SNF FOR 1998 THRU 2009  *   
      *                STARTING JULY 1, 1998                        *   
      ***************************************************************   
                                                                        
           PERFORM 0200-MAINLINE-CONTROL.                               
                                                                        
           MOVE HOLD-SNF-DATA        TO SNF-DATA.                       
                                                                        
           GOBACK.                                                      
                                                                        
       0200-MAINLINE-CONTROL.                                           
                                                                        
           MOVE SNF-DATA             TO HOLD-SNF-DATA.                  
                                                                        
           MOVE ALL '0'              TO HLD-RATE-RTC                    
                                        HOLD-VAR-DATA                   
                                        WK-PAYMENT-RATE                 
                                        WK-FACILITY-RATE.               
                                                                        
           MOVE CAL-VERSION          TO SNFPR-VERSION.                  
                                                                        
           IF  HLD-RTC = 00                                             
               PERFORM 400-CALC-THE-SNF                                 
                  THRU 400-EXIT.                                        
                                                                        
       400-CALC-THE-SNF.                                                
                                                                        
           MOVE HLD-FROM-DATE        TO HLD-PRICE-FROM-DATE             
                                                                        
           MOVE HLD-THRU-DATE        TO HLD-PRICE-THRU-DATE             
                                                                        
           IF HLD-PRICE-THRU-MMDD = '1001' AND                          
              HLD-PRICE-FROM-DATE < HLD-PRICE-THRU-DATE                 
              MOVE '0930'            TO HLD-PRICE-THRU-MMDD.            
                                                                        
           IF HLD-THRU-DATE < 19980701                                  
               MOVE '40'             TO HLD-RTC.                        
                                                                        
           IF HLD-RTC NOT = 00                                          
                  GO TO 400-EXIT.                                       
                                                                        
      **************YEARCHANGE 2019.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20180930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2019-67-PAYMENT-RATE-CALC                         
                 THRU 2019-67-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2019.0 *******************************   
                                                                        
      **************YEARCHANGE 2018.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20170930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2018-67-PAYMENT-RATE-CALC                         
                 THRU 2018-67-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2018.0 *******************************   
      **************YEARCHANGE 2017.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20160930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2017-67-PAYMENT-RATE-CALC                         
                 THRU 2017-67-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2017.0 *******************************   
      **************YEARCHANGE 2016.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20150930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2016-67-PAYMENT-RATE-CALC                         
                 THRU 2016-67-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2016.0 *******************************   
      **************YEARCHANGE 2015.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20140930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2015-67-PAYMENT-RATE-CALC                         
                 THRU 2015-67-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2015.0 *******************************   
      **************YEARCHANGE 2014.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20130930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2014-67-PAYMENT-RATE-CALC                         
                 THRU 2014-67-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2014.0 *******************************   
      **************YEARCHANGE 2013.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20120930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2013-67-PAYMENT-RATE-CALC                         
                 THRU 2013-67-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2013.0 *******************************   
      **************YEARCHANGE 2012.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20110930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2012-67-PAYMENT-RATE-CALC                         
                 THRU 2012-67-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2012.0 *******************************   
      **************YEARCHANGE 2011.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20100930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2011-67-PAYMENT-RATE-CALC                         
                 THRU 2011-67-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2011.0 *******************************   
      **************YEARCHANGE 2010.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20090930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2010-53-PAYMENT-RATE-CALC                         
                 THRU 2010-53-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2010.0 *******************************   
      **************YEARCHANGE 2009.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20080930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2009-53-PAYMENT-RATE-CALC                         
                 THRU 2009-53-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2009.0 *******************************   
      **************YEARCHANGE 2008.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20070930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2008-53-PAYMENT-RATE-CALC                         
                 THRU 2008-53-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2008.0 *******************************   
      **************YEARCHANGE 2007.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20060930                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2007-53-PAYMENT-RATE-CALC THRU 2007-53-EXIT       
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2007.0 *******************************   
      **************YEARCHANGE 2006.1 *******************************   
           IF HLD-PRICE-THRU-DATE > 20051231                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2006-53-PAYMENT-RATE-CALC                         
                 THRU 2006-53-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2006.1 *******************************   
      **************YEARCHANGE 2006.0 *******************************   
           IF HLD-PRICE-THRU-DATE > 20050930 AND                        
              HLD-PRICE-THRU-DATE < 20060101                            
              MOVE CBSA-WIR-AREA-WAGEIND                                
                                     TO AREA-WAGE-INDEX                 
              PERFORM 2006-44-PAYMENT-RATE-CALC                         
                 THRU 2006-44-EXIT                                      
              GO TO 400-EXIT.                                           
      **************YEARCHANGE 2006.0 *******************************   
                                                                        
           MOVE MSA-WIR-AREA-WAGEIND TO AREA-WAGE-INDEX.                
                                                                        
           IF HLD-PRICE-THRU-DATE > 20040930                            
              PERFORM 2005-PAYMENT-RATE-CALC                            
                 THRU 2005-EXIT                                         
              GO TO 400-EXIT.                                           
                                                                        
           IF HLD-PRICE-THRU-DATE > 20030930                            
              PERFORM 2004-PAYMENT-RATE-CALC                            
                 THRU 2004-EXIT                                         
                                                                        
              GO TO 400-EXIT.                                           
                                                                        
           IF HLD-PRICE-THRU-DATE > 20020930                            
              PERFORM 2003-PAYMENT-RATE-CALC                            
                 THRU 2003-EXIT                                         
              GO TO 400-EXIT.                                           
                                                                        
           IF HLD-PRICE-THRU-DATE > 20010930                            
              PERFORM 2002-PAYMENT-RATE-CALC                            
                 THRU 2002-EXIT                                         
              GO TO 400-EXIT.                                           
                                                                        
           IF HLD-PRICE-THRU-DATE > 20010331                            
              PERFORM 2001A-PAYMENT-RATE-CALC                           
                 THRU 2001A-EXIT                                        
              GO TO 400-EXIT.                                           
                                                                        
           IF HLD-PRICE-THRU-DATE > 20000930                            
              PERFORM 2001-PAYMENT-RATE-CALC                            
                 THRU 2001-EXIT                                         
              GO TO 400-EXIT.                                           
                                                                        
           IF HLD-PRICE-THRU-DATE > 19990930                            
              PERFORM 2000-PAYMENT-RATE-CALC                            
                 THRU 2000-EXIT                                         
              GO TO 400-EXIT.                                           
                                                                        
           IF HLD-PRICE-THRU-DATE > 19980630                            
              PERFORM 1999-PAYMENT-RATE-CALC                            
                 THRU 1999-EXIT                                         
              GO TO 400-EXIT.                                           
                                                                        
                                                                        
       400-EXIT.                                                        
           EXIT.                                                        
                                                                        
       500-GET-RATES.                                                   
                                                                        
      *-------------------------------------------------------*         
      *---------->> RATE SEARCH FOR FY2018 & FUTURE <<--------*         
      *--------->> NOW 4 REGIONS DUE TO QRP PENALTY <<--------*         
      *-------------------------------------------------------*         
                                                                        
           IF HLD-PRICE-FROM-DATE > 20170930                            
                                                                        
              PERFORM 650-WHICH-RATES-NOW-FUTURE                        
                      VARYING R1                                        
                          FROM 1 BY 1                                   
                             UNTIL R1 > 2                               
                                                                        
           ELSE                                                         
      *-------------------------------------------------------*         
      *------->>  RATE SEARCH FOR FY1998 THRU FY2017  <<------*         
      *--------->>       ONLY 2 REGIONS             <<--------*         
      *-------------------------------------------------------*         
               PERFORM 600-WHICH-RATES                                  
                       VARYING R1                                       
                           FROM 1 BY 1                                  
                              UNTIL R1 > 20                             
           END-IF.                                                      
                                                                        
                                                                        
       500-EXIT.    EXIT.                                               
                                                                        
                                                                        
       600-WHICH-RATES.                                                 
                                                                        
           IF HLD-PRICE-THRU-DATE NOT < REG-EFF-DATE (R1)               
                                                                        
             MOVE R-IP-RATE (R1, R2, R3)                                
                                     TO IP-RATE                         
             MOVE R-GS-RATE (R1, R2, R3)                                
                                     TO GS-RATE,                        
                                        NCM-COMPONENT                   
             MOVE R-TH-RATE (R1, R2, R3)                                
                                     TO TH-RATE,                        
                                        NCM-THR-COMPONENT               
             MOVE R-REHAB-RATE (R1, R2, R3)                             
                                     TO REHAB-RATE                      
           END-IF.                                                      
                                                                        
                                                                        
       650-WHICH-RATES-NOW-FUTURE.                                      
                                                                        
           IF HLD-PRICE-THRU-DATE NOT < REG-EFF-DATE-Q (R1)             
                                                                        
             MOVE R-IP-RATE-Q (R1, R2, R3)                              
                                     TO IP-RATE                         
             MOVE R-GS-RATE-Q (R1, R2, R3)                              
                                     TO GS-RATE                         
                                        NCM-COMPONENT                   
             MOVE R-TH-RATE-Q (R1, R2, R3)                              
                                     TO TH-RATE                         
                                        NCM-THR-COMPONENT               
             MOVE R-REHAB-RATE-Q (R1, R2, R3)                           
                                     TO REHAB-RATE                      
           END-IF.                                                      
                                                                        
                                                                        
                                                                        
       800-SEARCH-4-RUG.                                                
           SET RUG01                 TO 1.                              
           SEARCH RUG-DATA2 VARYING RUG01                               
             AT END                                                     
                 MOVE '20'           TO HLD-RTC                         
                 GO TO 800-SEARCH-EXIT                                  
             WHEN RUG-CODE (RUG01) = HLD-RUG                            
                                                                        
                 MOVE RUG-NURSING (RUG01, R3)                           
                                     TO NUR-INDEX                       
                 MOVE RUG-THERAPY (RUG01, R3)                           
                                     TO THR-INDEX                       
                 MOVE RUG-FACTOR  (RUG01, R3)                           
                                     TO FACTOR.                         
                                                                        
       800-SEARCH-EXIT.   EXIT.                                         
                                                                        
       900-SEARCH-4-RUG53.                                              
           SET RUG53-01              TO 1.                              
           SEARCH RUG53-DATA2 VARYING RUG53-01                          
             AT END                                                     
                 MOVE '20'           TO HLD-RTC                         
                 GO TO 900-SEARCH-EXIT                                  
                                                                        
             WHEN RUG53-CODE (RUG53-01) = HLD-RUG                       
                 MOVE RUG53-NURSING (RUG53-01, R3)                      
                                     TO NUR-INDEX                       
                 MOVE RUG53-THERAPY (RUG53-01, R3)                      
                                     TO THR-INDEX                       
                 MOVE RUG53-FACTOR  (RUG53-01, R3)                      
                                     TO FACTOR.                         
                                                                        
       900-SEARCH-EXIT.   EXIT.                                         
                                                                        
                                                                        
       910-SEARCH-4-RUG53-2010.                                         
                                                                        
           SET RUG53-2010-01         TO 1.                              
                                                                        
           SEARCH RUG53-2010-DATA2   VARYING RUG53-2010-01              
             AT END                                                     
                 MOVE '20'           TO HLD-RTC                         
                 GO TO 910-SEARCH-EXIT                                  
                                                                        
             WHEN RUG53-2010-CODE (RUG53-2010-01) = HLD-RUG             
                 MOVE RUG53-2010-NURSING (RUG53-2010-01, R3)            
                                     TO NUR-INDEX                       
                 MOVE RUG53-2010-THERAPY (RUG53-2010-01, R3)            
                                     TO THR-INDEX                       
                 MOVE RUG53-2010-FACTOR (RUG53-2010-01, R3)             
                                     TO FACTOR.                         
                                                                        
       910-SEARCH-EXIT.   EXIT.                                         
                                                                        
       911-SEARCH-4-RUG67-2011.                                         
                                                                        
           SET RUG67-2011-01         TO 1.                              
           SEARCH RUG67-2011-DATA2   VARYING RUG67-2011-01              
             AT END                                                     
                 MOVE '20'           TO HLD-RTC                         
                 GO TO 911-SEARCH-EXIT                                  
                                                                        
             WHEN RUG67-2011-CODE (RUG67-2011-01) = HLD-RUG             
                 MOVE RUG67-2011-NURSING (RUG67-2011-01, R3)            
                                     TO NUR-INDEX                       
                 MOVE RUG67-2011-THERAPY (RUG67-2011-01, R3)            
                                     TO THR-INDEX                       
                 MOVE RUG67-2011-FACTOR (RUG67-2011-01, R3)             
                                     TO FACTOR.                         
                                                                        
       911-SEARCH-EXIT.   EXIT.                                         
                                                                        
                                                                        
       912-SEARCH-4-RUG67-2012.                                         
           SET RUG67-2012-01         TO 1.                              
           SEARCH RUG67-2012-DATA2 VARYING RUG67-2012-01                
             AT END                                                     
                 MOVE '20'           TO HLD-RTC                         
                 GO TO 912-SEARCH-EXIT                                  
             WHEN RUG67-2012-CODE (RUG67-2012-01) = HLD-RUG             
                 MOVE RUG67-2012-NURSING (RUG67-2012-01, R3)            
                                     TO NUR-INDEX                       
                 MOVE RUG67-2012-THERAPY (RUG67-2012-01, R3)            
                                     TO THR-INDEX                       
                 MOVE RUG67-2012-FACTOR (RUG67-2012-01, R3)             
                                     TO FACTOR.                         
                                                                        
       912-SEARCH-EXIT.   EXIT.                                         
                                                                        
                                                                        
       913-SEARCH-4-RUG67-2013.                                         
           SET RUG67-2013-01         TO 1.                              
                                                                        
           SEARCH  RUG67-2013-DATA2                                     
             VARYING RUG67-2013-01                                      
             AT END                                                     
                 MOVE '20'           TO HLD-RTC                         
                 GO TO 913-SEARCH-EXIT                                  
                                                                        
             WHEN RUG67-2013-CODE (RUG67-2013-01) = HLD-RUG             
                                                                        
                 MOVE RUG67-2013-NURSING (RUG67-2013-01, R3)            
                                     TO NUR-INDEX                       
                 MOVE RUG67-2013-THERAPY (RUG67-2013-01, R3)            
                                     TO THR-INDEX                       
                 MOVE RUG67-2013-FACTOR (RUG67-2013-01, R3)             
                                     TO FACTOR.                         
                                                                        
       913-SEARCH-EXIT.   EXIT.                                         
                                                                        
                                                                        
       914-SEARCH-4-RUG67-2014.                                         
           SET RUG67-2014-01         TO 1.                              
                                                                        
           SEARCH  RUG67-2014-DATA2                                     
             VARYING RUG67-2014-01                                      
             AT END                                                     
                 MOVE '20'           TO HLD-RTC                         
                 GO TO 914-SEARCH-EXIT                                  
                                                                        
             WHEN RUG67-2014-CODE (RUG67-2014-01) = HLD-RUG             
                                                                        
                 MOVE RUG67-2014-NURSING (RUG67-2014-01, R3)            
                                     TO NUR-INDEX                       
                 MOVE RUG67-2014-THERAPY (RUG67-2014-01, R3)            
                                     TO THR-INDEX                       
                 MOVE RUG67-2014-FACTOR (RUG67-2014-01, R3)             
                                     TO FACTOR.                         
                                                                        
       914-SEARCH-EXIT.   EXIT.                                         
                                                                        
                                                                        
                                                                        
       915-SEARCH-4-RUG67-2015.                                         
           SET RUG67-2015-01         TO 1.                              
                                                                        
           SEARCH  RUG67-2015-DATA2                                     
             VARYING RUG67-2015-01                                      
             AT END                                                     
                 MOVE '20'           TO HLD-RTC                         
                 GO TO 915-SEARCH-EXIT                                  
                                                                        
             WHEN RUG67-2015-CODE (RUG67-2015-01) = HLD-RUG             
                                                                        
                 MOVE RUG67-2015-NURSING (RUG67-2015-01, R3)            
                                     TO NUR-INDEX                       
                 MOVE RUG67-2015-THERAPY (RUG67-2015-01, R3)            
                                     TO THR-INDEX                       
                 MOVE RUG67-2015-FACTOR (RUG67-2015-01, R3)             
                                     TO FACTOR.                         
                                                                        
       915-SEARCH-EXIT.   EXIT.                                         
                                                                        
                                                                        
                                                                        
       916-SEARCH-4-RUG67-2016.                                         
           SET RUG67-2016-01         TO 1.                              
                                                                        
           SEARCH  RUG67-2016-DATA2                                     
             VARYING RUG67-2016-01                                      
             AT END                                                     
                 MOVE '20'           TO HLD-RTC                         
                 GO TO 916-SEARCH-EXIT                                  
                                                                        
             WHEN RUG67-2016-CODE (RUG67-2016-01) = HLD-RUG             
                                                                        
                 MOVE RUG67-2016-NURSING (RUG67-2016-01, R3)            
                                     TO NUR-INDEX                       
                 MOVE RUG67-2016-THERAPY (RUG67-2016-01, R3)            
                                     TO THR-INDEX                       
                 MOVE RUG67-2016-FACTOR (RUG67-2016-01, R3)             
                                     TO FACTOR.                         
                                                                        
       916-SEARCH-EXIT.   EXIT.                                         
                                                                        
      *** --------------------  917-SEARCH-4 STARTS  --------------     
                                                                        
       917-SEARCH-4-RUG67-2017.                                         
           SET RUG67-2017-01         TO 1.                              
                                                                        
           SEARCH  RUG67-2017-DATA2                                     
             VARYING RUG67-2017-01                                      
             AT END                                                     
                 MOVE '20'           TO HLD-RTC                         
                 GO TO 917-SEARCH-EXIT                                  
                                                                        
             WHEN RUG67-2017-CODE (RUG67-2017-01) = HLD-RUG             
                                                                        
                 MOVE RUG67-2017-NURSING (RUG67-2017-01, R3)            
                                     TO NUR-INDEX                       
                 MOVE RUG67-2017-THERAPY (RUG67-2017-01, R3)            
                                     TO THR-INDEX                       
                 MOVE RUG67-2017-FACTOR (RUG67-2017-01, R3)             
                                     TO FACTOR.                         
                                                                        
       917-SEARCH-EXIT.   EXIT.                                         
                                                                        
A78600                                                                  
      *** --------------------  917-SEARCH-4 ENDS    --------------     
                                                                        
                                                                        
                                                                        
      *** --------------------  918-SEARCH-4 STARTS  --------------     
                                                                        
       918-SEARCH-4-RUG67-2018.                                         
           SET RUG67-2018-01-Q       TO 1.                              
                                                                        
           SEARCH  RUG67-2018-DATA2-Q                                   
             VARYING RUG67-2018-01-Q                                    
                 AT END                                                 
                    MOVE '20'        TO HLD-RTC                         
             GO TO 918-SEARCH-EXIT                                      
                                                                        
             WHEN RUG67-2018-CODE-Q (RUG67-2018-01-Q) = HLD-RUG         
                                                                        
                  MOVE RUG67-2018-NURSING-Q (RUG67-2018-01-Q, R3)       
                                     TO NUR-INDEX                       
                  MOVE RUG67-2018-THERAPY-Q (RUG67-2018-01-Q, R3)       
                                     TO THR-INDEX                       
                  MOVE RUG67-2018-FACTOR-Q (RUG67-2018-01-Q, R3)        
                                     TO FACTOR.                         
                                                                        
                                                                        
       918-SEARCH-EXIT.   EXIT.                                         
                                                                        
      *** --------------------  918-SEARCH-4 ENDS    --------------     
                                                                        
                                                                        
      *** --------------------  919-SEARCH-4 STARTS  --------------     
                                                                        
       919-SEARCH-4-RUG67-2019.                                         
           SET RUG67-2019-01-Q       TO 1.                              
                                                                        
           SEARCH  RUG67-2019-DATA2-Q                                   
             VARYING RUG67-2019-01-Q                                    
                 AT END                                                 
                    MOVE '20'        TO HLD-RTC                         
             GO TO 919-SEARCH-EXIT                                      
                                                                        
             WHEN RUG67-2019-CODE-Q (RUG67-2019-01-Q) = HLD-RUG         
                                                                        
                  MOVE RUG67-2019-NURSING-Q (RUG67-2019-01-Q, R3)       
                                     TO NUR-INDEX                       
                  MOVE RUG67-2019-THERAPY-Q (RUG67-2019-01-Q, R3)       
                                     TO THR-INDEX                       
                  MOVE RUG67-2019-FACTOR-Q (RUG67-2019-01-Q, R3)        
                                     TO FACTOR.                         
                                                                        
                                                                        
       919-SEARCH-EXIT.   EXIT.                                         
                                                                        
      *** --------------------  919-SEARCH-4 ENDS    --------------     
                                                                        
                                                                        
                                                                        
       1999-PAYMENT-RATE-CALC.                                          
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS                           
      ***************************************************************   
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-MSA-RURAL-CHECK                                       
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 800-SEARCH-4-RUG                                     
              THRU 800-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 1999-EXIT.                                          
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 1 OR 2 OR 3 OR 4                          
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 1999-EXIT.                                          
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-1999-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-1999-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF SNF-FED-BLEND = 1                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .25)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .75)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 2                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .50)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .50)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 3                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .75)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .25)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       1999-EXIT.   EXIT.                                               
                                                                        
       2000-PAYMENT-RATE-CALC.                                          
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS                           
      ***************************************************************   
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-MSA-RURAL-CHECK                                       
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 800-SEARCH-4-RUG    THRU 800-SEARCH-EXIT.            
                                                                        
           IF HLD-RTC NOT = 00 GO TO 2000-EXIT.                         
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF (HLD-THRU-DATE < 20000101 AND                             
               SNF-FED-BLEND = 0)                                       
              MOVE '61' TO HLD-RTC                                      
              MOVE ALL '0' TO HOLD-VAR-DATA                             
              GO TO 2000-EXIT.                                          
                                                                        
           IF SNF-FED-BLEND = 0 OR 1 OR 2 OR 3 OR 4                     
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60' TO HLD-RTC                                      
              MOVE ALL '0' TO HOLD-VAR-DATA                             
              GO TO 2000-EXIT.                                          
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2000-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2000-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF  (HLD-THRU-DATE > 20000331 AND                            
               (HLD-RUG = 'SE3' OR 'SE2' OR 'SE1' OR 'SSC' OR           
                          'SSB' OR 'SSA' OR 'CC2' OR 'CC1' OR           
                          'CB2' OR 'CB1' OR 'CA2' OR 'CA1' OR           
                          'RHC' OR 'RMC' OR 'RMB'))                     
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.2.                           
                                                                        
           IF (HLD-THRU-DATE > 19991231 AND                             
               SNF-FED-BLEND = 0)                                       
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 1                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .25)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .75)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 2                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .50)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .50)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 3                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .75)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .25)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2000-EXIT.   EXIT.                                               
       2001-PAYMENT-RATE-CALC.                                          
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS                           
      ***************************************************************   
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-MSA-RURAL-CHECK                                       
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 800-SEARCH-4-RUG                                     
              THRU 800-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2001-EXIT.                                          
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF (HLD-THRU-DATE < 20000101 AND                             
               SNF-FED-BLEND = 0)                                       
              MOVE '61'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2001-EXIT.                                          
                                                                        
           IF SNF-FED-BLEND = 0 OR 1 OR 2 OR 3 OR 4                     
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2001-EXIT.                                          
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2001-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2001-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF  (HLD-RUG = 'SE3' OR 'SE2' OR 'SE1' OR 'SSC' OR           
                          'SSB' OR 'SSA' OR 'CC2' OR 'CC1' OR           
                          'CB2' OR 'CB1' OR 'CA2' OR 'CA1' OR           
                          'RHC' OR 'RMC' OR 'RMB')                      
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.24                           
           ELSE                                                         
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.04.                          
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 1                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .25)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .75)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 2                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .50)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .50)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 3                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .75)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .25)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
                                                                        
       2001-EXIT.   EXIT.                                               
                                                                        
       2001A-PAYMENT-RATE-CALC.                                         
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE APR.1, 2001     
      ***************************************************************   
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-MSA-RURAL-CHECK                                       
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 800-SEARCH-4-RUG                                     
              THRU 800-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2001A-EXIT.                                         
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF (HLD-THRU-DATE < 20000101 AND                             
               SNF-FED-BLEND = 0)                                       
              MOVE '61'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2001A-EXIT.                                         
                                                                        
           IF SNF-FED-BLEND = 0 OR 1 OR 2 OR 3 OR 4                     
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2001A-EXIT.                                         
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2001-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2001-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF  (HLD-RUG = 'IB2' OR 'IB1' OR 'IA2' OR 'IA1' OR           
                          'BB2' OR 'BB1' OR 'BA2' OR 'BA1' OR           
                          'PE2' OR 'PE1' OR 'PD2' OR 'PD1' OR           
                          'PC2' OR 'PC1' OR 'PB2' OR 'PB1' OR           
                          'PA2' OR 'PA1' OR 'AAA')                      
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.04.                          
                                                                        
           IF  (HLD-RUG = 'RUC' OR 'RUB' OR 'RUA' OR 'RVC' OR           
                          'RVB' OR 'RVA' OR 'RHC' OR 'RHB' OR           
                          'RHA' OR 'RMC' OR 'RMB' OR 'RMA' OR           
                          'RLB' OR 'RLA')                               
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.107.                         
                                                                        
           IF  (HLD-RUG = 'SE3' OR 'SE2' OR 'SE1' OR 'SSC' OR           
                          'SSB' OR 'SSA' OR 'CC2' OR 'CC1' OR           
                          'CB2' OR 'CB1' OR 'CA2' OR 'CA1')             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.24.                          
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 1                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .25)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .75)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 2                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .50)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .50)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 3                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .75)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .25)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
                                                                        
       2001A-EXIT.   EXIT.                                              
                                                                        
       2002-PAYMENT-RATE-CALC.                                          
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2001     
      ***************************************************************   
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-MSA-RURAL-CHECK                                       
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 800-SEARCH-4-RUG                                     
              THRU 800-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2002-EXIT.                                          
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF (HLD-THRU-DATE < 20000101 AND                             
               SNF-FED-BLEND = 0)                                       
              MOVE '61'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2002-EXIT.                                          
                                                                        
           IF SNF-FED-BLEND = 0 OR 1 OR 2 OR 3 OR 4                     
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2002-EXIT.                                          
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2002-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2002-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF  (HLD-RUG = 'IB2' OR 'IB1' OR 'IA2' OR 'IA1' OR           
                          'BB2' OR 'BB1' OR 'BA2' OR 'BA1' OR           
                          'PE2' OR 'PE1' OR 'PD2' OR 'PD1' OR           
                          'PC2' OR 'PC1' OR 'PB2' OR 'PB1' OR           
                          'PA2' OR 'PA1' OR 'AAA')                      
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.04.                          
                                                                        
           IF  (HLD-RUG = 'RUC' OR 'RUB' OR 'RUA' OR 'RVC' OR           
                          'RVB' OR 'RVA' OR 'RHC' OR 'RHB' OR           
                          'RHA' OR 'RMC' OR 'RMB' OR 'RMA' OR           
                          'RLB' OR 'RLA')                               
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.107.                         
                                                                        
           IF  (HLD-RUG = 'SE3' OR 'SE2' OR 'SE1' OR 'SSC' OR           
                          'SSB' OR 'SSA' OR 'CC2' OR 'CC1' OR           
                          'CB2' OR 'CB1' OR 'CA2' OR 'CA1')             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.24.                          
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 1                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .25)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .75)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 2                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .50)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .50)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 3                                         
              COMPUTE WK-PAYMENT-RATE ROUNDED =                         
                 (HLD-PAYMENT-RATE * .75)                               
              COMPUTE WK-FACILITY-RATE ROUNDED =                        
                 (HLD-FACILITY-RATE * .25)                              
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                 (WK-PAYMENT-RATE + WK-FACILITY-RATE).                  
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
                                                                        
       2002-EXIT.   EXIT.                                               
                                                                        
       2003-PAYMENT-RATE-CALC.                                          
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2002     
      ***************************************************************   
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-MSA-RURAL-CHECK                                       
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 800-SEARCH-4-RUG                                     
              THRU 800-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2003-EXIT.                                          
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF (HLD-THRU-DATE < 20000101 AND                             
               SNF-FED-BLEND = 0)                                       
              MOVE '61'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2003-EXIT.                                          
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2003-EXIT.                                          
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2003-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2003-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF  (HLD-RUG = 'IB2' OR 'IB1' OR 'IA2' OR 'IA1' OR           
                          'BB2' OR 'BB1' OR 'BA2' OR 'BA1' OR           
                          'PE2' OR 'PE1' OR 'PD2' OR 'PD1' OR           
                          'PC2' OR 'PC1' OR 'PB2' OR 'PB1' OR           
                          'PA2' OR 'PA1' OR 'AAA')                      
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
           IF  (HLD-RUG = 'RUC' OR 'RUB' OR 'RUA' OR 'RVC' OR           
                          'RVB' OR 'RVA' OR 'RHC' OR 'RHB' OR           
                          'RHA' OR 'RMC' OR 'RMB' OR 'RMA' OR           
                          'RLB' OR 'RLA')                               
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.067.                         
                                                                        
           IF  (HLD-RUG = 'SE3' OR 'SE2' OR 'SE1' OR 'SSC' OR           
                          'SSB' OR 'SSA' OR 'CC2' OR 'CC1' OR           
                          'CB2' OR 'CB1' OR 'CA2' OR 'CA1')             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.20.                          
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
                                                                        
       2003-EXIT.   EXIT.                                               
       2004-PAYMENT-RATE-CALC.                                          
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2003     
      ***************************************************************   
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-MSA-RURAL-CHECK                                       
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 800-SEARCH-4-RUG                                     
              THRU 800-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2004-EXIT.                                          
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF (HLD-THRU-DATE < 20000101 AND                             
               SNF-FED-BLEND = 0)                                       
              MOVE '61'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2004-EXIT.                                          
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2004-EXIT.                                          
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2004-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2004-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF  (HLD-RUG = 'IB2' OR 'IB1' OR 'IA2' OR 'IA1' OR           
                          'BB2' OR 'BB1' OR 'BA2' OR 'BA1' OR           
                          'PE2' OR 'PE1' OR 'PD2' OR 'PD1' OR           
                          'PC2' OR 'PC1' OR 'PB2' OR 'PB1' OR           
                          'PA2' OR 'PA1' OR 'AAA')                      
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
           IF  (HLD-RUG = 'RUC' OR 'RUB' OR 'RUA' OR 'RVC' OR           
                          'RVB' OR 'RVA' OR 'RHC' OR 'RHB' OR           
                          'RHA' OR 'RMC' OR 'RMB' OR 'RMA' OR           
                          'RLB' OR 'RLA')                               
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.067.                         
                                                                        
           IF  (HLD-RUG = 'SE3' OR 'SE2' OR 'SE1' OR 'SSC' OR           
                          'SSB' OR 'SSA' OR 'CC2' OR 'CC1' OR           
                          'CB2' OR 'CB1' OR 'CA2' OR 'CA1')             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.20.                          
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
                                                                        
       2004-EXIT.   EXIT.                                               
                                                                        
       2005-PAYMENT-RATE-CALC.                                          
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2004     
      ***************************************************************   
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-MSA-RURAL-CHECK                                       
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 800-SEARCH-4-RUG                                     
              THRU 800-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2005-EXIT.                                          
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF (HLD-THRU-DATE < 20000101 AND                             
               SNF-FED-BLEND = 0)                                       
              MOVE '61'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2005-EXIT.                                          
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2005-EXIT.                                          
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2005-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2005-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9 OR                        
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2005-CONTINUE.                                     
                                                                        
           IF  (HLD-RUG = 'IB2' OR 'IB1' OR 'IA2' OR 'IA1' OR           
                          'BB2' OR 'BB1' OR 'BA2' OR 'BA1' OR           
                          'PE2' OR 'PE1' OR 'PD2' OR 'PD1' OR           
                          'PC2' OR 'PC1' OR 'PB2' OR 'PB1' OR           
                          'PA2' OR 'PA1' OR 'AAA')                      
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
           IF  (HLD-RUG = 'RUC' OR 'RUB' OR 'RUA' OR 'RVC' OR           
                          'RVB' OR 'RVA' OR 'RHC' OR 'RHB' OR           
                          'RHA' OR 'RMC' OR 'RMB' OR 'RMA' OR           
                          'RLB' OR 'RLA')                               
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.067.                         
                                                                        
           IF  (HLD-RUG = 'SE3' OR 'SE2' OR 'SE1' OR 'SSC' OR           
                          'SSB' OR 'SSA' OR 'CC2' OR 'CC1' OR           
                          'CB2' OR 'CB1' OR 'CA2' OR 'CA1')             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.20.                          
                                                                        
       2005-CONTINUE.                                                   
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2005-EXIT.   EXIT.                                               
                                                                        
       2006-44-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS                           
      *      EFFECTIVE OCT.1, 2005 TIL JAN.1, 2006                      
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30'              TO HLD-RTC                         
              GO TO 2006-44-EXIT.                                       
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 800-SEARCH-4-RUG                                     
              THRU 800-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2006-44-EXIT.                                       
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2006-44-EXIT.                                       
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2006-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2006-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE    OR                       
                         SNF-OTHER-DIAG-CODE2  OR                       
                         SNF-OTHER-DIAG-CODE3  OR                       
                         SNF-OTHER-DIAG-CODE4  OR                       
                         SNF-OTHER-DIAG-CODE5  OR                       
                         SNF-OTHER-DIAG-CODE6  OR                       
                         SNF-OTHER-DIAG-CODE7  OR                       
                         SNF-OTHER-DIAG-CODE8  OR                       
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2006-CONTINUE.                                     
                                                                        
           IF  (HLD-RUG = 'IB2' OR 'IB1' OR 'IA2' OR 'IA1' OR           
                          'BB2' OR 'BB1' OR 'BA2' OR 'BA1' OR           
                          'PE2' OR 'PE1' OR 'PD2' OR 'PD1' OR           
                          'PC2' OR 'PC1' OR 'PB2' OR 'PB1' OR           
                          'PA2' OR 'PA1' OR 'AAA')                      
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
           IF  (HLD-RUG = 'RUC' OR 'RUB' OR 'RUA' OR 'RVC' OR           
                          'RVB' OR 'RVA' OR 'RHC' OR 'RHB' OR           
                          'RHA' OR 'RMC' OR 'RMB' OR 'RMA' OR           
                          'RLB' OR 'RLA')                               
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.067.                         
                                                                        
           IF  (HLD-RUG = 'SE3' OR 'SE2' OR 'SE1' OR 'SSC' OR           
                          'SSB' OR 'SSA' OR 'CC2' OR 'CC1' OR           
                          'CB2' OR 'CB1' OR 'CA2' OR 'CA1')             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.20.                          
                                                                        
       2006-CONTINUE.                                                   
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
                                                                        
       2006-44-EXIT.   EXIT.                                            
                                                                        
       2006-53-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE JAN.1, 2006     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30'              TO HLD-RTC                         
              GO TO 2006-53-EXIT.                                       
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 900-SEARCH-4-RUG53                                   
              THRU 900-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2006-53-EXIT.                                       
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2006-53-EXIT.                                       
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2006-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2006-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE    OR                       
                         SNF-OTHER-DIAG-CODE2  OR                       
                         SNF-OTHER-DIAG-CODE3  OR                       
                         SNF-OTHER-DIAG-CODE4  OR                       
                         SNF-OTHER-DIAG-CODE5  OR                       
                         SNF-OTHER-DIAG-CODE6  OR                       
                         SNF-OTHER-DIAG-CODE7  OR                       
                         SNF-OTHER-DIAG-CODE8  OR                       
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2006-53-CONTINUE.                                  
                                                                        
       2006-53-CONTINUE.                                                
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2006-53-EXIT.   EXIT.                                            
                                                                        
       2007-53-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2007     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30' TO HLD-RTC                                      
              GO TO 2007-53-EXIT.                                       
                                                                        
           MOVE 1 TO R3.                                                
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1 TO R2                                              
           ELSE                                                         
              MOVE 2 TO R2.                                             
                                                                        
           PERFORM 900-SEARCH-4-RUG53  THRU 900-SEARCH-EXIT.            
                                                                        
           IF HLD-RTC NOT = 00 GO TO 2007-53-EXIT.                      
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60' TO HLD-RTC                                      
              MOVE ALL '0' TO HOLD-VAR-DATA                             
              GO TO 2007-53-EXIT.                                       
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2007-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2007-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE    OR                       
                         SNF-OTHER-DIAG-CODE2  OR                       
                         SNF-OTHER-DIAG-CODE3  OR                       
                         SNF-OTHER-DIAG-CODE4  OR                       
                         SNF-OTHER-DIAG-CODE5  OR                       
                         SNF-OTHER-DIAG-CODE6  OR                       
                         SNF-OTHER-DIAG-CODE7  OR                       
                         SNF-OTHER-DIAG-CODE8  OR                       
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2007-53-CONTINUE.                                  
                                                                        
       2007-53-CONTINUE.                                                
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2007-53-EXIT. EXIT.                                              
                                                                        
       2008-53-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2007     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30' TO HLD-RTC                                      
              GO TO 2008-53-EXIT.                                       
                                                                        
           MOVE 1 TO R3.                                                
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1 TO R2                                              
           ELSE                                                         
              MOVE 2 TO R2.                                             
                                                                        
           PERFORM 900-SEARCH-4-RUG53  THRU 900-SEARCH-EXIT.            
                                                                        
           IF HLD-RTC NOT = 00 GO TO 2008-53-EXIT.                      
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60' TO HLD-RTC                                      
              MOVE ALL '0' TO HOLD-VAR-DATA                             
              GO TO 2008-53-EXIT.                                       
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2008-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2008-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE    OR                       
                         SNF-OTHER-DIAG-CODE2  OR                       
                         SNF-OTHER-DIAG-CODE3  OR                       
                         SNF-OTHER-DIAG-CODE4  OR                       
                         SNF-OTHER-DIAG-CODE5  OR                       
                         SNF-OTHER-DIAG-CODE6  OR                       
                         SNF-OTHER-DIAG-CODE7  OR                       
                         SNF-OTHER-DIAG-CODE8  OR                       
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2008-53-CONTINUE.                                  
                                                                        
       2008-53-CONTINUE.                                                
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2008-53-EXIT. EXIT.                                              
                                                                        
       2009-53-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2009     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30' TO HLD-RTC                                      
              GO TO 2009-53-EXIT.                                       
                                                                        
           MOVE 1 TO R3.                                                
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1 TO R2                                              
           ELSE                                                         
              MOVE 2 TO R2.                                             
                                                                        
           PERFORM 900-SEARCH-4-RUG53  THRU 900-SEARCH-EXIT.            
                                                                        
           IF HLD-RTC NOT = 00 GO TO 2009-53-EXIT.                      
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60' TO HLD-RTC                                      
              MOVE ALL '0' TO HOLD-VAR-DATA                             
              GO TO 2009-53-EXIT.                                       
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2009-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2009-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2009-53-CONTINUE.                                  
                                                                        
       2009-53-CONTINUE.                                                
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2009-53-EXIT. EXIT.                                              
                                                                        
       2010-53-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2009     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30' TO HLD-RTC                                      
              GO TO 2010-53-EXIT.                                       
                                                                        
           MOVE 1 TO R3.                                                
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1 TO R2                                              
           ELSE                                                         
              MOVE 2 TO R2.                                             
                                                                        
           PERFORM 910-SEARCH-4-RUG53-2010  THRU 910-SEARCH-EXIT.       
                                                                        
           IF HLD-RTC NOT = 00 GO TO 2010-53-EXIT.                      
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60' TO HLD-RTC                                      
              MOVE ALL '0' TO HOLD-VAR-DATA                             
              GO TO 2010-53-EXIT.                                       
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2010-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2010-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2010-53-CONTINUE.                                  
                                                                        
       2010-53-CONTINUE.                                                
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2010-53-EXIT. EXIT.                                              
                                                                        
       2011-67-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2009     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30' TO HLD-RTC                                      
              GO TO 2011-67-EXIT.                                       
                                                                        
           MOVE 1 TO R3.                                                
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1 TO R2                                              
           ELSE                                                         
              MOVE 2 TO R2.                                             
                                                                        
           PERFORM 911-SEARCH-4-RUG67-2011  THRU 911-SEARCH-EXIT.       
                                                                        
           IF HLD-RTC NOT = 00 GO TO 2011-67-EXIT.                      
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60' TO HLD-RTC                                      
              MOVE ALL '0' TO HOLD-VAR-DATA                             
              GO TO 2011-67-EXIT.                                       
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2011-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2011-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2011-67-CONTINUE.                                  
                                                                        
       2011-67-CONTINUE.                                                
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2011-67-EXIT. EXIT.                                              
                                                                        
       2012-67-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2009     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30'              TO HLD-RTC                         
              GO TO 2012-67-EXIT.                                       
                                                                        
           MOVE 1 TO R3.                                                
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1 TO R2                                              
           ELSE                                                         
              MOVE 2 TO R2.                                             
                                                                        
           PERFORM 912-SEARCH-4-RUG67-2012                              
              THRU 912-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00 GO TO 2012-67-EXIT.                      
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2012-67-EXIT.                                       
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2012-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2012-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2012-67-CONTINUE.                                  
                                                                        
       2012-67-CONTINUE.                                                
                                                                        
           IF  SNF-FED-BLEND = 0                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE TO FED-PAYMENT-PORTION              
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2012-67-EXIT. EXIT.                                              
                                                                        
                                                                        
       2013-67-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2012     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30'              TO HLD-RTC                         
              GO TO 2013-67-EXIT.                                       
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 913-SEARCH-4-RUG67-2013                              
              THRU 913-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2013-67-EXIT.                                       
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2013-67-EXIT.                                       
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2013-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2013-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2013-67-CONTINUE.                                  
                                                                        
       2013-67-CONTINUE.                                                
                                                                        
           IF SNF-FED-BLEND = 0                                         
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE  TO FED-PAYMENT-PORTION             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2013-67-EXIT.                                                    
           EXIT.                                                        
                                                                        
                                                                        
       2014-67-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2013     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30'              TO HLD-RTC                         
              GO TO 2014-67-EXIT.                                       
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 914-SEARCH-4-RUG67-2014                              
              THRU 914-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2014-67-EXIT.                                       
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2014-67-EXIT.                                       
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2014-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2014-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2014-67-CONTINUE.                                  
                                                                        
       2014-67-CONTINUE.                                                
                                                                        
           IF SNF-FED-BLEND = 0                                         
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE  TO FED-PAYMENT-PORTION             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2014-67-EXIT.                                                    
           EXIT.                                                        
                                                                        
                                                                        
       2015-67-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2014     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30'              TO HLD-RTC                         
              GO TO 2015-67-EXIT.                                       
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 915-SEARCH-4-RUG67-2015                              
              THRU 915-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2015-67-EXIT.                                       
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2015-67-EXIT.                                       
                                                                        
                                                                        
                                                                        
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2015-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
                                                                        
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2015-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
                                                                        
           IF '042   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
                                                                        
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2015-67-CONTINUE.                                  
                                                                        
       2015-67-CONTINUE.                                                
                                                                        
           IF SNF-FED-BLEND = 0                                         
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE  TO FED-PAYMENT-PORTION             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2015-67-EXIT.                                                    
           EXIT.                                                        
                                                                        
                                                                        
       2016-67-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2015     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30'              TO HLD-RTC                         
              GO TO 2016-67-EXIT.                                       
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 916-SEARCH-4-RUG67-2016                              
              THRU 916-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2016-67-EXIT.                                       
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2016-67-EXIT.                                       
                                                                        
                                                                        
                                                                        
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2016-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
                                                                        
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2016-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
                                                                        
           IF 'B20   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
                                                                        
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2016-67-CONTINUE.                                  
                                                                        
       2016-67-CONTINUE.                                                
                                                                        
           IF SNF-FED-BLEND = 0                                         
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE  TO FED-PAYMENT-PORTION             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2016-67-EXIT.                                                    
           EXIT.                                                        
                                                                        
       2017-67-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2016     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30'              TO HLD-RTC                         
              GO TO 2017-67-EXIT.                                       
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              MOVE 1                 TO R2                              
           ELSE                                                         
              MOVE 2                 TO R2.                             
                                                                        
           PERFORM 917-SEARCH-4-RUG67-2017                              
              THRU 917-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2017-67-EXIT.                                       
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND = 0 OR 4                                    
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2017-67-EXIT.                                       
                                                                        
                                                                        
                                                                        
                                                                        
              COMPUTE NURSING-COMPONENT ROUNDED =                       
                         (NUR-INDEX * IP-RATE).                         
                                                                        
                                                                        
              COMPUTE THERAPY-COMPONENT ROUNDED =                       
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
              COMPUTE NCM-THR-COMPONENT ROUNDED =                       
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
                                                                        
              COMPUTE HLD-PAYMENT-RATE  ROUNDED =                       
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
                                                                        
              COMPUTE PAYMENT-RATE-ADJ ROUNDED =                        
                   (HLD-PAYMENT-RATE * PERCENT-2017-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
                                                                        
                                                                        
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                     (HLD-PAYMENT-RATE * PERCENT-2017-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
                                                                        
           IF 'B20   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
                                                                        
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * 2.28                          
               GO TO 2017-67-CONTINUE.                                  
                                                                        
       2017-67-CONTINUE.                                                
                                                                        
           IF SNF-FED-BLEND = 0                                         
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE  TO FED-PAYMENT-PORTION             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
                                                                        
       2017-67-EXIT.                                                    
           EXIT.                                                        
                                                                        
      ****----------------- ROUTINE MUST BE REPEATED FOR 2019 ----      
                                                                        
       2018-67-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2017     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30'              TO HLD-RTC                         
              GO TO 2018-67-EXIT.                                       
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              IF HLD-FED-BLEND-QRP-FAIL                                 
                   MOVE 3                 TO R2                         
               ELSE                                                     
                   MOVE 1                 TO R2                         
               END-IF                                                   
           ELSE                                                         
               IF HLD-FED-BLEND-QRP-FAIL                                
                   MOVE 4                 TO R2                         
               ELSE                                                     
                   MOVE 2                 TO R2                         
               END-IF                                                   
           END-IF.                                                      
                                                                        
                                                                        
           PERFORM 918-SEARCH-4-RUG67-2018                              
              THRU 918-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2018-67-EXIT.                                       
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND =  0 OR 1 OR 4                              
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2018-67-EXIT.                                       
                                                                        
           COMPUTE NURSING-COMPONENT ROUNDED =                          
                         (NUR-INDEX * IP-RATE).                         
                                                                        
                                                                        
           COMPUTE THERAPY-COMPONENT ROUNDED =                          
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
           COMPUTE NCM-THR-COMPONENT ROUNDED =                          
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
                                                                        
           COMPUTE HLD-PAYMENT-RATE  ROUNDED =                          
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
                                                                        
           COMPUTE PAYMENT-RATE-ADJ ROUNDED =                           
                   (HLD-PAYMENT-RATE * PERCENT-2018-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
                                                                        
                                                                        
           COMPUTE HLD-PAYMENT-RATE ROUNDED =                           
                     (HLD-PAYMENT-RATE * PERCENT-2018-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
      *------------------------------------------------------           
      *   APPLY AIDS ADD-ON IF INDICATED BY PRIN DX CODE    *           
      *------------------------------------------------------           
                                                                        
           IF 'B20   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
                                                                        
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * AIDS-ADD-ON-MULTIPLIER.       
               GO TO 2018-67-CONTINUE.                                  
                                                                        
       2018-67-CONTINUE.                                                
                                                                        
           IF SNF-FED-BLEND = 0                                         
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE  TO FED-PAYMENT-PORTION             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
      *                                                                 
                                                                        
       2018-67-EXIT.                                                    
           EXIT.                                                        
                                                                        
      ****---------------ROUTINE MUST BE REPEATED FOR 2020              
                                                                        
       2019-67-PAYMENT-RATE-CALC.                                       
      ***************************************************************   
      *     PAYMENT RATE CALCULATION PROCCESS EFFECTIVE OCT.1, 2018     
      ***************************************************************   
                                                                        
           IF HLD-CBSA = '     '                                        
              MOVE '30'              TO HLD-RTC                         
              GO TO 2019-67-EXIT.                                       
                                                                        
           MOVE 1                    TO R3.                             
                                                                        
           IF HLD-CBSA-RURAL-CHECK                                      
              IF HLD-FED-BLEND-QRP-FAIL                                 
                   MOVE 3                 TO R2                         
               ELSE                                                     
                   MOVE 1                 TO R2                         
               END-IF                                                   
           ELSE                                                         
               IF HLD-FED-BLEND-QRP-FAIL                                
                   MOVE 4                 TO R2                         
               ELSE                                                     
                   MOVE 2                 TO R2                         
               END-IF                                                   
           END-IF.                                                      
                                                                        
                                                                        
           PERFORM 919-SEARCH-4-RUG67-2019                              
              THRU 919-SEARCH-EXIT.                                     
                                                                        
           IF HLD-RTC NOT = 00                                          
              GO TO 2019-67-EXIT.                                       
                                                                        
           PERFORM 500-GET-RATES.                                       
                                                                        
           IF SNF-FED-BLEND =  0 OR 1 OR 4                              
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE '60'              TO HLD-RTC                         
              MOVE ALL '0'           TO HOLD-VAR-DATA                   
              GO TO 2019-67-EXIT.                                       
                                                                        
           COMPUTE NURSING-COMPONENT ROUNDED =                          
                         (NUR-INDEX * IP-RATE).                         
                                                                        
                                                                        
           COMPUTE THERAPY-COMPONENT ROUNDED =                          
                         (THR-INDEX * REHAB-RATE).                      
                                                                        
           COMPUTE NCM-THR-COMPONENT ROUNDED =                          
                         (FACTOR * NCM-THR-COMPONENT).                  
                                                                        
                                                                        
           COMPUTE HLD-PAYMENT-RATE  ROUNDED =                          
                         NURSING-COMPONENT +                            
                         THERAPY-COMPONENT +                            
                         NCM-THR-COMPONENT +                            
                         NCM-COMPONENT.                                 
                                                                        
                                                                        
           COMPUTE PAYMENT-RATE-ADJ ROUNDED =                           
                   (HLD-PAYMENT-RATE * PERCENT-2019-LABOR) *            
                     AREA-WAGE-INDEX.                                   
                                                                        
                                                                        
                                                                        
           COMPUTE HLD-PAYMENT-RATE ROUNDED =                           
                     (HLD-PAYMENT-RATE * PERCENT-2019-NLABOR) +         
                          PAYMENT-RATE-ADJ.                             
                                                                        
      *------------------------------------------------------           
      *   APPLY AIDS ADD-ON IF INDICATED BY PRIN DX CODE    *           
      *------------------------------------------------------           
                                                                        
           IF 'B20   ' = SNF-PRIN-DIAG-CODE   OR                        
                         SNF-OTHER-DIAG-CODE2 OR                        
                         SNF-OTHER-DIAG-CODE3 OR                        
                         SNF-OTHER-DIAG-CODE4 OR                        
                         SNF-OTHER-DIAG-CODE5 OR                        
                         SNF-OTHER-DIAG-CODE6 OR                        
                         SNF-OTHER-DIAG-CODE7 OR                        
                         SNF-OTHER-DIAG-CODE8 OR                        
                         SNF-OTHER-DIAG-CODE9  OR                       
                         SNF-OTHER-DIAG-CODE10 OR                       
                         SNF-OTHER-DIAG-CODE11 OR                       
                         SNF-OTHER-DIAG-CODE12 OR                       
                         SNF-OTHER-DIAG-CODE13 OR                       
                         SNF-OTHER-DIAG-CODE14 OR                       
                         SNF-OTHER-DIAG-CODE15 OR                       
                         SNF-OTHER-DIAG-CODE16 OR                       
                         SNF-OTHER-DIAG-CODE17 OR                       
                         SNF-OTHER-DIAG-CODE18 OR                       
                         SNF-OTHER-DIAG-CODE19 OR                       
                         SNF-OTHER-DIAG-CODE20 OR                       
                         SNF-OTHER-DIAG-CODE21 OR                       
                         SNF-OTHER-DIAG-CODE22 OR                       
                         SNF-OTHER-DIAG-CODE23 OR                       
                         SNF-OTHER-DIAG-CODE24 OR                       
                         SNF-OTHER-DIAG-CODE25                          
                                                                        
               COMPUTE HLD-PAYMENT-RATE ROUNDED =                       
                       HLD-PAYMENT-RATE * AIDS-ADD-ON-MULTIPLIER.       
               GO TO 2019-67-CONTINUE.                                  
                                                                        
       2019-67-CONTINUE.                                                
                                                                        
           IF SNF-FED-BLEND = 0                                         
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-FACILITY-RATE * 1.00.                         
                                                                        
           IF SNF-FED-BLEND = 4                                         
              MOVE HLD-PAYMENT-RATE  TO FED-PAYMENT-PORTION             
              COMPUTE HLD-PAYMENT-RATE ROUNDED =                        
                      HLD-PAYMENT-RATE * 1.00.                          
      *                                                                 
                                                                        
      *------------------------------------------------------           
      *   APPLY VBP USING THE VBP-MULTIPLIER &              *           
      *   DEVELOP THE VBP-PAY-DIFF                          *           
      *------------------------------------------------------           
                                                                        
                                                                        
           IF VBP-MULTIPLIER = ZEROES                                   
              MOVE '70'              TO HLD-RTC                         
              GO TO 2019-67-EXIT.                                       
                                                                        
           MOVE HLD-PAYMENT-RATE     TO INITIAL-PAYMENT-RATE.           
      *                                                                 
           COMPUTE VBP-ADJ-PYMT-RATE ROUNDED =                          
                   HLD-PAYMENT-RATE * HLD-VBP-MULTIPLIER.               
      *                                                                 
      *                                                                 
           COMPUTE HLD-VBP-PAY-DIFF ROUNDED =                           
                   VBP-ADJ-PYMT-RATE - INITIAL-PAYMENT-RATE.            
      *                                                                 
      *                                                                 
           MOVE VBP-ADJ-PYMT-RATE    TO HLD-PAYMENT-RATE.               
                                                                        
      *                                                                 
                                                                        
       2019-67-EXIT.                                                    
           EXIT.                                                        
                                                                        
      ******        L A S T   S O U R C E   S T A T E M E N T   *****   
