      ****************************************************************  
      * COPYBOOK: ADXONTBL                                           *  
      * EXTRA RURAL COUNTY CODES                                     *  
      *--------------------------------------------------------------*  
      * CATEGORY                    CY2019 CY2020 CY2021 CY2022      *  
      * (A) HIGH UTILIZATION         1.5%   0.5%   ----   ----       *  
      * (B) LOW POPULATION DENSITY   4.0%   3.0%   2.0%   1.0%       *  
      * (C) ALL OTHER                3.0%   2.0%   1.0%   ----       *  
      ****************************************************************  
       01 T-EXTRA-COUNTY-CODES.                                         
          05  T-EXTRA-COUNTY-CODE-DATA.                                 
              10  FILLER             PIC X(6) VALUE '01063A'.           
              10  FILLER             PIC X(6) VALUE '01129A'.           
              10  FILLER             PIC X(6) VALUE '05047C'.           
              10  FILLER             PIC X(6) VALUE '12075A'.           
              10  FILLER             PIC X(6) VALUE '13259A'.           
              10  FILLER             PIC X(6) VALUE '13263C'.           
              10  FILLER             PIC X(6) VALUE '16077A'.           
              10  FILLER             PIC X(6) VALUE '17057C'.           
              10  FILLER             PIC X(6) VALUE '17087C'.           
              10  FILLER             PIC X(6) VALUE '18047C'.           
              10  FILLER             PIC X(6) VALUE '18121C'.           
              10  FILLER             PIC X(6) VALUE '18171C'.           
              10  FILLER             PIC X(6) VALUE '19015C'.           
              10  FILLER             PIC X(6) VALUE '19099C'.           
              10  FILLER             PIC X(6) VALUE '20061C'.           
              10  FILLER             PIC X(6) VALUE '21043C'.           
              10  FILLER             PIC X(6) VALUE '22007A'.           
              10  FILLER             PIC X(6) VALUE '22067A'.           
              10  FILLER             PIC X(6) VALUE '25011C'.           
              10  FILLER             PIC X(6) VALUE '26067C'.           
              10  FILLER             PIC X(6) VALUE '26155C'.           
              10  FILLER             PIC X(6) VALUE '27075B'.           
              10  FILLER             PIC X(6) VALUE '28031A'.           
              10  FILLER             PIC X(6) VALUE '28051A'.           
              10  FILLER             PIC X(6) VALUE '28131A'.           
              10  FILLER             PIC X(6) VALUE '29053C'.           
              10  FILLER             PIC X(6) VALUE '29089C'.           
              10  FILLER             PIC X(6) VALUE '30095B'.           
              10  FILLER             PIC X(6) VALUE '37007C'.           
              10  FILLER             PIC X(6) VALUE '37029C'.           
              10  FILLER             PIC X(6) VALUE '37077C'.           
              10  FILLER             PIC X(6) VALUE '37085C'.           
              10  FILLER             PIC X(6) VALUE '39123C'.           
              10  FILLER             PIC X(6) VALUE '45027C'.           
              10  FILLER             PIC X(6) VALUE '47053A'.           
              10  FILLER             PIC X(6) VALUE '47161C'.           
              10  FILLER             PIC X(6) VALUE '48203A'.           
              10  FILLER             PIC X(6) VALUE '48431A'.           
              10  FILLER             PIC X(6) VALUE '51097C'.           
              10  FILLER             PIC X(6) VALUE '51113C'.           
              10  FILLER             PIC X(6) VALUE '51175C'.           
              10  FILLER             PIC X(6) VALUE '51620C'.           
              10  FILLER             PIC X(6) VALUE '54035C'.           
              10  FILLER             PIC X(6) VALUE '54065C'.           
              10  FILLER             PIC X(6) VALUE '55069C'.           
              10  FILLER             PIC X(6) VALUE '72001C'.           
              10  FILLER             PIC X(6) VALUE '72083C'.           
          05  FILLER     REDEFINES  T-EXTRA-COUNTY-CODE-DATA.           
              10  T-ECC-DATA         OCCURS    47 TIMES                 
                                     ASCENDING KEY IS T-ECC-CODE        
                                     INDEXED BY IX-ECC.                 
                  15  T-ECC-CODE     PIC X(5).                          
                  15  T-ECC-CATEGORY PIC X(1).                          
