      ******************************************************************
      * VPDTAB - VARIABLE PER DIEM ADJUSTMENT TABLE (STUB)
      * APPLIES TO 3 COMPONENTS: PT / OT / NTA
      * SNF PPS VARIABLE PER DIEM ADJUSTMENT FACTORS BY DAY
      ******************************************************************
       01  VARIABLE-PER-DIEM-TABLE.
           05  VX1                       PIC S9(04) COMP SYNC.
           05  VPD-ENTRIES.
               10  VPD-ENTRY OCCURS 100 TIMES.
                   15  VPD-PT-OT-FACT    PIC S9V9(06) COMP-3.
                   15  VPD-NTA-FACT      PIC S9V9(06) COMP-3.
