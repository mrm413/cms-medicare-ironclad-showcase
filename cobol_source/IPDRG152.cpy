      ***************************************************************
      **********YEARCHANGE 2015.2 ***********************************
      ***************************************************************
      ****  TABLE 5 FROM ANNUAL IPPS FINAL RULE                  ****
      ****  COPIED FROM D29L.@BFN2699.INPATCOB.COBR150(PPCAL150) ****
      ***************************************************************
       01  WK-DRGX-EFF-DATE      PIC X(08) VALUE '20141001'.
       01  PPS-DRG-TABLE.
           02  WK-DRG-DATA.
      ****** 08/01/2014 **********
               05  FILLER   PIC X(57)  VALUE
           '001 253920 286       368       HEART TRANSPLANT OR IMPLAN'.
               05  FILLER   PIC X(57)  VALUE
           '002 156820 157       187       HEART TRANSPLANT OR IMPLAN'.
               05  FILLER   PIC X(57)  VALUE
           '003 176399 263       322  Y    ECMO OR TRACH W MV 96+ HRS'.
               05  FILLER   PIC X(57)  VALUE
           '004 108533 202       244  Y    TRACH W MV 96+ HRS OR PDX '.
               05  FILLER   PIC X(57)  VALUE
           '005 104973 152       207       LIVER TRANSPLANT W MCC OR '.
               05  FILLER   PIC X(57)  VALUE
           '006 047461 079       088       LIVER TRANSPLANT W/O MCC  '.
               05  FILLER   PIC X(57)  VALUE
           '007 092986 159       186       LUNG TRANSPLANT           '.
               05  FILLER   PIC X(57)  VALUE
           '008 053302 098       113       SIMULTANEOUS PANCREAS/KIDN'.
               05  FILLER   PIC X(57)  VALUE
           '010 040849 078       096       PANCREAS TRANSPLANT       '.
               05  FILLER   PIC X(57)  VALUE
           '011 047380 113       141       TRACHEOSTOMY FOR FACE,MOUT'.
               05  FILLER   PIC X(57)  VALUE
           '012 033293 084       100       TRACHEOSTOMY FOR FACE,MOUT'.
               05  FILLER   PIC X(57)  VALUE
           '013 021357 058       066       TRACHEOSTOMY FOR FACE,MOUT'.
               05  FILLER   PIC X(57)  VALUE
           '014 109883 200       251       ALLOGENEIC BONE MARROW TRA'.
               05  FILLER   PIC X(57)  VALUE
           '016 058780 175       191       AUTOLOGOUS BONE MARROW TRA'.
               05  FILLER   PIC X(57)  VALUE
           '017 041603 089       124       AUTOLOGOUS BONE MARROW TRA'.
               05  FILLER   PIC X(57)  VALUE
           '020 094423 139       168       INTRACRANIAL VASCULAR PROC'.
               05  FILLER   PIC X(57)  VALUE
           '021 071555 119       134       INTRACRANIAL VASCULAR PROC'.
               05  FILLER   PIC X(57)  VALUE
           '022 044934 063       078       INTRACRANIAL VASCULAR PROC'.
               05  FILLER   PIC X(57)  VALUE
           '023 052939 079       110  Y    CRANIO W MAJOR DEV IMPL/AC'.
               05  FILLER   PIC X(57)  VALUE
           '024 037461 044       063  Y    CRANIO W MAJOR DEV IMPL/AC'.
               05  FILLER   PIC X(57)  VALUE
           '025 043374 074       097  Y    CRANIOTOMY & ENDOVASCULAR '.
               05  FILLER   PIC X(57)  VALUE
           '026 030011 047       061  Y    CRANIOTOMY & ENDOVASCULAR '.
               05  FILLER   PIC X(57)  VALUE
           '027 022824 025       033  Y    CRANIOTOMY & ENDOVASCULAR '.
               05  FILLER   PIC X(57)  VALUE
           '028 053968 096       123  Y Y  SPINAL PROCEDURES W MCC   '.
               05  FILLER   PIC X(57)  VALUE
           '029 031573 048       064  Y Y  SPINAL PROCEDURES W CC OR '.
               05  FILLER   PIC X(57)  VALUE
           '030 017835 026       034  Y Y  SPINAL PROCEDURES W/O CC/M'.
               05  FILLER   PIC X(57)  VALUE
           '031 041493 077       110  Y    VENTRICULAR SHUNT PROCEDUR'.
               05  FILLER   PIC X(57)  VALUE
           '032 020325 033       047  Y    VENTRICULAR SHUNT PROCEDUR'.
               05  FILLER   PIC X(57)  VALUE
           '033 015602 020       025  Y    VENTRICULAR SHUNT PROCEDUR'.
               05  FILLER   PIC X(57)  VALUE
           '034 037008 045       066       CAROTID ARTERY STENT PROCE'.
               05  FILLER   PIC X(57)  VALUE
           '035 022109 020       029       CAROTID ARTERY STENT PROCE'.
               05  FILLER   PIC X(57)  VALUE
           '036 017313 013       015       CAROTID ARTERY STENT PROCE'.
               05  FILLER   PIC X(57)  VALUE
           '037 031459 056       080       EXTRACRANIAL PROCEDURES W '.
               05  FILLER   PIC X(57)  VALUE
           '038 015870 023       034       EXTRACRANIAL PROCEDURES W '.
               05  FILLER   PIC X(57)  VALUE
           '039 010582 013       016       EXTRACRANIAL PROCEDURES W/'.
               05  FILLER   PIC X(57)  VALUE
           '040 037960 083       112  Y Y  PERIPH/CRANIAL NERVE & OTH'.
               05  FILLER   PIC X(57)  VALUE
           '041 021267 049       062  Y Y  PERIPH/CRANIAL NERVE & OTH'.
               05  FILLER   PIC X(57)  VALUE
           '042 018586 027       035  Y Y  PERIPH/CRANIAL NERVE & OTH'.
               05  FILLER   PIC X(57)  VALUE
           '052 015813 042       057       SPINAL DISORDERS & INJURIE'.
               05  FILLER   PIC X(57)  VALUE
           '053 009294 027       035       SPINAL DISORDERS & INJURIE'.
               05  FILLER   PIC X(57)  VALUE
           '054 013048 039       053  Y    NERVOUS SYSTEM NEOPLASMS W'.
               05  FILLER   PIC X(57)  VALUE
           '055 010191 030       041  Y    NERVOUS SYSTEM NEOPLASMS W'.
               05  FILLER   PIC X(57)  VALUE
           '056 017615 052       071  Y    DEGENERATIVE NERVOUS SYSTE'.
               05  FILLER   PIC X(57)  VALUE
           '057 010099 036       048  Y    DEGENERATIVE NERVOUS SYSTE'.
               05  FILLER   PIC X(57)  VALUE
           '058 016336 052       071       MULTIPLE SCLEROSIS & CEREB'.
               05  FILLER   PIC X(57)  VALUE
           '059 010290 039       047       MULTIPLE SCLEROSIS & CEREB'.
               05  FILLER   PIC X(57)  VALUE
           '060 007867 030       036       MULTIPLE SCLEROSIS & CEREB'.
               05  FILLER   PIC X(57)  VALUE
           '061 027571 057       074       ACUTE ISCHEMIC STROKE W US'.
               05  FILLER   PIC X(57)  VALUE
           '062 018555 041       048       ACUTE ISCHEMIC STROKE W US'.
               05  FILLER   PIC X(57)  VALUE
           '063 015098 030       034       ACUTE ISCHEMIC STROKE W US'.
               05  FILLER   PIC X(57)  VALUE
           '064 017381 046       063  Y    INTRACRANIAL HEMORRHAGE OR'.
               05  FILLER   PIC X(57)  VALUE
           '065 010643 034       042  Y    INTRACRANIAL HEMORRHAGE OR'.
               05  FILLER   PIC X(57)  VALUE
           '066 007530 024       028  Y    INTRACRANIAL HEMORRHAGE OR'.
               05  FILLER   PIC X(57)  VALUE
           '067 014527 041       054       NONSPECIFIC CVA & PRECEREB'.
               05  FILLER   PIC X(57)  VALUE
           '068 008395 024       030       NONSPECIFIC CVA & PRECEREB'.
               05  FILLER   PIC X(57)  VALUE
           '069 006985 021       026       TRANSIENT ISCHEMIA        '.
               05  FILLER   PIC X(57)  VALUE
           '070 016438 048       065  Y    NONSPECIFIC CEREBROVASCULA'.
               05  FILLER   PIC X(57)  VALUE
           '071 009748 035       045  Y    NONSPECIFIC CEREBROVASCULA'.
               05  FILLER   PIC X(57)  VALUE
           '072 006947 023       029  Y    NONSPECIFIC CEREBROVASCULA'.
               05  FILLER   PIC X(57)  VALUE
           '073 013290 040       053       CRANIAL & PERIPHERAL NERVE'.
               05  FILLER   PIC X(57)  VALUE
           '074 008847 030       038       CRANIAL & PERIPHERAL NERVE'.
               05  FILLER   PIC X(57)  VALUE
           '075 016595 052       066       VIRAL MENINGITIS W CC/MCC '.
               05  FILLER   PIC X(57)  VALUE
           '076 008629 032       038       VIRAL MENINGITIS W/O CC/MC'.
               05  FILLER   PIC X(57)  VALUE
           '077 016245 046       060       HYPERTENSIVE ENCEPHALOPATH'.
               05  FILLER   PIC X(57)  VALUE
           '078 009361 032       040       HYPERTENSIVE ENCEPHALOPATH'.
               05  FILLER   PIC X(57)  VALUE
           '079 006677 023       027       HYPERTENSIVE ENCEPHALOPATH'.
               05  FILLER   PIC X(57)  VALUE
           '080 012790 037       052       NONTRAUMATIC STUPOR & COMA'.
               05  FILLER   PIC X(57)  VALUE
           '081 007850 027       036       NONTRAUMATIC STUPOR & COMA'.
               05  FILLER   PIC X(57)  VALUE
           '082 019142 032       054       TRAUMATIC STUPOR & COMA, C'.
               05  FILLER   PIC X(57)  VALUE
           '083 012401 033       042       TRAUMATIC STUPOR & COMA, C'.
               05  FILLER   PIC X(57)  VALUE
           '084 008592 021       026       TRAUMATIC STUPOR & COMA, C'.
               05  FILLER   PIC X(57)  VALUE
           '085 019770 047       065  Y    TRAUMATIC STUPOR & COMA, C'.
               05  FILLER   PIC X(57)  VALUE
           '086 011181 032       041  Y    TRAUMATIC STUPOR & COMA, C'.
               05  FILLER   PIC X(57)  VALUE
           '087 007460 021       026  Y    TRAUMATIC STUPOR & COMA, C'.
               05  FILLER   PIC X(57)  VALUE
           '088 015708 038       054       CONCUSSION W MCC          '.
               05  FILLER   PIC X(57)  VALUE
           '089 009588 027       033       CONCUSSION W CC           '.
               05  FILLER   PIC X(57)  VALUE
           '090 007353 018       022       CONCUSSION W/O CC/MCC     '.
               05  FILLER   PIC X(57)  VALUE
           '091 015978 042       059  Y    OTHER DISORDERS OF NERVOUS'.
               05  FILLER   PIC X(57)  VALUE
           '092 008989 031       039  Y    OTHER DISORDERS OF NERVOUS'.
               05  FILLER   PIC X(57)  VALUE
           '093 006783 022       027  Y    OTHER DISORDERS OF NERVOUS'.
               05  FILLER   PIC X(57)  VALUE
           '094 033357 082       107       BACTERIAL & TUBERCULOUS IN'.
               05  FILLER   PIC X(57)  VALUE
           '095 023844 060       076       BACTERIAL & TUBERCULOUS IN'.
               05  FILLER   PIC X(57)  VALUE
           '096 020726 045       053       BACTERIAL & TUBERCULOUS IN'.
               05  FILLER   PIC X(57)  VALUE
           '097 031625 085       109       NON-BACTERIAL INFECT OF NE'.
               05  FILLER   PIC X(57)  VALUE
           '098 017189 055       069       NON-BACTERIAL INFECT OF NE'.
               05  FILLER   PIC X(57)  VALUE
           '099 011609 038       047       NON-BACTERIAL INFECT OF NE'.
               05  FILLER   PIC X(57)  VALUE
           '100 015304 042       056  Y    SEIZURES W MCC            '.
               05  FILLER   PIC X(57)  VALUE
           '101 007567 026       032  Y    SEIZURES W/O MCC          '.
               05  FILLER   PIC X(57)  VALUE
           '102 010073 030       040       HEADACHES W MCC           '.
               05  FILLER   PIC X(57)  VALUE
           '103 006915 023       029       HEADACHES W/O MCC         '.
               05  FILLER   PIC X(57)  VALUE
           '113 018611 038       052       ORBITAL PROCEDURES W CC/MC'.
               05  FILLER   PIC X(57)  VALUE
           '114 011568 024       031       ORBITAL PROCEDURES W/O CC/'.
               05  FILLER   PIC X(57)  VALUE
           '115 012780 035       046       EXTRAOCULAR PROCEDURES EXC'.
               05  FILLER   PIC X(57)  VALUE
           '116 014005 033       046       INTRAOCULAR PROCEDURES W C'.
               05  FILLER   PIC X(57)  VALUE
           '117 008239 018       022       INTRAOCULAR PROCEDURES W/O'.
               05  FILLER   PIC X(57)  VALUE
           '121 010635 040       050       ACUTE MAJOR EYE INFECTIONS'.
               05  FILLER   PIC X(57)  VALUE
           '122 006352 031       038       ACUTE MAJOR EYE INFECTIONS'.
               05  FILLER   PIC X(57)  VALUE
           '123 006732 020       025       NEUROLOGICAL EYE DISORDERS'.
               05  FILLER   PIC X(57)  VALUE
           '124 011432 034       046       OTHER DISORDERS OF THE EYE'.
               05  FILLER   PIC X(57)  VALUE
           '125 006852 024       031       OTHER DISORDERS OF THE EYE'.
               05  FILLER   PIC X(57)  VALUE
           '129 023284 038       054       MAJOR HEAD & NECK PROCEDUR'.
               05  FILLER   PIC X(57)  VALUE
           '130 012599 023       028       MAJOR HEAD & NECK PROCEDUR'.
               05  FILLER   PIC X(57)  VALUE
           '131 023703 041       057       CRANIAL/FACIAL PROCEDURES '.
               05  FILLER   PIC X(57)  VALUE
           '132 014248 022       027       CRANIAL/FACIAL PROCEDURES '.
               05  FILLER   PIC X(57)  VALUE
           '133 018644 037       054       OTHER EAR, NOSE, MOUTH & T'.
               05  FILLER   PIC X(57)  VALUE
           '134 010083 019       024       OTHER EAR, NOSE, MOUTH & T'.
               05  FILLER   PIC X(57)  VALUE
           '135 019150 042       057       SINUS & MASTOID PROCEDURES'.
               05  FILLER   PIC X(57)  VALUE
           '136 010509 018       024       SINUS & MASTOID PROCEDURES'.
               05  FILLER   PIC X(57)  VALUE
           '137 013735 037       049       MOUTH PROCEDURES W CC/MCC '.
               05  FILLER   PIC X(57)  VALUE
           '138 007888 019       024       MOUTH PROCEDURES W/O CC/MC'.
               05  FILLER   PIC X(57)  VALUE
           '139 009856 016       021       SALIVARY GLAND PROCEDURES '.
               05  FILLER   PIC X(57)  VALUE
           '146 019749 058       082       EAR, NOSE, MOUTH & THROAT '.
               05  FILLER   PIC X(57)  VALUE
           '147 012869 039       054       EAR, NOSE, MOUTH & THROAT '.
               05  FILLER   PIC X(57)  VALUE
           '148 008205 023       031       EAR, NOSE, MOUTH & THROAT '.
               05  FILLER   PIC X(57)  VALUE
           '149 006342 020       025       DYSEQUILIBRIUM            '.
               05  FILLER   PIC X(57)  VALUE
           '150 012943 037       049       EPISTAXIS W MCC           '.
               05  FILLER   PIC X(57)  VALUE
           '151 006690 022       027       EPISTAXIS W/O MCC         '.
               05  FILLER   PIC X(57)  VALUE
           '152 010162 036       045       OTITIS MEDIA & URI W MCC  '.
               05  FILLER   PIC X(57)  VALUE
           '153 006884 027       032       OTITIS MEDIA & URI W/O MCC'.
               05  FILLER   PIC X(57)  VALUE
           '154 013703 041       054       OTHER EAR, NOSE, MOUTH & T'.
               05  FILLER   PIC X(57)  VALUE
           '155 008848 031       039       OTHER EAR, NOSE, MOUTH & T'.
               05  FILLER   PIC X(57)  VALUE
           '156 006516 023       028       OTHER EAR, NOSE, MOUTH & T'.
               05  FILLER   PIC X(57)  VALUE
           '157 015814 047       063       DENTAL & ORAL DISEASES W M'.
               05  FILLER   PIC X(57)  VALUE
           '158 008519 030       039       DENTAL & ORAL DISEASES W C'.
               05  FILLER   PIC X(57)  VALUE
           '159 005935 020       025       DENTAL & ORAL DISEASES W/O'.
               05  FILLER   PIC X(57)  VALUE
           '163 050332 108       132  Y    MAJOR CHEST PROCEDURES W M'.
               05  FILLER   PIC X(57)  VALUE
           '164 026010 054       066  Y    MAJOR CHEST PROCEDURES W C'.
               05  FILLER   PIC X(57)  VALUE
           '165 018220 033       039  Y    MAJOR CHEST PROCEDURES W/O'.
               05  FILLER   PIC X(57)  VALUE
           '166 036610 087       111  Y    OTHER RESP SYSTEM O.R. PRO'.
               05  FILLER   PIC X(57)  VALUE
           '167 019818 052       066  Y    OTHER RESP SYSTEM O.R. PRO'.
               05  FILLER   PIC X(57)  VALUE
           '168 013291 031       039  Y    OTHER RESP SYSTEM O.R. PRO'.
               05  FILLER   PIC X(57)  VALUE
           '175 015271 052       063  Y    PULMONARY EMBOLISM W MCC  '.
               05  FILLER   PIC X(57)  VALUE
           '176 009670 035       042  Y    PULMONARY EMBOLISM W/O MCC'.
               05  FILLER   PIC X(57)  VALUE
           '177 019492 062       077  Y    RESPIRATORY INFECTIONS & I'.
               05  FILLER   PIC X(57)  VALUE
           '178 013909 050       061  Y    RESPIRATORY INFECTIONS & I'.
               05  FILLER   PIC X(57)  VALUE
           '179 009693 037       045  Y    RESPIRATORY INFECTIONS & I'.
               05  FILLER   PIC X(57)  VALUE
           '180 016869 053       069       RESPIRATORY NEOPLASMS W MC'.
               05  FILLER   PIC X(57)  VALUE
           '181 011582 037       049       RESPIRATORY NEOPLASMS W CC'.
               05  FILLER   PIC X(57)  VALUE
           '182 008088 026       033       RESPIRATORY NEOPLASMS W/O '.
               05  FILLER   PIC X(57)  VALUE
           '183 015101 048       060       MAJOR CHEST TRAUMA W MCC  '.
               05  FILLER   PIC X(57)  VALUE
           '184 009889 034       040       MAJOR CHEST TRAUMA W CC   '.
               05  FILLER   PIC X(57)  VALUE
           '185 006628 024       028       MAJOR CHEST TRAUMA W/O CC/'.
               05  FILLER   PIC X(57)  VALUE
           '186 015452 048       062  Y    PLEURAL EFFUSION W MCC    '.
               05  FILLER   PIC X(57)  VALUE
           '187 010691 035       045  Y    PLEURAL EFFUSION W CC     '.
               05  FILLER   PIC X(57)  VALUE
           '188 007609 026       032  Y    PLEURAL EFFUSION W/O CC/MC'.
               05  FILLER   PIC X(57)  VALUE
           '189 012136 039       050       PULMONARY EDEMA & RESPIRAT'.
               05  FILLER   PIC X(57)  VALUE
           '190 011743 042       051  Y    CHRONIC OBSTRUCTIVE PULMON'.
               05  FILLER   PIC X(57)  VALUE
           '191 009370 034       042  Y    CHRONIC OBSTRUCTIVE PULMON'.
               05  FILLER   PIC X(57)  VALUE
           '192 007190 028       033  Y    CHRONIC OBSTRUCTIVE PULMON'.
               05  FILLER   PIC X(57)  VALUE
           '193 014491 049       060  Y    SIMPLE PNEUMONIA & PLEURIS'.
               05  FILLER   PIC X(57)  VALUE
           '194 009688 038       045  Y    SIMPLE PNEUMONIA & PLEURIS'.
               05  FILLER   PIC X(57)  VALUE
           '195 007044 029       034  Y    SIMPLE PNEUMONIA & PLEURIS'.
               05  FILLER   PIC X(57)  VALUE
           '196 016635 054       069  Y    INTERSTITIAL LUNG DISEASE '.
               05  FILLER   PIC X(57)  VALUE
           '197 010615 037       046  Y    INTERSTITIAL LUNG DISEASE '.
               05  FILLER   PIC X(57)  VALUE
           '198 008054 028       034  Y    INTERSTITIAL LUNG DISEASE '.
               05  FILLER   PIC X(57)  VALUE
           '199 018345 058       075       PNEUMOTHORAX W MCC        '.
               05  FILLER   PIC X(57)  VALUE
           '200 010084 034       044       PNEUMOTHORAX W CC         '.
               05  FILLER   PIC X(57)  VALUE
           '201 007096 025       032       PNEUMOTHORAX W/O CC/MCC   '.
               05  FILLER   PIC X(57)  VALUE
           '202 008775 032       039       BRONCHITIS & ASTHMA W CC/M'.
               05  FILLER   PIC X(57)  VALUE
           '203 006535 025       030       BRONCHITIS & ASTHMA W/O CC'.
               05  FILLER   PIC X(57)  VALUE
           '204 007041 021       027       RESPIRATORY SIGNS & SYMPTO'.
               05  FILLER   PIC X(57)  VALUE
           '205 013999 040       053  Y    OTHER RESPIRATORY SYSTEM D'.
               05  FILLER   PIC X(57)  VALUE
           '206 007942 025       032  Y    OTHER RESPIRATORY SYSTEM D'.
               05  FILLER   PIC X(57)  VALUE
           '207 053425 124       144  Y    RESPIRATORY SYSTEM DIAGNOS'.
               05  FILLER   PIC X(57)  VALUE
           '208 022969 050       068       RESPIRATORY SYSTEM DIAGNOS'.
               05  FILLER   PIC X(57)  VALUE
           '215 154348 112       172       OTHER HEART ASSIST SYSTEM '.
               05  FILLER   PIC X(57)  VALUE
           '216 095238 130       158  Y Y  CARDIAC VALVE & OTH MAJ CA'.
               05  FILLER   PIC X(57)  VALUE
           '217 063291 088       099  Y Y  CARDIAC VALVE & OTH MAJ CA'.
               05  FILLER   PIC X(57)  VALUE
           '218 055693 064       073  Y Y  CARDIAC VALVE & OTH MAJ CA'.
               05  FILLER   PIC X(57)  VALUE
           '219 077067 098       119  Y Y  CARDIAC VALVE & OTH MAJ CA'.
               05  FILLER   PIC X(57)  VALUE
           '220 052056 066       073  Y Y  CARDIAC VALVE & OTH MAJ CA'.
               05  FILLER   PIC X(57)  VALUE
           '221 046347 049       054  Y Y  CARDIAC VALVE & OTH MAJ CA'.
               05  FILLER   PIC X(57)  VALUE
           '222 086570 099       120       CARDIAC DEFIB IMPLANT W CA'.
               05  FILLER   PIC X(57)  VALUE
           '223 062924 044       060       CARDIAC DEFIB IMPLANT W CA'.
               05  FILLER   PIC X(57)  VALUE
           '224 076733 079       098       CARDIAC DEFIB IMPLANT W CA'.
               05  FILLER   PIC X(57)  VALUE
           '225 058610 041       049       CARDIAC DEFIB IMPLANT W CA'.
               05  FILLER   PIC X(57)  VALUE
           '226 069573 064       087       CARDIAC DEFIBRILLATOR IMPL'.
               05  FILLER   PIC X(57)  VALUE
           '227 054493 024       035       CARDIAC DEFIBRILLATOR IMPL'.
               05  FILLER   PIC X(57)  VALUE
           '228 073113 112       135       OTHER CARDIOTHORACIC PROCE'.
               05  FILLER   PIC X(57)  VALUE
           '229 044606 065       075       OTHER CARDIOTHORACIC PROCE'.
               05  FILLER   PIC X(57)  VALUE
           '230 040755 043       050       OTHER CARDIOTHORACIC PROCE'.
               05  FILLER   PIC X(57)  VALUE
           '231 077247 106       125       CORONARY BYPASS W PTCA W M'.
               05  FILLER   PIC X(57)  VALUE
           '232 055976 079       086       CORONARY BYPASS W PTCA W/O'.
               05  FILLER   PIC X(57)  VALUE
           '233 073493 118       133  Y    CORONARY BYPASS W CARDIAC '.
               05  FILLER   PIC X(57)  VALUE
           '234 048816 080       086  Y    CORONARY BYPASS W CARDIAC '.
               05  FILLER   PIC X(57)  VALUE
           '235 057089 090       103  Y    CORONARY BYPASS W/O CARDIA'.
               05  FILLER   PIC X(57)  VALUE
           '236 037952 060       065  Y    CORONARY BYPASS W/O CARDIA'.
               05  FILLER   PIC X(57)  VALUE
           '237 050843 067       097       MAJOR CARDIOVASC PROCEDURE'.
               05  FILLER   PIC X(57)  VALUE
           '238 034241 026       037       MAJOR CARDIOVASC PROCEDURE'.
               05  FILLER   PIC X(57)  VALUE
           '239 047590 106       134  Y    AMPUTATION FOR CIRC SYS DI'.
               05  FILLER   PIC X(57)  VALUE
           '240 027594 072       088  Y    AMPUTATION FOR CIRC SYS DI'.
               05  FILLER   PIC X(57)  VALUE
           '241 014111 044       052  Y    AMPUTATION FOR CIRC SYS DI'.
               05  FILLER   PIC X(57)  VALUE
           '242 037242 058       074  Y    PERMANENT CARDIAC PACEMAKE'.
               05  FILLER   PIC X(57)  VALUE
           '243 026695 036       045  Y    PERMANENT CARDIAC PACEMAKE'.
               05  FILLER   PIC X(57)  VALUE
           '244 021555 024       029  Y    PERMANENT CARDIAC PACEMAKE'.
               05  FILLER   PIC X(57)  VALUE
           '245 046485 034       049       AICD GENERATOR PROCEDURES '.
               05  FILLER   PIC X(57)  VALUE
           '246 032368 040       054       PERC CARDIOVASC PROC W DRU'.
               05  FILLER   PIC X(57)  VALUE
           '247 020586 021       025       PERC CARDIOVASC PROC W DRU'.
               05  FILLER   PIC X(57)  VALUE
           '248 030411 048       064       PERC CARDIOVASC PROC W NON'.
               05  FILLER   PIC X(57)  VALUE
           '249 018808 024       030       PERC CARDIOVASC PROC W NON'.
               05  FILLER   PIC X(57)  VALUE
           '250 029885 051       069       PERC CARDIOVASC PROC W/O C'.
               05  FILLER   PIC X(57)  VALUE
           '251 020399 023       030       PERC CARDIOVASC PROC W/O C'.
               05  FILLER   PIC X(57)  VALUE
           '252 032646 054       079       OTHER VASCULAR PROCEDURES '.
               05  FILLER   PIC X(57)  VALUE
           '253 025532 040       055       OTHER VASCULAR PROCEDURES '.
               05  FILLER   PIC X(57)  VALUE
           '254 017304 022       028       OTHER VASCULAR PROCEDURES '.
               05  FILLER   PIC X(57)  VALUE
           '255 026051 067       085  Y    UPPER LIMB & TOE AMPUTATIO'.
               05  FILLER   PIC X(57)  VALUE
           '256 016986 055       066  Y    UPPER LIMB & TOE AMPUTATIO'.
               05  FILLER   PIC X(57)  VALUE
           '257 010558 032       041  Y    UPPER LIMB & TOE AMPUTATIO'.
               05  FILLER   PIC X(57)  VALUE
           '258 027613 049       063       CARDIAC PACEMAKER DEVICE R'.
               05  FILLER   PIC X(57)  VALUE
           '259 019924 027       035       CARDIAC PACEMAKER DEVICE R'.
               05  FILLER   PIC X(57)  VALUE
           '260 037456 079       106       CARDIAC PACEMAKER REVISION'.
               05  FILLER   PIC X(57)  VALUE
           '261 018552 035       046       CARDIAC PACEMAKER REVISION'.
               05  FILLER   PIC X(57)  VALUE
           '262 013978 024       030       CARDIAC PACEMAKER REVISION'.
               05  FILLER   PIC X(57)  VALUE
           '263 018664 037       056       VEIN LIGATION & STRIPPING '.
               05  FILLER   PIC X(57)  VALUE
           '264 028292 058       084  Y    OTHER CIRCULATORY SYSTEM O'.
               05  FILLER   PIC X(57)  VALUE
           '265 028641 028       043       AICD LEAD PROCEDURES      '.
               05  FILLER   PIC X(57)  VALUE
           '266 089920 084       106  Y Y  ENDOVASCULAR CARDIAC VALVE'.
               05  FILLER   PIC X(57)  VALUE
           '267 067517 050       058  Y Y  ENDOVASCULAR CARDIAC VALVE'.
               05  FILLER   PIC X(57)  VALUE
           '280 017289 047       060  Y    ACUTE MYOCARDIAL INFARCTIO'.
               05  FILLER   PIC X(57)  VALUE
           '281 010247 030       037  Y    ACUTE MYOCARDIAL INFARCTIO'.
               05  FILLER   PIC X(57)  VALUE
           '282 007562 020       024  Y    ACUTE MYOCARDIAL INFARCTIO'.
               05  FILLER   PIC X(57)  VALUE
           '283 016753 030       048       ACUTE MYOCARDIAL INFARCTIO'.
               05  FILLER   PIC X(57)  VALUE
           '284 007703 018       025       ACUTE MYOCARDIAL INFARCTIO'.
               05  FILLER   PIC X(57)  VALUE
           '285 005065 013       016       ACUTE MYOCARDIAL INFARCTIO'.
               05  FILLER   PIC X(57)  VALUE
           '286 021240 051       068       CIRCULATORY DISORDERS EXCE'.
               05  FILLER   PIC X(57)  VALUE
           '287 011290 025       032       CIRCULATORY DISORDERS EXCE'.
               05  FILLER   PIC X(57)  VALUE
           '288 027138 076       095  Y    ACUTE & SUBACUTE ENDOCARDI'.
               05  FILLER   PIC X(57)  VALUE
           '289 016991 058       070  Y    ACUTE & SUBACUTE ENDOCARDI'.
               05  FILLER   PIC X(57)  VALUE
           '290 012476 039       049  Y    ACUTE & SUBACUTE ENDOCARDI'.
               05  FILLER   PIC X(57)  VALUE
           '291 015097 047       059  Y    HEART FAILURE & SHOCK W MC'.
               05  FILLER   PIC X(57)  VALUE
           '292 009824 037       045  Y    HEART FAILURE & SHOCK W CC'.
               05  FILLER   PIC X(57)  VALUE
           '293 006762 026       031  Y    HEART FAILURE & SHOCK W/O '.
               05  FILLER   PIC X(57)  VALUE
           '294 010480 041       050       DEEP VEIN THROMBOPHLEBITIS'.
               05  FILLER   PIC X(57)  VALUE
           '295 006926 031       037       DEEP VEIN THROMBOPHLEBITIS'.
               05  FILLER   PIC X(57)  VALUE
           '296 012347 018       027       CARDIAC ARREST, UNEXPLAINE'.
               05  FILLER   PIC X(57)  VALUE
           '297 006475 013       016       CARDIAC ARREST, UNEXPLAINE'.
               05  FILLER   PIC X(57)  VALUE
           '298 004227 011       012       CARDIAC ARREST, UNEXPLAINE'.
               05  FILLER   PIC X(57)  VALUE
           '299 014094 044       057  Y    PERIPHERAL VASCULAR DISORD'.
               05  FILLER   PIC X(57)  VALUE
           '300 009770 036       044  Y    PERIPHERAL VASCULAR DISORD'.
               05  FILLER   PIC X(57)  VALUE
           '301 006776 027       033  Y    PERIPHERAL VASCULAR DISORD'.
               05  FILLER   PIC X(57)  VALUE
           '302 010311 029       039       ATHEROSCLEROSIS W MCC     '.
               05  FILLER   PIC X(57)  VALUE
           '303 006101 019       023       ATHEROSCLEROSIS W/O MCC   '.
               05  FILLER   PIC X(57)  VALUE
           '304 010016 033       042       HYPERTENSION W MCC        '.
               05  FILLER   PIC X(57)  VALUE
           '305 006272 021       026       HYPERTENSION W/O MCC      '.
               05  FILLER   PIC X(57)  VALUE
           '306 013687 039       052       CARDIAC CONGENITAL & VALVU'.
               05  FILLER   PIC X(57)  VALUE
           '307 007698 025       031       CARDIAC CONGENITAL & VALVU'.
               05  FILLER   PIC X(57)  VALUE
           '308 012107 038       049       CARDIAC ARRHYTHMIA & CONDU'.
               05  FILLER   PIC X(57)  VALUE
           '309 007865 027       033       CARDIAC ARRHYTHMIA & CONDU'.
               05  FILLER   PIC X(57)  VALUE
           '310 005493 019       023       CARDIAC ARRHYTHMIA & CONDU'.
               05  FILLER   PIC X(57)  VALUE
           '311 005662 018       022       ANGINA PECTORIS           '.
               05  FILLER   PIC X(57)  VALUE
           '312 007423 024       030       SYNCOPE & COLLAPSE        '.
               05  FILLER   PIC X(57)  VALUE
           '313 006138 018       021       CHEST PAIN                '.
               05  FILLER   PIC X(57)  VALUE
           '314 019195 050       067  Y    OTHER CIRCULATORY SYSTEM D'.
               05  FILLER   PIC X(57)  VALUE
           '315 009613 030       039  Y    OTHER CIRCULATORY SYSTEM D'.
               05  FILLER   PIC X(57)  VALUE
           '316 006210 020       024  Y    OTHER CIRCULATORY SYSTEM D'.
               05  FILLER   PIC X(57)  VALUE
           '326 053847 112       145  Y    STOMACH, ESOPHAGEAL & DUOD'.
               05  FILLER   PIC X(57)  VALUE
           '327 026532 060       078  Y    STOMACH, ESOPHAGEAL & DUOD'.
               05  FILLER   PIC X(57)  VALUE
           '328 014949 026       034  Y    STOMACH, ESOPHAGEAL & DUOD'.
               05  FILLER   PIC X(57)  VALUE
           '329 050776 117       144  Y    MAJOR SMALL & LARGE BOWEL '.
               05  FILLER   PIC X(57)  VALUE
           '330 025491 071       084  Y    MAJOR SMALL & LARGE BOWEL '.
               05  FILLER   PIC X(57)  VALUE
           '331 016580 043       048  Y    MAJOR SMALL & LARGE BOWEL '.
               05  FILLER   PIC X(57)  VALUE
           '332 047048 105       128  Y    RECTAL RESECTION W MCC    '.
               05  FILLER   PIC X(57)  VALUE
           '333 024728 063       073  Y    RECTAL RESECTION W CC     '.
               05  FILLER   PIC X(57)  VALUE
           '334 016032 036       042  Y    RECTAL RESECTION W/O CC/MC'.
               05  FILLER   PIC X(57)  VALUE
           '335 042881 109       132  Y    PERITONEAL ADHESIOLYSIS W '.
               05  FILLER   PIC X(57)  VALUE
           '336 023539 069       083  Y    PERITONEAL ADHESIOLYSIS W '.
               05  FILLER   PIC X(57)  VALUE
           '337 015596 041       050  Y    PERITONEAL ADHESIOLYSIS W/'.
               05  FILLER   PIC X(57)  VALUE
           '338 030701 076       092       APPENDECTOMY W COMPLICATED'.
               05  FILLER   PIC X(57)  VALUE
           '339 017340 050       059       APPENDECTOMY W COMPLICATED'.
               05  FILLER   PIC X(57)  VALUE
           '340 012137 029       035       APPENDECTOMY W COMPLICATED'.
               05  FILLER   PIC X(57)  VALUE
           '341 022862 047       064       APPENDECTOMY W/O COMPLICAT'.
               05  FILLER   PIC X(57)  VALUE
           '342 013112 027       035       APPENDECTOMY W/O COMPLICAT'.
               05  FILLER   PIC X(57)  VALUE
           '343 009404 016       019       APPENDECTOMY W/O COMPLICAT'.
               05  FILLER   PIC X(57)  VALUE
           '344 033079 087       112       MINOR SMALL & LARGE BOWEL '.
               05  FILLER   PIC X(57)  VALUE
           '345 017107 054       064       MINOR SMALL & LARGE BOWEL '.
               05  FILLER   PIC X(57)  VALUE
           '346 011991 038       043       MINOR SMALL & LARGE BOWEL '.
               05  FILLER   PIC X(57)  VALUE
           '347 026226 064       087       ANAL & STOMAL PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '348 013825 040       051       ANAL & STOMAL PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '349 009078 025       030       ANAL & STOMAL PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '350 024665 057       076       INGUINAL & FEMORAL HERNIA '.
               05  FILLER   PIC X(57)  VALUE
           '351 014024 035       044       INGUINAL & FEMORAL HERNIA '.
               05  FILLER   PIC X(57)  VALUE
           '352 009288 021       025       INGUINAL & FEMORAL HERNIA '.
               05  FILLER   PIC X(57)  VALUE
           '353 029077 064       083       HERNIA PROCEDURES EXCEPT I'.
               05  FILLER   PIC X(57)  VALUE
           '354 016915 041       050       HERNIA PROCEDURES EXCEPT I'.
               05  FILLER   PIC X(57)  VALUE
           '355 012521 026       032       HERNIA PROCEDURES EXCEPT I'.
               05  FILLER   PIC X(57)  VALUE
           '356 038573 084       113  Y    OTHER DIGESTIVE SYSTEM O.R'.
               05  FILLER   PIC X(57)  VALUE
           '357 021072 052       067  Y    OTHER DIGESTIVE SYSTEM O.R'.
               05  FILLER   PIC X(57)  VALUE
           '358 013737 031       039  Y    OTHER DIGESTIVE SYSTEM O.R'.
               05  FILLER   PIC X(57)  VALUE
           '368 018641 049       064       MAJOR ESOPHAGEAL DISORDERS'.
               05  FILLER   PIC X(57)  VALUE
           '369 010703 034       041       MAJOR ESOPHAGEAL DISORDERS'.
               05  FILLER   PIC X(57)  VALUE
           '370 007390 024       029       MAJOR ESOPHAGEAL DISORDERS'.
               05  FILLER   PIC X(57)  VALUE
           '371 018633 061       078  Y    MAJOR GASTROINTESTINAL DIS'.
               05  FILLER   PIC X(57)  VALUE
           '372 011343 046       056  Y    MAJOR GASTROINTESTINAL DIS'.
               05  FILLER   PIC X(57)  VALUE
           '373 008013 035       041  Y    MAJOR GASTROINTESTINAL DIS'.
               05  FILLER   PIC X(57)  VALUE
           '374 020182 060       079  Y    DIGESTIVE MALIGNANCY W MCC'.
               05  FILLER   PIC X(57)  VALUE
           '375 012429 042       053  Y    DIGESTIVE MALIGNANCY W CC '.
               05  FILLER   PIC X(57)  VALUE
           '376 009021 029       036  Y    DIGESTIVE MALIGNANCY W/O C'.
               05  FILLER   PIC X(57)  VALUE
           '377 017775 048       061  Y    G.I. HEMORRHAGE W MCC     '.
               05  FILLER   PIC X(57)  VALUE
           '378 010021 033       039  Y    G.I. HEMORRHAGE W CC      '.
               05  FILLER   PIC X(57)  VALUE
           '379 006776 023       027  Y    G.I. HEMORRHAGE W/O CC/MCC'.
               05  FILLER   PIC X(57)  VALUE
           '380 019265 054       070  Y    COMPLICATED PEPTIC ULCER W'.
               05  FILLER   PIC X(57)  VALUE
           '381 010875 036       044  Y    COMPLICATED PEPTIC ULCER W'.
               05  FILLER   PIC X(57)  VALUE
           '382 007591 027       032  Y    COMPLICATED PEPTIC ULCER W'.
               05  FILLER   PIC X(57)  VALUE
           '383 013215 042       053       UNCOMPLICATED PEPTIC ULCER'.
               05  FILLER   PIC X(57)  VALUE
           '384 008510 029       034       UNCOMPLICATED PEPTIC ULCER'.
               05  FILLER   PIC X(57)  VALUE
           '385 017649 059       078       INFLAMMATORY BOWEL DISEASE'.
               05  FILLER   PIC X(57)  VALUE
           '386 010040 039       049       INFLAMMATORY BOWEL DISEASE'.
               05  FILLER   PIC X(57)  VALUE
           '387 007449 030       036       INFLAMMATORY BOWEL DISEASE'.
               05  FILLER   PIC X(57)  VALUE
           '388 016100 053       069  Y    G.I. OBSTRUCTION W MCC    '.
               05  FILLER   PIC X(57)  VALUE
           '389 008717 036       044  Y    G.I. OBSTRUCTION W CC     '.
               05  FILLER   PIC X(57)  VALUE
           '390 006034 027       031  Y    G.I. OBSTRUCTION W/O CC/MC'.
               05  FILLER   PIC X(57)  VALUE
           '391 011976 038       051       ESOPHAGITIS, GASTROENT & M'.
               05  FILLER   PIC X(57)  VALUE
           '392 007388 028       035       ESOPHAGITIS, GASTROENT & M'.
               05  FILLER   PIC X(57)  VALUE
           '393 016893 047       066       OTHER DIGESTIVE SYSTEM DIA'.
               05  FILLER   PIC X(57)  VALUE
           '394 009448 034       043       OTHER DIGESTIVE SYSTEM DIA'.
               05  FILLER   PIC X(57)  VALUE
           '395 006574 024       030       OTHER DIGESTIVE SYSTEM DIA'.
               05  FILLER   PIC X(57)  VALUE
           '405 055387 108       144  Y    PANCREAS, LIVER & SHUNT PR'.
               05  FILLER   PIC X(57)  VALUE
           '406 028067 060       075  Y    PANCREAS, LIVER & SHUNT PR'.
               05  FILLER   PIC X(57)  VALUE
           '407 019472 040       048  Y    PANCREAS, LIVER & SHUNT PR'.
               05  FILLER   PIC X(57)  VALUE
           '408 038967 100       123       BILIARY TRACT PROC EXCEPT '.
               05  FILLER   PIC X(57)  VALUE
           '409 022218 063       075       BILIARY TRACT PROC EXCEPT '.
               05  FILLER   PIC X(57)  VALUE
           '410 016205 045       052       BILIARY TRACT PROC EXCEPT '.
               05  FILLER   PIC X(57)  VALUE
           '411 036261 095       117       CHOLECYSTECTOMY W C.D.E. W'.
               05  FILLER   PIC X(57)  VALUE
           '412 024166 063       074       CHOLECYSTECTOMY W C.D.E. W'.
               05  FILLER   PIC X(57)  VALUE
           '413 017708 042       051       CHOLECYSTECTOMY W C.D.E. W'.
               05  FILLER   PIC X(57)  VALUE
           '414 035545 087       106  Y    CHOLECYSTECTOMY EXCEPT BY '.
               05  FILLER   PIC X(57)  VALUE
           '415 020267 056       065  Y    CHOLECYSTECTOMY EXCEPT BY '.
               05  FILLER   PIC X(57)  VALUE
           '416 013465 036       042  Y    CHOLECYSTECTOMY EXCEPT BY '.
               05  FILLER   PIC X(57)  VALUE
           '417 024353 059       074       LAPAROSCOPIC CHOLECYSTECTO'.
               05  FILLER   PIC X(57)  VALUE
           '418 016600 040       048       LAPAROSCOPIC CHOLECYSTECTO'.
               05  FILLER   PIC X(57)  VALUE
           '419 012316 026       030       LAPAROSCOPIC CHOLECYSTECTO'.
               05  FILLER   PIC X(57)  VALUE
           '420 034621 081       114       HEPATOBILIARY DIAGNOSTIC P'.
               05  FILLER   PIC X(57)  VALUE
           '421 017699 042       056       HEPATOBILIARY DIAGNOSTIC P'.
               05  FILLER   PIC X(57)  VALUE
           '422 012352 027       033       HEPATOBILIARY DIAGNOSTIC P'.
               05  FILLER   PIC X(57)  VALUE
           '423 041961 098       132       OTHER HEPATOBILIARY OR PAN'.
               05  FILLER   PIC X(57)  VALUE
           '424 022993 062       079       OTHER HEPATOBILIARY OR PAN'.
               05  FILLER   PIC X(57)  VALUE
           '425 013793 037       044       OTHER HEPATOBILIARY OR PAN'.
               05  FILLER   PIC X(57)  VALUE
           '432 016710 047       062       CIRRHOSIS & ALCOHOLIC HEPA'.
               05  FILLER   PIC X(57)  VALUE
           '433 009173 033       041       CIRRHOSIS & ALCOHOLIC HEPA'.
               05  FILLER   PIC X(57)  VALUE
           '434 006229 024       029       CIRRHOSIS & ALCOHOLIC HEPA'.
               05  FILLER   PIC X(57)  VALUE
           '435 017524 052       068       MALIGNANCY OF HEPATOBILIAR'.
               05  FILLER   PIC X(57)  VALUE
           '436 011640 039       049       MALIGNANCY OF HEPATOBILIAR'.
               05  FILLER   PIC X(57)  VALUE
           '437 008971 027       035       MALIGNANCY OF HEPATOBILIAR'.
               05  FILLER   PIC X(57)  VALUE
           '438 017023 051       068       DISORDERS OF PANCREAS EXCE'.
               05  FILLER   PIC X(57)  VALUE
           '439 009071 036       044       DISORDERS OF PANCREAS EXCE'.
               05  FILLER   PIC X(57)  VALUE
           '440 006423 027       032       DISORDERS OF PANCREAS EXCE'.
               05  FILLER   PIC X(57)  VALUE
           '441 018835 050       068  Y    DISORDERS OF LIVER EXCEPT '.
               05  FILLER   PIC X(57)  VALUE
           '442 009266 033       042  Y    DISORDERS OF LIVER EXCEPT '.
               05  FILLER   PIC X(57)  VALUE
           '443 006512 025       031  Y    DISORDERS OF LIVER EXCEPT '.
               05  FILLER   PIC X(57)  VALUE
           '444 016212 047       061       DISORDERS OF THE BILIARY T'.
               05  FILLER   PIC X(57)  VALUE
           '445 010654 034       042       DISORDERS OF THE BILIARY T'.
               05  FILLER   PIC X(57)  VALUE
           '446 007569 024       029       DISORDERS OF THE BILIARY T'.
               05  FILLER   PIC X(57)  VALUE
           '453 111637 095       117       COMBINED ANTERIOR/POSTERIO'.
               05  FILLER   PIC X(57)  VALUE
           '454 080184 049       058       COMBINED ANTERIOR/POSTERIO'.
               05  FILLER   PIC X(57)  VALUE
           '455 062503 031       035       COMBINED ANTERIOR/POSTERIO'.
               05  FILLER   PIC X(57)  VALUE
           '456 094039 099       120       SPINAL FUS EXC CERV W SPIN'.
               05  FILLER   PIC X(57)  VALUE
           '457 069074 055       065       SPINAL FUS EXC CERV W SPIN'.
               05  FILLER   PIC X(57)  VALUE
           '458 052637 034       038       SPINAL FUS EXC CERV W SPIN'.
               05  FILLER   PIC X(57)  VALUE
           '459 066686 068       085  Y    SPINAL FUSION EXCEPT CERVI'.
               05  FILLER   PIC X(57)  VALUE
           '460 039998 030       035  Y    SPINAL FUSION EXCEPT CERVI'.
               05  FILLER   PIC X(57)  VALUE
           '461 050202 063       076       BILATERAL OR MULTIPLE MAJO'.
               05  FILLER   PIC X(57)  VALUE
           '462 034905 034       037       BILATERAL OR MULTIPLE MAJO'.
               05  FILLER   PIC X(57)  VALUE
           '463 053345 107       142  Y    WND DEBRID & SKN GRFT EXC '.
               05  FILLER   PIC X(57)  VALUE
           '464 030085 063       078  Y    WND DEBRID & SKN GRFT EXC '.
               05  FILLER   PIC X(57)  VALUE
           '465 019463 039       048  Y    WND DEBRID & SKN GRFT EXC '.
               05  FILLER   PIC X(57)  VALUE
           '466 051513 067       082  Y    REVISION OF HIP OR KNEE RE'.
               05  FILLER   PIC X(57)  VALUE
           '467 034231 038       043  Y    REVISION OF HIP OR KNEE RE'.
               05  FILLER   PIC X(57)  VALUE
           '468 027652 029       031  Y    REVISION OF HIP OR KNEE RE'.
               05  FILLER   PIC X(57)  VALUE
           '469 033905 061       072  Y    MAJOR JOINT REPLACEMENT OR'.
               05  FILLER   PIC X(57)  VALUE
           '470 021137 030       032  Y    MAJOR JOINT REPLACEMENT OR'.
               05  FILLER   PIC X(57)  VALUE
           '471 048737 063       087       CERVICAL SPINAL FUSION W M'.
               05  FILLER   PIC X(57)  VALUE
           '472 029166 024       034       CERVICAL SPINAL FUSION W C'.
               05  FILLER   PIC X(57)  VALUE
           '473 022655 015       018       CERVICAL SPINAL FUSION W/O'.
               05  FILLER   PIC X(57)  VALUE
           '474 035943 086       109  Y    AMPUTATION FOR MUSCULOSKEL'.
               05  FILLER   PIC X(57)  VALUE
           '475 020504 058       072  Y    AMPUTATION FOR MUSCULOSKEL'.
               05  FILLER   PIC X(57)  VALUE
           '476 011187 031       039  Y    AMPUTATION FOR MUSCULOSKEL'.
               05  FILLER   PIC X(57)  VALUE
           '477 031638 085       104  Y Y  BIOPSIES OF MUSCULOSKELETA'.
               05  FILLER   PIC X(57)  VALUE
           '478 022441 055       067  Y Y  BIOPSIES OF MUSCULOSKELETA'.
               05  FILLER   PIC X(57)  VALUE
           '479 017312 034       042  Y Y  BIOPSIES OF MUSCULOSKELETA'.
               05  FILLER   PIC X(57)  VALUE
           '480 030052 069       080  Y Y  HIP & FEMUR PROCEDURES EXC'.
               05  FILLER   PIC X(57)  VALUE
           '481 019776 047       051  Y Y  HIP & FEMUR PROCEDURES EXC'.
               05  FILLER   PIC X(57)  VALUE
           '482 016243 038       041  Y Y  HIP & FEMUR PROCEDURES EXC'.
               05  FILLER   PIC X(57)  VALUE
           '483 024205 020       024       MAJOR JOINT/LIMB REATTACHM'.
               05  FILLER   PIC X(57)  VALUE
           '485 030949 080       095       KNEE PROCEDURES W PDX OF I'.
               05  FILLER   PIC X(57)  VALUE
           '486 020656 055       063       KNEE PROCEDURES W PDX OF I'.
               05  FILLER   PIC X(57)  VALUE
           '487 015630 040       047       KNEE PROCEDURES W PDX OF I'.
               05  FILLER   PIC X(57)  VALUE
           '488 017225 035       042  Y    KNEE PROCEDURES W/O PDX OF'.
               05  FILLER   PIC X(57)  VALUE
           '489 013186 024       027  Y    KNEE PROCEDURES W/O PDX OF'.
               05  FILLER   PIC X(57)  VALUE
           '492 031873 065       080  Y Y  LOWER EXTREM & HUMER PROC '.
               05  FILLER   PIC X(57)  VALUE
           '493 020354 040       047  Y Y  LOWER EXTREM & HUMER PROC '.
               05  FILLER   PIC X(57)  VALUE
           '494 015397 027       032  Y Y  LOWER EXTREM & HUMER PROC '.
               05  FILLER   PIC X(57)  VALUE
           '495 030476 073       096  Y Y  LOCAL EXCISION & REMOVAL I'.
               05  FILLER   PIC X(57)  VALUE
           '496 017289 041       051  Y Y  LOCAL EXCISION & REMOVAL I'.
               05  FILLER   PIC X(57)  VALUE
           '497 012230 021       027  Y Y  LOCAL EXCISION & REMOVAL I'.
               05  FILLER   PIC X(57)  VALUE
           '498 021416 055       072       LOCAL EXCISION & REMOVAL I'.
               05  FILLER   PIC X(57)  VALUE
           '499 010753 023       029       LOCAL EXCISION & REMOVAL I'.
               05  FILLER   PIC X(57)  VALUE
           '500 032420 077       102  Y Y  SOFT TISSUE PROCEDURES W M'.
               05  FILLER   PIC X(57)  VALUE
           '501 016474 044       056  Y Y  SOFT TISSUE PROCEDURES W C'.
               05  FILLER   PIC X(57)  VALUE
           '502 011597 025       030  Y Y  SOFT TISSUE PROCEDURES W/O'.
               05  FILLER   PIC X(57)  VALUE
           '503 023338 069       085       FOOT PROCEDURES W MCC     '.
               05  FILLER   PIC X(57)  VALUE
           '504 015691 050       060       FOOT PROCEDURES W CC      '.
               05  FILLER   PIC X(57)  VALUE
           '505 012474 029       035       FOOT PROCEDURES W/O CC/MCC'.
               05  FILLER   PIC X(57)  VALUE
           '506 012881 034       043       MAJOR THUMB OR JOINT PROCE'.
               05  FILLER   PIC X(57)  VALUE
           '507 019154 043       052       MAJOR SHOULDER OR ELBOW JO'.
               05  FILLER   PIC X(57)  VALUE
           '508 015198 021       026       MAJOR SHOULDER OR ELBOW JO'.
               05  FILLER   PIC X(57)  VALUE
           '509 015494 036       045       ARTHROSCOPY               '.
               05  FILLER   PIC X(57)  VALUE
           '510 022857 048       058  Y    SHOULDER,ELBOW OR FOREARM '.
               05  FILLER   PIC X(57)  VALUE
           '511 016509 033       039  Y    SHOULDER,ELBOW OR FOREARM '.
               05  FILLER   PIC X(57)  VALUE
           '512 012963 020       024  Y    SHOULDER,ELBOW OR FOREARM '.
               05  FILLER   PIC X(57)  VALUE
           '513 014462 036       048       HAND OR WRIST PROC, EXCEPT'.
               05  FILLER   PIC X(57)  VALUE
           '514 008996 022       027       HAND OR WRIST PROC, EXCEPT'.
               05  FILLER   PIC X(57)  VALUE
           '515 032235 072       092  Y Y  OTHER MUSCULOSKELET SYS & '.
               05  FILLER   PIC X(57)  VALUE
           '516 020434 044       054  Y Y  OTHER MUSCULOSKELET SYS & '.
               05  FILLER   PIC X(57)  VALUE
           '517 017251 027       033  Y Y  OTHER MUSCULOSKELET SYS & '.
               05  FILLER   PIC X(57)  VALUE
           '518 030628 044       066  Y Y  BACK & NECK PROC EXC SPINA'.
               05  FILLER   PIC X(57)  VALUE
           '519 016468 031       040  Y Y  BACK & NECK PROC EXC SPINA'.
               05  FILLER   PIC X(57)  VALUE
           '520 011396 019       022  Y Y  BACK & NECK PROC EXC SPINA'.
               05  FILLER   PIC X(57)  VALUE
           '533 014495 044       057  Y    FRACTURES OF FEMUR W MCC  '.
               05  FILLER   PIC X(57)  VALUE
           '534 007594 029       035  Y    FRACTURES OF FEMUR W/O MCC'.
               05  FILLER   PIC X(57)  VALUE
           '535 012410 041       052  Y    FRACTURES OF HIP & PELVIS '.
               05  FILLER   PIC X(57)  VALUE
           '536 007201 030       035  Y    FRACTURES OF HIP & PELVIS '.
               05  FILLER   PIC X(57)  VALUE
           '537 008975 033       041       SPRAINS, STRAINS, & DISLOC'.
               05  FILLER   PIC X(57)  VALUE
           '538 006917 026       031       SPRAINS, STRAINS, & DISLOC'.
               05  FILLER   PIC X(57)  VALUE
           '539 018276 061       081  Y    OSTEOMYELITIS W MCC       '.
               05  FILLER   PIC X(57)  VALUE
           '540 012967 047       059  Y    OSTEOMYELITIS W CC        '.
               05  FILLER   PIC X(57)  VALUE
           '541 009218 035       045  Y    OSTEOMYELITIS W/O CC/MCC  '.
               05  FILLER   PIC X(57)  VALUE
           '542 019472 060       078  Y    PATHOLOGICAL FRACTURES & M'.
               05  FILLER   PIC X(57)  VALUE
           '543 011227 041       051  Y    PATHOLOGICAL FRACTURES & M'.
               05  FILLER   PIC X(57)  VALUE
           '544 007936 031       037  Y    PATHOLOGICAL FRACTURES & M'.
               05  FILLER   PIC X(57)  VALUE
           '545 025341 061       085  Y    CONNECTIVE TISSUE DISORDER'.
               05  FILLER   PIC X(57)  VALUE
           '546 011711 039       050  Y    CONNECTIVE TISSUE DISORDER'.
               05  FILLER   PIC X(57)  VALUE
           '547 007985 028       035  Y    CONNECTIVE TISSUE DISORDER'.
               05  FILLER   PIC X(57)  VALUE
           '548 019123 061       078       SEPTIC ARTHRITIS W MCC    '.
               05  FILLER   PIC X(57)  VALUE
           '549 011514 044       055       SEPTIC ARTHRITIS W CC     '.
               05  FILLER   PIC X(57)  VALUE
           '550 008390 032       039       SEPTIC ARTHRITIS W/O CC/MC'.
               05  FILLER   PIC X(57)  VALUE
           '551 015556 048       062  Y    MEDICAL BACK PROBLEMS W MC'.
               05  FILLER   PIC X(57)  VALUE
           '552 008698 032       038  Y    MEDICAL BACK PROBLEMS W/O '.
               05  FILLER   PIC X(57)  VALUE
           '553 012187 042       054       BONE DISEASES & ARTHROPATH'.
               05  FILLER   PIC X(57)  VALUE
           '554 007274 029       035       BONE DISEASES & ARTHROPATH'.
               05  FILLER   PIC X(57)  VALUE
           '555 012636 039       051       SIGNS & SYMPTOMS OF MUSCUL'.
               05  FILLER   PIC X(57)  VALUE
           '556 007183 026       032       SIGNS & SYMPTOMS OF MUSCUL'.
               05  FILLER   PIC X(57)  VALUE
           '557 014269 050       062  Y    TENDONITIS, MYOSITIS & BUR'.
               05  FILLER   PIC X(57)  VALUE
           '558 008522 034       040  Y    TENDONITIS, MYOSITIS & BUR'.
               05  FILLER   PIC X(57)  VALUE
           '559 018555 050       068  Y    AFTERCARE, MUSCULOSKELETAL'.
               05  FILLER   PIC X(57)  VALUE
           '560 010756 035       044  Y    AFTERCARE, MUSCULOSKELETAL'.
               05  FILLER   PIC X(57)  VALUE
           '561 006688 021       025  Y    AFTERCARE, MUSCULOSKELETAL'.
               05  FILLER   PIC X(57)  VALUE
           '562 013706 043       055  Y    FX, SPRN, STRN & DISL EXCE'.
               05  FILLER   PIC X(57)  VALUE
           '563 007756 030       035  Y    FX, SPRN, STRN & DISL EXCE'.
               05  FILLER   PIC X(57)  VALUE
           '564 015036 048       062       OTHER MUSCULOSKELETAL SYS '.
               05  FILLER   PIC X(57)  VALUE
           '565 009398 035       044       OTHER MUSCULOSKELETAL SYS '.
               05  FILLER   PIC X(57)  VALUE
           '566 006871 025       031       OTHER MUSCULOSKELETAL SYS '.
               05  FILLER   PIC X(57)  VALUE
           '570 023952 072       093  Y    SKIN DEBRIDEMENT W MCC    '.
               05  FILLER   PIC X(57)  VALUE
           '571 014664 052       063  Y    SKIN DEBRIDEMENT W CC     '.
               05  FILLER   PIC X(57)  VALUE
           '572 009919 037       044  Y    SKIN DEBRIDEMENT W/O CC/MC'.
               05  FILLER   PIC X(57)  VALUE
           '573 037073 087       129  Y    SKIN GRAFT FOR SKIN ULCER '.
               05  FILLER   PIC X(57)  VALUE
           '574 026298 071       093  Y    SKIN GRAFT FOR SKIN ULCER '.
               05  FILLER   PIC X(57)  VALUE
           '575 014926 045       056  Y    SKIN GRAFT FOR SKIN ULCER '.
               05  FILLER   PIC X(57)  VALUE
           '576 041423 081       116       SKIN GRAFT EXC FOR SKIN UL'.
               05  FILLER   PIC X(57)  VALUE
           '577 019812 040       056       SKIN GRAFT EXC FOR SKIN UL'.
               05  FILLER   PIC X(57)  VALUE
           '578 013162 026       035       SKIN GRAFT EXC FOR SKIN UL'.
               05  FILLER   PIC X(57)  VALUE
           '579 027263 071       092  Y    OTHER SKIN, SUBCUT TISS & '.
               05  FILLER   PIC X(57)  VALUE
           '580 015727 041       054  Y    OTHER SKIN, SUBCUT TISS & '.
               05  FILLER   PIC X(57)  VALUE
           '581 011338 022       028  Y    OTHER SKIN, SUBCUT TISS & '.
               05  FILLER   PIC X(57)  VALUE
           '582 013003 022       029       MASTECTOMY FOR MALIGNANCY '.
               05  FILLER   PIC X(57)  VALUE
           '583 010932 017       019       MASTECTOMY FOR MALIGNANCY '.
               05  FILLER   PIC X(57)  VALUE
           '584 017682 039       052       BREAST BIOPSY, LOCAL EXCIS'.
               05  FILLER   PIC X(57)  VALUE
           '585 013752 021       025       BREAST BIOPSY, LOCAL EXCIS'.
               05  FILLER   PIC X(57)  VALUE
           '592 014249 051       066  Y    SKIN ULCERS W MCC         '.
               05  FILLER   PIC X(57)  VALUE
           '593 010196 043       052  Y    SKIN ULCERS W CC          '.
               05  FILLER   PIC X(57)  VALUE
           '594 007124 031       039  Y    SKIN ULCERS W/O CC/MCC    '.
               05  FILLER   PIC X(57)  VALUE
           '595 019629 055       075       MAJOR SKIN DISORDERS W MCC'.
               05  FILLER   PIC X(57)  VALUE
           '596 009527 036       046       MAJOR SKIN DISORDERS W/O M'.
               05  FILLER   PIC X(57)  VALUE
           '597 016758 054       071       MALIGNANT BREAST DISORDERS'.
               05  FILLER   PIC X(57)  VALUE
           '598 011235 038       052       MALIGNANT BREAST DISORDERS'.
               05  FILLER   PIC X(57)  VALUE
           '599 007259 024       031       MALIGNANT BREAST DISORDERS'.
               05  FILLER   PIC X(57)  VALUE
           '600 009803 038       047       NON-MALIGNANT BREAST DISOR'.
               05  FILLER   PIC X(57)  VALUE
           '601 006314 027       033       NON-MALIGNANT BREAST DISOR'.
               05  FILLER   PIC X(57)  VALUE
           '602 014557 050       062  Y    CELLULITIS W MCC          '.
               05  FILLER   PIC X(57)  VALUE
           '603 008447 035       042  Y    CELLULITIS W/O MCC        '.
               05  FILLER   PIC X(57)  VALUE
           '604 012624 039       051       TRAUMA TO THE SKIN, SUBCUT'.
               05  FILLER   PIC X(57)  VALUE
           '605 007664 026       032       TRAUMA TO THE SKIN, SUBCUT'.
               05  FILLER   PIC X(57)  VALUE
           '606 013664 042       058       MINOR SKIN DISORDERS W MCC'.
               05  FILLER   PIC X(57)  VALUE
           '607 007296 028       036       MINOR SKIN DISORDERS W/O M'.
               05  FILLER   PIC X(57)  VALUE
           '614 024642 041       056       ADRENAL & PITUITARY PROCED'.
               05  FILLER   PIC X(57)  VALUE
           '615 014243 022       026       ADRENAL & PITUITARY PROCED'.
               05  FILLER   PIC X(57)  VALUE
           '616 041611 107       131  Y    AMPUTAT OF LOWER LIMB FOR '.
               05  FILLER   PIC X(57)  VALUE
           '617 019956 061       072  Y    AMPUTAT OF LOWER LIMB FOR '.
               05  FILLER   PIC X(57)  VALUE
           '618 013512 042       050  Y    AMPUTAT OF LOWER LIMB FOR '.
               05  FILLER   PIC X(57)  VALUE
           '619 032890 042       065       O.R. PROCEDURES FOR OBESIT'.
               05  FILLER   PIC X(57)  VALUE
           '620 018470 026       031       O.R. PROCEDURES FOR OBESIT'.
               05  FILLER   PIC X(57)  VALUE
           '621 015434 018       020       O.R. PROCEDURES FOR OBESIT'.
               05  FILLER   PIC X(57)  VALUE
           '622 038047 092       126  Y    SKIN GRAFTS & WOUND DEBRID'.
               05  FILLER   PIC X(57)  VALUE
           '623 018308 057       069  Y    SKIN GRAFTS & WOUND DEBRID'.
               05  FILLER   PIC X(57)  VALUE
           '624 011314 036       044  Y    SKIN GRAFTS & WOUND DEBRID'.
               05  FILLER   PIC X(57)  VALUE
           '625 024896 046       071       THYROID, PARATHYROID & THY'.
               05  FILLER   PIC X(57)  VALUE
           '626 013080 021       031       THYROID, PARATHYROID & THY'.
               05  FILLER   PIC X(57)  VALUE
           '627 008663 013       015       THYROID, PARATHYROID & THY'.
               05  FILLER   PIC X(57)  VALUE
           '628 032935 067       094  Y    OTHER ENDOCRINE, NUTRIT & '.
               05  FILLER   PIC X(57)  VALUE
           '629 022471 062       075  Y    OTHER ENDOCRINE, NUTRIT & '.
               05  FILLER   PIC X(57)  VALUE
           '630 014305 032       042  Y    OTHER ENDOCRINE, NUTRIT & '.
               05  FILLER   PIC X(57)  VALUE
           '637 013944 042       055  Y    DIABETES W MCC            '.
               05  FILLER   PIC X(57)  VALUE
           '638 008261 030       037  Y    DIABETES W CC             '.
               05  FILLER   PIC X(57)  VALUE
           '639 006068 022       027  Y    DIABETES W/O CC/MCC       '.
               05  FILLER   PIC X(57)  VALUE
           '640 011044 033       045  Y    MISC DISORDERS OF NUTRITIO'.
               05  FILLER   PIC X(57)  VALUE
           '641 007051 027       034  Y    MISC DISORDERS OF NUTRITIO'.
               05  FILLER   PIC X(57)  VALUE
           '642 011429 033       046       INBORN AND OTHER DISORDERS'.
               05  FILLER   PIC X(57)  VALUE
           '643 016460 054       068  Y    ENDOCRINE DISORDERS W MCC '.
               05  FILLER   PIC X(57)  VALUE
           '644 010199 038       047  Y    ENDOCRINE DISORDERS W CC  '.
               05  FILLER   PIC X(57)  VALUE
           '645 007180 028       034  Y    ENDOCRINE DISORDERS W/O CC'.
               05  FILLER   PIC X(57)  VALUE
           '652 031502 057       066       KIDNEY TRANSPLANT         '.
               05  FILLER   PIC X(57)  VALUE
           '653 057958 122       151  Y    MAJOR BLADDER PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '654 030973 077       087  Y    MAJOR BLADDER PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '655 022590 049       055  Y    MAJOR BLADDER PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '656 034517 069       090       KIDNEY & URETER PROCEDURES'.
               05  FILLER   PIC X(57)  VALUE
           '657 020111 044       052       KIDNEY & URETER PROCEDURES'.
               05  FILLER   PIC X(57)  VALUE
           '658 015299 027       030       KIDNEY & URETER PROCEDURES'.
               05  FILLER   PIC X(57)  VALUE
           '659 033813 075       100  Y    KIDNEY & URETER PROCEDURES'.
               05  FILLER   PIC X(57)  VALUE
           '660 018888 042       055  Y    KIDNEY & URETER PROCEDURES'.
               05  FILLER   PIC X(57)  VALUE
           '661 013494 023       027  Y    KIDNEY & URETER PROCEDURES'.
               05  FILLER   PIC X(57)  VALUE
           '662 030042 078       103       MINOR BLADDER PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '663 015285 039       052       MINOR BLADDER PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '664 012406 019       024       MINOR BLADDER PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '665 031585 091       117       PROSTATECTOMY W MCC       '.
               05  FILLER   PIC X(57)  VALUE
           '666 017512 047       063       PROSTATECTOMY W CC        '.
               05  FILLER   PIC X(57)  VALUE
           '667 009690 022       028       PROSTATECTOMY W/O CC/MCC  '.
               05  FILLER   PIC X(57)  VALUE
           '668 024989 065       086       TRANSURETHRAL PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '669 012662 030       040       TRANSURETHRAL PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '670 008957 020       025       TRANSURETHRAL PROCEDURES W'.
               05  FILLER   PIC X(57)  VALUE
           '671 016170 041       057       URETHRAL PROCEDURES W CC/M'.
               05  FILLER   PIC X(57)  VALUE
           '672 008496 018       022       URETHRAL PROCEDURES W/O CC'.
               05  FILLER   PIC X(57)  VALUE
           '673 035023 074       106       OTHER KIDNEY & URINARY TRA'.
               05  FILLER   PIC X(57)  VALUE
           '674 022600 053       070       OTHER KIDNEY & URINARY TRA'.
               05  FILLER   PIC X(57)  VALUE
           '675 014443 022       029       OTHER KIDNEY & URINARY TRA'.
               05  FILLER   PIC X(57)  VALUE
           '682 015194 046       061  Y    RENAL FAILURE W MCC       '.
               05  FILLER   PIC X(57)  VALUE
           '683 009512 036       044  Y    RENAL FAILURE W CC        '.
               05  FILLER   PIC X(57)  VALUE
           '684 006085 024       029  Y    RENAL FAILURE W/O CC/MCC  '.
               05  FILLER   PIC X(57)  VALUE
           '685 010025 027       037       ADMIT FOR RENAL DIALYSIS  '.
               05  FILLER   PIC X(57)  VALUE
           '686 017637 053       071       KIDNEY & URINARY TRACT NEO'.
               05  FILLER   PIC X(57)  VALUE
           '687 010054 036       046       KIDNEY & URINARY TRACT NEO'.
               05  FILLER   PIC X(57)  VALUE
           '688 006911 021       027       KIDNEY & URINARY TRACT NEO'.
               05  FILLER   PIC X(57)  VALUE
           '689 011172 042       052  Y    KIDNEY & URINARY TRACT INF'.
               05  FILLER   PIC X(57)  VALUE
           '690 007794 032       038  Y    KIDNEY & URINARY TRACT INF'.
               05  FILLER   PIC X(57)  VALUE
           '691 016238 029       038       URINARY STONES W ESW LITHO'.
               05  FILLER   PIC X(57)  VALUE
           '692 011286 018       022       URINARY STONES W ESW LITHO'.
               05  FILLER   PIC X(57)  VALUE
           '693 013433 039       051       URINARY STONES W/O ESW LIT'.
               05  FILLER   PIC X(57)  VALUE
           '694 006859 019       024       URINARY STONES W/O ESW LIT'.
               05  FILLER   PIC X(57)  VALUE
           '695 012450 041       053       KIDNEY & URINARY TRACT SIG'.
               05  FILLER   PIC X(57)  VALUE
           '696 006619 025       031       KIDNEY & URINARY TRACT SIG'.
               05  FILLER   PIC X(57)  VALUE
           '697 009229 026       034       URETHRAL STRICTURE        '.
               05  FILLER   PIC X(57)  VALUE
           '698 015625 051       064  Y    OTHER KIDNEY & URINARY TRA'.
               05  FILLER   PIC X(57)  VALUE
           '699 010170 035       044  Y    OTHER KIDNEY & URINARY TRA'.
               05  FILLER   PIC X(57)  VALUE
           '700 007110 026       032  Y    OTHER KIDNEY & URINARY TRA'.
               05  FILLER   PIC X(57)  VALUE
           '707 018155 029       039       MAJOR MALE PELVIC PROCEDUR'.
               05  FILLER   PIC X(57)  VALUE
           '708 013262 014       016       MAJOR MALE PELVIC PROCEDUR'.
               05  FILLER   PIC X(57)  VALUE
           '709 021341 043       070       PENIS PROCEDURES W CC/MCC '.
               05  FILLER   PIC X(57)  VALUE
           '710 012949 017       021       PENIS PROCEDURES W/O CC/MC'.
               05  FILLER   PIC X(57)  VALUE
           '711 022547 058       081       TESTES PROCEDURES W CC/MCC'.
               05  FILLER   PIC X(57)  VALUE
           '712 010632 026       037       TESTES PROCEDURES W/O CC/M'.
               05  FILLER   PIC X(57)  VALUE
           '713 014828 035       049       TRANSURETHRAL PROSTATECTOM'.
               05  FILLER   PIC X(57)  VALUE
           '714 007933 018       021       TRANSURETHRAL PROSTATECTOM'.
               05  FILLER   PIC X(57)  VALUE
           '715 020051 048       066       OTHER MALE REPRODUCTIVE SY'.
               05  FILLER   PIC X(57)  VALUE
           '716 011857 015       018       OTHER MALE REPRODUCTIVE SY'.
               05  FILLER   PIC X(57)  VALUE
           '717 017717 046       065       OTHER MALE REPRODUCTIVE SY'.
               05  FILLER   PIC X(57)  VALUE
           '718 010099 024       030       OTHER MALE REPRODUCTIVE SY'.
               05  FILLER   PIC X(57)  VALUE
           '722 017569 056       073       MALIGNANCY, MALE REPRODUCT'.
               05  FILLER   PIC X(57)  VALUE
           '723 010635 038       049       MALIGNANCY, MALE REPRODUCT'.
               05  FILLER   PIC X(57)  VALUE
           '724 006412 023       030       MALIGNANCY, MALE REPRODUCT'.
               05  FILLER   PIC X(57)  VALUE
           '725 011790 043       056       BENIGN PROSTATIC HYPERTROP'.
               05  FILLER   PIC X(57)  VALUE
           '726 007029 027       033       BENIGN PROSTATIC HYPERTROP'.
               05  FILLER   PIC X(57)  VALUE
           '727 014195 049       062       INFLAMMATION OF THE MALE R'.
               05  FILLER   PIC X(57)  VALUE
           '728 008006 032       039       INFLAMMATION OF THE MALE R'.
               05  FILLER   PIC X(57)  VALUE
           '729 010390 034       045       OTHER MALE REPRODUCTIVE SY'.
               05  FILLER   PIC X(57)  VALUE
           '730 006660 023       030       OTHER MALE REPRODUCTIVE SY'.
               05  FILLER   PIC X(57)  VALUE
           '734 025704 046       066       PELVIC EVISCERATION, RAD H'.
               05  FILLER   PIC X(57)  VALUE
           '735 012265 019       023       PELVIC EVISCERATION, RAD H'.
               05  FILLER   PIC X(57)  VALUE
           '736 044341 102       127       UTERINE & ADNEXA PROC FOR '.
               05  FILLER   PIC X(57)  VALUE
           '737 020198 052       061       UTERINE & ADNEXA PROC FOR '.
               05  FILLER   PIC X(57)  VALUE
           '738 012746 029       034       UTERINE & ADNEXA PROC FOR '.
               05  FILLER   PIC X(57)  VALUE
           '739 033133 067       093       UTERINE,ADNEXA PROC FOR NO'.
               05  FILLER   PIC X(57)  VALUE
           '740 016069 032       041       UTERINE,ADNEXA PROC FOR NO'.
               05  FILLER   PIC X(57)  VALUE
           '741 011846 018       021       UTERINE,ADNEXA PROC FOR NO'.
               05  FILLER   PIC X(57)  VALUE
           '742 015436 031       041       UTERINE & ADNEXA PROC FOR '.
               05  FILLER   PIC X(57)  VALUE
           '743 009954 018       020       UTERINE & ADNEXA PROC FOR '.
               05  FILLER   PIC X(57)  VALUE
           '744 017484 043       060       D&C, CONIZATION, LAPAROSCO'.
               05  FILLER   PIC X(57)  VALUE
           '745 009553 020       024       D&C, CONIZATION, LAPAROSCO'.
               05  FILLER   PIC X(57)  VALUE
           '746 014219 033       047       VAGINA, CERVIX & VULVA PRO'.
               05  FILLER   PIC X(57)  VALUE
           '747 008892 016       019       VAGINA, CERVIX & VULVA PRO'.
               05  FILLER   PIC X(57)  VALUE
           '748 010845 016       020       FEMALE REPRODUCTIVE SYSTEM'.
               05  FILLER   PIC X(57)  VALUE
           '749 025541 061       083       OTHER FEMALE REPRODUCTIVE '.
               05  FILLER   PIC X(57)  VALUE
           '750 011904 023       028       OTHER FEMALE REPRODUCTIVE '.
               05  FILLER   PIC X(57)  VALUE
           '754 019626 059       083       MALIGNANCY, FEMALE REPRODU'.
               05  FILLER   PIC X(57)  VALUE
           '755 011066 037       049       MALIGNANCY, FEMALE REPRODU'.
               05  FILLER   PIC X(57)  VALUE
           '756 006652 022       030       MALIGNANCY, FEMALE REPRODU'.
               05  FILLER   PIC X(57)  VALUE
           '757 015397 057       072       INFECTIONS, FEMALE REPRODU'.
               05  FILLER   PIC X(57)  VALUE
           '758 010518 043       053       INFECTIONS, FEMALE REPRODU'.
               05  FILLER   PIC X(57)  VALUE
           '759 006931 031       038       INFECTIONS, FEMALE REPRODU'.
               05  FILLER   PIC X(57)  VALUE
           '760 008399 027       036       MENSTRUAL & OTHER FEMALE R'.
               05  FILLER   PIC X(57)  VALUE
           '761 004988 018       022       MENSTRUAL & OTHER FEMALE R'.
               05  FILLER   PIC X(57)  VALUE
           '765 010924 038       047       CESAREAN SECTION W CC/MCC '.
               05  FILLER   PIC X(57)  VALUE
           '766 007562 029       031       CESAREAN SECTION W/O CC/MC'.
               05  FILLER   PIC X(57)  VALUE
           '767 008513 024       029       VAGINAL DELIVERY W STERILI'.
               05  FILLER   PIC X(57)  VALUE
           '768 011184 047       058       VAGINAL DELIVERY W O.R. PR'.
               05  FILLER   PIC X(57)  VALUE
           '769 018930 036       058       POSTPARTUM & POST ABORTION'.
               05  FILLER   PIC X(57)  VALUE
           '770 008029 016       022       ABORTION W D&C, ASPIRATION'.
               05  FILLER   PIC X(57)  VALUE
           '774 007168 026       031       VAGINAL DELIVERY W COMPLIC'.
               05  FILLER   PIC X(57)  VALUE
           '775 005643 021       023       VAGINAL DELIVERY W/O COMPL'.
               05  FILLER   PIC X(57)  VALUE
           '776 008117 027       036       POSTPARTUM & POST ABORTION'.
               05  FILLER   PIC X(57)  VALUE
           '777 010145 018       022       ECTOPIC PREGNANCY         '.
               05  FILLER   PIC X(57)  VALUE
           '778 005638 021       033       THREATENED ABORTION       '.
               05  FILLER   PIC X(57)  VALUE
           '779 006389 015       020       ABORTION W/O D&C          '.
               05  FILLER   PIC X(57)  VALUE
           '780 002880 012       014       FALSE LABOR               '.
               05  FILLER   PIC X(57)  VALUE
           '781 007546 027       040       OTHER ANTEPARTUM DIAGNOSES'.
               05  FILLER   PIC X(57)  VALUE
           '782 004057 017       023       OTHER ANTEPARTUM DIAGNOSES'.
               05  FILLER   PIC X(57)  VALUE
           '789 015547 018       018       NEONATES, DIED OR TRANSFER'.
               05  FILLER   PIC X(57)  VALUE
           '790 051268 179       179       EXTREME IMMATURITY OR RESP'.
               05  FILLER   PIC X(57)  VALUE
           '791 035015 133       133       PREMATURITY W MAJOR PROBLE'.
               05  FILLER   PIC X(57)  VALUE
           '792 021127 086       086       PREMATURITY W/O MAJOR PROB'.
               05  FILLER   PIC X(57)  VALUE
           '793 035968 047       047       FULL TERM NEONATE W MAJOR '.
               05  FILLER   PIC X(57)  VALUE
           '794 012731 034       034       NEONATE W OTHER SIGNIFICAN'.
               05  FILLER   PIC X(57)  VALUE
           '795 001724 031       031       NORMAL NEWBORN            '.
               05  FILLER   PIC X(57)  VALUE
           '799 050131 095       122       SPLENECTOMY W MCC         '.
               05  FILLER   PIC X(57)  VALUE
           '800 026403 056       071       SPLENECTOMY W CC          '.
               05  FILLER   PIC X(57)  VALUE
           '801 015477 029       035       SPLENECTOMY W/O CC/MCC    '.
               05  FILLER   PIC X(57)  VALUE
           '802 033601 078       106       OTHER O.R. PROC OF THE BLO'.
               05  FILLER   PIC X(57)  VALUE
           '803 018440 045       060       OTHER O.R. PROC OF THE BLO'.
               05  FILLER   PIC X(57)  VALUE
           '804 012265 025       032       OTHER O.R. PROC OF THE BLO'.
               05  FILLER   PIC X(57)  VALUE
           '808 022260 061       080       MAJOR HEMATOL/IMMUN DIAG E'.
               05  FILLER   PIC X(57)  VALUE
           '809 012037 038       048       MAJOR HEMATOL/IMMUN DIAG E'.
               05  FILLER   PIC X(57)  VALUE
           '810 008409 026       033       MAJOR HEMATOL/IMMUN DIAG E'.
               05  FILLER   PIC X(57)  VALUE
           '811 012829 036       049       RED BLOOD CELL DISORDERS W'.
               05  FILLER   PIC X(57)  VALUE
           '812 008162 026       034       RED BLOOD CELL DISORDERS W'.
               05  FILLER   PIC X(57)  VALUE
           '813 016534 036       050       COAGULATION DISORDERS     '.
               05  FILLER   PIC X(57)  VALUE
           '814 017048 050       069       RETICULOENDOTHELIAL & IMMU'.
               05  FILLER   PIC X(57)  VALUE
           '815 009948 034       043       RETICULOENDOTHELIAL & IMMU'.
               05  FILLER   PIC X(57)  VALUE
           '816 006882 024       029       RETICULOENDOTHELIAL & IMMU'.
               05  FILLER   PIC X(57)  VALUE
           '820 055226 118       156       LYMPHOMA & LEUKEMIA W MAJO'.
               05  FILLER   PIC X(57)  VALUE
           '821 023064 046       064       LYMPHOMA & LEUKEMIA W MAJO'.
               05  FILLER   PIC X(57)  VALUE
           '822 012805 022       028       LYMPHOMA & LEUKEMIA W MAJO'.
               05  FILLER   PIC X(57)  VALUE
           '823 044622 115       147       LYMPHOMA & NON-ACUTE LEUKE'.
               05  FILLER   PIC X(57)  VALUE
           '824 022951 060       078       LYMPHOMA & NON-ACUTE LEUKE'.
               05  FILLER   PIC X(57)  VALUE
           '825 013803 030       040       LYMPHOMA & NON-ACUTE LEUKE'.
               05  FILLER   PIC X(57)  VALUE
           '826 050900 105       139       MYELOPROLIF DISORD OR POOR'.
               05  FILLER   PIC X(57)  VALUE
           '827 023214 052       067       MYELOPROLIF DISORD OR POOR'.
               05  FILLER   PIC X(57)  VALUE
           '828 015321 028       035       MYELOPROLIF DISORD OR POOR'.
               05  FILLER   PIC X(57)  VALUE
           '829 034231 070       106       MYELOPROLIF DISORD OR POOR'.
               05  FILLER   PIC X(57)  VALUE
           '830 012523 024       031       MYELOPROLIF DISORD OR POOR'.
               05  FILLER   PIC X(57)  VALUE
           '834 052735 102       166       ACUTE LEUKEMIA W/O MAJOR O'.
               05  FILLER   PIC X(57)  VALUE
           '835 021042 047       076       ACUTE LEUKEMIA W/O MAJOR O'.
               05  FILLER   PIC X(57)  VALUE
           '836 011693 028       040       ACUTE LEUKEMIA W/O MAJOR O'.
               05  FILLER   PIC X(57)  VALUE
           '837 064631 165       224       CHEMO W ACUTE LEUKEMIA AS '.
               05  FILLER   PIC X(57)  VALUE
           '838 027923 069       099       CHEMO W ACUTE LEUKEMIA AS '.
               05  FILLER   PIC X(57)  VALUE
           '839 012525 049       057       CHEMO W ACUTE LEUKEMIA AS '.
               05  FILLER   PIC X(57)  VALUE
           '840 031058 077       106  Y    LYMPHOMA & NON-ACUTE LEUKE'.
               05  FILLER   PIC X(57)  VALUE
           '841 016226 047       062  Y    LYMPHOMA & NON-ACUTE LEUKE'.
               05  FILLER   PIC X(57)  VALUE
           '842 010777 030       039  Y    LYMPHOMA & NON-ACUTE LEUKE'.
               05  FILLER   PIC X(57)  VALUE
           '843 017902 055       074       OTHER MYELOPROLIF DIS OR P'.
               05  FILLER   PIC X(57)  VALUE
           '844 012058 041       054       OTHER MYELOPROLIF DIS OR P'.
               05  FILLER   PIC X(57)  VALUE
           '845 008551 030       039       OTHER MYELOPROLIF DIS OR P'.
               05  FILLER   PIC X(57)  VALUE
           '846 023264 058       080       CHEMOTHERAPY W/O ACUTE LEU'.
               05  FILLER   PIC X(57)  VALUE
           '847 011569 031       037       CHEMOTHERAPY W/O ACUTE LEU'.
               05  FILLER   PIC X(57)  VALUE
           '848 008454 025       030       CHEMOTHERAPY W/O ACUTE LEU'.
               05  FILLER   PIC X(57)  VALUE
           '849 014657 046       059       RADIOTHERAPY              '.
               05  FILLER   PIC X(57)  VALUE
           '853 052068 108       139  Y    INFECTIOUS & PARASITIC DIS'.
               05  FILLER   PIC X(57)  VALUE
           '854 023877 067       081  Y    INFECTIOUS & PARASITIC DIS'.
               05  FILLER   PIC X(57)  VALUE
           '855 017057 034       048  Y    INFECTIOUS & PARASITIC DIS'.
               05  FILLER   PIC X(57)  VALUE
           '856 048177 100       134  Y    POSTOPERATIVE OR POST-TRAU'.
               05  FILLER   PIC X(57)  VALUE
           '857 020500 057       070  Y    POSTOPERATIVE OR POST-TRAU'.
               05  FILLER   PIC X(57)  VALUE
           '858 013390 038       047  Y    POSTOPERATIVE OR POST-TRAU'.
               05  FILLER   PIC X(57)  VALUE
           '862 018506 055       072  Y    POSTOPERATIVE & POST-TRAUM'.
               05  FILLER   PIC X(57)  VALUE
           '863 009836 037       045  Y    POSTOPERATIVE & POST-TRAUM'.
               05  FILLER   PIC X(57)  VALUE
           '864 008419 029       036       FEVER                     '.
               05  FILLER   PIC X(57)  VALUE
           '865 015131 043       059       VIRAL ILLNESS W MCC       '.
               05  FILLER   PIC X(57)  VALUE
           '866 007425 028       034       VIRAL ILLNESS W/O MCC     '.
               05  FILLER   PIC X(57)  VALUE
           '867 027245 070       095  Y    OTHER INFECTIOUS & PARASIT'.
               05  FILLER   PIC X(57)  VALUE
           '868 010897 039       049  Y    OTHER INFECTIOUS & PARASIT'.
               05  FILLER   PIC X(57)  VALUE
           '869 006877 027       033  Y    OTHER INFECTIOUS & PARASIT'.
               05  FILLER   PIC X(57)  VALUE
           '870 058698 126       147  Y    SEPTICEMIA OR SEVERE SEPSI'.
               05  FILLER   PIC X(57)  VALUE
           '871 018072 051       066  Y    SEPTICEMIA OR SEVERE SEPSI'.
               05  FILLER   PIC X(57)  VALUE
           '872 010528 040       047  Y    SEPTICEMIA OR SEVERE SEPSI'.
               05  FILLER   PIC X(57)  VALUE
           '876 033533 074       147       O.R. PROCEDURE W PRINCIPAL'.
               05  FILLER   PIC X(57)  VALUE
           '880 006704 023       031       ACUTE ADJUSTMENT REACTION '.
               05  FILLER   PIC X(57)  VALUE
           '881 006464 033       044       DEPRESSIVE NEUROSES       '.
               05  FILLER   PIC X(57)  VALUE
           '882 006935 033       045       NEUROSES EXCEPT DEPRESSIVE'.
               05  FILLER   PIC X(57)  VALUE
           '883 013062 043       078       DISORDERS OF PERSONALITY &'.
               05  FILLER   PIC X(57)  VALUE
           '884 010783 041       059  Y    ORGANIC DISTURBANCES & MEN'.
               05  FILLER   PIC X(57)  VALUE
           '885 010217 054       074       PSYCHOSES                 '.
               05  FILLER   PIC X(57)  VALUE
           '886 008288 039       062       BEHAVIORAL & DEVELOPMENTAL'.
               05  FILLER   PIC X(57)  VALUE
           '887 009329 030       045       OTHER MENTAL DISORDER DIAG'.
               05  FILLER   PIC X(57)  VALUE
           '894 004450 021       030       ALCOHOL/DRUG ABUSE OR DEPE'.
               05  FILLER   PIC X(57)  VALUE
           '895 012152 096       123       ALCOHOL/DRUG ABUSE OR DEPE'.
               05  FILLER   PIC X(57)  VALUE
           '896 015244 048       065  Y    ALCOHOL/DRUG ABUSE OR DEPE'.
               05  FILLER   PIC X(57)  VALUE
           '897 006905 032       040  Y    ALCOHOL/DRUG ABUSE OR DEPE'.
               05  FILLER   PIC X(57)  VALUE
           '901 039929 090       132       WOUND DEBRIDEMENTS FOR INJ'.
               05  FILLER   PIC X(57)  VALUE
           '902 017433 051       070       WOUND DEBRIDEMENTS FOR INJ'.
               05  FILLER   PIC X(57)  VALUE
           '903 011280 033       044       WOUND DEBRIDEMENTS FOR INJ'.
               05  FILLER   PIC X(57)  VALUE
           '904 033637 073       107       SKIN GRAFTS FOR INJURIES W'.
               05  FILLER   PIC X(57)  VALUE
           '905 013889 036       046       SKIN GRAFTS FOR INJURIES W'.
               05  FILLER   PIC X(57)  VALUE
           '906 011789 023       033       HAND PROCEDURES FOR INJURI'.
               05  FILLER   PIC X(57)  VALUE
           '907 037872 074       102  Y    OTHER O.R. PROCEDURES FOR '.
               05  FILLER   PIC X(57)  VALUE
           '908 019575 043       057  Y    OTHER O.R. PROCEDURES FOR '.
               05  FILLER   PIC X(57)  VALUE
           '909 012556 026       033  Y    OTHER O.R. PROCEDURES FOR '.
               05  FILLER   PIC X(57)  VALUE
           '913 011410 034       047       TRAUMATIC INJURY W MCC    '.
               05  FILLER   PIC X(57)  VALUE
           '914 007009 024       030       TRAUMATIC INJURY W/O MCC  '.
               05  FILLER   PIC X(57)  VALUE
           '915 015381 037       050       ALLERGIC REACTIONS W MCC  '.
               05  FILLER   PIC X(57)  VALUE
           '916 005137 017       021       ALLERGIC REACTIONS W/O MCC'.
               05  FILLER   PIC X(57)  VALUE
           '917 014051 035       048  Y    POISONING & TOXIC EFFECTS '.
               05  FILLER   PIC X(57)  VALUE
           '918 006412 021       028  Y    POISONING & TOXIC EFFECTS '.
               05  FILLER   PIC X(57)  VALUE
           '919 016750 044       060       COMPLICATIONS OF TREATMENT'.
               05  FILLER   PIC X(57)  VALUE
           '920 009850 030       040       COMPLICATIONS OF TREATMENT'.
               05  FILLER   PIC X(57)  VALUE
           '921 006602 022       028       COMPLICATIONS OF TREATMENT'.
               05  FILLER   PIC X(57)  VALUE
           '922 014953 039       057       OTHER INJURY, POISONING & '.
               05  FILLER   PIC X(57)  VALUE
           '923 007348 023       033       OTHER INJURY, POISONING & '.
               05  FILLER   PIC X(57)  VALUE
           '927 155499 229       289       EXTENSIVE BURNS OR FULL TH'.
               05  FILLER   PIC X(57)  VALUE
           '928 053820 111       150       FULL THICKNESS BURN W SKIN'.
               05  FILLER   PIC X(57)  VALUE
           '929 023344 057       073       FULL THICKNESS BURN W SKIN'.
               05  FILLER   PIC X(57)  VALUE
           '933 027557 024       056       EXTENSIVE BURNS OR FULL TH'.
               05  FILLER   PIC X(57)  VALUE
           '934 015748 041       058       FULL THICKNESS BURN W/O SK'.
               05  FILLER   PIC X(57)  VALUE
           '935 014717 033       050       NON-EXTENSIVE BURNS       '.
               05  FILLER   PIC X(57)  VALUE
           '939 027647 062       089       O.R. PROC W DIAGNOSES OF O'.
               05  FILLER   PIC X(57)  VALUE
           '940 018274 037       053       O.R. PROC W DIAGNOSES OF O'.
               05  FILLER   PIC X(57)  VALUE
           '941 013531 022       029       O.R. PROC W DIAGNOSES OF O'.
               05  FILLER   PIC X(57)  VALUE
           '945 012709 086       104  Y    REHABILITATION W CC/MCC   '.
               05  FILLER   PIC X(57)  VALUE
           '946 010662 066       076  Y    REHABILITATION W/O CC/MCC '.
               05  FILLER   PIC X(57)  VALUE
           '947 011368 036       048  Y    SIGNS & SYMPTOMS W MCC    '.
               05  FILLER   PIC X(57)  VALUE
           '948 007131 026       033  Y    SIGNS & SYMPTOMS W/O MCC  '.
               05  FILLER   PIC X(57)  VALUE
           '949 010525 031       045       AFTERCARE W CC/MCC        '.
               05  FILLER   PIC X(57)  VALUE
           '950 005508 024       033       AFTERCARE W/O CC/MCC      '.
               05  FILLER   PIC X(57)  VALUE
           '951 009188 025       054       OTHER FACTORS INFLUENCING '.
               05  FILLER   PIC X(57)  VALUE
           '955 055727 080       116       CRANIOTOMY FOR MULTIPLE SI'.
               05  FILLER   PIC X(57)  VALUE
           '956 036692 066       080  Y    LIMB REATTACHMENT, HIP & F'.
               05  FILLER   PIC X(57)  VALUE
           '957 068453 099       140       OTHER O.R. PROCEDURES FOR '.
               05  FILLER   PIC X(57)  VALUE
           '958 038602 072       088       OTHER O.R. PROCEDURES FOR '.
               05  FILLER   PIC X(57)  VALUE
           '959 025447 046       057       OTHER O.R. PROCEDURES FOR '.
               05  FILLER   PIC X(57)  VALUE
           '963 027071 055       082       OTHER MULTIPLE SIGNIFICANT'.
               05  FILLER   PIC X(57)  VALUE
           '964 014769 042       052       OTHER MULTIPLE SIGNIFICANT'.
               05  FILLER   PIC X(57)  VALUE
           '965 009418 029       036       OTHER MULTIPLE SIGNIFICANT'.
               05  FILLER   PIC X(57)  VALUE
           '969 060815 123       170       HIV W EXTENSIVE O.R. PROCE'.
               05  FILLER   PIC X(57)  VALUE
           '970 022085 055       072       HIV W EXTENSIVE O.R. PROCE'.
               05  FILLER   PIC X(57)  VALUE
           '974 026849 067       093       HIV W MAJOR RELATED CONDIT'.
               05  FILLER   PIC X(57)  VALUE
           '975 013047 045       058       HIV W MAJOR RELATED CONDIT'.
               05  FILLER   PIC X(57)  VALUE
           '976 008774 032       039       HIV W MAJOR RELATED CONDIT'.
               05  FILLER   PIC X(57)  VALUE
           '977 011305 035       047       HIV W OR W/O OTHER RELATED'.
               05  FILLER   PIC X(57)  VALUE
           '981 049968 100       131  Y    EXTENSIVE O.R. PROCEDURE U'.
               05  FILLER   PIC X(57)  VALUE
           '982 028150 058       074  Y    EXTENSIVE O.R. PROCEDURE U'.
               05  FILLER   PIC X(57)  VALUE
           '983 018039 028       037  Y    EXTENSIVE O.R. PROCEDURE U'.
               05  FILLER   PIC X(57)  VALUE
           '984 034344 096       130       PROSTATIC O.R. PROCEDURE U'.
               05  FILLER   PIC X(57)  VALUE
           '985 018509 051       069       PROSTATIC O.R. PROCEDURE U'.
               05  FILLER   PIC X(57)  VALUE
           '986 010453 025       034       PROSTATIC O.R. PROCEDURE U'.
               05  FILLER   PIC X(57)  VALUE
           '987 033008 084       111  Y    NON-EXTENSIVE O.R. PROC UN'.
               05  FILLER   PIC X(57)  VALUE
           '988 017643 048       064  Y    NON-EXTENSIVE O.R. PROC UN'.
               05  FILLER   PIC X(57)  VALUE
           '989 010454 023       031  Y    NON-EXTENSIVE O.R. PROC UN'.
               05  FILLER   PIC X(57)  VALUE
           '998 000000 000       000       PRINCIPAL DIAGNOSIS INVALI'.
               05  FILLER   PIC X(57)  VALUE
           '999 000000 000       000       UNGROUPABLE               '.
           02  WK-DRG-DATA2 REDEFINES WK-DRG-DATA.
               05  DRG-TAB OCCURS 753
                           ASCENDING KEY IS WK-DRG-DRGX
                           INDEXED BY DRG-IDX.
                   10  DRG-DATA-TAB.
                     15  WK-DRG-DRGX               PIC X(03).
                     15  FILLER1                   PIC X(01).
                     15  DRG-WEIGHT                PIC 9(02)V9(04).
                     15  FILLER2                   PIC X(01).
                     15  DRG-GMALOS                PIC 9(02)V9(01).
                     15  FILLER3                   PIC X(05).
                     15  DRG-LOW                   PIC X(01).
                     15  FILLER5                   PIC X(01).
                     15  DRG-ARITH-ALOS            PIC 9(02)V9(01).
                     15  FILLER6                   PIC X(02).
                     15  DRG-PAC                   PIC X(01).
                     15  FILLER7                   PIC X(01).
                     15  DRG-SPPAC                 PIC X(01).
                     15  FILLER8                   PIC X(02).
                     15  DRG-DESC                  PIC X(26).
