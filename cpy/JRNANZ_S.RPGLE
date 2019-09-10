     **-- Retrieve journal entries:  -----------------------------------------**
     D RtvJrnE         Pr                  ExtProc( 'QjoRetrieveJournalEntries')
     D  RjRcvVar                  32767a          Options( *VarSize )
     D  RjRcvVarLen                  10i 0 Const
     D  RjJrnNamQ                    20a   Const
     D  RjRcvInfFmt                   8a   Const
     D  RjSltInf                  32767a   Const  Options( *NoPass: *VarSize )
     D  RjError                   32767a          Options( *NoPass: *VarSize )
     **-- Delete pointer handle:  --------------------------------------------**
     D DltPtrHdl       Pr                  ExtProc( 'QjoDeletePointerHandle' )
     D  DhPtrHdl                     10u 0 Const
     D  DhError                   32767a          Options( *NoPass: *VarSize )
      // -------------------------------------------------------------------
      // ds for
      // -------------------------------------------------------------------
      // Object
     d tObj            ds                  qualified template
     d  ID                           10
     d  LIB                          10
      // Files
     d tFiles$         ds                  qualified template
     d  nbr                           5u 0
     d  ofs                           5u 0 dim(500)
      // File
     d tFile$          ds                  qualified template
     d                                5u 0
     d  ID                           10
     d  LIB                          10
     d  mbr                          10
      // File
     d File$           ds                  likeds(tfile$) based(pFile$)
      // job
     d tJob            ds                  qualified template
     d  ID                           10
     d  user                         10
     d  no                            6
      // timestamp
     d tZ              ds                  qualified template
     d  x                             5u 0
     d  d                             7a
     d  t                             6a
     d tRcvRng         ds                  qualified template
     d  nbr                           5u 0
     d  Rcv1                               likeDs(tObj)
     d  Rcv2                               likeDs(tObj)
      // entry type
     d tTypes          ds                  qualified template
     d  nbr                           5u 0
     d  id                            2a   dim(300)
      // various
     didx              s              5u 0
     **-- Retrieve journal header                                           --**
     D JH              Ds                  qualified
     D     BytRtn                    10i 0
     D     OfsEH                     10i 0
     D     NbrEntRtv                 10i 0
     D     ConInd                     1a
     D     ConRcv                    10a
     D     ConLib                    10a
     D     ConSeqNbr                 20s 0
     D                               11a
     D    Data                    32754a
     **-- Entry header:
     D EH              Ds                  Based(pEH) qualified
     D  OfsHdrJrnE                   10u 0
     D  OfsNulValI                   10u 0
     D  OfsEntDta                    10u 0
     D  OfsTrnId                     10u 0
     D  OfsLglUoW                    10u 0
     D  OfsRcvInf                    10u 0
     D  SeqNbr                       20u 0
     D  TimStpC                       8a
     D  ThrId                        20u 0
     D  SysSeqNbr                    20u 0
     D  CntRrn                       20u 0
     D  CmtCclId                     20u 0
     D  PtrHdl                       10u 0
     D  RmtPort                       5u 0
     D  ArmNbr                        5u 0
     D  PgmLibAsp                     5u 0
     D  RmtAdr                       16a
     D  JrnCde                        1a
     D  EntTyp                        2a
     D  JobNam                       10a
     D  UsrNam                       10a
     D  JobNbr                        6a
     D  PgmNam                       10a
     D  PgmLib                       10a
     D  PgmLibAspDv                  10a
     D  objNam                       10a
     D  objLib                       10a
     D  FILmbr                       10a
     D  UsrPrf                       10a
     D  JrnId                        10a
     D  AdrFam                        1a
     D  SysNam                        8a
     D  IndFlg                        1a
     D  ObjNamInd                     1a
     D  BitFld                        1a
     D  Rsv                           9a
      //‚journal entry data
     D ED              Ds                  Based(pED) qualified
     D  DtaLen                        5s 0
     D                               11a
     D  val                       65535a
      // criter
     D JrnEntRtv       Ds
     D  JeNbrVarRcd                  10i 0
      //‚RCVRNG - *CURRENT, *CURCHAIN
     D JV01            Ds                  qualified
     D  len                          10i 0 Inz(%Size(JV01))
     D  key                          10i 0 Inz(1)
     D  DtaLen                       10i 0 Inz(40)
     D   Rcv1                        10a
     D   lib1                        10a
     D   Rcv2                        10a
     D   Lib2                        10a
      //‚FROMENT - *FIRST
     D JV02            Ds                  qualified
     D  Len                          10i 0 Inz(%Size(JV02))
     D  key                          10i 0 Inz(2)
     D  dtaLen                       10i 0 Inz(20)
     D  dta                          20a   Inz('*FIRST')
      //‚FROMTIME
     D JV03            Ds                  qualified
     D  len                          10i 0 Inz(%Size(JV03))
     D  key                          10i 0 Inz(3)
     D  dtaLen                       10i 0 Inz(26)
     D  dta                          26a
      //‚TOENT - *LAST
     D JV04            Ds                  qualified
     D  len                          10i 0 Inz(%Size(JV04))
     D  key                          10i 0 Inz(4)
     D  DtaLen                       10i 0 Inz(20)
     D  dta                          20a   Inz('*LAST')
      //‚TOTIME
     D JV05            Ds                  qualified
     D  len                          10i 0 Inz(%Size(JV05))
     D  key                          10i 0 Inz(5)
     D  DtaLen                       10i 0 Inz(26)
     D  Dta                          26a
      //‚NBRENT
     D JV06            Ds                  qualified
     D  Len                          10i 0 Inz(%Size(JV06))
     D  key                          10i 0 Inz(6)
     D  DtaLen                       10i 0 Inz(%Size(4))
     D  Dta                          10i 0 Inz(1000)
      //‚Entry type
     D JV08            Ds                  qualified
     D  len                          10i 0 Inz(%Size(JV08))
     D  key                          10i 0 Inz(8)
     D  dtaLen                       10i 0 Inz(164)
     D  nbrTyp                       10i 0
     D  typ                          10a   Dim(16)
      //‚JOB - *ALL
     D JV09            Ds                  qualified
     D  len                          10i 0 Inz(%Size(JV09))
     D  key                          10i 0 Inz(9)
     D  dtaLen                       10i 0 Inz(26)
     D  dta                          26a   Inz('*ALL')
      //‚PGM - *ALL
     D JV10            Ds                  qualified
     D  len                          10i 0 Inz(%Size(JV10))
     D  key                          10i 0 Inz(10)
     D  dtaLen                       10i 0 Inz(10)
     D  dta                          10a   Inz('*ALL')
      //‚USRPRF * *ALL
     D JV11            Ds                  qualified
     D  len                          10i 0 Inz(%Size(JV11))
     D  key                          10i 0 Inz(11)
     D  dtaLen                       10i 0 Inz(10)
     D  dta                          10a   Inz('*ALL')
      //‚FILE - *ALLFILE, *ALL
     D JV16            Ds                  qualified
     D  len                          10i 0 Inz(%Size(JV16))
     D  key                          10i 0 Inz(16)
     D  dtaLen                       10i 0 Inz(484)
     D   JcNbrFil                    10i 0
     D   JcFilNamQ                   30a   Dim(16)
