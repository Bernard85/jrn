     A                                      DSPSIZ(27 132 *DS4)
     A                                      CHGINPDFT(UL)
     A          R HDR1
     A                                      OVERLAY
     A            ZTL          100   O  1  2DSPATR(HI)
     A            ZCH          125A  O  4  3COLOR(BLU)
     A                                  3  1'Type options, press Enter.'
     A                                      COLOR(BLU)
     A                                  6  2'Opt'
     A                                      COLOR(WHT)
     A                                  6  6'Order'
     A                                      COLOR(WHT)
     A                                  6 12'-- Analysis ID --------------------
     A                                      -'
     A                                      COLOR(WHT)
     A                                  6 48'-- Entitle ------------------------
     A                                      ------------------------------------
     A                                      -----------'
     A                                      COLOR(WHT)
     A          R SFL1                      SFL
     A  01                                  SFLNXTCHG
     A            XCHOICE        1A  B  7  3CHECK(LC)
     A  01                                  DSPATR(RI PC)
     A            XSEQ           5Y 0B  7  6EDTCDE(Z)
     A            XID           35   O  7 12
     A            XTITLE        80A  O  7 48
     A          R CTL1                      SFLCTL(SFL1)
     A*%%TS  SD  20170924  155740  BERNARD85   REL-V7R3M0  5770-WDS
     A                                      CF01 CF02 CF03 CF04 CF05 CF06 CF07
     A                                      CF08 CF09 CF10 CF11 CF12 CF13 CF14
     A                                      CF15 CF16 CF17 CF18 CF19 CF20 CF21
     A                                      CF22 CF23 CF24 ROLLDOWN ROLLUP
     A                                      CSRLOC(CSRTOROW   CSRTOCOL)
     A                                      OVERLAY
     A                                      SFLCSRRRN(&SFLCSRRRN)
     A  88                                  SFLDSP
     A N89                                  SFLDSPCTL
     A  89                                  SFLCLR
     A                                      SFLSIZ(0018)
     A                                      SFLPAG(0018)
     A            SFLCSRRRN      5S 0H
     A            CSRTOROW       3S 0H
     A            CSRTOCOL       3S 0H
     A            SF1RRN         4S 0H
     A            MYSFLEND      11A  O 26120DSPATR(HI)
     A            ZFK          115A  O 26  3COLOR(BLU)
     A          R MSGSFL                    SFL SFLMSGRCD(27)
     A            MSGSFLKEY                 SFLMSGKEY
     A            PGMID                     SFLPGMQ(10)
     A          R MSGCTL                    SFLCTL(MSGSFL)
     A                                      SFLDSP SFLDSPCTL SFLINZ
     A N98                                  SFLEND
     A                                      SFLPAG(1) SFLSIZ(2)
     A            PGMID                     SFLPGMQ(10)
