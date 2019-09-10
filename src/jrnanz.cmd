             CMD        PROMPT('Analyse journal')

             PARM       KWD(SAVPATH) TYPE(*CHAR) LEN(50) MIN(1) +
                          VARY(*YES) CASE(*MIXED) PROMPT('Save path')

             PARM       KWD(TEXT) TYPE(*CHAR) LEN(50) MIN(1) +
                          VARY(*YES) CASE(*MIXED) PROMPT('Analyse +
                          description')

             PARM       KWD(JRN) TYPE(Q10DA) SNGVAL((*INTSYSJRN 0)) +
                          MIN(1) PROMPT('Journal')
             PARM       KWD(FILE) TYPE(E0B57) SNGVAL((*ALLFILE)) +
                          MAX(8) PROMPT('Journaled file')
             PARM       KWD(JOB) TYPE(Q113B) DFT(*ALL) +
                          SNGVAL((*ALL)) PROMPT('Job name')

             PARM       KWD(PGM) TYPE(*NAME) LEN(10) DFT(*ALL) +
                          SPCVAL((*ALL)) PGM(*YES) EXPR(*YES) +
                          PROMPT('Program')

             PARM       KWD(USRPRF) TYPE(*NAME) LEN(10) DFT(*ALL) +
                          SPCVAL((*ALL)) PGM(*YES) EXPR(*YES) +
                          PROMPT('User profile')

             PARM       KWD(NBRENT) TYPE(*INT4) DFT(*ALL) REL(*GE 1) +
                          SPCVAL((*ALL 0)) EXPR(*YES) +
                          PROMPT('Number of journal entries')

             PARM       KWD(FROMTIME) TYPE(E0C09) CHOICE(*NONE) +
                          PROMPT('Starting date and time')
             PARM       KWD(toTIME) TYPE(E0C53) CHOICE(*NONE) +
                          PROMPT('Ending date and time')
             PARM       KWD(RCVRNG) TYPE(E0BB5) DFT(*CURRENT) +
                          SNGVAL((*CURRENT) (*CURCHAIN) +
                          (*CURAVLCHN)) PROMPT('Range of journal +
                          receivers')
             PARM       KWD(ENTTYP) TYPE(*CHAR) LEN(2) DFT(*ALL) +
                          REL(*GE X'C000') SNGVAL((*ALL *A) (*RCD +
                          *R)) MAX(300) EXPR(*YES) PROMPT('Journal +
                          entry types')
 E0B57:      ELEM       TYPE(Q11B5) MIN(1) FILE(*UNSPFD) +
                          CHOICE(*NONE) PROMPT('File')
             ELEM       TYPE(*NAME) LEN(10) DFT(*FIRST) +
                          SPCVAL((*FIRST) (*ALL) (*NONE)) +
                          EXPR(*YES) PROMPT('Member')
 E0BB5:      ELEM       TYPE(Q10DA) MIN(1) CHOICE(*NONE) +
                          PROMPT('Starting journal receiver')
             ELEM       TYPE(Q10DA) DFT(*CURRENT) SNGVAL((*CURRENT)) +
                          PROMPT('Ending journal reicever')
 E0C09:      ELEM       TYPE(*DATE) MIN(1) EXPR(*YES)               +
                          PROMPT('Starting date')
             ELEM       TYPE(*time)        EXPR(*YES)               +
                          PROMPT('Starting time')
 E0C53:      ELEM       TYPE(*DATE) MIN(1) EXPR(*YES) PASSATR(*YES) +
                          PROMPT('Ending date')
             ELEM       TYPE(*time)        EXPR(*YES) PASSATR(*YES) +
                          PROMPT('Ending time')
 E0E7B:      ELEM       TYPE(*CHAR) LEN(2) RSTD(*YES) VALUES('A' 'B' +
                          'C' 'D' 'E' 'F' 'J' 'L' 'M' 'P' 'Q' 'R' +
                          'S' 'T' 'U' 'Y') MIN(1) EXPR(*YES) +
                          CHOICE(*NONE) PROMPT('Journal code value')
             ELEM       TYPE(*CHAR) LEN(2) RSTD(*YES) DFT(*ALLSLT) +
                          SPCVAL((*ALLSLT *A) (*IGNFILSLT *I) +
                          (*IGNOBJSLT *O)) EXPR(*YES) +
                          PROMPT('Journal code selection')

 Q10DA:      QUAL       TYPE(*NAME) LEN(10) MIN(1) EXPR(*YES)
             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) +
                          SPCVAL((*LIBL) (*CURLIB  '*CURLIB   ')) +
                          EXPR(*YES) PROMPT('Library')
 Q113B:      QUAL       TYPE(*NAME) LEN(10) MIN(1) EXPR(*YES)
             QUAL       TYPE(*NAME) LEN(10) EXPR(*YES) PROMPT('User')
             QUAL       TYPE(*CHAR) LEN(6) RANGE('000000' '999999') +
                          FULL(*YES) EXPR(*YES) PROMPT('Number')
 Q11B5:      QUAL       TYPE(*NAME) LEN(10) SPCVAL((*ALL)) MIN(1) +
                          EXPR(*YES)
             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) +
                          SPCVAL((*LIBL) (*CURLIB  '*CURLIB   ')) +
                          EXPR(*YES) PROMPT('Library')
