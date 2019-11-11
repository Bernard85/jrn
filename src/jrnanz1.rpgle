      /If Defined(*CRTBNDRPG)
     H DFTACtGRP(*NO) bnddir('U6') actgrp('GA6')
      /endif

      /copy cpy,jrnanz_s

      /copy cpy,u6env_h
      /copy cpy,u6file_h
      /copy cpy,u6ifs_h
      /copy cpy,u6ibm_h
      /copy cpy,u6jrn_h
      /copy cpy,u6msg_h
      /copy cpy,u6tree_h
      /copy cpy,u6xml_h

     d jrnanz1         Pi
     d   savID                       50a   varying
     d   text                        50a   varying
     D   jrn                               likeDs(tObj)
     D   files$                            likeds(tFiles$)
     D   jobX                              likeDs(tJob)
     d   pgmnam                      10a
     d   usrprf                      10a
     d   nbrent                      10u 0
     d   fromZ                             likeds(tZ)
     d   toZ                               likeds(tZ)
     d   RcvRng                            likeds(tRcvRng)
     d   types                             likeDs(tTypes)
      *
     d  savPath        s             80a   varying
     d  G              ds                  qualified
     d   no                           5u 0
     d   entRtv                   32767a   varying
     d   hDta                        10i 0
     d   ASEQ                        20u 0 inz(0)
     d   APOS                        20u 0 inz(0)
     d   nbe                         10u 0 inz(0)
      // root nodes
     d  lJournal       s               *
     d  journal        ds                  likeDs(tJournal) based(pJournal)
     d  lEntries       s               *
     d  entries        ds                  likeDs(tEntries) based(pEntries)
       loadSelect(*off);
       if ApiErr.BytAvl=0;
         //‚declare main Nodes
         pJournal=tree_getNewItem(%addr(tJournal):%size(tJournal));
         journal.ID=savId;
         journal.text=text;
         lJournal=tree_getNewLink(pJournal);

         lEntries=
          tree_getNewLink(tree_getNewItem(%addr(tEntries):%size(tEntries)));
         tree_linkToparent(lJournal:lEntries);

         //‚open file
         G.hDta=ifs_openforStore(env_getjournalPath()+savID+'.dta');
         //‚journal declaration
         dow ApiErr.BytAvl=0;
           pEH=%Addr(JH)+Jh.OfsEH;
           For Idx=1 to JH.NbrentRtv;
             pED=pEH+EH.OfsEntDta;
             //‚entry declaration
             OUT_ENTRY();
             //
             if EH.PtrHdl>0;
               DltPtrHdl(EH.PtrHdl:apierr);
             endif;
             if idx<JH.NbrEntRtv;
               pEH+=EH.OfsHdrJrnE;
             endIf;
           endFor;
           if Jh.ConInd='0';
             leave;
           endIf;

           G.nbe+=JH.NbrentRtv;

           loadSelect(*on);

         enddo;
         //‚close file
         ifs_close(G.hDta);
         xml_tree2XML(env_getJournalPath()+savID+'.jrn'
                     :lJournal
                     :%pAddr(jrn_XMLoutput));
       endif;
       if ApiErr.bytavl>0;
         msg_SndM(apiErr.msgid:apierr.msgdta);
       endif;
       *inlr=*on;
      //‚-----------------------------------------------------------------------
      //‚Validate filter criteria and get entries
      //‚-----------------------------------------------------------------------
     pLoadSelect       b
     d LoadSelect      pi
     d next                            n   const
     d ts              s               z
       G.entRtv='';
       JeNbrVarRcd = 0;
       //‚1) Receivers
       if next;
         Jv01.Rcv1 = Jh.ConRcv;
         Jv01.Lib1 = Jh.ConLib;

         if rcvrng.nbr=1;
           Jv01.Rcv2 = '*CURRENT' ;
           Jv01.Lib2 = ''         ;
         else;
           Jv01.Rcv2 = rcvrng.rcv2.id;
           Jv01.Lib2 = rcvrng.rcv2.lib;
         endif;

         Jv02.dta    = %char(Jh.ConSeqNbr);

         G.entRtv+=jv02;
         JeNbrVarRcd+=1;
       //‚Receptor selected ?
       else;
         if rcvrng.nbr>=1;
           Jv01.Rcv1 = rcvrng.rcv1.id;
           Jv01.Lib1 = rcvrng.rcv1.lib;
         endif;
         if rcvrng.nbr=2;
           Jv01.Rcv2 = rcvrng.rcv2.id;
           Jv01.Lib2 = rcvrng.rcv2.lib;
         endif;
       endIf;

       G.entRtv+=jv01;
       JeNbrVarRcd+=1;
       //‚3) From time
       if not next and fromz.x>0;
         ts=%date(fromz.d:*cymd0)+%time(fromz.t:*iso0);
         jv03.dta=%char(ts);
         JeNbrVarRcd+=1;
         G.entRtv+=jv03;
       endIf;
       //‚6) Number of entries
       if nbrent>g.nbe;
         jv06.dta=nbrent-g.nbe;
         JeNbrVarRcd+=1;
         G.entRtv+=jv06;
       endif;
       //‚8) Types selected
       if     types.nbr=1 and types.id(1)='*A';
       elseif types.nbr=1 and types.id(1)='*R';
         jv08.nbrtyp=9;
         jv08.typ(1)='BR';
         jv08.typ(2)='DL';
         jv08.typ(3)='DR';
         jv08.typ(4)='IL';
         jv08.typ(5)='PT';
         jv08.typ(6)='PX';
         jv08.typ(7)='UB';
         jv08.typ(8)='UP';
         jv08.typ(9)='UR';
         JeNbrVarRcd+=1;
         G.entRtv+=jv08;
       elseif types.nbr>0;
         jv08.nbrtyp=types.nbr;
         for G.NO=1 to types.nbr;
           jv08.typ(G.NO)=types.id(g.no);
         endFor;
         JeNbrVarRcd+=1;
         G.entRtv+=jv08;
       endIf;

       //‚9) job
       if jobx<>'*ALL';
         jv09.dta=jobx.id+jobx.user+jobx.no;
         JeNbrVarRcd+=1;
         G.entRtv+=jv09;
       endIf;
       //‚10) pgm
       if pgmnam<>'*ALL';
         jv10.dta=pgmNam;
         JeNbrVarRcd+=1;
         G.entRtv+=jv10;
       endIf;
       //‚11) user profil
       if usrprf<>'*ALL';
         jv11.dta=usrprf;
         JeNbrVarRcd+=1;
         G.entRtv+=jv11;
       endIf;
       //‚16) Files selected
       if files$.nbr>0;
         jv16.JcNbrFil=files$.nbr;
         for G.NO    =1 to files$.nbr;
           pFile$=%addr(files$)+files$.ofs(G.NO    );
           jv16.JcFilNamQ(G.NO    )=FILE$.ID+FILE$.LIB+FILE$.MBR;
         endFor;
         JeNbrVarRcd+=1;
         G.entRtv+=jv16;
       endIf;
       //‚Extract entries
       RtvJrnE(JH
       :%Size(JH)
       :jrn.id+jrn.lib
       :'RJNE0200'
       :jrnEntRtv+G.entRtv
       :ApiErr
       );
     p                 e
      //‚-----------------------------------------------------------------------
      //‚write journal entry
      //‚-----------------------------------------------------------------------
     pout_entry        b
     d out_entry       pi
      *
     d ND              ds                  likeds(xml_nodedefine) inz
     D   mystamp       s             20a
     d entry           ds                  likeds(tEntry) based(pEntry)
       pEntry=tree_getNewitem(%addr(tEntry):%size(tEntry));

       G.aSeq+=1;
       entry.det.aseq=g.aseq;
       if ed.dtalen>0;
         entry.det.apos=g.apos;
       endIf;
       entry.det.dtal=ed.dtalen;
       entry.det.seqn=EH.SeqNbr;
       entry.det.code=EH.JrnCde;
       entry.det.entt=EH.EntTyp;
       QWCCVTDT('*DTS':EH.TimStpC:'*YYMD':myStamp:ApiErr);
       entry.det.tstp=%char(%timestamp(mystamp:*ISO0));
       entry.det.jobNO=EH.jobnbr;
       entry.det.jobUSR=%trim(EH.UsrNam);
       entry.det.jobID=%trim(EH.JobNam);
       entry.det.pgm=EH.pgmnam;
       entry.det.obj=EH.objNam;
       entry.det.lib=EH.objLib;
       entry.det.mbr=EH.FILmbr;
       entry.det.ctrr=EH.CntRrn;
       entry.det.uspf=EH.usrPrf;
       entry.det.synm=EH.sysNam;
       tree_linkToParent(lEntries
                        :tree_getNewLink(pEntry));
       //‚write journal dta
       if ed.dtalen>0;
         ifs_print(G.hDta:%subst(ED.VAL:1:ED.DtaLen));
         G.apos+=ed.dtalen;
       endIf;
     p                 e
