      /If Defined(*CRTBNDRPG)
     H DFTACTGRP(*NO) bnddir('U6') actgrp('AG6')
      /endif
     FFilterupd CF   E             WORKSTN SFILE(SFL1:SFL1RRN) InfDS(wsDS)
     F                                     SFILE(SFL2:SFL2RRN)
      /copy cpy,u6Ibm_h
      /copy cpy,u6env_h
      /copy cpy,u6filter_h
      /copy cpy,u6ifs_h
      /copy cpy,u6index_h
      /copy cpy,u6jrn_h
      /copy cpy,u6msg_h
      /copy cpy,u6screen_h
      /copy cpy,u6Stat_h
      /copy cpy,u6tree_h
      /copy cpy,u6xml_h
      //‚--------------------------------------------------------------------
      //‚main
      //‚--------------------------------------------------------------------
     d FilterUP        pi
     d  rtnCode                       3i 0
     d  lStats                         *   const
     d  lFilters                       *   const
      //˜Global fields (screen 1)
     D G1              DS                  qualified
     d  screen                        3u 0 inz(1)
     D  fRefresh                       N
     d  lStat                          *   dim(18) inz(*null)
     d  lStat1                         *   inz(*null)
     d  lStat1_b4                      *   inz(*null)
     d  lStat9                         *   inz(*null)
     d  lStat9_b4                      *   inz(*null)
     d  lOpts                          *   inz(*null)
     d  lFKs                           *   inz(*null)
     d  fChange                        n   inz(*off)
      //˜Global fields (screen 2)
     D G2              DS                  qualified
     d  screen                        3u 0 inz(2)
     D  change                         N
     d  lModel                         *   dim(18) inz(*null)
     d  lModel1                        *   inz(*null)
     d  lModel1_b4                     *   inz(*null)
     d  lModel9                        *   inz(*null)
     d  lModel9_b4                     *   inz(*null)
     d  lOpts                          *   inz(*null)
     d  lFKs                           *   inz(*null)
     d  fChange                        n   inz(*off)
     d  Stat_          ds                  likeDs(tStat) based(pStat)
     d  lFilter        s               *
     d  Filter         ds                  likeDs(tFilter) based(pFilter)
     d  lModel         s               *
     d  Model          ds                  likeDs(tModel) based(pModel)
     d  fOmit          s               n
     d  iOmit          c                   const(7)
       //‚Title
       ZTL1='Select filters';
       //‚Load function keys
       screen_setFK(g1.lFKs:x'3a':'1':%pAddr(f10_1):'F10=Move to top');
       screen_setFK(g1.lFKs:x'f1':'1':%pAddr(Enter_1));
       screen_setFK(g1.lFKs:x'f4':*ON :%pAddr(rollUP_1));
       screen_setFK(g1.lFKs:x'f5':*ON :%pAddr(rolldown_1));
       zFK1=screen_getfkentitle(g1.lFKs);
       //‚Load options
       screen_SetOption(g1.lOpts:'1':'1=Select');
       zCH1=screen_getChoicesEntitle(g1.lOpts);
       zHdr1='Opt -- ID ----- '
           +'-- Text ------------------------------------------';
       //‚load items
       g1.lStat1=tree_getFirst(lStats);
       wrkScreen_1();
       *inlr=*on;
      //‚--------------------------------------------------------------------
      //‚Screen 1 - work screen
      //‚--------------------------------------------------------------------
     pwrkScreen_1      b
       dow g1.screen>0;
         if g1.screen=1;
           Screen_1();
         endif;
       endDo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - display items
      //‚--------------------------------------------------------------------
     p Screen_1        b
       //‚more item or bottom of list
       if g1.lStat9<>g1.lStat9_b4;
         sync_1();
       endIf;
       if g1.lStat1<>g1.lStat1_b4 or g1.fRefresh;
         loadWA_1();
         loadsfl_1();
         //‚more item or bottom of list
         screen_setSflEnd(mySfl1End:tree_getNext(g1.lStat9)=*null);
       endIf;
       //‚display activation
       write msgCtl;
       write hdr1;
       *in88=*on;
       *in89=*off;
       exfmt ctl1;
       //‚sweep error and messages
       msg_rmvPM(pgmID);
       g1.fRefresh=*off;
       g1.fChange=*off;
       //‚get/launch function key
       screen_processFK(pgmID:g1.lFKs:wsds.kp:%pAddr(Control_1));
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - inventoried models
      //‚--------------------------------------------------------------------
     pControl_1        b
     d Control_1       pi              n
      *
     D  fError         s               N   inz(*off)
       readc sfl1;
       dow not %eof();
         g1.fChange=*on;
         *in02=*off;
         tree_SetOption(g1.lStat(SFL1RRN):xChoice1);
         if %scan(xChoice1:' 1')=0;
           fError=*on;
           msg_SndPM(pgmID:'Option "'+xChoice1+'" is not valid');
           *in02=*on;
         endif;
         //
         update sfl1;
         readc sfl1;
       enddo;
       return fError;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - Load work area
      //‚--------------------------------------------------------------------
     p loadWA_1        b
      *
     d lStat           s               *
     d i               s              3u 0
       clear g1.lStat;
       lStat=g1.lStat1;
       for i=1 to 18;
         g1.lStat(i)=lStat;
         g1.lStat9=lStat;
         lStat=tree_getNext(lStat);
       endFor;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - Load subfile
      //‚--------------------------------------------------------------------
     p loadsfl_1       b
      *
     d stat            ds                  likeDs(tStat) based(pStat)
       //‚clear subfile
       *in88=*off;
       *in89=*on;
       WRITE ctl1;
       //‚Reload subfile
       for SFL1RRN=1 to 18;
         if g1.lStat(SFL1RRN)=*null;
           leave;
         endIf;
         xChoice1=tree_getOption(g1.lStat(SFL1RRN));
         pStat =tree_getItem(g1.lStat(SFL1RRN));
         xFil1='';
         %subst(xFil1:1)=stat.ID;
         %subst(xFil1:12)=stat.text;
         write sfl1;
       endFor;
       g1.lStat1_b4=g1.lStat1;
       g1.lStat9_b4=g1.lStat9;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - Roll-up
      //‚--------------------------------------------------------------------
     pRollUp_1         b
     d Rollup_1        pi
       if g1.lStat1=tree_GetFirst(lStats);
         msg_SndPM(pgmID:'You have reached the top of the list');
       else;
         g1.lStat9=tree_getPrev(g1.lStat1);
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - Roll-down
      //‚--------------------------------------------------------------------
     pRollDown_1       b
     d RollDown_1      pi
       if mySfl1End='Bottom';
         msg_SndPM(pgmID:'You have reached the bottom of the list');
       else;
         g1.lStat1=tree_getNext(g1.lStat9);
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - Enter
      //‚--------------------------------------------------------------------
     pEnter_1          b
     d Enter_1         pi
      *
     d rtnCode         s              3i 0 inz(0)
     d lStat           s               *
       lStat=tree_getFirst(lStats);
       dow lStat<>*null;
         if tree_getOption(lStat)='1';
           g1.fChange=*on;
           filter1UP(rtncode:lStat:lFilters);
           if rtnCode=3;
             return;
           endIf;
           tree_setOption(lStat:'');
           g1.fRefresh=*on;
         endIf;
         lStat=tree_getNext(lStat);
       endDo;
       if not g1.fChange;
         g1.screen=0;
         rtncode=6;
       endif;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - F10=Move to top
      //‚--------------------------------------------------------------------
     pf10_1            b
     d f10_1           pi
       if SFL1CSRRRN=0;
         msg_SndPM(pgmID:'Wrong cursor position');
       else;
         g1.lStat1=g1.lStat(SFL1CSRRRN);
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - Synch
      //‚--------------------------------------------------------------------
     pSync_1           b
     d Sync_1          pi
      *
     d i               s              3u 0
     d lX              s               *
       for i=1 to 18;
         lX=tree_getPrev(g1.lStat1);
         if lX=*null;
           leave;
         endIf;
         g1.lStat1=lX;
       endFor;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 2 - main
      //‚--------------------------------------------------------------------
     pfilter1UP        b
     d Filter1UP       pi
     d  rtnCode                       3i 0
     d  lStat                          *   const
     d  lfilters                       *   const
       //‚valid the stat
       pStat=tree_getItem(lStat);
       //‚Check/create the requested filter
       lFilter=tree_getLinkFromList(lFilters:kFilter:stat_.ID);
       if lFilter=*null;
         pFilter=tree_getNewItem(%addr(tFilter):%size(tFilter));
         Filter.ID=stat_.ID;
         lFilter=tree_getnewlink(pFilter);
         tree_linkToParent(lFilters:lFilter);
       endif;
       //‚sweep all items to see if it is omit or not
       lModel=tree_getFirst(stat_.lModels);
       dow lModel<>*null;
         pModel=tree_getItem(lModel);
         fOmit=tree_getLinkFromList(lFilter:kOmit:Model.ID)<>*null;
         if not fOmit;
           tree_setOption(lModel:'S');
         endIf;
         tree_setFlag(lModel:iOmit:fOmit);
         lModel=tree_getNextToDisplay(stat_.lModels:lModel);
       endDo;
       //‚Title
       ZTL2='Update filter '+stat_.text;
       //‚Load function keys
       screen_setFK(g2.lFKs:x'33':'0':%pAddr(F3_2):'F3=Exit');
       screen_setFK(g2.lFKs:x'3a':'1':%pAddr(f10_2):'F10=Move to top');
       screen_setFK(g2.lFKs:x'f1':'1':%pAddr(Enter_2));
       screen_setFK(g2.lFKs:x'f4':*ON :%pAddr(rollUP_2));
       screen_setFK(g2.lFKs:x'f5':*ON :%pAddr(rolldown_2));
       zFK2=screen_getfkentitle(g2.lFKs);
       //‚Load options
       tree_dealloc(g2.lOpts);
       screen_SetOption(g2.lOpts:'S':'S=Select');
       zCH2=screen_getChoicesEntitle(g2.lOpts);
       zHdr2='Opt -- ID ----- '
            +'-- Text ------------------------------------------';
       //‚load items
       g2.lModel1=tree_getFirst(stat_.lModels);
       wrkScreen_2();
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 2 - work screen
      //‚--------------------------------------------------------------------
     pwrkScreen_2      b
       g2.screen=2;
       dow g2.screen>0;
         if g2.screen=2;
           Screen2();
         endif;
       endDo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 2 - display items
      //‚--------------------------------------------------------------------
     p Screen2         b
       //‚more item or bottom of list
       if g2.lModel9<>g2.lModel9_b4;
         sync_2();
       endIf;
       if g2.lModel1<>g2.lModel1_b4;
         loadWA_2();
         loadsfl_2();
         //‚more item or bottom of list
         screen_setSflEnd(mySfl2End
                :tree_getNextToDisplay(stat_.lModels:g2.lModel9)=*null);
       endIf;
       //‚display activation
       write msgCtl;
       write hdr2;
       *in88=*on;
       *in89=*off;
       exfmt ctl2;
       //‚sweep error and messages
       msg_rmvPM(pgmID);
       g2.fChange=*off;
       //‚get/launch function key
       screen_processFK(pgmID:g2.lFKs:wsds.kp:%pAddr(Control_2));
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 2 - To select models
      //‚--------------------------------------------------------------------
     pControl_2        b
     d Control_2       pi              n
      *
     d Model           ds                  likeDs(tModel) based(pModel)
     D fError          s               N   inz(*off)
       readc sfl2;
       dow not %eof();
         *in02=*off;
         g2.fChange=*on;
         pModel=tree_getitem(g2.lModel(SFL2RRN));
         tree_SetOption(g2.lModel(SFL2RRN):xChoice2);
         if %scan(xChoice2:' S')=0;
           ferror=*on;
           msg_SndPM(pgmID:'Option "'+xChoice2+'" is not valid');
           *in02=*on;
         endif;
         //
         tree_setFlag(g2.lModel(SFL2RRN):iOmit:not(xChoice2='S'));
         update sfl2;
         readc sfl2;
       enddo;
       return ferror;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 2 - Load work area
      //‚--------------------------------------------------------------------
     p loadWA_2        b
      *
     d lModel          s               *
     d i               s              3u 0
       clear g2.lModel;
       lModel=g2.lModel1;
       for i=1 to 18;
         g2.lModel(i)=lModel;
         g2.lModel9=lModel;
         lModel=tree_getNextToDisplay(stat_.lModels:lModel);
       endFor;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 2 -  load subfile
      //‚--------------------------------------------------------------------
     p loadsfl_2       b
      *
     d Model           ds                  likeDs(tModel) based(pModel)
       //‚clear subfile
       *in88=*off;
       *in89=*on;
       WRITE ctl2;
       //‚Reload subfile
       for SFL2RRN=1 to 18;
         if g2.lModel(SFL2RRN)=*null;
           leave;
         endIf;
         xChoice2=tree_getOption(g2.lModel(SFL2RRN));
         pModel =tree_getItem(g2.lModel(SFL2RRN));
         xFil2='';
         %subst(xFil2:1)=Model.ID;
         write sfl2;
       endFor;
       g2.lModel1_b4=g2.lModel1;
       g2.lModel9_b4=g2.lModel9;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 2 - Roll-UP
      //‚--------------------------------------------------------------------
     pRollUp_2         b
     d Rollup_2        pi
       if g2.lModel1=tree_GetFirst(stat_.lModels);
         msg_SndPM(pgmID:'You have reached the top of the list');
       else;
         g2.lModel9=tree_getPrevToDisplay(stat_.lModels:g2.lModel1);
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 2 - Roll-down
      //‚--------------------------------------------------------------------
     pRollDown_2       b
     d RollDown_2      pi
       if mySfl2End='Bottom';
         msg_SndPM(pgmID:'You have reached the bottom of the list');
       else;
         g2.lModel1=tree_getNextToDisplay(stat_.lModels:g2.lModel9);
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 2 - Enter
      //‚--------------------------------------------------------------------
     pEnter_2          b
     d Enter_2         pi
     d lModel          s               *
     d Model           ds                  likeDs(tModel) based(pModel)
     d Omit            ds                  likeDs(tOmit) based(pOmit)
       if g2.Fchange;
         msg_SndPM(pgmID:'Press Enter again to confirm');
         return;
       endif;
       tree_deallocChilds(lFilter:*null);
       lModel=tree_getFirst(stat_.lModels);
       dow lModel<>*null;
        if tree_getFlag(lModel:iOmit);
          pModel=tree_getItem(lModel);
          pOmit=tree_getNewItem(%addr(tOmit):%size(tOmit));
          Omit.ID=Model.ID;
          tree_linkToParent(lFilter:tree_getNewLink(pOmit));
        endIf;
        lModel=tree_getNextToDisplay(stat_.lModels:lModel);
       endDo;
       //
       g2.screen=0;
       rtncode=6;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 2 - F3=Exit
      //‚--------------------------------------------------------------------
     pf3_2             b
     d f3_2            pi
       g2.screen=0;
       rtncode=3;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 2 - F10=Move to top
      //‚--------------------------------------------------------------------
     pf10_2            b
     d f10_2           pi
       if SFL2CSRRRN=0;
         msg_SndPM(pgmID:'Wrong cursor position');
       else;
         g2.lModel1=g2.lModel(SFL2CSRRRN);
       endIf;
     p                 e
      //‚-------------------------------------------------------------------
      //‚Screen 2 - synch
      //‚-------------------------------------------------------------------
     pSync_2           b
     d Sync_2          pi
      *
     d i               s              3u 0
     d lX              s               *
       for i=1 to 18;
         lX=tree_getPrevToDisplay(stat_.lModels:g2.lModel1);
         if lX=*null;
           leave;
         endIf;
         g2.lModel1=lX;
       endFor;
     p                 e
