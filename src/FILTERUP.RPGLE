      /If Defined(*CRTBNDRPG)
     H DFTACTGRP(*NO) bnddir('U6') actgrp('AG6')
      /endif
     FFilterUPd CF   E             WORKSTN SFILE(SFL1:SFlRRN) InfDS(wsDS)
      /copy cpy,u6Ibm_h
      /copy cpy,u6env_h
      /copy cpy,u6file_h
      /copy cpy,u6filter_h
      /copy cpy,u6ifs_h
      /copy cpy,u6index_h
      /copy cpy,u6jrn_h
      /copy cpy,u6msg_h
      /copy cpy,u6screen_h
      /copy cpy,u6tree_h
      /copy cpy,u6xml_h

      //˜Global fields
     D G               DS                  qualified
     d  screen                        3u 0 inz(1)
     D  change                         N
     D  error                          N
     d  lFile                          *   dim(18) inz(*null)
     d  lFile1                         *   inz(*null)
     d  lFile1_b4                      *   inz(*null)
     d  lFile9                         *   inz(*null)
     d  lFile9_B4                      *   inz(*null)
     d  lOpts                          *   inz(*null)
     d  lFKs                           *   inz(*null)
      //‚--------------------------------------------------------------------
      //‚main
      //‚--------------------------------------------------------------------
     d FilterUP        pi
     d  rtnCode                       3i 0
     d  lFiles                         *   const
     d  ID                           10a   varying const
     d  lfilters                       *
      *
     d  lFilter        s               *
     d  Filter         ds                  likeDs(tFilter) based(pFilter)
     d  lFile          s               *
     d  File           ds                  likeDs(tFile) based(pFile)
     d  fOmit          s               n
     d  iOmit          c                   const(7)
       //‚Check/create the root link
       if lFilters=*null;
         lFilters
         =tree_getnewlink(tree_getNewItem(%addr(tFilters):%size(tFilters)));
       endif;
       //‚Check/create the requested filter
       lFilter=tree_getLinkFromList(lFilters:kFilter:ID);
       if lFilter=*null;
         pFilter=tree_getNewItem(%addr(tFilter):%size(tFilter));
         Filter.ID=ID;
         lFilter=tree_getnewlink(pFilter);
         tree_linkToParent(lFilters:lFilter);
       endif;
       //‚sweep all items to see if it is omit or not
       lFile=tree_getFirst(lFiles);
       dow lFile<>*null;
         pFile=tree_getItem(lFile);
         fOmit=tree_getLinkFromList(lFilter:kOmit:file.ID)<>*null;
         if not fOmit;
           tree_setOption(lFile:'S');
         endIf;
         tree_setFlag(lFile:iOmit:fOmit);
         lFile=tree_getNext(lFile);
       endDo;
       //‚welcome message
       msg_SndPM(pgmID:env_getWelcomeMessage());
       //‚Title
       ZTL='Update filter';
       //‚Load function keys
       screen_setFK(g.lFKs:x'33':'0':%pAddr(F3):'F3=Exit');
       screen_setFK(g.lFKs:x'36':'1':%pAddr(f6):'F6=Save');
       screen_setFK(g.lFKs:x'3a':'1':%pAddr(f10):'F10=Move to top');
       screen_setFK(g.lFKs:x'f1':'1':%pAddr(Enter));
       screen_setFK(g.lFKs:x'f4':*ON :%pAddr(rollUP));
       screen_setFK(g.lFKs:x'f5':*ON :%pAddr(rolldown));
       zFK=screen_getfkentitle(g.lFKs);
       //‚Load options
       screen_SetOption(g.lOpts:'S':'S=Select');
       zCH=screen_getChoicesEntitle(g.lOpts);
       zHdr='Opt -- File -- '
           +'-- Text ------------------------------------------';
       //‚load items
       g.lFile1=tree_getFirst(lFiles);
       wrkScreen();
       *inlr=*on;
      //‚--------------------------------------------------------------------
      //‚work screen
      //‚--------------------------------------------------------------------
     pwrkScreen        b
       dow g.screen>0;
         if G.screen=1;
           Screen1();
         endif;
       endDo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - display items
      //‚--------------------------------------------------------------------
     p Screen1         b
       //‚more item or bottom of list
       if g.lFile9<>g.lFile9_b4;
         sync();
       endIf;
       if g.lFile1<>g.lFile1_b4;
         loadWA1();
         loadsfl();
         //‚more item or bottom of list
         screen_setSflEnd(mySflEnd:tree_getNext(g.lFile9)=*null);
       endIf;
       //‚display activation
       write msgCtl;
       write hdr1;
       *in88=*on;
       *in89=*off;
       exfmt ctl1;
       //‚sweep error and messages
       msg_rmvPM(pgmID);
       g.error=*off;
       //‚get/launch function key
       screen_processFK(pgmID:g.lFKs:wsds.kp:%pAddr(Control));
     p                 e
      //‚--------------------------------------------------------------------
      //‚to process file in directory
      //‚--------------------------------------------------------------------
     pControl          b
     d Control         pi              n
      *
     d File            ds                  likeDs(tFile) based(pFile)
       readc sfl1;
       dow not %eof();
         *in02=*off;
         pFile=tree_getitem(g.lFile(sflrrn));
         tree_SetOption(g.lFile(sflrrn):xChoice);
         if %scan(xChoice:' S')=0;
           g.error=*on;
           msg_SndPM(pgmID:'Option "'+xChoice+'" is not valid');
           *in02=*on;
         endif;
         //
         tree_setFlag(g.lFile(sflrrn):iOmit:not(xChoice='S'));
         update sfl1;
         readc sfl1;
       enddo;
       return g.error;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Load work area
      //‚--------------------------------------------------------------------
     p loadWA1         b
      *
     d lFile           s               *
     d i               s              3u 0
       clear g.lFile;
       lFile=g.lFile1;
       for i=1 to 18;
         g.lFile(i)=lFile;
         g.lFile9=lFile;
         lFile=tree_getNext(lFile);
       endFor;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Load subfile
      //‚--------------------------------------------------------------------
     p loadsfl         b
      *
     d file            ds                  likeDs(tFile) based(pFile)
       //‚clear subfile
       *in88=*off;
       *in89=*on;
       WRITE ctl1;
       //‚Reload subfile
       for sflrrn=1 to 18;
         if g.lFile(sflrrn)=*null;
           leave;
         endIf;
         xChoice=tree_getOption(g.lFile(sflrrn));
         pFile =tree_getItem(g.lFile(sflrrn));
         xFil='';
         %subst(xFil:1)=File.ID;
         %subst(xFil:12)=File.text;
         write sfl1;
       endFor;
       g.lFile1_b4=g.lFile1;
       g.lFile9_b4=g.lFile9;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Roll-UP
      //‚--------------------------------------------------------------------
     pRollUp           b
     d Rollup          pi
       if G.lFile1=tree_GetFirst(lFiles);
         msg_SndPM(pgmID:'You have reached the top of the list');
       else;
         g.lFile9=tree_getPrev(g.lFile1);
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Roll-down
      //‚--------------------------------------------------------------------
     pRollDown         b
     d RollDown        pi
       if mySflEnd='Bottom';
         msg_SndPM(pgmID:'You have reached the bottom of the list');
       else;
         g.lFile1=tree_getNext(g.lFile9);
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Enter
      //‚--------------------------------------------------------------------
     pEnter            b
     d Enter           pi
     p                 e
      //‚--------------------------------------------------------------------
      //‚F3=Exit
      //‚--------------------------------------------------------------------
     pf3               b
     d f3              pi
       G.screen=0;
       rtncode=3;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F6=Save
      //‚--------------------------------------------------------------------
     pf6               b
     d f6              pi
      *
     d lFile           s               *
     d File            ds                  likeDs(tFile) based(pFile)
     d Omit            ds                  likeDs(tOmit) based(pOmit)
       tree_deallocChilds(lFilter:*null);
       lFile=tree_getFirst(lFiles);
       dow lFile<>*null;
        if tree_getFlag(lFile:iOmit);
          pFile=tree_getItem(lFile);
          pOmit=tree_getNewItem(%addr(tOmit):%size(tOmit));
          Omit.ID=FILE.ID;
          tree_linkToParent(lFilter:tree_getNewLink(pOmit));
        endIf;
        lFile=tree_getNext(lFile);
       endDo;
       //
       g.screen=0;
       rtncode=6;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F10=Move to top
      //‚--------------------------------------------------------------------
     pf10              b
     d f10             pi
       if SFLCSRRRN=0;
         msg_SndPM(pgmID:'Wrong cursor position');
       else;
         g.lFile1=g.lFile(sflcsrrrn);
       endIf;
     p                 e
      //‚------------------------------------------------------------------- ---
      //‚Synch
      //‚------------------------------------------------------------------- ---
     pSync             b
     d Sync            pi
      *
     d i               s              3u 0
     d lX              s               *
       for i=1 to 18;
         lX=tree_getPrev(G.lFile1);
         if lX=*null;
           leave;
         endIf;
         G.lFile1=lX;
       endFor;
     p                 e
