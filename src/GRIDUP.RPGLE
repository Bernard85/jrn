      /If Defined(*CRTBNDRPG)
     H DFTACTGRP(*NO) bnddir('U6') actgrp('AG6')
      /endif
     Fgridupd   CF   E             WORKSTN SFILE(SFL1:SFlRRN) InfDS(wsDS)

      /copy cpy,u6Ibm_h
      /copy cpy,u6env_h
      /copy cpy,u6grid_h
      /copy cpy,u6msg_h
      /copy cpy,u6screen_h
      /copy cpy,u6tree_h
      /copy cpy,u6xml_h

      //˜Global fields
     d dspXh           ds
     d                                 *   inz(%addr(xh01))
     d                                 *   inz(%addr(xh02))
     d                                 *   inz(%addr(xh03))
     d                                 *   inz(%addr(xh04))
     d                                 *   inz(%addr(xh05))
     d                                 *   inz(%addr(xh06))
     d                                 *   inz(%addr(xh07))
     d                                 *   inz(%addr(xh08))
     d                                 *   inz(%addr(xh09))
     d                                 *   inz(%addr(xh10))
     d                                 *   inz(%addr(xh11))
     d                                 *   inz(%addr(xh12))
     d                                 *   inz(%addr(xh13))
     d                                 *   inz(%addr(xh14))
     d                                 *   inz(%addr(xh15))
     d                                 *   inz(%addr(xh16))
     d                                 *   inz(%addr(xh17))
     d                                 *   inz(%addr(xh18))
     d                                 *   inz(%addr(xh19))
     d                                 *   inz(%addr(xh20))
     d                                 *   inz(%addr(xh21))
     d                                 *   inz(%addr(xh22))
     d                                 *   inz(%addr(xh23))
     d                                 *   inz(%addr(xh24))
     d                                 *   inz(%addr(xh25))
     d                                 *   inz(%addr(xh26))
     d                                 *   inz(%addr(xh27))
     d                                 *   inz(%addr(xh28))
     d                                 *   inz(%addr(xh29))
     d                                 *   inz(%addr(xh30))
     d  pXH                            *   dim(30) overlay(dspXh:1)
      //˜Global fields
     D G               DS                  qualified
     d  gridID                       10A   varying
     d  screen                        3u 0 inz(1)
     d  lGrid                          *
     d  lColumn                        *   dim(21)
     d  lColumn1                       *
     d  lColumn1_b4                    *
     d  lColumn9                       *
     d  lColumn9_b4                    *
     d  fToSort                        n
     d  error                          n
     d  refresh                        n
     d  lFKs                           *
      //‚--------------------------------------------------------------------
      //‚main
      //‚--------------------------------------------------------------------
     d GridUP          pi
     d  rtnCode                       3i 0
     d  GridID                       10    const
       g.gridID=%trim(gridID);
       //‚welcome message
       msg_SndPM(pgmID:env_getWelcomeMessage());
       //‚Title
       ZTL='Work with grid - Columns';
       //‚Load function keys
       screen_setFK(g.lFKs:x'33':'1':%pAddr(F3):'F3=Save/Exit');
       screen_setFK(g.lFKs:x'3a':'1':%pAddr(f10):'F10=Move to top');
       screen_setFK(g.lFKs:x'3c':'0':%pAddr(f12):'F12=Cancel');
       screen_setFK(g.lFKs:x'b8':'1':%pAddr(f20):'F20=Renumber');
       screen_setFK(g.lFKs:x'f1':'1':%pAddr(Enter));
       screen_setFK(g.lFKs:x'f4':*ON :%pAddr(rollUP  ));
       screen_setFK(g.lFKs:x'f5':*ON :%pAddr(rolldown));
       zFK=screen_getfkentitle(g.lFKs);
       //‚load grid
       g.lGrid=tree_xml2tree(env_getClientPath()+g.gridID+'.grid'
                            :%paddr(grid_xmlinput));
       //‚work screens
       wrkScreen();
       //‚end of program
       *inlr=*on;
      //‚--------------------------------------------------------------------
      //‚work screen
      //‚--------------------------------------------------------------------
     pwrkScreen        b
       //‚Activate screen 1 + Column1                                        -
       g.screen=1;
       g.lColumn1=tree_getFirst(g.lGrid);
       dow g.screen>0;
         if G.screen=1;
           Screen1();
         endif;
       endDo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - display form
      //‚--------------------------------------------------------------------
     p Screen1         b
       if g.lColumn9<>g.lColumn9_b4;
         sync();
       endIf;
       if g.lColumn1<>g.lColumn1_b4 or g.refresh;
         loadWA1();
         loadSf1();
         //‚more item or bottom of list
         if tree_getNext(g.lColumn9)=*null;
           mySflEnd='Bottom';
         else;
           mySflEnd='More...';
         endIf;
       endIf;
       //‚display activation                                                 -
       write msgCtl;
       write hdr1;
       *in88=*on;
       *in89=*off;
       exfmt ctl1;
       msg_rmvPM(pgmID);
       //‚get/launch function key                                            -
       screen_processFK(pgmID:g.lFKs:wsds.kp:%pAddr(Control));
     p                 e
      //‚--------------------------------------------------------------------
      //‚check input
      //‚--------------------------------------------------------------------
     pControl          b
     d Control         pi
     d column          ds                  likeds(tColumn) based(pColumn)
     d w30a            s             30a
      *
       readc sfl1;
       dow not %eof();
         g.refresh=*on;
         pColumn=tree_getItem(g.lColumn(sflrrn));
         if column.seq<>xseq;
           g.fToSort=*on;
           column.seq=xseq;
         endIf;
         memcpy(%addr(w30a)
               :pXH(xhdrWidth)
               :xhdrWidth);
         column.hdr1=w30a;
         if column.edtwidth>30;
           column.hdrfit  =0;
           column.hdrwidth=30;
         else;
           column.hdrfit=xHdrFit;
           column.hdrwidth=column.edtwidth+column.hdrfit;
         endif;
         if column.edtWidth>column.hdrWidth;
           column.maxWidth=column.edtWidth;
         else;
           column.maxWidth=column.hdrWidth;
         endIf;
         column.formula=xFormula;
         readc sfl1;
       enddo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Loadind work area
      //‚--------------------------------------------------------------------
     p loadWA1         b
      *
     d lX              s               *
     d i               s              3u 0
       clear g.lColumn;
       g.refresh=*off;
       lX=g.lColumn1;
       for i=1 to 21;
         g.lColumn(i)=lX;
         g.lColumn9=lX;
         lX=tree_getNext(lX);
       endFor;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Loadind subfile
      //‚--------------------------------------------------------------------
     p loadSF1         b
      *
     d column          ds                  likeDs(tColumn) based(pColumn)
     d w30a            s             30a
       //‚clear subfile
       *in88=*off;
       *in89=*on;
       WRITE ctl1;
       //‚Reload subfile
       for sflrrn=1 to 21;
         if g.lColumn(sflrrn)=*null;
           leave;
         endIf;
         pColumn=tree_getItem(g.lColumn(sflrrn));

         xseq     =column.seq;
         xID      =column.ID;
         xformula =column.formula;
         xedtwidth=column.edtwidth;
         xhdrfit  =column.hdrfit;
         xhdrWidth=column.hdrWidth;
         w30a=column.hdr1;
         memcpy(pXH(xhdrWidth)
               :%addr(w30a)
               :xhdrWidth);
         %SubArr(*IN:1:30)=*Off;
         *in(xhdrWidth)=*on;
         write sfl1;
       endFor;
       g.lColumn1_b4=g.lColumn1;
       g.lColumn9_b4=g.lColumn9;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Roll-UP
      //‚--------------------------------------------------------------------
     pRollUp           b
     d RollUp          pi
       if G.lColumn1=tree_GetFirst(G.lGrid);
         msg_SndPM(pgmID:'You have reached the top of the list');
       else;
         g.lColumn9=tree_getPrev(g.lColumn1);
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
         g.lColumn1=tree_getNext(g.lColumn9);
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Enter
      //‚--------------------------------------------------------------------
     pEnter            b
     d Enter           pi
       if g.fToSort;
         g.fToSort=*off;
         //‚Sort elements                                                      -
         tree_Sort(g.lGrid:%paddr(column_comparator));
         g.lColumn1=tree_getFirst(g.lGrid);
         return;
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F3=Save/Exit
      //‚--------------------------------------------------------------------
     pf3               b
     d f3              pi
       f20();
       xml_tree2xml(env_getClientPath()+g.gridID+'.grid'
                   :g.lGrid
                   :%paddr(grid_XmlOutput));
       g.screen=0;
       rtnCode=fContinue;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F10=Move to top
      //‚--------------------------------------------------------------------
     pf10              b
     d f10             pi
       if SFLCSRRRN=0;
         msg_SndPM(pgmID:'Wrong cursor position');
       else;
         g.lColumn1=g.lColumn(sflcsrrrn);
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F12=Cancel
      //‚--------------------------------------------------------------------
     pf12              b
     d f12             pi
       G.screen=0;
       rtnCode=fStop;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F20=Renumber
      //‚--------------------------------------------------------------------
     pf20              b
     d f20             pi
      *
     d Seq             s             10u 0 inz(0)
     d lColumn         s               *
     d column          ds                  likeDs(tColumn) based(pColumn)
       //‚Resequence elements                                                -
       lColumn=tree_getFirst(g.lGrid);
       dow lColumn<>*null;
         pColumn=tree_getItem(lColumn);
         if Column.seq=0;
           leave;
         else;
           Seq+=10;
           Column.seq=seq;
         endIf;
         lColumn=tree_getNext(lColumn);
       endDo;
       g.lColumn1_b4=*null;
     p                 e
      //‚------------------------------------------------------------------- ---
      //‚Synch
      //‚------------------------------------------------------------------- ---
     pSync             b
     d Sync            pi
      *
     d i               s              3u 0
     d lX              s               *
       for i=1 to 21;
         lX=tree_getPrev(G.lColumn1);
         if lX=*null;
           leave;
         endIf;
         G.lColumn1=lX;
       endFor;
     p                 e
      //‚-----------------------------------------------------------------------
      //‚Comparator
      //‚-----------------------------------------------------------------------
     pcolumn_comparator...
     p                 b                   export
     d column_comparator...
     D                 PI             3i 0
     D  lColumn1                       *   const
     D  lColumn2                       *   const
      *
     D Column1         ds                  likeDs(tColumn) based(pColumn1)
     D Column2         ds                  likeDs(tColumn) based(pColumn2)
      *
        pColumn1=tree_GetItem(lColumn1);
        pColumn2=tree_GetItem(lColumn2);
        //‚-1 1st is lower than 2nd Column                                   --
        //‚+1 1st is greater than 2nd Column                                 --
        //‚0  they are equal                                                  --
        if     Column1.Seq>0 and Column2.Seq=0;
          return -1;
        elseif Column1.Seq=0 and Column2.Seq>0;
          return +1;
        elseif Column1.Seq<Column2.Seq;
          return -1;
        elseif Column1.Seq>Column2.Seq;
          return +1;
        elseif Column1.ID<Column2.ID;
          return -1;
        elseif Column1.ID>Column2.ID;
          return +1;
        endif;
        return 0;
     P                 E
