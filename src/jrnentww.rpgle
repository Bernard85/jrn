      /If Defined(*CRTBNDRPG)
     H DFTACTGRP(*NO) bnddir('U6') actgrp('AG6')
      /endif
     FJRNENTWWd CF   E             WORKSTN SFILE(SFL1:SFlRRN1) InfDS(wsDS)

      /copy cpy,u6Ibm_h
      /copy cpy,u6env_h
      /copy cpy,u6filter_h
      /copy cpy,u6file_h
      /copy cpy,u6fmt_h
      /copy cpy,u6grid_h
      /copy cpy,u6ifs_h
      /copy cpy,u6int_h
      /copy cpy,u6jrn_h
      /copy cpy,u6msg_h
      /copy cpy,u6screen_h
      /copy cpy,u6screen_s
      /copy cpy,u6Tree_h
      /copy cpy,u6xml_h
      /copy cpy,u6xView_h

      //˜Anchors
     D A               DS                  qualified
     d  lOpts                          *
     d  lFKs                           *
     d  lXViews                        *
     d  lFmts                          *
     d  lGrids                         *
     d  lYViews                        *
     d  lForms                         *
      //˜Global fields
     D G               DS                  qualified
     d  pScreen                        *   procptr
     d  lEntry1                        *
     d  lEntry1_b4                     *
     d  lEntry9                        *
     d  lEntry9_b4                     *
     d  fRefresh                       n
     d  Item                               dim(20) likeds(tItem)
     d  anzJrnPath                  255a   varying
     d  lJournal                       *
     d  lFiles                         *
     d  lEntries                       *
     d  lFilters                       *
     d  hdta                         10i 0
     d  error                          n
     d  CanTabRight                    n   inz(*off)
     d  CanTabLeft                     n   inz(*on)
     d  freePartWidth                 3u 0
     d  lEntry_last                    *
      *
     d  jrnXView       ds                  likeDs(txView) based(pJrnxView)
     d  lJrnXView      s               *
     d  margin         c                   const(4)
      *
     d  tItem          ds                  qualified
     d   lXView                        *
     d   lVariant                      *
      *
     d  journal        ds                  likeDs(tJournal) based(pJournal)
     d filters         ds                  likeds(tFilters) based(pFilters)
      //‚--------------------------------------------------------------------
      //‚main
      //‚--------------------------------------------------------------------
     d JRNENTWW        pi
     d   rtncode                      3i 0
     d   AnzID                       35
      *
     d lColumn         s               *
     d Column          ds                  likeDs(tColumn) based(pColumn)
       //‚get path for journal analysis
       g.anzJrnPath=env_getFileName(cJournal:%trim(anzID));
       //‚welcome message
       msg_SndPM(pgmID:env_getWelcomeMessage());
       //‚Load special procedure
       int_loadprocs();
       //‚Load journal                                                         -
       g.lJournal=tree_xml2tree(g.anzJrnPath:%paddr(jrn_xmlinput));
       pJournal=tree_getItem(g.lJournal);
       g.lEntries=tree_getLinkFromList(g.lJournal:kEntries);
       g.lFilters=tree_getLinkFromList(g.lJournal:kFilters);
       if g.lFilters=*null;
         g.lFilters=
         tree_getNewLink(tree_getNewItem(%addr(tFilters):%size(tFilters)));
         tree_linkToparent(g.lJournal:g.lFilters:g.lEntries);
       endIf;
       pFilters=tree_getItem(g.lFilters);
       jrn_tieEntries(g.lEntries);
       //‚Load function keys
       screen_setFK(A.lFKs:x'31':'1':%pAddr(f1):'F1=Where');
       screen_setFK(A.lFKs:x'33':'0':%pAddr(F3):'F3=Exit');
       screen_setFK(A.lFKs:x'37':'1':%pAddr(f7):'F7/F19=Left/all');
       screen_setFK(A.lFKs:x'38':'1':%pAddr(f8):'F8/F20=Right/all');
       screen_setFK(A.lFKs:x'39':'1':%pAddr(f9):'F9=Apply filter'
                                               :'F9=Display all ');
       screen_setFKcontext(a.lFKs:x'39':%char(%int(filters.activated)));

       screen_setFK(A.lFKs:x'3a':'1':%pAddr(f10):'F10=Top');
       screen_setFK(A.lFKs:x'b7':'1':%pAddr(f19));
       screen_setFK(A.lFKs:x'b8':'1':%pAddr(f20));
       screen_setFK(A.lFKs:x'b9':'1':%pAddr(f21):'F21=Filter');
       screen_setFK(A.lFKs:x'bc':'1':%pAddr(f24):'F24=Grid');
       screen_setFK(A.lFKs:x'f1':'1':%pAddr(Enter));
       screen_setFK(A.lFKs:x'f4':'1':%pAddr(rollUP));
       screen_setFK(A.lFKs:x'f5':'1':%pAddr(rolldown));
       //‚Load options
       screen_SetOption(A.lOpts:'5':'5=Data');
       screen_SetOption(A.lOpts:'j':'j=Journal');
       zCH=screen_getChoicesEntitle(A.lOpts);
       //‚Title display
       ZTL='Work with analysis '
          +journal.text
          +' ['+journal.ID+']';
       //‚load view for journal (fixed part)                                   -
       pJrnxView=xview_loadxView(a.lGrids:a.lFmts:'JRNENTRY');
       jrnXView.hdrColor=x'28';
       xview_PosAtLeft(jrnXView:%len(xFil)/2);
       xview_sethdrs(jrnXView:0);
       jrnxView.hdrs+=x'20'+'|';
       lJrnXview=tree_getNewLink(pJrnXView);

       g.freePartWidth=%len(xFil)-%len(jrnxView.hdrs);

       //‚load other views (free part)
       loadXViews();
       //‚position on the left
       f18();
       //‚get handle on data journal
       G.hDta=ifs_openForRead(env_getFileName(cData:%trim(AnzID)));
       //‚work screens                                                        -
       g.pScreen=%pAddr(screen1);
       g.lEntry1=tree_getFirst(g.lEntries);
       wrkScreens();
       //‚end of program                                                       -
       ifs_Close(g.hDta);
       tree_dealloc(a.lFKs);
       tree_dealloc(g.lJournal);
       *inlr=*on;
      //‚--------------------------------------------------------------------
      //‚loop on screens
      //‚--------------------------------------------------------------------
     pwrkScreens       b
     d wrkScreens      pi
      *
     d Screen          pr                  extproc(g.pScreen)
       //‚loop on screens                                                      -
       dow g.pScreen<>*null;
         screen();
       endDo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1 - list of entries about journal analysis
      //‚--------------------------------------------------------------------
     p Screen1         b
     d  Screen1        pi
     d fkProcess       pr                  extproc(pAction)
      *
     d pAction         s               *   procptr
     d fcontrol        s               n
       //‚refresh the work area
       if g.lEntry1<>g.lEntry1_b4
       or g.fRefresh
       or screen_toRefresh();
         zFK=screen_getfkentitle(A.lFKs);
         loadwa1();
       endif;
       //‚refresh the subfile
       if g.lEntry1<>g.lEntry1_b4
       or g.fRefresh
       or screen_toRefresh();
         loadSfl1();
       endIf;
       //‚display the limits                                                 -
       displayLimits();
       //‚display activation                                                 -
       write msgCtl;
       write hdr1;
       sflDsp=*on;
       sflClr=*off;
       exfmt ctl1;
       msg_rmvPM(pgmID);
       csrtorow=0;
       csrtocol=0;
       g.error=*off;
       //‚get/launch function key                                            -
       screen_processFK(pgmID:A.lFKs:wsds.kp:%pAddr(control));
     p                 e
      //‚--------------------------------------------------------------------
      //‚display the limits
      //‚--------------------------------------------------------------------
     p displayLimits   b
     d lXView          s               *
     d  XView          ds                    likeDS(tXView) based(pXView)
     d  leftColumn     ds                    likeDs(tColumn) based(pLeftcolumn)
     d  rightColumn    ds                    likeDs(tColumn) based(pRightColumn)
       lXView=tree_getfirst(a.lXViews);
       dow lXView<>*null;
         pXView=tree_getItem(lXView);
         pleftColumn=tree_getItem(XView.left.lColumn);
         prightColumn=tree_getItem(XView.right.lColumn);

         lXView=tree_getNext(lXView);
       endDo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Load screen work area
      //‚--------------------------------------------------------------------
     p loadwa1         b
      *
     d lEntry          s               *
     d Entry           ds                  likeds(tEntry) based(pEntry)
     d NO              s              3u 0 inz(0)
     d lXview          s               *
       clear g.Item;
       lEntry=tree_getCurrent(g.lEntry1:%pAddr(validator));
       dow lEntry<>*null;
         pEntry=tree_getItem(lEntry);
         //‚Check if enought row remains
         if NO=20;
           leave;
         endif;
         //‚store view+entry
         NO+=1;
         g.lEntry9=lEntry;
         if lXView<>Entry.lXView and Entry.lXView<>*null;
           lXView=Entry.lXView;
           g.Item(no).lVariant=lXView;
           g.Item(no).lXView=lXView;
         else;
           g.Item(no).lVariant=lEntry;
           g.Item(no).lXView=lXView;
           lEntry=tree_getNext(lEntry:%pAddr(validator));
         endIf;
       endDo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚validator  0=Skip 1=Take it
      //‚--------------------------------------------------------------------
     pvalidator        b
     d validator       pi             3i 0
     d  lEntry                         *   const
      *
     d entry           ds                  likeDs(tEntry) based(pEntry)
     d lFilterFile     s               *
     d filters         ds                  likeDs(tFilters) based(pFilters)
       //‚No filter input
       if g.lFilters=*null;
         return 1;
       endIf;
       //‚filter is activated?
       pFilters=tree_getItem(g.lFilters);
       if not Filters.activated;
         return 1;
       endif;
       //‚File object omited?
       pEntry=tree_getItem(lEntry);
       lFilterFile=tree_getLinkFromList(g.lFilters:kFilter:'FILE');
       if lFilterFile<>*null
       and tree_getLinkFromList(lFilterFile:kOmit:entry.det.obj)<>*null;
         return 0;
       endIf;
       return 1;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Load lines of subfile
      //‚--------------------------------------------------------------------
     ploadSFl1         b
     d loadSFl1        pi
      *
     d xView           ds                  likeDs(tXView)  based(pXView)
     d entry           ds                  likeDs(tEntry) based(pEntry)
     d entry0          ds                  likeDs(tEntry) based(pEntry0)
     d lColumn         s               *
     d column          ds                  likeds(tColumn) based(pColumn)
     d fmt             ds                  likeds(tFormat) based(pFmt)
     d fChg            s              3i 0
       //‚clear subfile
       sflDsp=*off;
       sflClr=*on;
       WRITE ctl1;
       g.cantabRight=*off;
       g.cantabLeft =*off;
       if g.item(1).lVariant=*null;
         sflRrn1=1;
         *in01=*on;
         xFil1=x'20'+'(No entries displayable)';
         write sfl1;
       endif;
       //‚load lines
       for sflRrn1=1 to 20;
         //‚Leave if no item
         if g.Item(sflRrn1).lVariant=*null;
           leave;
         endif;
         //‚clear the fields of the row
         *in01=*off;
         xCho='';
         xFil='';
         //‚Load line according to the kind of corresponding item
         if tree_isofthekind(kXView:g.Item(sflRrn1).lVariant:pXView);
           //‚header
           *in01=*on;
           xFil1='Opt'
                +jrnXView.hdrs
                +xview_setHdrs(XView:%len(jrnXView.hdrs));
           if sflrrn1>1;
             %subst(xfil1:1:1+%len(jrnXView.hdrs))='';
           endif;
           g.canTabRight=g.canTabRight or not XView.right.most;
           g.canTabLeft =g.canTabLeft  or not XView.Left.most;
         elseif tree_isofthekind(kEntry:g.Item(sflRrn1).lVariant:pEntry)
           and entry.det.dtaL>0
           and ENTRY.DET.CODE='R';
           //‚1) Choice
           xCho=tree_getOption(g.Item(sflRrn1).lVariant);
           //‚2) To force the cursor position
           if g.lEntry_last=g.Item(sflRrn1).lVariant;
             csrToCol=3;
             csrToRow=sflRrn1+6-1;
           endIf;
           //‚3) to setup the color
           if Entry.det.ENTT='DL'
           or entry.det.ENTT='DR';
             fChg=-1;
           else;
             fChg=%int(entry.pEntry0<>*null);
           endIf;
           //‚4) journal part
           pFmt=tree_getitem(jrnXView.lFmt);
           fmt.pBuffer1=%addr(entry.det);
           if entry.pEntry0<>*null;
             pEntry0=entry.pEntry0;
             fmt.pBuffer0=%addr(entry0.det);
           endif;
           loadSFC1(jrnXView:xFil:fChg);
           //‚5) separator
           %subst(xFil:%len(jrnXView.hdrs)-1:2)=x'20'+'|';
           //‚6) data part
           pFmt=tree_getitem(XView.lFmt);
           ifs_lseek(g.hDta:entry.det.aPos:0);
           ifs_read(g.hDta:fmt.pBuffer1:fmt.len);
           //
           if entry.pEntry0<>*null;
             ifs_lseek(g.hDta:entry0.det.aPos:0);
             ifs_read(g.hDta:fmt.pBuffer0:fmt.len);
           endif;
           loadSFC1(XView:xFil:fChg);
         endIf;
         write sfl1;
       endFor;
       //
       g.lEntry1_b4=g.lEntry1;
       g.lEntry9_b4=g.lEntry9;
       g.fRefresh=*off;
       //‚more item or bottom of list
       screen_setSflEnd(mySflEnd:tree_getNext(g.lEntry9:%pAddr(validator))
                       =*null);
     p                 e
      //‚--------------------------------------------------------------------
      //‚Load subfile columns
      //‚--------------------------------------------------------------------
     ploadSFC1         b
     d loadSFC1        pi
     d XView                               likeDs(tXView)
     d xFil                         128
     d fChg                           3i 0 const
      *
     d lColumn         s               *
     d Column          ds                  likeDs(tColumn) based(pColumn)
     d string          s          32000a
       //‚Loop on each cell
       lColumn=XView.left.lColumn;
       dow 1=1;
         pColumn=tree_getItem(lColumn);
         string=int_getStringFromArg(column.lFormula);
         //‚1) Attribut of the cell
         if fChg=0;
           //‚no change on the record
           %subst(xFil:Column.pos-1:1)=XView.Detcolor;
         elseif fChg=-1;
           //‚record deleted
           %subst(xFil:Column.pos-1:1)=x'2b';
         elseif int_getStringFromArg(column.lFormula:0)<>string;
           //‚field has been changed
           %subst(xFil:Column.pos-1:1)=XView.ChgColor;
         else;
           //‚no change on the field
           %subst(xFil:Column.pos-1:1)=XView.Detcolor;
         endIf;
         //‚2) Fill the value of the field
         if lColumn=XView.right.lColumn;
           //‚right column
           %subst(xFil:Column.pos:XView.right.width)
           =%subst(String:XView.right.pos:XView.right.width);
           if Column.pos+XView.right.width<%len(xFil);
             //‚right attribut
             %subst(xFil:Column.pos+XView.right.width:1)=x'20';
           endif;
           return;
         elseif lColumn=XView.left.lColumn;
           //‚left column
           %subst(xFil:Column.pos:XView.left.width)
           =%subst(String:XView.left.pos:XView.left.width);
         else;
           //‚mid column
           %subst(xFil:Column.pos)=%subst(String:1:column.maxWidth);
         endif;
         lColumn=tree_getNext(lColumn);
       endDo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚control input
      //‚--------------------------------------------------------------------
     pControl          b
     d Control         pi              n
       readc sfl1;
       dow not %eof();
         *in02=*off;
         if %scan(xCho:' 5j')=0;
           g.error=*on;
           msg_SndPM(pgmID:'Option "'+xCho+'" is not valid');
           *in02=*on;
         endif;
         update sfl1;
         tree_setOption(g.Item(sflRrn1).lVariant:xCho);
         readc sfl1;
       enddo;
       return g.error;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Roll-UP
      //‚--------------------------------------------------------------------
     pRollUp           b
     d RollUp          pi
      *
     d lEntry          s               *
     d lXView          s               *
     d Entry           ds                  likeds(tEntry) based(pEntry)
     d NO              s              3u 0 inz(0)
       if tree_getPrev(G.lEntry1:%pAddr(validator))=*null;
         msg_SndPM(pgmID:'You have reached the top of the list');
         return;
       endIf;
       lEntry=g.lEntry1;
       lEntry=tree_getPrev(lEntry:%pAddr(validator));
       dow lEntry<>*null;
         pEntry=tree_getItem(lEntry);
         if no=20;
           leave;
         endif;
         no+=1;
         g.lEntry1=lEntry;
         if lXView<>entry.lXView and entry.lXView<>*null;
           lXView=entry.lXView;
         else;
           lEntry=tree_getPrev(lEntry:%pAddr(validator));
         endif;
       enddo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Roll-down
      //‚--------------------------------------------------------------------
     pRollDown         b
     d RollDown        pi
       if mySflEnd='Bottom';
         msg_SndPM(pgmID:'You have reached the bottom of the list');
       else;
         g.lEntry1=tree_getNext(g.lEntry9);
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Enter
      //‚--------------------------------------------------------------------
     pEnter            b
     d Enter           pi
      *
     d lEntry          s               *
     d entry           ds                  likeds(tEntry) based(pEntry)
     d rtnCode         s              3i 0
     d option          s              1a
      *
      /copy cpy,jrnentdp_h
       //‚loop on each entry                                                 -
       lEntry=tree_getFirst(g.lEntries);
       dow lEntry<>*null;
         option=tree_getOption(lEntry);
         if %scan(option:'5j')>0;
           g.fRefresh=*on;
           pEntry=tree_getItem(lEntry);
           jrnEntDp(rtncode:option
                   :ztl:Entry
                   :g.hDta
                   :g.lFiles:a.lYViews:a.lFmts:a.lForms);
         g.lEntry_last=lEntry;
           if rtnCode>0;
             tree_setOption(lEntry:'');
           elseif rtnCode=0;
             leave;
           else;
             leave;
           endIf;
         endIf;
         lEntry=tree_getNext(lEntry);
       endDo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F1=Where am I?
      //‚--------------------------------------------------------------------
     pf1               b
     d f1              pi
      *
     d column          ds                  likeds(tColumn)
     d posOnColumn     s              5u 0
       csrtorow=wsds.csrfromrow;
       csrtocol=wsds.csrfromcol;
       if SFLCSRRRN=0
       or wsds.CsrFromCol<=margin
       or wsds.CsrFromCol-margin=%len(jrnxView.hdrs)-1
       or wsds.CsrFromCol-margin=%len(jrnxView.hdrs);
         msg_SndPM(pgmID:'Wrong cursor position');
       elseif wsds.CsrFromCol-margin<%len(jrnxView.hdrs);
         xview_getColumnAtPos(lJrnXView
                             :wsds.CsrFromCol-margin
                             :column
                             :posOnColumn);
         msg_SndPM(pgmID:'You are on the column '+%trim(column.formula)
                        +' '+%char(posOnColumn));
       elseif xview_getColumnAtPos(g.Item(sflcsrrrn).lXView
                                  :wsds.CsrFromCol-margin
                                  :column
                                  :posOnColumn);
         msg_SndPM(pgmID:'You are on the column '+%trim(column.formula)
                        +' '+%char(posOnColumn));
       else;
         msg_SndPM(pgmID:'No displayed column at the specified position');
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F3=Exit
      //‚--------------------------------------------------------------------
     pf3               b
     d f3              pi
       G.pScreen=*null;
       xml_tree2xml(g.anzJrnPath:g.lJournal:%paddr(jrn_xmloutput));
     p                 e
      //‚--------------------------------------------------------------------
      //‚F7=Left tab
      //‚--------------------------------------------------------------------
     pf7               b
     d f7              pi
      *
     d XView           ds                  likeDs(tXView) based(pXView)
       csrtorow=wsds.csrfromrow;
       csrtocol=wsds.csrfromcol;
       if SFLCSRRRN=0 or g.Item(1).lVariant=*null;
         msg_SndPM(pgmID:'Wrong cursor position');
       else;
         csrtocol=%len(jrnxView.hdrs)+5;
         pXView=tree_getItem(g.item(SFLCSRRRN).lXView);
         if XView.left.most;
           msg_SndPM(pgmID:'Format is on the most left position');
         else;
           xview_TabLeft(XView:g.freePartWidth);
           g.fRefresh=*on;
         endIf;
       endif;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F8=Right tab
      //‚--------------------------------------------------------------------
     pf8               b
     d f8              pi
      *
     d XView           ds                  likeDs(tXView) based(pXView)
       csrtorow=wsds.csrfromrow;
       csrtocol=wsds.csrfromcol;
       if SFLCSRRRN=0 or g.Item(1).lVariant=*null;
         msg_SndPM(pgmID:'Wrong cursor position');
       else;
         csrtocol=%len(jrnxView.hdrs)+5;
         pXView=tree_getItem(g.item(SFLCSRRRN).lXView);
         if XView.Right.most;
           msg_SndPM(pgmID:'Format is on the rightmost position');
         else;
           xview_TabRight(XView:g.freePartWidth);
           g.fRefresh=*on;
         endIf;
       endif;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F9=display all/apply filter
      //‚--------------------------------------------------------------------
     pf9               b
     d f9              pi
      *
     d filters         ds                  likeDS(tFilters) based(pFilters)
       pFilters=tree_getItem(g.lFilters);
       filters.activated=not filters.activated;
       g.fRefresh=*on;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F10=Move to top
      //‚--------------------------------------------------------------------
     pf10              b
     d f10             pi
       if SFLCSRRRN=0 or g.Item(1).lVariant=*null;
         msg_SndPM(pgmID:'Wrong cursor position');
       elseif tree_getKind(g.Item(sflcsrrrn).lVariant)=kXView;
         g.lEntry1=g.Item(sflcsrrrn+1).lVariant;
       else;
         g.lEntry1=g.Item(sflcsrrrn).lVariant;
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F18=Position all views on the left
      //‚--------------------------------------------------------------------
     pf18              b
     d f18             pi
      *
     d lXView          s               *
     d XView           ds                  likeDs(tXView) based(pXView)
       if g.canTabLeft;
         lXView=tree_getFirst(a.lXViews);
         dow lXView<>*null;
           pXView=tree_getItem(lXView);
           xview_PosAtLeft(XView:g.freePartWidth);
           lXView=tree_getNext(lXView);
         endDo;
         g.fRefresh=*on;
       else;
         msg_SndPM(pgmID:'Formats displayed are on the most left position');
       endif;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F19=Left tab on all
      //‚--------------------------------------------------------------------
     pf19              b
     d f19             pi
      *
     d lXView          s               *
     d XView           ds                  likeDs(tXView) based(pXView)
       if g.canTabLeft;
         lXView=tree_getFirst(a.lXViews);
         dow lXView<>*null;
           pXView=tree_getItem(lXView);
           xview_TabLeft(XView:g.freePartWidth);
           lXView=tree_getNext(lXView);
         endDo;
         g.fRefresh=*on;
       else;
         msg_SndPM(pgmID:'Formats displayed are on the most left position');
       endif;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F20=Right
      //‚--------------------------------------------------------------------
     pf20              b
     d f20             pi
      *
     d lXView          s               *
     d XView           ds                  likeDs(tXView) based(pXView)
       if g.canTabRight;
         lXView=tree_getFirst(a.lXViews);
         dow lXView<>*null;
           pXView=tree_getItem(lXView);
           xview_tabRight(XView:g.freePartWidth);
           lXView=tree_getNext(lXView);
         endDo;
         g.fRefresh=*on;
       else;
        msg_SndPM(pgmID:'Formats displayed are on the most right position');
       endif;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F21=Filter
      //‚--------------------------------------------------------------------
     pf21              b
     d f21             pi
      /copy cpy,filterup_h
     d rtnCode         s              3i 0
       filterUP(rtncode:g.lFiles:'FILE':g.lFilters);
       if rtnCode=6;
         g.fRefresh=*on;
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚F24=Grid
      //‚--------------------------------------------------------------------
     pf24              b
     d f24             pi
      /copy cpy,gridup_h
     d lXView          s               *
     d  XView          ds                  likeDs(tXView) based(pXView)
     d lGrid           s               *
     d  Grid           ds                  likeDs(tGrid) based(pGrid)
     d  gridID         s             10a   varying
     d  Column         ds                  likeDs(tColumn) based(pColumn)
     d  columnID       s             10a   varying
       if SFLCSRRRN=0;
         msg_SndPM(pgmID:'Wrong cursor position');
       else;
         lXView=g.item(sflcsrrrn).lXview;
         pXView=tree_getItem(lXView);
         lGrid=XView.lGrid;
         pGrid=tree_getItem(lGrid);
         gridID=grid.ID;
         gridUP(rtnCode:grid.ID);
         if rtnCode=fContinue;
           xview_reloadGrid(xview:a.lGrids:lGrid:grid.ID:g.freePartWidth);
           g.fRefresh=*on;
         endIf;
       endIf;
     p                 e
      //‚--------------------------------------------------------------------
      //‚XView are loaded for each entry
      //‚--------------------------------------------------------------------
     ploadXViews       b
     d loadXViews      pi
     d lEntry          s               *
     d Entry           ds                  likeDs(tEntry) based(pEntry)
     d lFile           s               *
     d File            ds                  likeDs(tFile) based(pFile)
       //‚Files are loaded from analysis
       lEntry=tree_getFirst(g.lEntries);
       dow lEntry<>*null;
         pEntry=tree_getItem(lEntry);
         if entry.det.code='R' and entry.det.entt<>'IL';
           //‚Files are loaded from object
           lFile=file_getFile(g.lFiles:%trim(entry.det.obj));
           //‚Load the corresponding XVIew
           if lFile<>*null;
             pFile=tree_getItem(lFile);
             entry.lXView
               =xview_getXView(a.lXViews:a.lGrids:a.lFmts:File.Format);
           endif;
         endif;
         lEntry=tree_getNext(lEntry);
       endDo;
     p                 e
