      /If Defined(*CRTBNDRPG)
     H DFTACTGRP(*NO) bnddir('U6') actgrp('AG6')
      /endif
     FJRNENTDPd CF   E             WORKSTN SFILE(SFL1:SFlRRN) InfDS(wsDS)
      /copy cpy,u6DS_h
      /copy cpy,u6file_h
      /copy cpy,u6fmt_h
      /copy cpy,u6form_h
      /copy cpy,u6ifs_h
      /copy cpy,u6int_h
      /copy cpy,u6jrn_h
      /copy cpy,u6msg_h
      /copy cpy,u6screen_h
      /copy cpy,u6screen_s
      /copy cpy,u6Tree_h
      /copy cpy,u6XML_h
      /copy cpy,u6YView_h
      //‚-------------------------------------------------------------------
      //‚main
      //‚-------------------------------------------------------------------
     d jrnentdp        pi
     d  rtnCode                       3i 0
     d  option                        1a
     d  zTL_                        131a
     d  Entry                          a   likeds(tEntry)
     d  hDta                         10i 0
     d  lFiles                         *
     d  lYViews                        *
     d  lFmts                          *
     d  lForms                         *
      //˜Anchors
     D A               DS                  qualified
     d  lActions                       *
      //˜Global
     D G               DS                  qualified
     d  pScreen                        *   procptr
     d  item                               dim(23) likeDs(tItem)
     d  armTop                             likeDs(tArm)
     d  rcdChg                         n
      //˜F9=data/journal
     d fF9             s               n
      //˜F11=Formula/Value
     d fF11            s               n
      //˜item
     d  tItem          ds                  qualified
     d   lVariant                      *
     d   segment                      5u 0
      //
     d  lYView         s               *
     d  YView          ds                  likeDs(tYView) based(pYView)
     d  lFile          s               *
     d  file           ds                  likeDs(tFile) based(pFile)
     d  fmt            ds                  likeDs(tFormat) based(pFmt)
       //‚title
       zTL=zTL_;
       //‚validate the view according to the option code
       fF9=option='5';
       resolveYView(fF9);
       //‚Load function keys
       screen_SetAction(A.lActions:x'f1':'0':%pAddr(Enter));
       screen_SetAction(A.lActions:x'33':'0':%pAddr(F3):'F3=Exit');
       if fF9;
       screen_SetAction(A.lActions:x'39':'0':%pAddr(F9):'F9=Journal');
       else;
       screen_SetAction(A.lActions:x'39':'0':%pAddr(F9):'F9=Data   ');
       endIf;
       screen_SetAction(A.lActions:x'3a':'0':%pAddr(f10):'F10=Move top');
       screen_SetAction(A.lActions:x'3b':'0':%pAddr(F11):'F11=Formula');
       screen_SetAction(A.lActions:x'f4':'0':%pAddr(rollUP));
       screen_SetAction(A.lActions:x'f5':'0':%pAddr(rolldown));
       zFK=screen_getfkentitle(A.lActions);
       //‚work screens
       g.pScreen=%paddr(Screen1);
       wrkScreens();
       *inlr=*on;
      //‚--------------------------------------------------------------------
      //‚validate the view
      //‚--------------------------------------------------------------------
     presolveYView     b
     d resolveYView    pi
     d  data                           n
      *
     d  Entry0         ds                  likeds(tEntry)
     d                                     based(Entry.pEntry0)
       if data;
         //‚get the file/format
         lFile=tree_getItemfromList(lFiles:kFile:entry.det.obj);
         pfile=tree_getItem(lFile);
         lYView=yview_getYView(lYViews:lForms:lFmts:file.format);
         pYView=tree_getItem(lYView);
         //‚get corresponding data
         pFmt=tree_getitem(YView.lFmt);
         ifs_lseek(hDta:entry.det.aPos:0);
         ifs_read(hDta:fmt.pBuffer1:fmt.len);
         if entry.det.ENTT='UP';
           g.rcdChg=*on;
           ifs_lseek(hDta:entry0.det.aPos:0);
           ifs_read(hDta:fmt.pBuffer0:fmt.len);
         endIf;
       else;
         //‚get the format
         lYView=yview_getYView(lYViews:lForms:lFmts:'JRNENTRY');
         pYView=tree_getItem(lYView);
         //‚get corresponding data
         pFmt=tree_getitem(YView.lFmt);
         fmt.pBuffer1=%addr(entry.det);
         if entry.det.ENTT='UP';
           g.rcdChg=*on;
           fmt.pBuffer0=%addr(entry0.det);
         endif;
       endif;
     p                 e
      //‚--------------------------------------------------------------------
      //‚loop on sceens
      //‚--------------------------------------------------------------------
     pwrkScreens       b
     d wrkScreens      pi
      *
     d Screen          pr                  extproc(g.pScreen)
       //‚loop on screens
       dow g.pScreen<>*null;
         screen();
       endDo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚Screen 1
      //‚--------------------------------------------------------------------
     pScreen1          b
     d Screen1         pi
      *
     d cond1           s               n
     d cond2           s               n
     d label           ds                  likeDs(tLabel) based(pLabel)
       //‚refresh the list
       if YView.armTop<>g.armTop;
         g.armTop=YView.armTop;
         loadSFL();
         //‚End of the Subfile ?
         if tree_isofthekind(kLabel:YView.armBot.lVariant:pLabel);
           cond1=YView.armBot.segment=%int((label.maxWidth-1)/70);
         else;
           cond1=*on;
         endIf;
         cond2=tree_getNextToDisplay(YView.armBot.lVariant)=*null;
         screen_setSflEnd(mySflEnd:cond1 and cond2);
       endif;
       //‚display activation
       write msgCtl;
       write hdr1;
       sflDsp=*on;
       sflClr=*off;
       exfmt ctl1;
       msg_rmvPM(pgmID);
       csrtorow=0;
       csrtocol=0;
       //‚get/launch function key
       screen_processFK(pgmID:A.lActions:wsds.kp:*null);
     p                 e
      //‚--------------------------------------------------------------------
      //‚Load the subfile
      //‚--------------------------------------------------------------------
     ploadSfl          b
     d loadSfl         pi
      *
     d lVariant        s               *
     d lPanel$         s               *
     d lLabel$         s               *
     d LabelChg        s               N
     d PanelChg        s               N
     d SegChg          s               N
     d NO              s              4s 0 inz(0)
     d segment         s              5i 0
     d s               s              5i 0
     d s0              s              5i 0
     d s9              s              5i 0
     d label           ds                  likeDs(tLabel) based(pLabel)
     d String1         s          32000a
     d String2         s          32000a
     d Segment1        s             70a
     d Segment2        s             70a
     d remainingRows   s              3u 0
     d NecessaryRows   s              3u 0
       //‚clear subfile
       sflClr=*on;
       sflDsp=*off;
       WRITE ctl1;
       lVariant=g.armTop.lVariant;
       //‚load the new panel
       if tree_getKind(lVariant)=kPanel;
         lPanel$=lVariant;
       else;
         lPanel$=tree_getParent(lVariant);
       endIf;
       PanelChg=*on;
       s0=g.armTop.segment;
       //‚loop on each item
       dow lVariant<>*null;
         if tree_isofthekind(kLabel:lVariant:pLabel);
           labelChg=*on;
           //‚load value(s) for the label
           if g.rcdChg;
             string1=int_getStringFromArg(label.lFormula:2);
             string2=int_getStringFromArg(label.lFormula:1);
           else;
             string1=int_getStringFromArg(label.lFormula:1);
           endIf;
           //‚loop on each segment
           s9=(label.maxWidth-1)/70;
           for s=s0 to s9;
             //‚load segments
             SegChg=cmpSegments(label:String1:String2:s);
             //‚check if enought space avalaible
             remainingRows=23-NO;
             NecessaryRows=%int(PanelChg)*2+%int(SegChg)*1+1;
             if remainingRows<NecessaryRows;
               leave;
             endIf;
             //‚Print new panel
             if PanelChg;
               printPanel(NO:lPanel$);
               PanelChg=*off;
             endIf;
             //‚Print segment
             printSegment(NO:lVariant:labelChg:SegChg:String1:string2:s);
             labelChg=*off;
           endFor;
         endIf;
         s0=0;
         lVariant=tree_getNextToDisplay(lVariant);
         //‚new panel?
         if tree_isOfTheKind(kPanel:lVariant);
           lPanel$=lVariant;
           PanelChg=*on;
         endIf;
       endDo;
     p                 e
      //‚--------------------------------------------------------------------
      //‚compare segments
      //‚--------------------------------------------------------------------
     pcmpSegments      b
     d cmpSegments     pi              N
     d  label                              const likeDs(tLabel)
     d  string1                   32000a   const
     d  string2                   32000a   const
     d  segment                       5i 0 const
      *
     d pos             s              5u 0
     d length          s              3i 0
       if not g.rcdChg;
         return *off;
       endIf;
       pos=segment*70+1;
       length=int_getMin(70:label.maxwidth-segment*70);
       return %subst(string1:pos:length)
            <>%subst(string2:pos:length);
     p                 e
      //‚-------------------------------------------------------------------
      //‚print panel
      //‚-------------------------------------------------------------------
     pprintPanel       b
     d printPanel      pi
     d  no                            4s 0
     d  lPanel$                        *
      *
     d panel           ds                  likeDs(tPanel) based(pPanel)
       pPanel=tree_getItem(lPanel$);
       //‚line feed
       if no>1;
         no+=1;
         //šline feed
         sflRRN=no;
         write sfl1;
         memoryItem(NO:lPanel$:0);
       endIf;
       //‚panel
       NO+=1;
       xFil=panel.text;
       sflrrn=no;
       write sfl1;
       memoryItem(NO:lPanel$:0);
     p                 e
      //‚-------------------------------------------------------------------
      //‚print segment (of label)
      //‚-------------------------------------------------------------------
     pprintSegment     b
     d printSegment    pi
     d  no                            4s 0
     d  lLabel                         *   const
     d  labelChg                       n   const
     d  segChg                         n   const
     d  string1                   32000a   const
     d  string2                   32000a   const
     d  segment                       5i 0 const
      *
     d label           ds                  likeDs(tLabel) based(pLabel)
       pLabel=tree_getItem(lLabel);
       //šnew label:print the text
       if labelChg;
         xFil=int_AddSpaceDot('  '+Label.text+' ':52);
         if g.rcdChg and string1<>string2;
           %subst(xFil:1:1)='ˆ';
           %subst(xFil:53:1)='€';
         endIf;
         if fF11;
           %subst(xFil:60-%len(label.formula)-2:%len(label.formula)+2)
           ='‚'+label.formula+x'20';
         endIf;
       endIf;
       //švalue
       %subst(xFil:62)=%subst(String1:segment*70+1:70);
       //‚if segment is changed:hight the changed value (before)
       if SegChg;
// x'    %subst(xFil:59:2)='€>';
       endif;
       no+=1;
       sflrrn=no;
       write sfl1;
       xFil='';
       memoryItem(NO:lLabel:segment);
       //‚if segment is changed:hight the changed value (post)
       if SegChg;
         no+=1;
         %subst(xFil:59:2)='ˆ>';
         %subst(xFil:62)=%subst(String2:segment*70+1:70);
         sflrrn=no;
         write sfl1;
         xFil='';
         memoryItem(NO:lLabel:segment);
       endIf;
     p                 e
      //‚-------------------------------------------------------------------
      //‚memory item
      //‚-------------------------------------------------------------------
     pmemoryItem       b
     d memoryItem      pi
     d  NO                            3u 0 const
     d  lVariant                       *   const
     d  segment                       5s 0 const
       g.item(NO).lVariant=lVariant;
       g.item(NO).segment =segment;
       YView.armBot.lVariant=lVariant;
       YView.armBot.segment =segment;
     p                 e
      //‚-------------------------------------------------------------------
      //‚Enter
      //‚-------------------------------------------------------------------
     pEnter            b
     d Enter           pi
       g.pScreen=*null;
       rtnCode=fContinue;
     p                 e
      //‚-------------------------------------------------------------------
      //‚F3=Exit
      //‚-------------------------------------------------------------------
     pF3               b
     d F3              pi
       g.pScreen=*null;
       rtnCode=fStop;
     p                 e
      //‚-------------------------------------------------------------------
      //‚F11=Formula/Value
      //‚-------------------------------------------------------------------
     pF11              b
     d F11             pi
       if fF11;
         zFK=%scanrpl('F11=Value  ':'F11=Formula':zFK);
       else;
         zFK=%scanrpl('F11=Formula':'F11=Value  ':zFK);
       endIf;
       fF11=not fF11;
       clear g.armTop;
     p                 e
      //‚-------------------------------------------------------------------
      //‚F10=Move to top
      //‚-------------------------------------------------------------------
     pF10              b
     d F10             pi
       if SFLCSRRRN=0;
         msg_SndPM(pgmID:'Wrong cursor position');
       else;
         YView.armTop.lVariant=g.item(SFLCSRRRN).lVariant;
         YView.armTop.segment=g.item(SFLCSRRRN).segment;
       endIf;
     p                 e
      //‚-------------------------------------------------------------------
      //‚F9=journal/data
      //‚-------------------------------------------------------------------
     pF9               b
     d F9              pi
       if fF9;
         zFK=%ScanRpl('F9=Journal':'F9=Data   ':zFK);
       else;
         zFK=%ScanRpl('F9=Data   ':'F9=Journal':zFK);
       endIf;
       fF9=not fF9;
       resolveYView(fF9);
     p                 e
      //‚-------------------------------------------------------------------
      //‚Roll-down
      //‚-------------------------------------------------------------------
     pRollDown         b
     d RollDown        pi
      *
     d label           ds                  likeDs(tLabel) based(pLabel)
       if mySflEnd='Bottom';
         msg_SndPM(pgmID:'You have reached the bottom of the form');
       else;
          YView.armTop=YView.armBot;
          pLabel=tree_getItem(YView.armBot.lVariant);
          if YView.armTop.segment<(label.maxWidth-1)/70;
            YView.armTop.segment+=1;
          else;
            YView.armTop.lVariant=tree_getNextToDisplay(YView.armTop.lVariant);
            YView.armTop.segment=0;
          endif;
       endIf;
     p                 e
      //‚-------------------------------------------------------------------
      //‚Roll-up
      //‚-------------------------------------------------------------------
     pRollUp           b
     d RollUp          pi
      *
     d lVariant        s               *
     d PanelChg        s               N
     d SegChg          s               N
     d NO              s              4s 0 inz(0)
     d segment         s              5i 0
     d s               s              5i 0
     d s0              s              5i 0
     d s9              s              5i 0
     d label           ds                  likeDs(tLabel) based(pLabel)
     d String1         s          32000a
     d String2         s          32000a
     d remainingRows   s              3u 0
     d NecessaryRows   s              3u 0
       lVariant=YView.armTop.lVariant;
       if YView.armTop.segment>0;
         s9=YView.armTop.segment-1;
       else;
         lVariant=tree_getPrevToDisplay(lVariant);
         if tree_isOftheKind(kLabel:lVariant:pLabel);
           s9=(label.maxWidth-1)/70;
         endIf;
       endIf;
       PanelChg=*on;
       //‚loop on each item
       dow lVariant<>*null;
         if tree_isofthekind(kForm:lVariant);
           return;
         elseif tree_isofthekind(kLabel:lVariant:pLabel);
           //‚load value(s) for the label
           if g.rcdChg;
             string1=int_getStringFromArg(label.lFormula:2);
             string2=int_getStringFromArg(label.lFormula:1);
           endIf;
           //‚loop on each segment
           for s=s9 downto 0;
             //‚load segments
             SegChg=cmpSegments(label:String1:String2:s);
             //‚check if enought space avalaible
             remainingRows=23-NO;
             NecessaryRows=%int(PanelChg)*2-%int(no=0)
                          +%int(SegChg)*1
                          +1;
             if remainingRows<NecessaryRows;
               return;
             endIf;
             no+=necessaryRows;
             //‚Print new panel
             PanelChg=*off;
             //‚Print segment
             YView.armtop.lVariant=lVariant;
             YView.armtop.segment=s;
           endFor;
         endIf;
         lVariant=tree_getPrevToDisplay(lVariant);
         //‚new panel/label?
         if tree_isOfTheKind(kPanel:lVariant);
           PanelChg=*on;
         elseif tree_isOfTheKind(kLabel:lVariant:pLabel);
           s9=(label.maxWidth-1)/70;
         endIf;
       endDo;
     p                 e
