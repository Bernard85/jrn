      /If Defined(*CRTBNDRPG)
     h oPTION(*NODEBUGIO:*SRCSTMT:*noshowcpy) DFTACTGrp(*no) bnddir('U6')
      /endif
      /copy qsysinc/qrpglesrc,QUSGEN
      /copy qsysinc/qrpglesrc,quslfld
      /copy qsysinc/qrpglesrc,qusrobjd

      /copy cpy,u6env_h
      /copy cpy,u6file_h
      /copy cpy,u6fmt_h
      /copy cpy,u6form_h
      /copy cpy,u6grid_h
      /copy cpy,u6ibm_h
      /copy cpy,u6tree_h
      /copy cpy,u6xml_h

     d iSpace          c                   const('FILERTV   QTEMP')
     d pSpace          s               *

     d QUSH0100_       ds                  likeDs(QUSH0100) based(pQUSH0100_)
     d QUSQLH_         ds                  likeDs(qusqlh)   based(pQUSQLH_)
     d QUSL0100_       ds                  likeDs(QUSL0100) based(pQUSL0100_)
      //‚-----------------------------------------------------------------------
      //‚main
      //‚-----------------------------------------------------------------------
     d FileRtv         pi
     d  FileID                       10
     d  fmtID                        10
     d  fFile                         1
     d  fFmt                          1
     d  fForm                         1
     d  fGrid                         1
     d  fAppli                        1a   options(*nopass)

     d lFile           s               *
     d file            ds                  likeDs(tFile)   based(pFile)

     d lFormat         s               *
     d format          ds                  likeDs(tFormat) based(pFormat)
     d lFields         s               *

     d lForm           s               *
     d form            ds                  likeDs(tForm)   based(pForm)

     d lPanel          s               *
     d panel           ds                  likeDs(tPanel)  based(pPanel)

     d lGrid           s               *
     d grid            ds                  likeDs(tGrid)   based(pGrid)
     d outPath         s             50a   varying
       //‚Appli or client path
       if %parms()=%parmNum(fAppli)
       and fAppli='Y';
         outPath=env_getAppliPath();
       else;
         outPath=env_getClientPath();
       endIf;
       //‚Retreive object description
       qusrobjd_(QUSD0200:%size(QUSD0200):'OBJD0200'
                :fileID+'*LIBL':'*FILE':apierr);
       //‚Set-up user space
       quscrtus(iSpace:'':65535:x'00':'*CHANGE':'':'*YES':apierr);
       QUSPTRUS(iSpace:pSpace);
       //‚List fields
       QUSLfld_(iSpace:'FLDL0100':fileID+'*LIBL'     :'*FIRST':'0':apiErr);
       //‚setup data structures
       pQUSH0100_=pSpace;
       pQUSQLH_=pSpace+QUSH0100_.QUSOHS;
       //‚*) force the format ID
       if fmtID='';
         fmtID=QUSQLH_.QUSRFN00;
       endif;
       //‚1) file
       pFile=tree_getNewItem(%addr(tFile):%size(tFile));
       file.ID=fileID;
       file.format=fmtID;
       file.text=%trim(QUSTD12);
       lFile=tree_getNewLink(pFile);
       //‚2) format
       pFormat=tree_getNewItem(%addr(tFormat):%size(tFormat));
       format.ID=fmtID;
       format.fldnbr=QUSH0100_.QUSNBRLE;
       format.len   =QUSQLH_.QUSRL;
       lFormat=tree_getNewLink(pFormat);
       lFields=tree_getNewLink(tree_getNewItem(%addr(tFields):%size(tFields)));
       tree_linktoparent(lFormat:lFields);
       //‚3) form
       pForm=tree_getNewItem(%addr(tForm):%size(tForm));
       form.ID=fmtID;
       form.text=QUSTD12;
       lForm=tree_getNewLink(pForm);
       pPanel=tree_getNewItem(%addr(tPanel):%size(tPanel));
       panel.text=%trim(QUSTD12)+':';
       lPanel=tree_getNewLink(pPanel);
       tree_linktoparent(lForm:lPanel);
       //‚4) grid
       pGrid=tree_getNewItem(%addr(tGrid):%size(tGrid));
       grid.ID=fmtID;
       grid.text=QUSTD12;
       lGrid=tree_getNewLink(pGrid);
       //‚detail
       detail();
       //‚1) Save the file
       if fFile='Y';
         xml_tree2XML(outPath+%trim(fileID)+'.file'
                     :lFile
                     :%pAddr(file_xmlOutput));
       endIf;
       //‚2) Save the format
       if fFmt='Y';
         xml_tree2XML(outPath+%trim(fmtID)+'.fmt'
                     :lFormat
                     :%pAddr(fmt_xmlOutput));
       endIf;
       //‚3) Save the form
       if fForm='Y';
         xml_tree2XML(outPath+%trim(fmtID)+'.form'
                     :lForm
                     :%pAddr(form_xmlOutput));
       endIf;
       //‚4) Save the grid
       if fGrid='Y';
         xml_tree2XML(outPath+%trim(fmtID)+'.grid'
                     :lGrid
                     :%pAddr(grid_xmlOutput));
       endIf;
       //‚Delete user space
       QUSDLTUS(iSpace:apierr);
       //‚free memory

       *inlr=*on;
      //‚-----------------------------------------------------------------------
      //‚detail
      //‚-----------------------------------------------------------------------
     pdetail           b
     d i               s              5u 0
     d field           ds                  likeDs(tField) based(pField)
     d label           ds                  likeDs(tLabel) based(pLabel)
     d column          ds                  likeDs(tColumn) based(pColumn)
     d no              s              5u 0 inz(0)
     d seq             s              5u 0 inz(0)
       for i=0 to QUSh0100_.QUSNBRLE-1;
         pQUSL0100_=pSpace+QUSh0100_.QUSOLD+i*QUSh0100_.QUSSEE;
         //‚2) format
         pField=tree_getNewItem(%addr(tField):%size(tField));
         field.ID         =QUSl0100_.QUSFN02;
         no+=1;
         field.no         =no;
         field.type       =QUSl0100_.QUSDT;
         field.len        =QUSl0100_.QUSFLB;
         field.dec        =QUSl0100_.QUSDP;
         field.digits     =QUSl0100_.QUSIGITS;
         field.posInBuffer=QUSl0100_.QUSOBP;
         tree_linktoparent(lFields:tree_getNewLink(pField));
         //‚3) form
         pLabel=tree_getNewItem(%addr(tLabel):%size(tLabel));
         label.text   =QUSl0100_.QUSFTD;
         label.formula=QUSl0100_.QUSFN02;

         label.maxWidth=QUSl0100_.QUSFLB;
         if  %scan(QUSl0100_.QUSDT:'PS')>0;
           label.MaxWidth=QUSl0100_.QUSIGITS+1;
           if  QUSl0100_.QUSDP>0;
             label.MaxWidth+=1;
           endIf;
         elseif QUSl0100_.QUSDT='B' and QUSl0100_.QUSFLB=4;
           label.MaxWidth=10;
         endif;

         tree_linktoparent(lPanel:tree_getNewLink(pLabel));
         //‚4) column
         seq+=10;
         pColumn=tree_getNewItem(%addr(tColumn):%size(tColumn));
         column.id     =QUSl0100_.QUSFN02;
         column.seq    =seq;
         column.text   =%trim(QUSl0100_.QUSFTD);
         column.formula=QUSl0100_.QUSFN02;
         column.edtwidth=QUSl0100_.QUSFLB;
         if  %scan(QUSl0100_.QUSDT:'PS')>0;
           column.edtwidth=QUSl0100_.QUSIGITS+1;
           if QUSl0100_.QUSDP>0;
             column.edtwidth+=1;
           endIf;
         elseif QUSl0100_.QUSDT='B' and QUSl0100_.QUSFLB=4;
           column.edtWidth=10;
         endif;
         if column.edtwidth>30;
           column.hdrWidth=30;
         else;
           column.hdrWidth=column.edtwidth;
         endif;
         column.hdrfit=0;
         column.maxWidth=column.edtwidth;
         column.hdr1    =%trim(QUSl0100_.QUSCH1);
         tree_linktoparent(lGrid:tree_getNewLink(pColumn));
       endfor;
     p                 e
