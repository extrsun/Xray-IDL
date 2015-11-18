;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rsaspect
;
;*PURPOSE:
; A procedure to read aspect data (roll angle, changes in X and Y pointing)
; from Rosat aspect (.CAS, .SAS, .SA for PSPC; .AO for HRI) FITS files,
; aspect quality data (aspect quality flag, star tracker flag) from aspect
; quality (.ASP for PSPC; ?? for HRI) FITS files, and plot time histories.
; Allows user to read in orbital data from SO files, select times from
; aspect history plots, and print out data for selected times.
;
;*CALLING SEQUENCE:
;	rsaspect,inputs,obinum,OPARMS=OPARMS
;
;*PARAMETERS:
; INPUTS:
;       inputs - input parameter description.
;                0 - use all defaults
;                1 - interative selection of parameters
;                string - parameter defintion of the form
;                   'parname1=value1,parname2=value2,,,,'
;                string array - each element of the array has
;                   form 'value' (entries must be given in correct order)
;                string of form '$filename' where filename is the
;                   name of the file containing one parameter
;                   per line in the form: parname=value
;       Any parameter not specified is set to its default.
;       Defaults for the parameters can be found by using interactive
;       selection params=1 or examinining the default (text) file,
;       ZDEF:RSGET.DEF.
;
;       The following parameters are availble.
;
;        OBSEQ       Root name of input data file 
;                    (null string not allowed, must be specified)
;        DIR         Directory containing input file 
;                    (default is current directory)
;        EXTYP       Extension of input file (e.g., CAS, ASP)
;                    (default is CAS)
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        TRIM        Determines whether or not data is trimmed to Good
;                    Time Intervals (default = Y)
;        CHATTER     Controls program feedback to user
;                    (default = 1)
;        READORB     Determines whether or not to read in corresponding
;                    orbital data (default = N)
;
; OPTIONAL INPUTS:
;	obinum - number(s) of obi sequences to plot, e.g. [1,3,4]
;                (default = all)
;
; OUTPUTS:
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rsaspect,1,obinum
;             ?obseq=rp123456
;	      ?dir=mydirectory
;             ?extyp=sa
;	      ?readorb=Y
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,extyp=sa,readorb=Y'
;        IDL>rsevrates,list,obinum
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(2)='sa'
;        IDL>list(10)='Y'
;        IDL>rsevrates,list,obinum
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     readorb=Y
;            *exit
;        IDL>rsevrates,'myinput.dat',obinum
;
;*RESTRICTIONS:
;	Routine attempts to display graphics on terminal.
;       Now reads FITS files directly. Do NOT convert to ST SDAS format.
;
;*NOTES:
;  PRINTPS should be set to the print command:
;    under VMS, type, e.g. PRINTPS :== print/que=ps/notify/delete
;          (PRINTPS is a system symbol.)
;    under Unix, add this command (or a similar one) to your .cshrc file
;          setenv PRINTPS "lpr -r -s"
;          (PRINTPS is an environmental variable.)
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  RSGETASP
;  MATCH_FILES
;  FITS_INFO
;  READFITS
;  TBGET
;  RSOBITIMES
;  RSGETASPQU
;  RSORB_POS
;  GETPLOTINTS
;  STACKPLOT
;
;*MODIFICATION HISTORY:
;    written 28 Aug 1991 by GAR
;    modified 20 Sep 1991 by GAR to use OBI UT times now stored in 
;                                aspect file headers
;    modified 28 Sep 1991 by GAR to read and plot aspect quality data
;    modifed to use GHRS derived parameter setting routines 05 Oct 1991 GAR
;    modified 07 Oct 1991 by GAR to read and plot one OBI at a time
;    modified 10 Oct 1991 by GAR to print s/c & UT times on plot
;    modified 28 Oct 1991 to call match_files for sequence header file, 
;       and to correct bug in specifying file extensions (GAR)
;    modified 5 Nov 1991 for compatibility with Sun Unix (GAR)
;    modified 18 Nov 1991 to use PSPRINT (GAR)
;    modified 04 Feb 1992 to change PSPRINT to PRINTPS (GAR)
;    modified 18 Feb 1992 to be compatible with RSGETASP (GAR)
;    modified 04 Jun 1992 to use READFITS, FITS_INFO, and new versions
;        of MATCH_FILES, RSGETASP, and RSORB_POS, and to return OPARMS (GAR)
;    modified 29 Dec 1993 (GAR) to read and plot aspect data from RDF format 
;      ancillary files as well
;-
;-------------------------------------------------------------------------------
pro rsaspect,inputs,obinum,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSASPECT, inputs, obinum (0 for all), [OPARMS=OPARMS]'
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, TRIM, CHATTER, and READORB' 
  print,'   from RSGET.DEF'
  retall
endif
if (npar lt 2) then obinum = 0            ; default is to read all OBIs
;
ptitle = !p.title
xtitle = !x.title
plims = [!xmin,!xmax,!ymin,!ymax]
devname = !d.name                          
;
dfile = 'rsget'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
extyp = strtrim(oparms(2),2)
instr = strtrim(oparms(3),2)
instr = strupcase(instr)
proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF
proc = strupcase(proc)
trim = strtrim(oparms(8),2)
chatter = fix(oparms(9))
readorb = strtrim(oparms(10),2)
;
; Figure out whether to read & plot aspect quality data or not
; for now, aspect quality data can only be read for PSPC (US format), or
; for RDF format
;
case proc of
  'US':  if (instr eq 'P') then aqplot = 1 else aqplot = 0
  'MPE': aqplot = 0
  'RDF': aqplot = 1
  else:  begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, MPE, RDF. Returning.'
         retall
         end
endcase         
;
;
if (readorb eq '') then begin      ; if readorb not set
  read,' Read in orbital data from corresponding orbit file(s)? (Y or N) ',$
        readorb
  oparms(10) = readorb
endif
yesno,readorb
;
; Read in data for all OBIs. Then plot data for one OBI at a time.
; Start by reading in aspect data
;
rsgetasp,oparms,obinum,sctasp,roll,delx,dely,nomasp,asperrs,scasp,flags, $
         ob=obinfo
asperrs = 0
scasp = 0
ibeg = obinfo.ibeg
iend = obinfo.iend
next = n_elements(ibeg)
;
nobseq = strtrim(oparms(0),2)
if (obseq ne nobseq) then obseq = nobseq    ;obseq may have been corrected
ndir = strtrim(oparms(1),2)
if (dir ne ndir) then dir = ndir            ;dir may have been as well
;
; Now read in aspect quality data - for now, just for PSPC, and just for US 
; format data
; Aspect quality intervals will be trimmed to obi intervals
;
if (aqplot eq 1) then begin
  case proc of
    'US':  begin
           aqtyp = ''
           oparms(2) = strtrim(aqtyp,2)
           rsgetaspqu,oparms,obinum,sctqual,qualflag,starflag
           fillaspqu,sctasp,sctqual,qualflag,starflag,qualpl,starpl
           oparms(2) = extyp
           end
    'RDF': begin
           qualpl = flags.qual
           starpl = flags.stt
           flags = 0
           end
  endcase
endif
;
; Convert from spacecraft time to hours UT
;
sctasp = (obinfo(0).utsbeg + sctasp - obinfo(0).sctbeg)/3600.
;
; Read in orbit data, if requested
;
if (readorb) then begin
  rsorb_pos,oparms,obinum,sctorb,satpos,orbvecs,relpos
  sctorb = (obinfo(0).utsbeg + sctorb - obinfo(0).sctbeg)/3600.
endif
;
; Plot data for one OBI at a time
;
obipl = obinum
if (obipl(0) eq 0) then obipl = indgen(next) + 1      ;OBI numbers start with 1
nobi = n_elements(obipl)              ;if OBIs are specified
print,' Plotting data for ',nobi,' OBIs'
;
for jj=0,nobi-1 do begin
  jobi = obipl(jj)
  jb = ibeg(jj)
  je = iend(jj)
;  
  sctbeg = obinfo(jj).sctbeg
  utsbeg = obinfo(jj).utsbeg
  yrbeg = obinfo(jj).yrbeg
  daybeg = obinfo(jj).daybeg
  sctend = obinfo(jj).sctend
  yrend = obinfo(jj).yrend
  dayend = obinfo(jj).dayend
  utsend = obinfo(jj).utsend
;
  pltit = strupcase(strtrim(obseq,2))+'   '+strupcase(strtrim(extyp,2))
  pltit = pltit+'   Obi number '
  xtit = 'Hours UT'
  ytits = ['X Trans','Y Trans','Roll Angle']
  if (aqplot eq 1) then ytits=[ytits,'AQ flag','ST flag']
;
  pltitle = pltit+strtrim(string(jobi(0)),2)
  addx = '  '+strtrim(string( fix(yrbeg) ),2)+'  day '+$
         strtrim(string( fix(daybeg) ),2)
;
;  check time array sctasp for large gaps. If large gaps exist,
;  then break each OBI up into sections & plot separately
;
  getplotints,sctasp(jb:je),kb,ke
  nsects = n_elements(kb)
  kb = kb + jb
  ke = ke + jb
  addtit = ''
  if (chatter eq 1) then $
    print,' Plotting aspect data for OBI ',string(jobi,'$(i3)'),$
          '  in ',string(nsects,'$(i3)'),' parts'
  for kk=0,nsects-1 do begin
    set_xy,sctasp(kb(kk)),sctasp(ke(kk))
    if (nsects gt 1) then addtit = '   section  '+strtrim(string(kk+1),2)
    !x.title = xtit+addx
;    !p.title = pltitle+addtit
    !p.title = ''
    if (aqplot eq 1) then $
       stackplot,sctasp(kb(kk):ke(kk)),delx(kb(kk):ke(kk)), $
         dely(kb(kk):ke(kk)),roll(kb(kk):ke(kk)),qualpl(kb(kk):ke(kk))+.01, $ 
         starpl(kb(kk):ke(kk))+.01,yname=ytits else $
       stackplot,sctasp(kb(kk):ke(kk)),delx(kb(kk):ke(kk)), $
         dely(kb(kk):ke(kk)),roll(kb(kk):ke(kk)),yname=ytits
;
    pltb = !x.crange(0)*3600.                   ;the minimum time plotted
    plte = !x.crange(1)*3600.                   ;the maximum time plotted
    plsctb = sctbeg + pltb - utsbeg
    plscte = sctend + plte - utsend
;
    sctit = 'SC times '+strtrim(string(sctbeg),2)+' to '+$
            strtrim(string(sctend),2)+' sec'
    sctit = sctit + '   ('+strtrim(string(plsctb),2)+' to '+$
            strtrim(string(plscte),2)+' sec plotted)'
    uttit = 'UT times '+strtrim(string(utsbeg),2)+' to '+$
            strtrim(string(utsend),2)+' sec '
    uttit = uttit + '   ('+strtrim(string(pltb),2)+' to '+$
            strtrim(string(plte),2)+' sec plotted)'
    if (!debug gt 0) then stop,'Stopping before plotting titles'
    xyouts,0.15,0.95,pltitle+addtit,/normal,charsize=1.2
    xyouts,0.05,0.89,sctit,/normal,charsize=1.0
    xyouts,0.05,0.85,uttit,/normal,charsize=1.0
;
; now allow user to make a hardcopy, if desired
;
    ans = ''
    read,' Do you wish to make a hardcopy of this plot (Y or N)?  ',ans
    yesno,ans
    if (ans eq 1) then begin
      ans = ''
      read,' Portrait (P) or Landscape (L) mode? (default = P) ',ans
      ans = strupcase(ans)
      set_plot,'ps'
      if (ans eq 'L') then device,/landscape else $
        device,/portrait,/inches,xoffset=1.0,xsize=7.0,yoffset=0.5,ysize=9.5
      if (aqplot eq 1) then $
       stackplot,sctasp(kb(kk):ke(kk)),delx(kb(kk):ke(kk)), $
         dely(kb(kk):ke(kk)),roll(kb(kk):ke(kk)),qualpl(kb(kk):ke(kk))+.01, $ 
         starpl(kb(kk):ke(kk))+.01,yname=ytits else $
       stackplot,sctasp(kb(kk):ke(kk)),delx(kb(kk):ke(kk)), $
         dely(kb(kk):ke(kk)),roll(kb(kk):ke(kk)),yname=ytits
      xyouts,0.15,0.965,pltitle+addtit,/normal,charsize=1.2
      xyouts,0.05,0.925,sctit,/normal,charsize=1.0
      xyouts,0.05,0.90,uttit,/normal,charsize=1.0
      device,/close
      set_plot,devname
      spawn,'PRINTPS idl.ps'
    endif    ;finished making a hard copy (if selected)
;
; now allow user to select times with cursor, to print out information
;
    seltim = ''
    read,' List info for selected times? (Y or N)  ',seltim
    yesno,seltim
    if (seltim eq 1) then begin
;
      cond1 = 120                              ;X (for tektronix)
      cond2 = 88                               ;x (for tektronix)
      if (devname eq 'X') then begin
        if (!version.os eq 'vms') then cond1 = 4 $    ;right cursor button
          else cond1 = 2                          ;if Sun, use middle button
      endif
;
      print,' Use the cursor to select times'
      if (!version.os eq 'vms') then $
        print,' Hit X (Tektronix) or right cursor button (X) to stop' $
        else $
        print,' Hit middle cursor button to stop' 
      print,'  '
      if (aqplot eq 1) then $
         print,' Day, Time (aspect), Delta X, Delta Y, Roll Angle, ',$
               'AQ Flag, ST Flag' else $
         print,' Day, Time (aspect), Delta X, Delta Y, Roll Angle '
      if (readorb) then begin
        print,' Time (orbit), Long., Lat., Height (km), Az., El., GHA,'+$
              ' Dist. (km)'
        print,' Sun, Moon, Sat. unit vectors (X, Y, Z, X, Y, Z components)'
      endif
      cursor,tsel,sel,1
      err = !err
;
      while ( (err ne cond1) and (err ne cond2) ) do begin    ;not X or x
        itim = where( abs(sctasp-tsel) eq min(abs(sctasp-tsel)) )  
        itim = itim(0)           ;find closest time
        print,'   '
        if (aqplot eq 1) then $
           print,format='(I3,F8.4,3F12.4,2I8)',daybeg,sctasp(itim),$
                 delx(itim),dely(itim),roll(itim),$
                 qualpl(itim),starpl(itim) else $
           print,format='(I3,F8.4,3F12.4,2I8)',daybeg,sctasp(itim),$
                 delx(itim),dely(itim),roll(itim)
        if (readorb) then begin
          ipos = where( abs(sctorb-tsel) eq min(abs(sctorb-tsel)) )
          ipos = ipos(0)
;
          print,'   '
          print,format='(3x,F8.4,6F9.3,F9.2)',sctorb(ipos),$
                satpos(ipos).lon,satpos(ipos).lat,satpos(ipos).height, $
                relpos(ipos).az,relpos(ipos).el,relpos(ipos).gha, $
                relpos(ipos).dist
          print,format='(3x,9F8.4)',orbvecs(ipos).sunx,orbvecs(ipos).suny, $
                orbvecs(ipos).sunz,orbvecs(ipos).moonx,orbvecs(ipos).moony, $
                orbvecs(ipos).moonz,orbvecs(ipos).satx,orbvecs(ipos).saty, $
                orbvecs(ipos).satz
        endif 
        cursor,tsel,sel,1
        err = !err
      endwhile
    endif
;
  endfor     ;finished plotting OBI jj
;
endfor       ;finished plotting aspect data for all OBIs
;
set_xy,plims(0),plims(1),plims(2),plims(3)
!p.title = ptitle
!x.title = xtitle
;
return
end         ;pro rsaspect
