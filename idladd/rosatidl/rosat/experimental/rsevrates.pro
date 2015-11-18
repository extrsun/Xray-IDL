;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rsevrates
;
;*PURPOSE:
; A procedure to read event rates from ROSAT .EVR files and plot time
; histories. 
; Allows user to read in orbital data from SO files, select times from
; event rates plots, and print out data for selected times.
; (do NOT convert files to ST SDAS format!)
;
;*CALLING SEQUENCE:
;	rsevrates,inputs,obinum, [oparms=oparms]
;
;*PARAMETERS:
; INPUTS:
;       inputs - input parameter description.
;                0 - use all defaults
;                1 - interative selection of parameters
;                string - parameter defintion of the form
;                   'parname1=value1,parname2=value2,,,,'
;                string array - each element of the array has
;                   form 'parname=value'
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
;                    (default is EVR)
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
;        IDL>rsevrates,1,obinum
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?readorb=Y
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,readorb=Y'
;        IDL>rsevrates,list,obinum
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
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
;
;*NOTES:
;  Will read FITS files directly. Do NOT convert files to ST SDAS format.
;  Input parameter TRIM is ignored (and need not be specified).
;  PRINTPS should be set to the print command:
;    under VMS, type, e.g. PRINTPS :== print/que=ps/notify/delete
;          (PRINTPS is a system symbol.)
;    under Unix, add this command (or a similar one) to your .cshrc file
;          setenv PRINTPS "lpr -r -s"
;          (PRINTPS is an environmental variable.)
;
;*SUBROUTINES CALLED:
;  RSGETEVR
;  RSGETPAR
;  MATCH_FILES
;  TBREAD
;  TBGET
;  RSOBITIMES
;  RSORB_POS
;  GETOBIEVR
;  STACKPLOT
;
;*MODIFICATION HISTORY:
;    written 16 May 1991 by GAR
;    modified to also plot hours in UT 17 May 1991 by GAR
;    modified to use STACKPLOT 16 July 1991 by GAR
;    modified 30 Aug 1991 by GAR so that OBI numbers begin with 1 (not 0)
;    modifed to use GHRS derived parameter setting routines 05 Oct 1991 GAR
;    modified 07 Oct 1991 by GAR to read and plot one OBI at a time
;    modified 09 Oct 1991 by GAR to print s/c & UT times on plot
;    modified 28 Oct 1991 to call match_files for sequence header file (GAR)
;    modified 8 Nov 1991 for compatibility with Sun Unix (GAR)
;    modified 18 Nov 1991 to use PSPRINT (GAR)
;    modified 04 Feb 1992 to change PSPRINT to PRINTPS (GAR)
;    modified 18 Feb 1992 to be compatible with RSGETEVR
;    modified 29 Feb 1992 to be compatible with RSGETEVR
;    modified 04 Jun 1992 to use READFITS, FITS_INFO, and new versions
;        of MATCH_FILES, RSGETEVR, and RSORB_POS, and to return OPARMS (GAR)
;    modified 02 Dec 1993 (GAR) to be compatible with changed tag names
;      within the output structures from RSGETEVR
;    modified 29 Dec 1993 (GAR) to read and plot aspect data from RDF format 
;      ancillary files as well
;-
;-------------------------------------------------------------------------------
pro rsevrates,inputs,obinum,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'RSEVRATES, inputs, obinum (0 for all), [OPARMS=OPARMS]'
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, CHATTER, and READORB'
  print,'   from RSGET.DEF'
  retall
endif
if (npar lt 2) then obinum = 0                 ; default is to read all
if (n_elements(obinum) eq 1) then obinum = obinum + intarr(1)
;
ptitle = !p.title
xtitle = !x.title
plims = [!xmin,!xmax,!ymin,!ymax]
devname = !d.name                          
xstyle = !x.style
;
dfile = 'rsget'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
extyp = strtrim(oparms(2),2)
instr = strtrim(oparms(3),2)
proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF
proc = strupcase(proc)
chatter = fix(oparms(9))
readorb = strtrim(oparms(10),2)
;
if (readorb eq '') then begin      ; if readorb not set
  read,' Read in orbital data from corresponding orbit file(s)? (Y or N) ',$
        readorb
  oparms(10) = readorb
endif
yesno,readorb
;
; Read in data for all OBIs. Then plot data for one OBI at a time.
; Start by reading in event rates data
;
rsgetevr,oparms,obinum,sctevr,rates,counts,rates2,ob=obinfo
ibeg = obinfo.ibeg
iend = obinfo.iend
next = n_elements(ibeg)
;
nobseq = strtrim(oparms(0),2)
if (obseq ne nobseq) then obseq = nobseq    ;obseq may have been corrected
ndir = strtrim(oparms(1),2)
if (dir ne ndir) then dir = ndir            ;dir may have been as well
;
;
; Convert from spacecraft time to hours UT
;
sctevr = (obinfo(0).utsbeg + sctevr - obinfo(0).sctbeg)/3600.
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
; calculate telemetry livetime correction factor
;
  ltcorr = rates.trans/(rates.acc > 0.5) 
;
  pltit = strupcase(strtrim(obseq,2))+'   '+strupcase(strtrim(extyp,2))+$
          '   Obi number '
  xtit = 'Hours UT'
  ytits = ['A1L','MV','ACC','TRANS','LTC','SAA-DA']
;
  pltitle = pltit+strtrim(string(jobi(0)),2)
  addx = '  '+strtrim(string( fix(yrbeg) ),2)+'  day '+$
         strtrim(string( fix(daybeg) ),2)
;
;  find all parts of this OBI where all the rates are equal to zero
;  break each OBI up into nonzero rate sections & plot separately
;
  getobievr,kb,ke,rates(jb:je).a1l,rates(jb:je).mv,rates(jb:je).acc,$
            rates(jb:je).trans
  nsects = n_elements(kb)
  kb = kb + jb
  ke = ke + jb
  addtit = ''
  if (chatter eq 1) then $
    print,' Plotting event rates for OBI ',string(jobi,'$(i3)'),$
          '  in ',string(nsects,'$(i3)'),' parts'
  for kk=0,nsects-1 do begin
    set_xy,sctevr(kb(kk)),sctevr(ke(kk))
    if (nsects gt 1) then addtit = '   section  '+strtrim(string(kk+1),2)
    !x.title = xtit+addx
;    !p.title = pltitle+addtit
    !p.title = ''
    !x.style = 1
    stackplot,sctevr(kb(kk):ke(kk)),rates(kb(kk):ke(kk)).a1l, $
              rates(kb(kk):ke(kk)).mv,rates(kb(kk):ke(kk)).acc, $
              rates(kb(kk):ke(kk)).trans,ltcorr(kb(kk):ke(kk)), $
              rates2(kb(kk):ke(kk)).sda,yname=ytits
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
        stackplot,sctevr(kb(kk):ke(kk)),rates(kb(kk):ke(kk)).a1l, $
                  rates(kb(kk):ke(kk)).mv,rates(kb(kk):ke(kk)).acc, $
                  rates(kb(kk):ke(kk)).trans,ltcorr(kb(kk):ke(kk)), $
                  rates2(kb(kk):ke(kk)).sda,yname=ytits
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
      print,' Day, Time (evr), A1, MV, AE, TE Rates, Counters,', $
            ' Livetime Corr.'
      if (readorb eq 1) then begin
        print,' Time (orbit), Long., Lat., Height (km), Az., El., GHA,'+$
              ' Dist. (km)'
        print,' Sun, Moon, Sat. unit vectors (X, Y, Z, X, Y, Z components)'
      endif
      cursor,tsel,ysel,1
      err = !err
;
      while ( (err ne cond1) and (err ne cond2) ) do begin    ;not X or x
        itim = where( abs(sctevr-tsel) eq min(abs(sctevr-tsel)) ) 
        itim = itim(0)          ;find closest time
        print,'   '
        print,format='(I3,F8.4,4F6.1,4F8.1,F6.3)',daybeg,$
              sctevr(itim),rates(itim),counts(itim),ltcorr(itim)
        if (readorb eq 1) then begin
          ipos = where( abs(sctorb-tsel) eq min(abs(sctorb-tsel)) )
          ipos = ipos(0)
;
;  this appears to be a bug in IDL? can't print the data structure numbers
;  the way that the format statement specifies ...
;
          print,'   '
          print,format='(3x,F8.4,6F9.3,F9.2)',sctorb(ipos),$
                satpos(ipos).lon,satpos(ipos).lat,satpos(ipos).height, $
                relpos(ipos).az,relpos(ipos).el,relpos(ipos).gha, $
                relpos(ipos).dist
          p1 = orbvecs.sunx & p1 = p1(ipos)
          p2 = orbvecs.suny & p2 = p2(ipos)
          p3 = orbvecs.sunz & p3 = p3(ipos)
          p4 = orbvecs.moonx & p4 = p4(ipos)
          p5 = orbvecs.moony & p5 = p5(ipos)
          p6 = orbvecs.moonz & p6 = p6(ipos)
          p7 = orbvecs.satx & p7 = p7(ipos)
          p8 = orbvecs.saty & p8 = p8(ipos)
          p9 = orbvecs.satz & p9 = p9(ipos)
          print,format='(3x,9F8.4)',orbvecs(ipos).sunx,orbvecs(ipos).suny, $
                orbvecs(ipos).sunz,orbvecs(ipos).moonx,orbvecs(ipos).moony, $
                orbvecs(ipos).moonz,orbvecs(ipos).satx,orbvecs(ipos).saty, $
                orbvecs(ipos).satz
        endif 
        cursor,tsel,ysel,1
        err = !err
      endwhile
    endif
;
  endfor     ;finished plotting OBI jj
;
endfor       ;finished plotting event rates for all OBIs
;
set_xy,plims(0),plims(1),plims(2),plims(3)
!p.title = ptitle
!x.title = xtitle
!x.style = xstyle
;
return
end         ;pro rsevrates
