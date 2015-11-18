;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rsorbit
;
;*PURPOSE:
; A procedure to plot a world map, SAA contours from the PDB, and the 
; Rosat satellite longitudes and latitudes (as read from the .SO files)
; between (and including) specified start and end times
; (do NOT convert files to ST SDAS format!)
;
;*CALLING SEQUENCE:
;	rsorbit,inputs,obinum,plcont=plcont,saamodel=saamodel,
;               x_color=x_color, oparms=oparms
;			or
;	rsorbit,plcont=plcont,saamodel=saamodel,x_color=x_color,oparms=oparms
;
;*PARAMETERS:
; INPUTS:
;       inputs - input parameter description.
;                0 - use all defaults
;                1 - interative selection of parameters
;                string - parameter defintion of the form
;                   'parname1=value1,parname2=value2,,,,'
;                string array - each element of the array has
;                   form 'value' (the values must be in the proper order)
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
;                    (default is SO)
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        TRIM        Determines whether or not data is trimmed to Good
;                    Time Intervals (default = 1)
;        CHATTER     Controls program feedback to user
;                    (default = 1)
;
; OPTIONAL INPUTS:
;       obinum - vector containing numbers of OBIs to be read (0 for all)
;       plcont - keyword telling whether or not to plot continents
;                (1=yes, 0=no)
;	saamodel - keyword vector or scalar of saa model numbers
;       x_color  - keyword vector of colors for the contour levels
;
; OUTPUTS:
;	a super plot
;
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rsorbit,1,obinum,pl=1,saa=[0,1,2,3,4,9]
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,chatter=0'
;        IDL>rsorbit,list,obinum,pl=1,saa=[0,1,2,3,4,9]
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(9)='0'
;        IDL>rsorbit,list,obinum,pl=1,saa=[0,1,2,3,4,9]
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rsorbit,'myinput.dat',obinum,pl=1,saa=[0,1,2,3,4,9]
;
;    This program may also be used to plot only the continents and SAA
;        contours, without reading any orbital data:
;        IDL>rsorbit,pl=1,saa=[0,1,2,3,4,9]
;
;*RESTRICTIONS:
;	Routine attempts to display graphics on terminal.
;
;*NOTES:
;  Will read FITS files directly. Do NOT convert to ST SDAS format.
;  Input parameters EXTYP and TRIM ignored (and need not be specified).
;  PRINTPS should be set to the print command:
;    under VMS, type, e.g. PRINTPS :== print/que=ps/notify/delete
;          (PRINTPS is a system symbol.)
;    under Unix, add this command (or a similar one) to your .cshrc file
;          setenv PRINTPS "lpr -r -s"
;          (PRINTPS is an environmental variable.)
;
;  No longer reads data from CONTINENT.DAT 
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  RSORB_POS
;  MATCH_FILES
;  FITS_INFO
;  READFITS
;  TBGET
;  RSOBITIMES
;  YMD2DN
;  RSORB_PLOT
;
;*MODIFICATION HISTORY:
;    based on HST_ORBIT kindly donated by GHRS team 26 Apr 1991
;    modified for Rosat 29 Apr 1991 by GAR
;    modified to change order of steps, allow user to specify whether
;       or not to plot continents   17 May 1991 by GAR
;    modified to work for HRI 02 Oct 1991 by GAR
;    modifed to use GHRS derived parameter setting routines 08 Oct 1991 GAR
;    modified 08 Nov 1991 for compatibility with Sun Unix (GAR)
;    modified 18 Nov 1991 to use PSPRINT (GAR)
;    modified 04 Feb 1992 to change PSPRINT to PRINTPS (GAR)
;    modified 18 Feb 1992 to be compatible with RSORB_POS (GAR)
;    22 Feb 1992 to be compatible with RSORB_POS (GAR)
;    modified 04 Jun 1992 to return OPARMS, and to be compatible with
;        newest version of RSORB_POS that reads FITS files directly (GAR)
;        also to allow plots of continents & SAA with no data
;    modified 29 Dec 1993 (GAR) to read and plot orbital data from RDF format 
;      ancillary files: calls new version of RSORB_POS (which now returns
;      spacecraft times instead of UT times); also to use intrinsic IDL
;      routines MAP_SET and MAP_CONTINENTS, and to add a title to the plot
;-
;-------------------------------------------------------------------------------
pro rsorbit,inputs,obinum,plcont=plcont,saamodel=saamodel,x_color=x_color,$ 
            oparms=oparms
;
npar = n_params(0)
if (not (keyword_set( saamodel ))) then $
  if (npar lt 1) then begin
    print,' RSORBIT, inputs, obinum (0 for all), plcont=plcont, '
    print,'          saamodel=saamodel, x_color=x_color, oparms=oparms'
    print,'          or'
    print,' RSORBIT, plcont=plcont, saamodel=saamodel, x_color=x_color,', $
          ' oparms=oparms'
    print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, and CHATTER from RSGET.DEF'
    retall
  endif
;
if (n_elements(saamodel) ne 0) then nsaa = n_elements(saamodel) else nsaa=1
if (n_elements(x_color) eq 0) then x_color= indgen(nsaa) + 250
if (n_elements(plcont) eq 0) then plcont = 1      ;default is to plot cont
;
; set up input param if inputs is specified (ie, npar = 1 or more)
;
if (npar ge 1) then begin
  dfile = 'rsget'
  rsgetpar,inputs,dfile,oparms
  obseq = strtrim(oparms(0),2)
  dir = strtrim(oparms(1),2)
  extyp = strtrim(oparms(2),2)
  instr = strtrim(oparms(3),2)
  proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF
  proc = strupcase(proc)
  chatter = fix(oparms(9))
;
  if (obseq eq '') then begin                      ;obseq must be defined
    read,'Please enter root name of input data file: ',obseq
    obseq = strtrim(obseq,2)
    oparms(0) = obseq
  endif  
;
  if (npar lt 2) then obinum = 0            ; default is to read all OBIs
  if (extyp eq '') then extyp = 'so'
;
  instr = strupcase(instr)      
  if (instr eq '') then instr = 'P'
  if (strlen(instr) gt 1) then instr = strmid(instr,0,1)
  if ((instr ne 'P') and (instr ne 'H')) then begin
    instr = ''
    read,' Please specify instrument - P = PSPC or H = HRI',instr
    instr = strupcase(instr)
    if (strlen(instr) gt 1) then instr = strmid(instr,0,1)
    oparms(3) = instr
  endif
;
  if ( (proc ne 'US') and (proc ne 'MPE') and (proc ne 'RDF') ) then begin
    print,proc,' is not a valid choice for processing format.'
    print,' Valid choices are US, MPE, RDF. Returning.'
    retall
  endif
endif
;
; read the orbit if inputs are given (ie, npar = 1 or more)
;
if (npar ge 1) then begin
  if (!debug gt 0) then print,'Now reading orbital info'
  rsorb_pos,oparms,obinum,sctime,satpos,orbvecs,relpos,ob=obinfo
;
  sctbeg = obinfo.sctbeg
  sctend = obinfo.sctend
  nobi = n_elements(sctbeg)
;
  nobseq = strtrim(oparms(0),2)
  if (obseq ne nobseq) then obseq = nobseq    ;obseq may have been corrected
  ndir = strtrim(oparms(1),2)
  if (dir ne ndir) then dir = ndir            ;dir may have been as well
endif
;
; set up limits for plot (use defaults)
;
devname = !d.name
map_set               ;plots longitude from -180 to 180, latitude from -90 to 90
map_grid,latdel=20,londel=20              ;plots long, lat grid lines
;
; read in and plot the continents if desired
;
if (plcont) then map_continents
ptitle = strupcase(obseq)+'  OBI '
addstr = ''
if (nobi ge 2) then $
   for jj=0,nobi-2 do addstr = addstr + strtrim(string(obinum(jj)),2)+', '
addstr = addstr + strtrim(string(obinum(nobi-1)),2)
ptitle = ptitle + addstr
xyouts,!cxmin+(!cxmax-!cxmin)/20,!cymax+(!cymax-!cymin)/40, $
       ptitle,charsize=1.5
;
; read in and set up the saa vertices for all models in the pdb, plot
; the specified model - if no model specified, don't plot
;
if (n_elements(saamodel) ne 0) then begin
  read_saa,lsaa,bsaa
  plot_saa,lsaa,bsaa,saamodel,x_color
endif
;
; now plot the orbit if observation sequence given
;
if (npar ge 1) then rsorb_plot,satpos.lon,satpos.lat,obinfo,obinum,$
                               orb_color=orb_color
;
ans = ''
read,' Do you wish to make a hardcopy of this plot (Y or N)?  ',ans
yesno,ans
if (ans eq 1) then begin
  set_plot,'ps'
  device,/landscape
  map_set               
  map_grid,latdel=20,londel=20 
  if (plcont) then map_continents,mlinethick=3
  if (n_elements(saamodel) ne 0) then plot_saa,lsaa,bsaa,saamodel,x_color
  if (npar ge 1) then rsorb_plot,satpos.lon,satpos.lat,obinfo,obinum,$
                                 orb_color=orb_color
  device,/close
  set_plot,devname
  spawn,'PRINTPS idl.ps'
endif
;
cond1 = 120                              ;X (for tektronix)
cond2 = 88                               ;x (for tektronix)
if (devname eq 'X') then begin
  if (!version.os eq 'vms') then cond1 = 4 $    ;right cursor button
     else cond1 = 2                          ;if Sun, use middle button
endif
;
if (npar ge 1) then begin
  ans = ''
  read,' List info for selected points in the orbit (Y or N)?  ',ans
  yesno,ans
  if (ans eq 1) then begin
    print,' Use the cursor to select points in the orbit'
    if (!version.os eq 'vms') then $
      print,' Hit X (Tektronix) or right cursor button (X) to stop' $
      else $
      print,' Hit middle cursor button to stop' 
    print,'  '
    print,' OBI, SC Time, Long., Lat., Height (km), Az., El., GHA, Dist. (km)'
    print,' Sun, Moon, Sat. unit vectors (X, Y, Z, X, Y, Z components)'
    cursor,xpos,ypos,1
    err = !err
;
    while ( (err ne cond1) and (err ne cond2) ) do begin    ;not X or x
      if (xpos lt 0.) then xpos = xpos + 360.
      distsq = (satpos.lon-xpos)*(satpos.lon-xpos) + $
               (satpos.lat-ypos)*(satpos.lat-ypos)
      if (!debug gt 4) then print,'min distsq = ',min(distsq)
      if ( min(distsq) lt 5.0 ) then begin        ;close enough to orbit
        ipos = where(distsq eq min(distsq))       ;closest pt on orbit
        ipos = ipos(0)
        tsel = sctime(ipos)
        iorb = where( (sctbeg le tsel) and (sctend ge tsel) )
        iorb = iorb(0)
;
        print,'   '
        print,format='(I3,F12.1,6F9.3,F9.2)',obinum(iorb),tsel, $
              satpos(ipos).lon,satpos(ipos).lat,satpos(ipos).height, $
              relpos(ipos).az,relpos(ipos).el,relpos(ipos).gha, $
              relpos(ipos).dist
        print,format='(6x,9F8.4)',orbvecs(ipos).sunx,orbvecs(ipos).suny, $
              orbvecs(ipos).sunz,orbvecs(ipos).moonx,orbvecs(ipos).moony, $
              orbvecs(ipos).moonz,orbvecs(ipos).satx,orbvecs(ipos).saty, $
              orbvecs(ipos).satz
      endif else begin
        print,'   '
        print,format='(" Too far from orbit:  ",3F9.3)',xpos,ypos,min(distsq)
      endelse
      cursor,xpos,ypos,1
      err = !err
    endwhile
  endif
;
endif
;
return
end           ;pro rsorbit
