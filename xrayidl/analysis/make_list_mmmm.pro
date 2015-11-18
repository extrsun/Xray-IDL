;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       make_list
;
;*PURPOSE:
; Retrieve data from a ROSAT photon list file and store in a photon list
; data structure. Allows only data with X & Y values within xmin-xmax,
; ymin-ymax, and times within tmin-tmax, to be retrieved.
; (do NOT convert file to ST SDAS format!)
;
;*CALLING SEQUENCE:
;       make_list,inputs,plist,plinfo,plhdr,oparms=oparms,tfile=tfile
;  	,xytpionly=,xshift=xshift,yshift=yshift,ashift=ashift
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
;       ZDEF:MAKE_LIST.DEF.
;
;       The following parameters are available.
;
;        OBSEQ       Root name of input data file 
;                    (null string not allowed, must be specified)
;        DIR         Directory containing input file 
;                    (default is current directory)
;        EXTYP       Extension of input file (e.g., FITS, MDS)
;                    (default is FITS for US files, TFITS for MPE files)
;        PROC        Format of processed files (e.g., US, MPE)
;                    (default is US)
;        TRIM        Determines whether or not data is trimmed to Good
;                    Time Intervals (default = Y)
;        CHATTER     Controls program feedback to user
;                    (default = 1)
;        XMIN        Minimum X value for photon list.
;        XMAX        Maximum X value for photon list
;                    (defaults = 0 for min & max X values of list)
;        YMIN        Minimum Y value for photon list.
;        YMAX        Maximum Y value for photon list
;                    (defaults = 0 for min & max Y values of list)
;        PIXEL       Pixel convention. = UNBL for unblocked, BL for blocked
;                    pixels. (def = UNBL)
;        BLOCK       Blocking factor for pixels. (def = 1 for UNBL pixels)
;        CTYP        Tells whether photons will be selected by detector (DET)
;                    or sky (SKY) coordinates. (def = SKY)
;
;	tfile - the name of the file containing time intervals to be used
;		for timing filtering.
;	xytpionly - if set, the list will include only the x, y, time, and
;		pi energy channel information.
;
; OUTPUTS:
;       plist    data structure containing x & y, dx & dy positions,
;                arrival times, and pha & phi channels of photons.
;                has the structure of replicate(row,num), where
;                row={xevent,x:0,y:0,pha:0,pi:0,time:0.0D0,dx:0,dy:0}
;                and num is the total number of photons
;       plinfo   data structure containing info concerning extraction.
;                has the structure {xevinfo,obseq:'',filename:'',proc:'',
;                ctyp:'',xmin:0.0,xmax:0.0,ymin:0.0,ymax:0.0,
;                region:'',numpix:0.0D0,
;                totexp:0.0D0,ntimes:0,tbeg:dblarr(200),tend:dblarr(200)}
;
;         where  xmin,xmax,ymin & ymax are limits of box containing region,
;                region is the ASCII descriptor for the extraction region,
;                numpix = area of region (in pixels) of extraction region,
;                totexp = total time interval for extraction,
;                ntimes = number of time intervals, tbeg & tend are vectors
;                containing start & stop times of extraction intervals
;
;       plhdr  - the FITS header for the events file
;                
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*PROCEDURE:
;   Reads the photon list info from the SDAS formatted file and creates
;   structure. Allows user to select data within a specified rectangle
;   or time interval first, before storing in the photon list. 
;   This will save memory when dealing with large photon lists.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>make_list,1,plist,plinfo,plhdr
;             ?obseq=rp123456
;             ?dir=mydir
;	      ?xmin=240.
;             ?xmax=272.
;             ?ymin=240.
;             ?ymax=272.
;             ?pixel=bl
;             ?block=30
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydir,xmin=240.,xmax=272.'
;        IDL>list=list+',ymin=240.,ymax=272.,pixel=bl,block=30'
;        IDL>make_list,list,plist,plinfo,plhdr
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,25)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydir'
;        IDL>list(12)=240.
;        IDL>list(13)=272.
;        IDL>list(14)=240.
;        IDL>list(15)=272.
;        IDL>list(16)='bl'
;        IDL>list(17)=30
;        IDL>make_list,list,plist,plinfo,plhdr
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;            block=30
;            *exit
;        IDL>make_list,'myinput.dat',plist,plinfo,plhdr
;
;*RESTRICTIONS:
;   Only FITS files allowed for now.
;
;*NOTES:
;   Will read events from FITS file directly. Do NOT convert to ST SDAS format.
;   When reading in events from a large events file, it will generally be
;     more efficient to read in the data for all times, and time filter
;     afterwards.
;   Only the first 200 time intervals will be stored in structure variable 
;     plinfo (tbeg and tend)
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  HEADFITS
;  READFITS
;  FITS_GET
;  TBGET
;  FTGET
;
;*MODIFICATION HISTORY:
;    written 10 Feb 92 (GAR)
;    modified 19 Feb 1992 to return tbeg & tend (GAR)
;    modified 12 Jun 1992 to use READFITS, new version of MATCH_FILES, 
;      and to return OPARMS (GAR)
;    modified 26 Jun 1992 to use new READFITS (to read in parts of the
;      events file) (GAR)
;    modified 10 Jul 1992 to use input variable BLOCK (replaces ZOOM), and
;      to allow selection by detector coordinates
;    modified 25 Aug 1992 (GAR) to read MPE format files (input variable PROC 
;      added; most calls to TBGET changed to FITS_GET & variable FTYPE used)
;      Several bugs in old version also fixed
;   modified 11 Oct 1992 (GAR) to fix bug in defining ftype (from 25 Aug)
;      and to combine numpix, tbeg, tend, and other info in structure variable 
;      plinfo, and to return FITS header for events file
;   modified 29 Oct 1992 (GAR) to fix bug in defining variable CTYP
;
;    fix bugs as indicated in the program and include an energy filter
;    Aug 1992 (WQD)
;
;    allow the time trims of the photon list using a file (*_gti.dat)
;    containing good time intervals produced in MAKE_EXPMAP Sept 4 1992 (WQD)
;    add a keyword XYTPIONLY to save time.
;    The modifications made by wqd are copied to the revised make_list
;    procedure which can be used to read MPE data. Some further modifications
;	have been made, including: the shift of MPE SKY coordinates to be
;	same as those used in US data; the reverse of the MPE Y axis.
; 	WQD, July 10,1993
;    add keyword xshift,yshift,ashift to make boresight corrections for
;    the x,y pixel coordinates of individidual counts. by wqd, June 8, 1994
;    A bug is fixed, moving the shift_xya outside the loop. wqd, June, 19, 1994
;    copy sections from the make_list.pro to accormodate the RDF, 
;	wqd, 7/14/94
;-
;-------------------------------------------------------------------------------
pro make_list,inputs,plist,plinfo,plhdr,oparms=oparms,tfile=tfile,xytpionly=xytpionly,xshift=xshift,yshift=yshift,ashift=ashift
;
COMMON descriptor,file_descript
;
if (n_params(0) eq 0) then begin
  print,'MAKE_LIST, inputs, PLIST, PLINFO, PLHDR, [OPARMS=OPARMS],tfile=tfile'
  print,',xytpionly=xytpionly  '
  print,'   Uses inputs OBSEQ, DIR, EXTYP, PROC, TRIM, CHATTER, XMIN, XMAX, '$
       +'YMIN, YMAX,'
  print,'               PIXEL, BLOCK, TMIN, TMAX, and CTYP from make_list.def'
  print,'  '
  retall
end
;
if n_elements(xshift) eq 0 then xshift=0
if n_elements(yshift) eq 0 then yshift=0
if n_elements(ashift) eq 0 then ashift=0

dfile = 'make_list'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF
if proc eq '' then proc=!proc
proc = strupcase(proc)
;extyp = '.fits'                         ;only FITS extension allowed for now
;if (proc eq 'MPE') then extyp = '_events.tfits'
trim = strtrim(oparms(8),2)
chatter = fix(oparms(9))
;
xlim = [float(oparms(12)),float(oparms(13))]
ylim = [float(oparms(14)),float(oparms(15))]
;
pixel = strtrim(oparms(16),2)
if (pixel eq '') then pixel = 'UNBL'      ;default is for unblocked pixels
if (strupcase(pixel) ne 'UNBL') then begin
  block = fix(oparms(17))
  if (block eq 0) then block = 1
endif else block = 1
;
ctyp = strupcase(strtrim(oparms(18),2))
if (ctyp eq '') then ctyp = 'SKY'         ;default is select by sky coord.
ans = ''
while ( (ctyp ne 'DET') and (ctyp ne 'SKY') ) do begin
  print,ctyp,' is not a valid option for coordinate selection.'
  print,' Valid options are SKY, DET.'
  read,' Enter a valid option or type q to quit',ans
  if ( (ans eq 'q') or (ans eq 'Q')) then retall
endwhile  
;
tmin = double(oparms(21))
tmax = double(oparms(22))
emin=fix(oparms(23)) ;added by wqd
emax=fix(oparms(24)) ;added by wqd
phaorpi=fix(oparms(25))
;
xlim = block*xlim                            ;change to unblocked pixels
ylim = block*ylim
xmin = long( xlim(0) - 1.*(xlim(0) lt 0) )   ;find integers which completely
xmax = long( xlim(1) + 1.*(xlim(1) gt 0) )   ;enclose the specified region
ymin = long( ylim(0) - 1.*(ylim(0) lt 0) )
ymax = long( ylim(1) + 1.*(ylim(1) gt 0) )
;
; now read photon list. Use match_files.
;
;--------------
extyp = 'fits'                         ;only FITS extension allowed for now
case proc of
  'US': ext = '.'+extyp
  'MPE': ext = '_events.t'+extyp
  'MPEUS': ext = '.'+extyp
  'RDF': ext = '_bas.'+extyp
  else : begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, MPE, RDF. Returning.'
         retall
         end
endcase
;
; Now use match_files to get the whole file name for the photon list.
;
match_files,dir,obseq,ext,name,nlist        ;look for the right files
;-------------
name = name(0)                              ;default = take latest version
oparms(0) = strtrim(obseq,2)        ;obseq & dir may have been corrected
oparms(1) = strtrim(dir,2)
;
;name = names(0)
if (chatter) then $
   print,' Reading photon list data from file ',name
;
fits_info,name,/sil
dum = file_descript
while (dum ne '') do try = gettok(dum,'BINTABLE')
dum = try
while (dum ne '') do try = gettok(dum,' ')
nphot = long(try)
;
xsel = abs(xmin) + abs(xmax)
ysel = abs(ymin) + abs(ymax)
tsel = abs(tmin) + abs(tmax)
esel = abs(emin) + abs(emax) ;added by WQD Aug 1992
shiftsel = abs(xshift)+abs(yshift)+abs(ashift)
allsel = xsel + ysel + tsel+esel+shiftsel
;
; read in header information - from 3rd table for US processing, 1rst for MPE
;
;-------------------
 if (proc eq 'MPE') then mpeshift=7680 $ ;shift SKY pixel values to be same as 
	else mpeshift=0		;those in US data. added by wqd July,12,93
;-----------------
case proc of
  'US': extnum = 3
  'MPE': extnum = 1
  'RDF': extnum = 2
  'MPEUS': extnum = 2
endcase
plhdr = headfits(name,ext=extnum)
;
start = 0 
;num = 20000. 
num = 30000L < nphot              ;in case fewer than NUM photons in file
; 32767 is the maximum value allowed by fits_get
nct = 0 
stop = 0
;----------
;  rows = rows(0:nphot-1) 
  yev = [-999]
  xev = [-999]
  dyev =[-999] 
  dxev =[-999]
  phaev =[-999]
  piev=[-999]
  tev =[-999d]
;=======================================================
if (allsel eq 0) then begin
;------------------------------------
;shifts don't effect in this part
  if (chatter) then $
     print,' Now reading data for ',nphot,' photons'
;
;  yev = intarr(nphot)
;  xev = yev
;  dyev = yev 
;  dxev = yev 
;  phaev = yev 
;  piev = yev
;  tev = dblarr(nphot)
;
; if (nphot lt num) then num = nphot       ;fewer than NUM photons in file
  while ( (start lt nphot) and (stop eq 0) ) do begin
    hdr = 0 & tab = 0 
    tab=readfits(name,hdr,ext=extnum,/sil,st=start,num=num)
;
    case proc of
      'US': ftype = 'y'
      'MPE': ftype = 'ypix'
      'RDF': ftype = 'y'
    endcase
    if (proc eq 'MPE') then begin
    	yev = [yev,mpeshift-fits_get(hdr,tab,ftype)] ;y-axis revised for MPE data
    endif else yev= [yev,  fits_get(hdr,tab,ftype)] 	

    case proc of
      'US': ftype = 'x'
      'MPE': ftype = 'xpix'
      'RDF': ftype = 'x'
    endcase
    if (proc eq 'MPE') then xev= [xev,  fits_get(hdr,tab,ftype)+mpeshift] $
	else xev= [xev,  fits_get(hdr,tab,ftype)]

    tev= [tev,  fits_get(hdr,tab,'time')]       
;
   if n_elements(xytpionly) eq 0 then begin ;to save time. wqd July 10, 1992
    case proc of
      'US': ftype = 'dx'
      'MPE': ftype = 'xdet'
      'RDF': ftype = 'detx'
      'MPEUS': ftype = 'xdet'
    endcase
    dxev= [dxev,  fits_get(hdr,tab,ftype)]
    case proc of
      'US': ftype = 'dy'
      'MPE': ftype = 'ydet'
      'RDF': ftype = 'dety'
      'MPEUS': ftype = 'dety'
    endcase
    dyev= [dyev,  fits_get(hdr,tab,ftype)]
    case proc of
      'US': ftype = 'pha'
      'MPEUS': ftype = 'pha'
      'MPE': ftype = 'raw_ampl'
      'RDF': ftype = 'pha'
    endcase
    phaev= [phaev,  fits_get(hdr,tab,ftype)]
   endif
    case proc of
      'US': ftype = 'pi'
      'MPEUS': ftype = 'pi'
      'MPE': ftype = 'ampl'
      'RDF': ftype = 'pi'
    endcase
    piev= [piev,  fits_get(hdr,tab,ftype)]
;
    nct = nct + num
;    start = start + num
;  endwhile
    start=nct ;added Aug 1992 (WQD)

  endwhile
    nct=n_elements(yev(1:*)) ;added Aug 1992 (WQD)
;
  ymin = min(yev(1:*))
  ymax = max(yev(1:*))
  xmin = min(xev(1:*))
  xmax = max(xev(1:*))
;-----------------------------------------
endif else begin
  if (chatter) then $
     print,' Now checking limits for ',nphot,' photons'
;
;  rows = lonarr(nphot)
  ybot = ymin & ytop = ymax & xbot = xmin & xtop = xmax
;  tbot = tmin & ttop = tmax 
  tbot = tmin & ttop = tmax & ebot=emin & etop=emax ;added by wqd
  if (tmin lt 0.0) then begin ;for a timing filtering. added by wqd
	if n_elements(tfile) eq 0 then tfile=obseq+'_gti.dat'
      	print,'Open good time interval file ',tfile
;      	openr,un,!data_dir+tfile,/get_lun
      	openr,un,dir+tfile,/get_lun
      	ninterval=0
      	readf,un,ninterval
      	tinterval=dblarr(2,ninterval)
      	readf,un,tinterval
      	free_lun,un
  endif 
;
;  while ( (start lt nphot) and (stop eq 0) ) do begin
  while ( start lt nphot) do begin ;stop is of no use
;    hdr = 0 & tab = 0 & try = 0 
;    indy = 0 & indx = 0 & indt = 0
;
    if (chatter) then $
	finish=start+num-1 < (nphot-1)
       print,' Checking limits for photons ',start,' to ',finish
    tab=readfits(name,hdr,ext=extnum,/sil,st=start,num=finish-start+1)
    np = num
    check = 1
    indy = indgen(finish-start+1)  ;add by wqd, April 26, 1996
				   ;in case ysel eq 0
;
  ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  if shiftsel ne 0 then begin
      ; making boresight corrections for individual counts
      if (proc eq 'MPE') then begin 
	ftype = 'ypix' 
        ytry = mpeshift-fits_get(hdr,tab,ftype)
	ftype='xpix' 
        xtry = fits_get(hdr,tab,ftype)+mpeshift 
      endif else begin
	ftype='y'
	ytry = fits_get(hdr,tab,ftype)
	ftype='x'
        xtry = fits_get(hdr,tab,ftype)
      endelse

      shift_xya,xtry,ytry,xshift=xshift,yshift=yshift,ashift=ashift   
      if (ymin eq 0) then ybot = min(ytry)
       if (ymax eq 0) then ytop = max(ytry) 
       if (xmin eq 0) then xbot = min(xtry)
       if (xmax eq 0) then xtop = max(xtry)

      indy = where( (ytry ge ybot) and (ytry le ytop) and $
		(xtry ge xbot) and (xtry le xtop) )
      if (indy(0) lt 0) then begin
        check = 0
        np = 0
      endif

  endif else begin

    if (ysel ne 0) then begin
      if (!debug gt 2) then print,'           Checking Y'
	if (ctyp eq 'DET') then begin 
		    case proc of
      			'US': ftype = 'dx'
      			'MPE': ftype = 'xdet'
		         'MPEUS': ftype = 'detx'
      			'RDF': ftype = 'detx'
    		    endcase
              try = fits_get(hdr,tab,ftype)
	endif else begin
	      if (proc eq 'MPE') then begin
		ftype = 'ypix' 
    	        try =mpeshift-fits_get(hdr,tab,ftype)
	      endif else begin
		ftype='y'
		try = fits_get(hdr,tab,ftype)
	      endelse
        endelse
;-----------------------
      if (ymin eq 0) then ybot = min(try)
      if (ymax eq 0) then ytop = max(try)
      indy = where( (try ge ybot) and (try le ytop) )
      if (indy(0) lt 0) then begin
        check = 0
        np = 0
      endif

    endif ; else indy = indgen(finish-start+1)  
		;to avoid problems in tbget. Aug 1992 (WQD)
    if (!debug gt 2) then print,'    After Y check: check, np:',check,np
    if ( (check) and (!debug gt 2) ) then $
       print,'    Y check: ',minmax(try(indy)),ybot,ytop
;
    if ( (xsel ne 0) and (check) ) then begin
      if (!debug gt 2) then print,'           Checking X'
      try = 0
;---------------
	if (ctyp eq 'DET') then begin 
	      	    case proc of
      			'US': ftype = 'dx'
      			'MPE': ftype = 'xdet'
			'MPEUS': ftype = 'detx'
      			'RDF': ftype = 'detx'
    		    endcase
              try = fits_get(hdr,tab,ftype,indy)
	endif else begin
	      if (proc eq 'MPE') then begin
		ftype='xpix' 
	 	try = fits_get(hdr,tab,ftype,indy)+mpeshift    
	      endif else begin
		ftype='x' 
		try = fits_get(hdr,tab,ftype,indy)
	      endelse
        endelse
;-----------------------
      if (xmin eq 0) then xbot = min(try)
      if (xmax eq 0) then xtop = max(try)
      indx = where( (try ge xbot) and (try le xtop) )
      if (indx(0) lt 0) then begin
        check = 0 
        np = 0
      endif else indy = indy(indx)
    endif
    if (!debug gt 2) then print,'    After X check: check, np:',check,np
    if ( (check) and (!debug gt 2) ) then $
       print,'    X check: ',minmax(try(indy)),xbot,xtop

  endelse

  ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    if ( (tsel ne 0) and (check) ) then begin
      if (!debug gt 2) then print,'           Checking T'
      try = 0
      indx = 0
      try = fits_get(hdr,tab,'time',indy)        
;      if (tmin eq 0) then tbot = min(try)
;      if (tmax eq 0) then ttop = max(try)
;     indt = where( (try ge tbot) and (try le ttop) )
;-------------------------------------------
     if(tmin ge 0.) then begin ;replaced by wqd
      	if (tmin eq 0) then tbot = min(try)
      	if (tmax eq 0) then ttop = max(try)
      	indt = where( (try ge tbot) and (try le ttop) )
     endif else begin
      	indt=intarr(n_elements(try))
      	kk=0
      	for k=0,ninterval-1 do begin
       	  sel=where((try ge tinterval(0,k)) and (try le tinterval(1,k)),nk)
          if nk ne 0 then indt(kk:kk+nk-1)=sel
          kk=kk+nk
      	endfor
      	indt=indt(0:kk-1)
      	indt=indt(sort(indt))
      	sel=0
     endelse
;-------------------------------------------------------------
      if (indt(0) lt 0) then begin
        check = 0
        np = 0 
      endif else indy = indy(indt)
    endif
    if (!debug gt 2) then print,'    After T check: check, np:',check,np
    if ( (check) and (!debug gt 2) ) then $
       print,'    T check: ',minmax(try(indy)),tbot,ttop
;---------------------------------------
; esel is added by wqd. Aug 1992
    if ( (esel ne 0) and (check) ) then begin
      try = 0
      indx = 0
      if phaorpi eq 1 then begin
	ftype = 'pha'
    	if (proc eq 'MPE') then ftype = 'raw_ampl'
	try = fits_get(hdr,tab,ftype,indy) 
      endif else begin
	ftype = 'pi'
    	if (proc eq 'MPE') then ftype = 'ampl'
 	try = fits_get(hdr,tab,ftype,indy)
      endelse    
      if (emin eq 0) then ebot = min(try)
      if (emax eq 0) then etop = max(try)
      inde = where( (try ge ebot) and (try le etop) )
      if (inde(0) lt 0) then begin
        check = 0
        np = 0 
      endif else indy = indy(inde)
    endif
;-------------------------------------
    if (check) then begin
      if (!debug gt 2) then print,'     Adding to rows: nct, np, start:', $
         nct,np,start
;      rows(nct) = indy + start
      np = n_elements(indy)
    endif
;
    nct = nct + np
    start = start + num
;    stop = (indy(0) lt 0)*nct 
;    stop = (ctyp eq 'SKY')*(indy(0) lt 0)*nct
; the above line is removed on Jan 9, 1995, which is useless and
; truncates the data reading before finishing reading the tab
;
;    if ( (chatter) and (not stop) and (np ne 0) ) then $
    if ( (chatter) and (np ne 0) ) then $
       print,np,' photons found within specified limits'


if np ne 0 then begin
    if (proc eq 'MPE') then begin
	ftype = 'ypix' 
    	yev = [yev,mpeshift-fits_get(hdr,tab,ftype,indy)]
    endif else begin
        ftype = 'y'
	yev= [yev, fits_get(hdr,tab,ftype,indy)]
    endelse

    if (proc eq 'MPE') then begin
	ftype = 'xpix' 
     	xev= [xev, fits_get(hdr,tab,ftype,indy)+mpeshift]
    endif else begin
	ftype = 'x'
	xev= [xev, fits_get(hdr,tab,ftype,indy)]
    endelse

    tev= [tev, fits_get(hdr,tab,'time',indy)]

    ftype = 'pi'
    if (proc eq 'MPE') then ftype = 'ampl'
    piev= [piev, fits_get(hdr,tab,ftype,indy)] 
;---------------------------------------------------
   if n_elements(xytpionly) eq 0 then begin ;to save time. wqd July 10, 1992
    case proc of
      'US': ftype = 'dx'
      'MPE': ftype = 'xdet'
      'MPEUS': ftype = 'detx'
      'RDF': ftype = 'detx'
    endcase
    dxev= [dxev, fits_get(hdr,tab,ftype,indy)]
    case proc of
      'US': ftype = 'dy'
      'MPE': ftype = 'ydet
      'MPEUS': ftype = 'dety'
      'RDF': ftype = 'dety'
    endcase
    dyev= [dyev, fits_get(hdr,tab,ftype,indy)]
    ftype = 'pha'
    if (proc eq 'MPE') then ftype = 'raw_ampl'
    phaev= [phaev, fits_get(hdr,tab,ftype,indy)]
   endif
endif
;
;    ib = ie + 1
;    start = start + num
  endwhile
endelse
;  rows = rows
  yev = yev(1:*)
  xev = xev(1:*)

    if shiftsel ne 0 then $
      shift_xya,xev,yev,xshift=xshift,yshift=yshift,ashift=ashift   
 if n_elements(xytpionly) eq 0 then begin
  dyev = dyev(1:*)
  dxev = dxev(1:*) 
  phaev = phaev(1:*)
 endif
  piev=piev(1:*)
  tev = tev(1:*)

;  endwhile
  nphot = nct
;
;
if (proc eq 'US') then begin              ;Use header info, if US processing
  axlen1 = sxpar(plhdr,'AXLEN1')
  axlen2 = sxpar(plhdr,'AXLEN2')
  xdet = sxpar(plhdr,'XS-XDET')
  ydet = sxpar(plhdr,'YD-DET')
  if (xsel eq 0) then begin
    xmin = 0.
    xmax = axlen1*(ctyp eq 'SKY') + xdet*(ctyp eq 'DET')
  endif
  if (ysel eq 0) then begin
    ymin = 0.
    ymax = axlen2*(ctyp eq 'SKY') + ydet*(ctyp eq 'DET')
  endif
endif else begin  
  if (xsel eq 0) then begin
    xmin = min(xev)
    xmax = max(xev)
  endif
  if (ysel eq 0) then begin
    ymin = min(yev)
    ymax = max(yev)
  endif
endelse
;
;if (tsel eq 0) then begin
;if (tmax eq 0) then begin ;replaced by wqd because tmin can be < 0
;  tmin = min(tev)
;  tmax = max(tev)
;endif
;
 if n_elements(xytpionly) eq 0 then begin ;to save time. wqd July 10, 1992
row = {xevent,x:0,y:0,pha:0,pi:0,time:0.0D0,dx:0,dy:0}
plist = replicate(row,nct)
plist.x = xev
plist.y = yev
plist.pha = phaev
plist.pi = piev
plist.time = tev
plist.dx = dxev
plist.dy = dyev
endif else begin
row = {xeve,x:0,y:0,pi:0,time:0.0D0}
plist = replicate(row,nct)
plist.x = xev
plist.y = yev
plist.pi = piev
plist.time = tev
endelse
;
numpix = (ymax - ymin + 1)*(xmax - xmin + 1)
hdr=0 & tab=0
if (tmin ge 0.0) then begin ;for a timing filtering. added by wqd
  tbeg = min(tev) ;corrected by wqd 
  tend = max(tev)
endif else begin
  tbeg=tinterval(0,*)              ;end wqd, Dec 1, 1993
  tend=tinterval(1,*)
endelse
;  
; now store information in structure variable plinfo
;
xcen = (xmin + xmax)/2.
xdel = xmax - xmin
ycen = (ymin + ymax)/2.
ydel = ymax - ymin
;
plinfo = {xevinfo,obseq:'',filename:'',proc:'',ctyp:'', $
          xmin:0.0,xmax:0.0,ymin:0.0,ymax:0.0,region:'',numpix:0.0D0, $
          totexp:0.0D0,ntimes:0,tbeg:dblarr(200),tend:dblarr(200)}
plinfo.obseq = oparms(0)
plinfo.filename = name
plinfo.proc = proc
plinfo.ctyp = ctyp
;
plinfo.xmin = xmin
plinfo.xmax = xmax
plinfo.ymin = ymin
plinfo.ymax = ymax
;
f = '$(f12.3)'
region = 'box ('+string(form=f,xcen)+','+string(form=f,ycen)+','
region = region+string(form=f,xdel)+','+string(form=f,ydel)+')'
plinfo.region = region
plinfo.numpix = numpix
;
totexp = total(tend - tbeg + 1)
ntimes = n_elements(tbeg)
plinfo.totexp = totexp
plinfo.ntimes = n_elements(tbeg)
ntimes = ntimes < 200
plinfo.tbeg = tbeg(0:ntimes - 1)
plinfo.tend = tend(0:ntimes - 1)
return
end                ;pro make_list
