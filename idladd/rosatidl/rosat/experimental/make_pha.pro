;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       make_pha
;
;*PURPOSE:
; A procedure to convert spectral rate (counts/s) and associated error 
; arrays into XSPEC format.
;
;*CALLING SEQUENCE:
;       make_pha,inputs,rate,sigrate,time,numpix,group=group
;
;*PARAMETERS:
; RATE     floating point vector containing extracted spectrum (counts/s)
;          from ROSAT pspc file (as given by make_spec, for example)
; SIGRATE  associated error vector
; TIME     integration time (seconds)
; NUMPIX   area of region (in pixels) from which rate & sigrate 
;          were extracted
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
;       ZDEF:MAKE_PHA.DEF
;
;       The following parameters are availble.
;
;        FNAME       name of output PHA file (without extension)
;        BKFIL       name of associated background file (default = NONE)
;        RSPFIL      name of associated response file (default = NONE)
;        CORFIL      name of associated correction file (default = NONE)
;
; OPTIONAL INPUTS:
;   GROUP    2d array giving beginning and ending channel boundaries
;            (default = Rosat 34 channel grouping)
;
; OUTPUTS:
;    Writes an XSPEC-readable (SF) .pha file containing the spectrum and 
;    associated errors.
;    Writes file in standard format readable by XSPEC.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>make_pha,1,rate,sigr,time,numpix
;             ?fname=n1234_src
;	      ?bkfil=n1234_bkg
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='fname=n1234_src,bkfil=n1234_bkg'
;        IDL>make_pha,list,rate,sigr,time,numpix
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,5)
;        IDL>list(0)'n1234_src'
;        IDL>list(1)='n1234_bkg'
;        IDL>make_pha,list,rate,sigr,time,numpix
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            fname=n1234_src
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;            *exit
;        IDL>make_pha,'myinput.dat',rate,sigr,time,numpix
;
;*RESTRICTIONS:
; none
;
;*NOTES:
; The defaults for the response matrices are set by the routine set_respfiles
; These are the names of the response matrices available for Rosat PSPC data
;    at the GSFC HEASARC
;  If you are running these programs at a different institution, then you
;     may wish to change the routine set_respfiles
;
;*SUBROUTINES CALLED:
;  SET_RESPFILES
;
;*MODIFICATION HISTORY:
;    written 12-7-91 by M.F. Corcoran
;    12-11-91 made /xanadu/spectral/rosat/pspc/gh.rsp the default response
;    12-11-91 changed grouping of channels 1-6 and 248-* to '.' from '*'
;    01-24-92 modified by GAR to allow arbitrary channel groupings and numbers
;             of pixels in region
;    02-08-92 changed to use defaults parameter file ZDEF:MAKE_PHA.DEF (GAR)
;    03-03-92 to fix bug when creating .pha file under VMS, and to include
;             new PSPC response matrices
;    19 Jan 93 (GAR) to update list of response matrices (to reflect changed
;              directories, mainly), & change list of response matrices so
;              that only the list appropriate for !version.os are used, &
;              to call set_respfiles, & to fix the logic by which rspfil is
;              defined
;              Documentation header also was updated
;-
;-------------------------------------------------------------------------------
pro make_pha,inputs,rate,sigrate,time,numpix,group=group
;
if (n_params(0) eq 0) then begin
   print,'MAKE_PHA, inputs, rate, sigrate, time, numpix, group=group'
   retall
end
if (n_elements(group) eq 0) then group = !group     ;use !group as default
;
dfile = 'make_pha'
rsgetpar,inputs,dfile,oparms
fname = strtrim(oparms(0),2)
bkfil = strtrim(oparms(1),2)
rspfil = strtrim(oparms(2),2)
corfil = strtrim(oparms(3),2)
;
if (bkfil eq '') then bkfil = 'none'
if (corfil eq '') then corfil = 'none'
set_respfiles,respopts,nresp
case 1 of
  (strupcase(rspfil) eq '') or (strupcase(rspfil) eq 'LIST'): begin
     ans = ''
     print,' Here are the names of some standard ('+!version.os+ $
           ') response matrices: '
     print,'  '
     forprint,indgen(nresp),'  '+respopts
     print,'  '
     print,'  Enter the number, or a number < 0 to use a different file,'
     read,'     or hit return to use the first file as default >',ans
     iresp = fix(ans)
     end
  (strupcase(rspfil) eq 'DEF'): iresp = 0
  (strupcase(rspfil) eq '0'): iresp = 0
  (strupcase(rspfil) eq '1'): iresp = 1
  (strupcase(rspfil) eq '2'): iresp = 2
  (strupcase(rspfil) eq '3'): iresp = 3
  (strupcase(rspfil) eq '4'): iresp = 4
  (strupcase(rspfil) eq '5'): iresp = 5
  (strupcase(rspfil) eq '6'): iresp = 6 
  (strupcase(rspfil) eq '7'): iresp = 7 < nresp
  else: iresp = 99999                            ;assume it's a file name
endcase
;
case 1 of
  (iresp lt 0): begin            ;use a nonstandard response file
     read,' Enter name of response file (return for default) >',ans
     if (ans ne '') then rspfil = ans else rspfil = respopts(0)
     end
  (iresp le (nresp-1)): rspfil = respopts(iresp)
  else: begin
     end
endcase
;
if (fname eq '') then begin                  ;make sure fname is defined
  fname=''
  read,'Enter filename (w/o extension): ',fname
endif
openw,lun,fname+'.pha',/get_lun,/f77,/segmented
;
if (n_elements(time) eq 0) then $
   read,' Enter integration time (sec) >',time
time = float(time)
if (n_elements(numpix) eq 0) then $         ;number of pixels not defined
   read,' Enter number of pixels in extraction region >',numpix
numpix = float(numpix)
;
hdr =  'SF01XSPEC data            '+!stime
hdr = hdr+padder(78-strlen(hdr))
writeu,lun,hdr
;
;  write ass. package
;
len = -28L
pktyp = 'ass. files'
pktyp = pktyp+padder(12-strlen(pktyp))
writeu,lun,len,pktyp,long(1),long(3),padder(8)
writeu,lun,strlen(bkfil),bkfil
writeu,lun,strlen(rspfil),rspfil
writeu,lun,strlen(corfil),corfil
;
; write file info package
;
len = -128L
detid = 'ROSAT PSPC'
detid = detid + padder(16-strlen(detid))
nbin = long(n_elements(rate)) & nchan=256L
;
cor = 0.0
indices = fltarr(3)*0.
spare = intarr(48)*0
pktyp = 'file info'
pktyp = pktyp + padder(12-strlen(pktyp))
if (!debug eq 1) then stop,' Stopping in make_pha after pktyp defined.'
a = 1.0
writeu,lun,len,pktyp,long(1),long(0),padder(8), $
       detid,nbin,nchan,time,a,numpix,cor,indices,spare
;
; write grouping package
;
len = -28L-256L
pktyp = 'grouping'
pktyp = pktyp + padder(12-strlen(pktyp))
grp = strarr(257,1)
grp(1:*) = '-'
grp(group(*,0)) = '+'
grp(1:6) = '.'
grp(248:*) = '.'
writeu,lun,len,pktyp,long(1),long(0),padder(8),grp(1:*)
;
; write pha per second package 
;
len = -4L*n_elements(rate)-28L
pktyp = 'pha per sec'
pktyp = pktyp+padder(12-strlen(pktyp))
writeu,lun,len,pktyp,long(1),long(0),padder(8),float(rate)
;
; write pha errors package
;
pktyp = 'pha errors'
pktyp = pktyp + padder(12-strlen(pktyp))
writeu,lun,len,pktyp,long(1),long(0),padder(8),float(sigrate)
close,lun ; all data written
;
return
end                ;pro make_pha
