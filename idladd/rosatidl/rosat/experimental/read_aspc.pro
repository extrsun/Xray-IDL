;+
;
;NAME:
;read_aspc
;
;PURPOSE:
; Read an ASCII spectral file created by WRITE_ASCSPC
;Useful as interim measure for transferring spectra created under VMS to
;Unix for spectral fitting in XSPEC
;
;CATEGORY
;ROSAT IDL tool
;
;CALLING SEQUENCE:
;read_aspc,fname,group,rate,sigrate,time,numpix,chatter=chatter
;
;INPUTS:
; fname     root name of input ASCII file (fname_asc.spc)
;
;OUTPUTS:
; RATE      floating point vector containing extracted spectrum (counts/s)
;           from ROSAT pspc file (as given by make_spec, for example)
; SIGRATE   associated error vector
; TIME      integration time (seconds)
; NUMPIX    area of region (in pixels) from which rate & sigrate 
;           were extracted
; GROUP     channel grouping (set to !group if not specified)
;
;OPTIONAL INPUT PARAMETERS:
; CHATTER   controls program feedback to user ("chattiness")
;
;OPTIONAL OUTPUT PARAMETERS:
;none
;
;COMMON BLOCKS:
;none
;SIDE EFFECTS:
;none
;RESTRICTIONS:
;none
;
;PROCEDURE:
;
;MODIFICATION HISTORY:
;written 02-20-1992 by GAR
;modified 13 July 1992 (GAR) to be compatible with READ_SPEC
;
;-
pro read_aspc,fname,group,rate,sigrate,time,numpix
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' READ_ASPC, fname, GROUP, RATE, SIGRATE, TIME, NUMPIX, chatter=chatter'
  retall
endif
if (n_elements(chatter) eq 0) then chatter = 1
;
if (chatter eq 1) then $
   print,' Reading spectral data from ASCII file ',fname
;
get_lun,un
openr,un,fname
readf,un,nchan,time,numpix
;
group = intarr(nchan,2)
rate = fltarr(nchan)
sigrate = rate
for ii=0,nchan-1 do begin
    readf,un,g1,g2,r,sigr
    group(ii,0) = g1
    group(ii,1) = g2
    rate(ii) = r
    sigrate(ii) = sigr
endfor
close,un
free_lun,un
;
return
end             ;pro read_aspc
