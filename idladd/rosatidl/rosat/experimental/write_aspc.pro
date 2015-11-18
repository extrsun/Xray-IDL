;+
;
;NAME:
;write_aspc
;
;PURPOSE:
;Given channel groups, count rates and uncertainties, write an ASCII file
;Useful as interim measure for transferring spectra created under VMS to
;Unix for spectral fitting in XSPEC
;
;CATEGORY
;ROSAT IDL tool
;
;CALLING SEQUENCE:
;write_aspc,rate,sigrate,time,numpix,fname=fname,group=group
;
;INPUTS:
; rate      floating point vector containing extracted spectrum (counts/s)
;           from ROSAT pspc file (as given by make_spec, for example)
; sigrate   associated error vector
; time      integration time (seconds)
; numpix    area of region (in pixels) from which rate & sigrate 
;           were extracted
; fname     root name of output ASCII file (fname_asc.spc)
; group     channel grouping (set to !group if not specified)
;
;OPTIONAL INPUT PARAMETERS:
;none
;
;OUTPUTS:
; An ASCII format file for ftp transfer (filename_asc.spc) which can be
; transferred via ftp and converted to an XSPEC format .pha file
; Use READ_ASCSPC to read data into IDL.
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
;
;-
pro write_aspc,rate,sigrate,time,numpix,fname=fname,group=group
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' WRITE_ASPC, rate, sigrate, time, numpix, fname=fname, group=group'
  retall
endif
if (n_elements(group) eq 0) then group = !group        ;use !group as default
if (n_elements(fname) eq 0) then begin
  fname = ''
  read,' Enter root name for ASCII output file (root_asc.spc) >',fname
endif
filename = fname+'_asc.spc'
;
print,' Writing spectral data to ASCII file ',filename
s=size(group) & nchan=s(1)
get_lun,un
openw,un,filename
printf,un,nchan,time,numpix          ;read_ascspc will need to know nchan
for ii=0,nchan-1 do $
    printf,un,group(ii,0),group(ii,1),rate(ii),sigrate(ii)
close,un
free_lun,un
;
return
end             ;pro write_aspc
