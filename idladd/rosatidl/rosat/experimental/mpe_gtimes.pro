;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   mpe_gtimes
;*PURPOSE:
; A procedure to read the _events.tfits header (MPE format data) and return
; the selected times (which I believe must be the same as the good time
; intervals)
;
;*CALLING SEQUENCE:
;   mpe_gtimes,obseq,tbeg,tend,ntimes,dir=dir
;
;*PARAMETERS:
; INPUTS:
;    obseq  - String variable giving the observation sequence; the part of 
;             the file name before '_events.tfits', e.g. 'WP700049'
;
; OPTIONAL INPUTS:
;    dir    - String variable giving the directory of the files. If not
;             specified, the current directory is assumed
;
; OUTPUTS:
;    tbeg   - Start times of the selected time intervals
;    tend   - Stop times of the selected time intervals
;    ntimes - Number of selected time intervals
;
;*RESTRICTIONS:
;    So far, has only been tested for German format PSPC data
;
;*NOTES:
;    Assumes that the good times are in the history records, and that the
;    record immediately preceding contains the string 'TIME_SEL'
;
;*SUBROUTINES CALLED:
;  MATCHFILES
;  GETHISTVAL
;
;*MODIFICATION HISTORY:
;    written 10 Aug 1993 (GAR)
;-
;-------------------------------------------------------------------------------
pro mpe_gtimes,obseq,tbeg,tend,ntimes,dir=dir
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' MPE_GTIMES, obseq, TBEG, TEND, NTIMES, dir=dir'
  retall
endif
;
filext='_events.tfits;*'
match_files,dir,obseq,filext,name,nlist        ;look for the right files
name = name(0)
hdr=headfits(name,ext=1)
gethistval,hdr,'TIM_SEL',value,lnum
lnum = lnum(0)
tsel = [-999999.D0]
;
i = 1
hdrrec=hdr(lnum+i)
try=gettok(hdrrec,'  ')                        ;strip off initial HISTORY
hdrrec = strtrim(hdrrec,2)
;
while (hdrrec ne '') do begin
  while (hdrrec ne '') do begin                ;strip off times, one by one
    try=gettok(hdrrec,'  ')
    tsel = [tsel,double(try)]
    hdrrec = strtrim(hdrrec,2)
  endwhile      
  i = i + 1
  hdrrec=hdr(lnum+i)
  try=gettok(hdrrec,'  ')                      ;strip off initial HISTORY
  hdrrec = strtrim(hdrrec,2)
endwhile
tsel = tsel(1:*)
;
ntimes = n_elements(tsel)/2
if (ntimes ne n_elements(tsel)/2.) then begin
  print,' Error: Number of selected times is not even. Returning.'
  retall
endif
;
itim = indgen(ntimes)*2
tbeg = tsel(itim)
tend = tsel(itim+1)
;
return
end        ;pro mpe_gtimes
