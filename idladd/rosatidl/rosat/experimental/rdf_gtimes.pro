;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   rdf_gtimes
;*PURPOSE:
; A procedure to read the _bas.fits STDGTI extension (RDF format data) and 
; return the good time intervals
;
;*CALLING SEQUENCE:
;   rdf_gtimes,obseq,tbeg,tend,ntimes,dir=dir
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
;    written 12 Aug 1993 (GAR)
;-
;-------------------------------------------------------------------------------
pro rdf_gtimes,obseq,tbeg,tend,ntimes,dir=dir
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RDF_GTIMES, obseq, TBEG, TEND, NTIMES, dir=dir'
  retall
endif
;
filext='_bas.fits;*'
match_files,dir,obseq,filext,name,nlist        ;look for the right files
name = name(0)                          ;default = take latest version
tab = readfits(name,hdr,ext=1)
tbeg = tbget(hdr,tab,'START')
tend = tbget(hdr,tab,'STOP')
ntimes = n_elements(tbeg)
;
return
end        ;pro rdf_gtimes
