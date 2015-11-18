;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   rsgtimes
;*PURPOSE:
; A procedure to read either the MPE format selected times (which I believe
; must be the same as the good time intervals) or the US or RDF format good time
; intervals 
;
;*CALLING SEQUENCE:
;   rsgtimes,obseq,tbeg,tend,ntimes,dir=dir,proc=proc
;
;*PARAMETERS:
; INPUTS:
;    obseq  - String variable giving the observation sequence; the part of 
;             the file name before '_events.tfits', e.g. 'WP700049'
;
; OPTIONAL INPUTS:
;    dir    - String variable giving the directory of the files. If not
;             specified, the current directory is assumed
;    proc   - format of processed files. Choices are US, MPE.
;             if not specified, US is assumed.
;
; OUTPUTS:
;    tbeg   - Start times of the selected time intervals
;    tend   - Stop times of the selected time intervals
;    ntimes - Number of selected time intervals
;
;*RESTRICTIONS:
;    So far, mpe_gtimes has only been tested for German format PSPC data
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  MPE_GTIMES
;  US_GTIMES
;  RDF_GTIMES
;
;*MODIFICATION HISTORY:
;    written  12 Aug 1993 (GAR)
;    modified 16 Dec 1993 (GAR) to read RDF format files
;-
;-------------------------------------------------------------------------------
pro rsgtimes,obseq,tbeg,tend,ntimes,dir=dir,proc=proc
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSGTIMES, obseq, TBEG, TEND, NTIMES, dir=dir, proc=proc'
  retall
endif
if (n_elements(proc) eq 0) then proc = 'US'
;
procuc = strupcase(proc)
case procuc of
  'US': us_gtimes,obseq,tbeg,tend,ntimes,dir=dir
  'MPE': mpe_gtimes,obseq,tbeg,tend,ntimes,dir=dir 
  'RDF': rdf_gtimes,obseq,tbeg,tend,ntimes,dir=dir
  else : begin
    print,proc,' is not a valid choice for processing format.'
    print,' Valid choices are US, MPE, RDF. Returning.'
  end
endcase
;
return
end        ;pro rsgtimes
