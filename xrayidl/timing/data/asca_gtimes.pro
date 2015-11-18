;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   asca_gtimes
;*PURPOSE:
; A procedure to read theSTDGTI extension (RDF format data) and
; return the good time intervals
;
;*PARAMETERS:
; INPUTS:
;    name  - file name including directory
;
; OUTPUTS:
;    tbeg   - Start times of the selected time intervals
;    tend   - Stop times of the selected time intervals
;
;*MODIFICATION HISTORY:
;    written 20 Feb 1998 (wqd)
;-
;-------------------------------------------------------------------------------
pro asca_gtimes,name,tbeg,tend
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' asca_gtimes,name,tbeg,tend'
  retall
endif
;
if !proc eq 'RDF' then ext=1 else ext=2 ;asca data
tab = readfits(name,hdr,ext=ext)
tbeg = tbget(hdr,tab,'START')
tend = tbget(hdr,tab,'STOP')
;
return
end
