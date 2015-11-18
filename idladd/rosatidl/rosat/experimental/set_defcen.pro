pro set_defcen,instr,ctyp,defxcen,defycen,pixsiz,defxlim,defylim
;
; Set default X & Y centers and pixel size for a given instrument (INSTR)
;   and coordinate type (CTYP)
; Also set default min & max values for X & Y ranges
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' SET_DEFCEN, instr, ctyp, DEFXCEN, DEFYCEN, PIXSIZ, DEFXLIM, DEFYLIM'
  retall & endif
;
if (strupcase(ctyp) eq 'SKY') then begin            ;sky coordinates
  pixsiz = 0.5                            ;original pixel size in arcsec
  defxcen = 7680.
  if (strupcase(instr) eq 'H') then defxcen = 4096.
  defycen = defxcen
  defxlim = [0,2.*defxcen]
  defylim = defxlim
endif else begin
  pixsiz = 1.0                            
  if (strupcase(instr) ne 'H') then pixsiz = 0.5*7680./4096.
  defxcen = 4096.
  if (strupcase(instr) eq 'H') then defxcen = 2048.
  defycen = defxcen
  defxlim = [0,2.*defxcen]
  defylim = defxlim
endelse
;
return
end          ;pro set_defcen
