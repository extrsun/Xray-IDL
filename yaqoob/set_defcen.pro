pro set_defcen,instr,ctyp,defxcen,defycen,pixsiz
;
; Set default X & Y centers and pixel size for a given instrument (INSTR)
;   and coordinate type (CTYP)
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' SET_DEFCEN, instr, ctyp, DEFXCEN, DEFYCEN, PIXSIZ'
  retall & endif
;
if (strupcase(ctyp) eq 'SKY') then begin            ;sky coordinates
  pixsiz = 0.5                            ;original pixel size in arcsec
  defxcen = 7680.
  if (strupcase(instr) eq 'H') then defxcen = 4096.
  defycen = defxcen
endif else begin
  pixsiz = 1.0                            
  if (strupcase(instr) ne 'H') then pixsiz = 0.5*7680./4096.
  defxcen = 4096.
  if (strupcase(instr) eq 'H') then defxcen = 2048.
  defycen = defxcen
endelse
;
return
end          ;pro set_defcen
