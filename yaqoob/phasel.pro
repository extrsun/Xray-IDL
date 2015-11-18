pro phasel,pha,phaout,pmin=pmin,pmax=pmax,thrsh=thrsh,npix=npix
; Select SIS events from event file which satisfy pha criteria
; INPUT
; pha => 9xn matrix with central pulse height plus that of
; surrounding 8 pixels
; OUTPUT
; phaout => a vector to be used to select coordinates and pulse
; heights of events satisfying criteria.
if n_params(0) eq 0 then begin
print,'PHASEL,pha,phaout,pmin=pmin,pmax=pmax,thrsh=thrsh,npix=npix'
retall
end
if n_elements(pmin) eq 0 then pmin=100
if n_elements(pmax) eq 0 then pmax=3000
if n_elements(thrsh) eq 0 then thrsh=30
if n_elements(npix) eq 0 then npix=8
a=(pha(0,*) gt pmin) and (pha(0,*) lt pmax) & b=(pha(1:*,*) lt thrsh)
phaout=reform(a and (fix(total(b,1)) eq npix))
return
end




