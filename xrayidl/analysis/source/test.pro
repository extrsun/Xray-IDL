pro psf_frac_e,offcenv,energy,radius,perclimit=perclimit
;
common share,offang,prof
if n_elements(perclimit) eq 0 then perclimit=[0.5,0.7,0.8,0.85,0.9,0.95]
na=n_elements(offcenv)
nl=n_elements(perclimit)
radius=fltarr(na,nl)

fout=1.-perclimit
maxfout=max(fout)
noff=6000
offang=findgen(noff)
frac=fltarr(noff)
for k=0, na-1 do begin
	rspsfp,offcenv(k),energy,offang,prof

	; total rate contained in the prof
	trate=prof(0)*!pi*offang(1)^2+simpson('get_intpsf' $
		,offang(0),offang(noff-1))	       
	print,'trate =',trate
	prof=prof/trate ;renormalize the profiles
	
	for kl=0,nl-1 do begin
	rate=prof(0)*!pi*offang(1)^2+simpson('get_intpsf',offang(0),perc(kk))
	radius(k,kk)=rate
	endfor
endfor
radius=radius/60.
return
end
;=================================================
function get_intpsf,angle
common share,offang,prof
linterp,offang,prof,angle,intpsf
;quadterp,offang,prof,angle,intpsf ;makes no significant difference
intpsf=intpsf*!pi*2.*angle
return,intpsf
end