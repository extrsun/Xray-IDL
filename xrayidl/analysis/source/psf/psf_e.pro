pro psf_e,offcenv,energy,radius,percr=percr
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - psf_e,offcenv,energy,radius,percr=percr'
return
endif
;
common share,offang,prof
if n_elements(perc) eq 0 then perc=[0.5,0.7,0.8,0.85,0.9,0.95]
na=n_elements(energy)
nl=n_elements(offcenv)
radius=fltarr(na,nl)

noff=4800.
offang=findgen(noff)
for k=0, na-1 do begin
 for kk=0,nl-1 do begin
	rspsfp,offcenv(kk),energy(k),offang,prof

	; total rate contained in the prof
	trate=prof(0)*!pi*offang(1)^2+simpson('get_intpsf' $
		,offang(1),offang(noff-1))	       
	 print,'trate =',trate
	prof=prof/trate ;renormalize the profiles
	
	rate=prof(0)*!pi*offang(1)^2+simpson('get_intpsf',offang(1),percr)
	radius(k,kk)=rate
 endfor
	print,'radius = ',radius(k,*)
endfor
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