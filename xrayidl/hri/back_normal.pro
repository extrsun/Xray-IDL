pro back_normal,f,fe,fb,bf,xf=xf


;+
; f, fe - vectors containing count fluxes and their errors (counts/s arcmin^2)
; fb - background flux vector 
; bf - output normalization of the non-cosmic X-ray background map
; xf - expected HRI x-ray background background	
; wqd, 9/5/1995
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  back_normal,f,fe,fb,bf,xf=xf'
return
endif
if n_elements(xf) eq 0 then begin
	xf=7.e-4 ; counts/s arcmin^2 from David et al., p24
			; with an uncertainty of a factor 2
	print,'xf = ', xf , ' is used'
endif
sr=1./fe^2
bf=total((f-xf)*fb*sr)/total(fb^2*sr)
chi=total((f-fb*bf-xf)^2*sr)
print,'chi^2, ndf = ',total((f-fb*bf-xf)^2*sr), n_elements(f)-1
print,'bf = ',bf
return
end