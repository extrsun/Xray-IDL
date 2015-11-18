pro fz_roots_main,aa,bb,delta,rratio
;-
; get the roots of a polynormial for a model HII region (30 Dor).
; written by wqd (7/7/98)
;+
if n_params() eq 0 then begin
	print,'CALLING SEQ = '
	print,'fz_rratio_main,aa,bb,delta,rratio'
return
endif
ab=bb/aa
ab2=ab^2
coefs=[ab2*(aa+1.)^2,-ab2*(aa+1.)-1.,ab2-2.*ab*(aa+1.),2.*ab,1.]
rratio=fz_roots(coefs)
print,'R3/R2= ',rratio
delta=(rratio-1.)/aa
print,'delta= ',delta
print,'poly= ',poly(rratio,coefs)
return
end