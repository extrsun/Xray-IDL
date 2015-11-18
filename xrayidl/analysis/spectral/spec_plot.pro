pro spec_plot,rate_b,sigrate_b,rate_s,sigrate_s
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - spec_plot,rate_b,sigrate_b,rate_s,sigrate_s'
return
endif
;
e=(!ebnds(*,0)+!ebnds(*,1))/2.
eint=!ebnds(*,1)-!ebnds(*,0)
;
;ymin=min((rate_s-sigrate_s)/eint*0.9 > 0.)
ymin=0
ymax=max((rate_b+sigrate_b)/eint*1.1)
plot,e,rate_b/eint,psym=4,Xtitle='Energy (keV)',Ytitle='cts/s keV deg^-2' $
,yrange=[ymin,ymax]
errplot,e,(rate_b-sigrate_b)/eint,(rate_b+sigrate_b)/eint
;
if n_params() gt 2 then begin
	oplot,e,rate_s/eint,psym=5
	errplot,e,(rate_s-sigrate_s)/eint,(rate_s+sigrate_s)/eint
endif
;
end