pro avg_least,xx,xxe,xxm,xxme,chi2=chi2,ndf=ndf,sigma=sigma,prob=prob $
,plot=plot,print=print,logprob=logprob
;+
; get an average value of a vector with error bars, using a least square fit
; xx, xxe - vectors of values and errors
; xxm, xxme - the average value and its error
; chi2 - chi^2 value
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - avg_least,xx,xxe,xxm,xxme,chi2=chi2,ndf=ndf,sigma=sigma,plot=plot,prob=prob,print=print,logprob=logprob'
return
endif
ww=1./xxe^2
xxme=sqrt(1./total(ww))
xxm=total(xx*ww)*xxme^2
chi2=total((xx-xxm)^2/xxe^2)
ndf=n_elements(xx)-1
sigma=(chi2-ndf)/sqrt(2.*ndf)
prob=1-chisqr_pdf(chi2,ndf)
if keyword_set(print) then begin
	if keyword_set(logprob) then $
		print,'chi2, ndf, prob, sigma = ',chi2,ndf,alog10(prob),sigma else $
		print,'chi2, ndf, prob, sigma = ',chi2,ndf,prob,sigma
endif
if keyword_set(plot) then begin
	ploterr,xx,xxe,psym=7
	oplot,[0,ndf],[1,1]*xxm
endif
return
end