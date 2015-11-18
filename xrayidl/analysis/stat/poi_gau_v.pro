pro lnls_rmf,expt,bc,smina,rmf,ecindlo,ecindn $
             ,slo=slo,shi=shi,sldvi=sldiv,esv=esv
if n_elements(esv) eq 0 then esv=1.e-4
nes=n_elements(esv)
ecindlo=indgen(nes) ;index of the starting bin
ecindn=indgen(nes)  ;number of bins

nbin=n_elements(expt)

;ouptut source flux vector
if n_elements(slo) eq 0 then slo=1.e-5
if n_elements(shi) eq 0 then shi=1.e-1
if n_elements(sldiv) eq 0 then sldiv=512
slv=[(alog10(shi)-alog10(slo))*findgen(sldiv+1)/sldiv+alog10(slo)]
sv=10^slv

rmf=fltarr(sldiv,nes) ;output metrices
rmft=fltarr(sldiv,nbin) ;temperory metrices for each expected source flux

ttd=expt*asfrac
for kk=0,nes-1 do begin ;loop over the expected source flux vector
    ec=es(kk)*ttd+bc ;expected counts for all bins
    eclo=ec-sigmalo*sqrt(ec) > smina*ttd+bc 
            ;lower output flux limit of the metrices 
    echi=(ec+sigmahi*sqrt(ec > 10.))

    for k=0L,nbin-1 do begin ;loop over the bins
        ;get the output source flux ranges within the lower and upper limits
        cv=sv*ttd(k)+bc(k)
        sel=where(cv gt eclo(k) and cv lt slvhi(k),nsel)
        if nsel ne 0 then begin 
            prob_poi_gau,ec(k),cv(sel),prob
            rmft(sel(0):sel(0)+nsel-1,k)=prob
        endif 
    endfor
    temp=total(rmft,2)/nbin
    rmf(*,kk)=temp
    sel=where(temp ne 0.,nsel)
    ecindlo(kk)=sel(0)
    ecindn(kk)=sel(nsel)-sel(0)+1
    rmft(*)=0.
endfor
stop
;write_rmf,slvlo,slvhi,esvlo,esvhi,ecinlo,ecindn
return
end 

