ns=9L
aprobth=-3.
lprobth=-7.
xs=(findgen(ns)-ns/2)*8./3600.
nss=intarr(ns,ns)
nsa=intarr(ns,ns)
cntra=fltarr(ns,ns)
for kk=0,ns-1 do begin 
    for k=0,ns-1 do begin 
        ocra=cra+xs(k) & ocdec=cdec+xs(kk) 
        sl=slo 
        map_ratio,sl,l,ocra,ocdec,bmin,bmax,ta,cbma,block,sfrac=asfrac $
          ,offaxis=offaxis*(!size_pixel/60.),rfac=1,count=count,back=back 
        poisig_v,total(back(*,blo:bhi),2),total(count(*,blo:bhi),2),lprob 
        nss(k,kk)=total(lprob lt lprobth) 
        ss=where(lprob gt aprobth,nsss)
        cntra(k,kk)=avg(imdiv(count(ss,*)-back(ss,*),expt(ss,*)))
        nsa(k,kk)=nsss
    endfor
endfor 
print,nss
print,cntra
print,nsa
end
