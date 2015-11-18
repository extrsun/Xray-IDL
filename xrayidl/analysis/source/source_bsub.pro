pro source_bsub,ls,lb,tminv,tmaxv,ns,nb,scr,scre,t,te,step=step
print,'calculating source CNTR'
source_thist,ls,tminv,tmaxv,ns,crs,crse,t1,t2,nps,step=step
print,'calculating background CNTR'
source_thist,lb,tminv,tmaxv,nb,crb,crbe,t1,t2,npb,step=step
scr=crs-crb*nps/npb
scre=sqrt(crse^2+(crbe*nps/npb)^2)
t=(t1+t2)*0.5
te=(t2-t1)*0.5
sel=where(scre gt 0.,nsel)
avg_least,scr(sel),scre(sel),mscr,mscre
ploterr,scr,scre,psym=7
oplot,[0,n_elements(scr)],[mscr,mscr]
end