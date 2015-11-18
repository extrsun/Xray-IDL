pro exp_47,cb,tb,tbs,dim=dim
if n_elements(dim) eq 0 then dim=512
get_image,t1,c1,dim=dim,blow=4,bhigh=5,tfile=!seq_no+'_gtiall.dat',ts,slow=3.,factor=1.5
get_image,t2,c2,dim=dim,blow=6,bhigh=7,tfile=!seq_no+'_gtiall.dat',ts,slow=3.,factor=1.5
t=fltarr(dim,dim,2)
c=intarr(dim,dim,2)
t(*,*,0)=t1
t(*,*,1)=t2
c(*,*,0)=c1
c(*,*,1)=c2
exp_broad,40.,c,t,ts,tb
sel=where(ts le 0.)
tbs=tb
tbs(sel)=ts(sel)
cb=c1+c2
end

