pro tmflux,slist,tbeg,tend,flist
; Compute source flux within major data intervals
; INPUT => slist,tbeg,tend (use TMBREAKS to compute last two)
; OUTPUT => flist (structure)
if n_params(0) eq 0 then begin
print,'TMFLUX,slist,tbeg,tend,flist'
print,'Compute source flux within major data intervals'
print,'Use TMBREAKS to compute ''tbeg'' and ''tend''
retall
end
time=slist.time & n1=n_elements(tbeg) & n2=n_elements(tend)
if (n1 ne n2) then goto,tm1 & n=fltarr(n1)
for i=0,(n1-1) do n(i)=total((time ge tbeg(i))and(time le tend(i)))
tm=tend-tbeg & flux=n/tm & err=sqrt(n)/tm & Dt=tm/2. & tc=tbeg+Dt
row={flxpt,flx:0.0,err:0.0,tbin:0.0D0,Dt:0.0}
flist=replicate(row,n1)
flist.flx=flux&flist.err=err&flist.tbin=tc&flist.Dt=Dt
flist=flist(where((err/flux) lt .5))
tm1:return
end


