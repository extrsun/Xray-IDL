pro plot_obtime,obs,obe,list,tv=tv
if n_elements(tv) eq 0 then tt=list.time-obs(0) else tt=tv-obs(0)
plot,tt
ntt=n_elements(tt)
ob1=obs-obs(0)
ob2=obe-obs(0)
for k=0,n_elements(ob1)-1 do begin
	oplot,[0,ntt],[1,1]*ob1(k)
	oplot,[0,ntt],[1,1]*ob2(k),line=1
endfor
return
end
