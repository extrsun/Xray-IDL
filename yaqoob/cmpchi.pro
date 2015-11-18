pro cmpchi,stat,dof,np
if n_params(0) eq 0 then begin
 print,'cmpchi,stat,dof'
 retall
end
nval=float((size(stat))(1))
statmin=min(stat) & statmax=max(stat)
dstat=(statmax-statmin)/float(np)
statval=statmin+findgen(np)*dstat
cumstat=fltarr(np)
for k=0l,np-1 do begin
 wc=where((stat le statval(k)),p)
 cumstat(k)=p/nval
endfor
plot,statval,cumstat,psym=1
np2=100
dstat2=(statmax-statmin)/float(np2)
x=statmin+findgen(np2)*dstat2
chis=fltarr(np2)
for j=0l,np2-1 do begin
 chis(j)=chi_sqr1(x(j),dof)
endfor
oplot,x,chis
return
end
