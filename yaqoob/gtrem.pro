pro gtrem,plist,int
; Remove anomalous GIS data 
; INPUT; plist
; OUTPUT; modified plist;  int (optional)
if n_params(0) eq 0 then begin
print,'GTREM,plist,int'
print,'Remove anomalous GIS data'
print,'Time shown is in units of 10s'
retall
end
tm=plist.time & tm=tm-tm(0) & int=0l & i=0
plot,histogram(tm,bin=10),/xst
print,'Use cursor to indicate max data height' & cursor,xc,yc,1 & mxd=yc
ts1:plot,histogram(tm,bin=10),yr=[0,mxd],/xst
a=intarr(2) & read,'Type next intervl in units of 10 s; 0,0 to exit: ',a
print,' ' & if total(a) eq 0 then goto,ts2
plot,histogram(tm,bin=10),xr=a,yr=[0,mxd],/xst
print,'Use cursor to mark time; hit below x-axis to skip this plot' 
print,'Press ''n'' for next hit, ''d'' to skip last hit, ''q'' to continue'
print,' '
repeat begin
ts11:cursor,xc,yc,1 & if xc lt 0 then xc=0 & if yc lt 0 then goto,ts1
i=i+1 & int=[int,long(xc*10)] & print,i,int(i)/10.
ts12:aa=get_kbrd(1)
if aa eq 'd' then begin
i=i-1 & int=int(0:i) & goto,ts11
endif
if ((aa ne 'n')and(aa ne 'q')) then goto,ts12
endrep until aa eq 'q'
goto,ts1
ts2:if (size(int))(0) eq 0 then goto,ts4
int=int(1:*) & n=n_elements(int)/2 & int=int(0:2*n-1)
int=reform(int,2,n) & print,int & print,total(int(1,*)-int(0,*))
ts10:read,'type ''0''(no action), ''1'' (mod plist): ',z
if z eq 0 then goto,ts4 & if z eq 1 then goto,ts3 
read,'wrong input; try again: ',z & goto,ts10
ts3:tmask,tm,int,msk & plist=plist(where(msk eq 0))
ts4:return
end


