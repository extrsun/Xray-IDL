pro tsel,plist,int
; Select intervals of good data in plist
; INPUT; plist
; OUTPUT; modified plist, int(optional)
if n_params(0) eq 0 then begin
print,'TSEL,plist,int'
print,'Select intervals of good data in plist'
retall
end
tm=plist.time & tm=tm-tm(0) & int=0l & i=0 & ans=' '
plot,histogram(tm,bin=16),/xst
print,'Use cursor to indicate max data height' & cursor,xc,yc,1 & mxd=yc
ts1:plot,histogram(tm,bin=16),yr=[0,mxd],/xst
a=intarr(2) & read,'give intervl in units of 16; 0,0 to exit: ',a
if total(a) eq 0 then goto,ts2
plot,histogram(tm,bin=16),xr=a,yr=[0,mxd],/xst
print,'use cursor to mark time; hit neg y to skip this plot' 
print,'Press any key for next time, ''n'' for next plot'
repeat begin
cursor,xc,yc,1 & if xc lt 0 then xc=0 & if yc lt 0 then goto,ts1
i=i+1 & print,i & int=[int,long(xc*16)]
aa=get_kbrd(1) & endrep until aa eq 'n'
goto,ts1
ts2:if (size(int))(0) eq 0 then goto,ts3
int=int(1:*) & n=n_elements(int)/2 & int=int(0:2*n-1)
int=reform(int,2,n) & print,int & print,total(int(1,*)-int(0,*))
read,'Ready to modify plist? y/(n): ',ans 
if ans ne 'y' then goto,ts3
tmask,tm,int,msk & plist=plist(where(msk))
ts3:return
end


