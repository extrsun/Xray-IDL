pro stsel,plist,sblist,int
; Select data segments from light curve
; To 1) Remove from ''plist'' 2) Put in a new file ''sblist'' 
; INPUT; plist
; OUTPUT; modified plist; sblist (optional)i; int (optional)
if n_params(0) eq 0 then begin
print,'STSEL,plist,sblist,int
print,'Select data segments from light curve'
print,'To 1) Remove from ''plist'' 2) Put in a new file ''sblist'''
print,'Time shown is in units of 16 s'
retall
end
tm=plist.time & tm=tm-tm(0) & int=0l & i=0
plot,histogram(tm,bin=16),/xst
print,'Use cursor to indicate max data height' & cursor,xc,yc,1 & mxd=yc
ts1:plot,histogram(tm,bin=16),yr=[0,mxd],/xst
a=intarr(2) & read,'Type next intervl in units of 16 s; 0,0 to exit: ',a
print,' ' & if total(a) eq 0 then goto,ts2
plot,histogram(tm,bin=16),xr=a,yr=[0,mxd],/xst
print,'Use cursor to mark time; hit below x-axis to skip this plot'
print,'Press ''n'' for next hit, ''d''to skip last hit,''q'' to continue'
print,' '
repeat begin
ts11:cursor,xc,yc,1 & if xc lt 0 then xc=0 & if yc lt 0 then goto,ts1
i=i+1 & int=[int,long(xc*16)] & print,i,int(i)/16.
ts12:aa=get_kbrd(1)
if aa eq 'd' then begin
i=i-1 & int=int(0:i) & print,i,int(i)/16. & goto,ts12
endif
if ((aa ne 'n')and(aa ne 'q')) then goto,ts12
endrep until aa eq 'q'
goto,ts1
ts2:if (size(int))(0) eq 0 then goto,ts4
int=int(1:*) & n=n_elements(int)/2 & int=int(0:2*n-1)
int=reform(int,2,n) & print,int & print,total(int(1,*)-int(0,*))
tmask,tm,int,msk & print,'ready to act'
read,'type ''0''(no action), ''1'' (mod plist), ''2'' (sblist): ',z
if z eq 0 then goto,ts4 & if z eq 1 then goto,ts3 
sblist=plist(where(msk eq 1)) & goto,ts4
ts3:plist=plist(where(msk eq 0))
ts4:return
end


