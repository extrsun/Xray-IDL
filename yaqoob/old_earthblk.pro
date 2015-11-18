pro EARTHBLK,dp14fl,sc,plist,EBBEG,EBEND,NEWLIST
; Use to read DP14 data and calculate earth angle for source  
; INPUTS 'dp14fl' => DP14 data file;  'sc' => source coordinates; 'plist'
; OUTPUTS =>  EBBEG, EBEND, (beginning and end times of earth occultations)
;             NEWLIST => Observing the earth
if n_params(0) eq 0 then begin
print,'EARTHBLK,''dp14fl'',sc,plist,EBBEG,EBEND,NEWLIST'
print,'Use to read DP14 data, calculate earth occultation times and adjust plist' 
print,'or create a new list of data when observing the earth'
retall
end
yr=[31,28,31,30,31,30,31,31,30,31,30,31]
on_ioerror,er10
close,1 & openr,1,dp14fl & skp=' ' & j=0 & ans=' '
a1=0 & a2=0 & a3=0 & a4=0 & a5=fltarr(2) & f=a5
readf,1,skp
er1:for i=0,9 do readf,1,skp
flag=0 & while flag eq 0 do begin
readf,1,a,b,c,d,f,format='(9x,i2,1x,i2,1x,i2,1x,i2,88x,2f7.2,:)'
a1=[a1,a] & a2=[a2,b] & a3=[a3,c] & a4=[a4,d] & a5=[[a5],[f]]
endwhile
er10:j=j+1 & print,j,' DP14 groups read'
if j gt 35 then goto, er11 & flag=1 & goto,er1
er11:n=n_elements(a1) & tm=dblarr(n)
for i=1,n-1 do begin
dy=long(total(yr(0:a1(i)-2)))+a2(i)-1
tm(i)=dy*86400+a3(i)*3600.+a4(i)*60.
endfor
ang=reform((angle(a5,sc))(0,*))
row={angl,tm:0.0D0,ang:0.0}
ebdat=replicate(row,n-1)
ebdat.tm=tm(1:*) & ebdat.ang=ang(1:*)
ebt=ebdat(where(ebdat.ang lt 80.)) & k=ebt.tm & m=([k,0]-[0,k])(1:*)
m=m(0:n_elements(m)-2) & ebbeg=[k(0),k(where(m gt 500.)+1)]
ebend=[k(where(m gt 500.)),k(n_elements(ebt)-1)]
time=plist.time & nn=n_elements(plist) & n=n_elements(ebbeg)
if time(nn-1) gt ebend(n-1) then print,'Need more DP14 input'
msk=intarr(nn)
for i=0l,n-1 do begin
msk=msk+(time gt ebbeg(i)) and (time le ebend(i))
endfor
read,'Ready for action: Type 0 (no action), 1 (Mod plist) 2 (New occ dat list)',g
if g eq 1 then plist=plist(where(msk eq 0))
if g eq 2 then newlist=plist(where(msk))
ea10:return
end
