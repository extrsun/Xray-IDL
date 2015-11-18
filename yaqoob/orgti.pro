pro orgti,tint,tintout,nout
if n_params(0) eq 0 then begin
 print,'ORGTI, TINT, TINTOUT, NOUT'
 print,' Do an OR operation on GTI intervals to remove overlaps '
 print,' both input and output arrays have dimensions (*,2) '
 retall
end
pstart=tint(*,0) & pstop=tint(*,1)
if total(where(pstart ge pstop)) gt 0.0 then print,$
'ORGTI: found bad GTIs '
print,'before rejection ',(size(pstart))(1),(size(pstop))(1)
wtm=where((pstart lt pstop),ntm)
if ntm le 0 then return
start=pstart(wtm) & stop=pstop(wtm)
print,' after rejection ',(size(start))(1),(size(stop))(1)
;forprint,pstart,pstop
iter=0l & nout=0l 
bck2: ni=(size(start))(1) & tintout=dblarr(ni,2)
nold = nout
;sort the input times by start time
srtst=sort(start)
start=start(srtst) & stop=stop(srtst)
;forprint,start,stop
j=0l  & k=0l
;where does the end of this time interval lie?
bck1: for i=0l,ni-1l do begin
;print,' i = ',i,' j= ',j,' k= ',k,indx
 if (stop(k) ge start(i)) and (stop(k) le stop(i)) then indx=i
 if (i lt ni-1l) then if  (stop(k) gt stop(i)) and (stop(k) lt start(i+1)) then indx=-1l-i
;i.e. fisrt case lies in bin i and second case lies in gap before bin (i+1)
endfor
if stop(k) gt stop(ni-1) then indx=-1l*ni
;print,'indx= ',indx
;if gap then this j interval is iteslf and k points -indx
if indx lt 0l then begin
  tintout(j,0)=start(k) & tintout(j,1)=stop(k)
  k=-1l*indx & j=j+1l
endif
if indx ge 0l then begin
   tintout(j,0)=start(k) & tintout(j,1)=stop(indx)
   k=indx+1l & j=j+1l
endif
if k lt ni then goto, bck1
nout=j
print,'ORGTI: size of new GTI = ',nout
iter=iter+1
if (nold ne nout) then begin
	start=tintout(0:nout-1,0) & stop=tintout(0:nout-1,1)
	 goto, bck2
endif
print,n_elements(tintout)
diff=tintout-tintout(0,0)
return
end
