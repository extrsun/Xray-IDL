pro timsim_theo,xmean,jmax
if n_params(0) eq 0 then begin
 print,'timsim_theo,xmean,jmax'
 print,'Compute theortical limit for TIMSIM'
 retall
endif
prob=0.0d0
print,'J 	KMIN     P0      P1	CUM PROB'
openw,1,'p1.dat'
q1=0.0
for j=1l,jmax do begin
	c0=float(j)  
	c1=c0+2.*sqrt(c0)+1.
	c2=c0-2.*sqrt(c0)+1.
	kmin=long(c1+0.9999)
	kmax=long(c2)
	if j gt 20 then big=1 else big=0
	poissprob,xmean,c0,pp
	p0=pp
	 p1=0.0d0
	printf,1,'!lower'
;	if j gt 4   then  begin
;	if float(kmax) lt c2 then  begin
	sep=abs(c2-c0)
	err=sqrt(float(kmax))+sqrt(c0)
;	if err gt sep then begin 
	 for k=1,kmax do begin
	  k1=float(k)
	  poissprob,xmean,k1,pp
	  p1=p1+pp
	  printf,1,k,p1,pp
	 endfor
;	endif
	printf,1,'!upper'
;	if float(kmin) gt c1 then begin
	err=sqrt(float(kmin))+sqrt(c0)
	sep=abs(c1-c0)
; 	if err gt sep then begin
	 for k=kmin,jmax do begin
		k1=float(k)
		poissprob,xmean,k1,pp
		p1=p1+pp
		printf,1,k,p1,pp
	 endfor	
	printf,1,'NO NO'
;	endif
	q1=max([0.,min([p1,1.d0])])
	prob=prob+p0*q1
	print,j,' ',kmin,p0,q1,prob
endfor
close,1
return
end
