pro levgti,tbounds,cts,clo,chi,gti
if n_params(0) eq 0 then begin
 print,'levgti,tbounds,cts,clo,chi,gti'
 print,' Given tbounds(*,2) and corresponding counts in the bins '
 print,'(cts) compute the GTI arrai where clo < cts < chi '
 retall
end
nt=(size(tbounds))(1) & gti=fltarr(nt,2)
i=0l & clse=0
;where is the first bin
if cts(0) gt clo and cts(0) lt chi then begin
 gti(i,0)=tbounds(0,0) & clse=1
endif
for k=1l,nt-1 do begin
;if a gti has already started check if this bin closes it
 inside= cts(k) gt clo and cts(k) lt chi
 if not inside and clse eq 1 then begin
  gti(i,1)=tbounds(k-1,1) & clse=0 & i=i+1
 endif
 if inside and clse eq 0 then begin
  gti(i,0)=tbounds(k,0) & clse=1
 endif
endfor
if clse eq 1 then gti(i,1)=tbounds(nt-1,1)
gti=gti(0:i,0:1)
return
end
