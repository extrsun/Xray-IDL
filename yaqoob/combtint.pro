pro combtint,tint_1,tint_2,tint_out
if n_params(0) eq 0 then begin
 print,' COMBTINT, tint_1, tint_2,tint_out '
 print,' Combine (AND) 2 sets of good time intervals into 1 set '
 print,' tstart = tint(*,0) & tend = tint(*,1) '
 retall
endif
trial_out=dblarr(10000,2)
nbin_1=(size(tint_1))(1) & nbin_2=(size(tint_2))(1)
nout=0l
;go through each interval in tint_1
for k=0l,nbin_1-1 do begin
;find out where the bounds of this interval lie in the 2nd array
;	lobin = -1000000l & hibin = -1000000l
  if tint_1(k,0) lt tint_2(0,0) then lobin=-1l
  if tint_1(k,1) lt tint_2(0,0) then hibin=-1l
  if tint_1(k,0) gt tint_2(nbin_2-1,1) then lobin=-1l-nbin_2
  if tint_1(k,1) gt tint_2(nbin_2-1,1) then hibin=-1l-nbin_2
  for j=0l,nbin_2-1 do begin
 if tint_1(k,0) ge tint_2(j,0) and tint_1(k,0) le tint_2(j,1) then lobin=j+1
 if tint_1(k,1) ge tint_2(j,0) and tint_1(k,1) le tint_2(j,1) then hibin=j+1
   if j gt 0l then begin
 if tint_1(k,0) gt tint_2(j-1,1) and tint_1(k,0) lt tint_2(j,0) then lobin=-j-1
 if tint_1(k,1) gt tint_2(j-1,1) and tint_1(k,1) lt tint_2(j,0) then hibin=-j-1
   endif
  endfor
;  print,k,lobin,hibin
  if lobin ge 0l and hibin ge 0l and (lobin eq hibin) then begin
;this k bin lies entirely within one of the j bins
	trial_out(nout,0)=tint_1(k,0) & trial_out(nout,1)=tint_1(k,1)
		nout = nout +1
  endif
  if lobin lt 0l and hibin lt 0l and (lobin ne hibin) then begin
;in this case the k interval starts and ends outside the j intervals
	numinc = abs(hibin-lobin) 
	for i=0l,numinc-1l do begin
		inx = abs(lobin)+i-1
	trial_out(nout,0)=tint_2(inx,0) & trial_out(nout,1)=tint_2(inx,1)
		nout=nout+1
	endfor
  endif
;k interval starts in a jbin and ends in another j bin
 if lobin ge 0l and hibin ge 0l and lobin ne hibin then begin
	numinc=hibin-lobin-1
	trial_out(nout,0)=tint_1(k,0) 
	trial_out(nout,1)=tint_2(lobin-1,1) & nout=nout+1
	if numinc gt 0 then begin
	 for i=lobin,hibin-2 do begin
		trial_out(nout,0)=tint_2(i,0)
		trial_out(nout,1)=tint_2(i,1)
		nout=nout+1
	 endfor
	endif 
	trial_out(nout,0)=tint_2(hibin-1,0)
	trial_out(nout,1)=tint_1(k,1)
	nout=nout+1
endif
;k interval starts outside a j interval and ends in a j interval
if lobin lt 0l and hibin gt 0l then begin
	numinc=hibin-abs(lobin)
	if numinc gt 0 then begin
	 for i=abs(lobin)-1,hibin-2 do begin
	   trial_out(nout,0)=tint_2(i,0)&trial_out(nout,1)=tint_2(i,1)
		nout=nout+1
	 endfor
	endif
	trial_out(nout,0)=tint_2(hibin-1,0)
	trial_out(nout,1)=tint_1(k,1)
	nout=nout+1
endif
;k interval starts inside a j interval and ends outside one
if lobin gt 0l and hibin lt 0l then begin
	numinc=abs(hibin)-1-lobin
	trial_out(nout,0)=tint_1(k,0)
	trial_out(nout,1)=tint_2(lobin-1,1)
	nout=nout+1
	if numinc gt 0 then begin
	for i=lobin,abs(hibin)-2 do begin
         trial_out(nout,0)=tint_2(i,0)& trial_out(nout,1)=tint_2(i,1)
	 nout=nout+1
	endfor
	endif
endif
endfor
if nout gt 0 then begin
tint_out=dblarr(nout,2)
tint_out=trial_out(0:nout-1,0:1)
endif
return
end
