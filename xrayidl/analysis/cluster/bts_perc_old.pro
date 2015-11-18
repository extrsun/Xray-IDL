pro bts_perc,paraa,pam,siglevel=siglevel,pbest=pbest
;+
; calculate the percentile of the angular distribution
; pbest - the best-fit angle (def = median of the boots trapping sample data
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - bts_perc,paraa,pam,siglevel=siglevel,pbest=pbest'
return
endif
sz=size(paraa)
rn=sz(2)
pam=fltarr(sz(1),rn,3)
for i=0,sz(1)-1 do begin
	if i ne 3 then begin
	   for j=0,rn-1 do begin 
		avg_median,paraa(i,j,*),pm,plo,phi,siglevel=siglevel
		pam(i,j,*)=[pm,plo,phi]
	   endfor
	endif else begin
	   for j=0,rn-1 do begin 
		median_circle,paraa(i,j,*),pm,plo,phi,siglevel=siglevel $
			,pbest=pbest(j)
		pam(i,j,*)=[pm,plo,phi]
	   endfor
	endelse
endfor
print,pam
return
end