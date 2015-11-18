pro bts_perc,paraa,pam,siglevel=siglevel,pbest=pbest,xp=xp,yp=yp
;+
; calculate the percentile of the angular distribution
; pbest - the best-fit angle (def = median of the boots trapping sample data
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - bts_perc,paraa,pam,siglevel=siglevel,pbest=pbest,xp=xp,yp=yp'
return
endif
if n_elements(xp) eq 0 then xp=0
if n_elements(yp) eq 0 then yp=0
sz=size(paraa)
rn=sz(2)
pam=fltarr(sz(1),rn,3)
for i=0,sz(1)-1 do begin
 	case i of
         0: for j=0,rn-1 do begin 
		avg_median,paraa(i,j,*)-xp,pm,plo,phi,siglevel=siglevel
		pam(i,j,*)=[pm,plo,phi]
	    endfor
         1: for j=0,rn-1 do begin 
		avg_median,paraa(i,j,*)-yp,pm,plo,phi,siglevel=siglevel
		pam(i,j,*)=[pm,plo,phi]
	    endfor
	 2: for j=0,rn-1 do begin 
		avg_median,paraa(i,j,*),pm,plo,phi,siglevel=siglevel
		pam(i,j,*)=[pm,plo,phi]
	    endfor
         3: for j=0,rn-1 do begin 
		if n_elements(pbest) ne 0 then $
                  median_circle,paraa(i,j,*),pm,plo,phi,siglevel=siglevel $
			,pbest=pbest(j) $
                  else median_circle,paraa(i,j,*),pm,plo,phi,siglevel=siglevel
		pam(i,j,*)=[pm,plo,phi]
	    endfor
  	 4: for j=0,rn-1 do begin ;to place the absolute shift
		dis=sqrt((paraa(1,j,*)-yp)^2+(paraa(0,j,*)-xp)^2)
		avg_median,dis,pm,plo,phi,siglevel=siglevel
		pam(i,j,*)=[pm,plo,phi]
	    endfor
	else: for j=0,rn-1 do begin 
		avg_median,paraa(i-1,j,*),pm,plo,phi,siglevel=siglevel
		pam(i,j,*)=[pm,plo,phi]
	      endfor
	endcase
endfor
print,pam
return
end
