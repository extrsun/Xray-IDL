;+
;
; routine to match 2 vectors xx & yy, & identify the matches
; returns sorted vectors xx(sort(xx)) and yy(sort(yy)), with
; index vectors xind and yind such that
; xx(xind) contains all values which are matched in yy and 
; yy(yind) contains all values which are matched in xx
; xx & yy may contain duplicate values
;
;-
pro match_w_dup,xx,yy,xind,yind,sorted=sorted
;
if (n_elements(sorted) eq 0) then sorted = 0      ;default is to sort
if (sorted eq 0) then begin
  xx = xx(sort(xx))
  yy = yy(sort(yy))
endif
loc=[xx,-1]
ldupx=[where(loc(1:*)-loc)]
;
loc=[yy,-1] 
ldupy=[where(loc(1:*)-loc)]
;
match,xx(ldupx),yy(ldupy),ix,iy
;
if N_elements( Ldupx ) EQ 1 then $
  kdup = Ldupx(0)+1         $
else kdup = [ Ldupx(0)+1 , Ldupx(1:*)-Ldupx ]
ntot = total(kdup(ix)) 
nel = n_elements(ix)
xind = intarr(ntot)
ict=0
for ii=0,nel-1 do begin & kb=ldupx(ix(ii))-kdup(ix(ii))+1 & $
  ni = kdup(ix(ii)) & xind(ict) = kb + indgen(ni) & ict=ict+ni & endfor
;
if N_elements( Ldupy ) EQ 1 then $
  kdup = Ldupy(0)+1         $
else kdup = [ Ldupy(0)+1 , Ldupy(1:*)-Ldupy ]
ntot = total(kdup(iy))
nel = n_elements(iy)
yind=intarr(ntot)
ict=0
for ii=0,nel-1 do begin & kb=ldupy(iy(ii))-kdup(iy(ii))+1 & $
  ni = kdup(iy(ii)) & yind(ict) = kb + indgen(ni) & ict=ict+ni & endfor
;
return
end           ;pro match_w_dup
