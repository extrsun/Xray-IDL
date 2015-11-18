pro lnsrch,xold,fold,g,p,x,ff,stpmax,check,proname,tolx=tolx
;+
; Backtracking for the best line search parameter.
; proname - the procedure name for evaluating function value, replacing
;	the original function name.
; A simple translation of the numerical recipe routine of the same name
; by wqd, 9/8/96
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE ---'
print,'lnsrch,xold,fold,g,p,x,ff,stpmax,check,proname,tolx=tolx'
return
endif
ALF=1.0e-4
if n_elements(tolx) eq 0 then TOLX=1.0e-7

	check=0
	sum=sqrt(total(p^2))
	if (sum gt stpmax) then p=p*stpmax/sum
	slope = total(g*p)
	test=max(abs(p)/(abs(xold) > 1.0))
	alamin=TOLX/test
	alam=1.0
	maxint=1000
	for kk=1,maxint do begin
		x=xold+alam*p
		call_procedure,proname,x,ff
		if (alam lt alamin) then begin
			x=xold
			check=1
			return ;converge on delta x
		endif else if (ff le fold+ALF*alam*slope) then return $
		else begin
			if (alam eq 1.0) then $
				tmplam = -slope/(2.0*(ff-fold-slope)) $
			else begin
			 rhs1 = ff-fold-alam*slope
			 rhs2=f2-fold2-alam2*slope
			 a=(rhs1/(alam*alam)-rhs2/(alam2*alam2))/(alam-alam2)
			 b=(-alam2*rhs1/(alam*alam)+alam*rhs2/(alam2*alam2)) $
				/(alam-alam2)
			 if (a eq 0.0) then tmplam = -slope/(2.0*b) $
				else begin
				 disc=b*b-3.0*a*slope
				 if (disc lt 0.0) then $
				 	print,"Roundoff problem in lnsrch." $
				 	else tmplam=(-b+sqrt(disc))/(3.0*a)
				endelse
				if (tmplam gt 0.5*alam) then $
					tmplam=0.5*alam
			 endelse
		endelse
		alam2=alam
		f2 = ff
		fold2=fold
		alam=tmplam > 0.1*alam
	endfor
stop,'interation exceeds ', maxint
return
end
