pro scoring,para,proname,fmin,ffdd=ffdd,itmax=itmax,double=double,ptol=ptol $
,stpmax=stpmax,status=status
;+
; Minimization with the scoring method and backtracking algorithm
; para - as input, initial parameter values; as output, parameter values
; 	which give the minimum function 
; proname - the procedure name for evaluating function value, derivatives,
;	and information matrix.
; fmin - output minimum function value
; double - if set, double precision for inverting the information matrix
; stpmax - the maximum step (def stpmax=0.2).
; ptol - parameter convergence requirement: the maximum fraction of change
;	of the parameter values in an iteration (def=10^-7).
; ffdd - the inverse of the information matrix.
;
;*CAUTION:
; para should be carefully normalized to have their values of order unit;
; otherwise, the stpmax needs to be selected carefully.
;
; implemented according to the methods described by C. Sarazin (1980, ApJ
; 236, 75) and by Press et al. (Numerical Recipes, section 9.7).
;
; written by wqd, 9/8/96
;-
if n_elements(stpmax) eq 0 then stpmax=0.2 
if n_elements(itmax) eq 0 then itmax=500
if n_elements(double) eq 0 then double=0
for k=1,itmax do begin
	parao=para
	call_procedure,proname,parao,ff,ffd,ffdd
	ffdd=nr_invert(ffdd,status,/double)
	if status eq 1 then begin
		print,'singular array: ffdd inverse'
		return
	endif
	if status eq 2 then begin
		print,'small pivot element: ffdd inverse'
		return
	endif		
	dpara=-ffd##ffdd
;	nr_ludcmp,ffdd,ind
;	dpara=-nr_lubksb(ffdd,ind,ffd)
	lnsrch,parao,ff,ffd,dpara,para,fmin,stpmax,check,proname,tolx=ptol 
		;backtracking
	if check eq 1 then return
;	print,'para,ff,ffd,ffdd,fmin ',para,ff,ffd,ffdd,fmin
	if !debug eq 1 then stop,'stop inside scoring loop'
endfor
print,'maximum itmax exceeded'
return
end
