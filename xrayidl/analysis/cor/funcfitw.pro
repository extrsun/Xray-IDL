FUNCTION FUNCFITW,X,Y,W,func,nfunc,YFIT,YBAND,SIGMA,A,comp_sub=comp_sub
;+
; NAME:
;	FUNCFITW
;
; PURPOSE:
;	Perform a least-square discrete function fit with optional
;        error estimates.
;
; CATEGORY:
;	Curve fitting.
;
; CALLING SEQUENCE:
;	Result = POLYFITW(X, Y, W, func, nfunc [, Yfit, Yband, Sigma, A])
;
; INPUTS:
;	    X:	The independent variable vector.
;
;	    Y:	The dependent variable vector.  This vector should be the same 
;		length as X.
;
;	    W:	The vector of weights.  This vector should be same length as 
;		X and Y.
;
;	 Func:  dimsision = n_elements(x)*nfunc function values at X.
;
;       Nfunc:	The number of functions.
;
; OUTPUTS:
;	FUNCFITW returns a vector of coefficients of length Nfunc.
;
; OPTIONAL OUTPUT PARAMETERS:
;	 Yfit:	The vector of calculated Y's.  Has an error of + or - Yband.
;
;	Yband:	Error estimate for each point = 1 sigma.
;
;	Sigma:	Reduced Chi square value.
;
;	    A:	Correlation matrix of the coefficients.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; MODIFICATION HISTORY:
;	Written by: 	WQD Aug 30 1992
; 	remove sqrt from sigma calculation (wqd 12/3/98)
;
;-
;ON_ERROR,2                      ;RETURN TO CALLER IF AN ERROR OCCURS
N = N_ELEMENTS(X) < N_ELEMENTS(Y) ; SIZE = SMALLER OF X,Y
A = DBLARR(nfunc,nfunc) ; LEAST SQUARE MATRIX, WEIGHTED MATRIX
B = DBLARR(nfunc)	; WILL CONTAIN SUM W*Y*func
if n_elements(comp_sub) eq 0 then Y_new=Y else Y_new=Y-comp_sub
;
for k=0,(nfunc-1) do B(k) = TOTAL(W*Y_New*func(*,k)) 
;
for k=0,(nfunc-1) do begin
	frac=w*func(*,k)
	for l=0,(nfunc-1) do begin
		a(l,k)=total(frac*func(*,l))
	endfor
endfor
;
if !debug eq 1 then stop
;	END ; END OF P LOOP, CONSTRUCTION OF A AND B
;
if n_elements(a) eq 1 then a=1./a else $
A = INVERT(A)
;
C = FLOAT(B # A)
;
if !debug eq 1 then stop
IF ( N_PARAMS(0) LE 5) THEN RETURN,C	; EXIT IF NO ERROR ESTIMATES
;
; COMPUTE OPTIONAL OUTPUT PARAMETERS.
;
yfit = fltarr(n) ; ONE-SIGMA ERROR ESTIMATES, INIT
for k=0,(nfunc-1) do yfit=yfit +c(k)*func(*,k) ; SUM BASIS VECTORS
;
VAR = TOTAL((YFIT-Y_New)^2 )/(N-nfunc)	; VARIANCE ESTIMATE, UNBIASED
;
;
SIGMA=SQRT(VAR)
YBAND = FLTARR(N)
for k=0,(nfunc-1) do begin ; COMPUTE CORRELATED ERROR ESTIMATES ON Y_New
	for l=0,(nfunc-1) do begin
		yband=func(*,l)*func(*,k)*a(l,k)
	endfor
endfor
	YBAND = YBAND*VAR
	YBAND = SQRT( YBAND )
sigma=total((yfit-y_new)^2*w)/(n-nfunc)
if n_elements(comp_sub) ne 0 then yfit=yfit+comp_sub ;add back this component
;stop
RETURN,C
END



