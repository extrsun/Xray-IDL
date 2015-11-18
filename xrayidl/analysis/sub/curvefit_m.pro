FUNCTION CURVEFIT,X,Y,W,A,SIGMAA, Function_Name = Function_Name,chisqr=chisqr $
,yfit=yfit
;+
; NAME:
;	CURVEFIT
;
; PURPOSE:
;	Non-linear least squares fit to a function of an arbitrary 
;	number of parameters.  The function may be any non-linear 
;	function where the partial derivatives are known or can be 
;	approximated.
;
; CATEGORY:
;	E2 - Curve and Surface Fitting.
;
; CALLING SEQUENCE:
;	Result = CURVEFIT(X, Y, W, A, SIGMAA, FUNCTION_NAME = name)
;
; INPUTS:
;	X:  A row vector of independent variables.
;
;	Y:  A row vector of dependent variable, the same length as x.
;
;	W:  A row vector of weights, the same length as x and y.
;		For no weighting,
;		w(i) = 1.0.
;		For instrumental weighting,
;		w(i) = 1.0/y(i), etc.
;
;	A:  A vector, with as many elements as the number of terms, that 
;	    contains the initial estimate for each parameter.  If A is double-
;	    precision, calculations are performed in double precision, 
;	    otherwise they are performed in single precision.
;
; KEYWORDS:
;	FUNCTION_NAME:  The name of the function (actually, a procedure) to 
;	fit.  If omitted, "FUNCT" is used. The procedure must be written as
;	described under RESTRICTIONS, below.
;
; OUTPUTS:
;	Returns a vector of calculated values.
;	A:  A vector of parameters containing fit.
;
; OPTIONAL OUTPUT PARAMETERS:
;	Sigmaa:  A vector of standard deviations for the parameters in A.
;	
; COMMON BLOCKS:
;	NONE.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	The function to be fit must be defined and called FUNCT,
;	unless the FUNCTION_NAME keyword is supplied.  This function,
;	(actually written as a procedure) must accept values of
;	X (the independent variable), and A (the fitted function's
;	parameter values), and return F (the function's value at
;	X), and PDER (a 2D array of partial derivatives).
;	For an example, see FUNCT in the IDL User's Libaray.
;	A call to FUNCT is entered as:
;	FUNCT, X, A, F, PDER
; where:
;	X = Vector of NPOINT independent variables, input.
;	A = Vector of NTERMS function parameters, input.
;	F = Vector of NPOINT values of function, y(i) = funct(x(i)), output.
;	PDER = Array, (NPOINT, NTERMS), of partial derivatives of funct.
;		PDER(I,J) = DErivative of function at ith point with
;		respect to jth parameter.  Optional output parameter.
;		PDER should not be calculated if the parameter is not
;		supplied in call.
;
; PROCEDURE:
;	Copied from "CURFIT", least squares fit to a non-linear
;	function, pages 237-239, Bevington, Data Reduction and Error
;	Analysis for the Physical Sciences.
;
;	"This method is the Gradient-expansion algorithm which
;	combines the best features of the gradient search with
;	the method of linearizing the fitting function."
;
;	Iterations are performed until the chi square changes by
;	only 0.1% or until 20 iterations have been performed.
;
;	The initial guess of the parameter values should be
;	as close to the actual values as possible or the solution
;	may not converge.
;
; MODIFICATION HISTORY:
;	Written, DMS, RSI, September, 1982.
;	Does not iterate if the first guess is good.  DMS, Oct, 1990.
;	Added CALL_PROCEDURE to make the function's name a parameter.
;		(Nov 1990)
;	12/14/92 - modified to reflect the changes in the 1991
;		   edition of Bevington (eq. II-27) (jiy-suggested by CreaSo)
;-
;	ON_ERROR,2		;RETURN TO CALLER IF ERROR
				;Name of function to fit
	if n_elements(function_name) le 0 then function_name = "FUNCT"

	A = 1.*A		;MAKE PARAMS FLOATING
	NTERMS = N_ELEMENTS(A)	;# OF PARAMS.
	NFREE = (N_ELEMENTS(Y)<N_ELEMENTS(X))-NTERMS ;Degs of freedom
	IF NFREE LE 0 THEN STOP,'Curvefit - not enough data points.'
	FLAMBDA = 0.001		;Initial lambda
	DIAG = INDGEN(NTERMS)*(NTERMS+1) ;SUBSCRIPTS OF DIAGONAL ELEMENTS
;
	FOR ITER = 1,20 DO BEGIN	;Iteration loop
;
;		EVALUATE ALPHA AND BETA MATRICIES.
;
	CALL_PROCEDURE, Function_name,X,A,YFIT,PDER ;COMPUTE FUNCTION AT A.
	BETA = (Y-YFIT)*W # PDER
	ALPHA = TRANSPOSE(PDER) # (W # (FLTARR(NTERMS)+1)*PDER)
;
	CHISQ1 = TOTAL(W*(Y-YFIT)^2)/NFREE ;PRESENT CHI SQUARED.
				; If a good fit, no need to iterate
	if chisq1 lt total(abs(y))/1e7/NFREE then begin
		sigmaa = fltarr(nterms)	;Return all 0's
		return, yfit
		endif
;
;	INVERT MODIFIED CURVATURE MATRIX TO FIND NEW PARAMETERS.
;


	REPEAT BEGIN
		C = SQRT(ALPHA(DIAG) # ALPHA(DIAG))
		ARRAY = ALPHA/C
		ARRAY(DIAG) = ARRAY(DIAG)*(1.+FLAMBDA)		
		ARRAY = INVERT(ARRAY)
		B = A+ ARRAY/C # TRANSPOSE(BETA) ;NEW PARAMS
		b(2)=b(2) > 0.01
;		stop,'before call'
		CALL_PROCEDURE, Function_name,X,B,YFIT	;EVALUATE FUNCTION
		CHISQR = TOTAL(W*(Y-YFIT)^2)/NFREE ;NEW CHISQR
		FLAMBDA = FLAMBDA*10.	;ASSUME FIT GOT WORSE
		ENDREP UNTIL CHISQR LE CHISQ1
;
	FLAMBDA = FLAMBDA/100.	;DECREASE FLAMBDA BY FACTOR OF 10
	A=B			;SAVE NEW PARAMETER ESTIMATE.
	PRINT,'ITERATION =',ITER,' ,CHISQR =',CHISQR
	PRINT,A
	IF ((CHISQ1-CHISQR)/CHISQ1) LE .001 THEN GOTO,DONE ;Finished?
	ENDFOR			;ITERATION LOOP
;

	message, 'Failed to converge', /INFORMATIONAL
;
DONE:   SIGMAA = SQRT(ARRAY(DIAG)/ALPHA(DIAG)) ;RETURN SIGMA'S
stop

	RETURN,YFIT		;RETURN RESULT
END
