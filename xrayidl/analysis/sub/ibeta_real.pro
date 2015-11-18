;+
; NAME:
;       IBETA_REAL
;
; PURPOSE:
;       This function computes the incomplete beta function, Ix(a, b) without
;	normalized by B(a,b).
;
; CATEGORY:
;       Special Functions.
;
; CALLING SEQUENCE:
;       Result = Ibeta_real(a, b, x)
;
; INPUTS:
;       A:    A scalar or vector of type integer, float or double that 
;             specifies the parametric exponent of the integrand.
;
;       B:    A scalar or vector of type integer, float or double that
;             specifies the parametric exponent of the integrand.
;
;       X:    A scalar or vector, in the interval [0, 1], of type integer, 
;	float 
;             or double that specifies the upper limit of integration.
;
; EXAMPLE:
; MODIFICATION HISTORY:
;       Written by:  wqd, Dec 1996.
;			modification of beta(a,b,x) to calculate incomplete
;			beta function including negative b value and an array
;			of the values.
;-

;======================
; the following is not modified.
function betacf, a, b, x
  on_error, 2
  eps   = 3.0e-7
  fpmin = 1.0e-30
  maxit = 100
  qab = a + b
  qap = a + 1.0
  qam = a - 1.0
    c = 1.0
    d = 1.0 - qab * x / qap
  if(abs(d) lt fpmin) then d = fpmin
  d = 1.0 / d
  h = d
  for m = 1, maxit do begin
    m2 = 2 * m
    aa = m * (b - m) * x / ((qam + m2) * (a + m2))
     d = 1.0 + aa*d
     if(abs(d) lt fpmin) then d = fpmin
     c = 1.0 + aa / c
     if(abs(c) lt fpmin) then c = fpmin
     d = 1.0 / d
     h = h * d * c
     aa = -(a + m) *(qab + m) * x/((a + m2) * (qap + m2))
     d = 1.0 + aa * d
     if(abs(d) lt fpmin) then d = fpmin
     c = 1.0 + aa / c
     if(abs(c) lt fpmin) then c = fpmin
     d = 1.0 / d
     del = d * c
     h = h * del
     if(abs(del - 1.0) lt eps) then return, h
  endfor
  message, 'Failed to converge within given parameters.'
end

function gammln, xx
  coff = [76.18009172947146d0,   -86.50532032941677d0,  $
          24.01409824083091d0,    -1.231739572450155d0, $
           0.1208650973866179d-2, -0.5395239384953d-5]
  stp = 2.5066282746310005d0
  x = xx
  y = x
  tmp = x + 5.5d0
  tmp = (x + 0.5d0) * alog(tmp) - tmp
  ser = 1.000000000190015d0
  for j = 0, n_elements(coff)-1 do begin
    y = y + 1.d0
    ser = ser + coff(j) / y
  endfor
  return, tmp + alog(stp * ser / x)
end

function ibeta_real, a, b, x
  if (min(x) lt 0 or max(x) gt 1) then message, $
    'x must be in the interval [0, 1].'

    bt = exp(a * alog(x) + b * alog(1.0 - x)) ;not devided by beta(a,b)
    nbin=n_elements(a) > (n_elements(b) > n_elements(x))
    x=fltarr(nbin)+x
    a=fltarr(nbin)+a
    b=fltarr(nbin)+b
    frac=x*0.
    for k=0,nbin-1 do frac(k)=betacf(a(k),b(k),x(k))
		; a, b, x should have the same dimesion
    beta_real=bt * frac / a
return,beta_real
end
