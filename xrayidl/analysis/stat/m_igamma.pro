Function M_Igamma, x, a, eps, complementary = comp

;+
; NAME:
;       M_IGAMMA
; VERSION:
;       3.0
; PURPOSE:
;       Calculates the incomplete gamma function.  Replacement for the IDL 
;       IGAMMA function which accepts only real input.
; CATEGORY:
;       Mathematical function (general).
; CALLING SEQUENCE:
;       Result = M_IGAMMA (X, A [,EPS ] [,/COMPLEMENTARY ])
; INPUTS:
;    X
;       Numeric, otherwise arbitrary.
;    A
;       Numeric scalar, non-complex, positive.
; OPTIONAL INPUT PARAMETERS:
;    EPS
;       Specifies precision level.  Default is machine precision.
; KEYWORD PARAMETERS:
;    /COMPLEMENTARY
;       Switch.  If set, 1 - IGAMMA(X) is returned.
; OUTPUTS:
;       Returns the incomplete gamma function of x, unless /COMPLEMENTARY is
;       set in which case returns 1 - the incomplete gamma function.
; OPTIONAL OUTPUT PARAMETERS:
;       None.
; COMMON BLOCKS:
;       None.
; SIDE EFFECTS:
;       None.
; RESTRICTIONS:
;       For reasons having to do with machine limits on sizes of numbers, the
;       parameter A cannot be too big.  The limit is roughly given by
;
;       A < P + (Ln(2*P))/2
;
;       Where P is the (natural) logarithm of the largest number the machine 
;       can process.  The limit is machine dependent.
;
;       Also, for values of X with large negative real part the calculation 
;       cannot converge and the result is replaced with a very large number
;       (machine limit).  A warning is displayed in this case.
; PROCEDURE:
;       Uses series expansion for small ABS(X) and continued fraction
;       expansion for larger values.  Calls CAST, CONFRAC, DEFAULT, ISNUM, 
;       M_ABS, M_LNGAMMA, and TOLER, from MIDL.
; MODIFICATION HISTORY:
;       Created 15-MAR-1996 by Mati Meron.
;       Modified 20-SEP-1998 by Mati Meron.  Underflow filtering added.
;-

    on_error, 1
    sinf = machar(double=Isnum(x,/double,typ=typ))
    teps = Default(eps,Toler(x),/dtype)
    compf = keyword_set(comp)
    lp = alog(2d)*sinf.maxexp
    ln = alog(2d)*sinf.minexp

    philim = !dpi/2
    if a le 0 then message, 'A must be greater then 0!'
    if (a + 1)*(alog(a + 1) - cos(philim)) - M_lngamma(a + 2) ge lp then $
        message, 'A value of A = ' + string(a, form = '(f8.4)') + ' is too big!'
    da = double(a)
    bound = (da + 1) > sqrt(-alog(teps) > 1)

    dx = dcomplex(x)
    ddx = double(dx)
    res = 0*dx
    sran = long(res)
    tem = dx + bound
    dum = where(M_abs(tem) le 2*bound, ndum)
    if ndum gt 0 then sran(dum) = 1
    lran = long(res)
    tem = (da - 1)*alog(M_abs(dx) > 1) - ddx - M_lngamma(da)
    dum = where(tem gt ln and tem lt lp, ndum)
    if ndum gt 0 then lran(dum) = 1
    phas = 0*ddx
    dum = where(M_imaginary(dx) ne 0, ndum)
    if ndum ne 0 then phas(dum) = atan(dx(dum))

    dum = where(dx eq 0, ndum)
    if ndum gt 0 and compf then res(dum) = 1

    dum = where (sran and lran and dx ne 0, ndum)
    if ndum gt 0 then begin
        tex = dx(dum)
        tres = exp(da*alog(tex) - tex - M_lngamma(da + 1))
        tem = tres
        ltem = lindgen(ndum)
        ntem = ndum
        n = 0l
        while ntem gt 0 do begin
            n = n + 1
            tem = tem*tex(ltem)/(da + n)
            tres(ltem) = tres(ltem) + tem
            em = M_abs(tex(ltem)/(da + n + 1))
            lltem = where(M_abs(tem*em*(1+em)) gt teps*M_abs(tres(ltem)), ntem)
            if ntem gt 0 then begin
                ltem = ltem(lltem)
                tem = tem(lltem)
            endif
        endwhile
        if compf then res(dum) = 1 - tres else res(dum) = tres
    endif

    dum = where(not sran and lran, ndum)
    if ndum gt 0 then begin
        len = ceil (-alog(teps)*(4/sqrt(da + 1) > sqrt(da + 1)/4))  
        alist = double([1l,1 + lindgen(len)/2])
        tlist = 1l + 2*lindgen(len/2)
        alist(tlist) = alist(tlist) - da
        blist = dblarr(len + 1, 2)
        blist(tlist,0) = 1
        blist([0, tlist + 1],1) = 1
        tex = dx(dum)
        tres = exp((da - 1)*alog(tex) - tex - M_lngamma(da))* $
            (tex*Confrac(alist,blist,tex,eps = teps, /rel))
        if compf then res(dum) = tres else res(dum) = 1 - tres
    endif

    dum = where(not sran and not lran and M_abs(phas) le philim, ndum)
    if ndum gt 0 then if compf then res(dum) = 0 else res(dum) = 1

    dum = where(not sran and not lran and M_abs(phas) gt philim, ndum)
    if ndum gt 0 then begin
        message, 'Cannot calculate for large negative values,', /continue
        message, 'replacing with machine maximum = ' + $
            string (sinf.xmax, form = '(e14.7)'), /continue
        res(dum) = sinf.xmax
    endif

    return, Cast(res,4,typ,/fix)
end
