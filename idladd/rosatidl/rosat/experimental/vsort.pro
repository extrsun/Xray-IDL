;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;            vsort
;
;*PURPOSE:
; A procedure to find the unique values in a vector, when the entries will
; be quantized more coarsely
;
;*CALLING SEQUENCE:
;	vsort,val,mult,trueval,indsrt,jbeg,jend,ntrue,mid=mid
;
;*PARAMETERS:
; INPUTS:
;       val      vector containing values to be sorted and quantized
;       mult     the multiplicative factor to be used for quantizing the
;                entries in val
;                the entries will be quantized as nint( mult*val )
;
; OPTIONAL INPUTS:
;       mid      keyword to tell whether the centers of the bins
;                should be integer (1 = default) or half-integer values (0)
;
; OUTPUTS:
;       trueval  the unique values of the quantized entries, sorted in
;                ascending order
;       indsrt   the indices of the entries in val which correspond to
;                the values of trueval
;       jbeg     for each element, gives the beginning index of the range
;                of elements in indsrt which correspond to the same element 
;                of trueval
;       jend     for each element, gives the ending index of the range of
;                elements in indsrt which correspond to the same element
;                of trueval
;       ntrue    number of unique quantized values
;
;*RESTRICTIONS:
;  If VAL is a large vector (> 20000 entries), then VSORT can be very slow.
;    Use VSORT_LARGE instead.
;
;*NOTES:
;    
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;  Written April 1993 by Gail Reichert (GSFC) (adapted from algorithm
;    developed by F. Varosi, GSFC)
;  Modified 10 Jul 1993 (GAR) to allow bin midpoints to be half integers
;  Modified  6 Nov 1993 (GAR) to fix bug when all values = -1
;-
;-------------------------------------------------------------------------------
;
pro vsort,val,mult,trueval,indsrt,jbeg,jend,ntrue,mid=mid
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' vsort,val,mult,trueval,indsrt,jbeg,jend,ntrue,mid=mid'
  retall
endif
if (n_elements(mid) eq 0) then mid = 1
;
;if mid=1, then bin midpoints will be integer values
;else, bin midpoints will be half-integer values
;
if (mid) then locr = nint(mult*val,/long) else locr = long(mult*val)
;
; Find the unique values of locr, and the indices of these in VAL.
;
indsrt = sort(locr)
locr = [locr(indsrt),-1]
jend = where(locr(1:*)-locr,nend)   ;end indices of duplicates
if (nend eq 0) then jend = n_elements(val) - 1     ;in case all values = -1
ntrue = n_elements(jend)            ;number unique values of quantized VAL
if (ntrue eq 1) then jbeg = 0 else $
   jbeg = [ 0, jend(0:ntrue-2)+1 ]  ;beginning indices of duplicates
;
;trueVAL = unique quantized values
;
trueval = locr(jend)/mult 
if (mid eq 0) then trueval = trueval + 0.5/mult
if (!debug ge 3) then stop,' Stopping in vsort at end'
;
return
end            ;pro vsort
