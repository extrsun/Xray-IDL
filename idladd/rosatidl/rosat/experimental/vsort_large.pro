;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;            vsort_large
;
;*PURPOSE:
; A procedure to sort large vectors, when the values will be quantized
; more coarsely
;
;*CALLING SEQUENCE:
;	vsort_large,val,mult,trueval,indsrt,jbeg,jend,nsize=nsize,mid=mid
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
; OPTIONAL INPUTS:
;       nsize    the number of elements to sort in one section
;                (val will be sorted section by section)
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
;  The number of entries in val should be larger than NSIZE (default
;    for NSIZE = 20000). If not, use procedure VSORT.
;
;*NOTES:
;  The procedure works by dividing VAL into sections of NSIZE elements,
;    and sorting section by section. The unique, quantized values in each 
;    section are found, and the ranges of indices in VAL corresponding to
;    these unique values. The unique values for the sections are then
;    combined, and the combined quantized values are sorted and duplicate
;    values removed. New vectors INDSRT, JBEG, and JEND are then constructed
;    from the combined vectors for the individual sections.
;    
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;  Written April 1993 by Gail Reichert (GSFC) (adapted from algorithm
;    developed by F. Varosi, GSFC)
;  Modified 10 Jul 1993 (GAR) to allow bin midpoints to be half integers
;-
;-------------------------------------------------------------------------------
;
pro vsort_large,val,mult,trueval,indsrt,jbeg,jend,ntrue,nsize=nsize,mid=mid
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' vsort_large,val,mult,trueval,indsrt,jbeg,jend,ntrue,nsize=nsize',$
        'mid=mid'
  retall
endif
if (n_elements(nsize) eq 0) then nsize = 20000.
if (n_elements(mid) eq 0) then mid = 1
;
nval = n_elements(val)
if (nval le nsize) then begin
  print,' Number of elements in VAL = ',nval,'. NSIZE =',nsize
  print,' VAL will be sorted in a single section anyway. Use VSORT instead.'
  print,' Returning.'
  retall
endif
;
; if mid=1, then bin midpoints will be integer values
; else, bin midpoints will be half-integer values
;
if (mid) then locrall = nint(mult*val,/long) else locrall = long(mult*val)
jbegsav = long(0*val)
jendsav = jbegsav
indsav = jbegsav
valsav = val*0.
;
nmax = n_elements(val)
nsect = fix(nmax/nsize) + 1
nprtot = 0
if (!debug ge 4) then stop,' Stopping in vsort_large after setting up'
;
for kk=0,nsect-1 do begin            ;sort locrall section by section
  kb = kk*nsize
  ke = (kb + nsize - 1) < (nmax - 1)
  ind = sort(locrall(kb:ke)) + kb
;
  locr = [locrall(ind),-1]
  kend = where(locr(1:*)-locr)        ;end indices of val duplicates
  npr = n_elements(kend)           ;number unique values of binned val
  if (npr eq 1) then kbeg = 0 else $
     kbeg = [ 0, kend(0:npr-2)+1 ]    ;start indices of val dups
;
  indsav(kb) = ind
  jbegsav(nprtot) = kbeg + kb
  jendsav(nprtot) = kend + kb
  valsav(nprtot) = locr(kend)/mult              
  nprtot = nprtot + npr
endfor
if (!debug ge 4) then stop,' Stopping in vsort_large after jbegsav defined'
;
jbegsav = jbegsav(0:nprtot-1)
jendsav = jendsav(0:nprtot-1)
valsav = valsav(0:nprtot-1)
;
sortall = sort(valsav)
locr = [valsav(sortall),-1]
kend = where(locr(1:*)-locr)        ;end indices of val duplicates
ntrue = n_elements(kend)            ;number unique values of binned val
if (ntrue eq 1) then kbeg = 0 else $
   kbeg = [ 0, kend(0:ntrue-2)+1 ]  ;beginning indices of val duplicates
;
;trueVAL = unique quantized values
;
trueval = locr(kend)                ;final list of unique values
if (mid eq 0) then trueval = trueval + 0.5/mult
;
kbegsav = jbegsav(sortall)
kendsav = jendsav(sortall)
if (!debug ge 4) then stop,' Stopping in vsort_large after kbegsav defined'
;
nct = 0L
nprev = 0L
indsrt = indsav*0.
jbeg = kbeg*0
jend = kend*0
for nn=0L,ntrue-1 do begin
  kb = kbeg(nn) 
  ke = kend(nn)
  for kk=kb,ke do begin
    kbs = kbegsav(kk) 
    kes = kendsav(kk)
    indsrt(nct) = indsav(kbs:kes)
    nct = nct + (kes-kbs+1)
  endfor
  jbeg(nn) = nprev
  jend(nn) = nct-1
  nprev = nct
endfor
if (!debug ge 3) then stop,' Stopping in vsort_large at end'
;
return
end            ;pro vsort_large
