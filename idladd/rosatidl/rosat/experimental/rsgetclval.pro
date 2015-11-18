pro rsgetclval,sct1,sct2,ttol,ind1,ind2,ibeg,iend
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSGETCLVAL,sct1,sct2,ttol,IND1,IND2,IBEG,IEND'
  retall
endif
;
if (n_elements(sct2) lt n_elements(sct1)) then begin
  print,' Time vector with the larger number of elements should be 2nd.'
  print,' Please check your inputs. Returning.'
  retall
endif
;
ind1 = [-1L]
ind2 = [-1L]
;
; Find elements in time vector sct2 closest to times in sct1
;
tabinv,sct2,sct1,i2
i2 = nint(i2,/long)
t2 = sct2(i2)
;
; Find corresponding elements in time vector sct1
;
tabinv,sct1,t2,i1
i1 = nint(i1,/long)
t1 = sct1(i1)
;
; Restrict both vectors to elements where the time difference is within the
; specified tolerance ttol
; If NBAD = NCHK, then none of the time differences are within the tolerance
;
nchk = n_elements(i1) - 1              ;will compare nchk to max(ibad)
ibad = where(abs(t1-t2) gt ttol,nbad)
if (nbad ge nchk) then begin
  print,' No time differences were within tolerance ',ttol
  print,' Please check your inputs. Returning.'
  retall
endif else begin                       ;some elements will be included
;
  if (nbad eq 0) then begin              ;all elements OK to include
    ib = 0
    ie = nchk
  endif else begin                       ;some elements must be excluded
;
; Find the sections of bad elements, and from these the beginning and
; ending indices of the sections of good elements
;
    if (nbad eq 1) then ibad = [ibad,ibad]        ;so min & max will work
    isect = where( (ibad(1:*)-ibad) gt 1,nsect)   ;look for bad elements
    ib = 0
    ie = min(ibad)-1
    if (nsect ge 1) then begin
      for ii=0,nsect-1 do begin
        ib = [ib,ibad(isect(ii))+1]
        ie = [ie,ibad(isect(ii)+1)-1]
      endfor
    endif
    ib = [ib,max(ibad)+1]
    ie = [ie,nchk]
;
; Fix the indices of the good sections, if the bad sections started at the
; beginning &/or ended at the end
;
    if (min(ibad) le 0) then begin
      ib = ib(1:*)
      ie = ie(1:*)
    endif
    nsect = n_elements(ib)
    if (max(ibad) eq nchk) then begin
      ib = ib(0:nsect-2)
      ie = ie(0:nsect-2)
    endif
  endelse
  nsect = n_elements(ib)
  for ii=0,nsect-1 do begin
    ind1 = [ind1,i1(ib(ii):ie(ii))]
    ind2 = [ind2,i2(ib(ii):ie(ii))]
  endfor
endelse
;
; Strip off the bogus intial values (-1L)
;
ind1 = ind1(1:*)
ind2 = ind2(1:*)
;
; Now find the indices of the good sections overall
;
t1 = sct1(ind1)
delt1 = avg(t1(1:10)-t1)
igap = where( (t1(1:*)-t1) gt delt1*1.01,ngap)
if (ngap eq 0) then begin
  ibeg = 0
  iend = n_elements(ind1)-1
endif else begin
  ibeg = [0,igap+1]
  iend = [igap,n_elements(ind1)-1]
endelse
;
return
end          ;pro rsgetclval
