pro gt_ebound, rsfil, nenerg, middl, lower, hiher, status
;-
; get the energy bounds from RMF a file
;+
if n_params(0) eq 0 then begin
print,'Call Seq - gt_ebound, rsfil, nenerg, middl, lower, hiher, status'
return
endif

status = 0
; Need to find the extension containing the energies used in the RMF.

iext = 1
found = 0

while not found do begin
  h = headfits(rsfil,ext=iext)
  rstnam = sxpar(h,'EXTNAME')
  if !err eq -1 then begin
    print,'ASCAARF: cannot find EXTNAME keyword in '+rsfil
	status=1
    retall
  endif
  found =  (strtrim(rstnam,2) eq 'EBOUNDS')
  iext = iext + 1
endwhile

iext = iext-1
print,'Reading energy bounds from '+rsfil
tab = readfits(rsfil,h,ext=iext,/silent)
lower = tbget(h,tab,'E_MIN')
hiher = tbget(h,tab,'E_MAX')
middl = (lower+hiher)/2.
nenerg=n_elements(middl)
return
end
