pro sou_struct_fits,slist,outfile,keyname=keyname,keyvalue=keyvalue,comment=comment,equi=equi,nrow=nrow,append=append
;+
; Output the source structure into an array of text and optionally into a file
;
; slist - source structure
; outfile - output file name
; keyname,keyvalue - fits keys and values to be added to h1 of the outfile
; comm - comment to be included in h1
; equi - equinox of the source coodinates (def = J2000)
; nrow - number of output rows
; append - if set, slist is appended to the existing file
;
; writen by wqd, 2/3/2002
;-
if n_params() eq 0 then begin
print,'Calling procedure - sou_struct_fits,slist,outfile,keyname=keyname,keyvalue=keyvalue,comment=comment,equi=equi,nrow=nrow,append=append'
print,''
return
endif
;
if keyword_set(append) ne 0 then begin
    sou_fits_info,outfile,slisto,/all
    slisto=[slisto,slist]
    h0=headfits(outfile)
endif else begin
    slisto=slist
    mkhdr,h0,2                  ;minimum header
endelse
nrow=n_elements(slisto)
FXWRITE,outfile,h0
fxbhmake,h1,nrow,'srctable'
if n_elements(equi) eq 0 then equi=2000
sxaddpar,h1,'EQUINOX',equi
nkey=n_elements(keyname)
if nkey ne 0 then for k=0,nkey-1 do sxaddpar,h1,keyname(k),keyvalue(k)
if n_elements(comment) then sxaddpar,h1,'Comment',comment
if nrow ne 0 then mwrfits,slisto,outfile,h1
return
end
