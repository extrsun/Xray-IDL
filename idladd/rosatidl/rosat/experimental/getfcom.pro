pro getfcom,flist,fcom,files,dir
;
; procedure to determine which files are to be converted
; if flist = scalar & = 'A', do them all (fcom = 0)
; (if selectobs = 1, then user is allowed to select from these)
; if flist = array or flist = scalar & ne 'A' then do the list (fcom = nfil)
;
zparcheck,'GETFCOM',flist,1,7,[0,1],'List'
;
s = size(flist) & nfil = s(0) 
if (nfil eq 1) then begin       ;flist is an array
  fcom = n_elements(flist)
  files = dir+flist
endif
if (nfil eq 0) then if (strupcase(flist) ne 'A') then begin    ;a single entry
  files = [dir+flist]
  fcom = 1
endif else begin
  fcom = 0
  files = strarr(1)
endelse
;
return
end           ;pro getfcom
