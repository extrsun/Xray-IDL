function mgettok,textv,sym,nsym=nsym
;+
; gettok for a vector of string with added keywords: nsym
; nsym - the value after nsym gettok
; written by wqd, 12/29/2001
;-

if n_elements(nsym) eq 0 then nsym=1
nitems=n_elements(textv)
textvs=strarr(nitems)
for k=0,nitems-1 do begin
	texts=textv(k)
	for kk=0,nsym-1 do textvs(k)=gettok(texts,sym)
	textv(k)=texts
endfor
return,textvs
end