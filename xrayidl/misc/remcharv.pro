pro remcharv,st,char	;Remove character
;+
; NAME:
;	REMCHAR
; PURPOSE:
;	Remove all appearances of character (char) from string (st)
;
; CALLING SEQUENCE:
;	REMCHAR, ST, CHAR
;
; INPUTS:
;	ST  - String from which character will be removed.  
;	CHAR- Character to be removed from string. 
;
; EXAMPLE:
;	If a = 'a,b,c,d,e,f,g' then 
;
;	IDL> remchar,a, ','
;
;      will give a = 'abcdefg'
;
; REVISIONS HISTORY
;	Written D. Lindler October 1986
;	Test if empty string needs to be returned   W. Landsman  Feb 1991
;       modified to operate on a vector char. WQD, July, 1993
;       The name of the procedure is renamed from remchar
;-
 bst = byte(st)                                 ;Convert string to byte

 bchar = byte(char) ;& bchar = bchar(0)          ;Convert character to byte

badt=[-999]
for k=0,n_elements(bchar)-1 do begin
 bad = where( bst eq bchar(k),nbad)
	if nbad ne 0 then badt=[badt,bad]
endfor
nbadt=n_elements(badt)
if nbadt gt 1 then begin
	remove,badt(1:nbadt-1),bst
	st=string(bst)
endif 
 return
 end
