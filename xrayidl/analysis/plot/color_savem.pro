pro color_save,itab,nametab,file=file,dir=dir
;-
; save the current color table to a file for the later use of loadct_self or
; loadct
; itab - the table number in the file
; nametab - the name of the table to be used
; file - the file name 
; dir - the directory where the file is located
; see modifyct for more details
; writen by wqd. Oct 3, 1993
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - color_save,itab,nametab,file=file,dir=dir'
return
endif
common colors,r_orig,g_orig,b_orig
if n_elements(file) eq 0 then file='colors1_self.tbl'
if n_elements(dir) eq 0 then dir='~wqd/rosat/analysis/plot/'
nc=!p.color-1
ind=findgen(254)*(nc/254.) ;loadct will read 256 element table and 
		;then interpolate them to the values of !p.color
;ind=findgen(256)*(!p.color/255.) ;loadct will read 256 element table and 
		;then interpolate them to the values of !p.color
oind=findgen(nc)
linterp,oind,r_orig(1:nc),ind,r
linterp,oind,b_orig(1:nc),ind,b
linterp,oind,g_orig(1:nc),ind,g
r=[r_orig(0),r,r_orig(nc+1)]
b=[r_orig(0),b,b_orig(nc+1)]
g=[r_orig(0),g,g_orig(nc+1)]
modifyct,itab,nametab,r,g,b,file=dir+file
stop
end