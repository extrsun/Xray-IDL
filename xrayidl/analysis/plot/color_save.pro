pro color_save,itab,nametab,file=file,dir=dir,nb=nb,nt=nt
;-
; save the current color table to a file for the later use of loadct_self or
; loadct
; itab - the table number in the file
; nametab - the name of the table to be used
; file - the file name 
; dir - the directory where the file is located
; see modifyct for more details
; nb and bt - bottom and top pixels which will not be interpreted in 
;	producing the color table.
; writen by wqd. Oct 3, 1993
; add the keywords, nb and nt to prevent interpretation if the keywords are
; set. wqd, 9/26/95
; modified to include the keywords of nb and nt for top pixels used for
; various purposes. Nov, 1995 by wqd
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - color_save,itab,nametab,file=file,dir=dir'
return
endif
common colors,r_orig,g_orig,b_orig
if n_elements(file) eq 0 then file='colors1_self.tbl'
if n_elements(dir) eq 0 then dir=!idl_dir+'/analysis/plot/'
if n_elements(nb) eq 0 then nb=1
if n_elements(nt) eq 0 then nt=1
nbin=256-nb-nt
;ndc=!d.n_colors
ndc=256
nc=ndc-nb-nt
ind=((findgen(nbin)+nb)*(nc/float(nbin)) > nb ) < (ndc-1-nt)
		;loadct will read 256 element table and 
		;then interpolate them to the values of !p.color
oind=findgen(ndc)
linterp,oind,r_orig,ind,r
linterp,oind,b_orig,ind,b
linterp,oind,g_orig,ind,g
if nb ne 0 then begin
	r=[r_orig(indgen(nb)),r]
	b=[b_orig(indgen(nb)),b]
	g=[g_orig(indgen(nb)),g]
endif
if nt ne 0 then begin
	r=[r,r_orig(indgen(nt)+(ndc-nt))]
	b=[b,b_orig(indgen(nt)+(ndc-nt))]
	g=[g,g_orig(indgen(nt)+(ndc-nt))]
endif
modifyct,itab,nametab,r,g,b,file=dir+file

stop
end
