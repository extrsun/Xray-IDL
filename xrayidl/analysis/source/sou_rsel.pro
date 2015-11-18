;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro sou_rsel,hdr,slist,rpsel=rpsel,outfile=outfile,ploton=ploton $
,infile=infile,probth=probth,slow=slow,image=image,s_c=s_c,relabel=relabel,remove=remove,cor=cor
;+
; Select source within an image with the option for a sub region as
; defined by rpsel
;
; hdr - header of the image from which pixels are selected
; slist - source structure (if infile is provided, slist is the output)
; rpsel - sub-region image pixel index (e.g., from defroi
; outfile - output file name (def = infile+'ratio')
; infile - input file name. If given, the source list will be read from the
;		file, overriding the input one.
; probth -  upper limit of the probability threshold for the source selection
; slow - the Signal-to-noise threshold for selecting sources (def = 0).
; image - image for display with source positions marked
; s_c - color for ploting sources 
; relabel - if set, the source number will be re-labeled, starting
;           from 1
; ploton - if set, original and sel sources are plotted
; re,pve - if set, the selected sources within rpsel will be removed
;          from the output source list slist.
;
; writen by wqd, May 23, 2003
;-
if n_params() eq 0 then begin
print,'sou_rsel,hdr,slist,rpsel=rpsel,outfile=outfile,infile=infile'
print,',probth=probth,slow=slow,image=image,s_c=s_c,relabel=relabel'
print,',ploton=ploton,remove=remove,cor=cor'
return
endif
;
;get the image parameters:
crval=sxpar(hdr,'crval*')
cra=crval(0) & cdec=crval(1)
naxis=sxpar(hdr,'naxis*')
refx=(sxpar(hdr,'crpix1')-0.5)
refy=(sxpar(hdr,'crpix2')-0.5)
del=abs(sxpar(hdr,'cdelt2'))
if abs(del+sxpar(hdr,'cdelt1')) gt 1.e-4*del $
  then stop,'delx not - dely!' else del=del*3600. ;in units of arcsec
;------------------------------------------------------------
if n_elements(infile) ne 0 then begin
	if n_elements(slow) eq 0 then slow=0.
                                ; read the sources from the input source file
	sou_fits_info,infile,slist,slow=slow,flow=flow,probth=probth,/all
	; otherwise use the input source list
    endif
if n_elements(image) eq 0 then image=fltarr(naxis(0),naxis(1))
if keyword_set(ploton) then begin
    cont_grey,image,hdr,/noc,cor=cor,/def
    if N_elements(s_c) eq 0 then s_c=!d.n_colors-1
    source_plot,'',cor,hdr,psym=6.,sym=2,sra=slist.ra,sdec=slist.dec $
      ,sn=slist.sn,/fits,s_c=s_c
endif
trans_dist,cra,cdec,slist.ra,slist.dec,xp,yp,/deg,pixsize=del

sel=where((xp+refx) gt 0 and (xp+refx) lt naxis(0) $
	and (yp+refy) gt 0 and (yp+refy) lt naxis(1), ns)
if ns eq 0 then stop,'no source within the image!' else $
	msel,sel,slist,xp,yp
	

; get source pixel locations in the image:
loc=long(yp+refy)*naxis(0)+long(xp+refx)
if N_elements(rpsel) ne 0 then begin
    if keyword_set(remove) then begin
        filter=fltarr(naxis(0),naxis(1))+1
        filter(rpsel)=0
    endif else begin
        filter=fltarr(naxis(0),naxis(1))
        filter(rpsel)=1
    endelse 
    sel=where(filter(loc) eq 1,nsel)
    if nsel eq 0 then begin
        print,'no source in the selected region!'
        return
    endif else slist=slist(sel)
endif 
if keyword_set(relabel) ne 0 then $
    slist.sn=indgen(n_elements(slist))+1
if n_elements(outfile) ne 0 then $
    sou_struct_fits,slist,outfile

if keyword_set(ploton) then source_plot,'',cor,hdr,psym=5.,sym=2 $
  ,sra=slist.ra,sdec=slist.dec,sn=slist.sn,/fits,s_c=s_c
return
end
