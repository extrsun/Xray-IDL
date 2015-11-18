pro list_image_para,lhdr,shdr,sxmin,symin,sdim,block=block
;+
; plot count close-ups of individual sources 
;
; list - evt list
; hdr - header of the list
; sra, sdec, sn - source ra, dec, and label 
; asdim - arcsecond dimension of each plot
; xnp - number of plots along the x-axis
; detagname - the event coordinate modifier (e.g., 'm' -  mergered
;             coord)
; cor - the normalized lower and upper boundaries of the page
; charsize, thick - character size and thickness (also used for the frame)
; xlsh, ylsh - the offset of the label from the left and upper
;              boundaries of each plot in normalized coordinates
; color - color for the counts
; shdr,pcor - header of the plotted image and the normalized
;             coordinates in the plot, useful only for a single plot.
;
; written by wqd, Aug. 19, 2005
; revised to use the proper event header, July 18, 2007
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sou_cim,list,hdr,sra,sdec,asdim,xnp'
print,',detagname=detagname,cor=cor,sn=sn,charsize=charsize,thick=thick'
print,',xlsh=xlsh,ylsh=ylsh,color=color,shdr=shdr,pcor=pcor'
return
endif

lcrval=sxpar(lhdr,'crval*')
ldel=sxpar(lhdr,'cdelt*')
lcrpix=sxpar(lhdr,'crpix*')

sdim=sxpar(shdr,'naxis*')
scrval=sxpar(shdr,'crval*')
sdel=sxpar(shdr,'cdelt*')
scrpix=sxpar(shdr,'crpix*')
block=nint(sdel/ldel)
sldim=sdim*block
trans_dist,lcrval(0),lcrval(1),scrval(0),scrval(1),xp,yp,pix=ldel(1)*3600.,/deg
sxmin=nint(lcrpix(0)+xp-0.5*sldim(0)+0.5)
symin=nint(lcrpix(1)+yp-0.5*sldim(1)+0.5)
block=block(1)
end
