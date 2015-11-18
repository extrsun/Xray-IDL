pro exp_spec,image_f,blow,bhigh,image_fspec,specfile=specfile, $
block=block,frac=frac
;+
; compare the flat fielding image with that the expected vignetting image
; to reject regions influnced by ribs
; image_f - the flat fielding image which may be produced with get_image
; blow,bhigh - the lower and upper bounds of the bands (1-7) used in producing
;		the flat fielding image
; image_fspec - the output flat fielding image with regions of 
;		0< image_f < frac*image_vig set equal to zero.
; specfile - a user input spectral data (channel 0-33) may be produced with
;		data in the central area (e.g. radius=40 bins) using 
;			"spec_get_dif"
; writen by WQD, Dec. 1, 1992
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - exp_spec,image_f,blow,bhigh,image_fspec, '
print,'specfile=specfile,block=block,frac=frac'
return
endif
;
if n_elements(block) eq 0 then block=!block
if n_elements(frac) eq 0 then frac=0.8
;
sz=size(image_f)
dim=sz(1)
dist_circle,dis,dim,(dim-1.)*0.5,(dim-1.)*0.5
dis=nint(dis)
sel=where(image_f gt 0.)
dismax=max(dis(sel))
;
angle=indgen(dismax+1)*(block*!size_pixel/60.) ;in units of arcmin
chmin=!bandgroup(blow,0)
chmax=!bandgroup(bhigh,1)
;
; get the vignetting values at the angles
;
vign_radial,angle,chmin,chmax,vig,specfile=specfile
;
; create the vignetting image
;
image_vig=image_f
image_vig(sel)=vig(dis(sel))
if !debug eq 1 then stop
;
; reject regions with the serious influnce of instrument structures
;
image_fspec=image_f
sel=where(image_f lt frac*image_vig and image_f gt 0.,nsel)
if nsel ne 0 then image_fspec(sel)=0.
;
if !debug eq 2 then stop
end