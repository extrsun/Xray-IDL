;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; anal_v
;
;*PURPOSE:
; analize individual sources detected with scan_v.pro by first subtracting
; sources in regions around an individual source in the consideration
;
;*CALLING SEQUENCE:
; anal_v, center_ra,center_dec, array_c,array_t,array_tsub,array_tsubnew, 
; block=block,radius=radius, blow=blow,bhigh=bhigh,threshold=threshold, 
; infile=infile,outfile=outfile,append=append,frac=frac
;
;*PARAMETERS:
;*INPUTS:
; center_ra, center_dec - the ra and dec of the image center (radian)
; array_c - the array containing counts of the image
; array_t - the array containing the exposure of the image
; array_tsub - source subtracted exposure image
; block - the bin size of the image in units of 0".5
; radius - the radius (in units of bins) of the circle 
; 	(centered at the image center) within which 
;	   the program is going to make the source analysis
;	(def = 50 arcmin)
; blow, bhigh - the lower and upper boundaries of the rosat bands
; 		to be used to get the source detection radii in detect_params
;		called by get_simga_v
; threshold - the lower limit of the signal-to-noise ratio of a peak which
;	is considered to be a source candidate (def = !threshold)
; infile - the input source file (from scan_v.pro)
; outfile - the output source file name. if file='no', there will be no 
; 	output file
; append - if set, the output will append to the existing file
; frac - the fraction of the core_size used as the radius to search for
;	local maxima of signal-to-noise ratios.
;
;*OUTPUTS:
; array_tsubnew - the array containing the exposure after subtracting regions
; 	contaminated with sources which have passed this program
; *_souanal.dat - the source file name if the keyword file is not provided
;
;*PROCEDURE
; use moving box technique to calculate the sigma (signal-to-noise ratio)
; of each pixels in subimages around individual input source positions
; (sources in the surrounding regions are first subtracted) 
;  and then search for the maximum local peaks
; (within circles defined by parameter star_area) which are above the 
; detect threshold. The positions of these peaks are considered to be 
; the estimated source's locations and are subtracted from the exposure image.
;
;*EXAMPLES:
; scan,center_ra,center_dec,array_c,array_t,array_tsub,radius=50.
;	, blow=4,bhigh=7,threshold=2.
;
;*RESTRICTIONS:
; The radius for the source analysis should be smaller than the half size
; of the image minus the annulus_out at the radius
;
;*NOTES:
; The source detection parameters are presented in the procedure detect_params.
; !threshod is used as a system variable
; 
;*SUBROUTINES CALLED:
; get_sigma_v
; star_search
; source_sub
;
;*MODIFICATION HISTORY:
; writen Sept 13, 1993 (WQD) 
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro anal_v, center_ra,center_dec, array_c,array_t,array_tsub,array_tsubnew, $
block=block,radius=radius, blow=blow,bhigh=bhigh,threshold=threshold, $
infile=infile,outfile=outfile,append=append,frac=frac
;
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - anal_v, center_ra,center_dec, array_c,array_t'
print,',array_tsub,array_tsubnew,block=block,radius=radius,'
print,'blow=blow,bhigh=bhigh,threshold=threshold,infile=infile,outfile=outfile'
print,',append=append,frac=frac'
return
endif
;
sz=size(array_c)
array_size=sz(1)
if n_elements(block) eq 0 then block=!block
if n_elements(radius) eq 0 then radius=50 ;arcmin
;
if n_elements(frac) eq 0 then frac=1.5
if n_elements(infile) eq 0 then infile=!seq_no+'_sou.dat'
; read the sources from the input source file
source_info,souno,ra,dec,sigma,cntr,i,j,ns=ns,soufile=infile,slow=0.,flow=0.,/self
;
if n_elements(outfile) eq 0 then $
	fname=!seq_no+'_souanal.dat' $ ;to store the information of sources 
	else fname=strtrim(outfile,2)
if strupcase(outfile) ne 'NO' then begin
	if keyword_set(append) eq 0 then $
	openw,un,!data_dir+fname,/get_lun else $
	openw,un,!data_dir+fname,/get_lun,/append
endif
;
hdim=(array_size-1.)*0.5
bin_arcmin=block*!size_pixel/60.

dis=sqrt((i-hdim)*(i-hdim)+(j-hdim)*(j-hdim)>1.e-10)*bin_arcmin

	sel=where(dis le radius,nsel)
	if nsel eq 0 then stop,'stop: no source within the radius'
	dis=dis(sel)
	i=i(sel)
	j=j(sel)
	ns=nsel


detect_params,dis,core_size,annulus_in,annulus_out,blow=blow,bhigh=bhigh
annulus_out = annulus_out  / bin_arcmin
annulus_in = annulus_in  / bin_arcmin
core_size = core_size / bin_arcmin
;
;calculate the min and max of the i,j for each box
;
hboxdim=fix(annulus_out+core_size+1)
jmin=(j-hboxdim) 
jmax=(j+hboxdim) 
imin=(i-hboxdim)
imax=(i+hboxdim)
;
boxdim=2*hboxdim+1              ;dimension of the boxes
;
; loop over individual sources
x_core=fltarr(ns)
y_core=fltarr(ns)
source_sn=fltarr(ns)
cntr=fltarr(ns)
nn=0
loc=lindgen(array_size,array_size)
for k=0L,(ns-1) do begin
	locs=loc(imin(k):imax(k),jmin(k):jmax(k))
      	array_cs=array_c(locs)
      	array_ts=array_t(locs)
      	filter=array_tsub(locs)
  	boxdims=boxdim(k)
      	dist_circle,dis,boxdims,(boxdims-1.)*0.5,(boxdims-1.)*0.5
	; subtract sources in regions roughly in the annulus
	c=where(filter le 0. and dis ge frac*core_size(k),nc)
      	if nc ne 0 then array_ts(c)=0.
      	bin_sel=where(dis le core_size(k),nbin)

	get_sigma_v, array_cs,array_ts,sigma_array,cntr_array, $
	block=block,blow=blow,bhigh=bhigh,bin_sel=bin_sel $
	,core_size=replicate(core_size(k),nbin) $
	,annulus_in=replicate(annulus_in(k),nbin) $
	,annulus_out=replicate(annulus_out(k),nbin)

	; find the sources corresponding the local sigma maxima in the field
	star_search_v,sigma_array,cntr_array, n_newsource,threshold=threshold $
 	,xc, yc, sn, countr, block=block,star_area=replicate(core_size(k),nbin)

	if n_newsource ge 1 then begin
		x_core(nn)=xc
		y_core(nn)=yc
		source_sn(nn)=sn
		cntr(nn)=countr
		nn=nn+1
	endif
endfor
nn=nn-1
if nn ne ns then begin
	x_core=x_core(0:nn-1)
	y_core=y_core(0:nn-1)
	source_sn=source_sn(0:nn-1)
	cntr=cntr(0:nn-1)
endif
;get position in a standard SASS image
x_core=i-hboxdim+x_core
y_core=j-hboxdim+y_core
ra_dist = (float(x_core) - hdim) * float(block)
dec_dist = (float(y_core) - hdim) * float(block)

trans_loct,ra_dist,dec_dist,center_ra,center_dec,star_ra,star_dec

;convert the RA and DEC into unit of hour, degree:

trans_degree,star_ra,star_dec,ra_hour, $
 ra_min,ra_sec,dec_deg,dec_min,dec_sec

;record these source into output file:
newstar=indgen(nn)+1
for k=0,(nn-1) do begin
 	print, newstar(k), ra_hour(k),ra_min(k),ra_sec(k) $
 	,dec_deg(k),dec_min(k),dec_sec(k), source_sn(k), cntr(k), $
 	x_core(k),y_core(k),format='(I3, 2(2i4, f7.2), f9.2, f11.5,2I4)'
	if strupcase(outfile) ne 'NO' then $
 	  printf,un, newstar(k),' |', ra_hour(k),ra_min(k) $
	  ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',source_sn(k), $
   	  ' |',cntr(k),' |', x_core(k),' |',y_core(k),' |' $
	  ,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,2(I4,a2))'
endfor
free_lun,un

;subtract detected sources from the field
if n_params() ge 6 then $
    array_tsubnew=source_sub_v(array_t,center_ra,center_dec,star_ra,star_dec, $
	cntr,blow=blow,bhigh=bhigh,block=block,factor=frac)
print,'There are total ',nn,' sources found in this area, compared with'
print,'the input number of sources, ',ns
if !debug eq 1 then stop
end