;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; anal
;
;*PURPOSE:
; analize individual sources detected with scan_v.pro by first subtracting
; sources in regions around an individual source in the consideration
;
;*CALLING SEQUENCE:
; anal, cra,cdec, array_c,array_t,array_tsub,array_tsubnew, 
; block=block,radius=radius, blow=blow,bhigh=bhigh,threshold=threshold, 
; infile=infile,outfile=outfile,append=append,frac=frac
;
;*PARAMETERS:
;*INPUTS:
; cra, cdec - the ra and dec of the image center (radian)
; array_c - the array containing counts of the image
; array_t - the array containing the exposure of the image
; array_tsub - source subtracted exposure image
; block - the bin size of the image in units of 0".5
; inr, outr - the inner and outer radii (in units of bins) of the region
; 	(centered at the image center) within which 
;	   the program is going to make the source analysis
;	(def = 60 arcmin). Annulus_out is cut to make sure sources within
;	the images are to be analyzed. But the region should be
;	smaller than that of the image.
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
; ssr - source search radius, def = core_size of the (90%) radius of a source
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
; (within circles defined by parameter ssr) which are above the 
; detect threshold. The positions of these peaks are considered to be 
; the estimated source's locations and are subtracted from the exposure image.
;
;*EXAMPLES:
; scan,center_ra,cdec,array_c,array_t,array_tsub,radius=50.
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
pro anal_map, cra,cdec,array_c,array_b,array_t,array_tsub, $
block=block,outr=outr,inr=inr,blow=blow,bhigh=bhigh,threshold=threshold, $
infile=infile,outfile=outfile,append=append,slow=slow,flow=flow $
,ssr=ssr
;
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - anal, cra,cdec, array_c,array_t'
print,',array_tsub,array_tsubnew,block=block,outr=outr,inr=inr,'
print,'blow=blow,bhigh=bhigh,threshold=threshold,infile=infile,outfile=outfile'
print,',append=append,slow=slow,flow=flow,ssr=ssr'
return
endif
;
sz=size(array_c)
array_size=sz(1)
if n_elements(threshold) eq 0 then threshold=3.
if n_elements(block) eq 0 then block=!block

if n_elements(outr) eq 0 then outr=54. ;arcmin. The actual search area is
					; outr+ssr 
if n_elements(inr) eq 0 then inr=0. ;arcmin
;
if n_elements(infile) eq 0 then infile='wl_sou47m.dat'
if n_elements(outfile) eq 0 then $
	outfile=infile+'a' $ ;to store the information of sources 
	else outfile=strtrim(outfile,2)
if strupcase(outfile) ne 'NO' then begin
	if keyword_set(append) eq 0 then $
	openw,un,outfile,/get_lun else $
	openw,un,outfile,/get_lun,/append
endif

; read the sources from the input source file
source_info,souno,sra,sdec,soufile=infile $
	,slow=slow,flow=flow,/deg
trans_dist,cra,cdec,sra,sdec,xp,yp,/deg
dis=sqrt(xp^2+yp^2)*!size_pixel/60.
sel=where(dis le outr and dis ge inr,nsel)
if nsel eq 0 then stop,'stop: no source within the radius'
dis=dis(sel)
hdim=(array_size-1.)*0.5
i=nint(hdim+xp(sel)/float(block))
j=nint(hdim+yp(sel)/float(block))
ns=nsel

detect_params,dis,core_size,blow=blow,bhigh=bhigh $
	,fac=dfac
bin_arcmin=block*!size_pixel/60.
core_size = core_size / bin_arcmin
;
;calculate the min and max of the i,j for each box
;
if n_elements(ssr) eq 0 then ssr=core_size else $
	ssr=core_size*0.+ssr ;to be a vector
;
core_size = core_size < fix(array_size/2.-dis/bin_arcmin-1-ssr) 
		;cut the outer outannulus to fit into the image
		;this mean that the source counts at the image edge can
		; be underestimated.
ssr = ssr < core_size ;the box has to be larger than ssr+max(ssr,core_size)

hboxdim=fix(ssr+core_size+1)
jmin=(j-hboxdim) 
jmax=(j+hboxdim) 
imin=(i-hboxdim)
imax=(i+hboxdim)
boxdim=2*hboxdim+1              ;dimension of the boxes
;
; loop over individual sources
x_core=fltarr(ns*2) ;there could be more than one source is a search radius
y_core=x_core
source_sn=x_core
cntr=x_core
core=x_core
nn=0
loc=lindgen(array_size,array_size)
for k=0L,(ns-1) do begin
	locs=loc(imin(k):imax(k),jmin(k):jmax(k))
      	array_cs=array_c(locs)
      	array_bs=array_b(locs)
      	array_ts=array_t(locs)
	if n_elements(array_tsub) ne 0 then begin
      		filter=array_tsub(locs)
  		boxdims=boxdim(k)
      		dist_circle,dis,boxdims,(boxdims-1.)*0.5,(boxdims-1.)*0.5
		; subtract sources only in the annulus
		c=where(filter le 0. and dis ge ssr(k),nc)
      		if nc ne 0 then array_ts(c)=0.
	endif
	get_sigma_map, array_cs,array_bs,array_ts,sigma_array,cntr_array, $
	core_size=core_size(k),radius=ssr(k),sfrac=sfrac

	; find the sources corresponding the local sigma maxima in the field
	star_search,sigma_array,cntr_array,n_newsource,threshold=threshold $
 	,xc, yc, sn, countr,star_area=ssr(k)
	if n_newsource ge 1 then begin
		x_core(nn)=i(k)-hboxdim(k)+xc
		y_core(nn)=j(k)-hboxdim(k)+yc
		source_sn(nn)=sn
		cntr(nn)=countr
		core(nn)=countr*0.+core_size(k)
		nn=nn+n_newsource
;	if n_newsource gt 1 then stop,'n_newsource gt 1'
	endif
endfor
x_core=x_core(0:nn-1)
y_core=y_core(0:nn-1)
source_sn=source_sn(0:nn-1)
cntr=cntr(0:nn-1)
core=core(0:nn-1)*bin_arcmin

;get position in a standard SASS image
ra_dist = (float(x_core) - hdim) * float(block)
dec_dist = (float(y_core) - hdim) * float(block)

trans_loct,ra_dist,dec_dist,cra,cdec,star_ra,star_dec,/deg

;convert the RA and DEC into unit of hour, degree:

trans_degree,star_ra,star_dec,ra_hour, $
 ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg
trans_dist,cra,cdec,star_ra,star_dec,xp,yp,/deg
dis=sqrt(xp^2+yp^2)*!size_pixel/60.


;record these source into output file:
newstar=indgen(nn)+1
for k=0,(nn-1) do begin
 	print, newstar(k), ra_hour(k),ra_min(k),ra_sec(k) $
 	,dec_deg(k),dec_min(k),dec_sec(k),source_sn(k),cntr(k) $
 	,dis(k),core(k),format='(I3, 2(2i4, f7.2), f9.2, f11.5,2f9.2)'
	if strupcase(outfile) ne 'NO' then $
 	  printf,un, newstar(k),' |', ra_hour(k),ra_min(k) $
	  ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',source_sn(k), $
   	  ' |',cntr(k),' |', dis(k),' |',core(k),' |' $
	  ,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,2(f9.2,a2))'
endfor
if strupcase(outfile) ne 'NO' then free_lun,un
end