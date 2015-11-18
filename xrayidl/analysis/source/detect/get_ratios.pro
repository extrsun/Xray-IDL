;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; get_ratios
;
;*PURPOSE:
; get count rate ratios using count rates in three pspc bands
;
;*CALLING SEQUENCE:
; get_ratio,array_cs,array_ts,array_cm,array_tm,array_ch,array_th $
; ,array_tsub, blow=blow,bhigh=bhigh,infile=infile,outfile=outfile $
; ,block=block,slow=slow,flow=flow
;
;*PARAMETERS:
;*INPUTS:
; array_cs(m,h) - the array containing counts in the soft(medium,hard) bands
; array_ts(m,h) - the array containing the exposure in 
;		the soft(medium,hard) bands
; array_tsub - source subtracted exposure array used for a filter for
;		sources in annulus around individual sources
; block - the bin size of the image in units of 0".5
; blow, bhigh - the lower and upper boundaries of the rosat bands
; 		to be used in calculating the source and background count
; 	rates. 
; infile - the input source file (e.g. anal_v.pro)
; outfile - the output source file name. if file='no', there will be no 
; 	output file
; slow - the minmum of the signal-to-noise ratio of a source that is to be
;		included in the calculation
; flow - the minmum of the count flux value of a source that is to be
;		included in the calculation
;
;*OUTPUTS:
; a source file containing the input text plus the broad band count rates
; and the ratios of individual sources
;
;*PROCEDURE
;
;*EXAMPLES:
; get_ratios,cs,ts,cm,tm,ch,th,tss,blow=1,bhigh=2,slow6
; 
;*SUBROUTINES CALLED:
; source_info
;
;*MODIFICATION HISTORY:
; writen Sept 14, 1993 (WQD) 
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro get_ratios,array_cs,array_ts,array_cm,array_tm,array_ch,array_th $
,array_tsub, blow=blow,bhigh=bhigh,infile=infile,outfile=outfile $
,block=block,slow=slow,flow=flow
;
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - get_ratios,array_cs,array_ts,array_cm,array_tm'
print,',array_ch,array_th,array_tsub,blow=blow,bhigh=bhigh,infile=infile'
print,',outfile=outfile,block=block,slow=slow,flow=flow'
return
endif
;
sz=size(array_cs)
array_size=sz(1)
if n_elements(slow) eq 0 then slow=4.
if n_elements(flow) eq 0 then flow=0.
;if n_elements(frac) eq 0 then frac=1.5
if n_elements(block) eq 0 then block=!block
if n_elements(infile) eq 0 then infile=!seq_no+'_souanal.dat'
if n_elements(outfile) eq 0 then $
	outfile=!seq_no+'_souratio.dat' $ ;to store the information of sources 
	else outfile=strtrim(outfile,2)
if strupcase(outfile) ne 'NO' then $
	openw,un,!data_dir+outfile,/get_lun
;
; read the sources from the input source file
source_info,souno,ra,dec,sigma,cntr,i,j,ns=ns,soufile=infile,/self,slow=slow,flow=flow,text=soutext
;
hdim=(array_size-1.)*0.5
bin_arcmin=block*!size_pixel/60.

dis=sqrt((i-hdim)*(i-hdim)+(j-hdim)*(j-hdim)>1.e-10)*bin_arcmin
detect_params,dis,core_size,annulus_in,annulus_out,blow=blow,bhigh=bhigh
annulus_out = annulus_out  / bin_arcmin
annulus_in = annulus_in  / bin_arcmin
core_size = core_size / bin_arcmin
;
;calculate the min and max of the i,j for each box
;
hboxdim=fix(annulus_out+1)
jmin=nint(j-hboxdim) 
jmax=nint(j+hboxdim) 
imin=nint(i-hboxdim)
imax=nint(i+hboxdim)
;
boxdim=2*hboxdim+1              ;dimension of the boxes
;
; loop over individual sources
ratio_sm=fltarr(ns)
ratio_hm=fltarr(ns)
ratio_hms=fltarr(ns)
bcntr=fltarr(ns)

loc=lindgen(array_size,array_size)
for k=0L,(ns-1) do begin
	locs=loc(imin(k):imax(k),jmin(k):jmax(k))
      	array_css=array_cs(locs)
      	array_tss=array_ts(locs)
      	array_csm=array_cm(locs)
      	array_tsm=array_tm(locs)
      	array_csh=array_ch(locs)
      	array_tsh=array_th(locs)
      	filter=array_tsub(locs)
  	boxdims=boxdim(k)
      	dist_circle,dis,boxdims,(boxdims-1.)*0.5,(boxdims-1.)*0.5
	incore=where(dis le core_size(k),nincore)
	inann=where((dis le float(annulus_out(k))) and $
	  (dis gt float(annulus_in(k))) and (filter gt 0.),ninann)
	if ninann ne 0 and nincore ne 0 then begin
		fss=total(array_css(incore))/total(array_tss(incore))
		fsm=total(array_csm(incore))/total(array_tsm(incore))
		fsh=total(array_csh(incore))/total(array_tsh(incore))
		fbs=total(array_css(inann))/total(array_tss(inann))
		fbm=total(array_csm(inann))/total(array_tsm(inann))
		fbh=total(array_csh(inann))/total(array_tsh(inann))
		fss=(fss-fbs) & fsm=(fsm-fbm) & fsh=(fsh-fbh)
		ratio_sm(k)=(fss-fsm)/(fss+fsm)
		ratio_hm(k)=(fsh-fsm)/(fsh+fsm)
		ratio_hms(k)=(fsh+fsm-fss)/(fsh+fsm+fss)
		bcntr(k)=(fss+fsm+fsh)*nincore
	endif
endfor
;record these source into output file:
for k=0,(ns-1) do begin
 	print,soutext(k),bcntr(k),ratio_sm(k),ratio_hm(k),ratio_hms(k) $
	,format='(a75,f9.4,3f7.3)'
	if strupcase(outfile) ne 'NO' then $
 	printf,un,soutext(k),bcntr(k),ratio_sm(k),ratio_hm(k),ratio_hms(k) $
	,format='(a75,f11.5,3f7.3)'
endfor
free_lun,un
if !debug eq 1 then stop
end