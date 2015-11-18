pro source_merge,cra,cdec,infile,outfile,slow=slow,flow=flow,text=text $
,sfrac=sfrac,dfac=dfac,fname=fname,sradius=sradius $
,blow=blow,bhigh=bhigh,dissort=dissort,nosort=nosort,radius=radius,rfix=rfix
;-
; Merge infiles created in source detections in individual image files
; or in individual bands
;
; infile - input file name containing a list of the image seq numbers and 
; 	will not be used if infile is given
; infile - a vector containing the source file names (including directories
;	relative to the current directory
; slow, flow - S/N and flux selection criteria for the sources to be merged
; sradius - the MAXIMUM core radius in which sources are considered to 
;	be duplicate ones (in arcmin). Def sradius=1 
; nosort - if set, no sorting of ston will be performed beform merging
; rfix - if given (arcmin), the match radius will be fixed.
;*outputs:
; outfile - output file name to contain the merged source list
; text - the merged source list
;*example
; source_merge,'rcra_sou_file.dat','rcra_sou,dat',ext='_souratio.dat',sradius=1;*NOTE:
;
; writen by WQD, Oct 17, 1993
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_merge,cra,cdec,infile,outfile'
print,',slow=slow,flow=flow,text=text,sfrac=sfrac,dfac=dfac,fname=fname'
print,',sradius=sradius,blow=blow,bhigh=bhigh,dissort=dissort,rfix=rfix'
return
endif

if n_elements(infile) eq 0 then infile='sou_wl47'

source_info,sno,ra,dec,ston,soufile=infile,slow=slow,flow=flow,text=text,/deg

if keyword_set(nosort) eq 0 then begin
	s=sort(-ston) ;to keep the detections of higher ston in merge_s
	ra=ra(s) & dec=dec(s)
	text=text(s) & ston=ston(s)
endif
merge_s,cra,cdec,ra,dec,text,blow=blow,bhigh=bhigh,radius=radius $
,sfrac=sfrac,fname=fname,sradius=sradius,dis=dis,ston=ston,dfac=dfac,rfix=rfix

; output the new source list
if n_elements(dissort) ne 0 then begin
	text=text(sort(dis)) 
endif else begin
	text=text(sort(ra))
endelse ; only text should be used later on
ns=n_elements(text)
nums=string(indgen(ns)+1)
text=strmid(nums,5,3)+strmid(text,3,strlen(text(0)))
if n_elements(outfile) eq 0 then outfile=infile+'m'
if outfile ne 'no' then begin
 	openw,unout,outfile,/get_lun
	for k=0,ns-1 do printf,unout,text(k)
endif
print,'The number of sources is ',ns
;for k=0,ns-1 do print,text(k)
free_lun,unout
return
end