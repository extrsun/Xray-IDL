pro make_emap_main,expmap,totexp,bands=bands,bandws=bandws,exptail=exptail $
	,gtifile=gtifile,tfile=tfile,zero=zero,bandcr=bandcr,poehdr=poehdr $
	,obdate=obdate
;-
; Main program for calculating a file containing "good time" intervals
; and creating broad band exposure map(s)
; 
; bands - vector containing numbers of channel bands over which
;                  to calculate exposure map
; bandws - vector containing the widthes of the bands
; tfile - the time intervals in which the good time intervals will 
;	be determined
; ZERO - ONLY IF SET, AREAS WITH HIGH PARTICLE FLUX WILL BE ZEROED
; EXPTAIL - A TAIL USED IN THE NAME OF THE OUTPUT EXPOSURE MAP(S) TO 
;		DISTINGUISH DIFFERENT EXPOSURE MAPS FOR A SAME IMAGE AND 
;		A SAME ENERGY BAND, BUT DIFFERENT TIME INTERVALS
; BANDCR - THE RELATIVE COUNT RATES IN THE 7 CHANNELS 
; obdate - needed if the files are in the format of MPEUS
;
;*outputs:
; expmap - the last exposure map
; totexp - the total exposure time in units of sec
; gtifile - the name of the file containing the good time intervals
;
; writen by WQD, Oct. 12, 1993
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - make_emap_main,expmap,totexp,bands=bands'
print,',bandws=bandws,exptail=exptail'
print,',gtifile=gtifile,tfile=tfile,zero=zero,bandcr=bandcr'
print,'The default bands start with channels: 1, 4, 6 with width of 2 channels'
return
endif
if n_elements(bands) eq 0 then bands=[2,4,6]
if n_elements(bandws) eq 0 then bandws=[1,2,2]
if n_elements(exptail) eq 0 then exptail=''
if n_elements(gtifile) eq 0 then gtifile=!seq_no+'_gti'+exptail+'.dat'
if keyword_set(tfile) eq 0 then tfile=!seq_no+'_actime.dat'
print,'The time interval file ',tfile,' is used'
input_int,actime,infile=tfile
actime=transpose(actime)
inputs='obseq='+!seq_no+',dir='+!data_dir+',proc='+!proc
make_emap_m,inputs,expmap,totexp,bands=bands,bandws=bandws,actime=actime $
	,exptail=exptail,zero=zero,bandcr=bandcr,poehdr=poehdr,obdate=obdate
if strupcase(gtifile) ne 'NONE' then $
	output_int,transpose(actime),totexp,outfile=gtifile
end