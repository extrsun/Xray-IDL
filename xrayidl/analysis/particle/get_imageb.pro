pro get_imageb,image_t,blow,bhigh,image_b,etime,tfile=tfile,dtype=dtype $
	,block=block,proc=proc
;+
; Obtain a background image containing particle events
; The tfile  is used for the time filtering
; The filter is assumed to be centered on the axis
; writen by WQD, Dec 2 , 1992
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- get_imageb,image_t,blow,bhigh,image_b,etime'
print,',tfile=tfile,dtype=dtype,block=block,proc=proc'
return
endif
;
if n_elements(block) eq 0 then block=!block
;
; first obtain the MV rate
;
if n_elements(proc) eq 0 then proc=strtrim(!proc,2)
inputs='obseq='+strtrim(!seq_no,2)+',dir='+strtrim(!data_dir,2)+',extyp=evr' $
	+',instr=p'+',proc='+proc
rsgetevr,inputs,0,sctime,rates
mvr=rates.mv ;rate
;
print,'filter the MV time with the tfile'
;
if n_elements(tfile) eq 0 then tfile=!seq_no+'_gti.dat'
filter_time,!data_dir+tfile,sctime,indsel
sctime=sctime(indsel)
mvr=mvr(indsel) ;for every two seconds
;
; get the particle distribution as a fluction of radius
;
sz=size(image_t)
dim=sz(1)
dist_circle,dis,dim,(dim-1.)*0.5,(dim-1.)*0.5 
dis=nint(dis)
sel=where(image_t gt 0.) 
dismax=max(dis(sel))
radius=indgen(dismax+1)*(block*!size_pixel/60.) ; in units of arcmin
;
chanlow=!bandch(blow-1,0) & chanhigh=!bandch(bhigh-1,1) 
get_particle,mvr,radius,chanlow,chanhigh,pcount,dtype=dtype
; get the particle background image
image_b=image_t
tran=(block*!size_pixel/60.)^2
image_b(sel)=pcount(dis(sel))*tran ; in units of counts/bin

etime=n_elements(sctime)*2. ;because of the MV step = 2 seconds

if !debug eq 1 then stop
end