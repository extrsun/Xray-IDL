;+
; ACIS_VAR_MAIN_LOOP
;
; Main program for the KS test of  Chandra ACIS sources
;
; Requirements:
;	ACIS_VAR_MAIN_PRE has been successfully run
;	rfac --- the factor multipled to the source radius for extracting
;		source counts
; Output files:
;	files ended with v, vB, vS, and vH are created (if nooutput not = 1)
;	in the source directory
;	to view the output on screen, make noprint=0
;
; written by wqd, Sept 14, 2002.
; 
;-
if n_elements(probth) eq 0 then probth=-2.
if n_elements(noprint) eq 0 then noprint=1 ;1 - no print out of the sources
if n_elements(nooutput) eq 0 then nooutput=0 ;0 - output the results to files

sbarea=srr*(!pi/float(block)^2)
ns=n_elements(sl)
vflag=strarr(ns)
vprob=fltarr(ns)
for nb=0,nbandn-1 do begin
	print,'Band: ',bandn(nb), 'energy range: ',bmin(blow(nb)),bmax(bhi(nb))
	cbm=total(cbma(*,*,blow(nb):bhi(nb)),3)
	s=where(list.energy ge bmin(blow(nb)) and list.energy lt bmax(bhi(nb)))
        sb=where(list.energy ge bmin(blow(nb)) and $
           list.energy lt bmax(bhi(nb)) and bcid eq 1)
	;cbm=image_comp(cbm,0.5)

	;filter the source list:
	;list_image,list,xmin,ymin,a,dim,block=block,filter=ts,sel=ssel,/rsel
	tts=list(sb).time

	;calculate the background counts in source regions:
	sb=sbarea*cbm(loc)

	;print,'Performing the KS test'
	row={sn:0,nxp:0.0,nyp:0.0,vsr:0.0,vbc:0.0} 
	vslist=replicate(row,n_elements(sl))
	vslist.sn=sl.sn
	vslist.nxp=nxp 
	vslist.nyp=nyp 
	vslist.vsr=vsr
	vslist.vbc=sb
	if noprint eq 1 then $
	 var_ks,tts,list(s),tlow,thi,vslist,probth=probth,/nopr,ssel=ssel $
        else var_ks,tts,list(s),tlow,thi,vslist,probth=probth,ssel=ssel
	;output in individual bands:
	if nooutput eq 0 then sou_struct_fits,vslist,soufile+'_v'+bandn(nb)
	vs=where(vslist.vprob lt probth,nvs)
        print,'min prob = ',min(vslist.vprob)
	if nvs ne 0 then begin
		vflag(vs)=vflag(vs)+bandn(nb)
		for k=0,nvs-1 do $
			vprob(vs(k))=vslist(vs(k)).vprob < vprob(vs(k))
	endif
	print,'Number of variable sources in this band = ',nvs
endfor
struct_col_add,sl,vprob,['vprob'],0.0,vsl_t
struct_col_add,vsl_t,vflag,['vflag'],'a',vsl
if nooutput eq 0 then sou_struct_fits,vsl,soufile+'_v'
ss=where(vsl.vflag ne '',ns)
if ns ne 0 then begin
 tagsel=['IAUID','SNR','SN','VPROB','VFLAG']
 sou_struct_out,vsl(ss),vstext,tagsel=tagsel
 print,tagsel
 for k=0,ns-1 do print,ss(k),': ',vstext(k)
endif
end
