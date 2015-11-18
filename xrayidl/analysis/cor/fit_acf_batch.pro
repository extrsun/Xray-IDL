pro fit_acf_batch,dim,device=device,seqno=seqno,outfile=outfile,endbin=endbin $
,slow=slow,flow=flow,factor=factor,exptail=exptail,tail=tail
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - fit_acf_batch,dim,device=device,seqno=seqno,outfile=outfile,endbin=endbin,slow=slow,flow=flow'
return
endif
if n_elements(tail) eq 0 then tail='acf'
if n_elements(device) eq 0 then device='x'
set_plot,device
if device eq 'ps' then device,/portrait,yoffset=1.,ysize=25
;
erase
pl=0.3
pr=0.7
pb=0.1
pt=0.98
pd=(pt-pb)/6
pbn=pb

if n_elements(seqno) ne 0 then begin 
	!seq_no=seqno
	!data_dir='/data1/wqd/archives/'+strtrim(!seq_no,2)+'/'
endif
if n_elements(outfile) ne 0 then openw,unit,!data_dir+outfile,/get

get_image,t,c,ts,dim=dim,slow=slow,flow=flow,/tonly,blow=2,bhigh=3,factor=factor,exptail=exptail
ta='22'
image_prf_acf,ts,ta,a,prfacf
!p.position=[pl,pbn,pr,pbn+pd*0.8]

fit_acf,ta,unit=unit,acf_p=prfacf,ncomp=1,endbin=endbin $
,acffile=!seq_no+'_'+tail+ta+'.dat' $
,iacffile=!seq_no+'_acfi23.dat'
xyouts,(pl+pr)*0.6,pbn+pd*0.8-0.025,/normal,alignment=0.5,'R2'
  pbn=pbn+pd
ta='33'
image_prf_acf,ts,ta,a,prfacf
!p.position=[pl,pbn,pr,pbn+pd*0.8] 
fit_acf,ta,unit=unit,acf_p=prfacf,ncomp=1,endbin=endbin,acffile=!seq_no+'_'+tail+ta+'.dat',iacffile=!seq_no+'_acfi23.dat'
xyouts,(pl+pr)*0.6,pbn+pd*0.8-0.025,/normal,alignment=0.5,'R3'
 pbn=pbn+pd

get_image,t,c,ts,dim=dim,slow=slow,flow=flow,/tonly,blow=4,bhigh=5,factor=factor,exptail=exptail
ta='44'
image_prf_acf,ts,ta,a,prfacf
!p.position=[pl,pbn,pr,pbn+pd*0.8] 
fit_acf,ta,unit=unit,acf_p=prfacf,ncomp=1,endbin=endbin,acffile=!seq_no+'_'+tail+ta+'.dat',iacffile=!seq_no+'_acfi45.dat'
xyouts,(pl+pr)*0.6,pbn+pd*0.8-0.025,/normal,alignment=0.5,'R4'
 pbn=pbn+pd

ta='55'
image_prf_acf,ts,ta,a,prfacf
!p.position=[pl,pbn,pr,pbn+pd*0.8] 
fit_acf,ta,unit=unit,acf_p=prfacf,ncomp=1,endbin=endbin,acffile=!seq_no+'_'+tail+ta+'.dat',iacffile=!seq_no+'_acfi45.dat'
xyouts,(pl+pr)*0.6,pbn+pd*0.8-0.025,/normal,alignment=0.5,'R5'
 pbn=pbn+pd

get_image,t,c,ts,dim=dim,slow=slow,flow=flow,/tonly,blow=6,bhigh=7,factor=factor,exptail=exptail
ta='66'
image_prf_acf,ts,ta,a,prfacf
!p.position=[pl,pbn,pr,pbn+pd*0.8] 
fit_acf,ta,unit=unit,acf_p=prfacf,ncomp=1,endbin=endbin,acffile=!seq_no+'_'+tail+ta+'.dat',iacffile=!seq_no+'_acfi67.dat'
xyouts,(pl+pr)*0.6,pbn+pd*0.8-0.025,/normal,alignment=0.5,'R6'
 pbn=pbn+pd

ta='77'
image_prf_acf,ts,ta,a,prfacf
!p.position=[pl,pbn,pr,pbn+pd*0.8]  
fit_acf,ta,unit=unit,acf_p=prfacf,ncomp=1,endbin=endbin,acffile=!seq_no+'_'+tail+ta+'.dat',iacffile=!seq_no+'_acfi67.dat'
xyouts,(pl+pr)*0.6,pbn+pd*0.8-0.025,/normal,alignment=0.5,'R7'

xyouts,pl-0.08,(pb+pt)*0.5,/normal,alignment=0.5,'ACF',orientation=90
xyouts,(pl+pr)*0.5,pb-0.05,/normal,alignment=0.5,'Angle (arcmin)' 

if n_elements(outfile) ne 0 then free_lun,unit
!p.position=0.
if !debug eq 4 then stop
end