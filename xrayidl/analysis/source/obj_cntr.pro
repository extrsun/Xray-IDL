;+
; Main program to calculate the count rates for given positions in a
; list (e.g., radio objects).
; see /home/wqd/data/a2125/sou/memo  /home/wqd/data/a2125/sou/memo
;
; wqd, 11/2/2003
;-
sou_det_params,instr,dim,block,ccd,bmin,bmax,dsfrac,asfrac,psffile,bpsffile,ftail,aimoff,subfac,vpfname=vpfname,vrfname=vrfname
if N_elements(oasfrac) ne 0 then asfrac=oasfrac else asfrac=0.9
;===================================================
;get the data file parameters
fname=evtfdir+evtfname+'.fits'
file_params,fname,h,cra,cdec,expt,nra,ndec,xoff,yoff,roll,aimoff=aimoff

;get exposure maps for both source detection and hardness ratio calculations:
maphead='_i' 
if n_elements(bv) eq 0 then bv=[1,2,3,4]
map_exp,expt,tb,ta,fhead=evtfname+maphead,mapdir=mapdir,bv=[1,2,3,4],tbw=tbw
cbma=readfits(soufdir+evtfname+'_cbma.fits')
;---------
;get the count list
    row={x:0L,y:0L,energy:0,ccd_id:0}
    list_xray,fname,l,emin=min(bmin),emax=max(bmax),ccd=ccd,row=row
;calculate the count rates:
trans_dist,cra,cdec,slo.ra,slo.dec,xp,yp,angle=offaxis,/deg
sl=slo
;map_ratio,sl,l,cra,cdec,bmin,bmax,ta,cbma,block,sfrac=asfrac,probth=probth,fac_ssr=fac_ssr,offaxis=offaxis*(!size_pixel/60.),rfac=1.,count=count,back=back,sel=sel,expt=expt,rs=rs,bv=bv,/no_update,/no_print
map_ratio,sl,l,cra,cdec,bmin,bmax,ta,cbma,block,sfrac=asfrac,probth=probth,fac_ssr=fac_ssr,offaxis=offaxis*(!size_pixel/60.),rfac=rfac,count=count,back=back,sel=sel,expt=expt,rs=rs,bv=bv,/no_update,/no_print,minrs=minrs
;-----------------------------------------------
nb=n_elements(sl)
if n_elements(tbw) eq 0 then tbw=!tbw 
nbt=n_elements(bv)
tbnorm=tbw(bv-1)/total(tbw(bv-1))
for k=0,nbt-1 do expt(*,k)=expt(*,k)*tbnorm(k)
expt=total(expt,2)

up_lim = poisson_uplim(count,sigma)
cntrb_hi=imdiv(up_lim-back,expt)

struct_col_add,sl,reform([cntrb_hi, count,back,expt],nb,4),['CNTRB_HI','COUNT','BACK','EXPT'],0.0*[1,1,1,1],sln
sl=sln
end
