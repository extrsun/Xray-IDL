pro create_weight_rmf,rmfdir,fileroot,outdir=outdir
;+
; Using the wfef file created by 'ciao' to create the final rsp file
;*INPUTS 
;  rmfdir - the directory saving the rmf files for individual positions
;          in CCD
;  fileroot - the root for the output file
;*OPTIONAL Inputs:
; outdir  - the directory which is used to create the output spectrum,
;           the default is './'
;*OUTPUTS
;  fileroot.rsp - the rsp file
; writen by Hui Dong, June 18th, 2007
;
;-

if n_elements(outdir) eq 0 then outdir='./'

outfef=fileroot+'.wfef'
refarf=fileroot+'.warf'
outrsp=fileroot+'.rsp'

row={ccd_id:0,chipx_lo:0L,chipx_hi:0L,chipy_lo:0L,chipy_hi:0L,weight:dblarr(1070),fraction:0D}
list_xray,outfef,relist
refh0=headfits(refarf)
list_xray,refarf,reflist,row={energ_lo:0D,energ_hi:0D,specresp:0D},hdr=refh1

rmffile=rmfdir+'ccd'+strtrim(relist.ccd_id,2)+'_x'+strtrim(relist.chipx_lo,2)+'-'+strtrim(relist.chipx_hi,2)+'_y'+strtrim(relist.chipy_lo,2)+'-'+strtrim(relist.chipy_hi,2)+'.rmf'
fra=relist.fraction
rspfile='ccd'+strtrim(relist.ccd_id,2)+'_x'+strtrim(relist.chipx_lo,2)+'-'+strtrim(relist.chipx_hi,2)+'_y'+strtrim(relist.chipy_lo,2)+'-'+strtrim(relist.chipy_hi,2)+'.rsp'
arffile='weight.arf'
local_rmffile='weight.rmf'

openw,lun,'rsp_fraction.list',/get_lun
rsz=size(relist)
for i=0,rsz(1)-1 do begin
    spawn,'rm '+local_rmffile
    spawn,'ln -s '+rmffile(i)+' '+local_rmffile
    FXWRITE,arffile,refh0 
    slist=reflist
    slist.specresp=relist(i).weight
    mwrfits,slist,arffile,refh1
    spawn,'marfrmf '+local_rmffile+' '+arffile+' '+rspfile(i)+' clobber=yes'
    printf,lun,rspfile(i),' ',1.0,format='(a,a1,f10.8)'
endfor
close,lun & free_lun,lun

spawn,"addrmf list='@'rsp_fraction.list rmffile="+outdir+outrsp+" clobber=yes"

spawn,'rm '+arffile+' '+local_rmffile
spawn,'rm ccd*'
spawn,'rm rsp_fraction.list'

end
