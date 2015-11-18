;   sh_600045
;** IDL script for reducing rp600045 data.  Since absorption only appears
;** in the soft energy band, only one image is loaded and fitted.
;** Written by kachun 1 August, 1993.

;** Define input parameters:
inrad = 40         ;** Radius of center source to cut out.
outrad = 55        ;** Inner radius in arcmin of image to keep for analysis.
etlow = 2900.      ;** Minimum exposure image value for use in fit.
file = '600045'    ;** Image number.
frac = 0.25        ;** Factor of resizing when compressing images.
comp = 1           ;** Computer type; vela = 0, other Sparc = 1.
op_limit = 6.      ;** Maximum opacity value for use in fit.
bnlo = 0           ;** Low energy band value (0, 1, or 2) of images in fit.
bnhi = 0           ;** High energy band value (0, 1, or 2) of images in fit.
blow = 1           ;** Lowest energy band of image to use in get_image.
bhigh = 2          ;** Highest energy band of image to use in get_image.
bin_size = 30/frac ;** Bin size of compressed images.
nhback = 0.0       ;** Background (to be found) to subtract from IRAS image.
ncol = 25          ;** Number of bins to use in fit.
exptail = 'all'    ;** Suffix of image file.
field = '1'        ;** Field number in paper.
fxytitle = [3.70,8.25] ;** Location of title inside fit plot (using xyouts).
irtail = '_ir4.fit';** Suffix of IRAS file.
nhinc = 0.394      ;** Size of column density interval.
slow = 3.5         ;** Minimum signal-to-noise ratio for source subtraction.
flow = 0.00330*2.  ;** Minimum count rate for source subtraction.
resid0 = .000158   ;** Residual correction factor derived from MPE data
                   ;** in units of ct/sec/arcmin^2.
factor = 1.5       ;** Size factor for source subtraction.
nfix = .51352030   ;** Value of fixed normalization.
soufile = 'rp'+file+'_sou_12.dat' ;** Source data file.
opfile = '~kachun/shadows/phoct_dat.ray_a.' ;** Absorption data file prefix.
workdir = '~kachun/shadows'               ;** Working directory.

defsysv,'!data_dir','/scratch1/kachun/rp'+file+'/'

;** Get ROSAT and IRAS images; set factor for source subtraction to 2. to
;** get rid of extended source:
sh_get,im_t0,im_c0,im_tsub0,ir,irh,comp,blow=blow,bhigh=bhigh, $
  exptail=exptail,irtail=irtail,file=file,factor=factor,slow=slow, $
  flow=flow,soufile=!data_dir+soufile
if workdir ne 'no' then cd,workdir
im_c0(where(im_tsub0 le 0.))=0.

;** Get solar background (after converting back to bin^-2) for
;** ROSAT image:
;solar_image,blow,bhigh,solar_f0,solar_fe0,slow=slow,factor=factor, $
; exptail=exptail,soufile=!data_dir+soufile,flow=flow
;solar_f = solar_f0/16.
;solar_fe = solar_fe0/16.

;** Convert residual correction factor into ct sec^-1 bin^-2:
resid = resid0/16.
print,'Residual background: ' + strtrim(resid0,2) + ' ct/sec/arcmin^2.'

;** Create count and exposure maps for later smoothing:
im_c4 = im_c0     ;** 512x512; no src. and outrad subtraction.
im_t4 = im_t0     ;** 512x512; no src. and outrad subtraction.
im_ts4 = im_tsub0 ;** 512x512; no IRAS src. and outrad subtraction.

;** Subtract out IRAS background:
if comp eq 1 then env2,file else env,file
ir0_out = readfits('rp600045_irback.fit',ir0h)
print,'Subtracting IRAS background'
irs = ir - ir0_out
if n_elements(where(irs le 0.)) gt 1 then irs(where(irs le 0.)) = 0.
cd,workdir

;** Use only inner outrad arcminutes of data:
sh_sub,im_tsub0,im_c0,im_csub0,irs,irsub0,outrad=outrad,inrad=inrad

;** Compress the images by a factor of 1./frac, and then remove the
;** outermost ring of pixels to take out a compression artifact:
print,'Compressing images:'
sh_comp,irsub0,irsub4,im_csub0,im_csub4,im_tsub0,im_tsub4,frac=frac, $
  outrad=outrad,file=file;,/zero

;** Do fit of the compressed images:
nhlow = min(irsub4)

back4 = im_tsub4*(resid0) ;** 128x128; IRAS src. and outrad subtracted.

yesno = ''
print,'Using zeroed IRAS image . . .'
sh_fit,irsub4,im_csub4,back4,im_tsub4,xflux,file=file,exptail=exptail, $
  field=field,nhinc=nhinc,bnlo=bnlo,bnhi=bnhi,opfile=opfile,nfix=nfix, $
  /binzero,/port,xytitle=fxytitle,ncol=ncol

;** Smooth for cont_grey subroutine:
yesno=''
read,'Plot cont_grey? ',yesno
if (yesno ne 'y') and (yesno ne 'yes') then stop

;** Smooth full 60' 4'-bin counts image (or 32x32 image) and then
;** blow back up to 1' bin size for comparison with IRAS image:
;sh_sub,im_ts4,im_c4,im_c4,im_t4,im_t4,outrad=outrad
im_c4(where(im_ts4 le 0.)) = 0.
sh_comp,im_t4,im_t4,im_c4,im_c4,im_ts4,im_ts4,frac=frac/4.,outrad=60, $
  /ircut,/ircomp
bk4 = im_ts4*(resid0)*16. ;** Convert from arcmin^-2 to bin^-2.
inimage= imdiv((im_c4-bk4),im_ts4)
inimage = inimage*(.5*bin_size/4./60.)^2  ;** Convert to arcmin^-2.
image_median,2,inimage,im_t4,im_ts4,flx,binmin=9,lmin=1
 
flx = image_comp(flx,4.,sample=1)
flx0 = flx
flx0(where(im_tsub4 le 0.)) = 0.
flx0 = image_cut(flx0,outrad,block=bin_size)
 
im_ts4 = image_comp(im_ts4,4.)
im_ts4 = image_cut(im_ts4,outrad,block=bin_size)
sel = where(im_ts4 gt 0.)
 
;** Plot IRAS contour and ROSAT greyscale plots:
frg = max(flx0(sel))-min(flx0(sel))
irs4 = image_cut(irsub4,outrad,/rec,block=bin_size)
modhdr,irh,irh4,dimx=110,dimy=110
 
n = 0.42
gmin = min(flx0(sel))+n*frg
gmax = .00092
cln = [1,0,0,0,0,0]
lvl = [0.2,0.7,1.2,1.7,2.2,2.7]
sh_greypl,irs4,irh4,flx0,file=file,exptail=exptail,soufile=soufile, $
  lvl=lvl,cln=cln,gmin=gmin,gmax=gmax,field=field,/xytitle,corner=corner, $
  ntickx=18
 
;** Create residual image:
read,'Plot residuals? ',yesno
if (yesno ne 'y') and (yesno ne 'yes') then stop
sh_opac,(xflux(2)*irsub4)*.01,image_op,bnlo,opfile=opfile
mimage=xflux(0)+xflux(1)*exp(image_op)
mimage = mimage*(.5*bin_size/60.)^2     ;** Convert to arcmin^-2.
mimage(where(im_tsub4 le 0.)) = 0.
mimage = image_cut(mimage,outrad,/rec,block=bin_size)
r_flx = imdiv(abs(flx0 - mimage),mimage) ;** Residual image.
 
!p.thick=2.75
!p.charsize=1.5
!p.charthick=1.5
cont_grey,irs4,irh4,bscale(r_flx),type=1,levels=lvl,c_line=cln, $
  subtitle='Residual of rp'+file+exptail+': f12',/putinfo
print,'Min. and max. of residuals: ',minmax(r_flx)
!p.thick=1.
!p.charsize=1.
!p.charthick=1.
 
stop
end
