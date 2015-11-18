pro sh_fit,irsub4,im_csub4,back4,im_tsub4,xflux,file=file,field=field, $
  exptail=exptail,nhinc=nhinc,bnlo=bnlo,bnhi=bnhi,opfile=opfile,nfix=nfix, $
  pfix=pfix,pmax3=pmax3,pmin3=pmin3,colbin=colbin,binzero=binzero,port=port, $
  xytitle=xytitle,ncol=ncol
;** Subroutine for the x-ray shadowing project for calculating chi^2 fits
;** and plotting results (and outputting to printer if so desired).
;** Written by kachun 6 April, 1994;
;** Modified by kachun 28 May, 1994; added in option to use xstyle to
;**   fix ends of x-axis at user defined points.
;** Modified by kachun 2 June, 1994; added colbin option to bin by column
;**   density instead of equal areas.
;** Modified by kachun 13 June, 1994; cosmetic changes in titles as well
;**   as the addition of the field parameter.  Also added binzero keyword
;**   that uses modified get_vec_aream to put all the 0 pixel into one bin.
;** Modified by kachun 20 June, 1994; added port keyword for portrait plot,
;**   and xytitle option for placing title inside plot window.
;** Modified by kachun 26 June, 1994; added option to overplot the two and
;**   three-parameter fits.
if n_elements(nfix) eq 0 then nfix = .52745461
if n_elements(pfix) eq 0 then pfix = 2
if n_elements(pmax3) eq 0 then pmax3 = [1.,1.,10.]
if n_elements(pmin3) eq 0 then pmin3 = [1.e-10,1.e-10,1.e-10]
if n_elements(pmax2) eq 0 then pmax2 = [1.,1.,nfix]
if n_elements(pmin2) eq 0 then pmin2 = [1.e-10,1.e-10,nfix]
if n_elements(file) eq 0 then file = ''
if n_elements(exptail) eq 0 then exptail = ''
if n_elements(field) ne 0 then ititle = 'Field '+strtrim(string(field),2) $
  else ititle = 'rp'+file+exptail

yesno=''
ftype=''
if keyword_set(colbin) ne 0 then begin
  get_vec,irsub4,im_csub4,back4,im_tsub4,nhvc,countvc,backvc,timevc, $
    nhinc=nhinc,flux=flux,eflux=eflux,ncol=ncol
endif else begin
  if keyword_set(binzero) eq 0 then begin
    get_vec_aream,irsub4,im_csub4,back4,im_tsub4,nhvc,countvc,backvc,timevc, $
      flux=flux,eflux=eflux,nhl=nhl,nhu=nhu,ncol=ncol,n_avg=2
  endif else begin
    get_vec_aream,irsub4,im_csub4,back4,im_tsub4,nhvc,countvc,backvc,timevc, $
      flux=flux,eflux=eflux,nhl=nhl,nhu=nhu,/binzero,ncol=ncol,n_avg=2
  endelse
endelse
repeat begin
  twofit = 0
  ok = 0
  print,'For three parameter fit, type 3; for two parameter fit, type 2;'
  read,'  else overplot two and three parameter fits (default): ',ftype
  if ftype eq '3' then begin
    fit_nlvc,nhvc,countvc,backvc,timevc,xflux,bnlo=bnlo,bnhi=bnhi, $
     opfile=opfile,mflux=mflux,chimin=chimin,pmin=pmin3,pmax=pmax3
  endif else if ftype eq '2' then begin
    fit_nlvc,nhvc,countvc,backvc,timevc,xflux,bnlo=bnlo,bnhi=bnhi, $
     opfile=opfile,mflux=mflux,chimin=chimin,pfix=pfix,pmin=pmin2,pmax=pmax2
  endif else begin
    twofit = 1
    fit_nlvc,nhvc,countvc,backvc,timevc,xflux3,bnlo=bnlo,bnhi=bnhi, $
     opfile=opfile,mflux=mflux3,chimin=chimin3,pmin=pmin3,pmax=pmax3
    fit_nlvc,nhvc,countvc,backvc,timevc,xflux2,bnlo=bnlo,bnhi=bnhi, $
     opfile=opfile,mflux=mflux2,chimin=chimin2,pfix=pfix,pmin=pmin2,pmax=pmax2
  endelse
  read,'Fix range of x-values in plot? ',yesno
  if (yesno eq 'y') or (yesno eq 'yes') then begin
    print,'Range of x-values: ',min(nhl),max(nhu)
    read,'Enter in minimum x value: ',yesno
    x0 = float(yesno)
    read,'Enter in maximum x value: ',yesno
    x1 = float(yesno)
    if x0 lt x1 then xrange=[x0,x1] else $
      print,'Minimum x is not less than maximum x!'
  endif else xrange=0
  if n_elements(xrange) ne 0 and total(xrange) ne 0 then begin
    if twofit eq 0 then sh_plot,nhvc,flux,eflux,mflux,title=ititle, $
      yfactor=1.e4,errl=nhl,erru=nhu,xrange=xrange,xytitle=xytitle
    if twofit eq 1 then sh_plot,nhvc,flux,eflux,mflux3,mflux2=mflux2, $
      title=ititle,yfactor=1.e4,errl=nhl,erru=nhu,xrange=xrange, $
      xytitle=xytitle
  endif else begin
    if twofit eq 0 then sh_plot,nhvc,flux,eflux,mflux,title=ititle, $
      yfactor=1.e4,errl=nhl,erru=nhu,xytitle=xytitle
    if twofit eq 1 then sh_plot,nhvc,flux,eflux,mflux3,mflux2=mflux2, $
      title=ititle,yfactor=1.e4,errl=nhl,erru=nhu,xytitle=xytitle
  endelse
  read,'Output plot to printer? ',yesno
  if (yesno eq 'y') or (yesno eq 'yes') then begin
    if keyword_set(port) then ps else landplot
    if n_elements(xrange) ne 0 and total(xrange) ne 0 then begin
      if twofit eq 0 then sh_plot,nhvc,flux,eflux,mflux,title=ititle, $
        yfactor=1.e4,errl=nhl,erru=nhu,xrange=xrange,xytitle=xytitle
      if twofit eq 1 then sh_plot,nhvc,flux,eflux,mflux3,mflux2=mflux2, $
        title=ititle,yfactor=1.e4,errl=nhl,erru=nhu,xrange=xrange, $
        xytitle=xytitle
    endif else begin
      if twofit eq 0 then sh_plot,nhvc,flux,eflux,mflux,title=ititle, $
        yfactor=1.e4,errl=nhl,erru=nhu,xytitle=xytitle
      if twofit eq 1 then sh_plot,nhvc,flux,eflux,mflux3,mflux2=mflux2, $
        title=ititle,yfactor=1.e4,errl=nhl,erru=nhu,xytitle=xytitle
    endelse
    lx
  endif
  read,'Make another fit? ',yesno
  if strtrim(yesno,2) eq 'n' or yesno eq 'no' then ok = 1
endrep until ok
 
return
end
