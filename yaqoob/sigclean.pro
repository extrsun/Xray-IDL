pro sigclean,plist,gti,clgti,sigma=sigma,bin=bin,plt=plt,verb=verb,grpmin=grpmin,fits=fits,mint=mint,goodpha=goodpha,phaname=phaname,med=med,qdpin=qdpin,qdpout=qdpout,make_qdp=make_qdp, loglun=loglun

	if n_params(0) eq 0 then begin
	  print,'sigclean,plist,gti,clgti,sigma=sigma,bin=bin,plt=plt,grpmin=grpmin,mint=mint,goodpha=goodpha,phaname=phaname,med=med,qdpin=qdpin,qdpout=qdpout'
	  print,'Returns new gti (clgti) with gtis removed that deviate'
	  print,'from the mean cnt rate by more than sigma st. devs.'
	  print,'Mean calculated using bins with more than 10 cts'
	  print,'Poisson statistics used for bins with less than 10 cts'
	  print,'sigma = 3 (default)'
	  print,'Currently only operates on gtis with at least 10 cnts'
	  print,'Plot results if /plt is set'
          print,'  initial/final qdp files saved in qdpin/qdpout if given'
	  print,'If bin is supplied, bin light curve rather then using gti'
	  print,'If grpmin is supplied, rebin light curve to at least grpmin counts/bin'
	  print,'If fits is supplied, save gti in a fits file'
	  print,'If mint is set, bin data such that no bin is smaller than size given in bin'
	  print,'   if bin is not supplied, only allow gti with nsec>mint'
	  print,'phaname = pha or pi (def. = pha)
	  print,'goodpha=[minpha,maxpha]
	  print,'     def [100,1800] pha for SIS, [71,850] pi for GIS, [10,240] pi for PSPC' 
	  print,'/med median is to be used to avg. cnt rate'
	  retall
	endif

        if n_elements(loglun) eq 0 then loglun = -1
 
	instr = tag_names(plist,/struct)
	if n_elements(phaname) eq 0 then if strpos(instr,'SEVE') ge 0 then $
	  phaname = 'pha' else phaname ='pi'
	if n_elements(goodpha) eq 0 then begin
	  if strpos(instr,'SEVE') ge 0 then goodpha = [100,1800]
	  if strpos(instr,'GEVE') ge 0 then goodpha = [71,850]
	  if strpos(instr,'PSPC') ge 0 then goodpha = [10,240]
	endif

	if n_elements(sigma) eq 0 then sigma = 3.0

	if phaname eq 'pha' then begin
	  printf, loglun, 'sigclean: Filtering pha from ' + strn(goodpha(0)) $
            +  ' to ' + strn(goodpha(1))
	  glist = plist(where(plist.pha ge goodpha(0) and plist.pha le $
	    goodpha(1)))
	endif else begin
	  printf, loglun, 'sigclean: Filtering pi from ' + strn(goodpha(0)) + $
            ' to ' + strn(goodpha(1))
	  glist = plist(where(plist.pi ge goodpha(0) and plist.pi le $
	    goodpha(1)))
	endelse

        if keyword_set(make_qdp) then make_qdp = 1 else make_qdp = 0
        if keyword_set(plt) then make_qdp = 1
        if n_elements(mint) eq 0 then mint=0

	if n_elements(bin) ne 0 then begin
	  printf, loglun, 'sigclean: Binning data using binsize = ' + $
            strn(bin) + ' sec.'
;	  timebin,glist,tbin,rate,sigrate,nsec,tbeg=gti(*,0),tend=gti(*,1),$
;	    bin=bin
          timbin, glist.time, gti, bin, tbin, nsec, rate, sigrate
          w = where(nsec gt mint)
;          sigrate = sqrt(rate(w))/nsec(w)
;	  rate = rate(w)/nsec(w)
          sigrate = sigrate(w)
          rate = rate(w)
	  nsec = nsec(w)
	  tbin = tbin(w)
	endif else begin
	  printf, loglun, 'sigclean: Using gtis as bins'
	  nsec = gti(*,1)-gti(*,0)
	  tbin = (gti(*,0)+gti(*,1))/2
	  gticnts,glist.time,gti,rate
	  sigrate = rate
	  w = where(nsec gt mint)
	  sigrate = sqrt(rate(w))/nsec(w)
	  rate = rate(w)/nsec(w)
	  nsec = nsec(w)
	  tbin = tbin(w)
	  bin = max(nsec)
	  mint = 0
	endelse
	if n_elements(mint) eq 0 then mint=0
	t0 = min(tbin)
	n = n_elements(rate)
	printf, loglun, 'sigclean: Initial net exposure: ',strn(total(nsec))
; set up grouping: grp(i,0) = starting bin, grp(i,1) = no. of bins
	grp = intarr(n,2)
	grp(*,0) = indgen(n)
	grp(*,1) = 1
	print,strn(n),' total bins'
	print,'Average bin size = ',strn(avg(nsec))
	print,'St. dev. of bin sizes = ',strn(stdev(nsec))
	tott = double(total(nsec))
	cnts = rate*nsec
	totcnts = float(total(rate*nsec))
; set up grouped counts and bin sizes
	gcnts = cnts
	gsec = nsec
	if n_elements(grpmin) ne 0 or mint gt 0 then begin
	  i = 0 & ng = 0
	  while i lt n do begin
	    gcnts(ng) = cnts(i)
	    gsec(ng) = nsec(i)
	    grp(ng,0) = i
	    grp(ng,1) = 1
	    if mint gt 0 then $
	      while i lt n-1 and gsec(ng) lt bin do begin
		i = i+1
		grp(ng,1) = grp(ng,1)+1
	        gcnts(ng) = gcnts(ng)+cnts(i)
		gsec(ng) = gsec(ng)+nsec(i)
	      endwhile else $  
	      while i lt n-1 and gcnts(ng) lt grpmin do begin
		i = i+1
		grp(ng,1) = grp(ng,1)+1
	        gcnts(ng) = gcnts(ng)+cnts(i)
		gsec(ng) = gsec(ng)+nsec(i)
	      endwhile
	    ng = ng+1
	    i = i+1
	  endwhile
	  printf, loglun, 'sigclean: ' + strn(ng) + ' bins after grouping'
	endif else ng = n
	gcnts = gcnts(0:ng-1)
	gsec = gsec(0:ng-1)
	grp = grp(0:ng-1,0:1)
	wl = where(gcnts lt 10,cl)
	if cl gt 0 then lowcnts = fix(gcnts(wl)) else lowcnts = 0
;	stop
	printf, loglun, 'sigclean: ' + strn(cl) + $
          ' bins have fewer than 10 cts, containing ' + $
	  strn(total(lowcnts)) + ' cnts or ' +  $
          string(total(lowcnts)/totcnts*100, '(f5.1)') + '% of total'
	printf, loglun, 'sigclean: ' + strn(gaussint(-sigma)*2*ng) + $
          ' bins should be outside of ',$
	  strn(sigma),' sigma by chance'
; Fudge low count errors using N. Gehrels' approx.
;	if cl gt 0 then sigrate(wl) = (sqrt(rate(wl)*nsec(wl)+0.75)+1)/nsec
; Compute group cnt rates and errors
        grate = gcnts*0.
        gsigrate = gcnts*0.
        w = where(gsec gt 0)
        grate(w) = gcnts(w)/gsec(w)
	gsigrate(w) = sqrt(gcnts(w))/gsec(w)
; Do chisq fit of constant using only bins with >=10 cts
	wh = where(gcnts ge 10,ch)
	mrate = total(grate(wh)/gsigrate(wh)^2)/total(1/gsigrate(wh)^2)
	chisq = total((grate(wh)-mrate)^2/gsigrate(wh)^2)
	printf, loglun, 'sigclean: Constant fit: ',strn(mrate),' cts/s'
	printf, loglun, 'sigclean: chisq(dof) = ',strn(chisq),'(',strn(ch-1),')','  chisq_nu = ',$
	  strn(chisq/(ch-1))
	if keyword_set(med) then begin
	  mrate = median(grate)
	  printf, loglun, 'sigclean: Using median cnt rate: ',strn(mrate)
	endif
	if keyword_set(make_qdp) then begin
;	  !p.multi(2) = 2
	  gtbin = gsec
	  gtsec = gsec
; Compute new bin midpoint, total on-time
	  for i=0,ng-1 do begin
	    v = tbin(grp(i,0):grp(i,0)+grp(i,1)-1)
	    gtbin(i) = (min(v)+max(v))/2
	    gtsec(i) = max(v)-min(v)+nsec(grp(i,0))/2+nsec(grp(i,0)+$
	      grp(i,1)-1)/2
	  endfor
	  gt0 = min(gtbin)
;	  plot_err2,gtbin-t0,grate,gtsec/2,gsigrate,psym=3,xr=[-bin/2,$
;	    max(gtbin)-gt0+bin],/xs,xtitle='Time (s)',ytitle='cts/s',$
;	    /nohat;,yr=minmax(grate)
;	  oplot,minmax(gtbin)-gt0,[mrate,mrate]
          if n_elements(qdpin) eq 0 then qdpin = 'temp1.qdp' 
	  openw,1,qdpin
	  printf,1,'read serr 1 2'
	  printf,1,'r y ',min(grate)*0.9,' ',max(grate)*1.1
	  for i=0,ng-1 do printf,1,gtbin(i)-t0,gtsec(i)/2,grate(i),gsigrate(i)
	  close,1
	  if keyword_set(plt) then spawn,'qdp ' + qdpin
;	  a = ''
;	  read,'Hit return to continue',a
	endif
; Gaussian points
	nsig = abs(mrate(wh)-grate(wh))/gsigrate(wh)
	w2 = where(abs(nsig) gt sigma,c2)
	good = indgen(ng)
	if c2 gt 0 then good(wh(w2)) = -1
; Low-count points
	if keyword_set(verb) then print,'Low-count points'
	for i=0,cl-1 do begin
; Compute expected number of counts in each bin and compare to observed
	  mcnts = mrate*gsec(wl(i))
	  if lowcnts(i) gt mcnts then prob = 1-poissum(lowcnts(i),mcnts) $
	    else prob = poissum(lowcnts(i)+1,mcnts)
	  if keyword_set(verb) then print,mcnts,lowcnts(i),prob
	  if prob lt gaussint(-sigma) then good(wl(i)) = -1
	endfor
	good = good(where(good ge 0,ngood))
	printf, loglun, 'sigclean: ' +strn(ng-ngood),' bins have cnts deviating by more than ',$
	  strn(sigma),' sigma'
; Compute new mean rate, chisq
	nw = good(where(gcnts(good) ge 10,gc))
	nmrate = total(grate(nw)/gsigrate(nw)^2)/total(1/gsigrate(nw)^2)
	nchisq = total((grate(nw)-nmrate)^2/gsigrate(nw)^2)
	printf,loglun, 'sigclean: New constant fit: ',strn(nmrate),' cts/s'
	printf,loglun, 'sigclean: New chisq(dof) = ',strn(nchisq),'(',strn(gc-1),')',$
	  '  chisq_nu = ',strn(nchisq/(gc-1))
	printf,loglun, 'sigclean: Significance of change to chisq = ',strn(chi_sqr1($
	  chisq-nchisq,ch-gc)*100),'%'
	if make_qdp then begin
	  
;	  plot_err2,gtbin(good)-gt0,grate(good),gtsec(good)/2,gsigrate(good),$
;	    psym=3,xr=[-bin/2,max(gtbin)-gt0+bin],/xs,xtitle='Time (s)',$
;	    ytitle='cts/s',/nohat;yr=minmax(grate)
;	  oplot,minmax(gtbin(good))-t0,[nmrate,nmrate]
;	  read,'Hit return to continue'
;	  !p.multi(2) = 0
          if n_elements(qdpout) eq 0 then qdpout = 'temp2.qdp' 
	  openw,1,qdpout
	  printf,1,'read serr 1 2'
	  printf,1,'r y ',min(grate(good))*0.9,' ',max(grate(good))*1.1
	  for i=0,ngood-1 do printf,1,gtbin(good(i))-gt0,gtsec(good(i))/2,$
	    grate(good(i)),gsigrate(good(i))
	  close,1
	  if keyword_set(plt) then spawn,'qdp ' + qdpout
	endif
	ngoodbins = total(grp(good,1))
	clgti = dblarr(ngoodbins,2)
	k = 0
	for i=0,ngood-1 do begin
	  for j=0,grp(good(i),1)-1 do begin
	    x= grp(good(i),0)+j
	    clgti(k,0) = tbin(x)-nsec(x)/2
	    clgti(k,1) = tbin(x)+nsec(x)/2
	    k = k+1
	  endfor
	endfor
        orgti, clgti, temp
        clgti = temp
	printf,loglun, 'sigclean: Final net exposure: ',strn(total(clgti(*,1)-clgti(*,0)))
	if k ne ngoodbins then stop
	if n_elements(fits) gt 0 then write_gtifits,fits,clgti
	return
	end
