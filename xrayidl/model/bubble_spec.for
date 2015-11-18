c----------------------------------------------------------------------
	program bubble_spec
c----------------------------------------------------------------------
c plot the spectrum in detail
c------------------------------------------------------------------------
	real integ
	external integ
	parameter n_spec=512
	CHARACTER*80 DEVICE,TEXT,TYPE,TITLE,xlab,ylab,label*2
	INTEGER PGBEGIN
c
	real h
	external h
	real spec(n_spec),aspec(n_spec)
	real spec_c(n_spec),aspec_c(n_spec)
c
	common/energies/ enspec(512)
c
	common/self/spec
c--------------------------------------------------------
c-----------------------------------------------------
	t=3.e6
	hyd=3.e21
	call getspec(t)
c
	call abstab(hyd,spec,aspec)
	do i=1,n_spec
	spec(i)=spec(i)*enspec(i)
	aspec(i)=aspec(i)*enspec(i)
	enddo
c	call spec_plot(spec,aspec)
	flux_broad_C=integ(spec,0.16,3.5)
c
	hyd=3.e21
	tb=3.*1.e6
	taumax=0.1e6/tb !1x10**5K is selected as the lowest temperature in the integration.
c
	do k=1,n_spec
	spec_c(k)=0.
	aspec_c(k)=0.
	enddo
c
	hmax=h(taumax**2.5)
	type*,'hmax==',hmax
	x=0.001
	dm=0.005
222	xd=min((1.-x)**0.4/(x*x)*hmax*dm,0.01)
	xd=min(xd,0.5*(1.-x))
	xi=x+0.5*xd
	t=tb*(1-xi)**0.4
	if(taumax.gt.t/tb) goto 333
	x=x+xd
c
	call getspec(t)
c
	weight=3.*xi*xi*xd/(1.-xi)**0.8
	do k=1,n_spec
	spec_c(k)=spec_c(k)+spec(k)*weight
	enddo
	goto 222
333	continue
c-------------------------------------------------------
	flux_broad_C=integ(spec_c,0.16,3.5)
	flux_tot_c=integ(spec_c,0.05,6.)
	type*,'f_broad,f_t==',flux_broad_c,flux_tot_c
	call abstab(hyd,spec_c,aspec_c)
	flux_broad=integ(aspec_c,0.16,3.5)
	flux_tot=integ(aspec_c,0.05,6.)
	type*,'f_broad,f_t==',flux_broad,flux_tot
	ratio=flux_broad/flux_tot
	type*,'ratio of broad to total after ab=',ratio
	ratio=flux_broad_c/flux_tot_c
	type*,'ratio of broad to total before ab.=',ratio
	ratio=flux_broad/flux_tot_c
	type*,'flux_broad/flux_tot_c=',ratio
c
	do i=1,n_spec
	spec(i)=spec(i)*enspec(i)
	aspec(i)=aspec(i)*enspec(i)
	enddo
	call spec_plot(spec_c,aspec_c)
c
	stop
	end
c--------------------------------------------------------------
c-------------------------------------------------------------
	subroutine spec_plot(spec,aspec)
c----------------------------------------------------------------------
c plot the spectrum 
c
	parameter n_spec=512
	real xx(n_spec),yy(n_spec),ayy(n_spec)
	real x1(n_spec),x2(n_spec)
	CHARACTER*60 DEVICE,TEXT,TYPE,TITLE,xlab,ylab,label*2,texta(10)*30
	character*30 namet
	INTEGER PGBEGIN
	real bin1(n_spec),bin2(n_spec),spec(n_spec),aspec(n_spec)
c
	common/param/dele,delhalf
	common/energies/ enspec(512)
c
	do j=1,n_spec
	enspec(j)=0.03+0.02*j !0.05-10.25
	enddo
	do k=1,n_spec-1
c	type*,'bin1,bin2,spec==',bin1(k),bin2(k),spec(k)
	x1(k)=log10(enspec(k))
	x2(k)=log10(enspec(k+1))
	xx(k)=log10((enspec(k)+enspec(k+1))/2.)
	yy(k)=log10(spec(k)+0.000000001)
	if(yy(k).gt.ymax) ymax=yy(k)
	if(yy(k).lt.ymin) ymin=yy(k)
	ayy(k)=log10(aspec(k)+0.000000001)
c	type*,xx(k),yy(k)
	enddo
c
	xl=x1(1)
	xu=log10(6.) !x2(n_spec-1)
	yl=ymin
	yu=4. !ymax*1.2
c
  10	WRITE(6,'(A)') '$Graphics device/type: '
	read(*,'(a)') device
	IF (PGBEGIN(0,DEVICE,1,1).NE.1) GOTO 10
	type*,'xl,xu,yl,yu',xl,xu,yl,yu
	if(device.eq.'app.dat/apple') then
	call grsetlw(3)
	endif
	title=' '
	xlab='Energy (kev)'!
	ylab='\fiI\d\gn\fn\u (10\u-23\d erg cm\u3\ds\u-1\dkeV\u-1\d)'
	call pgenv(xl,xu,yl,yu,0,30)
	call pglabel(xlab,ylab,title)
	call grsetlw(1)
	call myhist(n_spec-1,x1,x2,yy)
	call grsetlw(3)
	call myhist(n_spec-1,x1,x2,ayy)
c
	call pgend
	return
	end
c--------------------------------------------------------------
c----------------------------------------------------------
	subroutine myhist(mk,x1,x2,expy)
	real x1(*),x2(*),xx(1100),yy(1100),expy(*)
	xx(1)=x1(1)
	yy(1)=-10.
	m=2
	do 30 n=1,mk
	xx(m)=x1(n)
	xx(m+1)=x2(n)
	yy(m)=expy(n)
	yy(m+1)=expy(n)
	m=m+2
30	continue
	xx(m)=x2(mk)
	yy(m)=-10.
	call pgline(m,xx,yy)
	return
	end
c------------------------------------------------------------
c------------------------------------------------------------
	function h(w)
	real h
	h=125./156.-5./13.*w**(13./5.)+5./4.*w**(8./5.)-5./3.*w**(3./5.)
	return
	end
c--------------------------------------------------------
c-------------------------------------------------------
	subroutine getspec(temp)
	character*40 filen
	parameter n_spec=512
	real bin1(n_spec),bin2(n_spec),spec(n_spec)
	common/self/spec
c
	common/param/dele,delhalf
	common/energies/ enspec(512)
c---------------------------------------------------------------------
	dele=0.02
	delhalf=dele/2.
	do j=1,n_spec
	enspec(j)=0.03+0.02*j !0.05-10.25
	enddo
c	filen='[wqd.fin]line21'
	filen='[wqd.fin]lin050'
c	type*,'spectrum file ',filen,' is used'
	ispc=19
	call open(ispc,filen,'blk','old')
	call raymond(temp,ispc,bin1,bin2,spec)
	close(ispc)
	return
	end
