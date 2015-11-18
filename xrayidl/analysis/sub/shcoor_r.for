	program shcoor
	character*40 a*40,aa*11,filen,text1*60,text2*60
	open(10,file='list_obs',status='old')
	open(29,file='plot_f',status='new')
	type*,'give ddla_limit'
	accept*,ddla_limit
	nfile=0
25	READ(10,511,END=100) text1,time,IH,IM,SEC,IDG,IMD,SECD,text2
c	write(*,511) text1,IH,IM,SEC,IDG,IMD,SECD,text2
511	format(a52,f6.2,2x,2i3,1x,f6.2,2x,i3,1x,i3,2x,f6.2,a40) !list_obs.txt (rosat)
c
	if(time.ne.0.and.time.lt.5.) goto 25
	call radian (IH,IM,SEC,IDG,IMD,SECD,xf,yf)
	CALL GETGC(Xf,Yf,DDLO,DDLA)
	if(abs(ddla).gt.ddla_limit) goto 25
	nfile=nfile+1
	write (29,513) text1,time,IH,IM,SEC,IDG,IMD,SECD,ddlo,ddla,text2
	write(*,513) text1,time,IH,IM,SEC,IDG,IMD,SECD,ddlo,ddla,text2
513	format(a52,f6.2,2x,2i3,f6.2,i3,i3,f6.2,2f5.1,a33) !list_obs.txt (rosat)
	goto 25
100	continue
	type*,'total file, expt=',nfile
	stop
	end
c-------------------------------------------------------------------------
c   The subroutine GETGC transfers the earth coordinates right accension
c and declination to the galatic coordinates logitude and latitude
c------------------------------------------------------------------------
	SUBROUTINE GETGC(ra,DEC,DDL,DDLA)
	PARAMETER (PI=3.1415927)
	C=COS(62.6*PI/180.)
	S=SIN(62.6*PI/180.)
	R=282.25*PI/180.
	SLAT=SIN(DEC)*C-COS(DEC)*SIN(ra-R)*S
	IF(SLAT.GE.1.) THEN
        DDLA=90.
	DDL=0.
	GOTO 90
	ENDIF
	IF(SLAT.LE.-1.) THEN
	DDLA=-90
	DDL=0.
	GOTO 90
	ENDIF
	RLAT=ASIN(SLAT)
	DDLA=RLAT*180./PI
	EC=COS(DEC)*COS(ra-R)/COS(RLAT)
	ES=(COS(DEC)*SIN(ra-R)*C+SIN(DEC)*S)/COS(RLAT)
	IF(ES.GE.1.) THEN
	DDL=90.
	GOTO 90
	ENDIF
	IF(EC.LE.-1.) THEN
	DDL=-90.
	GOTO 90
	ENDIF	
	DDL=acos(ec)*180./pi
	If(es.ge.0.) then
	ddl=ddl+33.
	else
	DDL=360.-DDL+33.
	endif
90	continue
	return
	end	
