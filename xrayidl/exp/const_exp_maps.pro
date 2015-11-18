From jam@astro.psu.edu Fri Aug 28 12:51:53 1992
Received: by vela.Colorado.EDU.colorado.edu (cu.generic.890828)
Received: from astrod (astrod.astro.psu.edu) by nexus.astro.psu.edu (4.1/Nexus-1.3)
	id AA14501; Fri, 28 Aug 92 14:50:50 EDT
Received: by astrod (4.1/Client-1.3)
	id AA11960; Fri, 28 Aug 92 14:50:38 EDT
Date: Fri, 28 Aug 92 14:50:38 EDT
From: "Jeff Mendenhall" <jam@astro.psu.edu>
Message-Id: <9208281850.AA11960@astrod>
To: wqd@vela.Colorado.EDU
Subject: Re:  exposure maps
Cc: jam@astrod.astro.psu.edu
Status: RO

Hi Qingde,

i am forwarding the lastest version of the 'const_exp_maps.pro' routine with the 
correction i mentioned earlier applied. i am also forwarding the fortran version of 
the same code. i have not worked with the fortran version lately but it should be
ok. you can test it by generating exposure maps in both idl and fortran and comparing them.

the inputs of the fortran version are the same as that of hte idl version. however, the 
fortran version asks for eventrate and livtime file inputs. these are the ascii outputs 
generated by the const_exp_data.pro routine i have already sent to you. 

finally, i am sending you the pi=90:131 bin exposure map. we figured out the problem with it
earlier this week and it seems to generate a nice map. i will ftp this to your account. 

Q. do you have the ascii instrument maps used as input to the fortran code? these are the 
counterparts to the idl fits file instrument maps?  if not i will send them to you. 

i would also like to warn you about using the exposure maps you are currently generating. they
are based on instrument maps generated from the pspc used for the all sky survey. therefore,
they are appropriate for AO1 observations but may not be ideal for AO2+ observations due to
the switching over to the second pspc after pointing at the sun. Steve Snowden is
currently working on new instrument maps for the current pspc. until then, the exp maps you have
are better than the original exp map, but not ideal. we hope to distribute the new maps soon.

please keep me informed of you progress.

Cheers,

jeff

p.s.	if you have over 7 obis in you observation, you may have trouble with the 
	const_exp_data.pro routine. to resolve this problem, simply gointo the initialization
	section of const_exp_Data.pro (l. 22) and add the appropriate number of obis for
	your observation.


;
;
;	PROGRAM	: CONST_EXP_MAPS
;	AUTHOR	: STEVE SNOWDEN MPE
;	DATE	: APRIL, 1992
;
;	** THIS PROGRAM IS AN IDL TRANSLATION OF THE ORIGINAL 
;	VAX FORTRAN PROGRAM CREATED BY STEVE SNOWDEN OF 
;	MPE. **
;		- jeffrey a. mendenhall
;
;	THIS PROGRAM GENERATES ENERGY DEPENDENT EXPOSURE MAPS 
;	FOR ROSAT POINTED OBSERVATIONS. INITIALLY, ENERGY
;	DEPENDENT INSTRUMENT MAPS ARE READ AS WELL AS INFORMATION
;	RELATED TO THE PARTICULAR POINTED OBSERVATION IN 
;	QUESTION. LIVE TIME FRACTIONS ARE THEN CALCULATED BASED
;	ON EVENT RATES. FINALLY, THE TOTAL LIVE TIME EXPOSURE
;	IS FOLDED ACROSS THE INSTRUMENT MAP ACCORDING TO THE
;	VARIOUS ASPECT SOLUTIONS CORRESPONDING TO THE OBSERVATION.
;	
;	INPUTS:	
;                       1. *.CAS FILENAME (ASPECT INFO)
;                       2. *.EVR FILENAME (EVENT RATE INFO)
;                       3. NUMBER OF OBI's
;                       4. ENERGY BAND SELECTION
;                       5. GOOD TIMES FILENAME
;       OUTPUTS:
;                       1. ENERGY DEPENDENT EXPOSURE MAP
;                       2. ASPECT DATA FILE
;                       3. EVENTRATE DATA FILE
;                       4. LIVETIME DATA FILE
;

PRO	CONST_EXP_MAPS

;	Set up arrays

	MAP   =FLTARR(512,512)
	RMAP  =FLTARR(512,512)
	ASPECT=FLTARR(51,51,1501)
	TEMPAR=FLTARR(512,512)
	EXPARR=FLTARR(512,512)
	INSTMP=STRARR(7)
        SCALE =DBLARR(7)

;	Initialization
	
	DEADTP=234.
        INFILE=''
	ACTIME=''
	EXPMAP=''
	REFILE=''

;	Instrument map information

        INSTMP(0)='/bulk1/burrows/expmaps/exposure_8_19.fits'
        INSTMP(1)='/bulk1/burrows/expmaps/exposure_20_41.fits'
        INSTMP(2)='/bulk1/burrows/expmaps/exposure_42_51.fits'
        INSTMP(3)='/bulk1/burrows/expmaps/exposure_52_69.fits'
        INSTMP(4)='/bulk1/burrows/expmaps/exposure_70_90.fits'
        INSTMP(5)='/bulk1/burrows/expmaps/exposure_91_131.fits'
        INSTMP(6)='/bulk1/burrows/expmaps/exposure_132_201.fits' 
        SCALE(0)=1.450173E-4
	SCALE(1)=4.841082E-5 
	SCALE(2)=6.500746E-5 
	SCALE(3)=6.1726414E-5
	SCALE(4)=6.04025954E-5 
	SCALE(5)=6.1157329E-5 
	SCALE(6)=6.1680003E-5


;	Get intruductory info

;	Get reference exposure maps so that the header information may be used 
;	when creating the new, energy dependent map.

	PRINT,'Enter name of reference exposure map /w fits extension: '
	READ,REFILE

;	Open file containing times the user has identified as valid and 
;	uncontaminated. These may be the original obi intervals or subsets
;	thereof. See intro documentation for format of this file.
	
	PRINT,'Enter filename containing accepted time intervals:'
        READ,ACTIME

        OPENR,92, ACTIME 
        PRINT,'ACTIME filesize:'
        READ,SACT
        ACTDATA=FLTARR(2,SACT)
        READF,92,ACTDATA	

;	Generate Attitude and Livtime data sets

	PRINT,'Creating input data sets...'
	CONST_EXP_DATA,ATTDATA,LIVDATA

	S=SIZE(ATTDATA)
	SATT=S(2)
	
	S=SIZE(LIVDATA)
	SLIV=S(2)


; 	Read a sample of the roll angles and take an average, the processing
; 	will offset from this angle

	DTEMP=0
	DTEMP=TOTAL(ATTDATA(4,0:S(2)))
	DTEMP=DTEMP/S(2)
	THETA=DTEMP/7200.
	THETOF=DTEMP


; 	Start the loop over the attitude file

;       Initialize looping parameters

        IOS =0
        NG=0
        NB1=0
        NB2=0
        NB3=0
        NB4=0
        ISCSO=1.0
        IACTBE=1.0
        IACTEN=1.0
        ILIVEN=1.0
        ILIVEN=1.0
        IAEXE=1.0
        IAXE=1.0
        IA1LL=1.0
        NUM=0
        TOTIME=0
        TOTEXP=0
        DONE=0
        rec=0

	
	PRINT,'Looping over attitude file to find good times...'
	
	I=0
	I=LONG(I)
	J=0
	J=LONG(J)
	K=0
	K=LONG(K)
        WHILE (I LT SATT AND DONE EQ 0) DO BEGIN
            ISCS = ATTDATA(1,I)
	    IX   = ATTDATA(2,I)
            IY   = ATTDATA(3,I)
	    IROLL= ATTDATA(4,I)
	    IY   = -IY
	    DELT = ISCS - ISCSO
            ISCSO= ISCS

; 	Process the attitude step, first check the accepted time file	
; 	to see if the attitude step is in an accepted time period

                WHILE (J LT SACT AND ISCS GT IACTEN) DO BEGIN
		    IACTBE=ACTDATA(0,J)
		    IACTEN=ACTDATA(1,J)
		    J=J+1
                END
		IF J EQ SACT THEN DONE=1

                IF((ISCS GT IACTBE) AND (DONE EQ 0)) THEN BEGIN
                    NUM = NUM + 1
; 	Accepted time, now find the live time

                    WHILE (K LT SLIV-2 AND ISCS GT ILIVEN) DO BEGIN
			ILIVEN=LIVDATA(1,K)
                        AEXE  =LIVDATA(2,K)
                        AXE   =LIVDATA(3,K)
			A1LL  =LIVDATA(4,K)
			K=K+1
                    END
		    IF K EQ SLIV THEN DONE=1

; 	Calculate the live time fraction

                    LIVTIM,A1LL,DEADTP,AXE,AEXE,FLIVE1,FLIVE2,FLIVE

; 	The attitude steps are on 1-second intervals, calculate the exposure

                    EXP = DELT*FLIVE
                    TOTIME = TOTIME + DELT
                    TOTEXP = TOTEXP + EXP

;  	Set the X, Y, and ROLL values for the aspect array.  Steps are 
;  	in units of 14.94733 arc seconds (historical reasons).

                    IF(A1LL GT 30.) THEN BEGIN
                        X = 26. + FLOAT(IX)/29.894656
                        IX = FIX(X)
                        IF((IX GE 1) AND (IX LE 51)) THEN BEGIN
                            Y = 26. + FLOAT(IY)/29.894656
                            IY = FIX(Y)
                            IF((IY GE 1) AND (IY LE 51)) THEN BEGIN
                                ROLL = 600. + (FLOAT(IROLL)-THETOF)/149.4733 
                                IR = FIX(ROLL)
				print,IR
                                IF((IR GE 1) AND (IR LE 1500)) THEN BEGIN

; 	Add to the aspect array

                                    ASPECT(IX,IY,IR) =  ASPECT(IX,IY,IR) + EXP
                                    NG = NG + 1
                                END ELSE NB4 = NB4 + 1
                            END ELSE NB3 = NB3 + 1
                        END ELSE  NB2 = NB2 + 1
                    END ELSE NB1 = NB1 + 1
                END
            I=I+1
        END
        PRINT, 'GOOD STEPS =', NG
        PRINT, 'BAD STEPS = ', NB1, NB2, NB3, NB4
        PRINT, NUM, TOTIME, TOTEXP


;	Loop through and create exposure maps

	ANSWER='Y'
        YESNO,ANSWER
 
        WHILE ANSWER EQ 1 DO BEGIN
        ANSWER=''

        PRINT, 'Enter name of output exposure map /w fits extension:'
        READ,EXPMAP
        PRINT,''

;       Select and read instrument map
        
        PRINT, 'Enter input band'
        PRINT, '1 => CH 8-19'
        PRINT, '2 => CH 20-41'
        PRINT, '3 => CH 42-51'
        PRINT, '4 => CH 52-69'
        PRINT, '5 => CH 70-90'
        PRINT, '6 => CH 91-131'
        PRINT, '7 => CH 132-201'
        READ, IB
        PRINT,''
 
        PRINT, 'READING INSTRUMENT MAP...'
        MAP=READFITS(INSTMP(IB-1),H,/NOSCALE)
        
 
;       Center the instrument map, invert the Y-axis, and turn it real

        PRINT,'Centering instrument map...'
	I=INDGEN(463)
	I=I+24
	II=512-I	
       	RMAP(*,II)=SCALE(IB-1)*MAP(*,I-12)
	

;	Now cast the exposure

	PRINT,'Casting exposure...'
	EXPARR(*)=0
        NB1 = 0
        FOR IR=0,1500 DO BEGIN
            PRINT, IR
            NG = 0
            FOR IX=0,50 DO BEGIN
                FOR IY=0,50 DO BEGIN
                    IF(ASPECT(IX,IY,IR) GT 0) THEN BEGIN
                        IF(NG EQ 0) THEN BEGIN

; 	First nonzero aspect point with this roll angle, make a rotated map

                            NG = 1
                            ANGLE  = THETA + (IR-600.)*0.02076017
			    ANGLE  = 3.1415927*ANGLE/180.
                            COSROL = COS(ANGLE)
                            SINROL = SIN(ANGLE)

; 	Zero the temp array
	
			  TEMPAR(*)=0

; 	Calculate the rotated array
	
;	Make this part more efficient
			   
			   FOR I=24,487 DO BEGIN
                                FOR II=24,487 DO BEGIN
                                    IF(RMAP(I,II) NE 0.00000) THEN BEGIN
                                        X = (I - 256.)
                                        Y = (II - 256.)
                                        XX = COSROL*X + SINROL*Y
                                        YY = COSROL*Y - SINROL*X
                                        IXX = FIX(XX + 256.5)
                                        IYY = FIX(YY + 256.5)
                          TEMPAR(IXX,IYY) = TEMPAR(IXX,IYY)+RMAP(I,II)
                                    END
                                ENDFOR
                            ENDFOR

                        END

; 	Nonzero element, cast it
	
		TEMP=ASPECT(IX,IY,IR)*TEMPAR
		TEMP=SHIFT(TEMP,IX-26,IY-26)
		EXPARR=EXPARR+TEMP

		END
	    ENDFOR
        ENDFOR
	ENDFOR

;	WRITE EXPOSURE MAP TO FITS FILE.


;	GET OUTPUT INFO AND OPEN OUTPUT FILES.

	PRINT,'Transferring file to fits format...'
	PRINT,''
        PRINT,'Opening reference exposure map to get header info...'
 	DATA=READFITS(REFILE,REFHEAD,/NOSCALE)

;
;       Write data to fits fomat.
;
 
        PRINT,''
        PRINT,'Writing idl array back into fits format: ',EXPMAP
	SXADDPAR,REFHEAD,'BSCALE',1.0
	SXADDPAR,REFHEAD,'BZERO',0.0
	WRITEFITS,EXPMAP,EXPARR,REFHEAD
	
	PRINT,'Do you wish to create another exposure map? ;
	READ,ANSWER
	YESNO,ANSWER

	END
        
        print,'Normal Termination.'
        close,92
        
        END








C
C
C	PROGRAM : CONST_EXP_MAPS.F
C	AUTHOR  : STEVE SNOWDEN MPE
C       DATE    : APRIL, 1992
C
C      ** THIS PROGRAM IS A SUN FORTRAN TRANSLATION OF THE ORIGINAL 
C      VAX FORTRAN PROGRAM CREATED BY STEVE SNOWDEN OF
C      MPE. **
C              - jeffrey a. mendenhall
C
C      THIS PROGRAM GENERATES ENERGY DEPENDENT EXPOSURE MAPS
C      FOR ROSAT POINTED OBSERVATIONS. INITIALLY, ENERGY
C      DEPENDENT INSTRUMENT MAPS ARE READ AS WELL AS INFORMATION
C      RELATED TO THE PARTICULAR POINTED OBSERVATION IN
C      QUESTION. LIVE TIME FRACTIONS ARE THEN CALCULATED BASED
C      ON EVENT RATES. FINALLY, THE TOTAL LIVE TIME EXPOSURE
C      IS FOLDED ACROSS THE INSTRUMENT MAP ACCORDING TO THE
C      VARIOUS ASPECT SOLUTIONS CORRESPONDING TO THE OBSERVATION.
C
C	INPUTS:
C			1. ATTITUDE FILENAME (ASPECT INFO)
C			2. LIVTIME FILENAME  (EVENT RATE INFO)
C			3. ACTIME FILENAME   (GOOD TIME INTERVALS)
C			4. ENERGY BAND SELECTION
C
C	OUTPUTS:
C			1. ENERGY DEPENDENT EXPOSURE MAPS


C	Initializations

	INTEGER K,N,M
        INTEGER*2 MAP(512,512),I1,I2,I3,I4,I5,I6,I7,I8

        INTEGER*4 IA1LL, IAEXE, IAXE, I, J, IB, 
     +      IERR, II, ILIVEN, IOS, IR, IROLL, ISCSO, 
     +      ITEMP, IX, IXX, IY, IYY, NB1, NB2, NB3, 
     +      NB4, NG, NUM

        REAL*4 A1LL, AEXE, ANGLE, ASPECT(51,51,1501), AXE, 
     +      COSROL, DEADTP, DELT, EXP, EXPARR(512,512), FLIVE, 
     +      FLIVE1, FLIVE2, RMAP(512,512), ROLL, SCALE(7), 
     +      SINROL, TEMPAR(512,512), THETA, THETOF, 
     +      TOTEXP, TOTIME, X, XX, Y, YY

        REAL*8 DSCS, DTEMP, ISCS, IACTBE, IACTEN
	

        CHARACTER*80 INFILE, INSTMP(7), EXPMAP

        DATA INSTMP /
     +      '/bulk1/burrows/expmaps/MOD_DET_8_19.ASC',
     +      '/bulk1/burrows/expmaps/MOD_DET_20_41.ASC',
     +      '/bulk1/burrows/expmaps/MOD_DET_42_51.ASC',
     +      '/bulk1/burrows/expmaps/MOD_DET_52_69.ASC',
     +      '/bulk1/burrows/expmaps/MOD_DET_70_90.ASC',
     +      '/bulk1/burrows/expmaps/MOD_DET_91_131.ASC',
     +      '/bulk1/burrows/expmaps/MOD_DET_132_201.ASC'/
        
	DATA SCALE /1.450173E-4, 4.841082E-5, 6.500746E-5, 
     +      6.1726414E-5, 6.04025954E-5, 6.1157329E-5, 6.1680003E-5/

        DATA DEADTP /234./


C	Get output file info

	PRINT *,''
	PRINT *, 'Enter name of output exposure map ascii file:'
	READ  *, EXPMAP
	OPEN(UNIT=94,FILE=EXPMAP)

C	 Open the aspect, eventrate, and good times files


 1000   FORMAT(A80)

        PRINT *, 'Enter the aspect file name'
        READ 1000, INFILE
        OPEN(UNIT=90,FILE=INFILE)

	PRINT *, 'Enter the eventrate file name'
        READ 1000, INFILE
        OPEN(UNIT=91,FILE=INFILE)

	PRINT *, 'Enter the good times file file name'
        READ 1000, INFILE
        OPEN(UNIT=92,FILE=INFILE)

	
C  	Read a sample of the roll angles and take an average, the processing
C  	will offset from this angle

        DTEMP = 0.
        DO I=1,1000
            READ(90,*,IOSTAT=IOS) ITEMP, DSCS, IX, IY, IROLL
            DTEMP = DTEMP + IROLL
        ENDDO
        DTEMP = DTEMP/1000
        THETA = DTEMP/7200.
        THETOF = DTEMP
        REWIND(90)


C  	Start the loop over the attitude file

	PRINT *,''
	PRINT *,'Looping over attitude file to find good times...'
        IOS = 0
        DO WHILE (IOS .EQ. 0)
            READ(90,*,IOSTAT=IOS) ITEMP, DSCS, IX, IY, IROLL
            ISCS = DSCS
            DELT = ISCS - ISCSO
            ISCSO = ISCS
            IY = -IY
            IF(IOS .EQ. 0) THEN

C  	Process the attitude step, first check the accepted time file
C  	to see if the attitude step is in an accepted time period

                DO WHILE ((ISCS .GT. IACTEN) .AND. (IOS .EQ. 0))
                    READ(92,*,IOSTAT=IOS) IACTBE, IACTEN
                ENDDO

                IF((ISCS .GT. IACTBE) .AND. (IOS .EQ. 0)) THEN
                    NUM = NUM + 1
                    IF(MOD(NUM,1000) .EQ. 0) PRINT *, NUM

C  	Accepted time, now find the live time

                    DO WHILE ((ISCS .GT. ILIVEN) .AND. (IOS .EQ. 0))
                        READ(91,*,IOSTAT=IOS) 
     +                          ITEMP, ILIVEN, IAEXE, IAXE, IA1LL
                        A1LL = IA1LL
                        AEXE = IAEXE
                        AXE = IAXE
                    ENDDO

C  	Calculate the live time fraction

                   CALL LIVTIM(A1LL,DEADTP,AXE,AEXE,FLIVE1,
     +                      FLIVE2,FLIVE,IERR)

C  	The attitude steps are on 1-second intervals, calculate the exposure

                    EXP = DELT*FLIVE
                    TOTIME = TOTIME + DELT
                    TOTEXP = TOTEXP + EXP

C  	Set the X, Y, and ROLL values for the aspect array.  Steps are 
C  	in units of 14.94733 arc seconds (historical reasons).

                    IF(A1LL .GT. 30.) THEN 
                        X = 26. + FLOAT(IX)/29.894656
                        IX = INT(X)
                        IF((IX .GE. 1) .AND. (IX .LE. 51)) THEN
                            Y = 26. + FLOAT(IY)/29.894656
                            IY = INT(Y)
                            IF((IY .GE. 1) .AND. (IY .LE. 51)) THEN
                                ROLL = 600. + 
     +                                  (FLOAT(IROLL)-THETOF)/149.4733
                                IR = INT(ROLL)
                                IF((IR .GE. 1) .AND. (IR .LE. 1500)) THEN

C  	Add to the aspect array

                                    ASPECT(IX,IY,IR) = 
     +                                  ASPECT(IX,IY,IR) + EXP
                                    NG = NG + 1
                                ELSE
                                    NB4 = NB4 + 1
                                ENDIF
                            ELSE
                                NB3 = NB3 + 1
                            ENDIF
                        ELSE
                            NB2 = NB2 + 1
                        ENDIF
                    ELSE
                        NB1 = NB1 + 1
                    ENDIF
                ENDIF
            ENDIF
        ENDDO
        PRINT *, 'GOOD STEPS =', NG
        PRINT *, 'BAD STEPS = ', NB1, NB2, NB3, NB4
        PRINT *, NUM, TOTIME, TOTEXP	
	PRINT *,''
	PRINT *,''

	ANSWER=1
	
	DO 100 WHILE (ANSWER .EQ. 1)

C  	Read in the instrument map

        PRINT *, 'Enter input band'
        PRINT *, '1 => CH 8-19'
        PRINT *, '2 => CH 20-41'
        PRINT *, '3 => CH 42-51'
        PRINT *, '4 => CH 52-69'
        PRINT *, '5 => CH 70-90'
        PRINT *, '6 => CH 91-131'
        PRINT *, '7 => CH 132-201'
        READ *, IB
        OPEN(UNIT=90,FILE=INSTMP(IB))
 
C       Read instrument map.
 
	PRINT *,''
        WRITE(*,*) 'READING INSTRUMENT MAP...'
        DO I=1,512
                K=1
                DO J=1,64
                        READ(90,*) I1,I2,I3,I4,I5,I6,I7,I8
                        MAP(K,I)=I1
                        MAP(K+1,I)=I2
                        MAP(K+2,I)=I3
                        MAP(K+3,I)=I4
                        MAP(K+4,I)=I5
                        MAP(K+5,I)=I6
                        MAP(K+6,I)=I7
                        MAP(K+7,I)=I8
                K=K+8
                ENDDO
        ENDDO
        CLOSE(90)

C  	Center the instrument map, invert the Y-axis, and turn it real

	PRINT *,''
	PRINT *,'Centering/Inverting map...'
        DO I=25,488
            DO II=25,488
                RMAP(I,514-II) = SCALE(IB)*MAP(I,II-12)
            ENDDO
        ENDDO



C  	Now cast the exposure

        NB1 = 0
        DO IR=1,1500
            PRINT *, IR
            NG = 0
            DO IX=1,51
                DO IY=1,51
                    IF(ASPECT(IX,IY,IR) .GT. 0.) THEN
                        IF(NG .EQ. 0) THEN

C  	First nonzero aspect point with this roll angle, make a rotated map

                            NG = 1
                            ANGLE = THETA + (IR-600.)*0.02076017
                            COSROL = COSD(ANGLE)
                            SINROL = SIND(ANGLE)

C  	Zero the temp array

                            DO I=25,488
                                DO II=25,488
                                    TEMPAR(I,II) = 0.
                                ENDDO
                            ENDDO

C   	Calculate the rotated array

                            DO I=25,488
                                DO II=25,488
                                    IF(RMAP(I,II) .NE. 0.) THEN
                                        X = (I - 257.)
                                        Y = (II - 257.)
                                        XX = COSROL*X + SINROL*Y
                                        YY = COSROL*Y - SINROL*X
                                        IXX = INT(XX + 257.5)
                                        IYY = INT(YY + 257.5)
                                        TEMPAR(IXX,IYY) = 
     +                                      TEMPAR(IXX,IYY) + RMAP(I,II)
                                    ENDIF
                                ENDDO
                            ENDDO
                        ENDIF

C  	Nonzero element, cast it

                        DO I=25,488
                            IXX = I + IX - 26
                            DO II=25,488
                                IF(TEMPAR(I,II) .NE. 0.) THEN
                                    IYY = II + IY - 26
                                    EXPARR(IXX,IYY) = EXPARR(IXX,IYY) +
     +                                  ASPECT(IX,IY,IR)*TEMPAR(I,II)
                                ENDIF
                            ENDDO
                        ENDDO
                    ENDIF
                ENDDO
            ENDDO
        ENDDO
	
	
	

C	WRITE EXPOSURE MAP OUTPUT FILE.
	
	PRINT *,''
	PRINT *,'Writing exposure map to 8 X 32768 ascii file:',EXPMAP
	do n=1,512
	  do m=1,64
	    write(94,*) EXPARR(n,(m-1)*8+1),EXPARR(n,(m-1)*8+2),
     +      EXPARR(n,(m-1)*8+3),EXPARR(n,(m-1)*8+4),EXPARR(n,(m-1)*8+5),
     +	    EXPARR(n,(m-1)*8+6),EXPARR(n,(m-1)*8+7),EXPARR(n,(m-1)*8+8)
	  enddo
	  m=1
	enddo

	PRINT *,''
	PRINT *,'Do you wish to create another exposure map?'
	PRINT *,'		1=YES'
	PRINT *,'		2=NO'

	READ(*,*) ANSWER
	
 100	CONTINUE
	
	PRINT *,''
	PRINT *,'Closing files: Normal Termination'
        STOP
        END






