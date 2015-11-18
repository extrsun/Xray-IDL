

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






