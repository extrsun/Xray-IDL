C    SUBROUTINE H12 (MDE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV)
C    C. L. LAWSON AND R.J. HANSON, JET PROPULSION LABORATORY, 1973 JUN 12
C    TO APPEAR IN 'SOLVING LEAST SQUARES PROBLEMS'.
C
C    CONSTRUCTION AND/OR APPLICATION OF A SINGLE HOUSEHOLDER
C    TRANSFORMATION.  Q=I+U*(U**T)/B
C
C    MDE= 1 OR 2  TO SELECT ALGORITHM H1 OR H2
C    LPIVOT IS THE INDEX OF THE PIVOT ELEMENT
C    L1, M IF L1.LE.M THE TRANSFORMATION WILL BE CONSTRUCTED TO 
C          ZERO ELEMENTS INDEXED FROM L1 THRU M.  IF L1.GT.M
C          THE SUBROUTINE DOES AN IDENTITY TRANSFORMATION.
C    U(), IUE, UP  ON ENTRY TO H1 U() CONTAINS THE PIVOT VECTOR.
C                  IUE IS THE STORAAGE INCREMENT BETWEEN ELEMENTS.
C                  ON EXIT FROM H1 U() AND UP CONTAIN QUANTITIES
C                  DEFINING THE VECTOR U OF THE HOUSEHOLDER
C                  TRANSFORMATION.  ON ENTRY TO H2 U() AND UP
C                  SHOULD CONTAIN QUANTITIES PREVIOUSLY COMPUTED
C                  BY H1.  THESE WILL NOT BE MODIFIED BY H2.
C    C()   ON ENTRY TO H1 OR H2 C() CONTAINS A MATRIX WHICH WILL BE
C          REGARDED AS A SET OF VECTORS TO WHICH THE HOUSEHOLDER
C          TRANSFORMATION IS TO BE APPLIED. ON EXIT C() CONTAINS
C          THE SET OF TRANSFORMED VECTORS.
C    ICE()  STORAGE INCREMENT BETWEEN ELEMENTS OF VECTORS IN C().
C    ICV    STORAGE INCREMENT BETWEEN VECTORS IN C().
C    NCV    NUMBER OF VECTORS IN C() TO BE TRANSFORMED.  IF NCV.LE.0
C           NO OPERATIONS WILL BE DONE ON C().
C
      SUBROUTINE H12 (MDE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV)
      DIMENSION U(IUE,M), C(1)
      DOUBLE PRECISION SM, B
      ONE=1.
C
      IF (0.GE.LPIVOT.OR.LPIVOT.GE.L1.OR.L1.GT.M) RETURN
      CL=ABS(U(1,LPIVOT))
      IF (MDE.EQ.2) GO TO 60
C
C  *******************CONSTRUCT THE TRANSFORMATION****************
C
         DO 10 J=L1,M
 10      CL=AMAX1(ABS(U(1,J)),CL)
      IF (CL) 130,130,20
 20   CLINV=ONE/CL
      SM=(DBLE(U(1,LPIVOT))*CLINV)**2
         DO 30 J=L1,M
 30      SM=SM+(DBLE(U(1,J))*CLINV)**2
C
C      CONVERT  PREC SM TO SNGL PREC SM1
C
      SM1=SM
      CL=CL*SQRT(SM1)
      IF (U(1,LPIVOT)) 50,50,40
 40   CL=-CL
 50   UP=U(1,LPIVOT)-CL
      U(1,LPIVOT)=CL
      GO TO 70
C
C **************APPLY THE TRANSFORMATION I+U*(U**T)/B TO C***************
C
 60   IF (CL) 130,130,70
 70   IF (NCV.LE.0) RETURN
      B=DBLE(UP)*U(1,LPIVOT)
C  
C   B MUST BE NONPOSITIVE HERE. IF B=0. THEN RETURN.
C
      IF (B) 80,130,130
 80   B=ONE/B
      I2=1-ICV+ICE*(LPIVOT-1)
      INCR=ICE*(L1-LPIVOT)
          DO 120 J=1,NCV
          I2=I2+ICV
          I3=I2+INCR
          I4=I3
          SM=C(I2)*DBLE(UP)
                DO 90 I=L1,M
                SM=SM+C(I3)*DBLE(U(1,I))
 90             I3=I3+ICE
          IF (SM) 100,120,100
 100      SM=SM*B
          C(I2)=C(I2)+SM*DBLE(UP)
                DO 110 I=L1,M
                C(I4)=C(I4)+SM*DBLE(U(1,I))
 110            I4=I4+ICE
 120      CONTINUE
 130  RETURN
      END
