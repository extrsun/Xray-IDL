pro hri_psf_off,E,ANG,ENC,RAD
; mistaken as the psf for the hri. wqd 9/30/97
; edited by jane t July 28th 1993
; dumps components of off-axis psf versus radius from centroid into qdp file
;C  calculates PSPC off-axis PSF (normalized surface brightness)
; Updated September 15th 1993 to improve scattering term
;
;P  ENE                 R   I   Energy [keV]
;P  EPS			R   I	off-axis angle [arc min]
;P  X                   R   I   angle from target position [arcsec]
;
;P  IERR                I   O   = 0 no error 
;P                              = 1 energy outside bounds 0.07-3 keV
;P                              = 3 
;
;     OUTPUT:
;             PSFOFF      surface brightness of point spread function,
;                         normalized, so that  Integral 2*PI*r*dr*f
;                         from 0 to infinity = 1 [1/arcsec2]
;
;   include_block_name          description
;   routines_called    type     description
;
;***********************************************************************
; ;
      CIRC=fltarr(61)
      psfa=fltarr(605)
        PI = 3.1415927
;
        IERR = 0
;
        IF((E GE 70.) AND (E LE 3000.)) THEN begin
            ENE = E/1000.
;
;  Gaussian sigma Detector
;
            SDETSQ = 108.7*ENE^(-0.888)+1.121*ENE^6
;
;  exponential e-folding angle
;
            RC = SQRT(50.61*ENE^(-1.472)+6.80*ENE^5.62)
;
;  scattering Lorentzian break angles
;
            BREAK1 = 39.95/ENE
            BREAK2 = 861.9/ENE
;
;  scattering Lorentzian slope
;
           ALPHA2 = 2.119 + 0.212*ENE
;
            for IEPS = 0,60 do begin
                EPS = IEPS
                TOT = 0.
                X = 0
                A2 = 10^(-1.635+0.639*ENE+0.052*ENE*ENE)* $
                           EXP(-0.5*(EPS/12.)^2)
;
;  scattering fraction
;
                A3 = 0.075*ENE^1.43
;
;  avoid "exponential artefact"       
;
                A2 = A2 < (1.0-A3)
;
;  Gaussian fraction (total integral under the PSF is 1)
;
                A1 = 1.0 - A2 - A3
                for J=1,601 do begin
;
;  Gaussian sigma Mirror
;
                    STELSQ = 0.219*EPS^2.848
                    SIGMA = SQRT(SDETSQ+STELSQ)
;
;  normalization by integrals 0-infinity
;
                    FNOR1 = A1/(2.*PI*SIGMA*SIGMA)
                    FNOR2 = A2/(2.*PI*RC*RC)
                    AUX = 1. + BREAK2*BREAK2/BREAK1/BREAK1
                    B3 = BREAK2*BREAK2/(BREAK1*BREAK1)
                    FNOR3 = A3/(PI*(aLOG10(AUX)+(2*B3)/(AUX*(ALPHA2-2.))))
;
;  calculate function
;
                    ARG1 = 0.5*(X/SIGMA)^2
                    IF(ARG1 GE 75.) then ARG1 = 75.
                    ARG2 = X/RC
                    IF(ARG2 GE 75.) then ARG2 = 75.
                    IF(X LE BREAK2) THEN begin
                        PSF1 = FNOR1*EXP(-ARG1)
                        PSF2 = FNOR2*EXP(-ARG2)
                        PSF3 = FNOR3/(BREAK1*BREAK1+X*X)
                        PSFOFF = PSF1 + PSF2 + PSF3
                    endif ELSE begin
                        PSF1 = FNOR1*EXP(-ARG1)
                        PSF2 = FNOR2*EXP(-ARG2)
                        PSF3 = FNOR3/(BREAK1*BREAK1+BREAK2*BREAK2)* $
                               (X/BREAK2)^(-ALPHA2)
                        PSFOFF = PSF1 + PSF2 + PSF3
                    ENDelse
                    SDET = SQRT(SDETSQ)
                    STEL = SQRT(STELSQ)
                    IF(J EQ 1) THEN begin
                        PSFA(J) = 1.E6*PSFOFF*(0.5*0.5)
                    endif ELSE begin
                        PSFA(J) = 1.E6*PSFOFF*((J-0.5)*(J-0.5) - $       
                   (J-1.5)*(J-1.5))
                    ENDelse
                    TOT = TOT + PSFA(J)
                    X = X + 5.
                ENDfor
                SUM = 0.
                J = 0
                TOT = ENC*TOT
                WHILE (SUM LT TOT) do begin
                    J = J + 1
                    SUM = SUM + PSFA(J)
                ENDwhile
                CIRC(IEPS) = (J-1.)/12.
;                PRINT *, IEPS, CIRC(IEPS)
            ENDfor
;            IC = 1
        ENDIF
        IA = NINT(ANG) + 1
        IF(IA GT 61) then IA = 61
        RAD = CIRC(IA)
;
stop
        RETURN
        END     
