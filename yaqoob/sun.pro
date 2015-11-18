function sun,d
; Gives approximate R.A. and Decl of the SUN on day of year 'd'
d=float(d)
ra=279.1+1.286*d-.007466*d^2+6.809e-5*d^3-2.828e-7*d^4 $
+5.332e-10*d^5-3.634e-13*d^6 & ra=ra mod 360.
dec=-23.14+.07562*d+.003538*d^2-3.547e-6*d^3-1.263e-7*d^4 $
+4.57e-10*d^5-4.421e-13*d^6
return,z=transpose([[ra],[dec]])
end

