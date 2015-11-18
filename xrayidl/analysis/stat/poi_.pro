function POI_, Ncount, Mu

         if (Mu eq 0) then return, FLTARR(N_ELEMENTS(Ncount))

         lnP  = Ncount*ALOG(double(Mu))-Mu-LNGAMMA(Ncount+1)
         return, EXP(lnP)
end

