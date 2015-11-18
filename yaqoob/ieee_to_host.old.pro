pro ieee_to_host, data, IDLTYPE = idltype
;+
; NAME:
;    IEEE_TO_HOST
; PURPOSE:
;    To translate an IDL variable in IEEE-754 representation (as used, for
;    example, in FITS data ), into the host machine architecture.
;
; CALLING SEQUENCE:
;    IEEE_TO_HOST, data, [ IDLTYPE = , ]
;
; INPUT-OUTPUT PARAMETERS:
;     data - any IDL variable, scalar or vector.   It will be modified by
;            IEEE_TO_HOST to convert from IEEE to host representation.  Byte 
;            and string variables are returned by IEEE_TO_HOST unchanged
;
; OPTIONAL KEYWORD INPUTS:
;     IDLTYPE - scalar integer (1-7) specifying the IDL datatype according
;             to the code given by the SIZE function.     This keyword
;             is usually when DATA is a byte array to be interpreted as
;             another datatype (e.g. FLOAT).
;
; EXAMPLE:
;     A 2880 byte array (named FITARR) from a FITS record is to be 
;     interpreted as floating and converted to the host representaton:
;
;      IDL> IEEE_TO_HOST, bytarr, IDLTYPE = 4     
;
; METHOD:
;     The BYTEORDER procedure is called with the appropiate keyword
;
; RESTRICTION:
;     Will run *much* faster for floating or double precision if the IDL version
;     is since 2.2.2 when the /XDRTOF keyword  became available to BYTEORDER.
;     However, IEEE_TO_HOST should still work in earlier versions of IDL
;     Note that as of V3.0.0 the /XDRTOD keyword was not working properly
;     on big endian machines (e.g. DecStation) so IEEE_TO_HOST has a 
;     workaround
;
; MODIFICATION HISTORY:
;     Written, W. Landsman   Hughes/STX   May, 1992
;     Fixed error Case statement for float and double   September 1992
;     Workaround to /XDRTOD problem on DecStations January 1993 
;-
 On_error,2 

 if N_params() EQ 0 then begin
    print,'Syntax - IEEE_TO_HOST, data, [ IDLTYPE = ]
    return
 endif  

 npts = N_elements( data )
 if npts EQ 0 then $
     message,'ERROR - IDL data variable (first parameter) not defined'

 sz = size(data)
 if not keyword_set( idltype) then idltype = sz( sz(0)+1)

 case idltype of

      1: return                             ;byte

      2: byteorder, data, /NTOHS            ;integer

      3: byteorder, data, /NTOHL            ;long

      4: begin                              ;float
         if since_version('2.2.2') then byteorder, data, /XDRTOF $         
         else begin
            case !VERSION.OS of
            'vms': conv_unix_vax, data
            'mipsel': byteorder, data, /LSWAP  
            '386i': byteorder, data, /LSWAP 
             '386': byteorder, data, /LSWAP 
            else:
            endcase
         endelse
         end

      5: BEGIN                              ;double
       
            if !VERSION.OS eq 'vms' then begin

                if since_version('2.2.2') then $ 
                          byteorder, data, /XDRTOD  else $
                          conv_unix_vax, data

            endif else if (!VERSION.ARCH EQ 'mipsel') or $
                          (!VERSION.ARCH EQ '386i') or $
                          (!VERSION.OS EQ 'windows') or $
                          (!VERSION.ARCH EQ '386') then begin 

                    dtype = sz( sz(0) + 1)
                    if ( dtype EQ 5 ) then data = byte(data, 0, npts*8) $
                                      else npts = npts/8
                    data = reform( data, 8 , npts ,/OVER)
                    data = rotate( data, 5)
                    if ( dtype EQ 5 ) then data = double(data, 0, npts)
                    data = reform( data, sz(1:sz(0)), /OVER )

             endif
         end
     
      6: BEGIN                              ;complex
           fdata = float(data)
           byteorder, fdata, /XDRTOF
           idata = imaginary( temporary(data) )
           byteorder, idata, /XDRTOF
           data = complex( fdata, idata )
         END

      7: return                             ;string

       8: BEGIN				    ;structure

	Ntag = N_tags( data )

	for t=0,Ntag-1 do  begin
          temp = data.(t)
          ieee_to_host, temp
          data.(t) = temp
        endfor 

       END
 ENDCASE


return
end 
