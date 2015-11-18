; $Id: chisquare.pro,v 1.0 2006/01/14
;
; Copyright (c) 2005-2006, Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.

function chisquare,array
;+
; NAME:
;       chisquare
; PURPOSE:
;       calculate the chi square value of an array.
; EXPLANATION:
;       This function could be used to n-dimensional array.
; CALLING SEQUENCE:
;       chisquarevalue=chisquare(array)
; INPUT PARAMETERS:
;       array              = the input array
; EXAMPLE:
;       x=[1,2,3,4,5]
;       chisquarevalue=chisquare(x)

meanvalue=mean(array)
chisquarevalue=sqrt(total((array-meanvalue)^2))/meanvalue
return,chisquarevalue
end
