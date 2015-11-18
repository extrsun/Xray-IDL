;==========================================================================
; INDEX_TO_POINT PROCEDURE
; Patrick Broos, 1993
;
; Converts a 1-D index into a 2-D array, e.g. the indexes a "where" command
; returns, to the 2-D coordinates [X,Y] of the array element.
; The parameter "specs: should be the output of the size command, e.g.
;   index_to_point, where(image GT 0), x, y, size(image)
;==========================================================================
PRO index_to_point, index, X, Y, specs

Ncol = specs(1) & Nrow = specs(2)

Y = fix( index / Ncol )
X = Index - (Y * Ncol)
return
end

