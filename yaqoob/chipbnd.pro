pro chipbnd,instr,chip,x1,x2,y1,y2
;get chip boundaries
if instr eq 0 and chip le 0 then sq = 1
if instr eq 0 and chip eq 1 then sq = 2
if instr eq 0 and chip eq 2 then sq = 4
if instr eq 0 and chip ge 3 then sq = 3
if instr gt 0 and chip le 0 then sq = 4
if instr gt 0 and chip eq 1 then sq = 3
if instr gt 0 and chip eq 2 then sq = 1
if instr gt 0 and chip ge 3 then sq = 2
if sq eq 1 then begin
x1=1 & x2=418 & y1=423 & y2=842
endif
if sq eq 2 then begin
x1=441 & x2=858 & y1=423 & y2=842
endif
if sq eq 3 then begin
x1=2 & x2=419 & y1=1 & y2=420
endif
if sq eq 4 then begin 
x1=443 & x2=860 & y1=1 & y2=420
endif
return
end
