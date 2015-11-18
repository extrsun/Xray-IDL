pro tes,xx
xx=[-999]
i=3
while n_elements(xx) le 98 do begin
 c=where(i mod (indgen(i-2)+2) eq 0, nc)
if nc eq 0 then xx=[xx,i]
i=i+1
endwhile
xx=[1,2,xx(1:*)]
print,xx
return
end