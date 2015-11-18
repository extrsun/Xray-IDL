pro calcsigma, filename, tfilename

image=mrdfits(filename,0,fitshead)
timage=mrdfits(tfilename,0,tfitshead)

tnumber=n_elements(where(timage[*,*] gt 0))
totalvalue=total(image[*,*])
meanvalue=totalvalue/float(tnumber)

sigma=sqrt(total(image[*,*]-meanvalue)^2/tnumber)

print,'sigma = ',sigma

end

