
rosatlib
low=[7,9,11,14,17,20,24,28,32,37,42,47,52,58,64,70,77,84,91,99,107,115]
low=[low,123,132,141,150,160,170,180,191,202,213,224,236]
hi=[8,10,13,16,19,23,27,31,36,41,46,51,57,63,69,76,83,90,98,106,114]
hi=[hi,122,131,140,149,159,169,179,190,201,212,223,235,247]
!group(0,0)=low
!group(0,1)=hi
low=[.07,.09,.11,.14,.17,.20,.24,.28,.32,.37,.42,.47,.52,.58,.64]
low=[low,.70,.77,.84,.91,.99,1.07,1.15,1.23,1.32,1.41,1.5,1.6,1.7]
low=[low,1.8,1.91,2.02,2.13,2.24,2.36]
hi=[.09,.11,.14,.17,.20,.24,.28,.32,.37,.42,.47,.52,.58,.64,.70,.77]
hi=[hi,.84,.91,.99,1.07,1.15,1.23,1.32,1.41,1.5,1.6,1.7,1.8,1.91,2.02]
hi=[hi,2.13,2.24,2.36,2.48]
!ebnds(0,0)=low
!ebnds(0,1)=hi
delvar,low,hi
;
if (!d.name eq 'TEK') then device,/tek4100
print,'  '
print,' Welcome to IDL'
print,'   '
print,' Use doc_library to get info about a particular procedure'
print,'   '
print,' Use doc_list to get lists of some useful procedures, with descriptions'
print,'   '
print,' To see how to run a procedure, type the name of the procedure with'
print,'     no arguments, or type help,/routines'
print,'   '
print,'   '
print,' For general news as of June 5, 1992, see ZDOC:NEWS.TXT' 
print,'   '
print,' For a history of changes/additions, see ZDOC:CHANGES.TXT'
print,'   '
.run ~yaqoob/ascaidl/ieee_to_host
.run ~yaqoob/ascaidl/host_to_ieee
