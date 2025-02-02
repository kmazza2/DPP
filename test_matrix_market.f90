program main
use :: matrix_market
integer nnzmax
integer iunit,ounit
character rep*10
character field*7
character symm*19
character ifile*32,ofile*32
parameter (nnzmax=100000)
integer ival(nnzmax)
double precision rval(nnzmax)
complex cval(nnzmax)
integer indx(nnzmax)
integer jndx(nnzmax)
iunit = 8
open(unit=iunit,file='test_real_mat.mtx')
print *,'Reading header only...'
call mminfo(iunit,rep,field,symm,nrows,ncols,nnz)
print *,'  Matrix is type: ',rep,' ',field,' ',symm
print *,'  Matrix size: ',nrows,' by ',ncols,' with ', nnz,' nonzeros.'
print *,'Reading header and data...'
call mmread(iunit,rep,field,symm,nrows,ncols,nnz,nnzmax, indx,jndx,ival,rval,cval)
print *,'  Matrix is type: ',rep,' ',field,' ',symm
print *,'  Matrix size: ',nrows,' by ',ncols,' with ', nnz,' nonzeros.'
if( rep .eq. 'array' ) then
  print *,'  Dense array.'
  print *,'  First two entries:'
  if ( field .eq. 'integer' ) then
     print *,(ival(i),i=1,2)
  elseif ( field .eq. 'real') then
     print *,(rval(i),i=1,2)
  elseif ( field .eq. 'complex' ) then
     print *,(cval(i),i=1,2)
  endif
else
  print *,'  Sparse (coordinate) array.'
  print *,'  First two entries:'
  if ( field .eq. 'integer' ) then
    print *,indx(1),jndx(1),ival(1)
    print *,indx(2),jndx(2),ival(2)
  elseif ( field .eq. 'real') then
    print *,indx(1),jndx(1),rval(1)
    print *,indx(2),jndx(2),rval(2)
  elseif ( field .eq. 'complex' ) then
    print *,indx(1),jndx(1),cval(1)
    print *,indx(2),jndx(2),cval(2)
  elseif ( field .eq. 'pattern' ) then
    print *,indx(1),jndx(1)
    print *,indx(2),jndx(2)
  endif
endif
stop
end
