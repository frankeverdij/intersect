!*****************************************************************
!*  Sorts a sparse matrix with rows IA, colums IB and            *
!*  elements RA, all of length N, in ascending order, rows first *
!*  then columns by the Heapsort method                          * 
!* ------------------------------------------------------------- *
!* INPUTS:                                                       *
!*	    N	  size of tables IA, IB and RA                   *
!*          IA	  rowindex to be sorted                          *
!*          IB	  columnindex to be sorted                       *
!*          RA	  matrix element                                 *
!* OUTPUT:                                                       *
!*	    IA    rowindex table sorted in ascending order       *
!*	    IB    columnindex table sorted in ascending order    *
!*	    RA    matrix element table sorted                    *
!*                                                               *
!* NOTE: The Heapsort method is a N Log2 N routine,              *
!*       and can be used for very large arrays.                  *
!*                                                               *
!* ORIGINAL CODE: Numerical Recipes (C) Press et al.             *
!* Modified for sparse matrices by Frank Everdij 19/02/2008      *
!*****************************************************************

SUBROUTINE HEAPSORT2(IA,IB,IC,N)

  implicit none
  integer IA(N), IB(N), IC(N), N
  
  integer IIA, IIB, IIC, I, J, L, IR
  
  L=N/2+1
  IR=N
  !The index L will be decremented from its initial value during the
  !"hiring" (heap creation) phase. Once it reaches 1, the index IR 
  !will be decremented from its initial value down to 1 during the
  !"retirement-and-promotion" (heap selection) phase.
10 continue
  if(L > 1)then
    L=L-1
    IIA=IA(L)
    IIB=IB(L)
    IIC=IC(L)
  else
    IIA=IA(IR)
    IIB=IB(IR)
    IIC=IC(IR)
    IA(IR)=IA(1)
    IB(IR)=IB(1)
    IC(IR)=IC(1)
    IR=IR-1
    if(IR.eq.1)then
      IA(1)=IIA
      IB(1)=IIB
      IC(1)=IIC
      return
    end if
  end if
  I=L
  J=L+L
20 if(J.le.IR)then
  if(J < IR)then
    if((IA(J) < IA(J+1)) .or. ((IA(J) .eq. IA(J+1)) .and. (IB(J) < IB(J+1))))  J=J+1
  end if
  if((IIA < IA(J)) .or. ((IIA .eq. IA(J)) .and. (IIB < IB(J)))) then
    IA(I)=IA(J)
    IB(I)=IB(J)
    IC(I)=IC(J)
    I=J; J=J+J
  else
    J=IR+1
  end if
  goto 20
  end if
  IA(I)=IIA
  IB(I)=IIB
  IC(I)=IIC
  goto 10
END


SUBROUTINE HEAPSORT3(RA,RB,N)

  implicit none
  integer N
  real(8) RA(N), RB(N)
  
  integer I, J, L, IR
  real(8) RRA, RRB
  
  L=N/2+1
  IR=N
  !The index L will be decremented from its initial value during the
  !"hiring" (heap creation) phase. Once it reaches 1, the index IR 
  !will be decremented from its initial value down to 1 during the
  !"retirement-and-promotion" (heap selection) phase.
10 continue
  if(L > 1)then
    L=L-1
    RRA=RA(L)
    RRB=RB(L)
  else
    RRA=RA(IR)
    RRB=RB(IR)
    RA(IR)=RA(1)
    RB(IR)=RB(1)
    IR=IR-1
    if(IR.eq.1)then
      RA(1)=RRA
      RB(1)=RRB
      return
    end if
  end if
  I=L
  J=L+L
20 if(J.le.IR)then
  if(J < IR)then
    if((RA(J) < RA(J+1)) .or. ((RA(J) == RA(J+1)) .and. (RB(J) < RB(J+1))))  J=J+1
  end if
  if((RRA < RA(J)) .or. ((RRA == RA(J)) .and. (RRB < RB(J)))) then
    RA(I)=RA(J)
    RB(I)=RB(J)
    I=J; J=J+J
  else
    J=IR+1
  end if
  goto 20
  end if
  RA(I)=RRA
  RB(I)=RRB
  goto 10
END
