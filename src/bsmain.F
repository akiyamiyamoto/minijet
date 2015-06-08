C***********************************************************************
C      MAIN PROGRAM FOR BASES
C
C    To calculate total cross section for
C      e+e- --> mini-jet.
C$Id: bsmain.F,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
C***********************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
      EXTERNAL FNCMNJ
      REAL*8 estim, sgima, ctime
      INTEGER*4  it1, it2
      REAL*4 STIME
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF
      include 'mnjfili.inc'

      lu=6


************************************************************************
*               initialization of BASES/SPRING 5.1
************************************************************************
*=======================================================================
*          initialization of bases by calling bsinit
*=======================================================================
*         -------------
           call bsinit
*         -------------
*=======================================================================
*      initialization of parameters
*          for kinematics and matrix elements
*      initialization of histograms
*=======================================================================

*         ------------------
           call userin
*         ------------------
************************************************************************
*              numerical integration by BASES 5.1
************************************************************************

      call bases( FNCMNJ, estim, sigma, ctime, it1, it2 )

      call bhplot( lu )

      print *,'Bases terminated : Cross section=',
     >  estim,' +- ',sigma,' pb'

c      call bsinfo( lu )
c      if(LOOP.LT.NLOOP) goto 1
c      close( lu )

      if( basesresult(1:4).ne."NONE") then
        print *,'basesresult=',basesresult(1:lnblnk(basesresult))
        lun=23
        open(lun,file=basesresult(1:lnblnk(basesresult)),
     >     status='unknown',form='unformatted')
        call bswrit( lun )

c .. save BSRSLT at the end
        write (lun) AVGI,SD,CHI2A,STIME,ITG,ITF
        close ( lun )
      endif

      stop
      end
