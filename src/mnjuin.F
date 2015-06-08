C********************************************************************
C*$Id: mnjuin.F,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
C* --------------------
C*  Subroutine MNJUIN
C* --------------------
C*(Function)
C*  User initialization routine.  Usually, do histogram initialization.
C*(Input/Output)
C*  None
C*(Author)
C*  A. Miyamoto  21-September 1993.
C*
C********************************************************************
C*
      SUBROUTINE MNJUIN
C
      IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE "mnjbasi.inc"
      INCLUDE "mnjprmi.inc"
      INCLUDE "mnjevti.inc"
C
C ... Definition of BASES HISTOGRAM Buffer.
C
      INTEGER*4 IBUF
      PARAMETER (NHIST = 50, NSCAT = 10 )
      COMMON /PLOTB/ IBUF( 281*NHIST + 2527*NSCAT + 281 )
C
C=====< Entry Point >===================================================
C

C -------------------------------
C Parameters for BASES integration.
C -------------------------------
      NCALL=300000
      ITMX1=100
      ITMX2=100
      ACC1=0.3
      ACC2=0.2

      EBEAM=29.0
      PTMIN=2.4
      PTMIN=3.0
      PTMAX=8.0
      YMAX1=0.7
      YMAX2=0.7
C For Xband 500 GeV parameter
      NDISTR=0
      NUMFLV=5
      NCALL=3000000
      EBEAM=250.0
      NGMINS=26
      NGPLUS=26
      PTMIN=1.6
      YMAX3=200.0
      YMAX4=200.0
      PTMAX=EBEAM

c***********************************************************
C Get job parameters from a file
c***********************************************************

      call getparameters(5)

C -----------------------------------------
C   Initializa BASES Histogram buffer.
C -----------------------------------------

      CALL BHINIT( NHIST, NSCAT )

C
C -----------------------------------------
C     Histogram definition.
C -----------------------------------------
C

      CALL XHINIT(1,  0.0D0 , XU(1), 50,'X1  ')
      CALL XHINIT(2,  0.0D0 , XU(2), 50,'X2  ')
      CALL XHINIT(3,  0.0D0 , XU(3), 50,'X3  ')
      CALL XHINIT(4,  0.0D0 , XU(4), 50,'X4  ')
      CALL XHINIT(5,  0.0D0 , XU(5), 50,'X5  ')
      CALL XHINIT(6,  0.0D0 , XU(6), 50,'X6  ')

      XPTMIN=1.6D0
      XPTMAX=6.6D0
      CALL XHINIT(11, XPTMIN, XPTMAX, 50,'Pt')
      CALL XHINIT(13, XPTMIN, XPTMAX, 50,'Pt(for Direct)')
      CALL XHINIT(14, XPTMIN, XPTMAX, 50,'Pt(for 1 resolved)')
      CALL XHINIT(15, XPTMIN, XPTMAX, 50,'Pt(for two resolved)')
      CALL XHINIT(16, 0.D0, 20.D0,  20,'S of each ds/do type@')
C
      RETURN
      END
C********************************************************************
C*
C* -------------------------
C*  Subroutine MNJUFL(WEIGHT)
C* -------------------------
C*(Function)
C*  User routine for the histogram accumulation.  This routine is
C*  called in every call to the integrand function, FNCMNJ.
C*(Input)
C*  WEIGHT :  Function value at this phase space.
C*(Output)
C*  None
C*(Author)
C*  A. Miyamoto  21-September 1993.
C*
C********************************************************************
C*
      SUBROUTINE MNJUFL(WEIGHT)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "mnjevti.inc"
C
C ======================================================================
C
C ----------------------------------------------------------------------
C (1) Histograming
C ----------------------------------------------------------------------
C
      CALL XHFILL(1,XG(1), WEIGHT )
      CALL XHFILL(2,XG(2), WEIGHT)
      CALL XHFILL(3,XG(3), WEIGHT)
      CALL XHFILL(4,XG(4), WEIGHT)
      CALL XHFILL(5,XG(5), WEIGHT)
      CALL XHFILL(6,XG(6), WEIGHT)
      PT     = PCM*SQRT( (1.D0-CS)*(1.D0+CS) )
      CALL XHFILL(11,PT , WEIGHT )
C
      IAPRC = IABS(NPRC)
      IF( IAPRC.EQ.1 ) THEN
        CALL XHFILL(13,PT,WEIGHT)
      ELSEIF( IAPRC.EQ.2.OR.IAPRC.EQ.3 ) THEN
        CALL XHFILL(14,PT,WEIGHT)
      ELSE
        CALL XHFILL(15,PT,WEIGHT)
      ENDIF
C
      CALL XHFILL( 16, DFLOAT(IAPRC), WEIGHT )
      CALL XHFILL( 17, Y3LAB, WEIGHT )
      CALL XHFILL( 17, Y4LAB, WEIGHT )

      RETURN
      END
