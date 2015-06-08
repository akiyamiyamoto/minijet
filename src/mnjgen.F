C$Id: mnjgen.F,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
C
C  Main program for the event generation of
C
C      e e --> mini jet event.(q-q,q-qbar,q-g,g-g events are generated)
C
C
C234567890---------2---------3---------4---------5---------6---------7--
C
C USERIN
C
      SUBROUTINE USERIN
C
      IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE "mnjbasi.inc"
      INCLUDE "mnjevti.inc"
      INCLUDE "mnjprmi.inc"

      DATA IFIRST/1/
C
C=====< Entry Point >===================================================
C
C -------------------------------
C Description of the variables.
C -------------------------------
C
C  X(1) ; X1 for the photon energy emitted from e-
C  X(2) ; X2 for the photon energy emitted from e+
C  X(3) ; X3 for the parton energy of X1 photon
C  X(4) ; X4 for the parton energy of X2 photon
C  X(5) ; Cos(th) in sub-process rest frame.
C
      DO 100 I = 1, 6
        XL(I) = 0.D0
        XU(I) = 1.D0
100   CONTINUE
C
      NDIM  =      6
      NWILD =      NDIM

      print *,'Userin was called... NDIM=6'
C
C -------------------------------
C Initialize User parameter and Histogram.
C -------------------------------
C
      CALL MNJUIN

      XLAM2  = XLAM*XLAM
      XXLAM2 = XXLAM*XXLAM
      RS      = 2.*EBEAM
C -------------------------------
C Print Job parameters.
C -------------------------------
      IF( IFIRST .EQ. 1 ) THEN
        CALL MNJTIT
        IFIRST = 0
      ENDIF
C
      RETURN
      END
C
C
C234567890---------2---------3---------4---------5---------6---------7--
C
C FUNCTION CROSS
C
C234567890---------2---------3---------4---------5---------6---------7--
C
      REAL FUNCTION FNCMNJ*8(X)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  X(25)
C
      INCLUDE "mnjevti.inc"
      INCLUDE "mnjtypi.inc"
      INCLUDE "mnjprmi.inc"
C
      PARAMETER ( GEV2PB=0.38927D+9,  ALPHA=1.D0/137.0359895D0,
     >            PI=3.14159265D0,    TWOPI=2.D0*PI,
     >            AME=0.51099906D-3,  AME2=AME*AME)
      DATA    IFIRST/1/
      SAVE    IFIRST
      EXTERNAL MNJBLK
C
C
C ======================================================================
C
C ----------------------------------------------------------------------
C (1) Initialization at the very begining of the job.
C ----------------------------------------------------------------------
C
      IF( IFIRST .EQ. 1 ) THEN
        S      = (2.*EBEAM)**2

        XXMIN  = 0.0
        X1MIN  = XXMIN
        X1MAX  = 1.0D0 - 1.D-4
        X1WID  = X1MAX - X1MIN
        X2MIN  = XXMIN
        X2MAX  = 1.0D0 - 1.D-4
        X2WID  = X2MAX - X2MIN
        X3MIN  = XXMIN
        X3MAX  = 1.0D0 - 1.D-4
        X3WID  = X3MAX - X3MIN
        X4MIN  = XXMIN
        X4MAX  = 1.0D0 - 1.D-4
        X4WID  = X4MAX - X4MIN

        CSMAX  = 1.0
        CSMIN  = -CSMAX
        CSWID  =  CSMAX- CSMIN

        XLAM2  = XLAM*XLAM
        XXLAM2 = XXLAM*XXLAM
        STHR1 =  50.
        STHR2 = 500.
C
C ... Calculate Lamda^2 used in Alpha~s for 3 and 5 flavour.
C
        IF(NLAM.EQ.0) THEN
          XXLM03 = XXLAM
          XXLM05 = XXLAM
          XXLM23 = XXLAM2
          XXLM25 = XXLAM2
        ELSE
          ALPHAS = 12.*PI/((33.-2*4)*DLOG(STHR1/XXLAM2))
          XXLM23 = STHR1/DEXP( 12.D0*PI/(ALPHAS*27.D0) )
          ALPHAS = 12.*PI/((33.-2*4)*DLOG(STHR2/XXLAM2))
          XXLM25 = STHR2/DEXP( 12.D0*PI/(ALPHAS*23.D0) )
          XXLM03 = SQRT(XXLM23)
          XXLM05 = SQRT(XXLM25)
        ENDIF
        IFIRST = 0

      ENDIF
C
C ----------------------------------------------------------------------
C (2) Set Beam energy and flux factor.
C ----------------------------------------------------------------------
C
      X1    = X1MIN + X1WID*X(1)
      X2    = X2MIN + X2WID*X(2)
      X3    = X3MIN + X3WID*X(3)
      X4    = X4MIN + X4WID*X(4)

      FNCMNJ = 0
      DIRECT = 0
      ONERES = 0
      TWORES = 0

C-----------------------------------------------------------------
C (0) Decide the process
C
C-----------------------------------------------------------------
      XMULT = FLOAT(MAXPRC)
      INDX  = XMULT*X(6)+1.
      INDX  = MIN(MAX(1,INDX),MAXPRC)
C
      NPRC  = KCDATA(0,INDX)
      IAPRC = IABS(NPRC)
      IF( IAPRC.EQ.0 )                        RETURN
C
C ..    Select the type of event to generate.
C
      IF( NGNTYP .NE. 0 ) THEN
        IF( NGNTYP .EQ. 1 ) THEN
            IF( IAPRC.NE.2 .AND. IAPRC.NE.3 ) RETURN
        ELSEIF( NGNTYP.EQ.2 ) THEN
            IF( IAPRC.LE.3 )                  RETURN
        ELSEIF( NGNTYP.EQ.4 ) THEN
            IF( IAPRC.EQ.1 )                  RETURN
        ELSE
            IF( IAPRC.NE.NGNPRC )             RETURN
        ENDIF
      ENDIF

      SHAT  = X1*X2*S
      EGAM1 = X1*EBEAM
      EGAM2 = X2*EBEAM
      EGAM10= X1*EBEAM
      EGAM20= X2*EBEAM
      IF( KCDATA(1,INDX).NE.22) THEN
        SHAT  = SHAT*X3
        EGAM1 = X1*X3*EBEAM
      ENDIF
      IF( KCDATA(2,INDX).NE.22) THEN
        SHAT  = SHAT*X4
        EGAM2 = X2*X4*EBEAM
      ENDIF

      CS    = CSMIN + CSWID*X(5)
      PCM   = 0.5*SQRT(SHAT)
      PT    = PCM *SQRT((1.D0-CS)*(1.D0+CS))
C--
C ... Decide QSQ
C--
      IF( NQSSRC .EQ. 0 ) THEN
        QSQ   = SHAT*0.25
      ELSE
        QSQ   = PT*PT
      ENDIF
C----
C   Decide # of flavour according to the Q^2
C----
      IF( NUMFLV .EQ. 0 ) THEN
        NFLV =   3
        IF( QSQ  .GT. STHR1) NFLV = 4
        IF( QSQ  .GT. STHR2) NFLV = 5
      ELSE
        NFLV = NUMFLV
      ENDIF
C------
C .. Remove flavour production below threshold.
C-----
        IAD1 = IABS(KCDATA(1,INDX))
        IAD2 = IABS(KCDATA(2,INDX))
        IAD3 = IABS(KCDATA(3,INDX))
        IAD4 = IABS(KCDATA(4,INDX))
C--
C .. For massive quark case(NMASQ>=1) we assumes no c/b quark exist in
C    a photon.
C--
        IF(NMASQ.EQ.0                )    THEN
         IF( IAD1.GT.NFLV.AND.IAD1.LT.10) RETURN
         IF( IAD2.GT.NFLV.AND.IAD2.LT.10) RETURN
        ELSEIF(NMASQ.GE.1) THEN
         NFLV3 = 3
         IF( IAD1.GT.NFLV3.AND.IAD1.LT.10) RETURN
         IF( IAD2.GT.NFLV3.AND.IAD2.LT.10) RETURN
        ENDIF
        IF( IAD3.GT.NFLV.AND.IAD3  .LT.10) RETURN
        IF( IAD4.GT.NFLV.AND.IAD4  .LT.10) RETURN

C
C================================================================
C 1) Calculate Differential cross section of hard sub-process.
C===============================================================
C--
        CALL MNJCRS( KCDATA(0,INDX), SHAT, IAD1, IAD2, IAD3, IAD4,
     +                    CS, PTMIN,  NMASQ,   DSDCS ,PCM ,QSQ)
C- =============================================================
C--
C DSDCS is negative if set of  valiables is outside of kinematical
C       boundary.
C--
         IF(DSDCS.LE. 0.0)                     RETURN
C- =============================================================
C--
C Calculate PT once more, since PCM may be changed for massive
C   quark case.
C--
        PT    = PCM*SQRT((1.D0-CS)*(1.D0+CS))
        IF( PT.LT.PTMIN.OR.PT.GT.PTMAX ) RETURN
C--
C ... calculate the rapidity of jet to make a cut
C--
      BETAF  = (EGAM1-EGAM2)/(EGAM1+EGAM2)
      BETAFH  = 0.5D0*DLOG( (1.+BETAF)/(1.-BETAF) )
      PZCM    = PCM *CS
      IF(ABS(1.0 -CS).LT.1.0E-10) THEN
        PRINT *,' warning:      CS=1 '
      ENDIF
      Y3CM    = 0.5D0*DLOG( (1+CS)/(1-CS) )
      Y4CM    = -Y3CM
      Y3LAB   = Y3CM + BETAFH
      Y4LAB   = Y4CM + BETAFH
C YMAX4 <0   one-jet inclusive.
C FOR inclusive one-jet cross section, count twice for cetral two-jets.
       FONE = 1.0D0
      IF( YMAX4.LE.0.D0       ) THEN
        IF( ABS(Y3LAB).GT.YMAX3 .AND. ABS(Y4LAB).GT.YMAX3) RETURN
        IF( ABS(Y3LAB).LT.YMAX3 .AND. ABS(Y4LAB).LT.YMAX3) THEN
          FONE = NONEJT
        ENDIF
      ELSEIF( YMAX4.GT.0.D0       ) THEN
C YMAX4 >0   two-jet inclusive.
        IF( ABS(Y3LAB).GT.YMAX3 ) RETURN
        IF( ABS(Y4LAB).GT.YMAX4 ) RETURN
      ENDIF
C===================================================================
C 2)  Calculate structure function, when initial parton is quark or g
C===================================================================
      IF( IAD1.EQ.22 ) THEN
        F3 = 1.D0
      ELSE
        IF(NDISTR.EQ.0) THEN
          ITYP = 1
          IF( QSQ .GT. STHR1 ) ITYP = 2
          IF( QSQ .GT. STHR2 ) ITYP = 3
          CALL MNJPRB(ITYP, IAD1, QSQ, X3, XLAM2, F3 )
        ELSE
          CALL MNJPRB(NDISTR, IAD1, QSQ, X3, XLAM2, F3 )
        ENDIF
      ENDIF
C
      IF( IAD2.EQ.22 ) THEN
        F4 = 1.D0
      ELSE
        IF(NDISTR.EQ.0) THEN
          ITYP = 1
          IF( QSQ .GT. STHR1 ) ITYP = 2
          IF( QSQ .GT. STHR2 ) ITYP = 3
          CALL MNJPRB(ITYP, IAD2, QSQ, X4, XLAM2, F4 )
        ELSE
          CALL MNJPRB(NDISTR, IAD2, QSQ, X4, XLAM2, F4 )
        ENDIF
      ENDIF

C===========================================================
C ... Weight factor according to the quark charge
C===========================================================
        IF( NFLV.EQ.3 ) THEN
          ALPHAS = 12.*PI/((33.-2*NFLV)*DLOG(QSQ/XXLM23)*0.12)
        ELSEIF( NFLV.EQ.5 ) THEN
          ALPHAS = 12.*PI/((33.-2*NFLV)*DLOG(QSQ/XXLM25)*0.12)
        ELSE
          ALPHAS = 12.*PI/((33.-2*NFLV)*DLOG(QSQ/XXLAM2)*0.12)
        ENDIF
        IF( IAPRC.EQ.1 ) THEN
          IF( MOD(IAD3,2).EQ.1 ) THEN
            F5 = (1./3.)**4
          ELSE
            F5 = (2./3.)**4
          ENDIF
        ELSEIF( IAPRC.EQ.2 ) THEN
          IADT = IABS(KCDATA(1,INDX))
          IF( IADT .GT. 10 ) IADT = IABS(KCDATA(2,INDX))
          IF( MOD(IADT,2).EQ.1 ) THEN
            F5 = (1./3.)**2
          ELSE
            F5 = (2./3.)**2
          ENDIF
          F5 = F5*ALPHAS
        ELSEIF( IAPRC.EQ.3 ) THEN
          IF( MOD(IAD3,2).EQ.1 ) THEN
            F5 = (1./3.)**2
          ELSE
            F5 = (2./3.)**2
          ENDIF
          F5 = F5*ALPHAS
        ELSE
          F5 = ALPHAS*ALPHAS
        ENDIF

C===============================================================
C 3)  Get weight of the photon.
C================================================================
      IF( NGMINS.EQ.0 ) THEN
        CALL MNJGAM(1, X1, EBEAM, F1)
      ELSEIF ( NGMINS.EQ.2 ) THEN
        CALL MNJGAM(2, X1, QSQ, F1)
      ELSE
          CALL MNJGAM(NGMINS, X1, SQRT(QSQ), F1 )
      ENDIF
C
      IF( NGPLUS.EQ.0 ) THEN
        CALL MNJGAM(1, X2, EBEAM, F2)
      ELSEIF ( NGPLUS.EQ.2 ) THEN
        CALL MNJGAM(2, X2, SQRT(QSQ), F2)
      ELSE
        CALL MNJGAM(NGPLUS, X2, EBEAM, F2 )
      ENDIF

C===============================================================
C 4)  Obtain cross section.
C================================================================

        YACOB = X1WID*X2WID*CSWID*X3WID*X4WID*XMULT
        FNCMNJ= F1*F2*DSDCS*YACOB*F3*F4*F5*FONE

        IF( FNCMNJ.LT. 0.0D0 ) THEN
          PRINT *,' '
          PRINT *,' Error !! Negative Function value.',
     >            ' Check Input parameters'
          PRINT *,' FUNC =',FNCMNJ
          PRINT *,' X(1:3)=',X(1),X(2),X(3),X(4),X(5)
          PRINT *,' F1   =',F1
          PRINT *,' F2   =',F2
          PRINT *,' F3   =',F3
          PRINT *,' F4   =',F4
          PRINT *,' F5   =',F5
          PRINT *,' SHAT =',SHAT
          PRINT *,' DSDCS=',DSDCS
          PRINT *,' NPRC =',NPRC
          PRINT *,' INDX =',INDX,' KCDATA=',(KCDATA(I,INDX),I=0,4)
          PRINT *,' YACOB=',YACOB
          PRINT *,' PT   =',PT
          PRINT *,' X1   =',X1
          PRINT *,' X2   =',X2
          PRINT *,' X3   =',X3
          PRINT *,' X4   =',X4
        ENDIF


      XG(1)  = X(1)
      XG(2)  = X(2)
      XG(3)  = X(3)
      XG(4)  = X(4)
      XG(5)  = X(5)
      XG(6)  = X(6)
      XG(21) = X1
      XG(22) = X2
      XG(23) = X3
      XG(24) = X4
C
C ----------------------------------------------------------------------
C (8) Histograming
C ----------------------------------------------------------------------
C
      WEIGHT = FNCMNJ
      CALL MNJUFL( WEIGHT )

      RETURN
      END
C
C234567890---------2---------3---------4---------5---------6---------7--
C
C USROUT
C
C234567890---------2---------3---------4---------5---------6---------7--
C
      SUBROUTINE USROUT
C
      COMMON /LOOP0/ LOOPC
      REAL*8 AVG1, SD, CHI2A,CPU
      COMMON /BSRSLT/AVG1,SD,CHI2A,CPU,ITF
C
C
      WRITE(6,100) AVG1,SD
100   FORMAT(1H1,
     > ' **************************************************',/,
     >       '             End of MINIJET-BASES ',/,
     > 5X,'Total cross section = ',F10.4,' +- ',F10.4,'pb',/,
     > ' **************************************************')
C
      RETURN
      END
