C***********************************************************************
C*$Id: mnjprb.F,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
C* ----------------------------------------------=====
C*  Subroutine MNJPRB(NTYP, NFLV, QSQ, X, XLAM2, PROB)
C* ----------------------------------------------=====
C*(Function)
C*   Calculate parton density inside photon.
C*(Input)
C*   NTYP ; Type of model to be used for the parton density function.
C*      = 1 ; DG with Nflv=3
C*      = 2 ; DG with Nflv=4
C*      = 3 ; DG with Nflv=5
C*      = 4 ; LAC, SET-I
C*      = 5 ; LAC, SET-II
C*      = 6 ; LAC, SET-III
C*      = 7 ; DO
C*      = 8 ; DO + VMD
C*      = 9 ; Modified DO + VMD
C*   NFLV ; Parton ID, should be >0 1(d),2(u),3(s),4(c),5(b),21(g)
C*          otherwise PROB=1.0
C*   QSQ  ; QSQ of the process (GeV^2)
C*   X    ; Bjorken X
C*   XLAM2 ; Lambda^2 (GeV^2)
C*
C*(Output)
C*   PROB  ; Probability to find parton of energy X
C*(Author)
C*   A. Miyamoto  24-Jan-1992  ; Original version.
C*
C***********************************************************************
C
      SUBROUTINE MNJPRB(NTYP, NFLV, QSQ, X, XLAM2, PROB)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     REAL*8     XPDF(0:5)
      PARAMETER  (ALPHA=1./137.0359895)
      PARAMETER  (PI=3.141592654)
      PARAMETER  (TWOPI=2.*PI)
      PARAMETER  (E2UP=4./9., E2DOWN=1./9.)
      REAL*8     CHSQ(5)
      DATA  CHSQ/E2DOWN, E2UP, E2DOWN, E2UP, E2DOWN/
C
C ======< Entry Point >================================================
C
C ------------------------------------------------------
C --- Skip in case of direct process
C ------------------------------------------------------
C
      IF( NFLV.EQ.22 ) THEN
        PROB = 1.0D0
        RETURN
      ENDIF
C
C
C ------------------------------------------------------
C (1) DG parametrization.
C ------------------------------------------------------
C
      IF( NTYP .GE.1.AND.NTYP.LE.3 ) THEN
        NFE = NTYP
        IF( NFLV.EQ.21 ) THEN
          TLOG = DLOG(QSQ/XLAM2)
          CALL PYSTGA(NFE,X,TLOG,XPGL,XPQU,XPQD)
          PROB   = XPGL*ALPHA/X
        ELSE
          TLOG = DLOG(QSQ/XLAM2)
          CALL PYSTGA(NFE,X,TLOG,XPGL,XPQU,XPQD)
          IF( MOD(NFLV,2).EQ.1 ) THEN
            PROB = XPQD*ALPHA/X
          ELSE
            PROB = XPQU*ALPHA/X
          ENDIF
        ENDIF

C
C ------------------------------------------------------
C (2) LAC parametrization.
C ------------------------------------------------------
C
C     ELSEIF( NTYP.GE.4 .AND. NTYP.LE.6 ) THEN
C       IF( QSQ.GE.1.0D5 .OR.
C    >      (NTYP.EQ.4.AND.QSQ.LE.4.0D0) .OR.
C    >      (NTYP.EQ.5.AND.QSQ.LE.4.0D0) .OR.
C    >      (NTYP.EQ.6.AND.QSQ.LE.1.0D0) )  THEN
C         PROB = 0.0
C       ELSEIF( NFLV.EQ.21 ) THEN
C         CALL PHLAC(NTYP-3, X, QSQ, XPDF )
C         PROB = XPDF(0)/X
C       ELSE
C         CALL PHLAC(NTYP-3, X, QSQ, XPDF )
C         PROB = XPDF(NFLV)/X
C       ENDIF
C
C ------------------------------------------------------
C (3) DO parametrization.
C ------------------------------------------------------
C
      ELSEIF( NTYP.EQ.7) THEN
        TRM1   = ( 1.81 - 1.67*X + 2.16*X*X )*X**0.70
     >         / (1. - 0.4*LOG(1.-X) )
        TRM2   = 0.0038*(1.-X)**1.82/(X**1.18)
        TOTWPI = LOG(QSQ/XLAM2)/TWOPI*ALPHA/X
        IF( NFLV.EQ.21 ) THEN
          PROB = TOTWPI*0.194*(1-X)**1.03/(X**0.97)
        ELSE
          PROB = TOTWPI*(CHSQ(NFLV)*TRM1 + TRM2 )
        ENDIF

C
C ------------------------------------------------------
C (4) DO + VMD
C ------------------------------------------------------
C
      ELSEIF( NTYP.EQ.8) THEN
        TOTWPI = LOG(QSQ/XLAM2)/TWOPI*ALPHA/X
        IF( NFLV.EQ.21 ) THEN
          PROB = TOTWPI*0.194*(1-X)**1.03/(X**0.97)
          PROB = PROB + ALPHA*2.*(1.-X)**3/X
        ELSE
          TRM1   = ( 1.81 - 1.67*X + 2.16*X*X )*X**0.70
     >           / (1. - 0.4*LOG(1.-X) )
          TRM2   = 0.0038*(1.-X)**1.82/(X**1.18)
          PROB = TOTWPI*(CHSQ(NFLV)*TRM1 + TRM2)
          IF( NFLV.LT.4 ) THEN
            QVMD = ALPHA*(5./16.*(1.-X)/SQRT(X) + 0.1/X*(1-X)**5)
            PROB = PROB + QVMD
          ENDIF
        ENDIF

C
C ------------------------------------------------------
C (5) Modified DO + VMD
C ------------------------------------------------------
C
      ELSEIF( NTYP.EQ.9 ) THEN
        IF( NFLV.EQ.21 ) THEN
          TOTWPI = LOG(QSQ/XLAM2)/TWOPI*ALPHA/X
          PROB = TOTWPI*0.194*(1-X)**1.03/(X**0.97)
          PROB = PROB + ALPHA*2.*(1.-X)**3/X
        ELSE
          IF( X.GT.0.05 ) THEN
            TRM1   = ( 1.81 - 1.67*X + 2.16*X*X )*X**0.70
     >             / (1. - 0.4*LOG(1.-X) )
            TRM2   = 0.0038*(1.-X)**1.82/(X**1.18)
            TOTWPI = LOG(QSQ/XLAM2)/TWOPI*ALPHA/X
            PROB   = TOTWPI*(CHSQ(NFLV)*TRM1 + TRM2)
          ELSE
            XX     = 0.05
            TRM1   = ( 1.81 - 1.67*XX + 2.16*XX*XX )*XX**0.70
     >             / (1. - 0.4*LOG(1.-XX) )
            TRM2   = 0.0038*(1.-XX)**1.82/(XX**1.18)
            T      = LOG(QSQ/XLAM2)
            TOTWPI = T/TWOPI*ALPHA
            PR05   = TOTWPI*(CHSQ(NFLV)*TRM1 + TRM2)
            CIQ    = PR05/T*(0.05**1.6)
            PROB    = CIQ*T*X**(-1.6)
          ENDIF
          IF( NFLV.LT.4 ) THEN
            QVMD   = ALPHA*(5./16.*(1.-X)/SQRT(X) + 0.1/X*(1-X)**5)
            PROB   = PROB + QVMD
          ENDIF
        ENDIF
      ENDIF

C
C
C
      RETURN
      END
