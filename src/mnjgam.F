C***********************************************************************
C*$Id: mnjgam.F,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
C*    ------------------------------------=====
C*    Subroutine MNJGAM( NTYPE, X, EBEAM, FX )
C*    ------------------------------------=====
C*
C*(Function)
C*    Get weight of phton spectrum.
C*(Input)
C*    NTYPE  : Type of Photon spectrum
C*      = 1 for DG type Bremstrahlung spectrum
C*      = 2 for Bremstrahlung spectrum used by DG in the study for LC
C*      =10 for Flat beam Beamstrahlungf spectrum
C*      =21 for Beamstrahlung spectrum for 150 GeV S band JLC
C*      =22 for Beamstrahlung spectrum for 250 GeV S band JLC
C*      =23 for Beamstrahlung spectrum for 150 GeV C band JLC
C*      =24 for Beamstrahlung spectrum for 250 GeV C band JLC
C*      =25 for Beamstrahlung spectrum for 150 GeV X band JLC
C*      =26 for Beamstrahlung spectrum for 250 GeV X band JLC
C*      =27 for Beamstrahlung spectrum for 250 GeV GLC
C*      =28 for Beamstrahlung spectrum for 500 GeV GLC
C*    X      : Real*8 randum variable from 0 to 1.
C*             Photon Energy=X*Ebeam
C*    EBEAM  : Beam energy in GeV unit, required when NTYPE=1
C*           : Qsuare, when NTYPE=2               
C*(Output)
C*    FX     : Weight for the photon.
C*      	0 to 1.
C*(Author)
C*    A.Miyamoto  9-Feb,1992  Original version.
C*               18-Jul-1998  Add NEW JLC parameters.
C*               23-Jun-2004  Add GLC parameters
C*
C***********************************************************************
C
      SUBROUTINE MNJGAM( NTYPE, X, EBEAM, FX )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (PI=3.14159265,
     >           TWOPI=2*PI,
     >           ALPHA = 1./137.0359895 ,
     >           AME    = 0.51099906D-3 ,
     >           AME2   = AME*AME )
      PARAMETER (ONEO3  = 1.D0/3.D0,
     >           TWOO3  = 2.D0/3.D0 )
      PARAMETER (RECLAS=2.81794092D-13)

C ... JLC Beam parameters.

      INCLUDE "mnjbemi.inc"

      DATA JOBEAM/0/
C
C ======================================================================
C
C ----------------------------------------------------------------------
C (1) Switch according to the type of Photon spectrum.
C ----------------------------------------------------------------------
C
C ... DG type Bremstrahlung spectrum.
C
      IF( NTYPE.EQ.1 ) THEN
        FX    = ALPHA/(PI*X)*
     >        ( (1.D0+(1.D0-X)**2)*(DLOG(EBEAM/AME)-0.5D0)
     >        + X**2*0.5D0*(DLOG(2.D0*(1.D0-X)/X)+1.D0)
     >        + (2.D0-X)**2*0.5D0*DLOG(2*(1.D0-X)/(2.D0-X))
     >      )
C
C ... DG type Bremstrahlung spectrum used for the background
C     study for Linear Collier.
C
      ELSEIF( NTYPE.EQ.2 ) THEN
        FX    = 0.85D0*ALPHA/(PI*X)*DLOG(EBEAM/AME)
     >            *(1.D0+(1.D0-X)**2)
C
C ... Flux for photon-photon collision.
C
      ELSEIF( NTYPE.EQ.3 ) THEN
      IF( X.LT.0.8 ) THEN
       FX = 0.0
        ELSE
       FX = 5.0
      ENDIF
C
C
C ... Flat beam spectrum used by DG.
C
      ELSEIF( NTYPE.EQ.10 ) THEN
        IF( X.GT.0.84 ) THEN
       FX = 0.0D0
      ELSE
       FX = (2.25D0-DSQRT(X/0.166D0))*(((1.D0-X)/X)**TWOO3)
        ENDIF
C
C ... JLC Beamstraulung spectrum.
C     Average Photon spectrum during the collision.
C
      ELSEIF( NTYPE.GE.21.AND.NTYPE.LE.28) THEN
      NBEAM = NTYPE - 20
      IF( NBEAM.NE.JOBEAM ) THEN
       JOBEAM = NBEAM
          E0 = BEAMP(1,NBEAM)
          R0 = E0/AME
          UPSBAR = 5./6.*RECLAS*RECLAS*R0*BEAMP(5,NBEAM)/
     >        (ALPHA*BEAMP(4,NBEAM)*(BEAMP(2,NBEAM)+BEAMP(3,NBEAM)))
          XNCL = 1.06*ALPHA*RECLAS*BEAMP(5,NBEAM)*
     >         2./(BEAMP(2,NBEAM)+BEAMP(3,NBEAM))
          XNGAM = XNCL/SQRT( 1.D0+UPSBAR**TWOO3)
          XKAPPA= 2.D0/(3.D0*UPSBAR)
CUX2      IGAM = GAMMA(ONEO3, GAMVAL )
          GAMVAL = DGAMMA(ONEO3)
          FACT  = XKAPPA**ONEO3/GAMVAL
        ENDIF
        G = 1.D0 - ( 0.5D0*(1+X)*XNCL/XNGAM + 0.5D0*(1-X) )
     >           * (1-X)**TWOO3
        FX= FACT/(X**TWOO3 * (1.D0-X)**ONEO3) *DEXP(-XKAPPA*X/(1-X))
     >    * ( (1-1./6./SQRT(XKAPPA))/G
     >          *(1-(1-DEXP(-G*XNGAM))/(G*XNGAM))
     >        + 1./6./SQRT(XKAPPA)
     >          *(1.-(1.-DEXP(-XNGAM))/XNGAM)   )

      ENDIF

      RETURN
      END
