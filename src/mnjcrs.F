C***********************************************************************
C*$Id: mnjcrs.F,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
C* ---------------------------------------------======
C*   Subroutine MNJCRS( NPROC, SHAT, IAD1, IAD2, IAD3, IAD4,
C*   +                COSTH , PTMIN, NMASQ,    DSDCS , PCM, QSQ)
C* ---------------------------------------------======
C*(Function)
C*  Calculate sub-process cross section.
C*(Input)
C*  NPROC  ; Process ID.
C*            1 = gamma + gamma   ---> q qbar
C*            2 = gamma + q       ---> gluon + q
C*           -2 = q + gamma       ---> gluon + q
C*            3 = gamma + gluon   ---> q + qbar
C*           -3 = gluon + gamma   ---> q + qbar
C*            4 = q_i   + q_i     ---> q_i + q_i
C*            5 = q_i   + q_j     ---> q_i + q_j
C*            6 = q_i + \bar(q_i) ---> q_i + \bar(q_i)
C*            7 = q_i + \bar(q_i) ---> q_j + \bar(q_j)
C*            8 = q_i + \bar(q_j) ---> q_i + \bar(q_j)
C*           -8 = \bar(q_j) + q_i ---> q_i + \bar(q_j)
C*            9 = q_i + \bar(q_i) ---> gluon + gluon
C*           -9 = \bar(q_i) + q_i ---> gluon + gluon
C*           10 = q_i + gluon     ---> q_i + gluon
C*          -10 = gluon + q_i     ---> q_i + gluon
C*           11 = gluon + gluon   ---> q_i + q_i
C*           12 = gluon + gluon   ---> gluon + gluon
C*   SHAT ; Center of mass square
C*   IAD1,IAD2,IAD3,IAD4; parton flavour index.
C*               =1(d) =2(u) =3(s) = 4(c) =5(b) =21(photon) =22(gluon)
C*   COSTH ; production angle in CM system
C*   PTMIN ; minimum PT cut value.
C*   NMASQ ;
C*          =0 use mass less formula.
C*          =1 taken into account c/b quark mass for production process
C*             of these quarks.
C*          =2 taken into account u/d/s/c/b quark mass in a gamma gamma
C*             --> Q barQ process.
C*(Output)
C*   DSDCS ; dsigma/dcos(th)      (pb)
C*   PCM   ; CM momenta of hard-subprocess( in a final state).
C*   QSQ   ; energy scale QSQ used in Alphas and q(x). This values
C*           may be changed in the heavy mass quark cases.
C*(Author)
C*   A. Miyamoto   19-Nov-1991  Original version.
C*   H. Hayashii   27-Jul-1992  Add cross section of massive quark
C* production process.
C*********************************************************************
C*
      SUBROUTINE MNJCRS( NPROC, SHAT, IAD1, IAD2, IAD3, IAD4,
     +                  COSTH, PTMIN, NMASQ,    DSDCS , PCM, QSQ)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "mnjqmsi.inc"

      PARAMETER (GEV2PB=0.38927E+9)
      PARAMETER (ALPHA=1./137.0359895)
      PARAMETER (PI=3.14159265358979323846)
      PARAMETER (TWOPI=2.*PI)
      LOGICAL   FQPROD, FQEXCT
CDMP  DATA NDMP  /0/
C
C======< Entry Point >===============================================
C
      DSDCS= -0.0000000001D0
      PCM  = -0.0000000001D0
      IAPRC = IABS(NPROC)
      ALPHAS = 0.12
      CS    = COSTH
      IF( NPROC.LT.0 ) CS = -COSTH

C--
C Now quark mass can be taken into account in a heavy quark pro
C duction processes.
C--
C           --- direct  gamma + gamma ---> q bar q (q=d/u/s/c/b)
       FQPROD = (NMASQ.EQ.2 .AND. IAPRC.EQ.1 .AND. IAD3.GE.1).OR.
C           --- direct   gamma+ gamma ---> Q bar Q (Q=c/b)
     +          (NMASQ.EQ.1 .AND. IAPRC.EQ.1 .AND. IAD3.GE.4).OR.
C           --- one resolve gamma + g  ---> Q bar Q (Q=c/b)
     +          (NMASQ.EQ.1 .AND. IAPRC.EQ.3 .AND. IAD3.GE.4).OR.
C           --- two resolve q +barq    ---> Q bar Q (Q=c/b)
     +          (NMASQ.EQ.1 .AND. IAPRC.EQ.7 .AND. IAD3.GE.4).OR.
C           --- two resolve g + g     ---> Q bar Q (Q=c/b)
     +          (NMASQ.EQ.1 .AND. IAPRC.EQ.11.AND. IAD3.GE.4)
C--
C  Heavy quark excitation proccess.
C--
       FQEXCT = (NMASQ.GE.1 .AND. IAD1.GE.4.AND.IAD1.LT.10) .OR.
     +          (NMASQ.GE.1 .AND. IAD2.GE.4.AND.IAD2.LT.10)
C--
C Obatin S,T,U and check kinematical boundary condition.
C--
        S = SHAT
      IF(FQEXCT    )          THEN
        PRINT *,' MNJCRS: sorry  cross section of this process '
        PRINT *,' does not prepared.'
        PRINT *,' IAPRC=',IAPRC,' NMASQ =',NMASQ
        PRINT *,' IAD1,IAD2,IAD3,IAD4=',IAD1,IAD2,IAD3,IAD4
        STOP
      ELSEIF(FQPROD    )          THEN
        AM   = QMAS(IAD3)
        AMSQ = AM * AM
        SMIN = 4.* AMSQ
        SHMIN= 4.* QHMAS(IAD3)**2
        IF( S .LT. SHMIN)                       RETURN
        PCM  = SQRT(S- SMIN )/2.
        T    = AMSQ - 0.5*S*( 1 - SQRT(1. - SMIN/S)* CS)
        U    = AMSQ - 0.5*S*( 1 + SQRT(1. - SMIN/S)* CS)
C- =============================================================
C- Take  QSQ =  SHAT
C for massive quark case(c/b),.
C   ref.  M.Drees etal.  Nucl.Phys. B339(1990) 355
C   ref.  B.L. Combridge Nucl.Phys. B151(1979) 429
C--
           QSQ = SHAT
C-
C Apply Ptmin cut, also,for dirct process of u/d/s quark production
C even though quark masses are taken into account in a cross section,
C if required.
C--
        IF(NMASQ.EQ.2 .AND. IAPRC.EQ.1 .AND. IAD3.LE.3)  THEN
        PT   = PCM*SQRT((1.0D0  - CS)*(1.0D0 + CS))
        IF( PT.LT. PTMIN )                      RETURN
        ENDIF
C--
C mass less kinematics.
C--
      ELSE
        SMIN = 4.* PTMIN**2
        IF( S .LT. SMIN )                       RETURN
        PCM  = SQRT(S)/2.
        PT   = PCM*SQRT((1.0D0  - CS)*(1.0D0 + CS))
        IF( PT.LT. PTMIN )                      RETURN
        T = -0.5*S*(1.-CS)
        U = -0.5*S*(1.+CS)
      ENDIF
C
C ... Switch according to the process
C
C gamma + gamma --> q + qbar
      IF( IAPRC .EQ. 1 ) THEN
        IF(.NOT.FQPROD)           THEN
        DSDCS = PI*ALPHA*ALPHA/S*3.
     >        *(T/U +U/T)
        ELSE
        SMIN = 4.* AMSQ
        DSDT  = 2.*PI*ALPHA*ALPHA/S/S*3.
     >        *( (AMSQ-T)/(AMSQ-U)  +(AMSQ-U)/(AMSQ-T)
     >         + 4.* ( AMSQ/(AMSQ-T) + AMSQ/(AMSQ-U) )
     >         - 4.* ( AMSQ/(AMSQ-T) + AMSQ/(AMSQ-U) )**2 )
        DSDCS = 0.5* S * SQRT( 1. - SMIN/S) * DSDT
        ENDIF
C
C gamma + q --> gluon + q
      ELSEIF( IAPRC .EQ. 2 ) THEN
        DSDCS = -4.*PI*ALPHA*ALPHAS/(3.*S)
     >          *(T/S+S/T)
C
C gamma + gluon --> q + qbar
      ELSEIF( IAPRC .EQ. 3 ) THEN
        IF(.NOT. FQPROD)          THEN
        DSDCS = 0.5*PI*ALPHA*ALPHAS/S
     >          *(U/T+T/U)
        ELSE
        SMIN = 4.* AMSQ
        DSDT  =    PI*ALPHA*ALPHAS/S/S
     >        *( (AMSQ-T)/(AMSQ-U)  +(AMSQ-U)/(AMSQ-T)
     >         + 4.* ( AMSQ/(AMSQ-T) + AMSQ/(AMSQ-U) )
     >         - 4.* ( AMSQ/(AMSQ-T) + AMSQ/(AMSQ-U) )**2 )
        DSDCS = 0.5* S * SQRT( 1. - SMIN/S) * DSDT
        ENDIF
C
C q_i + q_i --> q_i + q_i
      ELSEIF( IAPRC .EQ. 4 ) THEN
        DSDCS = 0.5*PI*ALPHAS*ALPHAS/S
     >         *( 4./9.*( (S*S+U*U)/(T*T) + (S*S+T*T)/(U*U) )
     >            -8./27.*S*S/(U*T) )
        DSDCS = 0.5*DSDCS
C
C q_i + q_j --> q_i + q_j
      ELSEIF( IAPRC .EQ. 5 ) THEN
        DSDCS = 0.5*PI*ALPHAS*ALPHAS/S
     >         *4./9.*(S*S+U*U)/(T*T)
C
C q_i + bar(q_i) --> q_i + bar(q_i)
      ELSEIF( IAPRC .EQ. 6 ) THEN
        DSDCS = 0.5*PI*ALPHAS*ALPHAS/S
     >         *( 4./9.*( (S*S+U*U)/(T*T) + (T*T+U*U)/(S*S) )
     >           - 8./27.*U*U/(S*T) )
C
C q_i + bar(q_i)  ---> q_j + bar(q_j)
      ELSEIF( IAPRC .EQ. 7 ) THEN
        IF(.NOT. FQPROD)          THEN
        DSDCS = 0.5*PI*ALPHAS*ALPHAS/S
     >          *4./9.*( (T*T+U*U)/(S*S) )
         ELSE
        SMIN = 4.* AMSQ
        DSDT  =     PI*ALPHAS*ALPHAS/S/S
     >          *4./9.*( (  (AMSQ-T)**2 + (AMSQ-U)**2 + 2.0*AMSQ*S )
     >          /(S*S)  )
        DSDCS = 0.5*S * SQRT( 1.0 - SMIN/S) * DSDT
         ENDIF
C
C q_i + bar(q_j) --> q_i + bar(q_j)
      ELSEIF( IAPRC .EQ. 8 ) THEN
        DSDCS = 0.5*PI*ALPHAS*ALPHAS/S
     >          *4./9.*(S*S+U*U)/(T*T)
C
C q_i + bar(q_i) --> gluon + gluon
      ELSEIF( IAPRC .EQ. 9 ) THEN
        DSDCS = 0.5*PI*ALPHAS*ALPHAS/S
     >        *( 32./27.*(U*U+T*T)/(U*T) - 8./3.*(U*U+T*T)/(S*S) )
C
C q_i + gluon ---> q_i + gluon
      ELSEIF( IAPRC .EQ. 10 ) THEN
        DSDCS = 0.5*PI*ALPHAS*ALPHAS/S
     >        *( -4./9.*(U*U+S*S)/(U*S) + (U*U+S*S)/(T*T) )
C--
C gluon + gluon ---> q_i + bar(q_i)
C--
      ELSEIF( IAPRC .EQ. 11 ) THEN
      IF(.NOT. FQPROD)          THEN
        DSDCS = 0.5*PI*ALPHAS*ALPHAS/S
     >        *(  1./6.*(U*U+T*T)/(U*T) - 3./8.*(U*U+T*T)/(S*S) )
C           symmetrize factor .
        DSDCS = 0.5*DSDCS
      ELSE
        SMIN = 4.* AMSQ
        DSDT  =     PI*ALPHAS*ALPHAS/S/S/16. *(
     >       12.0/S/S*(AMSQ-T)*(AMSQ-U)
     >    +  8./3.*( (AMSQ-T)*(AMSQ -U) -2.*AMSQ*(AMSQ+T))/(AMSQ-T)**2
     >    +  8./3.*( (AMSQ-T)*(AMSQ -U) -2.*AMSQ*(AMSQ+U))/(AMSQ-U)**2
     >    -  2./3.* AMSQ*(S- 4.0*AMSQ)/( AMSQ-T)/(AMSQ-U)
     >    -  6.   *((AMSQ-T)*(AMSQ-U) + AMSQ*(U-T) )/S/(AMSQ-T)
     >    -  6.   *((AMSQ-T)*(AMSQ-U) + AMSQ*(T-U) )/S/(AMSQ-U)
     >    )
C      DSDT = 0.5 * DSDT
        DSDCS = 0.5*S * SQRT( 1.0 - SMIN/S) * DSDT
      ENDIF
C
C gluon + gluon ---> gluon + gluon
      ELSEIF( IAPRC .EQ. 12 ) THEN
        DSDCS = 0.5*PI*ALPHAS*ALPHAS/S
     >        *4.5*(3.-U*T/(S*S) - U*S/(T*T) - S*T/(U*U) )
        DSDCS = 0.5*DSDCS

      ELSE
        PRINT *,'%Error in MNJCRS ... Invalid NPROC is specified.',
     >          ' NPROC=',NPROC
      ENDIF
C
      DSDCS = DSDCS * GEV2PB
CDMP  IF(NDMP.LE.30)  THEN
CDMP     NDMP = NDMP + 1
CDMP    PRINT *,' MNJCRS: '
CDMP    PRINT *,' IAPRC=',IAPRC,' NMASQ =',NMASQ
CDMP    PRINT *,' IAD1,IAD2,IAD3,IAD4=',IAD1,IAD2,IAD3,IAD4
CDMP    PRINT *,' FQPROD  =',FQPROD,' CS =',CS
CDMP    PRINT *,' AM    =',AM  ,' S=',S,' T=',T,' U=',U
CDMP    PRINT *,' DSDCS =',DSDCS
CDMP    ENDIF
      RETURN
      END
