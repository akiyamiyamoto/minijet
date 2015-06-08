C***********************************************************************
C*
C* ------------------===========================
C* Subroutine MNJMPL( NFINAL, PARTON, GBUF , PVCT )
C* -------------------===========================
C*(Function)
C*  Make Parton_List from generated 4 momentum information.
C*(Input)
C*  none
C*(Output)
C*  NFINAL    :  Number of final partons, including spectators.
C*             = 0 if event is not created by some reason.
C*  PARTON(20,4) : Particle_List of final particles.
C*                 to be stored in a bank Spring:Parton_List
C*                 (1) = Particle serial number.
C*                 (2) = Particle ID (d=1,u=2,s=3,c=4,b=5,g=21,gamma=22)
C*                 (3) = Mass (GeV)
C*                 (4) = Charge
C*                 (5) = Px(GeV)
C*                 (6) = Py(GeV)
C*                 (7) = Pz(GeV)
C*                 (8) = E(GeV)
C*                 (9) =  index of hard scatering particle.
C*                (10) =    not used
C*                (11) =    not used
C*                (12) = # daughter parton
C*                (13) = particle serial # of the 1st daughter
C*                (14) = particle serial # of the parent daughter
C*                (15) =    color flow infor for LesHouse format
C*                (16) =    anti-color flow info for LesHouse format
C*                (17) = helicity
C*                (18) = colour single group ID
C*                (19) =    not used
C*                (20) =  10000*KS + KH
C*  GBUF(20)   : Generated event information.
C*                ( 1) = Energy of resolved parton from e- side.(EGAM1)
C*                ( 2) = Energy of resolved parton from e+ side.(EGAM2)
C*                ( 3) = Energy of photon from e- (EGAM10)
C*                ( 4) = Energy of photon from e+ (EGAM20)
C*                ( 5) = sqrt(Shat)
C*                ( 6) = Minijet Pt.
C*                ( 7) = Cos(Theta)^*
C*                ( 8) = Phi^*
C*                ( 9) = W_{gamma-gamma}
C*                (10) = Rapidity of parton -1  in Lab(e+e-CM) system
C*                (11) = Rapidity of another parton in Lab(e+e-CM)system
C*                (12) = PCM
C*                (13) = not used
C*                (14) = not used
C*                (15) = not used.
C*                (20) = WT( event weight factor).
C*  PVCT(4,3) :  4 momentum of
C*               1=hard-subsystem measured in Lab(e+e- CM) system.
C*               2=Hard scattered q or gluon in the sub-system.
C*               3=Hard scattered q-bar or gluon in the sub-system.
C*
C*
C*  A. Miyamoto  27-June-1992
C*
C***********************************************************************
C*
      SUBROUTINE MNJMPL( NFINAL, RBUF, GBUF, PVCT)
C
      REAL*4  PVCT(4,3), RBUF(20,4), GBUF(20)
C
      INCLUDE "mnjevti.inc"
      INCLUDE "mnjtypi.inc"
      INCLUDE "mnjprmi.inc"
      INCLUDE "mnjqmsi.inc"
C
      PARAMETER (PI    = 3.141592653E0,
     >           TWOPI = PI*2.E0)
      REAL*4  PCM3(4)
C
      LOGICAL FQPROD
C
      REAL*8  DRN
C
C =====< Entry Point >=================================================
C
C
C
C Know process type and determine final particles.
C
C
      NFINAL = 0
      IAPRC  = IABS(NPRC)
      IF( IAPRC.EQ.0 ) RETURN
      IAD1 = IABS(KCDATA(1,INDX))
      IAD2 = IABS(KCDATA(2,INDX))
      IAD3 = IABS(KCDATA(3,INDX))
      IAD4 = IABS(KCDATA(4,INDX))
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
C
C .. Set CM momentum
C
      E1 = EGAM1
      E2 = EGAM2

C ... PVCT(,1) ; CM vector.
      PVCT(1,1) = 0
      PVCT(2,1) = 0
      PVCT(3,1) = E1-E2
      PVCT(4,1) = E1+E2
      WGG       = 2.*SQRT(E1*E2)
C
C Set 4 momenta according to the mode.
C
      SN    = SQRT((1.-CS)*(1.+CS))
      PHI   = TWOPI*DRN(IDUM)
CDEBUG
CDEBUG
      PVCT(1,2) = PCM*SN*COS(PHI)
      PVCT(2,2) = PCM*SN*SIN(PHI)
      PVCT(3,2) = PCM*CS
      PVCT(4,2) = PCM
Ctaken into account quark masses.
      IF(FQPROD) PVCT(4,2) = SQRT(PCM**2 + QMAS(IAD3)**2)
      PVCT(1,3) = -PVCT(1,2)
      PVCT(2,3) = -PVCT(2,2)
      PVCT(3,3) = -PVCT(3,2)
      PVCT(4,3) = PCM
Ctaken into account quark masses.
      IF(FQPROD) PVCT(4,3) = SQRT(PCM**2 + QMAS(IAD4)**2)
      PTEVT     = PCM*SN
      GBUF(1)   = EGAM1
      GBUF(2)   = EGAM2
      GBUF(3)   = EGAM10
      GBUF(4)   = EGAM20
CD    GBUF(5)  :  = sqrt(shut) = WGG
      GBUF(5)   = WGG
      GBUF(6)   = PTEVT
C     GBUF(7)   = CS
      GBUF(8)   = PHI
      WGG00     = 2.*SQRT(EGAM10*EGAM20)
      GBUF(9)   = WGG00
      GBUF(12)  = PCM
C   WT ; event weight factor.
      WT = 1.0
      GBUF(20)  = WT
C
C .. In case of Direct process
C
       CALL UVZERO( 80 , RBUF)
      XF = 1.0
C  SPECOF -- off spectator jet generation.
CSPECOF            XF = 0.0001

      IF( IAPRC .EQ. 1 ) THEN
        NFINAL = 2
        RBUF(2,1) = KCDATA(3,INDX)
        RBUF(2,2) = KCDATA(4,INDX)
        DO 200 I = 1, NFINAL
          CALL UBSTBK( PVCT(1, I+1), PVCT(1,1), RBUF(5,I))
200     CONTINUE
        RBUF(20,1) = 10000.
        RBUF(20,2) = 0.
        IP1= 1
        IP2= 2
        RBUF(15,1) = 501
        RBUF(16,2) = 501
C
C ... 1 Resolved, gamma+q --> gluon + q case.
C       NPRC = +2, emit q from e+
C       NPRC = -2, emit q from e-
C
      ELSEIF( IAPRC .EQ. 2 ) THEN
        NFINAL = 3
        IF( NPRC.GT.0 ) THEN
          RBUF(2,1) = KCDATA(4,INDX)
          RBUF(2,2) = KCDATA(3,INDX)
          RBUF(2,3) =-KCDATA(4,INDX)
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,2))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,1))
          ESPEC     = EGAM20*(1-XG(24))* XF
          RBUF(7,3) = -ESPEC
          RBUF(8,3) =  ESPEC
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 0.
          IP1= 1
          IP2= 2
        ELSE
          RBUF(2,3) = KCDATA(3,INDX)
          RBUF(2,2) = KCDATA(4,INDX)
          RBUF(2,1) =-KCDATA(3,INDX)
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,2))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,3))
          ESPEC     = EGAM10*(1-XG(23))*XF
          RBUF(7,1) = ESPEC
          RBUF(8,1) = ESPEC
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 0.
          IP1= 2
          IP2= 3
        ENDIF

        DO 210 INP=1, 3
          IDQK = RBUF(2,INP)
          IF(IDQK.EQ.21) THEN
             RBUF(15,INP)=502
             RBUF(16,INP)=501
          ELSEIF( IDQK.GT.0 ) THEN
             RBUF(15,INP)=501
          ELSE
             RBUF(16,INP)=502
          ENDIF
210     CONTINUE

C
C ... 1 Resolved, gamma + gluon --> q qbar case
C       NPRC = +3, emit q from e+
C       NPRC = -3, emit q from e-
C     Assumes always u-ubar spectator.
C
      ELSEIF( IAPRC .EQ. 3 ) THEN
        NFINAL = 3
        IF( NPRC.GT.0 ) THEN
          RBUF(2,1) = KCDATA(3,INDX)
          RBUF(2,2) = 21
          RBUF(2,3) = KCDATA(4,INDX)
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,1))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,3))
          ESPEC     = EGAM20*(1-XG(24))*XF
          RBUF(7,2) = -ESPEC
          RBUF(8,2) = ESPEC
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 0.
          IP1= 1
          IP2= 3
        ELSE
          RBUF(2,1) = KCDATA(3,INDX)
          RBUF(2,2) =21
          RBUF(2,3) = KCDATA(4,INDX)
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,1))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,3))
          ESPEC     = EGAM10*(1-XG(23))*XF
          RBUF(7,2) = ESPEC
          RBUF(8,2) = ESPEC
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 0.0
          IP1= 1
          IP2= 3
        ENDIF

        DO 220 INP=1, 3
          IDQK = RBUF(2,INP)
          IF(IDQK.EQ.21) THEN
             RBUF(15,INP)=502
             RBUF(16,INP)=501
          ELSEIF( IDQK.GT.0 ) THEN
             RBUF(15,INP)=501
          ELSE
             RBUF(16,INP)=502
          ENDIF
220     CONTINUE
C
C ... 2 Resolved
C
      ELSE
        IAD1 = IABS(KCDATA(1,INDX))
        IAD2 = IABS(KCDATA(2,INDX))
        IAD3 = IABS(KCDATA(3,INDX))
        IAD4 = IABS(KCDATA(4,INDX))
C  Initial state is collision among quarks/anti-quarks
        IF( IAD1.LT.10 .AND. IAD2.LT.10 ) THEN
          NFINAL = 4
          RBUF(2,1) =-KCDATA(1,INDX)
          RBUF(2,2) = KCDATA(3,INDX)
          RBUF(2,3) = KCDATA(4,INDX)
          RBUF(2,4) =-KCDATA(2,INDX)
C--
C Symmetrize color flow for q_i q_i --> q_i q_i scattering.
C--
            IF(IAPRC.EQ.4 ) THEN
              IF(DRN(IDUM).GT.0.5)   NPRC = - NPRC
             ENDIF
          IF(NPRC.GT.0)  THEN
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,2))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,3))
          IP1= 2
          IP2= 3
          ELSE
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,3))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,2))
          IP1= 3
          IP2= 2
          ENDIF
          ESPEC1    = EGAM10*(1-XG(23))*XF
          ESPEC2    = EGAM20*(1-XG(24))*XF
          RBUF(7,1) = ESPEC1
          RBUF(7,4) =-ESPEC2
          RBUF(8,1) = ESPEC1
          RBUF(8,4) = ESPEC2
          RBUF(20,1)= 10000.
          RBUF(20,2)= 0.
          RBUF(20,3)= 10000.
          RBUF(20,4)= 0.
C--   q_i q_ibar --> g g case.
          IF(IAPRC.EQ.9)  THEN
            RBUF(20,1)= 10000.
            RBUF(20,2)= 10000.
            RBUF(20,3)= 10000.
            RBUF(20,4)= 0.
            IF( IFIX(RBUF(2,1)).GT.0 ) THEN
              RBUF(15,1)=501
              RBUF(16,2)=501
              RBUF(15,2)=502
              RBUF(16,3)=502
              RBUF(15,3)=503
              RBUF(16,4)=503
            ELSE
              RBUF(16,1)=501
              RBUF(15,2)=501
              RBUF(16,2)=502
              RBUF(15,3)=502
              RBUF(16,3)=503
              RBUF(15,4)=503
            ENDIF
          ELSE
            IF( IFIX(RBUF(2,1)).GT.0 ) THEN
              RBUF(15,1)=501
              RBUF(16,2)=501
            ELSE
              RBUF(16,1)=501
              RBUF(15,2)=501
            ENDIF
            IF( IFIX(RBUF(2,4)).GT.0 ) THEN
              RBUF(15,4)=502
              RBUF(16,3)=502
            ELSE
              RBUF(16,4)=502
              RBUF(15,3)=502
            ENDIF
          ENDIF

C ------- Initial state is q-gloun collision
        ELSEIF( IAD1.LT.10 .AND. IAD2.GT.10 ) THEN
          NFINAL = 4
          RBUF(2,1) =-KCDATA(1,INDX)
          RBUF(2,4) = KCDATA(3,INDX)
          RBUF(2,2) = KCDATA(4,INDX)
          RBUF(2,3) = 21
          IF ( IFIX(RBUF(2,1)).LT.0 ) THEN
            RBUF(16,1)=501
            RBUF(15,3)=501
            RBUF(16,3)=502
            RBUF(15,2)=502
            RBUF(16,2)=503
            RBUF(15,4)=503
          ELSEIF( IFIX(RBUF(2,1)).NE.21) THEN
            RBUF(15,1)=501
            RBUF(16,3)=501
            RBUF(15,3)=502
            RBUF(16,2)=502
            RBUF(15,2)=503
            RBUF(16,4)=503
          ENDIF

          IF(NPRC.GT.0)  THEN
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,4))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,2))
          IP1= 4
          IP2= 2
          ELSE
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,2))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,4))
          IP1= 2
          IP2= 4
          ENDIF
          ESPEC1    = EGAM10*(1-XG(23))*XF
          ESPEC2    = EGAM20*(1-XG(24))*XF
          RBUF(7,1) = ESPEC1
          RBUF(7,3) =-ESPEC2
          RBUF(8,1) = ESPEC1
          RBUF(8,3) = ESPEC2
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 10000.
          RBUF(20,4)= 0.

C -- Hard process is gluon+quark collision
        ELSEIF( IAD1.GT.10 .AND. IAD2.LT.10 ) THEN
          NFINAL = 4
          RBUF(2,3) = 21
          RBUF(2,2) = KCDATA(3,INDX)
          RBUF(2,1) = KCDATA(4,INDX)
          RBUF(2,4) =-KCDATA(2,INDX)
          IF ( IFIX(RBUF(2,1)).GT.0 .AND. IFIX(RBUF(2,1)).NE.21) THEN
            RBUF(15,1)=501
            RBUF(16,2)=501
            RBUF(15,2)=502
            RBUF(16,3)=502
            RBUF(15,3)=503
            RBUF(16,4)=503
          ELSEIF(IFIX(RBUF(2,1)).NE.21) THEN
            RBUF(16,1)=501
            RBUF(15,2)=501
            RBUF(16,2)=502
            RBUF(15,3)=502
            RBUF(16,3)=503
            RBUF(15,4)=503
          ENDIF
          IF(NPRC.GT.0) THEN
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,2))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,1))
          IP1= 2
          IP2= 1
          ELSE
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,1))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,2))
          IP1= 1
          IP2= 2
          ENDIF
          ESPEC1    = EGAM10*(1-XG(23))*XF
          ESPEC2    = EGAM20*(1-XG(24))*XF
          RBUF(7,3) = ESPEC1
          RBUF(7,4) =-ESPEC2
          RBUF(8,3) = ESPEC1
          RBUF(8,4) = ESPEC2
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 10000.
          RBUF(20,4)= 0.
        ELSE
C  gluon + gluon --> gluon + gluon case.
          NFINAL = 4
          RBUF(2,1) = 21
          RBUF(2,2) = KCDATA(3,INDX)
          RBUF(2,3) = KCDATA(4,INDX)
          RBUF(2,4) = 21
C--
C Symmetrize color flow for g  g    -->  g  g   scattering.
C--
            IF(DRN(IDUM).GT.0.5)   NPRC = - NPRC
          IF(NPRC.GT.0 ) THEN
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,2))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,3))
          IP1= 2
          IP2= 3
          ELSE
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,3))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,2))
          IP1= 3
          IP2= 2
          ENDIF
          ESPEC1    = EGAM10*(1-XG(23))*XF
          ESPEC2    = EGAM20*(1-XG(24))*XF
          RBUF(7,1) = ESPEC1
          RBUF(7,4) =-ESPEC2
          RBUF(8,1) = ESPEC1
          RBUF(8,4) = ESPEC2
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 10000.
          RBUF(20,4)= 0.
          IF( IAPRC.EQ.12 ) THEN
            RBUF(15,1)=501
            RBUF(16,2)=501
            RBUF(15,2)=502
            RBUF(16,3)=502
            RBUF(15,3)=503
            RBUF(16,4)=503
            RBUF(15,4)=504
            RBUF(16,1)=504
          ENDIF
C  gluon + gluon --> q + qbar case.
          IF(IAPRC.EQ.11) THEN
            RBUF(2,1) = 21
            RBUF(2,2) = KCDATA(3,INDX)
            RBUF(2,3) = KCDATA(4,INDX)
            RBUF(2,4) = 21
            RBUF(20,1)= 10000.
            RBUF(20,2)= 0.
            RBUF(20,3)= 10000.
            RBUF(20,4)= 0.
            IF( IFIX(RBUF(2,2)).GT.0 ) THEN
              RBUF(15,2)=501
              RBUF(16,1)=501
              RBUF(16,3)=502
              RBUF(15,4)=502
              RBUF(16,4)=503
              RBUF(15,1)=503
            ELSE
              RBUF(16,2)=501
              RBUF(15,1)=501
              RBUF(15,3)=502
              RBUF(16,4)=502
              RBUF(15,4)=503
              RBUF(16,1)=503
            ENDIF
          ENDIF
        ENDIF
      ENDIF
          RBUF(9,IP1) = 3
          RBUF(9,IP2) = 4
C-- CALICULATE RAPIDITY
       Y1 = .5 * ALOG((RBUF(8,IP1) + RBUF(7,IP1))/
     +                (RBUF(8,IP1) - RBUF(7,IP1))  )
       Y2 = .5 * ALOG((RBUF(8,IP2) + RBUF(7,IP2))/
     +                (RBUF(8,IP2) - RBUF(7,IP2))  )
       GBUF(10) = Y1
       GBUF(11) = Y2
C--
C     Boost to hard scattering C-M system to obation conTH*
C     GBUF(7)   = CS
          CALL UBSTFD( RBUF(5,IP1), PVCT(1,1), PCM3(1)   )
        PP     = SQRT(PCM3(1)**2 + PCM3(2)**2 + PCM3(3)**2)
        GBUF(7) = PCM3(3)/PP
C
      RETURN
      END
