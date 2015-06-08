C$Id: mnjevti.inc,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
C
C This common contains event information.
C
      REAL*8          RS, EBEAM, XG(25), EGAM1, EGAM2
     >              , CS, PCM, EGAM10, EGAM20, FONE
      INTEGER*4       NPRC, INDX
      COMMON /MNJEVT/ RS, EBEAM, XG  , EGAM1, EGAM2
     >              , CS, PCM, EGAM10, EGAM20, FONE
     >              , NPRC, INDX
      SAVE /MNJEVT/
C
C   RS     ; Center of mass  energy
C   EBEAM  ; Beam energy
C   XG(1:6); Copy of random variables used for the event generation.
C   XG(21) ; X1 = E_Gamma/E_beam from e-
C     (22) ; X2 = E_Gamma/E_beam from e+
C     (23) ; X3 = E_parton/E_gamma from e-
C     (24) ; X4 = E_parton/E_gamma from e+
C   CS     ; Cos(th) of the event
C   PCM    ; CMS Momentum of haard collision
C
C   EGAM10 ; X1*EBEAM
C   EGAM20 ; X2*EBEAM
C   EGAM1  ; X1*X3*EBEAM
C   EGAM2  ; X2*X4*EBEAM
C
C   FONE   ; Weight of the event, when count one-jet inclusive cross
C           section.
C
C   NPRC   ; Process ID of generated event
C   INDX   ; Pointer to KCDATA, of generated event
