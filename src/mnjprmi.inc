C$Id: mnjprmi.inc,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
C***********************************************************************
C*
C*  common /MNJPRM/
C*
C*  job parameters for mini-jet event generation.
C*
C***********************************************************************
C*
      COMMON /MNJPRM/ NGNTYP, NGNPRC, NQSSRC, NUMFLV,
     >                XXLAM,  XLAM, XXLAM2, XLAM2,
     >                YMAX3, YMAX4, PTMIN, PTMAX,
     >                NDISTR, NMASQ, NLAM,
     >                NGMINS, NGPLUS, NONEJT
      SAVE /MNJPRM/
C*
      INTEGER*4  NGNTYP, NGNPRC
C*
C*  NGNTYP = 0  ; Generate all type.
C*         = 1  ; Generate one resolved process.
C*         = 2  ; Generate two resolved process
C*         = 3  ; Generate only one sub-process specified by NGNPRC
C*  NGNPRC = 1 to 12 ; Generate the single subprocess whose process
C*                     ID is NGNPRC
C*
      INTEGER*4  NQSSRC
C*
C*  NQSSRC = 0  ; Q^2 = \hat(S)
C*         = 1  ; Q^2 = Pt^2
C*
      INTEGER*4  NUMFLV
C*
C*  NUMFLV; Number of flavour to generate.
C*         = 0  ; Nflv= 5 when QSQ > 500, =4 when 500 > QSQ > 50
C*         = 3  ; Always NFLV = 3
C*         = 4  ; Always NFLV = 4
C*         = 5  ; Always NFLV = 5
C*
      REAL*8  XXLAM, XLAM
      REAL*8  XXLAM2, XLAM2
C*
C*  XXLAM  ; Lambda to be used to calculate alpha_S
C*  XLAM   ; Lambda to be used to calculate parton density function.
C*
      INTEGER*4  NLAM
C*  NLAM  ; Treatment of Lambda in hard scattering. i.e. XXLAM.
C*         = 0  ; fixed value given XXLAM is used for Nf=3,4,5
C*         = 1  ; XXLAM is used for Nf=4, but is changed for Nf=3,5
C*                so that alpha_s becomes continuous at the mass
C*                threshold of new flavour.
      INTEGER*4  NDISTR
C*
C*    NDISTR ; Parton distribution function of photon
C*      = 0 ; DG, select automatically according to the qsquare.
C*      = 1 ; DG with Nflv=3
C*      = 2 ; DG with Nflv=4
C*      = 3 ; DG with Nflv=5
C*      = 4 ; LAC, SET-I
C*      = 5 ; LAC, SET-II
C*      = 6 ; LAC, SET-III
C*      = 7 ; DO
C*      = 8 ; DO + VMD
C*      = 9 ; Modified DO + VMD
C*
      INTEGER*4  NMASQ
C*
C*    NMASQ  ; taken into account mass effect in hard subprocess.
C*      = 0 ; all quark mass are assumed mass less.
C*      = 1 ; taken into account c/b quark mass.
C*      = 2 ; taken into account u/d/s/c/b quark mass.(correct for
C*            direct process only).
C*
      INTEGER*4 NGMINS, NGPLUS
C
C     NGMINS  : Source of photon beam
C         0=Bremstraulung from e-, 1=Beamstrulung from e-
C     NGPLUS  : Source of photon beam
C         0=Bremstraulung from e+, 1=Beamstrulung from e+
C
      REAL*8    YMAX3, YMAX4
C  YMAX3  : Maximum rapidity to accept event for jet-3
C  YMAX4  : Maximum rapidity to accept event for jet-4
C
      REAL*8    PTMIN, PTMAX
C  PTMIN  : Minimum Pt of produced jet.
C  PTMAX  : Maximum Pt of produced Jet.
C
      INTEGER*4 NONEJT
C  NONEJT ; A way to count cross section, when calculate one-jet
C           inclusive cross section with y-cut.
C    = 1  ; always count one-jet as one-event
C    = 2  ; Count one-jet as two, when both jet lies within y-cut value.
