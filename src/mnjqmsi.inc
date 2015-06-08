C$Id: mnjqmsi.inc,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
      REAL*8 QMAS(6), QHMAS(6)
      COMMON /MNJQMS/ QMAS, QHMAS
      SAVE /MNJQMS/
C
C   We tooked  same c and b quark masses taken in
C   M.Drees and R.M. Godbole (Nucl.Phys. B339(1990) 355)
C   And for the determination of the boundary of the integration
C   region, we use the more appropriate mass Mq of the lowest
C   lying Q-meson. (see above paper).
C                        D     U     S      C    B
CC    DATA      QMAS / 0.325, 0.325, 0.5,  1.5,  4.5,  0/
CC    DATA      QHMAS/ 0.325, 0.325, 0.5,  1.87, 5.3,  0/
