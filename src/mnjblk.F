C$Id: mnjblk.F,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
C
      BLOCK DATA MNJBLK

      INCLUDE "mnjprmi.inc"
      INCLUDE "mnjbemi.inc"
      INCLUDE "mnjqmsi.inc"

      DATA NGNTYP/0/
      DATA NGNPRC/1/
      DATA NQSSRC/1/
      DATA NUMFLV/4/
      DATA NDISTR/1/
      DATA NMASQ/1/
      DATA XLAM/0.4/
      DATA XXLAM/0.4/
      DATA NLAM/0/
      DATA YMAX3/0.7/
      DATA YMAX4/0.7/
      DATA NGMINS/0/
      DATA NGPLUS/0/
      DATA NONEJT/1/
      DATA PTMIN/3.0/
      DATA PTMAX/8.0/
C*
C*   Beam parameter for JLC
C*
C
      DATA BEAMP/
     >  150., 335.D-7, 3.92D-7,  80.D-4, 1.56D10,  3.1D33, 1.80 ,
     >  250., 301.D-7, 3.04D-7,  80.D-4, 1.30D10,  4.4D33, 1.62 ,
     >  150., 335.D-7, 3.92D-7,  80.D-4, 1.00D10,  5.9D33, 1.19 ,
     >  250., 260.D-7, 3.04D-7,  80.D-4, 1.00D10,  9.7D33, 1.44 ,
     >  150., 335.D-7, 3.92D-7,  85.D-4, 0.63D10,  3.5D33, 0.80 ,
     >  250., 260.D-7, 3.04D-7,  67.D-4, 0.63D10,  6.3D33, 0.95 ,
     >  250., 243.D-7, 3.0D-7,  110.D-4, 0.75D10, 17.7D33, 1.49 ,
     >  250., 219.D-7, 2.1D-7,  110.D-4, 0.75D10, 18.5D33, 1.42 /

C
C   We tooked  same c and b quark masses taken in
C   M.Drees and R.M. Godbole (Nucl.Phys. B339(1990) 355)
C   And for the determination of the boundary of the integration
C   region, we use the more appropriate mass Mq of the lowest
C   lying Q-meson. (see above paper).
C                        D     U     S      C    B
      DATA      QMAS / 0.325, 0.325, 0.5,  1.5,  4.5,  0/
      DATA      QHMAS/ 0.325, 0.325, 0.5,  1.87, 5.3,  0/

      END
