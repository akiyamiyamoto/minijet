C
C$Id: mnjbemi.inc,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
      REAL*8   BEAMP(7,8)
      COMMON /MNJBEM/ BEAMP
      SAVE /MNJBEM/
C
C     Parameters for beamstrahlung photon generation.
C
C     BEAMP(1,i) = Beam Energy (GeV)
C          (2,i) = Sigma_x at IP (cm)
C          (3,i) = Sigma_y at IP (cm)
C          (4,i) = Sigma_z at IP (cm)
C          (5,i) = Number of particles in a bunch
C          (6,i) = Luminosity without pinch effect.
C          (7,i) =
C
C     DATA BEAMP/
C    >  150., 335.D-7, 3.92D-7,  80.D-4, 1.56D10,  3.1D33, 1.80 ,
C    >  250., 301.D-7, 3.04D-7,  80.D-4, 1.30D10,  4.4D33, 1.62 ,
C    >  150., 335.D-7, 3.92D-7,  80.D-4, 1.00D10,  5.9D33, 1.19 ,
C    >  250., 260.D-7, 3.04D-7,  80.D-4, 1.00D10,  9.7D33, 1.44 ,
C    >  150., 335.D-7, 3.92D-7,  85.D-4, 0.63D10,  3.5D33, 0.80 ,
C    >  250., 260.D-7, 3.04D-7,  67.D-4, 0.63D10,  6.3D33, 0.95 /

