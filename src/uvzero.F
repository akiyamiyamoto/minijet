C**************************************************************
C                                                             *
C SUBROUTINE UVZERO( NWORD, ARRAY )                           *
C                                                             *
C  (Purpose)                                                  *
C       Zero clear array.    .                                *
C  (Input)                                                    *
C       ARRAY : Target array.                                 *
C       NWORD : # of words ( size of ARRAY.)                  *
C  (Author)                                                   *
C       A. Miyamoto    18-Jun-1986                            *
C$Id: uvzero.F,v 1.2 2004/07/01 00:41:33 miyamoto Exp $                                                         *
C**************************************************************
C
      SUBROUTINE UVZERO( NW, NARRAY)
      REAL*4 NARRAY(NW)
      DO 100 I = 1, NW
        NARRAY(I) = 0
100   CONTINUE
      RETURN
      END
