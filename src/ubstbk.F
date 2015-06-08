CC********************************************************************CC
C*                                                                    *C
C*==========================--===                                     *C
C*  Subroutine UBSTBK(PB,PR,PA)                                       *C
C*==========================--===                                     *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    routine to transform PB(4) in PR(4)-rest frame to PA(4)         *C
C*    in PR(4)-moving frame.                                          *C
C* (Inputs)                                                           *C
C*     PB(4) = 4-vector of a particle in PR-rest frame.               *C
C*     PR(4) = 4-vector of a reference particle.                      *C
C* (Output)                                                           *C
C*     PA(4) = 4-vector of a particle in PR-moving frame.             *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*       UCONJ4, UBSTFD                                               *C
C* (Update Record)                                                    *C
C*    7/29/85  K. Fujii  Original version.                            *C
C*$Id: ubstbk.F,v 1.2 2004/07/01 00:41:33 miyamoto Exp $                                                                *C
CC********************************************************************CC
C
C ============================--===
      SUBROUTINE UBSTBK(PB,PR,PA)
C ============================--===
C
      IMPLICIT REAL*4 (A-H,O-Z)
      DIMENSION PB(4),PR(4),PA(4),PRNV(4)
CC    CALL UCONJ4(PR,PRNV)
      PRNV(1) = -PR(1)
      PRNV(2) = -PR(2)
      PRNV(3) = -PR(3)
      PRNV(4) =  PR(4)
      CALL UBSTFD(PB,PRNV,PA)
      RETURN
      END
