CC********************************************************************CC
C*                                                                    *C
C*==========================--===                                     *C
C*  Subroutine UBSTFD(PB,PR,PA)                                       *C
C*==========================--===                                     *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    routine to transform PB(4) to PR(4)-rest frame ,PA(4).          *C
C* (Inputs)                                                           *C
C*     PB(4) = 4-vector of a particle.                                *C
C*     PR(4) = 4-vector of a reference particle.                      *C
C* (Output)                                                           *C
C*     PA(4) = 4-vector of a particle in PR-rest frame.               *C
C* (Relation)                                                         *C
C*     Calls                                                          *C
C*        UDOT4                                                       *C
C* (Update Record)                                                    *C
C*    7/29/85  K. Fujii  Original version.                            *C
C*$Id: ubstfd.F,v 1.2 2004/07/01 00:41:33 miyamoto Exp $                                                                *C
CC********************************************************************CC
C
C ============================--===
      SUBROUTINE UBSTFD(PB,PR,PA)
C ============================--===
C
      IMPLICIT REAL*4 (A-H,O-Z)
      DIMENSION PB(4),PR(4),PA(4)
C     AM=UDOT4(PR,PR)
      AM=PR(4)*PR(4)-PR(1)*PR(1)-PR(2)*PR(2)-PR(3)*PR(3)
      IF(AM.LE.0.) GO TO 9999
      AM=SQRT(AM)
C     PA4=UDOT4(PB,PR)/AM
      PA4=(PB(4)*PR(4)-PB(1)*PR(1)-PB(2)*PR(2)-PB(3)*PR(3))/AM
      A=(PB(4)+PA4)/(PR(4)+AM)
      PA(4)=PA4
      DO 10 I=1,3
      PA(I)=PB(I)-A*PR(I)
   10 CONTINUE
      RETURN
C
 9999 WRITE(6,*) ' >>>> Error in UBSTFD. -ve mass squared.>>>'
C
      END
