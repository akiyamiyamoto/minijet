C********************************************************************
C*$Id: mnjtit.F,v 1.2 2004/07/01 00:41:33 miyamoto Exp $
C* -------------------
C*  Subroutine MNJTIT
C* -------------------
C*(Function)
C*  Print job parameters to unit# 6.
C*(Input/Output)
C*  obtained from commons.
C*(Author)
C*  A. Miyamoto  21-September, 1993.
C*
C********************************************************************
C
      SUBROUTINE MNJTIT
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "mnjbasi.inc"
      INCLUDE "mnjevti.inc"
      INCLUDE "mnjprmi.inc"

C
C=====< Entry Point >===================================================
C
        PRINT *,' Parameters for BASES'
        PRINT *,'     NCALL =',NCALL
        PRINT *,'     ITMX1 =',ITMX1,'   ITMX2=',ITMX2
        PRINT *,'     ACC1  =',ACC1, '   ACC2 =',ACC2
        PRINT *,' '
        PRINT *,' Generate mini-jet event due to mini-jet.'

        PRINT *,' EBEAM =',EBEAM,' GeV'
        PRINT *,' PTMIN =',PTMIN,' GeV'

CC      READ(9,*) NTPJLC
CC      READ(9,*) NSEED
CC      CALL DRNSET(NSEED)
C
        PRINT *,'***** Other input parameters. ***********'
        PRINT *,' NGNTYP=',NGNTYP,'(Generated event type.)'
        PRINT *,'       =0 To generate all type events.'
        PRINT *,'       =1 To generate only one resolved process.'
        PRINT *,'       =2 To generate only two resolved process.'
        PRINT *,'       =3 To generate single subprocess specified ',
     >                         ' by NGNPRC.'
        PRINT *,'       =4 To generate only one and two res. processes'
        PRINT *,' NGNPRC=',NGNPRC,' (Subprocess type',
     >                         ' Valid when NGNTYP=3)'
        PRINT *,'       = 1 ;  gamma + gamma --> q + qbar '
        PRINT *,'       = 2 ;  gamma + q     --> gluon + q'
        PRINT *,'       = 3 ;  gamma + gluon --> q + qbar '
        PRINT *,'       = 4 ;  q_i + q_i     --> q_i + q_i '
        PRINT *,'       = 5 ;  q_i + q_j     --> q_i + q_j '
        PRINT *,'       = 6 ;  q_i + bar(q_i)--> q_i + bar(q_i)'
        PRINT *,'       = 7 ;  q_i + bar(q_i)--> q_j + bar(q_j)'
        PRINT *,'       = 8 ;  q_i + bar(q_j)--> q_i + bar(q_j)'
        PRINT *,'       = 9 ;  q_i + bar(q_i)--> gluon + gluon '
        PRINT *,'       =10 ;  q   + gluon   --> q + gluon '
        PRINT *,'       =11 ;  gluon + gluon --> q + qbar    '
        PRINT *,'       =12 ;  gluon + gluon --> gluon + gluon '
        PRINT *,' NQSSRC=',NQSSRC,' (How to calculate Q^2)'
        PRINT *,'       = 0 ;  Q^2 = \hat(S)'
        PRINT *,'       = 1 ;  Q^2 = Pt^2   '
        PRINT *,' NUMFLV=',NUMFLV,' # of flavour to generate'
        PRINT *,'       = 0 ;  Nflv=5 when QSQ>500, =4 when 500>QSQ>50',
     >                       ' else 3'
        PRINT *,'       = 1 ;  Nflv=3 fixed.'
        PRINT *,'       = 2 ;  Nflv=4 fixed.'
        PRINT *,'       = 3 ;  Nflv=5 fixed.'
        PRINT *,' NDISTR=',NDISTR,' Parton distribution function '
        PRINT *,'       = 0 ; DG, select automatically',
     >                 'according to the qsquare.         '
        PRINT *,'       = 1 ; DG with Nflv=3              '
        PRINT *,'       = 2 ; DG with Nflv=4              '
        PRINT *,'       = 3 ; DG with Nflv=5              '
        PRINT *,'       = 4 ; LAC, SET-I                  '
        PRINT *,'       = 5 ; LAC, SET-II                 '
        PRINT *,'       = 6 ; LAC, SET-III                '
        PRINT *,'       = 7 ; DO                          '
        PRINT *,'       = 8 ; DO + VMD                    '
        PRINT *,'       = 9 ; Modified DO + VMD           '
        PRINT *,' NMASQ =',NMASQ, ' treat massive quark in hard process'
        PRINT *,'       = 0 ; all quarks are mass less.   '
        PRINT *,'       = 1 ; taken account c/b quark mass'
        PRINT *,'       = 2 ; taken account all quark mass'
        PRINT *,' NGMINS=',NGMINS,'Photon source from e- beam',
     >           '(0=Bremstrulung, 1=Beamstrulung)'
        PRINT *,' NGPLUS=',NGPLUS,'Photon source from e+ beam',
     >           '(0=Bremstrulung, 1=Beamstrulung)'
        PRINT *,' XLAM  =',XLAM,'(GeV^2) Lambda value to calculate ',
     >                          ' Parton density function.'
        PRINT *,' XXLAM =',XXLAM,'(GeV^2) Lambda value to calculate ',
     >                          ' Alpha_S'
        PRINT *,' NLAM  =',NLAM,' treatment of LAMDA  in hard scatering'
        PRINT *,'       = 0 ; fix to XXLAM   for all flavour'
        PRINT *,'       = 1 ; use XXLAM values for Nf= 4  and    '
        PRINT *,'             changes for Nf=3,5 so that alpha becomes'
        PRINT *,'             continuous.                             '
        PRINT *,' YMAX3 =',YMAX3,'Rapidity max for jet-3'
        PRINT *,' YMAX4 =',YMAX4,'Rapidity max for jet-4'

CC      PRINT *,' SEED for random variable.=',NSEED
C*
      RETURN
      END
