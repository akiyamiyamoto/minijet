C***********************************************************************
C      MAIN PROGRAM FOR SPRING
C
C    To generate events of the process
C      e+e- --> mini-jet.
C$Id: spmain.F,v 1.1 2004/07/01 00:41:33 miyamoto Exp $
C***********************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
      EXTERNAL FNCMNJ
      REAL*8 estim, sgima, ctime
      INTEGER*4  it1, it2
      REAL*4 STIME
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF

      include 'mnjfili.inc'

************************************************************************
*               initialization of BASES/SPRING 5.1
************************************************************************
*=======================================================================
*          initialization of bases by calling bsinit
*=======================================================================
*     -------------
      call bsinit
*     -------------
*     ------------------
      call userin
*     ------------------
*=======================================================================
*          read the probability information from the file
*=======================================================================
      lun = 23
      open(lun,file=basesresult(1:lnblnk(basesresult)),
     >     status='old',form='unformatted')
*     -------------
      call bsread( lun )
      read (lun) AVGI,SD,CHI2A,STIME,ITG,ITF

*     -------------
      close( lun )

*=======================================================================
*      initialization of parameters
*          for kinematics and matrix elements
*      initialization of histograms
*=======================================================================
      lunevt = 0
      lunrun = 0     
      if ( eventfile(1:4) .ne. 'NONE' ) then
        lunevt   = 14
        open(lunevt,file=eventfile(1:lnblnk(eventfile)),
     >     status='unknown',form='formatted')
        lunrun   = 15
        open(lunrun,file=runfile(1:lnblnk(runfile)),
     >     status='unknown',form='formatted')
        call mnjwrun(lunrun)
        close(lunrun)
      endif

*=======================================================================
*     initialization of additional histograms for spring
*=======================================================================

*=======================================================================
*     event generation
*=======================================================================

      mxtry  = 50
      mxevnt = 1000
      mxevnt=numgenevt


      do 100 nevnt = 1, mxevnt
         call spring( fncmnj, mxtry )

         call spevnt
*-----------------------------------------------------------------------
*        compute the four vectors of generated event
*          from the kinematical variables
*-----------------------------------------------------------------------

  100 continue

      lu=6
      call spinfo( lu )
      call shplot( lu )

      if ( lunevt .ne. 0 ) then
        close( lunevt )
      endif

      print *,'Normal end of spring'
      close( lu )
      stop
      end

C
C***********************************************************************
C*===================                                                  *
C* Subroutine SPINIT                                                   *
C*===================                                                  *
C*                                                                     *
C*     This routine is called at Run initialization.                   *
C*     Store Run conditions into Begin_Run records.                    *
C*                                                                     *
C*   Coded by A. Miyamoto  28-Aug-1990                                 *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE SPINIT


c      COMMON / LUDAT3 /   DPAR ( 20 ), IDB ( 120 ), CBR ( 400 ),
c     #                    KDP (1600 )
C
C =====< Entry Point >=================================================
C
C ---------------------------------------------------------------------
C (1) Set seed for event generation.
C ---------------------------------------------------------------------
C
C      LUSEED = 141421356
C      CALL LRNSET(LUSEED)
C
C --------------------------
C (2) Parameters for LUND fragmentation.
C     SET Short lived particles to stable.
C     K0s = 37, K0l=38, sigma+=43,sigma0=44, sigma-=45, Lambda=57
C --------------------------
c        IDB(38) = 0
c        IDB(37) = 0
c        IDB(38) = 0
c        IDB(43) = 0
c        IDB(44) = 0
c        IDB(45) = 0
c        IDB(46) = 0
c        IDB(57) = 0


 
      RETURN
      END


C***********************************************************************
C*===================
C* Subroutine MNJWRUN(lrun)
C*===================
C*(Function)
C*     Write Run information
C*(Input)
C*     lrun : Output logical unit number
C*
C*   Coded by A. Miyamoto  29-June-2004 
C*
C***********************************************************************
      subroutine mnjwrun(lrun)

      implicit real*8(a-h,o-z)
      include 'mnjevti.inc'

      REAL*4 STIME
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF

      real*8 ebmup(2)

      ebmup(1)=ebeam 
      ebmup(2)=ebeam

c ..                idbmup1, idbmup2
      write(lrun,*) '11 -11',ebmup(1),ebmup(2), 
c...   pdfup1, pdfup2, pdfsup1,pdfsup2
     >  ' -1 -1 -1 -1 ',
c...   idwtup nprup
     > ' 3  1 '
c      Output as 2nd record
c...   cross section is in unit of pb
c...   xsecup, xerrup, xmaxup, lprup  

      write(lrun,*) AVGI,' ',SD,' ',
     > (AVGI+SD),' 3001 '

      return
      end


C***********************************************************************
C*                                                                     *
C*===================                                                  *
C* Subroutine SPEVNT                                                   *
C*===================                                                  *
C*                                                                     *
C*(Purpose)                                                            *
C*   To enerate One event.                                             *
C*(Author)                                                             *
C*   Coded by A. Miyamoto 28-Aug-1990                                  *
C*   A.Miyamnoto  14-Aug-1992  Get Seed from common.                   *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE SPEVNT
C
      INCLUDE "mnjevti.inc"
      include 'mnjfili.inc'
C
      REAL*4  PVCT(4,20)
      REAL*4  RBUF(20,50), GBUF(20)
      REAL*8  PELE(4), PPOS(4), PMID(4)
      REAL*8  AME/0.510998902D-3/
      REAL*8  qmass, qmass2
      integer*4 icol(2)
C
      DATA  NEVENT/0/
C
C =====< Entry Point >=================================================
C
C
C ---------------------------------------------------------------------
C (1) Set Event number.
C ---------------------------------------------------------------------
C
      NEVENT = NEVENT + 1
C
C --------------------------------------------------------------------
C (2) prepare 4 momentum of decay particle.
C --------------------------------------------------------------------
C
      NRET = -1
      CALL MNJMPL( NFINAL, RBUF, GBUF, PVCT )
      IF( NFINAL .LE. 0 ) RETURN

C
C --------------------------------------------------------------------
C (3) Hadronize parton.
C --------------------------------------------------------------------
C
C      CALL MNJHAD( NFINAL, RBUF )

C --------------------------------------------------------------------
C (4) Output event information to the file
C --------------------------------------------------------------------
  
      nup=4+nfinal
      IAPRC=IABS(NPRC)

      idprup=3001
      xwgtup=1.0
      scalup=-1.0
      aqedup=-1.0
      aqcdup=-1.0

      pele(1)=0
      pele(2)=0
      pele(3)=ebeam
      pele(4)=sqrt(pele(3)*pele(3)+ame*ame)
      ppos(1)=0
      ppos(2)=0
      ppos(3)=-ebeam
      ppos(4)=sqrt(ppos(3)*ppos(3)+ame*ame)

c      write(levt,*) nup, idprup, xwgtup,
c     >     scalup, aqedup, aqcdup
      write(lunevt,400) nup, idprup, xwgtup,
     >     scalup, aqedup, aqcdup
 400  format(2i6,4G16.10)


c     write parton momentum
      write(lunevt, 510) (pele(i),i=1,4),ame
      write(lunevt, 520) (ppos(i),i=1,4),ame
510   format(' 11 -1 0 0 0 0 ',5G17.9,' 0.0 0.0')
520   format('-11 -1 0 0 0 0 ',5G17.9,' 0.0 0.0')

C .. e- emits photon, e+ emits quark 
c      IF( IAPRC.EQ.2 ) THEN
        pele(4)=ebeam-egam10
        pele(3)=sqrt((pele(4)-ame)*(pele(4)+ame))
        ppos(4)=ebeam-egam20
        ppos(3)=-sqrt((ppos(4)-ame)*(ppos(4)+ame))
        write(lunevt,610) (pele(i),i=1,4),ame
        write(lunevt,620) (ppos(i),i=1,4),ame
610   format(' 11  1 1 2 0 0 ',5G17.9,' 0.0 0.0')
620   format('-11  1 1 2 0 0 ',5G17.9,' 0.0 0.0')
c      ENDIF 

      istup=1
      do 500 i=1, nfinal
        id=rbuf(2,i)
        qmass2=rbuf(8,i)*rbuf(8,i)-rbuf(5,i)*rbuf(5,i)
     >           -rbuf(6,i)*rbuf(6,i)-rbuf(7,i)*rbuf(7,i)
        if ( qmass2 .gt. 0.0 ) then
             qmass=sqrt(qmass2)
        else 
             qmass=0.0
        endif
        icol(1)=rbuf(15,i)
        icol(2)=rbuf(16,i)
        write(lunevt,550) id, istup, icol(1), icol(2),
     >    (rbuf(k,i),k=5,8),  qmass
550     format(i8,i4,' 1 2 ',2i5,' ',5g17.9,' 0.0 0.0 ')
500   continue

      RETURN
      END
C***********************************************************************
C*===================                                                  *
C* SUBROUTINE MNJSPE                                                   *
C*===================                                                  *
C*       By this subroutine, make termination of the Data_Bank_System  *
C*       and Handypack etc.                                            *
C*       When you don't need such routines, keep this just dummy.      *
C*                                                                     *
C*       Coded by S.Kawabata        September '84                      *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MNJSPE
C
C
C      CALL LRNGET(ILUSED)
C      WRITE(6,100)  ILUSED
C100   FORMAT(1H1,' ------------------------------------------------'/
C     +        '   FINAL SEED VALUE ',I12/
C     +        ' -------------------------------------------------')
      RETURN
      END
