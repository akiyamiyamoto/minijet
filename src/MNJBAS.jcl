//XXXXBS   JOB CLASS=M,REGION=6500K                                             
//*********************************************************************         
//*                                                                             
//*  BASES for ee --> Two photon resonance.                                     
//*                                                                             
//*  A. Miyamoto  15-Aug-1989  Original version.                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//JOBPROC DD DSN=T#PB.TOPAZ.PROCLIB,DISP=SHR                                    
//*                                                                             
// EXEC FORT7CLG,                                                               
//**    PARM.FORT='OPT(3),S,NOSTATIS,DEBUG(SUBCHK,ARGCHK,UNDEF)',               
//      PARM.FORT='OPT(3),NOS,NOSTATIS',                                        
//      PARM.LKED='NOMAP,LET'                                                   
//FORT.SYSINC DD DSN=TAYM.MINIJET.CPC.FORT,DISP=SHR                             
//FORT.SYSIN DD DSN=TAYM.MINIJET.CPC.FORT(BSMAIN),DISP=SHR                      
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJCRS),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJGAM),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJPRB),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJGEN),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJUIN),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJTIT),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJBLK),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(PYSTGA),DISP=SHR                               
//LKED.SYSLIB DD                                                                
//            DD DSN=MLIB.BASES50.LOAD,DISP=SHR                                 
//            DD DSN=T#PB.FORTLIB.LOAD,DISP=SHR                                 
//*                                                                             
//GO.SYSIN DD *                                                                 
     1,  1  Current loop count,   Max. Loop count                               
    -4      Print Flag                                                          
     0      Input Flag                                                          
    20.     CPU time limit in minutes                                           
//******   BASES results.                                                       
//*O.FT23F001 DD DUMMY                                                          
//GO.FT23F001 DD DISP=(NEW,CATLG),                                              
//  DSN=TAYM.@.BASES.TEST.S58GEV.DEFAULT,                                       
//  DCB=(RECFM=VBS,LRECL=23472,BLKSIZE=23476),                                  
//  SPACE=(TRK,(10,10),RLSE)                                                    
