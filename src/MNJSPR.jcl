//XXXXMNJG JOB CLASS=E,REGION=6500K                                             
//**********************************************************************        
//*                                                                             
//*  Generate e+e- --> mini-jet events                                          
//*                                                                             
//*  A.Miyamoto  @)_May-1990                                                    
//*                                                                             
//**********************************************************************        
//*                                                                             
//  EXEC  FORT7CLG,                                                             
//  PARM.FORT='OPT(3),NOS,AE,NOSTATIS',                                         
//**PARM.FORT='OPT(3),NOS,AE,NOSTATIS,DEBUG(SUBCHK,ARGCHK,UNDEF)',              
//  PARM.LKED='NOMAP'                                                           
//FORT.SYSINC DD DSN=TAYM.MINIJET.CPC.FORT,DISP=SHR                             
//FORT.SYSIN DD DSN=TAYM.MINIJET.CPC.FORT(MNJSPR),DISP=SHR                      
//**DD DSN=TAYM.MINIJET.CPC.FORT(MNJBNK),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJCRS),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJGAM),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJGEN),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJHAD),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJMPL),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJPRB),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJTIT),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJUIN),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(MNJBLK),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(PYSTGA),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(UP4MAS),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(UBSTBK),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(UBSTFD),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(UVZERO),DISP=SHR                               
//  DD DSN=TAYM.MINIJET.CPC.FORT(DRN),DISP=SHR                                  
//  DD DSN=TAYM.MINIJET.CPC.FORT(LRNSET),DISP=SHR                               
//*                                                                             
//LKED.SYSLIB DD                                                                
//            DD DSN=MLIB.BASES50.LOAD,DISP=SHR                                 
//            DD DSN=T#GN.G#LUND63.LOAD,DISP=SHR                                
//            DD DSN=T#PB.FORTLIB.LOAD,DISP=SHR                                 
//*                                                                             
//GO.FT05F001 DD *                                                              
    10000000                                                                    
    1.0                                                                         
//**                                                                            
//*                                                                     00001110
//******   BASES results to read                                                
//GO.FT23F001 DD DISP=SHR,LABEL=(,,,IN),                                        
//  DSN=TAYM.@.BASES.TEST.S58GEV.DEFAULT                                        
//*                                                                             
