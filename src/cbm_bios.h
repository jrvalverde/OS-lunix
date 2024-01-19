;// CBM BIOS defines (of Commodore 64)


;// bios zeropage usage

#define  bios_status     $90
#define  bios_openfiles  $98
#define  bios_stdin      $99
#define  bios_stdout     $9a
#define  bios_fnamelen   $b7
#define  bios_logfnr     $b8
#define  bios_secadr     $b9
#define  bios_devicenum  $ba
#define  bios_fname      $bb
#define  bios_tablogfnr  $259
#define  bios_tabdevnum  $263
#define  bios_tabsecadr  $26d

;// bios constants

#define  bios_maxfiles  10

;// bios routines

#define  bios_setfls     $ffba
#define  bios_setnam     $ffbd
#define  bios_open       $ffc0
#define  bios_close      $ffc3
#define  bios_chkin      $ffc6
#define  bios_chkout     $ffc9
#define  bios_clrch      $ffcc
#define  bios_input      $ffcf
#define  bios_print      $ffd2
#define  bios_load       $ffd5
#define  bios_save       $ffd8
#define  bios_get        $ffe4
#define  bios_clall      $ffe7
