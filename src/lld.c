/* 6502/10 linker (lld) Version 0.05

   Written by Daniel Dallmann (aka Poldi) in Sep 1996.
   This piece of software is freeware ! So DO NOT sell it !!
   You may copy and redistribute this file for free, but only with this header
   unchanged in it. (Or you risc eternity in hell)

   If you've noticed a bug or created an additional feature, let me know.
   My (internet) email-address is Daniel.Dallmann@studbox.uni-stuttgart.de

   Jun 21 1997 *poldi*  added: environment variable that holds a list of
                               labrarys that are included per default.
                        added: flag for quiet operation
                        fixed: duplicated global error message

   Jun 15 1997 *poldi*  fixed: bug in library creation code.

   Dec 15 1996 *poldi*  added support of LUnix-code

   ...
*/

#include <stdio.h>
#include <stdlib.h>

#undef debug

#define USE_GETENV              /* use LLD_LIBRARYS to find librarys */
#define PATH_SEPARATOR ':'      /* character used as path separator  */

  /* export LLD_LIBRARYS=/usr/lib/c64:/usr/lib/lunix
                                     ^        ^ dir. separator
                               path separator

  NOTE: LLD_LIBRARYS  used for normal objectfiles
        LUNA_LIBRARYS used for lunix objectfiles
  */

#define files_max 50        /* max number of files to link */
#define globals_max 500     /* max number of globals in an objectfiles */
#define label_length_max 40 /* max length of label-names */
#define lib_num_max 4       /* max number of librarys to link against */
#define mod_num_max files_max /* max number of objects in a library */

#define nothing (-1)

static char *global[globals_max];
static char *unknown[globals_max];
static int  glob_val[globals_max];
static int  global_num;
static int  unknown_num;
static int  errors;
static char str[150];

static char *infile[files_max];
static int  infile_num;
static char *file_output;

static char  *code_buffer;
static int  code_buffer_ptr;
static int  code_buffer_length;
static int  quiet_mode;

void error(char *text)
{
  printf("  error:%s\n",text);
  errors++;
}

void derror(char *text)
{
  printf("  panic:%s\n",text);
  exit(1);
}

int search_global(char *name)
{
  int i;

# ifdef debug
  printf("# search global \"%s\"\n",name);
# endif

  i=0;
  while (i<global_num) {
    if (!strcmp(global[i],name)) return i;
    i++; }

  return nothing;
}

void ill_object(char *name)
{
  sprintf(str,"%s: illegal object format",name);
  error(str);
  exit(1);
}

void ill_library(char *name)
{
  sprintf(str,"%s: illegal library format",name);
  error(str);
  exit(1);
}

int search_unknown(char *name)
{
  int i;

# ifdef debug
  printf("# search unknown \"%s\"\n",name);
# endif

  i=0;
  while (i<unknown_num) {
    if (!strcmp(unknown[i],name)) return i;
    i++; }

  return nothing;
}

void remove_unknown(i)
{

# ifdef debug
  printf("# remove unknown \"%s\"\n",unknown[i]);
# endif

  free(unknown[i]);
  unknown_num--;
  while (i<unknown_num) {
    unknown[i]=unknown[i+1];
    i++; }
}

char *str_sav(char *string)
{
  char *tmpstr;

  tmpstr=(char*)malloc(strlen(string)+1);
  if (tmpstr==NULL) {
    error("out of memory");
    exit(1); }

  strcpy(tmpstr,string);
  return tmpstr;
}

void add_global(char *name,int val)
{
  int i;
  char *tmpstr;

# ifdef debug
  printf("# add global \"%s\"=%i\n",name,val);
# endif

  if (global_num>=globals_max) {
    error("too many globals");
    exit(1); }

  i=search_global(name);
  if (i!=nothing) {
    sprintf(str,"duplicated label \"%s\"",name);
    error(str);
    return; }

  i=search_unknown(name);
  if (i!=nothing) remove_unknown(i);

  global[global_num]=str_sav(name);
  glob_val[global_num]=val;
  global_num++;
}  

void add_unknown(char *name)
{
  int i;
  char *tmpstr;

# ifdef debug
  printf("# add unknown \"%s\"\n",name);
# endif

  if (unknown_num>=globals_max) {
    error("too many globals");
    exit(1); }

  i=search_unknown(name);
  if (i!=nothing) return;

  unknown[unknown_num]=str_sav(name);
  unknown_num++; 
}

int read_byte(FILE *stream, char *name)
{
  static int tmp;

  tmp=fgetc(stream);
  if (tmp==EOF) {
    sprintf(str,"unexpected EOF reading \"%s\"",name);
    error(str);
    exit(1); }

  return tmp;
}

void write_byte(FILE *stream, char byte)
{
  if (fputc(byte,stream)==EOF) {
    error("i/o-error while writing to outfile");
    exit(1); }
}

void make_code(int flags, int val, char *fname)
{
  if (code_buffer_ptr+2>=code_buffer_length) 
    printf("%i of %i !\n",code_buffer_ptr,code_buffer_length); /*ill_object(fname);*/

  if ((flags&0x03)==0x03) {
    /* put word */
    code_buffer[code_buffer_ptr++]=val & 0xff;
    code_buffer[code_buffer_ptr++]=(val>>8) & 0xff; }

  else if (flags&0x01) {
    /* only put low byte */
    code_buffer[code_buffer_ptr++]=val & 0xff; }

  else if (flags&0x02) {
    /* only put high byte */
    code_buffer[code_buffer_ptr++]=(val>>8)&0xff; }

  else ill_object(fname);

  return;
}

FILE *open_ext(char *file, int *size)
{
  FILE *inf;

  inf=fopen(file,"r");
  if (inf==NULL) {
    sprintf(str,"can't open inputfile \"%s\"",file);
    error(str);
    exit(1); }

  read_byte(inf,file);
  read_byte(inf,file);
  read_byte(inf,file);

  /* skip globals */

  while (read_byte(inf,file)!=0) {
    while (read_byte(inf,file)!=0) ;
    read_byte(inf,file);
    read_byte(inf,file); }

  /* skip length of module-code */

  *size=read_byte(inf,file)+(read_byte(inf,file)<<8);

  return inf;
}

void make_lib()
{
  int i,j,f_num;
  FILE *outf, *inf;
  char *fname;
  int  tmp;
  int  size;

  outf=fopen(file_output,"w");
  if (outf==NULL) {
    error("can't write to output-file");
    exit(1); }

  write_byte(outf,'l');
  write_byte(outf,'i');
  write_byte(outf,'b');

  f_num=0;
  while (f_num<infile_num) {
    fname=infile[f_num];

    inf=fopen(fname,"r");
    if (inf==NULL) {
      sprintf(str,"can't open inputfile \"%s\"",infile[f_num]);
      error(str);
      exit(1); }

    tmp= (read_byte(inf,fname)!='o'); 
    tmp|=(read_byte(inf,fname)!='b');
    tmp|=(read_byte(inf,fname)!='j');
    if (tmp) ill_object(fname);

    /* add globals of module to archive */

    while ((tmp=read_byte(inf,fname))!=0) {
      write_byte(outf,tmp);
      while ((tmp=read_byte(inf,fname))!=0) write_byte(outf,tmp);
      write_byte(outf,0);
      write_byte(outf,read_byte(inf,fname));
      write_byte(outf,read_byte(inf,fname)); }
    write_byte(outf,0);

    /* add length of module-code */

    write_byte(outf,read_byte(inf,fname));
    write_byte(outf,read_byte(inf,fname));

    /* add externals of module */

    while ((tmp=read_byte(inf,fname))!=0) {
      write_byte(outf,tmp);
      while ((tmp=read_byte(inf,fname))!=0) write_byte(outf,tmp);
      write_byte(outf,0); }
    write_byte(outf,0);

    fclose(inf);
    f_num++; }

  write_byte(outf,1); /* end mark */

  f_num=0;
  while (f_num<infile_num) {
    fname=infile[f_num];

    inf=open_ext(fname,&size);

    /* skip externals of module */

    /* add externals of module a seconds time */

    while ((tmp=read_byte(inf,fname))!=0) {
      write_byte(outf,tmp);
      while ((tmp=read_byte(inf,fname))!=0) write_byte(outf,tmp);
      write_byte(outf,0); }
    write_byte(outf,0);

    /* add code of module to archive */

    while ((tmp=fgetc(inf))!=0) {
      write_byte(outf,tmp);
      if (tmp==0) {
        if (fgetc(inf)!=EOF) ill_object(fname);
	    break; }
      if (tmp<0x80) {
        while (tmp!=0) {
          write_byte(outf,read_byte(inf,fname));
          tmp--; } }
      else if (((tmp&0xf0)==0x80) || ((tmp&0xf0)==0xc0)) {
        write_byte(outf,read_byte(inf,fname));
        write_byte(outf,read_byte(inf,fname)); }
      else if ((tmp&0xf0)==0xd0) {
        write_byte(outf,read_byte(inf,fname));
        write_byte(outf,read_byte(inf,fname));
        write_byte(outf,read_byte(inf,fname));
        write_byte(outf,read_byte(inf,fname)); }
      else ill_object(fname);
	  }

    write_byte(outf,0);
    fclose(inf);
    if (!quiet_mode) printf("  \"%s\" added.\n",fname);
    f_num++; }
  if (!quiet_mode) printf("done.\n");
}

void add_code(FILE *inf, char *fname, int pc)
{
  int  tmp,i,j;
  int  lab_map[globals_max];
  int  map_size;
  char tmpname[label_length_max];

  /* read list of externals and create remap-table */

  map_size=0;
  while ((tmp=read_byte(inf,fname))!=0) {
    j=1;
    tmpname[0]=tmp;
    while ((tmpname[j]=read_byte(inf,fname))!=0) j++;
    lab_map[map_size++]=search_global(tmpname); }

  /* relocate code */

# ifdef debug
  printf("adding code of \"%s\" at %i (bufferspace %i bytes)\n",fname,pc,code_buffer_length);
# endif


  while ((tmp=fgetc(inf))!=EOF) {
    if (tmp==0) {
      /* end of code mark */
      break; }
    else if (tmp<0x80) {
      /* code fragment without change */
      if (code_buffer_ptr+tmp>=code_buffer_length) 
        ill_object(fname);
      while (tmp!=0) {
        code_buffer[code_buffer_ptr++]=read_byte(inf,fname);
        tmp--; } }
    else if ((tmp&0xf0)==0x80) {
      /* normal relocation */
      i=read_byte(inf,fname)|(read_byte(inf,fname)<<8);
      make_code(tmp,i+pc,fname); }
    else if ((tmp&0xf0)==0xc0) {
      /* insert value of external */
      i=read_byte(inf,fname)|(read_byte(inf,fname)<<8);
      if (i<0 || i>map_size) {
        printf("no. of external out of range\n");
        ill_object(fname); }
      make_code(tmp,glob_val[lab_map[i]],fname); }
    else if ((tmp&0xf0)==0xd0) {
      /* insert modified value of external */
      i=read_byte(inf,fname)|(read_byte(inf,fname)<<8);
      j=read_byte(inf,fname)|(read_byte(inf,fname)<<8);
      if (j&0x8000) j-=0x10000;
      if (i<0 || i>map_size) {
        printf("no. of external out of range\n");
        ill_object(fname); }
      make_code(tmp,glob_val[lab_map[i]]+j,fname); }
    else {
      printf("prefix %i unknown\n",tmp);
      ill_object(fname); }
  }
}

void write_buffer(FILE *outf)
{
  int i;

# ifdef debug
  printf("writing buffer (%i/%i bytes)\n",code_buffer_ptr,code_buffer_length);
# endif  
  i=0;
  while (i<code_buffer_ptr) write_byte(outf,code_buffer[i++]);
  code_buffer_ptr=0;
}

void Howto()
{
  printf("lld [-lLoqs] input-files...\n");
  printf("  linker for objectcode created by 'luna' 6502/10 assembler\n");
  printf("  this is version 0.05\n");
  printf("  -l library-file\n");
  printf("  -L = create library instead of executable\n");
  printf("  -o output-file (default is c64.out or c64.lib)\n");
  printf("  -q quiet operation\n");
  printf("  -s address (start address in decimals, default 4096)\n");
#ifdef USE_GETENV
  printf(" environment\n");
  printf("  LLD_LIBRARYS ':' separated list of standard librarys\n");
  printf("  LUNIX_LIBRARYS same list but used in lunix-mode\n");
#endif
  exit(1);
}

void main(int argc, char **argv)
{
  int           i,j,flag,need_flag;
  unsigned int  pc;
  int           f_num;
  FILE          *inf;
  FILE          *outf;
  int           tmp;
  char          *fname;
  unsigned int  size;
  char          tmpname[label_length_max];
  int           pc_start;
  int           pc_end;

  int           lib_flag;
  char          *lib[lib_num_max];
  int           lib_num;
  int           *mod_flag[lib_num_max];
  int           *lib_clen[lib_num_max];
  int           lib_size[lib_num_max];

  int           solved_flag;
  int           mod_num;
  int           *mod__flag;
  int           mod_cnt;
  int           lunix_mode;

  char          *envtmp;

  lib_num=infile_num=errors=global_num=unknown_num=0;
  pc=0x1000; /* default value, also for LUnix-executables */

  lunix_mode=quiet_mode=0;
  i=1; lib_flag=0;
  while ( i<argc ) {
    if (argv[i][0]=='-') {
      j=1;
      while (argv[i][j]!='\0') {
        switch (argv[i][j]) {
          case 'o': { i++; file_output=argv[i]; j=0; break; }
          case 'L': { lib_flag=1; break; }
          case 'q': { quiet_mode=1; break; }
          case 'l': { 
            i++;
            if (lib_num>=lib_num_max) {
              error("too many librarys");
              exit(1); }
            lib[lib_num]=argv[i];
            lib_num++;
            j=0; break; }
          case 's': { 
            i++; 
            if (sscanf(argv[i],"%i",&pc)==0) Howto();
            j=0; break; }
          default:  Howto();
          }
        if (i==argc) Howto();
        if (j==0) break;
        j++; }
	} else {
      if (infile_num>=files_max) {
        error("too many inputfiles");
        exit(1); }
      infile[infile_num]=argv[i];
      infile_num++; }
    i++; }

  if (infile_num==0) { printf("%s: No input file\n",argv[0]); exit(1); }
  if (file_output==NULL) 
    if (!lib_flag) file_output="c64.out"; 
    else {
      file_output="c64.lib"; }

  if (lib_flag) {
    if (lib_num!=0) {
      error("can't create library including librarys");
      exit(1); }
    make_lib();
    exit(0); }

  if (pc>0xffff) {
    error("illegal address");
    exit(1); }

  pc_start=pc;

  /* first built up database */

  f_num=0;
  while (f_num<infile_num) {
    fname=infile[f_num];

#   ifdef debug
    printf("# processing file \"%s\", pc=%i\n",fname,pc);
#   endif

    inf=fopen(fname,"r");
    if (inf==NULL) {
      sprintf(str,"can't open inputfile \"%s\"",infile[f_num]);
      error(str);
      exit(1); }

    tmp= read_byte(inf,fname);
    if (tmp=='O') {
      lunix_mode=1;
      if ((pc & 0x00ff)!=0) {
        error("illegal address");
        exit(1); }
      tmp=0;
      if (f_num!=0) error("LUnix-object must be first file"); }
     else 
      tmp=(tmp!='o');
    tmp|=(read_byte(inf,fname)!='b');
    tmp|=(read_byte(inf,fname)!='j');
    if (tmp) ill_object(fname);
    
    /* read list of globals and their value */

    while ((tmp=read_byte(inf,fname))!=0) {
      j=1;
      tmpname[0]=tmp;
      while ((tmpname[j]=read_byte(inf,fname))!=0) {
        j++;
        if (j>=label_length_max) {
          error("label too long");
          exit(1); } }
      tmp=read_byte(inf,fname)+(read_byte(inf,fname)<<8);
      add_global(tmpname,tmp+pc);
      }

     size=read_byte(inf,fname)+(read_byte(inf,fname)<<8);
     
#    ifdef debug
     printf("# size is %i\n",size);
#    endif

     /* read list of externals */

    while ((tmp=read_byte(inf,fname))!=0) {
      j=1;
      tmpname[0]=tmp;
      while ((tmpname[j]=read_byte(inf,fname))!=0) {
        j++;
        if (j>=label_length_max) {
          error("label too long");
          exit(1); } }
      if (search_global(tmpname)==nothing) add_unknown(tmpname);
      }

    fclose(inf);
    
    pc+=size;
    if (pc>0xffff) {
      error("code crossed 64k border");
      exit(1); }

  f_num++; }

#ifdef USE_GETENV
  /* add librays specified by environment variable */

  if (lunix_mode)
    envtmp=getenv("LUNIX_LIBRARYS");
  else
    envtmp=getenv("LLD_LIBRARYS");

  if (envtmp!=NULL) {
    char *tmpname;

    while (1) {
      i=0;
      while (envtmp[i]!='\0' && envtmp[i]!=PATH_SEPARATOR) i++;
      if (lib_num>=lib_num_max) {
        error("too many librarys\n");
        exit(1); }
      tmpname=(char*)malloc(sizeof(char)*(i+1));
      strncpy(tmpname,envtmp,i);
      tmpname[i]='\0';
      lib[lib_num++]=tmpname;
      if (envtmp[i]!='\0') envtmp=&envtmp[i+1];
      else break; } }
#endif

  /* now get symbols from librarys */

  f_num=0;
  while (f_num<lib_num) {
    fname=lib[f_num];

#   ifdef debug
    printf("# processing library \"%s\", pc=%i\n",fname,pc);
#   endif

    /* find out what modules of this library must be included */

    mod__flag=(int*) malloc(sizeof(int)*mod_num_max);
    lib_clen[f_num]=(int*) malloc(sizeof(int)*mod_num_max);

    if (mod__flag==NULL || lib_clen[f_num]==NULL) {
      error("out of memory");
      exit(1); }

    mod_num=0; i=0;
    solved_flag=1;
    while (solved_flag) {
      solved_flag=0;

#     ifdef debug
      printf("\n# next pass... i=%i\n",i);
#     endif

      inf=fopen(fname,"r");
      if (inf==NULL) {
        sprintf(str,"can't open library \"%s\"",fname);
        error(str);
        exit(1); }

      tmp= (read_byte(inf,fname)!='l'); 
      tmp|=(read_byte(inf,fname)!='i');
      tmp|=(read_byte(inf,fname)!='b');
      if (tmp) ill_library(fname);
    
      /* read list of globals and their value */

      mod_cnt=0;
      while ((tmp=read_byte(inf,fname))!=1) {
        if (i==0) {
          mod__flag[mod_num]=0;
          mod_num++; }
        flag=need_flag=0;
        while (tmp!=0) {
          j=1;
          tmpname[0]=tmp;
          while ((tmpname[j]=read_byte(inf,fname))!=0) {
            j++;
            if (j>=label_length_max) {
            error("label too long");
            exit(1); } }
          tmp=read_byte(inf,fname)+(read_byte(inf,fname)<<8);
          if(search_unknown(tmpname)!=nothing) {
            need_flag=1;
            if (mod__flag[mod_cnt]==0) flag=1;
            if (i==-1)
              add_global(tmpname,pc+tmp); }
          else {
            if (i==-1 && search_global(tmpname)!=nothing) {
              char message[200];
              sprintf(message,"duplicated global \"%s\"",tmpname);
              error(message); }}
          tmp=read_byte(inf,fname); }
        tmp=read_byte(inf,fname)+(read_byte(inf,fname)<<8);
        lib_clen[f_num][mod_cnt]=tmp;
        if (i==-1 && need_flag) {
          pc+=tmp;
          if (pc>0xffff) {
            error("code crossed 64k border");
            exit(1); } }
        if (flag) {
          /* okay, we need this module, so add its unknowns ! */
#         ifdef debug
          printf("# need module %i\n",mod_cnt);
#         endif
          mod__flag[mod_cnt]=1;
          solved_flag=1;
          while ((tmp=read_byte(inf,fname))!=0) {
            j=1;
            tmpname[0]=tmp;
            while ((tmpname[j]=read_byte(inf,fname))!=0) {
              j++;
              if (j>=label_length_max) {
              error("label too long");
              exit(1); } }
            if (search_global(tmpname)==nothing) add_unknown(tmpname); }
          }
        else {
          while (read_byte(inf,fname)!=0)
            while (read_byte(inf,fname)!=0) ; }

        mod_cnt++; }
      i++;
      if (!solved_flag) if (i!=0) { i=-1; solved_flag=1; }
	  }

    mod_flag[f_num]=mod__flag;
    lib_size[f_num]=mod_num;
    f_num++; }

  /* are there still unresolved labels ? ... */

# ifdef debug
  printf("\n");
# endif

  i=0;
  while (i<unknown_num) {
    sprintf(str,"undefined reference to \"%s\"",unknown[i]);
    error(str);
    i++; }

  if (errors) {
    printf("summary: %i error(s), linking stopped\n",errors);
    exit(1); }

# ifdef debug
  printf("# second pass...\n\n");
# endif

  outf=fopen(file_output,"w");
  if (outf==NULL) {
    sprintf(str,"can't open outputfile \"%s\"",file_output);
    error(str); }

  pc_end=pc;
  pc=pc_start;
  if (!lunix_mode) {
    write_byte(outf,pc & 0xff);
    write_byte(outf,(pc>>8)&0xff); /* put start address */ }
  else {
    write_byte(outf,0xff);
    write_byte(outf,0xff); /* put LUnix-magic $ffff */
    pc_end++; /* have to add $02=endofcode-marker */ }

  f_num=0;
  while (f_num<infile_num) {
    fname=infile[f_num];

#   ifdef debug
    printf("# processing file \"%s\", pc=%i\n",fname,pc);
#   endif

    inf=open_ext(fname,&size);
     
#   ifdef debug
    printf("# size is %i\n",size);
#   endif

    code_buffer_length=size+2;
    code_buffer=malloc( (size_t) code_buffer_length);

    if (code_buffer==NULL) derror("out of memory");

    add_code(inf,fname,pc);
    if (getc(inf)!=EOF) ill_object(fname);

    if (lunix_mode && f_num==0) {
      /* have to adapt something in the header ! */
      code_buffer[65]=pc_start>>8;
      code_buffer[2]=1+((pc_end-pc_start-1)>>8);
#     ifdef debug
      printf("  lunix base page = %i\n",code_buffer[65]);
      printf("  lunix code length = %i pages (%i bytes)\n",code_buffer[2],pc_end-pc_start);
#     endif
      }

    write_buffer(outf);
    free(code_buffer);

    fclose(inf);
    
    pc+=size;

# ifdef debug
  printf("\n");
# endif

  f_num++; }

  /* include stuff from librarys */

  f_num=0;
  while (f_num<lib_num) {
    fname=lib[f_num];

#   ifdef debug
    printf("# processing library \"%s\", pc=%i\n",fname,pc);
#   endif

    mod_num=0; i=0;

    inf=fopen(fname,"r");
    if (inf==NULL) {
      sprintf(str,"can't open library \"%s\"",infile[f_num]);
      error(str);
      exit(1); }

    read_byte(inf,fname);
    read_byte(inf,fname);
    read_byte(inf,fname);
    
    /* skip big lib-header */

    while ((tmp=read_byte(inf,fname))!=1) {
      while (tmp!=0) {
        while ((tmpname[j]=read_byte(inf,fname))!=0) ;
        read_byte(inf,fname);
        read_byte(inf,fname);
        tmp=read_byte(inf,fname); }
      read_byte(inf,fname);
      read_byte(inf,fname);
      while (read_byte(inf,fname)!=0)
        while (read_byte(inf,fname)!=0) ; }

    /* add modules we need */

    mod__flag=mod_flag[f_num];
    mod_cnt=0;
    while (mod_cnt<lib_size[f_num]) {
      if (mod__flag[mod_cnt]) {
        code_buffer_length=lib_clen[f_num][mod_cnt]+2;
#       ifdef debug
        printf("# including module %i at pc=%i, %i bytes\n",mod_cnt,pc,code_buffer_length);
#       endif
        code_buffer=malloc(code_buffer_length);
        if (code_buffer==NULL) derror("out of memory");
        add_code(inf,fname,pc);
        write_buffer(outf);
        free(code_buffer);
        pc+=lib_clen[f_num][mod_cnt]; }
      else {
        /* skip externals */
        while (read_byte(inf,fname)!=0)
          while (read_byte(inf,fname)!=0) ;
        /* skip code of module */
        while ((tmp=fgetc(inf))!=EOF) {
          if (tmp==0) break;
          if (tmp<0x80) {
            while (tmp!=0) {
              read_byte(inf,fname);
              tmp--; } }
          else if (((tmp&0xf0)==0x80) || ((tmp&0xf0)==0xc0)) {
            read_byte(inf,fname);
            read_byte(inf,fname); }
          else if ((tmp&0xf0)==0xd0) {
            read_byte(inf,fname);
            read_byte(inf,fname);
            read_byte(inf,fname);
            read_byte(inf,fname); }
          else ill_object(fname);
	      }
        if (tmp!=0) ill_library(fname); }
      mod_cnt++; }

    fclose(inf);
    free(mod__flag);
    free(lib_clen[f_num]);
    f_num++; }

  write_byte(outf,0x02); /* add endofcode-marker */

  fclose(outf);
  if (!quiet_mode) printf("done, %i bytes of code\n",pc-pc_start);
  exit(0);
}







