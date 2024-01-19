/* LUnix-preprocessor (lupo) Version 0.17

   Written by Daniel Dallmann (aka Poldi) in Sep 1996.
   This piece of software is freeware ! So DO NOT sell it !!
   You may copy and redistribute this file for free, but only with this header
   unchanged in it. (Or you risc eternity in hell)

   If you've noticed a bug or created an additional feature, let me know.
   My (internet) email-address is Daniel.Dallmann@studbox.uni-stuttgart.de

 Oct  9 *poldi* changed: '.' now is a separator
 
 Jun 29 *poldi* fixed: nested macros
                fixed: expand marcos in remarks ??

 Jun 26 *Stefan Haubenthal*
                added: predefined: _LUPO_
                                   _DATE_ "(Month #day #year)"

 Jun 21 *poldi* added: searchpath for includes now read from environment
                       LUPO_INCLUDEPATH

 May 20 *poldi* added: -q option (quiet mode)

 May 18 *paul g-s* 
                feature: Added -l option to add a .line directive before 
		         each line of source.
			 This slows it down a litte, but means luna can give
			 the line an error was found in the original .c
		fix:     Changed error messages to be meaningful for emacs
		fix:     Added system include facility (#include <foo.h>)
		         Directories searched for includes are (in order):
			 /usr/include/c64, /usr/include/lunix, /usr/include
			 /usr/local/include/c64,
			 /usr/local/include/lunix,
			 and /usr/local/include
		bugfix:  You can now have quoted parameters in macros with
		         seperators

 May  6 *poldi* bugfix: nested macros. 

 Apr  3 *poldi* bugfix: Tabs in macro-definitions.

 Mar 21 1997 *poldi* added:  specialfunction directives
                        %%<commandlist>%%
                        with commandlist = {command{,command}+}
                        and command one of
                          next - generate next unique label
                          pcur - print current label (last generated)
                          ptop - print first label on special-stack
                          plast- print 2nd label on special-stack
                          plast[ 2..9] - print 3rd, 4, 5... label from stack
                          push - push current label on stack
                          pop  - remove top stack element
                          swap - swap upper two stack elements
                          swap[ 2..9]. swap deeper stack elements

 Dec 16 *poldi* bugfix: ";" now isn't a remark, when in a string.

 Nov  3 *poldi* bugfix: tabs now really are white spaces
                        "." may be part of macro-names.
                feature: lupo now removes remarks (prefixed by ";")
                         this can be switched off (-r option)
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#undef debug

/* Function prototypes */
int  get_expr(char *line, int *pos);
int  pushSourceFile(char *foo);
int  popSourceFile();
char *getSourceFile();
FILE *fopenSystemInclude(char *foo);

#define USE_GETENV     /* use LUPO_INCLUDEPATH to find includefiles */
#define PATH_SEPARATOR ':'      /* character used as path separator */
#define DIRECTORY_SEPARATOR '/' /* character used as dir. separator */

  /* export LUPO_INCLUDEPATH=/usr/include/c64:/usr/include/lunix
                                             ^            ^ dir. separator
                                       path separator
  */

#define TIMEFORMAT "\"(%b %d %Y) \""  /* format string for strftime() */
                                      /* used to predefine _DATE_     */
#define include_depth_max 5
#define if_depth_max 10
#define line_length_max 500
#define defines_max 300
#define macro_length_max 1000
#define parameters_max 10          /* not over 30 ! */
#define spec_lab_stack_size 20     /* max depth of stack */

#define no_match 0x6fff

static FILE *infile;
static int  line;
static int  lpos[include_depth_max-1];
static char *infilename[include_depth_max];
static FILE *infilestream[include_depth_max];
static int  infiles;
static int  errors;

static char *defs[defines_max];
static int  parcount[defines_max];
static char *repl[defines_max];
static char *mpar[parameters_max];
static int  defcount;
static int  remove_remarks;
static int  add_dotLines;

static int  spec_label_count;
static int  spec_lab_stack[spec_lab_stack_size];
static int  spec_lab_stack_ptr;

static int  quiet_mode;

/* This is where getSourceFile() builds the source pedigree string */
char sourcePedigree[8192];
/* This is where the previous source pedigree is kept 
   (for ensuring terseness of error output) */
static char oldSourcePedigree[8192];

void error(char *message)
{
  static int i;

  if ((infiles>0)&&(strcmp(oldSourcePedigree,getSourceFile())))
    {
      i=infiles;
      printf("In file \"%s\",",infilename[i]);
      i--;
      while(i>=0) {
	printf("Included from %s:%i\n",infilename[i],lpos[i]);
	i--; }
      /* Update source pedigree */
      strcpy(oldSourcePedigree,getSourceFile());
  }
  printf("%s:%i: %s\n",infilename[infiles],line,message);

  errors++;
}

void Howto() 
{
  printf("lupo [-dloqrs] file\n");
  printf("  little universal preprocessor version 0.17\n");
  printf("  -dname[=macro] - predefine macro\n");
  printf("  -l             - Insert .line directives for error tracking\n");
  printf("  -o file        - define outputfile (default lupo.out)\n");
  printf("  -q             - quiet mode\n");
  printf("  -r             - don't remove remarks\n");
  printf("  -s             - don't remove leading spaces\n");
#ifdef USE_GETENV
  printf(" environment\n");
  printf("  LUPO_INCLUDEPATH used to search for includefiles.\n");
#endif
  exit(1);
}

int nextchar(char *string, int pos)
{
  /* printf("nexchar:\"%s\"\n",&string[pos]); */

  while (string[pos]==' ' || string[pos]=='\t') pos++;
  return pos;
}

int is_sep(char a)
{
  if ( (a>='a' && a<='z') ||
       (a>='A' && a<='Z') ||
       (a>='0' && a<='9') ||
       a=='_' || a=='@' ||
       a=='ö' || a=='Ö' ||
       a=='ü' || a=='Ü' ||
       a=='ä' || a=='Ä' ||
       a=='ß' ) return 0;

  return 1;
}

#ifdef debug

void macroout(char *mac)
{
  int i;

  printf("\"");
  i=0;
  while (mac[i]!='\0') {
    if (mac[i]<32) printf("\\%i",mac[i]);
    else printf("%c",mac[i]);
    i++; }
  printf("\"\n");
}

#endif

char *str_sav(char *string)
{
  char *hlp;

# ifdef debug  
  printf("str_sav:"); macroout(string);
# endif

  hlp=(char *)malloc(strlen(string)+1);
  if (hlp==NULL) { error("out of memory"); exit(1); }
  strcpy(hlp,string);
  return hlp;
}

int readline(char *buffer)
{
  int i=0, quoted=0;
  static int tmp;
  line++;

  if (infiles<0) return EOF;

  while (1) {
    if (i==line_length_max-1) { 
      error("line too long");
      buffer[i]='\0';
      return 0; }
    tmp=getc(infile);
    if (tmp==EOF) {
      if (quoted) error("stray \\");
      if (i==0) {
        fclose(infile);
	popSourceFile(); /* remove filename from the stack for .line's */
        if (infiles==0) { infiles=-1; return EOF; }
        free(infilename[infiles]);
        infiles--;
        infile=infilestream[infiles];
        line=lpos[infiles]; }
      buffer[i]='\0';
      return 0; }
    if (tmp=='\\' && !quoted) { quoted=1; continue; }
    if (tmp=='\n') {
      if (quoted) { line++; quoted=0; continue; }
      buffer[i]='\0';
      return 0; }
    if (tmp<32 && tmp!='\t') continue;
    if (quoted) { buffer[i++]='\\'; quoted=0; }
    buffer[i++]=tmp; }
}

int match(char *string,int pos,char *pattern)
{
  int i=0;

  while (pattern[i]!='\0') {
    if (pattern[i]!=string[pos+i]) return no_match;
    i++; }

  if (!is_sep(string[pos+i])) return no_match;
  return pos+i;
}

int search_def(char *name)
{
  int i;
 
  i=0;
  while (i<defcount) {
    if (!strcmp(defs[i],name)) return i;
    i++; }

  return no_match;
}

int get_term(char *line, int *pos)
{
  int tmp;
  int hlp,hlp2;

  *pos=nextchar(line,*pos);

  if (line[*pos]=='!') {
    *pos+=1;
    return get_term(line,pos) ? 0:1 ; }

  if (line[*pos]=='(') {
    *pos+=1;
    tmp=get_expr(line,pos);
    *pos=nextchar(line,*pos);
    if (line[*pos]!=')') error("syntax");
    *pos+=1;
    return tmp; }
  
  /* or val */

  hlp=*pos;
  while (!is_sep(line[hlp])) hlp++;
  hlp2=line[hlp];
  line[hlp]='\0';
  if (search_def(&line[*pos])!=no_match) tmp=1; else tmp=0;
  line[hlp]=hlp2;
  *pos=hlp;
  return tmp; 
}

int get_sum(char *line, int *pos)
{
  int tmp;

  tmp=1;
  while(1) {
    tmp=tmp & get_term(line,pos);
    *pos=nextchar(line,*pos);
    if (line[*pos]!='&') return tmp;
    *pos+=1; }
} 

int get_expr(char *line, int *pos)
{
  int tmp;

  tmp=0;
  while(1) {
    tmp=tmp | get_sum(line,pos);
    if (line[*pos]!='|') return tmp;
    *pos+=1; }
} 

/* Special funktions look like:  %%<commlist>%%
   with commlist: {comm}{,comm}+
   with comm:    next,pcur,ptop,plast[n],push,pop,swap[n] */

int special_function(char *l)
{
  char replbuf[8];
  int  i,j,f,n;

  j=n=f=0;
  i=2;
  while (l[i]!='\0') {

    if (l[i]=='%' && l[i+1]=='%') {
      i++; 
      break; } /* end of commlist */

    while (1) {
      if ((n=match(l,i,"next"))!=no_match) {
        /* next - gernerate next label */
        spec_label_count++;
        break; }

      if ((n=match(l,i,"pcur"))!=no_match) {
        /* pcur - prints current label */
        sprintf(replbuf,"__%i",spec_label_count);
        if (f) error("more than one print command in specialfunction");
        f=-1;
        break; }

      if ((n=match(l,i,"ptop"))!=no_match) {
        /* ptop - prints top stack element */
        if (spec_lab_stack_ptr==0)
          error("specialstack underflow");
        else
          sprintf(replbuf,"__%i",spec_lab_stack[spec_lab_stack_ptr-1]);
        if (f) error("more than one print command in specialfunction");
        f=-1;
        break; }

      if ((n=match(l,i,"plast"))!=no_match) {
        /* plast - prints last [n] stack element */

        if (l[n]==' ' && l[n+1]>'0' && l[n+1]<='9') {
          j=l[n+1]-'0';
          n+=2; } else j=1;

        if (spec_lab_stack_ptr<=j) {
          error("specialstack underflow");
          strcpy(replbuf,"__null"); }
        else
          sprintf(replbuf,"__%i",spec_lab_stack[spec_lab_stack_ptr-j-1]);
        if (f) error("more than one print command in specialfunction");
        f=-1;
        break; }

      if ((n=match(l,i,"push"))!=no_match) {
        /* push - pushes current label on specialstack */
        if (spec_lab_stack_ptr>=spec_lab_stack_size) 
          error("specialstack overflow");
        else
          spec_lab_stack[spec_lab_stack_ptr++]=spec_label_count;
        break; }

      if ((n=match(l,i,"pop"))!=no_match) {
        /* pop - removes top element from stack */
        if (spec_lab_stack_ptr<1) 
          error("specialstack undeflow");
        else
          spec_lab_stack_ptr--;
        break; }

      if ((n=match(l,i,"swap"))!=no_match) {
        /* swap [n] - swaps two stack elements (n <-> n-1) */

        if (l[n]==' ' && l[n+1]>'0' && l[n+1]<='9') {
          j=l[n+1]-'0';
          n+=2; } else j=1;

        if (spec_lab_stack_ptr<=j) {
          error("specialstack underflow");
          strcpy(replbuf,"__null"); }
        else {
          int tmp;
          tmp=spec_lab_stack[spec_lab_stack_ptr-j];
          spec_lab_stack[spec_lab_stack_ptr-j]= \
                                       spec_lab_stack[spec_lab_stack_ptr-j-1];
          spec_lab_stack[spec_lab_stack_ptr-j-1]=tmp; }
        break; }

      error("unknown specialfunction");
      
      n=i;
      while (l[n]!='\0' && l[n]!=',' && l[n]!='%') n++;
      break; }

    if (l[n]==',') i=n+1; 
    else {
      i=n;
      if (l[n]!='\0' && l[n]!='%') error("\",\" expected"); }
    }

  /* remove whole specialfunction */

  if (l[i]=='\0') {
    error("unterminated specialfunction");
    l=""; }

  else {
    n=0; i++;
    while ((l[n]=l[i+n])!='\0') n++; }

  /* insert label if neccessary */

  if (f) {
    n=strlen(replbuf);
    i=strlen(l);
    while (i>=0) {
      l[i+n]=l[i];
      i--; }
    n--;
    while (n>=0) {
      l[n]=replbuf[n];
      n--; }
  }

return f;
}

void process_line(char *l)
{
  int  i,need,a,b;
  int  x,y;
  char *tmp;
  char *tmp2;
  char *tmp3;
  int  str_quote,quote;
  static char *loc_mpar[parameters_max];

# ifdef debug
  printf("processing:\"%s\"\n",l);
# endif

  str_quote=quote=0;
  i=0; x=0;
  while (l[i]!='\0') {
    if (x==0) {
      if (!quote && !str_quote) {
        if (!is_sep(l[i])) {
          x=0; y=no_match;
          while (x<defcount) {
            if ((y=match(l,i,defs[x]))!=no_match) break;
            x++; }
          if ((y!=no_match) && (parcount[x]==0 || l[y]=='(') ) {

#           ifdef debug
            printf("match found !\n");
#           endif

            need=strlen(repl[x]);
            if (parcount[x]>0) {
              int quoted=0;
              tmp3=str_sav(l);
              tmp3[y]=',';
              a=0;
              while (a<parcount[x]) {
                if (tmp3[y]==')') { error("missing parameter"); return; }
                if (tmp3[y]!=',') { error("syntax"); return; }
                tmp3[y]='\0';
                b=y=nextchar(l,y+1);
                loc_mpar[a]=&tmp3[b];
                while (tmp3[y]!='\0' && ((tmp3[y]!=',' && tmp3[y]!=')')||
                        quoted)) {
                  if (tmp3[y]=='\"')  quoted^=1; /*"*/
                  y++; }
                if (quoted) error("unterminated string");
                if (tmp3[b]=='\"' && tmp3[y-1]=='\"') { /*"*/
                  /* Strip surrounding quotes */
                  loc_mpar[a]=&tmp3[b+1];
                  tmp3[y-1]='\0'; }
                a++;
                }

              if (tmp3[y]==',')
                { error("too many parameters"); return; }
              if (tmp3[y]!=')')
                { error("syntax"); return; }
              tmp3[y]='\0';
              y++;
              a=0;
              tmp=repl[x];
              while (tmp[a]!='\0') {
                if (tmp[a]<32 && tmp[a]>1) {
                  need+=strlen(loc_mpar[tmp[a]-2])-1; }
                a++; }

#             ifdef debug         
              printf("macroparameters: ");
              a=0;
              while (a<parcount[x]) {
                printf("\"%s\" ",loc_mpar[a]);
                a++; }
              printf("\n");
#             endif

            } 
            else tmp3=NULL;
          
            if (need<y-i) {
              a=i+need; b=y;
              while ( (l[a++]=l[b++])!='\0' );
              l[a]='\0'; }

            if (need>y-i) {
              b=strlen(l); a=b+(need-y+i);
              while (b>=y) l[a--]=l[b--];  }
          
            a=0;
            tmp=repl[x];
            while (tmp[a]!='\0') {
              if (tmp[a]<32 && tmp[a]>1) {
                b=0;
                tmp2=loc_mpar[tmp[a++]-2];
                while (tmp2[b]!='\0') l[i++]=tmp2[b++]; }
              else l[i++]=tmp[a++]; }

#           ifdef debug
            printf("replaced, now:\"%s\"\n",l);
#           endif

            if (tmp3!=NULL) free(tmp3);
            x=0; }
          else x=1;  } } }
    else {
      if (is_sep(l[i])) x=0;
      }

    if (!quote && !str_quote && l[i]==';') {
      if (remove_remarks) {
        l[i]='\0';
#       ifdef debug
        printf("deleted remark\n");
#       endif
	    }
      return; }

    if (l[i]=='\"' && !quote) { /*"*/
      if (str_quote) str_quote=0; else str_quote=1; }
    if (l[i]=='\\') quote=1; else quote=0;

    i++; }
}

void process_line_spec(char *l)
{
  int  i,x;
  int  str_quote,quote;

# ifdef debug
  printf("processing_spec:\"%s\"\n",l);
# endif

  str_quote=quote=0;
  i=0; x=0;
  while (l[i]!='\0') {
    if (!quote && !str_quote) {
      if (l[i]=='%' && l[i+1]=='%')
        special_function(&l[i]);
      if (l[i]==';') return; }

    if (l[i]=='\"' && !quote) { /*"*/
      if (str_quote) str_quote=0; else str_quote=1; }
    if (l[i]=='\\') quote=1; else quote=0;

    i++; }

}

int read_macro_def(char *lbuf, int *j)
{
  int args=-1;
  int i,p;

  /* read mask, count parameters (and remember their name) */

  p=(*j);

  {
    int quoted=0;
    while ((!is_sep(lbuf[(*j)]))||quoted) 
      {
	if (lbuf[(*j)]=='\"')   /*"*/
	  {
	    quoted^=1;
	  }
	(*j)++;
      }
  }

  if (lbuf[(*j)]=='(') {
    lbuf[(*j)]=',';
    while (lbuf[i=nextchar(lbuf,(*j))]==',') {
      lbuf[(*j)]='\0';
      (*j)=nextchar(lbuf,i+1);
      if (lbuf[(*j)]=='\0') { 
        error("syntax"); return no_match; }
      if (args>=parameters_max) { 
        error("too many parameter"); return no_match; }
      mpar[++args]=&lbuf[(*j)];
      while (!is_sep(lbuf[(*j)])) (*j)++; }
    if (lbuf[i]!=')') error("syntax");
    lbuf[(*j)]='\0';
    (*j)=nextchar(lbuf,i+1); }
  else {
    if (lbuf[(*j)]!='\0') { 
      if (lbuf[(*j)]!=' ' && lbuf[(*j)]!='\t') { 
        error("syntax"); return no_match; }
      lbuf[(*j)]='\0'; 
      (*j)=nextchar(lbuf,(*j)+1); } }

  if (defcount>=defines_max) { 
    error("too many defines"); return no_match; }
  parcount[defcount]=args+1;
  if (search_def(&lbuf[p])!=no_match) { 
    error("redefined macro"); return no_match; }

  defs[defcount]=str_sav(&lbuf[p]);

# ifdef debug
  printf("new macro with %i parameter:",args+1);

  i=0;
  while (i<=args) printf(" \"%s\"",mpar[i++]);
  printf("\n");
# endif

  return args;
}

add_define(char *defname, char *replacement)
{
  defs[defcount]=defname;
  repl[defcount]=replacement;
  defcount++;
  if (defcount>=defines_max) { 
    error("too many defines"); return no_match; }
}

void main(int argc, char **argv)
{
  static int  i,j,p,tmp1,tmp2;
  static char lbuf[line_length_max];
  static char buf[100];
  static int  quiet_mode;
  static char *file_input;
  static char *file_output;
  static char *tmpstr;
  static FILE *outfile;
  static int  disable,depth;
  static int  if_flag[if_depth_max];
  static int  remove_spaces=1;
  static time_t bin_time;
  static struct tm *timeptr;

  file_input=file_output=NULL;
  quiet_mode=errors=disable=depth=0;
  if_flag[depth]=no_match;
  defcount=0;
  remove_remarks=1;
  add_dotLines=0;

  i=1;
  while ( i<argc ) {
    if (argv[i][0]=='-') {
      j=1;
      while (argv[i][j]!='\0') {
        switch (argv[i][j]) {
          case 'd': { 
            if (defcount>=defines_max) { 
              error("too many defines\n"); 
              exit(1); }
            tmpstr=&argv[i][j+1];
            p=0;
            while (!is_sep(tmpstr[p])) p++;
            if (tmpstr[p]=='=') {
              tmpstr[p]='\0';
              defs[defcount]=str_sav(tmpstr);
              repl[defcount]=str_sav(&tmpstr[p+1]); }
            else if (tmpstr[p]=='\0') {
              defs[defcount]=str_sav(tmpstr);
              repl[defcount]=str_sav(""); }
            else Howto();
            defcount++;
            j=0; break; }
          case 'o': { i++; file_output=argv[i]; j=0; break; }
          case 'r': { remove_remarks=0; break; }
          case 's': { remove_spaces=0; break; }
          case 'l': { add_dotLines=1; break; }
          case 'q': { quiet_mode=1; break; }
          default:  Howto();
          }
        if (i==argc) Howto();
        if (j==0) break;
        j++; }
	} else {
      if (file_input!=NULL) Howto();
      file_input=argv[i]; pushSourceFile(file_input); }
    i++; }

  if (file_input==NULL) { printf("%s: No input file\n",argv[0]); exit(1); }
  if (file_output==NULL) file_output="lupo.out";

  outfile=fopen(file_output,"w");
  if (outfile==NULL) {
    printf("can't write to outputfile\n");
    exit(1); }
  line=0;
  infiles=0;
  infile=fopen(file_input,"r");
  if (infile==NULL) {
    printf("can't open inputfile\n");
    exit(1); }
  infilestream[0]=infile;
  infilename[0]=file_input;

  spec_label_count=0;
  spec_lab_stack_ptr=0;

  /* add some default defines */

  add_define("_LUPO_","");

  time(&bin_time);
  timeptr=localtime(&bin_time);
  strftime(buf,100,TIMEFORMAT,timeptr);
  add_define("_DATE_",str_sav(buf));
  
  /* start preprocessing input file */

  if (!quiet_mode) printf("Working...\n");

  while (readline(lbuf)!=EOF) {

#   ifdef debug
    printf("line=\"%s\" depth=%i, disable=%i, if_flag=%i\n",lbuf,depth,disable,if_flag[depth]);
#   endif

    i=nextchar(lbuf,0);
    if (lbuf[i]=='\0') continue;
    
    i=nextchar(lbuf,0);
    if (lbuf[i]=='#') {
    
      /* is a preprocessor directive */
    
      i=nextchar(lbuf,++i);
      if (lbuf[i]=='\0') continue; /* was NULL-directive */

      if ((j=match(lbuf,i,"if"))!=no_match ||
          (j=match(lbuf,i,"ifdef"))!=no_match ) {
        p=get_expr(lbuf,&j);
        if (lbuf[j]!='\0' && lbuf[j]!=';') error("syntax");
        depth++; 
        if (depth>=if_depth_max) {
          error("too deep if");
          exit(1); }
        if (!disable && !p) { 
           disable=depth;
           if_flag[depth]=0; }
        else if_flag[depth]=1;
        continue; }

      if ((j=match(lbuf,i,"elif"))!=no_match) {
        if (if_flag[depth]==no_match) { 
          error("elif without if");
          continue; }
        p=get_expr(lbuf,&j);
        if (lbuf[j]!='\0' && lbuf[j]!=';') error("syntax");
        p=if_flag[depth]==0 && p;
        if (!p && !disable) disable=depth;
        if (p && disable==depth) disable=0;
        if (p) if_flag[depth]=1;
        if (depth==0) error("elif without if");
        continue; }

      if ((j=match(lbuf,i,"ifndef"))!=no_match) {
        p=get_term(lbuf,&j);
        j=nextchar(lbuf,j);
        if (lbuf[j]!='\0' && lbuf[j]!=';') error("syntax");
        depth++;
        if (depth>=if_depth_max) {
          error("too deep if");
          exit(1); }
        if (p && !disable) {
          disable=depth;
          if_flag[depth]=0; }
        else if_flag[depth]=1;
        continue; }

      if ((j=match(lbuf,i,"else"))!=no_match) {
        j=nextchar(lbuf,j);
        if (lbuf[j]!='\0' && lbuf[j]!=';') error("syntax");
        if (depth==0 || if_flag[depth]==no_match) 
          error("else without if"); 
        else {
          if (if_flag[depth]==0) {
            disable=0;
            if_flag[depth]=1; }
          else if (!disable) disable=depth; }
        continue; }

      if ((j=match(lbuf,i,"endif"))!=no_match) {
        j=nextchar(lbuf,j);
        if (lbuf[j]!='\0' && lbuf[j]!=';') error("syntax");
        if (disable==depth) disable=0;
        if (depth==0) error("endif without if"); else {
          if_flag[depth]=no_match;
          depth--; }
        continue; }

      if ((j=match(lbuf,i,"enddef"))!=no_match) {
        error("enddef without begindef");
        continue; }

      if ((j=match(lbuf,i,"msg"))!=no_match) {
        if (!disable) printf("%s\n",&lbuf[nextchar(lbuf,j)]);
        continue; }

      if ((j=match(lbuf,i,"error"))!=no_match) {
        if (!disable) {
          error("preprocessing stopped");
          exit(1); }
        continue; }

      if ((j=match(lbuf,i,"include"))!=no_match) {
        if (disable) continue;

        j=nextchar(lbuf,j);
        process_line(&lbuf[j]);

        if (lbuf[j]=='\0') { error("missing filename"); continue; }
        if ((lbuf[j]!='\"')&&(lbuf[j]!='<'))            /*"*/
	    { error("'\"' or '<' expected"); continue; }
        i=++j;

        if (lbuf[i-1]=='<') lbuf[i-1]='>';
        while ((lbuf[j]!=lbuf[i-1]) && lbuf[j]!='\0') {
          j++; }
        if (lbuf[j]=='\0') error("unterminated filename in #include");
        lbuf[j]='\0';

        if (lbuf[i-1]=='\"') { /*"*/
          /* Local include */
          infile=fopen(&lbuf[i],"r");
          if (infile!=NULL)
            pushSourceFile(&lbuf[i]); }
        else
          /* System include */
          infile=fopenSystemInclude(&lbuf[i]);

        if (infile==NULL) {
          sprintf(buf,"can't open include file \"%s\"",&lbuf[i]);
          error(buf);
          infile=infilestream[infiles];
          continue; }

        lpos[infiles]=line;
        if (lbuf[nextchar(lbuf,j+1)]!='\0') error("syntax");
        infiles++;
        if (infiles>=include_depth_max) { error("too many nested includes"); exit(1); }
        infilestream[infiles]=infile;
        infilename[infiles]=str_sav(&lbuf[i]);
        line=0;
        continue; }

      if ((j=match(lbuf,i,"undef"))!=no_match) {
        if (disable) continue;

        j=nextchar(lbuf,j);

        if (lbuf[j]=='\0') { error("missing argument"); continue; }

        p=j;
        while (!is_sep(lbuf[j])) j++;

        if (lbuf[j]!='\0') {
          i=nextchar(lbuf,j);
          if (lbuf[i]!='\0' && lbuf[i]!=';') { error("syntax"); continue; }
          lbuf[j]='\0'; }

        i=search_def(&lbuf[p]);
        if (i!=no_match) {
          free(defs[i]);
          free(repl[i]);
          defcount--;
          while (i<defcount) {
            parcount[i]=parcount[i+1];
            defs[i]=defs[i+1];
            repl[i]=repl[i+1];
            i++; }

#         ifdef debug
		  printf("removed!\n");
#         endif

          }
	    continue; }

      if ((j=match(lbuf,i,"define"))!=no_match ||
          (j=match(lbuf,i,"begindef"))!=no_match) {

        int args,flag;
        flag=(lbuf[i]=='b');

        if (disable) {

          if (flag) {
            /* search for #enddef */
            tmp2=0;
            while (readline(lbuf)!=EOF) {
              i=nextchar(lbuf,0);
              if (lbuf[i]!='#') continue;
              i=nextchar(lbuf,i+1);
              if ((j=match(lbuf,i,"enddef"))==no_match) {
                error("#directive in macro-definition");
                continue; }
              j=nextchar(lbuf,j);
              if (lbuf[j]!='\0' && lbuf[j]!=';') error("syntax");
              tmp2=1;
              break; }
            if (!tmp2) error("begindef without enddef"); }

            continue; }

        j=nextchar(lbuf,j);
        process_line(&lbuf[j]);
        if (lbuf[j]=='\0') { error("missing argument"); continue; }

        if ((args=read_macro_def(lbuf,&j))==no_match) continue;

        /* read replacement, and replace parameter by single ascii */

        if (!flag) {
          /* makro defined in this line */
          i=p=j;
          while (lbuf[j]!='\0') {
            if (!is_sep(lbuf[j])) {
              tmp1=0; tmp2=no_match;
              while (tmp1<=args) {
                if ((tmp2=match(lbuf,j,mpar[tmp1]))!=no_match) break;
                tmp1++; }
              if (tmp2!=no_match) {
                lbuf[i++]=(char) tmp1+2;
                j=tmp2; 
                continue; } }
            if (lbuf[j]>=' ' || lbuf[j]=='\1') lbuf[i++]=lbuf[j];
            else if (lbuf[j]=='\t') lbuf[i++]=' '; 
            j++; }

          lbuf[i]='\0';
          repl[defcount]=str_sav(&lbuf[p]); }

        else {
          /* makro defined in next lines, terminated with #enddef */
          char *lbuf2;

          i=nextchar(lbuf,j);
          if (lbuf[i]!='\0' && lbuf[i]!=';') error("syntax");
          tmpstr=malloc(macro_length_max+1);
          if (tmpstr==NULL) { error("out of memory"); exit(1); }

          tmp2=0; p=0;
          lbuf2=malloc(line_length_max);
          if (lbuf2==NULL) { error("out of memory"); exit(1); }

          while (readline(lbuf2)!=EOF) {
            i=nextchar(lbuf2,0);
            if (lbuf2[i]=='#') {
              i=nextchar(lbuf2,i+1);
              if ((j=match(lbuf2,i,"enddef"))!=no_match) {
                j=nextchar(lbuf2,j);
                if (lbuf2[j]!='\0' && lbuf2[j]!=';') error("syntax");
                tmp2=1;
                break; }
              else error("#directive in macro-definition"); }
            else {
              process_line(lbuf2);
              j=0;
              while (lbuf2[j]!='\0' && p<macro_length_max) {
                if (!is_sep(lbuf2[j])) {
                  tmp1=0; tmp2=no_match;
                  while (tmp1<=args) {
                    if ((tmp2=match(lbuf2,j,mpar[tmp1]))!=no_match) break;
                    tmp1++; }
                  if (tmp2!=no_match) {
                    tmpstr[p++]=(char) tmp1+2;
                    j=tmp2; 
                    continue; } }
                if (lbuf2[j]>=' ' || lbuf2[j]=='\1') tmpstr[p++]=lbuf2[j]; 
                else if (lbuf2[j]=='\t') tmpstr[p++]=' '; 
                j++; }
              tmp2=0; }
            if (p<macro_length_max) tmpstr[p++]=(char) 1; }

          free(lbuf2);
          if (!tmp2) error("begindef without enddef"); 

          if (p>0 && tmpstr[p-1]==(char) 1) p--;
          if (p>macro_length_max) {
            error("macro too long");
            p=macro_length_max; } 

          tmpstr[p]='\0';
          repl[defcount]=str_sav(tmpstr);
          free(tmpstr);
#         ifdef debug  
          printf("str_sav:"); macroout(repl[defcount]);
#         endif
          }
        
        defcount++;
        continue; } 

      sprintf(buf,"unknown directive \"%s\"",&lbuf[i]);
      error(buf);
      continue; }
    
    if (disable==0) {
      process_line(lbuf);
      process_line_spec(lbuf);
      i=0;
      if (remove_spaces) { 
        while (lbuf[i]==' ' || lbuf[i]=='\t') i++; }

      if (!remove_spaces || lbuf[i]!='\0') {
	if (add_dotLines)
	  {
	    fprintf(outfile,"\t.line %d%s\n",line,getSourceFile());
	  }
        while (lbuf[i]!='\0') {
          if (lbuf[i]==(char) 1) fprintf(outfile,"\n");
          else fputc(lbuf[i],outfile); 
          i++; }
        fprintf(outfile,"\n"); }
	  }
   }

   fclose(outfile);
   if (depth!=0) error("if without endif");
   if (spec_lab_stack_ptr!=0) error("non empty special stack at end of file");
   if (errors) {
     printf("summary: %i error(s)\n",errors);
     exit(1); }
   else { 
     if (!quiet_mode) printf("done, no errors\n"); 
     exit(0); }
}

char sf_name[include_depth_max][1024];
int sf_count=0;

int pushSourceFile(char *foo)
{
  strcpy(sf_name[sf_count],foo);
  sf_count++;
  return(0);
}

int popSourceFile()
{
  sf_count--;
  return(0);
}

char *getSourceFile()
{
  int i;

  /* Clear source pedigree string */
  sourcePedigree[0]=0;

  /* Build source pedigree list */
  for(i=sf_count;i>0;i--)
    {
      sprintf(sourcePedigree,"%s,%s",sourcePedigree,sf_name[i-1]);
    }

  return(sourcePedigree);
}

FILE *fopenSystemInclude(char *foo)
{
  FILE *boo;
  char temp[1024];

#ifdef USE_GETENV
  char *envtmp;

  envtmp=getenv("LUPO_INCLUDEPATH");
  if (envtmp!=NULL) {
    int i;

    while (1) {
      /* scan all paths in this string */
      i=0;
      while (envtmp[i]!='\0' && envtmp[i]!=PATH_SEPARATOR) i++;
      strncpy(temp,envtmp,i);
      temp[i]=DIRECTORY_SEPARATOR;
      strcpy(&temp[i+1],foo);
      if ((boo=fopen(temp,"r"))!=NULL) {
        pushSourceFile(temp);
        return boo; }
      if (envtmp[i]!='\0') envtmp=&envtmp[i+1];
      else break; }

    return NULL; }

  error("environment varable \"LUPO_INCLUDEPATH\" is undefined");
  return NULL;

#else

  /* no getenv, then check some standard (UNIX) paths */
  
  sprintf(temp,"/usr/include/c64/%s",foo);
  if ((boo=fopen(temp,"r")))
    {
      pushSourceFile(temp);
      return(boo);
    }
  sprintf(temp,"/usr/include/lunix/%s",foo);
  if ((boo=fopen(temp,"r")))
    {
      pushSourceFile(temp);
      return(boo);
    }

  sprintf(temp,"/usr/local/include/c64/%s",foo);
  if ((boo=fopen(temp,"r")))
    {
      pushSourceFile(temp);
      return(boo);
    }
  sprintf(temp,"/usr/local/include/lunix/%s",foo);
  if ((boo=fopen(temp,"r")))
    {
      pushSourceFile(temp);
      return(boo);
    }

  sprintf(temp,"/usr/include/%s",foo);
  if ((boo=fopen(temp,"r")))
    {
      pushSourceFile(temp);
      return(boo);
    }

  sprintf(temp,"/usr/local/include/%s",foo);
  if ((boo=fopen(temp,"r")))
    {
      pushSourceFile(temp);
      return(boo);
    }

  /* Cant find it */
  return(NULL);

#endif 

}






