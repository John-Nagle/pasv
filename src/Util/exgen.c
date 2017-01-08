#include <stdio.h>
#define getput putchar(getchar())

 char what_string[] =  "@(#)exgen.c	1.1 ";

/* This program reads from its standard input a series of
 * Pascal routine defintions.  It writes on its standard
 * output a series of external declarations meant to be put in
 * a Pascal .h file.
 *
 * The program expects syntactically correct Pascal; it leaves syntax
 * checking to the compiler.  Routine definitions are recognized by the
 * word "procedure" or "function" in column one; the program is not bright
 * enough to ignore these words if they appear in the first column of
 * a comment.
 * 
 * If the program contains internal routines that should not be
 * exported, those routines should be indented.
 */
main()
{ char c, last_c, *keyword, *printkey;
  int in_args;

  c = getchar();
  while(c != EOF)
  { /* c is the first character of a line */
    keyword = (c == 'p') ? "procedure" : "function";
    printkey = keyword;
    for (;;)
    { if (c == *keyword++)
      { if (*keyword == '\0')
          goto read_key;
        else
        {  c = getchar();
           if (c==EOF)
             goto early_eof;
           continue;
      } }
      else goto discard_line;
    }

    read_key:
    { printf(printkey);
      /* read and print the arguments to the routine.  in_arg = 1
       * means we are between the parens of the argument.
       */
      in_args = 0;
      c = getput;
      while(! (c == ';' && in_args == 0))
      { /* the body of this loop gets the character after the construct
         * begun by c.
         */
        switch(c)
        {
        default:
          c = getput;
          break;

        case '(':
          c = getput;
          if (c == '*') 
          { /* read past a comment */
            last_c = getput;
            for(;;)
            { c = getput;
              if (c == EOF)
                goto early_eof;
              if (last_c == '*' && c == ')')
                break;
              else
                last_c = c;
          } }
          else
            in_args = 1;
          break;

        case ')':
          c = getput;
          in_args = 0;
          break;

        case '{':
          do c = getput; while (!(c == '}' || c == EOF));
          if (c == EOF)
            goto early_eof;
          else
            c = getput;
          break;

        case EOF:
          goto early_eof;
          break;
      } }

      printf(" external;\n");
    }

    discard_line:
    { while (! (c=='\n' || c==EOF))
        c = getchar();
      if (c == EOF)
        goto early_eof;
      else
        c = getchar();
  } }

  exit(0);

  early_eof:
  { fprintf(stderr, "early eof\n");
  exit(1);
} }
