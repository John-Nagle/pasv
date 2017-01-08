#include <stdio.h>                      /* new I/O package */
main ()
				       /* readable.c 1.2 of 2/23/95.  */
{
    char    ch;
    ch = getchar();                    /* get first character */
    while (ch != EOF)                  /* while not eof do */
    {
	outchar (ch);		       /* output char */
	ch = getchar ();	       /* get next char */
    }
    putchar('\n');                     /* end with final newline */
    exit(0);				/* normal exit */
}
outchar (outch) char    outch;	       /* edit char in C representation */
{
    int     outval;		       /* value of char as an integer */
    outval = 0177 & outch;	       /* outval = ord(outch) */
    if (outval < 040 || outval > 0177) /* if outside graphics */
    {

	printf ("<0%o>", outval);
	if (outval == '\n' || outval == '\r')
				       /* handle end of line */
	{
	    putchar ('\r');
	    putchar ('\n');
	}			       /* with CR LF */
    }
    else
	putchar (outch);	       /* otherwise put as given */

}

