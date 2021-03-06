#include "global.h"
#include "dfuncs.h"
#ifndef okport				/* Franz, Inc. compatability */
#include "ioext.h"			/* (they moved the def of xports) */
#endif
static char what_string[] = "@(#)pipeopen.c	2.2";

/*
 *    (pipeopen filedesc mode)
 * translate a numeric file descriptor to a port that is open for read ("r"),
 * or write ("w").  The mode arguments can be a string or atom.
 *
 * Because of the messing around lisp does with arguments to foreign functions,
 * pipeopen can only do a limited amount of checking on its arguments.
 */
lispval
Lpipeopen(filedesc, mode)
lispval mode;
int * filedesc;
{
    FILE *port;
    char *modech;
    int filei;

    for(;;)
    {   modech = (char *)
	   verify(mode, "pipeopen: second arg must be atom or string, not ");

        if (modech[0] == 'r' || modech[0] == 'w')
	    break;

        mode = errorh(Vermisc,"pipeopen: modes are only r and w",
		nil,TRUE,31,(char *) 0);
    }

    port = fdopen(*filedesc, modech);

    return( (lispval) (xports + (port - _iob)));
}
