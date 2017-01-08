/*
	getevents --  extracts EVENT entries from Boyer-Moore library
		      file.
				Version 1.7 of 12/12/85
*/
#include <stdio.h>
/*
	copyevent  --  event has been found - copy it to output

        An event is an expression with balanced parentheses.
*/
copyevent()
{   int parens = 0;				/* parens seen */
    char ch = 0;				/* working char */
    char lastch;				/* previous char */
    int didout = 0;				/* true if did output */
    for (;;)					/* until parens balance */
    {   lastch = ch;				/* previous char */
        ch = getchar();				/* get next char */
	if (ch == '\n') ch = ' ';		/* newline to space */
	if (lastch == ' ' && ch == ' ') continue; /* remove multiple spaces */
	if (ch == '(') parens++;		/* if (, inc */
	if (ch == EOF)				/* if EOF */
	{   fprintf(stderr,"Missing ) at EOF.");/* bad, not balanced */
	    exit(1);				/* fails */
	}
	if (parens)				/* if within parens */
	{	putchar(ch);			/* output this char */
		didout = 1;			/* note output */
	}
	if (ch == ')') parens--;		/* if ), dec */
        if (parens == 0) 			/* if balanced */
	{   if (didout) putchar('\n');		/* finish line */
	    return;				/* and continue */
	}
    }						/* end event loop */
}
#define EVENTLEV 2				/* look for event at this lev */
main()
{   	char ch = ' ';				/* working char */
	char *keypnt;				/* scan pointer */
	short parens = 0;			/* parenthesis level */
	for (;;)				/* forever */
	{	if (ch == 0) ch = EOF;		/* null is EOF */
		if (ch == EOF) break;		/* handle EOF */
		if (ch == '(')			/* if left paren */
		{	parens++; 		/* adjust level */
			ch = getchar();		/* on to next char */
			continue;		/* and try again */
		}
		if  (ch == ')')			/* if right paren */
		{	if (parens <= 0)	/* unbalanced */
			{	fprintf(stderr,"Extra ) in library.\n");
				exit(1);	/* fails */
			}
			parens--;		/* count down */
			ch = getchar();		/* on to next char */
			continue;		/* and try again */
		}
		if (parens != EVENTLEV) 	/* consider only at lev */
		{
			ch = getchar();		/* on to next char */
			continue;		/* and try again */
		}
		if (ch != ' ')			/* if not beginning keypnt */
		{
			ch = getchar();		/* get a char */
			continue;		/* try again */
		}
		/*	Start looking for the key word. */
		keypnt = " event ";		/* begin scan */
		for (;;)			/* check for EVENT */
		{	
			if (*keypnt++ != ch) 
				break;		/* if not EVENT word */
			if (*keypnt == 0)	/* if EVENT word OK */
			{	copyevent(); 	/* copy the event */
				ch = getchar();	/* get next char */
				break;		/* out of EVENT state */
			}
			ch = getchar();		/* get next char */
		}				/* end event scan loop */
	}					/* end after newline */
	if (ch == EOF) 				/* normal exit */
	{	printf("STOP\n");		/* finish file */
		exit(0);			/* exit */
	}
}
