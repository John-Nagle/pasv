
/*
	Overprint N times
*/
#include <stdio.h>
#define OVERPRINTS 5				/* times to overprint */
#define LINEL 250				/* max line length */
char line[LINEL];				/* working line */
int linel;					/* pos in line */
/*
	get a line, return EOF or \n
*/
char getline()
{   
    char ch;					/* working char */
    linel = 0;					/* chars in line */
    for (;;) 
    {   ch = getchar();				/* get next char */
        if (ch == '\n') break;			/* done */
	if (ch == EOF) break;			/* done */
	if (linel >= LINEL)			/* if line too long */
	{   fprintf(stderr,"Line too long.\n");	/* so state */
	    exit(1);				/* fails */
	}
	line[linel++] = ch;			/* save char */
    }
    return(ch);					/* return EOF or char */
}

main()
{   int i;
    while (getline() != EOF)			/* while not EOF */
    {   for (i = 0; i < OVERPRINTS; i++)	/* for overstrikes */
	{   fwrite(line,linel,1,stdout);	/* write the line */
	    putchar('\r');			/* begin overprint */
        }
	putchar('\n');				/* finish the line */
    }
}
