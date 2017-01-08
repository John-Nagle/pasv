/*
	Filter for underscore removal for names in Pascal programs

	Removes underscores in program text, but not comments and
	strings.

	Version 1.1 of 8/27/82.

	This is a finite-state transducer, and should handle
	all valid Pascal programs correctly.

*/
#include <stdio.h>
main()
{
char ch;                                /* char just read */
int state;                              /* size of input line so far */
    state = 0;                          /* initialize */
    while(1)
    {   ch = getchar();                 /* get next char */
	switch(state){                  /* get into current state */
	case 0: {switch(ch){            /* state 0: normal text */
		case '(':  { state = 1; break;}/* possible comment start */
		case '\'': { state = 3; break;}/* quoted string constant start */
		case '{':  { state = 2; break;}/* comment start */
		default:   { break; }   /* all others */
		} break; }
	case 1: {switch(ch){            /* state 1: possible comment start */
		case '*':  { state = 2; break;}/* comment start */
		case '{':  { state = 2; break;}/* comment start */
		case '\'': { state = 3; break;}/* quoted string constant start */
		default:   { state = 0; break;}/* not comment start */
		} break; }
	case 2: {switch(ch){            /* state 2: inside comment */
		case '}':  { state = 0; break;}/* comment end */
		case '*':  { state = 5; break;}/* possible comment end */
		default: { break;};            /* all others */
		} break;}
	case 3: {switch(ch){            /* state 3: inside quote */
		case '\'': { state = 4; break;}/* possible end of quote */
		default: { break; }     /* all others */
		} break; }
	case 4: {switch(ch){            /* state 4: possible end of quote */
		case '\'': { state = 3; break;}/* double quote within quote string */
		default:   { state = 0; break;}/* all others */
		} break; }
	case 5: {switch(ch){            /* state 5: possible comment end */
		case ')':  { state = 0; break;}/* comment end */
		case '}':  { state = 0; break;}/* comment end */
		case '*':  { state = 5; break;}/* possible comment end */
		default:   { state = 2; break;}/* all others */
		} break; }
	}                               /* end transducer states */
	if (ch == EOF) break;           /* end of input */
	if (state == 0)                 /* if in state zero */
	{   if (ch >= 'A' && ch <= 'Z') /* if upper-case character */
	    { ch = ch + ('a' - 'A'); }  /* convert to lower case */
	    if (ch != '_')              /* if not an underscore */
	    {   putchar(ch); }          /* put char out */
	} else                          /* end in program text */
	{   putchar(ch);   }            /* otherwise always just copy */
    }                                   /* end main loop */
    exit(0);                            /* exit, flushing buffers */
}
