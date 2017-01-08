/*
	tcopy -- tape copy program

	Copies arbitrary tapes, any block size up to a maximum.
	Copying is terminated by an EOF mark.

				Version 1.5 of 7/17/81
*/
#define MAXBYTES 5000			/* size limit of copy */
#include <stdio.h>
int optiond = 0;                        /* if -d keyletter */
int optionv = 0;			/* if -v keyletter */
long blockno = 0;			/* block number */
char buf[MAXBYTES+2];			/* working buffer */
/*
	main program
*/
main(argc,argv)
int argc;
char *argv[];
{
int inod;	                        /* input open descriptors */
int outod;				/* output open descriptor */

int fargn = 0;                          /* number of file args */
char *fargs[2];                         /* address of file args */
int arg;                                /* current arg being processed */

char key;                               /* current keyletter */

    for (arg = 1; arg < argc; arg++)    /* scan arguments */
    {   if (argv[arg][0] == '-')        /* if keyletter */
	{   key = argv[arg][1];         /* get keyletter */
	    switch (key) {              /* fan out on keyletter */
	    case 'd': { optiond++; break;}/* debug */
	    case 'v': { optionv++; break;}/* print block counts */
	    default:  {                 /* unknown keyletter */
		fprintf(stderr,"Bad option: -%c\n",key); /* diagnose */
		exit(-1);
		}
	    }                           /* end switch */
	}                               /* end keyletter processing */
	else				/* not keyletter, must be file */
	{
	    if (fargn > 1)                  /* if too many file args */
	    {   fprintf(stderr,"Too many file args\n"); /* so state */
		exit(-1);                   /* error exit */
	    }
	    fargs[fargn++] = argv[arg];     /* remember file arg */
	}				/* end file arg */
    }                                   /* end arg processing */
    if (fargn != 2)                      /* if not two file args */
    {   fprintf(stderr,"Usage:  tcopy  <input tape device>  <output tape device>\n");
	exit(-1);                       /* error */
    }
    inod = open(fargs[0],0);            /* open arg 1 for reading */
    if (inod < 0)                       /* if open failed */
    {   fprintf(stderr,"Cannot open %s.\n",fargs[0]); /* so state */
	exit(-1);                       /* fails */
    }
    outod = open(fargs[1],1);		/* open arg 2 for writing */
    if (outod < 0)			/* if open failed */
    {   fprintf(stderr,"Cannot open %s. \n",fargs[0]); /* so state */
	exit(-1);			/* fails */
    }
    docopy(inod,outod);			/* do the copy */
    if (blockno == 0) exit(1);		/* zero-length copy */
    exit(0);				/* normal completion */
}
/*
	docopy -- perform the tape copy 
*/
docopy(in,out)
int in,out;				/* open descriptors */
{   int rdstat;				/* read status */
    int wrstat;				/* write status */
    while(1)				/* copy until done */
    {   rdstat = read(in,buf,sizeof(buf)); /* read block */
        if (rdstat == 0) break;		/* normal EOF */
	blockno++;			/* read something, count it */
	if (rdstat < 0)			/* if error */
	{   fprintf(stderr,"Read error (status %d) on block %D.\n",
		rdstat,blockno);
	    exit(-1);			/* fails */
	}
	if (rdstat > sizeof(buf)-2)	/* if oversize block */
	{   fprintf(stderr,"Block %D is too big for tcopy - limit %d bytes.\n",
		blockno,sizeof(buf)-2);	/* msg */
	    exit(-1);			/* fails */
	}
	if (optiond)			/* if debugging */
	{   printf("Block %D: %d bytes.\n",blockno, rdstat); /* print info */}
	wrstat = write(out,buf,rdstat);	/* write the block */
	if (wrstat < 0)			/* if write error */
	{   fprintf(stderr,"Write error (status %d) on block %D.\n",
		wrstat,blockno);
	    exit(-1);			/* fails */
	}
    }
    if (optionv)				/* if verbose mode */
    {   printf("%D blocks copied.\n",blockno); }
}
