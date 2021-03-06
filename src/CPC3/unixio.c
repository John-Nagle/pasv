#include "pascaliodefs.h"

/* This file contains extensions to the Pascal libarary to allow
 * access to Unix I/O primitives not found in Standard Pascal
 */

WriteUnixioId()
{       printf("unixio.c 1.8");
}


/*
 * PerrorC(msg1,msg2) prints the messages and the error message
 * representing the current value of "errno".  
 */
PerrorC(msg1,msg2)
char msg1[], msg2[];
{
	char ss[100];			/* working string */
	perror(sprintf(ss,"%s%s",msg1,msg2)); /* edit error msg */
}
/*
	pipein  --  open a pipe for reading, given open descriptor
*/
pipein(filep, fdesc)
	register struct iorec	*filep;
	long                    fdesc;
{
	char ss[50];				/* for filename */
	if (_actfile[fdesc])			/* if already open */
	{
		fprintf(stderr,"pipein: descriptor %d already open.\n",fdesc);
		return;
	}
	sprintf(ss,"pipe_input_%d",fdesc);	/* name of file */
	filep = GETNAME(filep, ss, (long) strlen(ss), (long) 0);
	filep->fbuf = fdopen((int) fdesc, "r");
	if (filep->fbuf == NULL) {
		PerrorC("Could not open ", filep->pfname);
		return;
	}
	filep->funit |= (SYNC | FREAD | EOLN);	/* set unit flags */
	filep->funitno = fdesc;			/* set file descriptor */
	_actfile[fdesc] = filep;		/* set back pointer */
	setbuf(filep->fbuf, &filep->buf[0]);
}
/*
	pipeout  --  open a pipe for writing, given open descriptor
*/
pipeout(filep, fdesc)
	register struct iorec	*filep;
	long			fdesc;
{
	char ss[50];				/* for filename */
	if (_actfile[fdesc])			/* if already open */
	{
		fprintf(stderr,"pipeout: descriptor %d already open.\n",fdesc);
		return;
	}
	sprintf(ss,"pipe_output_%d",fdesc);	/* name of file */
	filep = GETNAME (filep, ss, (long) strlen(ss), (long) 0);
	filep->fbuf = fdopen((int)fdesc, "w");
	if (filep->fbuf == NULL) {
		PerrorC("Could not create ",filep->pfname);
		return;
	}

	filep->funit |= (EOFF | FWRITE);
	filep->funitno = fdesc;			/* set file descriptor */
	_actfile[fdesc] = filep;		/* set back pointer */
	setbuf(filep->fbuf, &filep->buf[0]);
}
