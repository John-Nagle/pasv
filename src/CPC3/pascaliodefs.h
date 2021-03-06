/* Copyright (c) 1979 Regents of the University of California */

/* static	char sccsid[] = "@(#)iorec.h 1.1 8/27/80"; */
/*
	This is a part of the Pascal library, but is lifted from the
	4.2BSD sources and moved to the SUNs.  It ought to be in the
	include libraries for the SUNs, but it isn't.  This allows
	us to compile unixio.c, which contains our seek, tell, and
	pipe interfaces.
*/
#include <stdio.h>
#define NAMSIZ 76

struct iorec {
	char		*fileptr;	/* ptr to file window */
	long		lcount;		/* number of lines printed */
	long		llimit;		/* maximum number of text lines */
	FILE		*fbuf;		/* FILE ptr */
	struct iorec	*fchain;	/* chain to next file */
	long		*flev;		/* ptr to associated file variable */
	char		*pfname;	/* ptr to name of file */
	short		funit;		/* file status flags */
	short		funitno;	/* unit number */
	long		size;		/* size of elements in the file */
	char		fname[NAMSIZ];	/* name of associated UNIX file */
	char		buf[BUFSIZ];	/* I/O buffer */
	char		window[1];	/* file window element */
};

/*
 * unit flags
 */
#define SPEOLN	0x100	/* 1 => pseudo EOLN char read at EOF */
#define	FDEF	0x080	/* 1 => reserved file name */
#define	FTEXT	0x040	/* 1 => text file, process EOLN */
#define	FWRITE	0x020	/* 1 => open for writing */
#define	FREAD	0x010	/* 1 => open for reading */
#define	TEMP	0x008	/* 1 => temporary file */
#define	SYNC	0x004	/* 1 => window is out of sync */
#define	EOLN	0x002	/* 1 => at end of line */
#define	EOFF	0x001	/* 1 => at end of file */
/*
	Externals
*/
extern struct iorec *GETNAME();		/* library */
struct iorec *_actfile[_NFILE];/* indexed by open descriptor */
