#
/*
*	SCCS File Type - Text Formatting - /sys/source/s2
*
* 	What identification string
*/
static char *_S_mm	"@(#)PWB_mm.c	1.2";

/*
 * mm - PWB Memorandum Macro Proceedure
 *
 * Used to run nroff with a series of special flags, etc.
 */

#define YES 1
#define NO 0
#define NICEVAL 4
#define CATBUFSIZ 200

int neqn NO;
int col NO;
int tbl NO;

char *pitch "10";
char *dev "V";
char *term "T2";
char *g "";

char file[200];
char command[200];
char temp[100];

extern int fout;

main (argc, argv)
int argc;
char *argv[];
{
	register char *cp, *cpp, *com;
	char *cf;
	char *copy (), *cat ();
	int i;

	fout = dup (1);
	if (argc == 1) {
		printf ("usage: %s [options] [files]\n", argv[0]);
		printf ("-e => want eqn\n");
		printf ("-t => want tbl\n");
		printf ("-c => want col\n");
		printf ("-12 => want 12 pitch\n");
		printf ("-rC3 => D R A F T\n");
		printf ("-rB1 => Table of Contents (with .TC macro)\n");
		printf ("other options for nroff or PWB/MM\n\n");
		printf ("Devices supported at FACC:\n");
		printf ("  -TV => on Versatec (default)\n");
		printf ("  -TAJ12 => 12-pitch AJ\n");
		printf ("  -TAJ10 => 10-pitch Versatec\n");
		printf ("  -THP => on HP\n\n");
		printf ("Other devices (unsupported)\n");
		printf ("  -450 => on DASI450\n");
		printf ("  -300S => on DASI300S\n");
		printf ("  -300s => on DASI300S\n");
		printf ("  -300 => on DASI 300\n");
		printf ("  -tn300 => on Terminet 300\n");
		printf ("  -tn => on Terminet 300\n");
		printf ("  -ti => on TI\n");
		printf ("  -37 => on TTY 37\n\n");
		flush ();
		exit (1);
	}
	cpp = &temp[0];
	for (i = 1; i < argc; i++) {
		cp = &argv[i][1];
		if (argv[i][0] != '-')
			break;                  /* found file name */
		if (equal (cp, "e"))
			neqn = YES;
		else if (equal (cp, "t"))
			tbl = YES;
		else if (equal (cp, "12"))
			pitch = "12";
		else if (equal (cp, "TAJ12")) {
			pitch = "12";
			dev = "AJ12";
			term = "T1";
		} else if (equal (cp, "TAJ10")) {
			pitch = "10";
			dev = "AJ10";
			term = "T1";
		} else if (equal (cp, "TV")) {
			dev = "V";
			term = "T2";
		} else if (equal (cp, "c"))
			col = YES;
		else if (equal (cp, "450")) {
			g = "";
			dev = "450";
			term = "T0";
		} else if (equal (cp, "ti") || equal (cp, "tn300")
			   || equal (cp, "tn")) {
			dev = "tn300";
			term = "T0";
			col = YES;
		} else if (equal (cp, "300S") || equal (cp, "300s")) {
			g = "";
			term = "T0";
			dev = "300S";
		} else if (equal (cp, "THP")) {
			g = "|hp";
			term = "T0";
			dev = "HP";
		} else if (equal (cp, "37")) {
			g = "";
			dev = "37";
			term = "T0";
		} else if (equal (cp, "300")) {
			g = "";
			term = "T0";
			dev = "300";
		} else
			cpp = copy (cat (&cp[-1], " "), cpp);
	}
	if (equal (dev, "V"))
		col = YES;              /* force col if Versatec */
	if (cp[-1] == '-') {
		printf ("No text file to process!\n");
		flush ();
		exit (1);
	}
	if (equal (pitch, "12")) {
		if (equal (dev, "450"))
			dev = "450-12";
		else if (equal (dev, "300"))
			dev = "300-12";
		else if (equal (dev, "300S"))
			dev = "300S-12";
		else if (!equal (dev, "AJ12")) {
			printf ("Terminal type %s does not allow 12 pitch\n",
				 dev);
			flush ();
			exit (1);
		}
	}
	if (col) {
		if (equal (dev, "450") || equal (dev, "450-12"))
			g = "|col|450";
		else if (equal (dev, "tn300"))
			g = "|col";
		else if (equal (dev, "300"))
			g = "|col|gsi";
		else if (equal (dev, "300-12"))
			g = "|col|gsi +12 -2";
		else if (equal (dev, "300S"))
			g = "|col|300S";
		else if (equal (dev, "300S-12"))
			g = "|col|300S +12 -2";
		else if (equal (g, ""))
			g = "|col";
		else
			g = "|col|hp";
	}
	cf = &file[0];
	for (; i < argc; i++)
		cf = copy (cat (argv[i], " "), cf);     /* get file names */
	com = &command[0];
	if (tbl) {
		com = copy (cat ("tbl ", file), com);
		com = copy ("|", com);
		cpp = copy (" -", cpp);
	}
	if (neqn) {
		if (tbl)
			com = copy ("neqn|", com);
		else {
			com = copy (cat ("neqn ", file), com);
			com = copy ("|", com);
			cpp = copy (" -", cpp);
		}
	}
	if (!tbl && !neqn)
		copy (cat (" ", file), cpp);
	com = copy (cat ("nroff.new -R -mm -r", term), com);
	com = copy (cat (" -T", dev), com);
	com = copy (cat (" ", temp), com);         /* get args + files */
	com = copy (g, com);            /* get filters */
/*
	printf ("%s\n", command);
*/
	flush ();
	nice (NICEVAL);
	execl ("/bin/sh", "sh", "-c", command, 0);
	perror ("mm");
	exit (2);
}

char *
copy (fr, to)
register char *fr, *to;
{
	while (*fr)
		*to++ = *fr++;
	*to = 0;
	return to;
}

char catbuf[CATBUFSIZ];

char *
cat (s1, s2)
register char *s1, *s2;
{
	register char *cp;

	cp = &catbuf[0];
	while ((cp < &catbuf[CATBUFSIZ-1]) && *s1)
		*cp++ = *s1++;
	while ((cp < &catbuf[CATBUFSIZ-1]) && *s2)
		*cp++ = *s2++;
	*cp = 0;
	return catbuf;
}

equal (s1, s2)
register char *s1, *s2;
{
	while (*s1 && *s2)
		if (*s1++ != *s2++)
			return NO;
	if (!*s1 && !*s2)
		return YES;
	else
		return NO;
}

