/* Program to connect vcg and theorem prover with pipes */
/*
	Valid calls are

		pasver3b xxx		log VCs
		pasver3b xxx yyy	log VCs, and save raw for debug

	The two passes, pasver3c and pasver4, are started as separate
	processes and intercommunicate via a pair of pipes.
*/
char *what_string = "@(#)jver.c	1.12";
#define MINOD 6				/* first legal open descriptor */
/*
	make_pipe  --  make pipe for intercommunication between processes
*/
make_pipe(pipedesc)
int pipedesc[];
{ int dummy;				/* placeholder OD */ 
  dummy = dup(2);			/* tie up an od */
  if (dummy < MINOD)			/* if too small */
  {	make_pipe(pipedesc);		/* recurse */
	close(dummy);			/* give dummy back */
	return;				/* done */
  }
  if (pipe(pipedesc) != 0)
  { printf("Cannot create pipe\n");
    exit(1);
  }
}
/*
	make_file  --  make file for logging of debug info
*/
int make_file(filename)
char filename[];
{   int filedesc;			/* returned file descriptor */
    int dummy;				/* placeholder OD */ 
    dummy = dup(2);			/* tie up an od */
    if (dummy < MINOD)			/* if too small */
    {	filedesc = make_file(filename);	/* recurse */
	close(dummy);			/* give dummy back */
	return(filedesc);		/* done */
    }
    filedesc = creat(filename, 0644); 
    if (filedesc == -1)
    {   printf("Cannot create %s\n",filename);
	exit(4);
    }
    if (filedesc > 9)
    {   printf("INTERNAL ERROR: File descriptor is too big.\n");
	exit(5);
    }
    return(filedesc);
}
main(argc, argv)
int argc;
char *argv[];
{ int vcg_to_prover[2], prover_to_vcg[2]; /* pipe descriptors */
  int both_to_logger[2];		/* logging pipe */
  /* defines tell use which end of pipes are which */
  int tracedesc; 	/* file descriptor to used; -1 means none */
  int vcdesc;		/* file descriptor for VC saving for thm debug */

# define read_end 0
# define write_end 1
  char *arg0;				/* which command to execute */
  char *arg1 = 0, *arg2 = 0, *arg3 = 0, *arg4 = 0;	/* additional args */
  char ss1[12], ss2[12], ss3[12], ss4[12];/* additional args */

  tracedesc = -1;			/* assume no trace file */
/*
	Set up logging process if required
*/
  vcdesc = -1;				/* assume no VC file */
  if (argc > 1)
  {   
      make_pipe(both_to_logger); 	/* set up logging for people */
      tracedesc = both_to_logger[write_end]; /* set write end of logging */
      switch (fork())	{		/* fork off logging process */
  	case -1: /* miscarriage! */
    		printf("Can't fork\n");
    		exit(3);
  
  	case 0: /* child: the logger */
		close(both_to_logger[write_end]);	/* close other end */
		logger(both_to_logger[read_end],argv[1]); /* run logger */
		exit(0);		/* exits normally */
	default: /* parent - the main program */
		close(both_to_logger[read_end]);	/* close other end */
		break;
	}
      if (argc > 2)
      {    vcdesc = make_file(argv[2]);	/* log VCs for thm prover if needed */
      }
  } 
/*
	Set up prover and vcg, and the pipes between them
*/
  make_pipe(vcg_to_prover);
  make_pipe(prover_to_vcg);
  switch (fork())
  {
  case -1: /* miscarriage! */
    printf("Can't fork\n");
    exit(3);
  
  case 0: /* child: the prover */
    arg0 = "pasver4";			/* command to execute */
    sprintf(ss1,"%d",vcg_to_prover[read_end]);
    arg1 = ss1;				/* set arg */
    sprintf(ss2,"%d",prover_to_vcg[write_end]);
    arg2 = ss2;				/* set arg */
    close(vcg_to_prover[write_end]);
    close(prover_to_vcg[read_end]);
    break;

  default: /* parent: the vcg */
    arg0 = "pasver3c";			/* command to execute */
    sprintf(ss1,"%d",prover_to_vcg[read_end]);
    arg1 = ss1;				/* set arg */
    sprintf(ss2,"%d",vcg_to_prover[write_end]);
    arg2 = ss2;				/* set arg */
    close(prover_to_vcg[write_end]);
    close(vcg_to_prover[read_end]);
    if (vcdesc > -1)	{		/* if VC recording */
	sprintf(ss4,"%d",vcdesc);	/* add arg */
	arg4 = ss4;			/* set arg */
	}
    break;
  }
  if (tracedesc > -1) {			/* if tracing */
	sprintf(ss3,"%d",tracedesc);	/* add arg */
	arg3 = ss3;			/* set arg */
	}
					/* do the command, does not return */
  execlp(arg0, arg0, arg1, arg2, arg3, arg4, 0);
  printf("Cannot execute %s.\n", arg0);	/* if exec fails */
  exit(4);
}
/*
	logger  --  log messages to VC logging file

	This is a kludge so that the system will work on Eunice, where
	two proceses can share the same pipe but not the same file.
*/
logger(inod,outfil)
int inod;				/* input open descriptor */
char outfil[];				/* output file name */
{
	int outod;			/* output file descriptor */
	int stat;			/* count or status */
	char buf[128];			/* for reading pipe */
	outod = make_file(outfil);	/* make output file */
	for (;;) {			/* until EOF or trouble */
		stat = read(inod,buf,sizeof(buf)); /* read a block */
		if (stat == 0) break;	/* normal exit */
		if (stat < 0) { perror("Log pipe read error"); exit(1);}
		stat = write(outod,buf,stat);	/* write a block */
		if (stat < 0) { perror("Log file write error"); exit(1);}
	}
	close(inod); close(outod);	/* normal close */
	return;				/* success */
}
