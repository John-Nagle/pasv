{	Icode Operators	Must match Pass One}	stnumop		=   0;	xchop   	=   1;	delop   	=   2;	fixop   	=   3;	monitop   	=   4;	identop   	=   5;	procop   	=   6;	endop   	=   7;	nullop   	=   8;	referop   	=   9;	stolop   	=  10;	storop   	=  11;	stofop   	=  12;	succop   	=  16;	predop   	=  17;	uceqop   	=  24;	ucneop   	=  25;	ucgtop   	=  26;	ucleop   	=  27;	ucgeop   	=  28;	ucltop   	=  29;	umaxop   	=  30;	uminop   	=  31;	iaddop   	=  32;	isubop   	=  33;	imulop   	=  34;	idivop   	=  35;	imodop   	=  36;	inegop   	=  40;	iabsop   	=  41;	ioddop   	=  42;	ceilop   	=  44;	floorop   	=  45;	saddop   	=  48;	ssubop   	=  49;	smulop   	=  50;	sdivop   	=  51;	resclop   	=  53;	iceqop   	=  56;	icneop   	=  57;	icgtop   	=  58;	icleop   	=  59;	icgeop   	=  60;	icltop   	=  61;	imaxop   	=  62;	iminop   	=  63;	faddop   	=  64;	fsubop   	=  65;	fmulop   	=  66;	fdivop   	=  67;	fnegop   	=  72;	fabsop   	=  73;	floatop   	=  74;	truncop   	=  75;	roundop   	=  76;	fxeqop   	=  80;	fxneop   	=  81;	fxgtop   	=  82;	fxleop   	=  83;	fxgeop   	=  84;	fxltop   	=  85;	fxmaxop   	=  86;	fxminop   	=  87;	fceqop   	=  88;	fcneop   	=  89;	fcgtop   	=  90;	fcleop   	=  91;	fcgeop   	=  92;	fcltop   	=  93;	fmaxop   	=  94;	fminop   	=  95;	notop   	=  96;	eqvop   	= 104;	xorop   	= 105;	nimpop   	= 106;	rimpop   	= 107;	impop   	= 108;	nrimpop   	= 109;	orop    	= 110;	andop   	= 111;	complop   	= 112;	unionop   	= 113;	interop   	= 114;	sdiffop   	= 115;	sgensop   	= 117;	sadelop   	= 118;	emptyop   	= 119;	sceqop   	= 120;	scneop   	= 121;	scgtop   	= 122;	scleop   	= 123;	scgeop   	= 124;	scltop   	= 125;	inop    	= 126;	sanyop   	= 127;	signlop		= 130;	fieldop   	= 131;	ofsetop   	= 132;	indirop   	= 133;	indexop   	= 134;	movemop   	= 135;	invokop   	= 138;	rtempop   	= 140;	dtempop   	= 141;	ifop    	= 144;	caseop   	= 145;	entryop   	= 146;	loopop   	= 147;	exitop   	= 148;	forop   	= 149;	blockop		= 150;	xhndlop		= 151;	seqop   	= 152;	waitop   	= 154;	sendop   	= 155;	tsigop   	= 156;	lockop   	= 157;	enablop   	= 158;	isgnlop		= 159;	litscop   	= 160;	literop   	= 162;	rdataop   	= 163;	litdop   	= 164;	raiseop   	= 165;	vceqop   	= 168;	vcneop   	= 169;	vcgtop   	= 170;	vcleop   	= 171;	vcgeop   	= 172;	vcltop   	= 173;	dvadop   	= 174;	varblop   	= 176;	paramop   	= 192;	callop   	= 208;	icallop   	= 224;	defarop		= 244;	fcallop		= 245;		{ generated within pass 2 }	vinitop		= 246;	measop		= 247;	depthop		= 248;	defndop		= 249;	oldop		= 250;	vdefnop		= 251;	vdeclop		= 252;	vheadop		= 253;	asertop		= 254;	linenop		= 255;{	subcodes for ASERT operator}	assertsubcode 	= 1;	statesubcode 	= 2;	summarysubcode 	= 3;		entrysubcode 	= 11;				exitsubcode 	= 12;				effectsubcode 	= 13;				invariantsubcode= 14;		{ INVARIANT of monitor/module }		entryexitsubcode= 15;		{ INVARIANT of procedure/function }	initentrysubcode= 16;		{ ENTRY of module/monitor }	initexitsubcode = 17;		{ EXIT of module/monitor }