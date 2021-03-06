{
	Constant definitions used by verifier.

	These definitions are part of both pass 1 and pass 2.
}
targetintegermin = -32768;		{ smallest target machine integer }
targetintegermax =  32767;		{ largest target machine integer  }
					{ -2**31 to 2**31-1 }
targetnumbermin = -2147483647;		{ smallest target machine anything }
targetnumbermax = 2147483647;		{ largest target machine anything }

precisionmin = -20;			{ minimum precision value }
precisionmax =  20;			{ maximum precision value }
addressmin = -524287;			{ smallest address in bits for 64K }
addressmax = 524288;			{ largest address }
identifiermax = 15;			{ max length of identifier }

blockmax = 10000;			{ max blocks per program }
symbolmax = 50000;			{ max symbols per program }
itemmax = 9;				{ max record depth }
filesmax = 50;				{ max different source files }
linemax = 30000;			{ max lines per file }
linetextmax = 120;			{ max chars per line (=mbuf) }
