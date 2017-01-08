program getrules(input,output);
(*
	getrules --  extracts EVENT entries from Boyer-Moore library
		      file.
				Version 1.5 of 12/31/82
*)
const keylength = 6;				(* length of key *)
var ch: char;					(* working char *)
    EOL, EOF: char;				(* constants *)
    keypos: 0..keylength;			(* position in key *)
    key: array [1..keylength] of char;		(* key to search for *)
    endfile, notkey: boolean;			(* flags for main *)
(*
	getchar  --  get character from input
*)
function getchar: char;
var chr: char;					(* working char *)
begin
    if eof then getchar := EOF 			(* EOF case *)
    else if eoln then begin			(* if end of line *)
	readln;					(* go to next line *)
	getchar := EOL;				(* return EOL *)
    end else begin read(chr); getchar := chr; end;(* otherwise return char *)
end {getchar};
(*
	copyevent  --  event has been found - copy it to output
	The entire event appears on one line, with level 1 lists
	quoted.
*)
procedure copyevent;
var parens: integer;				(* parens seen *)	
    ch: char;					(* working character *)
begin   
    parens := 0;				(* parens seen *)
    ch := EOF;					(* working char *)
    repeat
        ch := getchar;				(* get next char *)
	while ch = EOL do ch := getchar;	(* ignore newlines *)
	if ch = '(' then parens := parens + 1;	(* handle paren count *)
	if ch = ')' then parens := parens - 1;	(* handle paren count *)
	if ch = EOF then			(* if EOF *)
	begin   message('Missing ) at EOF.');(* bad, not balanced *)
		halt;				(* fails *)
	end;
	write(ch);				(* output this char *)
	until parens <= 0;			(* continue if unbalanced *)
    writeln;
end (* copyevent *);
(*
	Main program
*)
begin
    EOL := chr(10);				(* newline *)
    EOF := chr(4);				(* EOT *)
    key := 'EVENT ';				(* look for line with this *)
    endfile := false;				(* not at EOF yet *)
    while not endfile do begin			(* until EOF *)
    begin
	while (ch <> EOL) and (not endfile) do ch := getchar; (* skip to EOL *)
	if ch = EOL then begin			(* if start of line *)
	    keypos := 0;			(* begin string search *)
	    notkey := false;			(* no key try yet *)
	    repeat				(* search for key *)
		if keypos = keylength then begin(* if key matched *)
		    copyevent;			(* copy the event *)
		    notkey := true;		(* and skip out *)
		end else begin			(* if not matched yet *)
		    ch := getchar;		(* get next char *)
		    keypos := keypos + 1;	(* on to next key slot *)
		    if key[keypos] <> ch then notkey := true;
		    end;
		until notkey;			(* until key success or fail *)
	    end					(* end event scan loop *)
	end;					(* end after newline *)
	endfile := endfile or eof;		(* note EOF state *)
    end;					(* end outer loop *)
    writeln('STOP');				(* finish file *)
end.
