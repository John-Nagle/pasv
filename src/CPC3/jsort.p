program Jsort(input, p2jcode, p3jcode, output);
(* This program reads a p2jcode, which is a list of J-units, and sorts
 * the units using their first records as keys.
 *)
////{$I unixio.h}
const
  StringPoolSize = 100000;
  MaxJunits = 1000;

type
  StringIndex = 1..StringPoolSize;
  FilePosition = integer;
  Relation = (Less, Equal, Greater, Uncertain);

  UnitDescriptor = record
    UnitPos: fileptr;        (* Arg given to SEEK to get the first byte
			      * of the J-unit.
			      *)
    UnitLen: integer;        (* The number of characters in the unit (including
			      * line-end characters).
			      *)
    KeyPos:  StringIndex;    (* A pointer to a copy of the first line *)
    KeyLen:  integer end;    (* The length of the first line (not counting its
			      *	line-end character).
			      *)

var

(* StringPool is a pot of storage in which the initial records of each
 * Junit in p2jcode are kept.  No delimiters are stored StringPool; instead
 * a length is stored elsewhere.  NextString is the index of the next
 * available byte in StringPool.
 *)

p2jcode, p3jcode: text;

StringPool: packed array [StringIndex] of char;
NextString: StringIndex;

QuoteEND: array [1..3] of char; (* a pseudo-constant *)
(* The array A is what is actually sorted.  There is one slot in A for
 * every J-unit.
 *)
A: array [1..MaxJunits] of UnitDescriptor;
Asize: 0..MaxJunits;

SwapTemp: UnitDescriptor; (* Used to swap elements of A during sorting *)

(* Variables used only by QuickSort, but not stacked in recursive calls *)
LessTop, GreaterBot, MidVal: integer;
${I jsort.h}

procedure BuildA;
(* This routine reads p2jcode and creates the array A as described above. *)
var
  C: char;
  State: -1 .. 4;
begin
  reset(p2jcode);
  Asize := 0;

  (* For every J-unit in p2jcode ... *)
  while not eof(p2jcode) do begin

    if Asize = MaxJunits then begin
      writeln('jsort: unit limit exceeded');
      halt end;
    Asize := Asize + 1;

    with A[Asize] do begin
      UnitPos := TELL(p2jcode);
      KeyPos := NextString;
      KeyLen := 0;

      (* Read the first line of the J-unit into the String Pool *)
      while not eoln(p2jcode) do begin

	if NextString = StringPoolSize then begin
	  writeln('jsort: string pool overflow');
	  halt end;

	read(p2jcode, StringPool[NextString]);
	NextString := NextString + 1;
	KeyLen := KeyLen + 1 end;
	
	(* Count characters read so far *)
	UnitLen := KeyLen;

	(* Scan the rest of the unit, reading looking for the delimiter
	 * 'END' in column 1.Count the characters as they are read.
         * The variable State contains the number of characters in 'END'
	 * that have been found at the beginning of the current line.
         * State = -1 means the current line does not begin 'END'.
	 *)

	repeat
	if eof(p2jcode) then begin
	  writeln('jsort: malformed file');
	  halt end;
	
	if eoln(p2jcode) then begin
	  State := 0;
	  get(p2jcode) end
	else begin
	  read(p2jcode, C);
          if State >= 0 then begin
	    if C = QuoteEND[State+1]
	    then State := State + 1
	    else State := -1 end end;

	UnitLen := UnitLen + 1;
        until State = 3;

      (* Read the rest of the last line *)
      while not eoln(p2jcode) do begin
      get(p2jcode);
      UnitLen := UnitLen + 1 end;

      (* Read the final line-end *)
      get(p2jcode);
      UnitLen := UnitLen + 1 end end end;

function StringCompare(var X, Y: UnitDescriptor): Relation;
(* Lexicographically compare the keys of X and Y,
 * returning Less, Greater, or Equal, to indicate the result.
 *)
var
  Xindex, Xend, Yindex, Yend: integer;
  Result: Relation;
begin
  Xindex := X.KeyPos;
  Xend   := Xindex + X.KeyLen;
  Yindex := Y.KeyPos;
  Yend   := Yindex + Y.KeyLen;

  (* The following loop has the invariant that the strings compare
   * equal from X.KeyPos and Y.KeyPos up to and but not including
   * Xindex and Yindex.  On each iteration of the loop, we either
   * bump Xindex and Yindex while maintaining the invariant, or figure
   * out the answer and return.
   *)

  Result := Uncertain;
  repeat
    if Xindex = Xend then 
      if Yindex = Yend then 
	Result := Equal
      else (* We have reached the end of X but not Y *)
	Result := Less
    else if Yindex = Yend then 
      (* We have reached the end of Y but not X *)
      Result := Greater
    else begin
      (* We have reached the end of neither X nor Y *)
      if StringPool[Xindex] < StringPool[Yindex] then
	Result := Less
      else if StringPool[Xindex] > StringPool[Yindex] then
	Result := Greater
      else begin
	(* assert StringPool[Xindex] = StringPool[Yindex] *)
	Xindex := Xindex + 1;
	Yindex := Yindex + 1 end end
    until Result <> Uncertain;
  
  StringCompare := Result end;

procedure QuickSort(Lo, Hi: integer);
(* Sort segment of A starting at Lo and continuing up to but not
 * including Hi, using quicksort.
 * The records are sorted in increasing lexicographic order of the keys.
 *)
begin
  if Lo+2 <= Hi then begin
    LessTop := Lo;
    GreaterBot := Hi - 1;

    (* Pick an element near the "middle" around which to partition the
     * array.  Move this element to SwapTemp, and then move the "hole"
     * created to A[GreaterBot].
     *)
    MidVal := (Lo + Hi) div 2; (*  Because Lo < Hi, Lo <= Mid < Hi *)
    SwapTemp := A[MidVal];
    A[MidVal] := A[GreaterBot];

    (* The invariants of the following loop is:
     *    All but one element of the array are kept in A[J], where Lo <= J < Hi
     *       and J <> GreaterBot.  The remaining element is in SwapTemp.
     *
     *    For all J where Lo <= J < LessTop, A[J] < SwapTemp
     *    For all J where GreaterBot < J < Hi, A[J] > SwapTemp
     *)

    while LessTop <> GreaterBot do begin
      case StringCompare(A[LessTop], SwapTemp) of
      Less:
	LessTop := LessTop + 1;

      Equal: begin
	writeln('QuickSort: duplicate unit names');
	halt end;

      Greater: begin
	A[GreaterBot] := A[LessTop];
	GreaterBot := GreaterBot - 1;
	A[LessTop] := A[GreaterBot] end end end;

    (* Now that the hole has stopped moving, fill it in. *)
    A[GreaterBot] := SwapTemp;

    QuickSort(Lo, LessTop);
    QuickSort(GreaterBot+1, Hi) end end;

procedure WriteOut;
(* Create the file p3jcode by reordering p2jcode as specified by A. *)
var 
  C: char;
  J, K: integer;
begin

  rewrite(p3jcode);
  if Asize >= 1 then				(* avoid compiler bug *)
  for J := 1 to Asize do with A[J] do begin
    SEEK(p2jcode, UnitPos);
    if UnitLen >= 1 then			(* avoid compiler bug *)
    for K := 1 to UnitLen do begin
      if eoln(p2jcode) then begin
	writeln(p3jcode);
	get(p2jcode) end
      else begin
        read(p2jcode, C);
	write(p3jcode, C) end end end end;
begin
  if argc <> 1 then
    writeln('% W %')
  else begin
    QuoteEND := 'END';
    NextString := 1;
    Asize := 0;
    BuildA;
    QuickSort(1, Asize+1);
    WriteOut end end.
