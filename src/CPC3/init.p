#include "global.h"

procedure WriteInitId;
begin
  writeln('init.p 1.10') end;

procedure InitAlphaString;
(* Input: none.
 * Output: AlphaString.
 * Effect: Machine-independent initialization of AlphaString
 *)
begin
  AlphaString[ 0] := '0';
  AlphaString[ 1] := '1';
  AlphaString[ 2] := '2';
  AlphaString[ 3] := '3';
  AlphaString[ 4] := '4';
  AlphaString[ 5] := '5';
  AlphaString[ 6] := '6';
  AlphaString[ 7] := '7';
  AlphaString[ 8] := '8';
  AlphaString[ 9] := '9';
  AlphaString[10] := 'A';
  AlphaString[11] := 'B';
  AlphaString[12] := 'C';
  AlphaString[13] := 'D';
  AlphaString[14] := 'E';
  AlphaString[15] := 'F';
  AlphaString[16] := 'G';
  AlphaString[17] := 'H';
  AlphaString[18] := 'I';
  AlphaString[19] := 'J';
  AlphaString[20] := 'K';
  AlphaString[21] := 'L';
  AlphaString[22] := 'M';
  AlphaString[23] := 'N';
  AlphaString[24] := 'O';
  AlphaString[25] := 'P';
  AlphaString[26] := 'Q';
  AlphaString[27] := 'R';
  AlphaString[28] := 'S';
  AlphaString[29] := 'T';
  AlphaString[30] := 'U';
  AlphaString[31] := 'V';
  AlphaString[32] := 'W';
  AlphaString[33] := 'X';
  AlphaString[34] := 'Y';
  AlphaString[35] := 'Z';
  AlphaString[36] := 'a';
  AlphaString[37] := 'b';
  AlphaString[38] := 'c';
  AlphaString[39] := 'd';
  AlphaString[40] := 'e';
  AlphaString[41] := 'f';
  AlphaString[42] := 'g';
  AlphaString[43] := 'h';
  AlphaString[44] := 'i';
  AlphaString[45] := 'j';
  AlphaString[46] := 'k';
  AlphaString[47] := 'l';
  AlphaString[48] := 'm';
  AlphaString[49] := 'n';
  AlphaString[50] := 'o';
  AlphaString[51] := 'p';
  AlphaString[52] := 'q';
  AlphaString[53] := 'r';
  AlphaString[54] := 's';
  AlphaString[55] := 't';
  AlphaString[56] := 'u';
  AlphaString[57] := 'v';
  AlphaString[58] := 'w';
  AlphaString[59] := 'x';
  AlphaString[60] := 'y';
  AlphaString[61] := 'z';
  AlphaString[62] := '.';
  AlphaString[63] := '_';
  AlphaString[64] := '~' end;

procedure InitGlobal;
begin
  (* "constants" *)
  InitAlphaString;
  Dirty := false;
  DelimiterSet := ['(', ')', ' ', '$', '!', ':', '@', '+','-','/'];
  NextString := 1;

  QuoteBoolean :=      StringCreate('(boolean)/)');
  QuoteTrue :=         StringCreate('(true!)/)');
  NullString :=        StringCreate('/)');
  QuoteNew :=          StringCreate('new/)');
  QuoteDefined :=      StringCreate('defined/)');
  
  EndString := 'END';

  InitKeywordTable;  (* Note -- InitKeyword table uses NullString *)
  NextStringBase := NextString;
  InitVariableTable;
  InitTP;
  RequireList := nil;

  (* Serially reused variable *)
  InitVariableTable end;

procedure InitUnit;
begin
  LineCount := 0;
  StatementLine := 1;
  NextEnv := 1;
  NextString := NextStringBase;
  MaxLabelBucket := 0;
  InitReadChar;
  InitPointerName;
  end;
