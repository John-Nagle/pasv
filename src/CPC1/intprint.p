{$T+}
program intprint(input,output);
{INTPRINT: program to print the intermediate code and data
 files produced by pass 1 of the Simplified Language compiler.
 
 V2.0 - 22 Jan 80 - Ed Nelson  Ford Motor Co., E&RS
 V1.1 - 10/12/82 - converted to UNIX by J. Nagle, FACC, SEO

 This version of INTPRINT asks the user
 to specify a single character option
     where options =
       S: select procedures interactively;
       D: print data file.
 to select the procedures to be printed:
 Allowed user responses are:
  Y - print the current procedure;
  N - do not print;
  E - wrapup files and exit.
 
The output file may be specified as TTY: to print the intermediate
file on the terminal.

 
}
 
 
const mtabsize = 180;
type mtabtype = array [0..mtabsize] of packed array [0..4] of char;
     byte = 0 .. 255;
var int, dat: file of integer; 
    nch: integer;
    printdata, selectprocs, prnt, done: boolean;
    mtab: mtabtype;
 

procedure initprocedure;
begin
mtab[  0]:='nop  '; mtab[  1]:='xch  '; mtab[  2]:='del  '; mtab[  3]:='fix  '; 
mtab[  4]:='monit'; mtab[  5]:='ident'; mtab[  6]:='proc '; mtab[  7]:='end  '; 
mtab[  8]:='null '; mtab[  9]:='refer'; mtab[ 10]:='stol '; mtab[ 11]:='stor '; 
mtab[ 12]:='stof '; mtab[ 13]:='error'; mtab[ 14]:='error'; mtab[ 15]:='error'; 
mtab[ 16]:='succ '; mtab[ 17]:='pred '; mtab[ 18]:='error'; mtab[ 19]:='error'; 
mtab[ 20]:='error'; mtab[ 21]:='error'; mtab[ 22]:='error'; mtab[ 23]:='error'; 
mtab[ 24]:='uceq '; mtab[ 25]:='ucne '; mtab[ 26]:='ucgt '; mtab[ 27]:='ucle '; 
mtab[ 28]:='ucge '; mtab[ 29]:='uclt '; mtab[ 30]:='umax '; mtab[ 31]:='umin '; 
mtab[ 32]:='iadd '; mtab[ 33]:='isub '; mtab[ 34]:='imul '; mtab[ 35]:='idiv '; 
mtab[ 36]:='imod '; mtab[ 37]:='error'; mtab[ 38]:='error'; mtab[ 39]:='error'; 
mtab[ 40]:='ineg '; mtab[ 41]:='iabs '; mtab[ 42]:='iodd '; mtab[ 43]:='error'; 
mtab[ 44]:='ceil '; mtab[ 45]:='floor'; mtab[ 46]:='error'; mtab[ 47]:='error'; 
mtab[ 48]:='sadd '; mtab[ 49]:='ssub '; mtab[ 50]:='smul '; mtab[ 51]:='sdiv '; 
mtab[ 52]:='error'; mtab[ 53]:='rescl'; mtab[ 54]:='error'; mtab[ 55]:='error'; 
mtab[ 56]:='iceq '; mtab[ 57]:='icne '; mtab[ 58]:='icgt '; mtab[ 59]:='icle '; 
mtab[ 60]:='icge '; mtab[ 61]:='iclt '; mtab[ 62]:='imax '; mtab[ 63]:='imin '; 
mtab[ 64]:='fadd '; mtab[ 65]:='fsub '; mtab[ 66]:='fmul '; mtab[ 67]:='fdiv '; 
mtab[ 68]:='error'; mtab[ 69]:='error'; mtab[ 70]:='error'; mtab[ 71]:='error'; 
mtab[ 72]:='fneg '; mtab[ 73]:='fabs '; mtab[ 74]:='float'; mtab[ 75]:='trunc'; 
mtab[ 76]:='round'; mtab[ 77]:='error'; mtab[ 78]:='error'; mtab[ 79]:='error'; 
mtab[ 80]:='fxeq '; mtab[ 81]:='fxne '; mtab[ 82]:='fxgt '; mtab[ 83]:='fxlt '; 
mtab[ 84]:='fxge '; mtab[ 85]:='fxlt '; mtab[ 86]:='fxmax'; mtab[ 87]:='fxmin'; 
mtab[ 88]:='fceq '; mtab[ 89]:='fcne '; mtab[ 90]:='fcgt '; mtab[ 91]:='fcle '; 
mtab[ 92]:='fcge '; mtab[ 93]:='fclt '; mtab[ 94]:='fmax '; mtab[ 95]:='fmin '; 
mtab[ 96]:='not  '; mtab[ 97]:='error'; mtab[ 98]:='error'; mtab[ 99]:='error'; 
mtab[100]:='error'; mtab[101]:='error'; mtab[102]:='error'; mtab[103]:='error'; 
mtab[104]:='eqv  '; mtab[105]:='xor  '; mtab[106]:='nimp '; mtab[107]:='rimp '; 
mtab[108]:='imp  '; mtab[109]:='nrimp'; mtab[110]:='or   '; mtab[111]:='and  '; 
mtab[112]:='compl'; mtab[113]:='union'; mtab[114]:='inter'; mtab[115]:='sdiff'; 
mtab[116]:='error'; mtab[117]:='sgens'; mtab[118]:='sadel'; mtab[119]:='empty'; 
mtab[120]:='sceq '; mtab[121]:='scne '; mtab[122]:='scgt '; mtab[123]:='scle '; 
mtab[124]:='scge '; mtab[125]:='sclt '; mtab[126]:='in   '; mtab[127]:='sany '; 
mtab[128]:='error'; mtab[129]:='error'; mtab[130]:='error'; mtab[131]:='field'; 
mtab[132]:='ofset'; mtab[133]:='indir'; mtab[134]:='index'; mtab[135]:='movem'; 
mtab[136]:='error'; mtab[137]:='error'; mtab[138]:='invok'; mtab[139]:='error'; 
mtab[140]:='rtemp'; mtab[141]:='dtemp'; mtab[142]:='error'; mtab[143]:='error'; 
mtab[144]:='if   '; mtab[145]:='case '; mtab[146]:='entry'; mtab[147]:='loop '; 
mtab[148]:='exit '; mtab[149]:='for  '; mtab[150]:='error'; mtab[151]:='error'; 
mtab[152]:='seq  '; mtab[153]:='error'; mtab[154]:='wait '; mtab[155]:='send '; 
mtab[156]:='tsig '; mtab[157]:='lock '; mtab[158]:='enabl'; mtab[159]:='error'; 
mtab[160]:='litsc'; mtab[161]:='error'; mtab[162]:='liter'; mtab[163]:='rdata'; 
mtab[164]:='litd '; mtab[165]:='raise'; mtab[166]:='error'; mtab[167]:='error'; 
mtab[168]:='vceq '; mtab[169]:='vcne '; mtab[170]:='vcgt '; mtab[171]:='vcle '; 
mtab[172]:='vcge '; mtab[173]:='vclt '; mtab[174]:='dvad '; mtab[175]:='error'; 
mtab[176]:='varbl'; mtab[177]:='param'; mtab[178]:='call '; mtab[179]:='icall';
mtab[180]:='error'; 
end;

 
procedure nextch;
begin
  read(int,nch);			{ read next icode byte }
end {nextch};
 
procedure outmne (i: integer);
 
begin {outmne}
  write(output,mtab[i]:5,'  ')
end {outmne};
 
procedure outc (c: char);
begin
  if prnt then write(output,c:1)
end {outc};
 
procedure outnl;
begin
  if prnt then writeln(output)
end  {outnl};

procedure out8;
begin
  nextch; if prnt then write(output,nch:4)
end {out8};
 
procedure out16;
var i:integer;
begin
  nextch; i := nch*256;
  nextch; if prnt then write(output, nch+i:6)
end {out16};
 
procedure outlevel;
begin
  if prnt then write(output, nch mod 16:2)
end {outlevel};
 
procedure outsize;
begin
  nextch;
  if prnt then write(output, ':', nch:3)
end {outsize};
 
procedure outds;
begin
  outsize;
  outc(','); out16
end {outds};

procedure outbfld;
begin
  outsize;
  outc(','); out8
end {outbfld};

procedure outdvad;
begin
  nextch;
  if prnt then write(output,':', nch div 16:2, ',', nch mod 16:2);
  outc(',');
  out16
end {outdvad};
 
procedure options;
var ch: char;
begin
  printdata := false;
  selectprocs := false;
  write(output,'options: '); 
  if not eoln(input) then
  begin
    repeat
      read(input,ch);
      if (ch = 'd') or (ch = 'D')
        then printdata := true
      else if (ch = 's') or (ch = 'S')
        then selectprocs := true
      else if ch = ' '
        then ch := ch
      else writeln(output,'Error in option. Valid options are: S,D');
    until eoln(input)
  end
end {options};
 
procedure askuser;
var ch: char;
begin
  write(output,'?'); 
  readln(input); ch := input^;
  prnt := (ch='Y') or (ch='y');
  if (ch='E') or (ch='e') then done := true
end {ask_user};
 
 
procedure scancode;
var i, n, opcode, opcode2: integer;
begin {scancode}
  writeln(output);				{ blank line }
  writeln(output,' Code'); writeln(output,' ----'); writeln(output);
  repeat
    nextch;
    opcode := nch;
    if opcode > 239 then opcode2 := 180
      else if opcode > 223 then opcode := 179 {icall}
      else if opcode > 207 then opcode2 := 178 {call}
      else if opcode > 191 then opcode2 := 177 {param}
      else if opcode > 175 then opcode2 := 176 {varbl}
      else opcode2 := opcode;
    if opcode2 = 5 {ident} then
    begin
      prnt := true;
      writeln(output)
    end;
    if prnt then
    begin
      writeln(output); write(output,  opcode:4, '  '); {print op code number}
      outmne(opcode2)                 {print mnemonic}
    end;
 
case opcode2 of
  0,1,2: ;

  3: begin  
      out8; outc(','); out16; outc(':');  {range low}
      out8; outc(','); out16; outc(' ');    {range high}
      outc('p'); out8; outc(','); out16;    {precision}
     end;

  4: begin  
      out8; out8;  {monit}
     end;
 
  5: begin {ident}
       if selectprocs then writeln(output);
       nextch; n := nch;
       outc('''');
       for i := 1 to n do
       begin
         nextch;
	 write(output,chr(nch));
	 if selectprocs then write(output,chr(nch))
       end;
       outc('''');
       if selectprocs then askuser;
     end;
 
  6: begin
      out8; out8; {proc}
     end;
 
  7: begin {end}
       out8;  outc(',');
       out8;  outc(',');
       out16; outc(',');
       out16; outc(',');
       out16; outnl; outnl
     end;
 
  8,9,10,11,12,13,14,15,16,17,18,19,20,
  21,22,23,24,25,26,27,28,29,30,31,32,33,34,
  35,36,37,38,39,40,41,42,43,44,45,46,47,48,
  49,50,51,52,54,55,56,57,58,59,60,61,
  62,63,64,65,66,67,68,69,70,71,72,73,74,75,
  76,77,78,79,80,81,82,83,84,85,86,87,88,89,
  90,91,92,93,94,95,96,97,98,99,100,
  101,102,103,104,105,106,107,108,109,110,
  111,112,113,114,115,116,117,118,119,120,
  121,122,123,124,125,126,127,128,129,130: ;
 
  53: begin  {rescl}
      	out8; outc(','); out16; outc(':');
	out8; outc(','); out16; outc(' ');
	outc('p'); out8; outc(','); out16
      end;

  131: outbfld;  {field}
  132,133,134,135: {ofset,indir,index,movem}
     outds;
 
  136,137: ;
 
  138: begin {invok}
         out8; outc(','); out8
       end;
 
  139: ;
 
  140,141: out8; {rtemp,dtemp}
 
  142,143,144: ;
 
  145: begin {case}
         out8; outc(','); out8
       end;
 
  146,147,152: out8; {entry,loop,seq}
 
  148,149: ; {exit,for}
 
  150,151,153,154,155,156,157,158,159: ;

  160: begin  {litsc}
         out8; outc(','); out16
       end;

  161: ;

 
  162: out16; {liter}
 
  163: begin {rdata}
         out16
       end;
 
  164: begin {litd}
         for i := 1 to 3 do
         begin
           out16; outc(',')
         end;
         out16
       end;
 
  165,166,167,168,169,170,171,172,173: ;

  174: outdvad;   {DVAD  (device address) }

  175: ;
 
  176,177: begin {varbl,param}
             outlevel; outds
           end;
 
  178: begin {call}
         outlevel; outsize;
         outc('('); out8; outc(')');
         outc(','); out8
       end;
 
  179: begin {icall}
         outlevel; 
	 outc('('); out8; outc(')')
       end;

  180:
 
end {case};
 
    if eof(int) then done := true
  until done
 
end {scancode};
 
 
procedure scandata;
const nbytes = 8 {bytes printed per line};
var
  i, n, dataloc, lineloc: integer;
  bytes: array [1..nbytes] of byte;
  byt: byte;
  inch: integer;
  finished: boolean;
begin {scandata}
  page(output);				{ eject as required }
  writeln(output); writeln(output,' Data'); writeln(output,' ----'); writeln(output);
  dataloc := 0;
  lineloc := 0;
  finished := eof(dat);
  while not finished do
  begin
    n := 0;
    repeat {collect enough data for 1 line}
      read(dat,inch);			{ read one byte }
      dataloc := dataloc + 1;
      n := n + 1;
      bytes[n] := inch;
      finished := eof(dat)
    until finished or (n = nbytes);
    if n > 0 then
    begin {print the line}
      write(output, lineloc:6, ': ');
      for i := 1 to n do
      {print bytes}
      begin
        byt := bytes[i];
	write(output, byt:4);
        if (byt >= 32) and (byt <= 126)
	  then write(output, ' ''', chr(byt), ''' ')
          else write(output, '     ');
      end;
      lineloc := lineloc + nbytes;
      writeln(output)
    end {of print the line}
  end {while not finished};
  writeln(output); writeln(output, ' *** eof ***')
end {scandata};
 
 
begin {INTPRINT}
  initprocedure;                        { initialize constants }
  options;				{ read options from terminal }
  reset(int,'pasf-icode');			{ ***TEMP*** set input file }
  reset(dat,'pasf-data');			{ ***TEMP*** set input file }

  done := false; prnt := true;
  scancode;
  if printdata then
  begin
    scandata
  end;
  writeln(output); 
  writeln(output,' end INTPRT.'); 
end.
