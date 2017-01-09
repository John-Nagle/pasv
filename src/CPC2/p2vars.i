var
  SINK: packed array [1..40] of char;    { for dummy WHAT string replacements }

  tree:    ptn;                { root of icode tree }       
  vartree: varnodep;            { root of variable tree }  
  blockhead, blocktail: blocknodep;    { pointers to block chain }
 
                {Miscellaneous variables}
                {***********************}

  namesize: integer;             {length of current procedure name}
  name: array[1..15] of char;        {current procedure name}
  programloadtype: loadtype;        {internal/external indicator}
  seriouserror: boolean;        {true if badnode or internalerror}
  fatalerror: boolean;            {fatal error has occured}
  usererrors: integer;            {non-internal errors}
  gencnt: 0..jlinelengthmax;        {position on jcode line }
  firstline: lineinfo;            {first line number in routine}
  currentblockp: blocknodep;        {pointer to current block info }
  lastblockp: blocknodep;        {pointer to last block info }
  nodesallocated: integer;        {count of allocated nodes}
  lastsourcefile: integer;        {last file number printed in diag}
  srcbuf: sourceline;            {last source line printed}
  labelserial: labelid;            {label counter for jcode labels}
  tempserial: tempid;            {last TEMP$ number}
  clockserial: timestamp;        {last clock tick}
  lastrdataaddr: integer;        {current byte addr in constant data }
  lastrdatabyte: byte;            {current byte read at lastrdataaddr}
  blockdepthtid: tempid;        {TEMP number of DEPTH expression}
  zeroexpr: ptn;            {dummy icode node for zero constant}
  trueexpr: ptn;            {dummy icode node for true}
  cardinalvarnode: varnodep;        {dummy varnode for 0..32767 type }
  booleanvarnode: varnodep;        {dummy varnode for boolean }
  sideeffectinthisstmt: boolean;    {true if side effect in current stmt}
  lastfilepath: record            {last file pathname cache}
    lppath: filepath;        {the pathname}
    lpfnum: integer;        {its number}
    lpsize: 0..maxfilepath;        {its lengt}
    end;
                {Files used in pass2}
                {*******************}
  {switches to control output from pass2:
  debug: generate tree dump (pascal.dbg) char d
  }

  debugg: boolean;
  comments: boolean;            { comments desired in jcode? }

{
    Files read by second pass
}
  int: file of integer;        { file of intermediate code}
  dat: file of integer;        { intermediate home for data and case tables}
  vars: file of varitem;    { variable definitions }
  src: file of sourceline;    { file of source lines}
{
    Files written by second pass
}
  jcd: text;            { jcode }
  dbg: text;            { debug print }
{
    constant tables
}
optab: array [byte] of optabitem;    { operator table }
mttab: array [machinetype] of mttabitem;{ machine type table }
nulllineinfo: lineinfo;            { null line number for genlineid }
{
    Scope information  --  used in interpretation of
    VARBL, FIELD, and PARAM `levels', which are relative
    to the static procedure nest.       
}
    scopestack: array [0..scopedepthmax] of record{ procedure stack }
    scopepin: integer;            { procedure number this lev }
    nonscopes: integer;            { nested non-scopes (modules) }
    end;    
    scopedepth: 0..scopedepthmax;        { current nesting depth }
    blockdepth: 0..blockdepthmax;        { depth into block stack }
    blocksequence: integer;            { scope serial number }
{
    WITH information  --  state of enclosing WITH statements
}
    rtemptab: array [0..rtempmax] of ptn;    { values from WITH clauses }

{
    Substitution information -- used for actual/formal binding, etc.
}
    sbtop: substituteposition;            { size of subtab table }
    sbtab: array [1..maxsubstack] of subitem;    { substitutions table }
{
    Record type number information  --  used to distinguish record types
}
    rectab: recindextab;            { record type table }
