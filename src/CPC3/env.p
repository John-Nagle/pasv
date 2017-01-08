#include "global.h"

procedure WriteEnvId;
begin
  writeln('env.p 1.6') end;

function AllocateEnv;
var
  NewNext: integer; (* The value of NextEnv after allocation *)
  j: EnvIndex;
begin
  (* See if there is room for a new environment *)
  NewNext := NextEnv + EnvLength;
  if NewNext > EnvPoolSize then begin
    writeln('environment overflow');
    Abort end;

  (* Clear the new environment *)
  if NewNext-1 >= NextEnv then 		(* avoid compiler bug *)
  for j := NextEnv to NewNext-1 do 
    EnvPool[j] := 0;

  (* Return it. *)
  AllocateEnv := NextEnv;
  NextEnv := NewNext end;

procedure CopyEnv;
var X: VariableIndex;
begin
  for X := 0 to EnvLength-1 do 
    EnvPool[Cpy+X] := EnvPool[Org+X] end;
