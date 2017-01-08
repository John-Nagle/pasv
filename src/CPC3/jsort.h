(* The following two declarations are used to bring in library routines to
 * do direct access I/O on text files.
 *)

procedure OurSeek(var f: text; n: FilePosition);
(* This routine moves f^ to the place returned by OurTell *)
  external;

function OurTell(var f: text): FilePosition;
(* This routine returns the position of f^ in f. *)
  external;
