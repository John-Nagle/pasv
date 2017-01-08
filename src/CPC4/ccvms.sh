
#
#	ccvms   --   perform c compile, generating VMS object if
#		     running under Eunice
#
#			Required only for files to be read in with cfasl
#			in Lisp.
#
#	Force VMS outputs under Eunice; no action under Unix
#
AS_IMAGE=/bin/vmsas
LD_IMAGE=/bin/vmsld
export AS_IMAGE
export LD_IMAGE
#	Do the compile
cc $*
