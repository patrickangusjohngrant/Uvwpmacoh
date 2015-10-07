OCAMLMAKEFILE=OCamlMakefile

SOURCES=pipes.ml util.ml web.ml filesystem.ml fuseimpl.ml init.ml
RESULT=init
CLIBS=
LIBS=
THREADS=threads
TRASH=*~ \\\#*
CFLAGS=-D_FILE_OFFSET_BITS=64 -I. -pthread -g
LDFLAGS=

PACKS=core async yojson Fuse pcre uri cohttp.async
LIBDIRS= 
INCDIRS=$(LIBDIRS)

target: native-code

-include $(OCAMLMAKEFILE)
