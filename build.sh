mkdir -p _build/src

gcc -c -o _build/src/float32.o -I /usr/local/lib/ocaml src/float32.c

corebuild -r -pkgs batteries,zip,yojson,fileutils,ounit \
-Is lib,src,src/classloader,src/opcode,src/instruction,tool,test,test/opcode,test/ocaml \
-lflags -custom,src/float32.o $*

path="_build/${@: -1}"

if [[ "$path" =~ .*\.(byte|native)$ ]]; then
  name=`basename "$path"`
  rm "$name"
  mv "$path" ./
fi
