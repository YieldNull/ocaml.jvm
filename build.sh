gcc -c -o _build/src/float32.o -I /usr/local/lib/ocaml src/float32.c
corebuild -r -pkgs batteries,zip,yojson,fileutils -Is lib,src,src/classloader,tool \
-lflags -custom,src/float32.o $*

path="_build/${@: -1}"

if [[ "$path" =~ .*\.(byte|native)$ ]]; then
  name=`basename "$path"`
  rm "$name"
  mv "$path" ./
fi
