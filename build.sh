mkdir -p _build/src

path="_build/${@: -1}"

if [ ${path: -5} == ".byte" ];then
  ocamlc="-custom,"
else
  ocamlc=""
fi

corebuild -r -pkgs batteries,zip,yojson,fileutils,ounit \
-Is lib,src,src/classloader,src/opcode,src/instruction,tool,test,test/opcode,test/ocaml \
-lflags $ocamlc`pwd`/src/float32.c $*

if [[ "$path" =~ .*\.(byte|native)$ ]]; then
  name=`basename "$path"`
  rm "$name"
  mv "$path" ./
fi
