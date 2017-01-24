corebuild -r -pkgs batteries,zip,yojson,fileutils -Is lib,src,src/classloader,tool $*

path="_build/${@: -1}"

if [[ "$path" =~ .*\.(byte|native)$ ]]; then
  name=`basename "$path"`
  rm "$name"
  mv "$path" ./
fi
