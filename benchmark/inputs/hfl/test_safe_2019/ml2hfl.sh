#/bin/bash
if [ -d $1 ]; then
  dir=$1
  mkdir -p out/$1
  for file in `ls $1 | grep -E '\.ml$'`; do
    # echo "### $file"
    ml2hfl "$dir/$file" > "out/$dir/${file%.*}.in"
    if [ $? -ne 0 ]; then
      echo "$dir/$file failed"
    fi
  done
elif [ -f $1 ]; then
  dir=${1%/*}
  file=${1##*/}
  mkdir -p out/$dir
  ml2hfl "$dir/$file" > "out/$dir/${file%.*}.in" || echo "failed."
else
  echo "$dir is not directory"
  exit 1
fi
