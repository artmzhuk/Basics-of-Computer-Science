
countLines(){
  TOTAL=0
  for file in `find "$1" -name "*.c" -o -name "*.h"`
  do
    numOfLines=$(grep -cve '^\s*$' $file)
    TOTAL=$(($TOTAL + $numOfLines))
    echo $file $numOfLines
  done
  echo "Количество непустых строк:" $TOTAL
}

countLines "$1"
