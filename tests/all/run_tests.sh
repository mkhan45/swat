tests=0
pass=0

for file in *.sax; do
	tests=$((tests + 1))
	../../sax "$file"
	mv "$file.val" "$file.mgl"
	sax -q "$file"
	if diff "$file.val" "$file.mgl" > /dev/null; then
		pass=$((pass + 1))
	else 
		echo "$file: fail"
	fi
done

rm *.val *.mgl
echo "$pass/$tests passed"
