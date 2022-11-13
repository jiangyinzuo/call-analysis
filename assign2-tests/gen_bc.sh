SRC=$(find -name '*.c')
for s in $SRC
do
	echo $s
	clang -emit-llvm -O0 -g -c $s
done