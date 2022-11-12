SRC=$(find -name '*.c')
for s in $SRC
do
	clang -emit-llvm -O0 -g -c $s
done