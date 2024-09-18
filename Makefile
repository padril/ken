test: test.o lib.o
	ld $^ -o $@

%.o: %.s
	nasm -f elf64 -O3 $< -o $@
	#nasm -f elf64 -g -Fdwarf $< -o dwarf_$@
	
.PHONY: clean
clean:
	rm *.o

