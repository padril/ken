current := $(shell pwd)
build := $(current)/build
obj := $(build)/objects
src := $(current)/src

depend := $(build)/.depend
haskell_obj := $(obj)/haskell
haskell_src := $(src)/haskell

ghc := $(shell which ghc)
ghc_opt := -i$(haskell_obj) -outputdir $(haskell_obj) -v


.PHONY: build
build : setup depend ken

ken : $(haskell_obj)/Main.o
	ghc -o $(build)/$@ $(wildcard $(haskell_obj)/*.o)

$(haskell_obj)/%.o : $(haskell_src)/%.hs
	$(ghc) $(ghc_opt) -c $<

# %.o : %.s
# 	nasm -f elf64 -O3 $< -o $@
# 	#nasm -f elf64 -g -Fdwarf $< -o dwarf_$@
	
.PHONY: clean
clean :
	rm -f $(wildcard $(haskell_obj)/*.o)
	rm -f $(wildcard $(haskell_obj)/*.hi)
	rm -f $(depend)
	
.PHONY: setup
setup :
	mkdir -p $(build) $(obj) $(haskell_obj)

.PHONY: depend
depend :
	$(ghc) \
		-dep-suffix '' \
		-dep-makefile '$(depend)' \
		-M $(ghc_opt) $(haskell_src)/*.hs
	sed -i '' $(depend)


# GHC doesn't like making nice looking dependencies, so we need to do some path
# manipulation magic. see build/.depend for why
hi_path := $(haskell_obj:$(current)/%=%)
$(hi_path)/%.hi : $(haskell_obj)/%.o
	@:

ifeq (,$(filter clean,$(MAKECMDGOALS)))
include $(depend)
endif

$(depend) : setup depend

