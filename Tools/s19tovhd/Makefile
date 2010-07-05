
MKFRAGS := ../../mkfiles
export MKFRAGS

C_SRC := S19toVHD.cpp
HEADERS := 

include $(MKFRAGS)/def_cmds.mk

S19toVHD$(EXE_EXT): $(C_SRC) $(HEADERS)
	g++ -I. -DWIN32 -o $@ $<

clean:
	rm S19toVHD$(EXE_EXT)


