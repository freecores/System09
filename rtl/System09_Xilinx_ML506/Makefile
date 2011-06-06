#===================================================================
# File:        Makefile
# Author:      David Burnette 
# Created:     July 5, 2007
# 
# Description: 
#  Makefile to build the System09 by John Kent
# 
#  This makefile will build John Kent's entire System09 project 
#  (RTL synthesis and monitor ROMs) and even download the final 
#  bitstream to the prototype board.
# 
#  You can use Xilinx ISE interactively to add new RTL source files
#  to this project.
#
# Usage:
#  Use 'make help' to get a list of options.
#
# Dependencies:
#  Depends on makefile fragments in the 'MKFRAGS' directory.
#
# Revision History:
#   dgb  2007-07-05  Original version
#
#   dgb  2008-04-07  Split out files into fragments. Modified
#                    ROM source generation to be per src directory.
#
#===================================================================

MKFRAGS := ../../mkfiles
export MKFRAGS

BRAM_TYPE := b16
export BRAM_TYPE

#===================================================================
# User-modifiable variables
#
# This name must match the name of the design in Xilinx ISE (case
# sensitive). 
DESIGN_NAME := system09
#
# Constraint file (unfortunately it cannot be extracted from ISE)
UCF_FILE    := ml506.ucf
#
# Technology family (unfortunately it cannot be extracted from ISE)
FAMILY      := virtex5

# List of ROM VHDL files
.PHONY: roms
roms:
	@$(MAKE) -C ../../Tools/as09
	@$(MAKE) -C ../../Tools/s19tovhd
	@$(MAKE) -C ../../src/sys09bug sys09s3s.vhd
	@$(MAKE) -C ../../src/Flex9 flex9ram.vhd

#===================================================================
# You should not need to edit anything below this line

# XESS Tools
ifeq "$(findstring CYGWIN_NT,$(shell uname -s))" "CYGWIN_NT"
XESSPATH := $(shell cygpath "$(XSTOOLS_BIN_DIR)")
else
XESSPATH := $(XSTOOLS_BIN_DIR)
endif
XSLOAD     := "$(XESSPATH)/xsload.exe"

include ../../mkfiles/xilinx_rules.mk

#===================================================================
# TARGETS

.PHONY: all
all: bit 

.PHONE: tcl
tcl: roms $(DESIGN_NAME).tcl
	@$(ECHO) "Regenerating $(DESIGN_NAME).xst and $(DESIGN_NAME).prj from $(DESIGN_NAME).tcl"
	$(XILINX_NORMAL)/bin/$(XILINX_PLAT)/xtclsh system09.tcl rebuild_project

.PHONY: bit
bit: roms $(DESIGN_NAME).bit

.PHONY: impact
impact: roms bit do_impact

prom: roms $(DESIGN_NAME).mcs

.PHONY: help
help:
	@$(ECHO) "Use this Makefile to regenerate the entire System09 bitstream"
	@$(ECHO) "after modifying any of the source RTL or 6809 assembler code."
	@$(ECHO) ""
	@$(ECHO) "This makefile uses the following project files from the Xilinx ISE"
	@$(ECHO) "   $(XST_FILE)"
	@$(ECHO) ""
	@$(ECHO) "You use Xilinx ISE interactively to add new RTL source files."
	@$(ECHO) ""
	@$(ECHO) "            Availiable targets"
	@$(ECHO) 
	@$(ECHO) "  For building all or part of the system:"
	@$(ECHO) "    roms      - Run asm09 and then generate the VHDL RTL rom files"
	@$(ECHO) "    bit       - Rebuild the entire system and generate the bitstream file"
	@$(ECHO) "    all       - Rebuild everything"
	@$(ECHO) "    prom      - Rebuild the entire system and generate an MCS prom file"
	@$(ECHO) "    exo       - Rebuild the entire system and generate an EXO prom file"
	@$(ECHO) 
	@$(ECHO) "  For downloading the bitstream to the board:"
	@$(ECHO) "    xsload    - Download the bitstream to the FPGA via XSLOAD"
	@$(ECHO) "    usbxsload - Download the bitstream to the FPGA via XSLOAD"
	@$(ECHO) "    impact    - Download the bitstream to the FPGA via iMPACT"
	@$(ECHO) 
	@$(ECHO) "  For project maintenance:"
	@$(ECHO) "    help      - Print this help text"
	@$(ECHO) "    clean     - Clean up the ISE files"
	@$(ECHO) ""

.PHONY: clean
clean:
	-$(MAKE) -C ../../src/sys09bug clean
	-$(MAKE) -C ../../src/Flex9 clean
	-$(RM) *.ncd *.ngc *.ngd *.twr *.bit *.mcs *.stx *.ucf.untf *.mrp *.ptwx *_map.map
	-$(RM) *.ncl *.ngm *.prm *_pad.txt *.twx *.log *.syr *.par *.exo *.xpi *.xrpt *.xml
	-$(RM) *.cmd_log *.ngr *.bld *_summary.html *.nc1 *.pcf *.bgn tmp.ut
	-$(RM) *.pad *.placed_ncd_tracker *.routed_ncd_tracker *_pad.csv *.drc
	-$(RM) *.pad_txt $(DESIGN_NAME)_impact.cmd *.unroutes $(DESIGN_NAME)_vhdl.prj
	-$(RMDIR) _ngo _xmsgs xst xlnx_auto_0_xdb xst_tmp_dirs

.PHONY: cleanall
cleanall: clean
	-$(MAKE) -C ../../Tools/as09 clean
	-$(MAKE) -C ../../Tools/s19tovhd clean

debug_vars:
	@$(ECHO) "XST_FILE    = '$(XST_FILE)'"
	@$(ECHO) "PRJ_FILE    = '$(PRJ_FILE)'"
	@$(ECHO) "HDL_FILES   = '$(HDL_FILES)'"
	@$(ECHO) "PART        = '$(PART)'"
	@$(ECHO) "DEVICE      = '$(DEVICE)'"
	@$(ECHO) "SPEED       = '$(SPEED)'"
	@$(ECHO) "PACKAGE     = '$(PACKAGE)'"
	@$(ECHO) "UCF_FILE    = '$(UCF_FILE)'"
	@$(ECHO) "BSD_FILE    = '$(BSD_FILE)'"
	@$(ECHO) "XSTHDPDIR    = '$(XSTHDPDIR)'"
	@$(ECHO) "TMPDIR    = '$(TMPDIR)'"
