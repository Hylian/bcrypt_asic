#
# Based off the CMU ECE 18-447 Makefile by Joshua Wise, Yoongu Kim, Zhipeng Zhao
#


###################################################################
### Setup
###################################################################

TIMESTAMP := outputs/$(shell date +%m%d-%H%M%S)
OUTPUT ?= $(TIMESTAMP)

SYNTHDIR = dc
SYNTH = lab
TB = testbench

###################################################################
### Constants
###################################################################

# text attributes: normal, bold, underline
n=\e[0m
b=\e[1m
u=\e[4m

# bold+green
g=\e[1;32m

# bold+red
r=\e[1;31m

# debug message
m=$gBcrypt Makefile: $n

###################################################################
### Compile Verilog
###################################################################

compile:
	@echo -e "$mCopying Verilog into $(OUTPUT)..."
	@mkdir -p $(OUTPUT)/sim/src
	@cp `find src -iname '*.sv' -o -iname '*.v' -o -iname '*.vh'`	$(OUTPUT)/sim/src
	@echo -e "$mCompiling Verilog..."
	cd $(OUTPUT)/sim; ncvlog -SV -linedebug -messages -incdir src src/*.v $(shell ls src/*.sv)
	@if grep '*W' $(OUTPUT)/sim/ncvlog.log >/dev/null; \
		then echo -e '$m$rCompiler log has warnings!$n'; fi
	@echo -e "$mElaborating Verilog..."
	cd $(OUTPUT)/sim; ncelab +access+rwc -messages worklib.$(TB)
	@if grep '*W' $(OUTPUT)/sim/ncelab.log >/dev/null; \
		then echo -e '$m$rElaborator log has warnings!$n'; fi

###################################################################
### Simulate Verilog
###################################################################

sim: compile 
	@echo -e "$mCopying NCSim configuration into $(OUTPUT)..."
	@cp ncsim/* $(OUTPUT)/sim
	@echo -e "$mSimulating Verilog in $(OUTPUT)..."
	cd $(OUTPUT)/sim; ncsim worklib.$(TB):module -input ncsim.tcl
	@if grep '*W' $(OUTPUT)/sim/ncsim.log >/dev/null; \
		then echo -e '$m$rSimulator log has warnings!$n'; fi
	@echo -e "$mSimulation has completed in $(OUTPUT)."
	@echo -e "$mTo view waveforms, execute the following command: $bsimvision $(OUTPUT)/sim/waveforms.shm$n"

###################################################################
### Synthesize Verilog
###################################################################

synth:
	@mkdir -p $(OUTPUT)/synth/src
	@cp `find src -iname '*.v' -o -iname '*.sv' -o -iname '*.vh'` $(OUTPUT)/synth/src
	@cp $(SYNTHDIR)/$(SYNTH).dc $(OUTPUT)/synth
	@cd $(OUTPUT)/synth; dc_shell-xg-t -f $(SYNTH).dc
	@echo -e "$mSynthesis has completed in $(OUTPUT)."
	@echo -e "Timing results can be found at $(OUTPUT)/synth/timing.rpt"
	@echo -e "Power results can be found at $(OUTPUT)/synth/power.rpt"
	@echo -e "Area results can be found at $(OUTPUT)/synth/area.rpt"
	@echo -e "Netlist can be found at $(OUTPUT)/synth/netlist.sv"
