module sram(
	clk, rst_l, wl, rw_sel, bl,
	data_ready, op_type
	);

	input bit clk, rst_l;
	input bit [7:0] wl;
	input bit rw_sel;      //0 read, 1 write
	input bit [63:0] bl;

	output bit data_ready;
	output bit op_type;

	bit bl_ready;
	bit wl_ready;
	bit op_in;

	always_ff @(posedge clk, negedge rst_l) begin
	  if (~rst_l) begin
	    bl_ready <= 0;
	    op_in <= 0;
	    data_ready <= 0;
	    op_type <= 0;
	  end
	  else begin
	    bl_ready <= ((bl == {64{1'b1}}) || rw_sel);
	    op_in <= rw_sel;
	    data_ready <= wl_ready;
	    op_type <= op_in;
	  end
	end

	always_ff @(negedge clk, negedge rst_l) begin
	  if (~rst_l) begin
	    wl_ready <= 0;
	  end
	  else begin
	    wl_ready <= bl_ready;
	  end
	end

endmodule : sram
