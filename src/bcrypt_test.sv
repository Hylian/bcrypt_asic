module test;

	logic clk, clk_0, clk_l, rst_l, clk_1, clk_2, clk_2_1; 
	logic clk_wr_addr, clk_rw_sel, clk_p_xor0, clk_p_xor;
	logic clk_3, en_clk_1, en_clk_2, en_clk_3;
	logic start, en, en_1, en_2, en_3, en_4, en_5, cost0;
	logic [63:0] cost;
	logic [6:0] wr_addr;
	logic [31:0] re_addr, re_addr_in;
	logic csp, cs0, cs1, cs2, cs3, data_ready;
	logic [6:0] wl_0, wl_1, wl_2, wl_3;
	logic d_sel0, d_sel1, d_sel2, d_sel3, salt_key_sel;
	logic [63:0] bl_0, bl_1, bl_2, bl_3;
	logic [31:0] l, r;
	logic [8:0] psel;
    logic rx, tx;
    logic [31:0] s0data, s1data, s2data, s3data;

	clock_fsm m1(en, en_1, en_2, en_3, en_4, cost0, 
			 clk, rst_l, clk_3, 
		     clk_0, clk_l, clk_1, clk_2, clk_2_1, en_clk_2, en_clk_1,
			 clk_wr_addr, clk_rw_sel, 
			 clk_p_xor0, clk_p_xor, clk_ctext_load);

	cycle_count m2(clk_2, rst_l, clk, start, 
		       en, en_1, en_2, en_3, en_4, clk_3,
			   en_clk_2, en_clk_3);

	state_fsm m3(clk, clk_3, rst_l, cost, start, 
		     en, en_1, en_2, en_3, en_4, en_5, cost0);

	addr_calc m4(clk_wr_addr, rst_l, en_4, 
		     wr_addr, csp, cs0, cs1, cs2, cs3);

	sram_ctrl m5(clk_wr_addr, clk_rw_sel, clk_1, clk_2, rst_l,
		     l,      r,    wr_addr, re_addr,
		     cs0,    cs1,    cs2,    cs3, data_ready,
		     wl_0,   wl_1,   wl_2,   wl_3,
		     d_sel0, d_sel1, d_sel2, d_sel3,
		     bl_0,   bl_1,   bl_2,   bl_3);
	
    p_ctrl m6(clk_1, clk_2, rst_l, clk_wr_addr, clk_p_xor, 
			  wr_addr, csp, psel, salt_key_sel);

    bcrypt m7(clk, rst_l, rx, tx, clk_0, clk_1, clk_2, clk_2_1, clk_3, clk_wr_addr,
			  clk_ctext_load, clk_p_xor0, clk_p_xor,
			  en_clk_3, en_clk_2, en_1, en_2, en_3, en_4, salt_key_sel, psel,
			  s0data, s1data, s2data, s3data, re_addr, l, r);

	always begin
	  #50 clk = ~clk;
	end

	/* data_ready variable (output from sram) */
	always_ff @(negedge clk_1, negedge clk_2, 
		    posedge data_ready, negedge rst_l) begin
	  if (~rst_l) begin
	    data_ready <= 0;
	  end
	  else if (data_ready) begin
	    data_ready <= #15 0;
	  end
	  else begin
	    data_ready <= #15 1;
	  end
	end

	always_ff @(posedge clk_1) begin
	  $display("xor new R with p%d", (m1.count-5'd1));
	end

	always_ff @(posedge clk_2) begin
	  $display("**********************************");
	end

	initial begin
	  $monitor("    tcount=%d    ,", 
				m2.count);
	  clk = 0;
	  cost = 64'd1;
	  rst_l = 1;
	  start = 0;
	  $display("******** t=0 *********\n");
	  #10 
	  $display("******* reset ********\n");
	  rst_l = 0;
	  #10 
	  $display("***** un-reset *******\n");
	  rst_l = 1;
	  #1000 
	  $display("******* start ********\n");
	  start = 1;
	  //#5000
	  #1000000
	  //#7000000
	  $display("******* reset ********\n");
	  rst_l = 0;
	  start = 0;
	  #1000
	  $display("***** un-reset *******\n");
	  rst_l = 1;
	  #1000 
	  $display("******* start ********\n");
	  start = 1;
	  #5000 $finish;
	end
endmodule
