module test;

	logic clk, clk_0, clk_l, rst_l, clk_1, clk_2, clk_2_1; 
	logic clk_wr_addr, clk_rw_sel, clk_p_xor0, clk_p_xor;
	logic clk_3, en_clk_1_0, en_clk_1_17, en_clk_2, en_clk_3;
	logic start, load, en, en_1, en_2, en_3, en_4, en_5, cost0;
	logic int_rst_l, ext_rst_l;
	logic [63:0] cost_a;
	logic [5:0] cost;
	logic [127:0] salt;
	logic [575:0] salt_c, key, key_c;
	logic [6:0] wr_addr;
	logic [31:0] re_addr, re_addr_in;
	logic csp, cs0, cs1, cs2, cs3, data_ready;
	logic [6:0] wl_0, wl_1, wl_2, wl_3;
	logic d_sel0, d_sel1, d_sel2, d_sel3, salt_key_sel;
	logic [63:0] bl;
	logic [31:0] l, r;
	logic [8:0] psel;
    logic rx, tx;
    logic [31:0] s0data, s1data, s2data, s3data;
	logic [31:0] ct_Orph, ct_eanB, ct_ehol, ct_derS, ct_cryD, ct_oubt;
	logic [325:0] hash;

	clock_fsm m1(en, en_1, en_2, en_3, en_4, cost0, load,
			 clk, rst_l, clk_3, 
		     clk_0, clk_l, clk_1, clk_2, clk_2_1, 
			 en_clk_2, en_clk_1_0, en_clk_1_17,
			 clk_wr_addr, clk_rw_sel, 
			 clk_p_xor0, clk_p_xor, clk_ctext_load);

	cycle_count m2(clk_2, rst_l, clk, start, 
		       en, en_1, en_2, en_3, en_4, clk_3,
			   en_clk_2, en_clk_3);

	state_fsm m3(clk, clk_3, cost_a, start, 
			 int_rst_l, ext_rst_l,
		     en, en_1, en_2, en_3, en_4, en_5, cost0, rst_l);

	addr_calc m4(clk_wr_addr, rst_l, en_4, 
		     wr_addr, csp, cs0, cs1, cs2, cs3);

	sram_ctrl m5(clk_wr_addr, clk_rw_sel, clk_1, clk_2, rst_l, en_clk_1_17,
		     l,      r,    wr_addr, re_addr, data_ready,
		     wl_0,   wl_1,   wl_2,   wl_3,
		     d_sel0, d_sel1, d_sel2, d_sel3,
		     bl);
	
    p_ctrl m6(clk_1, clk_2, rst_l, clk_wr_addr, clk_p_xor, 
			  wr_addr, csp, psel, salt_key_sel);

	load_costsaltkey m7(cost, key, salt, clk_0, rst_l,
					 cost_a, key_c, salt_c, start);

	output_rst m8(clk, ext_rst_l, cost, salt,
				  ct_Orph, ct_eanB, ct_ehol, ct_derS, ct_cryD, ct_oubt,
				  en_5, hash, int_rst_l);

    bcrypt m9(clk, rst_l, start,
			  rx, tx, 
			  clk_0, clk_1, clk_2, clk_2_1, clk_3,
			  clk_wr_addr, clk_ctext_load, clk_p_xor0, clk_p_xor,
			  en_clk_3, en_clk_2, en_1, en_2, en_3, en_4,
			  cost0, salt_key_sel, psel,
			  s0data, s1data, s2data, s3data, re_addr, salt_c, key_c,
			  l, r, ct_Orph, ct_eanB, ct_ehol, ct_derS, ct_cryD, ct_oubt);

	sram m10(clk_1, clk_2, rst_l, {wl_3, wl_2, wl_1, wl_0}, clk_rw_sel, 
			bl, d_sel0, d_sel1, d_sel2, d_sel3,
			cs0, cs1, cs2, cs3,
			data_ready, s0data, s1data, s2data, s3data);

	always begin
	  #50 clk = ~clk;
	end

	/* data_ready variable (output from sram) */
	/*
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
	*/

	always @(posedge int_rst_l) begin
	  $display("hash=%d", hash);
	end

	initial begin
	  $monitor("state=%d, rounds_left=%d", m3.state, m3.cost_curr);
	  clk = 0;
	  cost = 5'd8;		//cost input
	  salt = 128'd10;	//salt input
	  key = 576'd89;	//key  input
	  ext_rst_l = 1;
	  load = 0;
	  $display("******** t=0 *********\n");
	  #10 
	  $display("******* reset ********\n");
	  ext_rst_l = 0;
	  #10 
	  $display("***** un-reset *******\n");
	  ext_rst_l = 1;
	  #1000 
	  $display("******* load ********\n");
	  load = 1;
	  //#50000
	  //#1000000
	  //#7000000
	  #1000000000
	  $display("******* done ********\n");
	  //ext_rst_l = 0;
	  //load = 0;
	  //#1000
	  //$display("***** un-reset *******\n");
	  //ext_rst_l = 1;
	  //#1000 
	  //$display("******* start ********\n");
	  //load = 1;
	  #5000 $finish;
	end
endmodule
