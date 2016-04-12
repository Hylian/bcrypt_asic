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
    logic rx, tx, load_en;
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

	load_costsaltkey m7(cost, key, salt, clk_0, clk_2_1, rst_l,
					 cost_a, key_c, salt_c, start, load_en);

	output_rst m8(clk, ext_rst_l, cost, salt,
				  ct_Orph, ct_eanB, ct_ehol, ct_derS, ct_cryD, ct_oubt,
				  en_5, hash, int_rst_l);

    bcrypt m9(clk, rst_l, start, load_en,
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

	string base64;
	
	bit [5:0] index[0:52];
	
	always_comb begin
	  index[0]  = hash[319:314];
	  index[1]  = hash[313:308];
	  index[2]  = hash[307:302];
	  index[3]  = hash[301:296];
	  index[4]  = hash[295:290];
	  index[5]  = hash[289:284];
	  index[6]  = hash[283:278];
	  index[7]  = hash[277:272];
	  index[8]  = hash[271:266];
	  index[9]  = hash[265:260];
	  index[10] = hash[259:254];
	  index[11] = hash[253:248];
	  index[12] = hash[247:242];
	  index[13] = hash[241:236];
	  index[14] = hash[235:230];
	  index[15] = hash[229:224];
	  index[16] = hash[223:218];
	  index[17] = hash[217:212];
	  index[18] = hash[211:206];
	  index[19] = hash[205:200];
	  index[20] = hash[199:194];
	  index[21] = {hash[193:192], 4'b0};
	  index[22] = hash[191:186];
	  index[23] = hash[185:180];
	  index[24] = hash[179:174];
	  index[25] = hash[173:168];
	  index[26] = hash[167:162];
	  index[27] = hash[161:156];
	  index[28] = hash[155:150];
	  index[29] = hash[149:144];
	  index[30] = hash[143:138];
	  index[31] = hash[137:132];
	  index[32] = hash[131:126];
	  index[33] = hash[125:120];
	  index[34] = hash[119:114];
	  index[35] = hash[113:108];
	  index[36] = hash[107:102];
	  index[37] = hash[101:96];
	  index[38] = hash[95:90];
	  index[39] = hash[89:84];
	  index[40] = hash[83:78];
	  index[41] = hash[77:72];
	  index[42] = hash[71:66];
	  index[43] = hash[65:60];
	  index[44] = hash[59:54];
	  index[45] = hash[53:48];
	  index[46] = hash[47:42];
	  index[47] = hash[41:36];
	  index[48] = hash[35:30];
	  index[49] = hash[29:24];
	  index[50] = hash[23:18];
	  index[51] = hash[17:12];
	  index[52] = {hash[11:8], 2'b0};
	end


	always_ff @(negedge int_rst_l) begin
	  if (en_5) begin
	  	$display("hash=%b", hash);
	  	$write("$2a$%h$", hash[325:320]);
	  	for (int i=0; i<53; i++) begin
		  $write("%0s", base64[index[i]]);
	  	end
	  	$display("\n");
		$finish;
	  end
	end


/*
	always_ff @(posedge clk_2) begin
	  if (en_3) begin
		$display("l=%h, r=%h", l, r);
	  end
	end
*/

/*
	always_ff @(negedge clk_1, negedge clk_2, negedge clk_2_1) begin
	  $display("l=%h\nr=%h\nre_addr=%h", l, r, re_addr);
	  $display("feistelXorMem=%h\nP0=%h\nenclk2=%d\n",
				m9.feistelXorMem, m9.P0, en_clk_2);
	end
*/

/*
	always_ff @(negedge int_rst_l) begin
	  $display("hash=%b", hash);
	end
*/

	initial begin
	  $monitor("state=%d, rounds_left=%d", m3.state, m3.cost_curr);
	  clk = 0;
	  cost = 5'd4;		//cost input
	  salt = 128'hb9e40330d2c10bbd8bd30cbd0220ceea;	//salt input
	  key = 576'h6d796b6579;	//key  input
	  base64 = "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
	  ext_rst_l = 1;
	  load = 0;
	  //$display("******** t=0 *********\n");
	  #10 
	  //$display("******* reset ********\n");
	  ext_rst_l = 0;
	  #10 
	  //$display("***** un-reset *******\n");
	  ext_rst_l = 1;
	  #1000 
	  //$display("******* load ********\n");
	  load = 1;
	  //#1000000
	  //#7000000
	  //#1000000000
	  //$display("******* done ********\n");
	  //ext_rst_l = 0;
	  //load = 0;
	  //#1000
	  //$display("***** un-reset *******\n");
	  //ext_rst_l = 1;
	  //#1000 
	  //$display("******* start ********\n");
	  //load = 1;
	  //#5000 $finish;
	end
endmodule
