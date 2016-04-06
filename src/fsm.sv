module state_fsm(
	clk, clk_3, rst_l, cost, start,
	en,   en_1, en_2,
	en_3, en_4, en_5, cost0
	);

	/* Inputs */
	input bit clk, clk_3, rst_l, start;
	input bit [63:0] cost;

	/* Outputs */
	output bit en,   en_1, en_2;
	output bit en_3, en_4, en_5, cost0;

	/* State Logic */
	bit [2:0] state, state_n, state_m;

	/* Intermediary Logic */
	bit [2:0] d_0, d_1, d_2, d_3, d_4, d_5;
	bit [63:0] cost_curr, cost_n;
	bit costshift, sel0, sel1;

	/* FF for keeping track of cost */
	always_ff @(posedge costshift, negedge rst_l) begin
	  if (~rst_l) begin
	    cost_curr <= 0;
	  end
	  else if (costshift) begin
	    cost_curr <= cost_n;
	  end
	end

	/* State FF */
	always_ff @(posedge clk_3, negedge rst_l) begin
	  if (~rst_l) begin
	    state <= 0;
	  end
	  else if (clk_3) begin
	    state <= state_n;
	  end
	end

	/* FFs for state En signals */
	always @(negedge clk, negedge rst_l) begin
	  if (~rst_l) begin
		en   <= 0;
	    en_1 <= 0;
	    en_2 <= 0;
	    en_3 <= 0;
	    en_4 <= 0;
	    en_5 <= 0;
	  end
	  else if (~clk) begin
		en   <= d_0;
	    en_1 <= d_1;
	    en_2 <= d_2;
	    en_3 <= d_3;
	    en_4 <= d_4;
	    en_5 <= d_5;
	  end
	end
	
	/* Logic for enable signals */
	always_comb begin
	  d_0 = ((state < 3'd5) && (state > 0));
	  d_1 = (state == 3'd1);
	  d_2 = (state == 3'd2);
	  d_3 = (state == 3'd3);
	  d_4 = (state == 3'd4);
	  d_5 = (state == 3'd5);
	end

	assign state_m = (sel0) ? 2 : (state + 1'd1);
	assign state_n = (sel1) ? 4 : state_m;

	/* Logic for keeping track of cost */
	and g0(sel0, en_3, ~(cost0));
	and g1(sel1, en_1, cost0);
	assign cost0 = (cost_curr == 0);
	assign cost_n = (en) ? (cost_curr - 64'd1) : cost;
	assign costshift = (en) ? en_3 : start;

endmodule : state_fsm

/* fsm for main gaited clock, 18 + 1 cycles */
module clock_fsm (
	en, en_1, en_2, en_3, en_4, cost0,
	clk, rst_l, clk_3,
	clk_0, clk_l, clk_1, clk_2, clk_2_1, 
	en_clk_2, en_clk_1,
	clk_wr_addr, clk_rw_sel, clk_p_xor0, clk_p_xor, clk_ctext_load
	);

	/* Inputs */
	input bit en, en_1, en_2, en_3, en_4, cost0, clk, rst_l, clk_3;

	/* Outputs */
	output bit clk_0, clk_l, clk_1, clk_2, clk_2_1, en_clk_2, en_clk_1;
	output bit clk_wr_addr, clk_rw_sel, clk_p_xor0, clk_p_xor, clk_ctext_load;

	/* Intermediary logic */
	bit [4:0] count, count_n;
	bit d_1, d_2, d_3, d_4;
	bit q_1, q_2, q_3, q_4;
	bit go;
	bit addr_en;

	/* FF for cycle count */
	always_ff @(posedge go, negedge rst_l) begin
	  if (~rst_l) begin
	    count <= 0;
	  end
	  else if (go) begin
	    count <= count_n;
	  end
	end

	/* latches for clock gating */
	always @(clk_l, d_1) begin
	  q_1 <= (clk_l) ? d_1 : q_1;
	end
	
	always @(clk_l, d_2) begin
	  q_2 <= (clk_l) ? d_2 : q_2;
	end

	always @(clk_l, d_3) begin
	  q_3 <= (clk_l) ? d_3 : q_3;
	end

	always @(clk_l, d_3) begin
	  q_4 <= (clk_l) ? d_3 : q_4;
	end

	/* enable signal for clk_2 */
	always_ff @(negedge clk, negedge rst_l) begin
	  if (~rst_l) begin
	    en_clk_2 <= 0;
	  end
	  else if (~clk) begin
	    en_clk_2 <= d_2;
	  end
	end

	/* enable signal for first clk_1 in cycle */
	always_ff @(posedge clk, negedge rst_l) begin
	  if (~rst_l) begin
	    d_4 <= 0;
	  end
	  else begin
	    d_4 <= en_clk_2;
	  end
	end

	always_ff @(negedge clk, negedge rst_l) begin
	  if (~rst_l) begin
	    en_clk_1 <= 0;
	  end
	  else begin
	    en_clk_1 <= d_4;
	  end
	end

	/* clock signal for write address calculation */
	always_ff @(posedge clk_2, negedge clk_1) begin
	  clk_wr_addr <= (clk_2 && (en_1 || en_2 || en_3));
	end

	/* clock signal for read/write select */
	always_ff @(posedge clk_2, posedge clk_1) begin
	  clk_rw_sel <= clk_2;
	end

	/* clock signal for p xor key (first) */
	always_ff @(posedge clk_2_1, negedge clk_1) begin
	  clk_p_xor0 <= clk_2_1;
	end

	/* clock signal for p xor key, salt (subsequent) */
	always_ff @(posedge clk_3, negedge clk_1) begin
	  clk_p_xor <= (clk_3 && (((~cost0) && (en_1 || en_3)) || en_2));
	end

	/* clock signal for ctext load */
	always_ff @(posedge clk_2, negedge clk_1) begin
	  clk_ctext_load <= (clk_2 && en_4);
	end

	not g0(clk_l, clk);
	and g1(clk_1, q_1, clk, en);
	and g2(clk_2, q_2, clk, en);
	and g3(clk_2_1, q_3, clk, en);
	or  g4(addr_en, en_1, en_2, en_3);
	and g5(clk_0, q_4, clk);

	assign go = (en) ? clk : 0;
	assign d_2 = (count > 5'd18);
	assign count_n = (d_2) ? 5'd1 : (count + 5'd1);
	assign d_3 = (count == 5'd0);
	assign d_1 = (~(d_2 || d_3));

endmodule : clock_fsm

/* cycle count, controls ready signal for state fsm */
module cycle_count (
	clk_2, rst_l, clk, start, 
	en, en_1, en_2, en_3, en_4, clk_3,
	en_clk_2, en_clk_3
	);

	/* Inputs */
	input bit clk_2, rst_l, clk, start, en_clk_2;
	input bit en, en_1, en_2, en_3, en_4;

	/* Output */
	output bit clk_3, en_clk_3;

	/* Intermediary logic */
	bit [9:0] count, count_n;
	bit [9:0] comp;
	bit sel;
	bit d0, d1, d2;
	bit ready;

	/* FF for cycle count */
	always_ff @(posedge clk_2, negedge rst_l) begin
	  if (~rst_l) begin
	    count <= 0;
	  end
	  else if (clk_2) begin
	    count <= count_n;
	  end
	end

	/* FF for start / ready signals */
	always_ff @(posedge start, posedge en_4, negedge rst_l) begin
	  if ((~rst_l) || en_4) begin
	    ready <= 0;
	  end
	  else begin
	    ready <= 1;
	  end
	end

	/* FF for clk_3 signal */
	always @(posedge clk, negedge rst_l) begin
	  if (~rst_l) begin
	    d1 <= 0;
	  end
	  else if (clk) begin
	    d1 <= d0;
	  end
	end

	/* en_clk_3 signal */
	and g1(en_clk_3, d1, en_clk_2);

	or g0(sel, en_1, en_2, en_3);

	assign clk_3 = (en) ? (d1 && clk_2) : ready;
	assign comp = (sel) ? 10'd519 : 10'd190;
	assign d0 = (count > comp);
	assign count_n = (d0) ? 0 : (count + 1);

endmodule : cycle_count

module addr_calc(
	clk_addr, rst_l, en_4, wr_addr,
	csp, cs0, cs1, cs2, cs3
	);

	input bit clk_addr, rst_l, en_4;
	output bit [6:0] wr_addr;
	output bit csp, cs0, cs1, cs2, cs3;

	/* intermediary logic */
	bit d0, d1, en_cs;
	bit [6:0] wr_addr_n, comp;
	bit [2:0] cs, cs_n;

	/* FF for calculating next addr */
	always_ff @(negedge clk_addr, negedge rst_l) begin
	  if (~rst_l) begin
	    wr_addr <= 0;
	  end
	  else if (~clk_addr) begin
	    wr_addr <= wr_addr_n;
	  end
	end

	/* FF for determining chip select */
	always_ff @(negedge d0, negedge rst_l) begin
	  if (~rst_l) begin
	    cs <= 0;
	  end
	  else if (~d0) begin
	    cs <= cs_n;
	  end
	end

	/* FF for determining cs disable */
	always_ff @(posedge en_4, negedge rst_l) begin
	  if (~rst_l) begin
	    en_cs <= 0;
	  end
	  else if (en_4) begin
	    en_cs <= 1;
	  end
	end

	assign comp = (csp) ? 7'd7 : 7'd126;
	assign d0 = (wr_addr > comp);
	assign wr_addr_n = (d0) ? 0 : (wr_addr + 7'd1);
	assign d1 = (cs > 3'd3);
	assign cs_n = (d1) ? 0 : (cs + 3'd1);
	assign csp = ((cs == 3'd0) && ~(en_cs));
	assign cs0 = ((cs == 3'd1) && ~(en_cs));
	assign cs1 = ((cs == 3'd2) && ~(en_cs));
	assign cs2 = ((cs == 3'd3) && ~(en_cs));
	assign cs3 = ((cs == 3'd4) && ~(en_cs));

endmodule : addr_calc

module sram_ctrl(
	clk_wr_addr, clk_rw_sel, clk_1, clk_2, rst_l,
	l, r, wr_addr, re_addr,
	cs0, cs1, cs2, cs3, data_ready,
	wl_0, wl_1, wl_2, wl_3,
	d_sel0, d_sel1, d_sel2, d_sel3,
	bl_0, bl_1, bl_2, bl_3
	);

	/* Input logic */
	input bit clk_wr_addr, clk_rw_sel, clk_1, clk_2, rst_l;
	input bit cs0, cs1, cs2, cs3, data_ready;
	input bit [31:0] l, r, re_addr;
	input bit [6:0] wr_addr;

	/* Output logic */
	output bit [6:0] wl_0, wl_1, wl_2, wl_3;
	output bit d_sel0, d_sel1, d_sel2, d_sel3;
	output bit [63:0] bl_0, bl_1, bl_2, bl_3;

	/* FFs for addr, data in, precharge */
	bit [63:0] data_in;
	bit [6:0] wr_addr_in;
	bit [31:0] re_addr_in;
	bit [63:0] precharge;

	always_ff @(posedge clk_wr_addr, posedge data_ready, 
		    negedge rst_l) begin
	  if ((~rst_l) || data_ready) begin
	    data_in <= 0;
	  end
	  else begin
	    data_in <= {l, r};
	  end
	end

	always_ff @(posedge data_ready, negedge clk_2, negedge rst_l) begin
	  if ((~rst_l) || data_ready) begin
	    wr_addr_in <= 0;
	  end
	  else begin
	    wr_addr_in <= wr_addr;
	  end
	end

	always_ff @(posedge data_ready, negedge clk_1, negedge rst_l) begin
	  if ((~rst_l) || data_ready) begin
	    re_addr_in <= 0;
	  end
	  else begin
	    re_addr_in <= re_addr;
	  end
	end

	always_ff @(posedge clk_1, posedge data_ready, negedge rst_l) begin
	  if ((~rst_l) || data_ready) begin
	    precharge <= 0;
	  end
	  else begin
	    precharge <= {64{1'b1}};
	  end
	end

	/* bitline muxing */
	bit [63:0] d0, d1, d2, d3;
	bit [63:0] in_0, in_1, in_2, in_3;
	
	and g0[63:0] (d0, {64{cs0}}, data_in);
	and g1[63:0] (d1, {64{cs1}}, data_in);
	and g2[63:0] (d2, {64{cs2}}, data_in);
	and g3[63:0] (d3, {64{cs3}}, data_in);

	or g4[63:0] (bl_0, precharge, d0);
	or g5[63:0] (bl_1, precharge, d1);
	or g6[63:0] (bl_2, precharge, d2);
	or g7[63:0] (bl_3, precharge, d3);

	/* wordline muxing */
	bit [6:0] ad0, ad1, ad2, ad3;

	and  g8[6:0] (ad0, wr_addr_in, {7{cs0}});
	and  g9[6:0] (ad1, wr_addr_in, {7{cs1}});
	and g10[6:0] (ad2, wr_addr_in, {7{cs2}});
	and g11[6:0] (ad3, wr_addr_in, {7{cs3}});

	assign wl_0 = (clk_rw_sel) ? ad0 : re_addr_in[7:1];
	assign wl_1 = (clk_rw_sel) ? ad1 : re_addr_in[15:9];
	assign wl_2 = (clk_rw_sel) ? ad2 : re_addr_in[23:17];
	assign wl_3 = (clk_rw_sel) ? ad3 : re_addr_in[31:25];

	assign d_sel0 = re_addr_in[0];
	assign d_sel1 = re_addr_in[8];
	assign d_sel2 = re_addr_in[16];
	assign d_sel3 = re_addr_in[24];

endmodule : sram_ctrl

module p_ctrl(
	clk_1, clk_2, rst_l, clk_wr_addr, clk_p_xor,
	wr_addr, csp, psel, salt_key_sel
	);

	/* Inputs */
	input bit clk_1, clk_2, rst_l, clk_wr_addr, clk_p_xor, csp;
	input bit [6:0] wr_addr;

	/* psel[0] selects p0 and p1, etc */
	output bit [8:0] psel;
	output bit salt_key_sel;

	bit [8:0] p_adr;
	bit psel_n;

	/* FF for write address */
	always_ff @(posedge clk_2, negedge rst_l) begin
	  if (~rst_l) begin
	    p_adr <= 0;
	  end
	  else begin
	    p_adr[8] <= p_adr[7];
	    p_adr[7] <= p_adr[6];
	    p_adr[6] <= p_adr[5];
	    p_adr[5] <= p_adr[4];
	    p_adr[4] <= p_adr[3];
	    p_adr[3] <= p_adr[2];
	    p_adr[2] <= p_adr[1];
	    p_adr[1] <= p_adr[0];
	    p_adr[0] <= psel_n;
	  end
	end

	/* FF for salt / key xor select */
	always_ff @(negedge clk_p_xor, negedge rst_l) begin
	  salt_key_sel <= (~rst_l) ? 0 : (~salt_key_sel);
	end

	assign psel_n = ((wr_addr == 0) && csp);
	and g0[8:0] (psel, p_adr, {9{clk_wr_addr}});

endmodule : p_ctrl
