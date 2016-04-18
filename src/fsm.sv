module state_fsm(
	clk, clk_3, cost, start,
	int_rst_l, ext_rst_l,
	en,   en_1, en_2,
	en_3, en_4, en_5, cost0, rst_l
	);

	/* Inputs */
	input bit clk, clk_3, int_rst_l, ext_rst_l, start;
	input bit [63:0] cost;

	/* Outputs */
	output bit en,   en_1, en_2;
	output bit en_3, en_4, en_5, cost0, rst_l;

	/* State Logic */
	bit [2:0] state, state_n, state_m;

	/* Intermediary Logic */
	bit [2:0] d_0, d_1, d_2, d_3, d_4, d_5;
	bit [63:0] cost_curr, cost_n;
	bit costshift, sel0, sel1;

	/* FF for keeping track of cost */
	always_ff @(posedge costshift, negedge rst_l) begin
	  if (~rst_l) begin
		$display("resetting");
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

	/* chip reset logic */
	and g2(rst_l, int_rst_l, ext_rst_l);

endmodule : state_fsm

/* fsm for main gaited clock, 18 + 1 cycles */
module clock_fsm (
	en, en_1, en_2, en_3, en_4, cost0, load,
	clk, rst_l, clk_3,
	clk_0, clk_l, clk_1, clk_2, clk_2_1, 
	en_clk_2, en_clk_1_0, en_clk_1_17,
	clk_wr_addr, clk_rw_sel, clk_p_xor0, clk_p_xor, clk_ctext_load
	);

	/* Inputs */
	input bit en, en_1, en_2, en_3, en_4, cost0, load, clk, rst_l, clk_3;

	/* Outputs */
	output bit clk_0, clk_l, clk_1, clk_2, clk_2_1, en_clk_2, en_clk_1_0, en_clk_1_17;
	output bit clk_wr_addr, clk_rw_sel, clk_p_xor0, clk_p_xor, clk_ctext_load;

	/* Intermediary logic */
	bit [4:0] count, count_n;
	bit d_1, d_2, d_3, d_4, d_5;
	bit q_1, q_2, q_3, q_4;
	bit go, load_in;
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

	/* FF for starting load in */
	always_ff @(posedge load, negedge rst_l) begin
	  if (~rst_l) begin
	    load_in <= 0;
	  end
	  else begin
	    load_in <= 1;
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
	    en_clk_1_0 <= 0;
	  end
	  else begin
	    en_clk_1_0 <= d_4;
	  end
	end

	/* enable signal for last clk_1 in cycle */
	always_ff @(negedge clk, negedge rst_l) begin
	  if (~rst_l) begin
	    en_clk_1_17 <= 0;
	  end
	  else begin
	    en_clk_1_17 <= (count_n > 5'd18);
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
	and g5(d5, q_4, clk);

	assign go = (en) ? clk : 0;
	assign d_2 = (count > 5'd18);
	assign count_n = (d_2) ? 5'd1 : (count + 5'd1);
	assign d_3 = (count == 5'd0);
	assign d_1 = (~(d_2 || d_3));
	assign clk_0 = (load_in) ? d5 : 0;

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
	en_clk_1_17,
	l, r, wr_addr, re_addr, data_ready,
	wl_0, wl_1, wl_2, wl_3,
	d_sel0, d_sel1, d_sel2, d_sel3,
	bl
	);

	/* Input logic */
	input bit clk_wr_addr, clk_rw_sel, clk_1, clk_2, rst_l;
	input bit en_clk_1_17;
	input bit data_ready;
	input bit [31:0] l, r, re_addr;
	input bit [6:0] wr_addr;

	/* Output logic */
	output bit [6:0] wl_0, wl_1, wl_2, wl_3;
	output bit d_sel0, d_sel1, d_sel2, d_sel3;
	output bit [63:0] bl;

	/* FFs for addr, data in, precharge */
	bit [63:0] data_in;
	bit [6:0] wr_addr_in;
	bit [31:0] re_addr_in;
	bit [63:0] precharge;

/*
	always_ff @(posedge clk_wr_addr, negedge rst_l) begin
	  if (~rst_l) begin
	    data_in <= 0;
	  end
	  else begin
	    data_in <= {l, r};
	  end
	end

	always_ff @(negedge clk_2, negedge rst_l) begin
	  if (~rst_l) begin
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
*/

	always_ff @(posedge clk_1, negedge rst_l) begin
	  if (~rst_l) begin
	    precharge <= 0;
	  end
	  else begin
	    precharge <= (en_clk_1_17) ? 64'd0 : {64{1'b1}};
	  end
	end

	/* bitline muxing */
	or g4[63:0] (bl, precharge, {l, r});

	/* wordline muxing */
	assign wl_0 = (clk_rw_sel) ? wr_addr : re_addr[7:1];
	assign wl_1 = (clk_rw_sel) ? wr_addr : re_addr[15:9];
	assign wl_2 = (clk_rw_sel) ? wr_addr : re_addr[23:17];
	assign wl_3 = (clk_rw_sel) ? wr_addr : re_addr[31:25];

	assign d_sel0 = re_addr[0];
	assign d_sel1 = re_addr[8];
	assign d_sel2 = re_addr[16];
	assign d_sel3 = re_addr[24];

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

module load_costsaltkey(
	cost, key, salt, clk_0, clk_2_1, rst_l,
	cost_a, key_c, salt_c, start, load_en);

	input bit [575:0] key;
	input bit [127:0] salt;
	input bit [5:0] cost;
	input bit clk_0, rst_l, clk_2_1;
	
	output bit [575:0] key_c, salt_c;
	output bit [63:0] cost_a;
	output bit start, load_en;

	bit [1:0] key_ready;
	bit cost_ready, salt_ready;
	bit [9:0] key_count, key_shift_len;

	/* shift reg for cycling key */
	always_ff @(posedge clk_0, negedge rst_l) begin
	  if (~rst_l) begin
	    key_c <= 0;
	  end
	  else if (key_ready == 2'b00) begin
		key_c <= key;
	  end
	  else if (key_ready == 2'b01) begin
	    key_c <= (key_c << 1'b1);
	  end
	  else if (key_ready == 2'b10) begin
	    key_c <= (key_c >> 1'b1);
		key_c[575] <= key_c[key_shift_len];
	  end
	end

	/* counter to count key length */
	always_ff @(posedge clk_0, negedge rst_l) begin
	  if (~rst_l) begin
	    key_count <= 0;
	  end
	  else if (key_ready == 2'b01) begin
	    key_count <= (key_count + 10'd1);
	  end
	  else if (key_ready == 2'b10) begin
	    key_count <= (key_count - 10'd1);
	  end
	end

	/* keep track of key state */
	always_ff @(negedge clk_0, negedge rst_l) begin
	  if (~rst_l) begin
	    key_ready <= 2'b00;
		key_shift_len <= 0;
	  end
	  else if (key_ready == 2'b00) begin
		key_ready <= 2'b01;
	  end
	  else if (key_ready == 2'b01) begin
		key_ready <= ((key_c[575]) || key_shift_len == 10'd575) ? 2'b10 : 2'b01;
		key_shift_len <= (key_c[575]) ? key_count : 0;
	  end
	  else if (key_ready == 2'b10) begin
		key_ready <= (key_count == 10'd0) ? 2'b11 : 2'b10;
	  end
	end

	/* cycle salt */
	always_ff @(posedge clk_0, negedge rst_l) begin
	  if (~rst_l) begin
		salt_ready <= 0;
	  end
	  else if (salt_ready == 0) begin
		salt_c <= {salt[63:0], {4{salt}}};
		salt_ready <= 1;
		$display("%s", salt_c);
	  end
	end
	
	/* cost -> 2^cost */
	always_ff @(posedge clk_0, negedge rst_l) begin
	  if (~rst_l) begin
	    cost_a <= 0;
		cost_ready <= 0;
	  end
	  else if (cost_ready == 0) begin
		cost_a[cost] <= 1;
		cost_ready <= 1;
	  end
	end

	/* signal to start fiestel */
	always_ff @(posedge clk_0, posedge clk_2_1, negedge rst_l) begin
	  if (~rst_l) begin
	    start <= 0;
		load_en <= 0;
	  end
	  else if (clk_2_1) begin
		start <= 0;
	  end
	  else if (load_en) begin
		load_en <= 0;
		start <= 1;
	  end
	  else begin
		load_en <= (((key_ready == 2'b11) && salt_ready) && cost_ready);
	  end
	end

endmodule : load_costsaltkey

module output_rst (
	clk, ext_rst_l,
	cost, salt, ct_Orph, ct_eanB, ct_ehol,
	ct_derS, ct_cryD, ct_oubt, en_5, 
	hash, int_rst_l
	);

	input bit [5:0] cost;
	input bit [127:0] salt;
	input bit [31:0] ct_Orph, ct_eanB, ct_ehol, ct_derS, ct_cryD, ct_oubt;
	input bit clk, ext_rst_l, en_5;

	output bit [325:0] hash;
	output bit int_rst_l;

	bit rst_ready;

	always_ff @(posedge clk, negedge ext_rst_l) begin
	  if (~ext_rst_l) begin
		hash <= 0;
		int_rst_l <= 1;
		rst_ready <= 0;
	  end
	  else if (rst_ready) begin
		int_rst_l <= 0;
	    rst_ready <= 0;
	  end
	  else if (~int_rst_l) begin
		int_rst_l <= 1;
	  end
	  else if (en_5) begin
		hash <= {cost, salt, ct_Orph, ct_eanB, ct_ehol, ct_derS, ct_cryD, ct_oubt};
		rst_ready <= 1;
	  end
	end

endmodule : output_rst
