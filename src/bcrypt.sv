module bcrypt(
  clk, reset_l, start, load_en,
  rx, tx,
  clk_0, clk_1, clk_2, clk_2_1, clk_3,
  clk_wr_addr, clk_ctext_load, clk_p_xor0, clk_p_xor,
  en_clk_1_0, en_clk_1_16, en_clk_3, en_clk_2, en_1, en_2, en_3, en_4,
  cost0, salt_key_sel, psel,
  s1Data, s2Data, s3Data, s4Data, re_addr, salt_c, key_c,
  l_out, r_out, ct_Orph, ct_eanB, ct_ehol, ct_derS, ct_cryD, ct_oubt
);

  /* Inputs */
  input logic clk, reset_l, start, load_en;
  input logic cost0, rx;
  input logic clk_0, clk_1, clk_2, clk_2_1, clk_3;
  input logic clk_wr_addr, clk_ctext_load, clk_p_xor0, clk_p_xor;
  input logic en_clk_1_0, en_clk_1_16, en_clk_3, en_clk_2, en_1, en_2, en_3, en_4, salt_key_sel;
  input logic [8:0] psel;
  input logic [31:0] s1Data, s2Data, s3Data, s4Data;
  input logic [575:0] salt_c, key_c;

  /* Outputs */
  output logic [31:0] re_addr, l_out, r_out;
  output logic tx;
  output logic [31:0] ct_Orph, ct_eanB, ct_ehol, ct_derS, ct_cryD, ct_oubt;

  /* UART Interface */
  logic [31:0] uartDataOut, uartDataIn;
  logic uartInReady, uartOutReady, uartInValid, uartOutValid;
  logic uartTxBusy, uartRxBusy;
  logic uartRxOverrunError, uartRxFrameError;
  logic [15:0] uartPrescale;
  //uartDataIn assigned in ctext section

  //assign uartPrescale = 0;
  //uart #32 (clk, !reset_l, uartDataIn, uartInValid, uartReady, uartDataOut,
  //uartOutValid, uartOutReady, rx, tx, uartTxBusy, uartRxBusy,
  //uartRxOverrunError, uartRxFrameError, uartPrescale);

  /* Salt Shift Register */
  logic [31:0] salt127, salt63; // R
  logic [31:0] salt95, salt31; // L 
  logic shiftSaltR, shiftSaltL, selSaltR, selSaltL, selSalt;
  logic [31:0] L, R;
 
  always_ff @(posedge load_en, posedge en_clk_2, negedge reset_l) begin
	if (~reset_l) begin
	  salt127 <= 0;
	  salt95 <= 0;
	  salt63 <= 0;
	  salt31 <= 0;
	end
	else if (en_clk_2) begin
	  if (en_1) begin
	    salt127 <= salt63;
		salt63 <= salt127;
	    salt95 <= salt31;
	    salt31 <= salt95;
	  end
	end
	else if (load_en) begin
//	  $display("salt127=%h\nsalt95=%h\nsalt63=%h\nsalt31=%h\n", 
//	  		salt_c[63:32], salt_c[31:0], salt_c[127:96], salt_c[95:64]);
	  salt127 <= salt_c[127:96];
	  salt95 <= salt_c[95:64];
	  salt63 <= salt_c[63:32];
	  salt31 <= salt_c[31:0];
	end
  end
 
/*
  always_ff @(posedge clk_0, posedge en_clk_2, negedge reset_l) begin
    if(!reset_l) begin
      salt127 <= 0;
      salt63 <= 0;
      salt95 <= 0;
      salt31 <= 0;
    end
    else if (en_clk_2) begin
	  if (en_1) begin
	    salt127 <= salt63;
		salt63 <= salt127;
	    salt95 <= salt31;
	    salt31 <= salt95;
	  end
	end
    else if (clk_0) begin
      if(shiftSaltR) begin
	    salt127 <= uartDataOut;
	    salt63 <= salt127;
      end
      if(shiftSaltL) begin
	    salt95 <= uartDataOut;
	    salt31 <= salt95;
      end
    end
  end
*/

  /* EDITED Key Shift Register */
  logic [575:0] key;
  logic shiftKey;

  always_ff @(posedge load_en, negedge reset_l) begin
	if (~reset_l) begin
	  key <= 0;
	end
	else begin
	  key <= key_c;
	end
  end
 
/* 
  always_ff @(posedge clk_0, negedge reset_l) begin
    if(!reset_l) begin
      key <= 0;
    end
    else if(clk_0) begin
      if(shiftKey) key <= {uartDataOut, (key >> 32)};
    end
  end
*/

  /* EDITED Cost Shift Register and Comparison */
/*  
  logic [31:0] cost;
  logic shiftCost;

  always_ff @(posedge clk_0, negedge reset_l) begin
    if(!reset_l) begin
      cost <= 0;
    end
    else if(clk) begin
      if(shiftCost) begin
	    cost <= uartDataOut;
      end
    end
  end
*/

  /* EDITED Ciphertext Shift Register */
  logic selCT;
  logic ctext_load, ctext_load_en, ctext_load_en2;

  assign ctext_load = ((en_clk_2 && en_4) || en_clk_3);
  assign ctext_load_en = ~((en_clk_3 && ((~en_3) || (~cost0))));
  assign ctext_load_en2 = ~(en_clk_3 && ((~en_4) || (~cost0)));
 
  always_ff @(posedge en_clk_2, negedge reset_l) begin
    if (!reset_l) begin  
	  ct_Orph <= 32'h4f727068;
      ct_eanB <= 32'h65616e42;
      ct_ehol <= 32'h65686f6c;
      ct_derS <= 32'h64657253;
      ct_cryD <= 32'h63727944;
      ct_oubt <= 32'h6f756274;
    end
    else if (en_4) begin
//	  $display("[63:32]=%h\n[31:0] =%h", R, L);
	  ct_Orph <= ct_ehol;
	  ct_ehol <= ct_cryD;
	  ct_cryD <= R;
	  ct_eanB <= ct_derS;
	  ct_derS <= ct_oubt;
	  ct_oubt <= L;
    end
  end

/* **** old ****
  always_ff @(posedge clk_2, negedge reset_l) begin
    if(!reset_l) begin
      ct_Orph <= 32'h4f727068;
      ct_eanB <= 32'h65616e42;
      ct_ehol <= 32'h65686f6c;
      ct_derS <= 32'h64657253;
      ct_cryD <= 32'h63727944;
      ct_oubt <= 32'h6f756274;
    end
    else if (ctext_load) begin
//	  $display("\n*********************");
//	  $display("%h\n%h\n%h\n%h\n%h\n%h\n",
//	  			ct_Orph, ct_ehol, ct_cryD, ct_eanB, ct_derS, ct_oubt);
	  ct_Orph <= (ctext_load_en) ? ct_ehol : ct_Orph;
	  ct_ehol <= (ctext_load_en) ? ct_cryD : ct_ehol;
	  ct_cryD <= (ctext_load_en2) ? l_out : ct_cryD;
	  ct_eanB <= (ctext_load_en) ? ct_derS : ct_eanB;
	  ct_derS <= (ctext_load_en) ? ct_oubt : ct_derS;
	  ct_oubt <= (ctext_load_en2) ? r_out : ct_oubt;
    end
  end
*/

  assign uartDataIn = selCT ? ct_eanB : ct_Orph;

  /* EDITED P-array Shift Register */
  logic [31:0] P0, P1, P2, P3, P4, P5, P6, P7, P8;
  logic [31:0] P9, P10, P11, P12, P13, P14, P15, P16, P17;
  logic p_xor;
  logic [575:0] p_key_out;

  assign p_key_out = salt_key_sel ? salt_c : key_c;
  assign p_xor = (clk_p_xor || clk_p_xor0);

  always_ff @(posedge en_clk_2, negedge en_clk_1_0, negedge reset_l) begin
	if (~reset_l || ~en_clk_2) begin
	  l_out <= 0;
	  r_out <= 0;
	end
	else begin
//	  $display("[63:32]=%h\n[31:0] =%h", R, L);
	  l_out <= R;
	  r_out <= L;
	end
  end

  always_ff @(posedge clk_1, negedge reset_l) begin
    if (~reset_l) begin
      P0 <= 32'h243f6a88;
      P1 <= 32'h85a308d3;
      P2 <= 32'h13198a2e;
      P3 <= 32'h03707344;
      P4 <= 32'ha4093822;
      P5 <= 32'h299f31d0; 
      P6 <= 32'h082efa98;
      P7 <= 32'hec4e6c89;
      P8 <= 32'h452821e6;
      P9 <= 32'h38d01377;
      P10 <= 32'hbe5466cf;
      P11 <= 32'h34e90c6c;
      P12 <= 32'hc0ac29b7;
      P13 <= 32'hc97c50dd;
      P14 <= 32'h3f84d5b5;
      P15 <= 32'hb5470917;
      P16 <= 32'h9216d5d9;
      P17 <= 32'h8979fb1b;
	end
	else if (clk_p_xor0) begin
/*	  $display("first");
	  $display("salt(1) or key(0)? %d", salt_key_sel);
	  $display("salt/key = %h", p_key_out);
	  $display("p0=%h  xor %h", P0, p_key_out[32*(17)   +: 32]);
	  $display("p1=%h  xor %h", P1, p_key_out[32*(16)   +: 32]);
	  $display("p2=%h  xor %h", P2, p_key_out[32*(15)   +: 32]);
	  $display("p3=%h  xor %h", P3, p_key_out[32*(14)   +: 32]);
	  $display("p4=%h  xor %h", P4, p_key_out[32*(13)   +: 32]);
	  $display("p5=%h  xor %h", P5, p_key_out[32*(12)   +: 32]);
	  $display("p6=%h  xor %h", P6, p_key_out[32*(11)   +: 32]);
	  $display("p7=%h  xor %h", P7, p_key_out[32*(10)   +: 32]);
	  $display("p8=%h  xor %h", P8, p_key_out[32*(9)   +: 32]);
	  $display("p9=%h  xor %h", P9, p_key_out[32*(8)   +: 32]);
	  $display("p10=%h xor %h", P10, p_key_out[32*(7) +: 32]);
	  $display("p11=%h xor %h", P11, p_key_out[32*(6) +: 32]);
	  $display("p12=%h xor %h", P12, p_key_out[32*(5) +: 32]);
	  $display("p13=%h xor %h", P13, p_key_out[32*(4) +: 32]);
	  $display("p14=%h xor %h", P14, p_key_out[32*(3) +: 32]);
	  $display("p15=%h xor %h", P15, p_key_out[32*(2) +: 32]);
	  $display("p16=%h xor %h", P16, p_key_out[32*(1) +: 32]);
	  $display("p17=%h xor %h", P17, p_key_out[32*(0) +: 32]);
*/	//  $display("parray=%h", P0 ^ p_key_out[32*(17) +: 32]);
	  P0  <= P0  ^ p_key_out[32*(17) +: 32];
	  P1  <= P1  ^ p_key_out[32*(16) +: 32];
	  P2  <= P2  ^ p_key_out[32*(15) +: 32];
	  P3  <= P3  ^ p_key_out[32*(14) +: 32];
	  P4  <= P4  ^ p_key_out[32*(13) +: 32];
	  P5  <= P5  ^ p_key_out[32*(12) +: 32];
	  P6  <= P6  ^ p_key_out[32*(11) +: 32];
	  P7  <= P7  ^ p_key_out[32*(10) +: 32];
	  P8  <= P8  ^ p_key_out[32*(9)  +: 32];
	  P9  <= P9  ^ p_key_out[32*(8)  +: 32];
	  P10 <= P10 ^ p_key_out[32*(7)  +: 32];
	  P11 <= P11 ^ p_key_out[32*(6)  +: 32];
	  P12 <= P12 ^ p_key_out[32*(5)  +: 32];
	  P13 <= P13 ^ p_key_out[32*(4)  +: 32];
	  P14 <= P14 ^ p_key_out[32*(3)  +: 32];
	  P15 <= P15 ^ p_key_out[32*(2)  +: 32];
	  P16 <= P16 ^ p_key_out[32*(1)  +: 32];
	  P17 <= P17 ^ p_key_out[32*(0)  +: 32];
	end
	else if (clk_p_xor) begin
/*	  $display("salt(1) or key(0)? %d", salt_key_sel);
	  $display("salt/key = %h", p_key_out);
	  $display("p0=%h  xor %h", P1, p_key_out[32*(17)  +: 32]);
	  $display("p1=%h  xor %h", P2, p_key_out[32*(16)  +: 32]);
	  $display("p2=%h  xor %h", P3, p_key_out[32*(15)  +: 32]);
	  $display("p3=%h  xor %h", P4, p_key_out[32*(14)  +: 32]);
	  $display("p4=%h  xor %h", P5, p_key_out[32*(13)  +: 32]);
	  $display("p5=%h  xor %h", P6, p_key_out[32*(12)  +: 32]);
	  $display("p6=%h  xor %h", P7, p_key_out[32*(11)  +: 32]);
	  $display("p7=%h  xor %h", P8, p_key_out[32*(10)  +: 32]);
	  $display("p8=%h  xor %h", P9, p_key_out[32*(9)  +: 32]);
	  $display("p9=%h  xor %h", P10, p_key_out[32*(8)  +: 32]);
	  $display("p10=%h xor %h", P11, p_key_out[32*(7)  +: 32]);
	  $display("p11=%h xor %h", P12, p_key_out[32*(6)  +: 32]);
	  $display("p12=%h xor %h", P13, p_key_out[32*(5)  +: 32]);
	  $display("p13=%h xor %h", P14, p_key_out[32*(4)  +: 32]);
	  $display("p14=%h xor %h", P15, p_key_out[32*(3)  +: 32]);
	  $display("p15=%h xor %h", P16, p_key_out[32*(2)  +: 32]);
	  $display("p16=%h xor %h", P17, p_key_out[32*(1)  +: 32]);
	  $display("p17=%h xor %h", P0, p_key_out[32*(0)  +: 32]);
*/	//  $display("parray=%h", P1 ^ p_key_out[32*(17) +: 32]);
	  P0  <= P1  ^ p_key_out[32*(17) +: 32];
	  P1  <= P2  ^ p_key_out[32*(16) +: 32];
	  P2  <= P3  ^ p_key_out[32*(15) +: 32];
	  P3  <= P4  ^ p_key_out[32*(14) +: 32];
	  P4  <= P5  ^ p_key_out[32*(13) +: 32];
	  P5  <= P6  ^ p_key_out[32*(12) +: 32];
	  P6  <= P7  ^ p_key_out[32*(11) +: 32];
	  P7  <= P8  ^ p_key_out[32*(10) +: 32];
	  P8  <= P9  ^ p_key_out[32*(9)  +: 32];
	  P9  <= P10 ^ p_key_out[32*(8)  +: 32];
	  P10 <= P11 ^ p_key_out[32*(7)  +: 32];
	  P11 <= P12 ^ p_key_out[32*(6)  +: 32];
	  P12 <= P13 ^ p_key_out[32*(5)  +: 32];
	  P13 <= P14 ^ p_key_out[32*(4)  +: 32];
	  P14 <= P15 ^ p_key_out[32*(3)  +: 32];
	  P15 <= P16 ^ p_key_out[32*(2)  +: 32];
	  P16 <= P17 ^ p_key_out[32*(1)  +: 32];
	  P17 <= P0  ^ p_key_out[32*(0)  +: 32];
	end
	else begin
	  
/*	  if (psel) begin
		//$display("%h", {l_out, r_out});
		
		if (psel[0]) begin
		  $display("parray[0] =%h", l_out);
		  $display("parray[1] =%h", r_out);
		end
		else if (psel[1]) begin
		  $display("parray[2] =%h", l_out);
		  $display("parray[3] =%h", r_out);
		end
		else if (psel[2]) begin
		  $display("parray[4] =%h", l_out);
		  $display("parray[5] =%h", r_out);
		end
		else if (psel[3]) begin
		  $display("parray[6] =%h", l_out);
		  $display("parray[7] =%h", r_out);
		end
		else if (psel[4]) begin
		  $display("parray[8] =%h", l_out);
		  $display("parray[9] =%h", r_out);
		end
		else if (psel[5]) begin
		  $display("parray[10]=%h", l_out);
		  $display("parray[11]=%h", r_out);
		end
		else if (psel[6]) begin
		  $display("parray[12]=%h", l_out);
		  $display("parray[13]=%h", r_out);
		end
		else if (psel[7]) begin
		  $display("parray[14]=%h", l_out);
		  $display("parray[15]=%h", r_out);
		end
		else if (psel[8]) begin
		  $display("parray[16]=%h", l_out);
		  $display("parray[17]=%h", r_out);
		end

	  end
*/	  
	  P0  <= (psel[0]) ? l_out : P1;
	  P1  <= (psel[0]) ? r_out : P2;
	  P2  <= (psel[1]) ? l_out : P3;
	  P3  <= (psel[1]) ? r_out : P4;
	  P4  <= (psel[2]) ? l_out : P5;
	  P5  <= (psel[2]) ? r_out : P6;
	  P6  <= (psel[3]) ? l_out : P7;
	  P7  <= (psel[3]) ? r_out : P8;
	  P8  <= (psel[4]) ? l_out : P9;
	  P9  <= (psel[4]) ? r_out : P10;
	  P10 <= (psel[5]) ? l_out : P11;
	  P11 <= (psel[5]) ? r_out : P12;
	  P12 <= (psel[6]) ? l_out : P13;
	  P13 <= (psel[6]) ? r_out : P14;
	  P14 <= (psel[7]) ? l_out : P15;
	  P15 <= (psel[7]) ? r_out : P16;
	  P16 <= (psel[8]) ? l_out : P17;
	  P17 <= (psel[8]) ? r_out : P0;
	end
  end

  /* EDITED Feistel */

  logic selFeistelMemOrZero;
  logic [31:0] feistelXorMem;
  logic xor_load;
  logic feistel_clk;
  logic [31:0] cout_h;

  //assign selFiestelMemOrZero = (~en_clk_2);

  //assign feistelXorMem = (((s1Data + s2Data) ^ s3Data) + s4Data);
  assign re_addr = ((P0 ^ feistelXorMem) ^ R);
  assign cout_h = (((P0 ^ feistelXorMem) ^ R) ^ P2);
  assign xor_load = (en_clk_2 && (en_1 || en_2 || en_3));

  or g0(feistel_clk, clk_1, clk_2);

  logic [5:0] round;

  always_ff @(posedge feistel_clk, posedge clk_2_1, negedge reset_l) begin
    if(!reset_l) begin
      L <= 0;
      R <= 0;
	  feistelXorMem <= 0;
	  round <= 0;
    end
    else if (clk_1) begin
	  feistelXorMem <= (((s1Data + s2Data) ^ s3Data) + s4Data);
	  L <= ((en_clk_2 || en_clk_1_0) || clk_p_xor0) ? R : ((en_clk_1_16) ? cout_h : re_addr);
	  R <= L;
	  round <= (round+6'd1);
	end
	else if (ctext_load) begin
	  L <= (ct_Orph & {32{ctext_load_en}});
	  R <= (ct_eanB & {32{ctext_load_en}});
	end
	else if (clk_2_1 || xor_load) begin
//	  $display("\nload");
//	  $display("saltL=%h\nsaltR=%h\nL=%h\nR=%h",
//				salt63, salt31,
//	  			R ^ (salt63 & {32{en_1}}),
//	  			L ^ (salt31 & {32{en_1}}));
	  L <= R ^ (salt63 & {32{en_1}});
	  R <= L ^ (salt31 & {32{en_1}});
	end
  end

  /* L_p to Memory Address Mapping */
  /* already in fsm 
  logic [6:0] feistel_s1_addr, feistel_s2_addr, feistel_s3_addr, feistel_s4_addr;
  assign feistel_s1_addr = L_p[31:25];
  assign feistel_s2_addr = L_p[23:17];
  assign feistel_s3_addr = L_p[15:9];
  assign feistel_s4_addr = L_p[7:1];
  */

  /* Memory Address Write Counter */
  /* already in fsm
  logic [6:0] memWriteAddrCtr; // Increment through all the addresses for this SRAm
  logic [1:0] memWriteSRAMCtr; // Increment through each of the 4 SRAMs
  logic incrementWriteAddrCtr, incrementWriteSRAMCtr, clearSRAMCtrs;

  always_ff @(posedge clk, negedge reset_l) begin
    if(!reset_l) begin
      memWriteAddrCtr <= 0;
      memWriteSRAMCtr <= 0;
    end
    else if(clk) begin
      if(clearSRAMCtrs) begin
	memWriteAddrCtr <= 0;
	memWriteSRAMCtr <= 0;
      end
      else begin
	if(incrementWriteAddrCtr) memWriteAddrCtr <= memWriteAddrCtr + 1;
	if(incrementWriteSRAMCtr) memWriteSRAMCtr <= memWriteSRAMCtr + 1;
      end
    end
  end
  */

  /* Memory Address Read/Write Mux */
  /* already in fsm
  logic selS1Addr, selS2Addr, selS3Addr, selS4Addr;
  assign s1Addr = selS1Addr ? memWriteAddrCtr : feistel_s1_addr;
  assign s2Addr = selS2Addr ? memWriteAddrCtr : feistel_s2_addr;
  assign s3Addr = selS3Addr ? memWriteAddrCtr : feistel_s3_addr;
  assign s4Addr = selS4Addr ? memWriteAddrCtr : feistel_s4_addr;
  */

endmodule: bcrypt
