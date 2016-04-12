module bcrypt(
  clk, reset_l, start, load_en,
  rx, tx,
  clk_0, clk_1, clk_2, clk_2_1, clk_3,
  clk_wr_addr, clk_ctext_load, clk_p_xor0, clk_p_xor,
  en_clk_3, en_clk_2, en_1, en_2, en_3, en_4,
  cost0, salt_key_sel, psel,
  s1Data, s2Data, s3Data, s4Data, re_addr, salt_c, key_c,
  L, R, ct_Orph, ct_eanB, ct_ehol, ct_derS, ct_cryD, ct_oubt
);

  /* Inputs */
  input logic clk, reset_l, start, load_en;
  input logic cost0, rx;
  input logic clk_0, clk_1, clk_2, clk_2_1, clk_3;
  input logic clk_wr_addr, clk_ctext_load, clk_p_xor0, clk_p_xor;
  input logic en_clk_3, en_clk_2, en_1, en_2, en_3, en_4, salt_key_sel;
  input logic [8:0] psel;
  input logic [31:0] s1Data, s2Data, s3Data, s4Data;
  input logic [575:0] salt_c, key_c;

  /* Outputs */
  output logic [31:0] re_addr, L, R;
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
//	  $display("loading...salt_c=%h", salt_c);
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
	  ct_cryD <= (ctext_load_en2) ? L : ct_cryD;
	  ct_eanB <= (ctext_load_en) ? ct_derS : ct_eanB;
	  ct_derS <= (ctext_load_en) ? ct_oubt : ct_derS;
	  ct_oubt <= (ctext_load_en2) ? R : ct_oubt;
    end
  end

  assign uartDataIn = selCT ? ct_eanB : ct_Orph;

  /* EDITED P-array Shift Register */
  logic [31:0] P0, P1, P2, P3, P4, P5, P6, P7, P8;
  logic [31:0] P9, P10, P11, P12, P13, P14, P15, P16, P17;
  logic p_xor;
  logic [575:0] p_key_out;
  logic [31:0] l_in, r_in;

  assign p_key_out = salt_key_sel ? salt_c : key_c;
  assign p_xor = (clk_p_xor || clk_p_xor0);

  always_ff @(posedge clk_wr_addr, negedge clk_wr_addr, negedge reset_l) begin
	if (~reset_l || ~clk_wr_addr) begin
	  l_in <= 0;
	  r_in <= 0;
	end
	else begin
	  l_in <= L;
	  r_in <= R;
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
	else if (p_xor) begin
	  P0  <= P1  ^ p_key_out[32*(0)  +: 32];
	  P1  <= P2  ^ p_key_out[32*(1)  +: 32];
	  P2  <= P3  ^ p_key_out[32*(2)  +: 32];
	  P3  <= P4  ^ p_key_out[32*(3)  +: 32];
	  P4  <= P5  ^ p_key_out[32*(4)  +: 32];
	  P5  <= P6  ^ p_key_out[32*(5)  +: 32];
	  P6  <= P7  ^ p_key_out[32*(6)  +: 32];
	  P7  <= P8  ^ p_key_out[32*(7)  +: 32];
	  P8  <= P9  ^ p_key_out[32*(8)  +: 32];
	  P9  <= P10 ^ p_key_out[32*(9)  +: 32];
	  P10 <= P11 ^ p_key_out[32*(10) +: 32];
	  P11 <= P12 ^ p_key_out[32*(11) +: 32];
	  P12 <= P13 ^ p_key_out[32*(12) +: 32];
	  P13 <= P14 ^ p_key_out[32*(13) +: 32];
	  P14 <= P15 ^ p_key_out[32*(14) +: 32];
	  P15 <= P16 ^ p_key_out[32*(15) +: 32];
	  P16 <= P17 ^ p_key_out[32*(16) +: 32];
	  P17 <= P0  ^ p_key_out[32*(17) +: 32];
	end
	else begin
	  P0  <= (psel[0]) ? l_in : P1;
	  P1  <= (psel[0]) ? r_in : P2;
	  P2  <= (psel[1]) ? l_in : P3;
	  P3  <= (psel[1]) ? r_in : P4;
	  P4  <= (psel[2]) ? l_in : P5;
	  P5  <= (psel[2]) ? r_in : P6;
	  P6  <= (psel[3]) ? l_in : P7;
	  P7  <= (psel[3]) ? r_in : P8;
	  P8  <= (psel[4]) ? l_in : P9;
	  P9  <= (psel[4]) ? r_in : P10;
	  P10 <= (psel[5]) ? l_in : P11;
	  P11 <= (psel[5]) ? r_in : P12;
	  P12 <= (psel[6]) ? l_in : P13;
	  P13 <= (psel[6]) ? r_in : P14;
	  P14 <= (psel[7]) ? l_in : P15;
	  P15 <= (psel[7]) ? r_in : P16;
	  P16 <= (psel[8]) ? l_in : P17;
	  P17 <= (psel[8]) ? r_in : P0;
	end
  end

  /* EDITED Feistel */

  logic selFeistelMemOrZero;
  logic [31:0] feistelXorMem;
  logic xor_load;
  logic feistel_clk;

  //assign selFiestelMemOrZero = (~en_clk_2);

  assign feistelXorMem = (((s1Data + s2Data) ^ s3Data) + s4Data);
  assign re_addr = (en_clk_2) ? R : ((P0 ^ feistelXorMem) ^ R);
  assign xor_load = (en_clk_2 && (en_1 || en_2 || en_3));

  or g0(feistel_clk, clk_1, clk_2);

  always_ff @(posedge feistel_clk, posedge clk_2_1, negedge reset_l) begin
    if(!reset_l) begin
      L <= 0;
      R <= 0;
    end
    else if (clk_1) begin
//	  $display("clk1\n");
	  L <= re_addr;
	  R <= L;
	end
	else if (ctext_load) begin
//	  $display("clk2 ctext_load, en=%d\n", ctext_load_en);
	  L <= (ct_Orph & {32{ctext_load_en}});
	  R <= (ct_eanB & {32{ctext_load_en}});
	end
	else if (clk_2_1 || xor_load) begin
//	  $display("clk2 xor_load, en=%d\n", en_1);
	  L <= L ^ (salt31 & {32{en_1}});
	  R <= R ^ (salt63 & {32{en_1}});
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
