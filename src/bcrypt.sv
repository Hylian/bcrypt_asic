module bcrypt(
  clk, reset_l, 
  rx, tx
);

  /* Inputs */
  input logic clk, reset_l;
  input logic rx;

<<<<<<< Updated upstream:src/bcrypt.sv
=======



>>>>>>> Stashed changes:bcrypt.sv
  /* Outputs */
  output logic tx;

  /* SRAM Interface */
  logic [63:0] s1Data, s2Data, s3Data, s4Data;
  logic [6:0] s1Addr, s2Addr, s3Addr, s4Addr;
  logic s1_en, s2_en, s3_en, s4_en;

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

  /* Feistel Variables */
  logic [31:0] L_p, L, R;

  /* Salt Shift Register */
  logic [31:0] salt127, salt63; // R
  logic [31:0] salt95, salt31; // L 
  logic shiftSaltR, shiftSaltL, selSaltR, selSaltL, selSalt;
  
  always_ff @(posedge clk, negedge reset_l) begin
    if(!reset_l) begin
      salt127 <= 0;
      salt63 <= 0;
      salt95 <= 0;
      salt31 <= 0;
    end
    else if(clk) begin
      if(shiftSaltR) begin
	salt127 <= selSaltR ? salt63 : uartDataOut;
	salt63 <= salt127;
      end
      if(shiftSaltL) begin
	salt95 <= selSaltL ? salt31 : uartDataOut;
	salt31 <= salt95;
      end
    end
  end

  /* Key Shift Register */
  logic [575:0] key;
  logic shiftKey;
  
  always_ff @(posedge clk, negedge reset_l) begin
    if(!reset_l) begin
      key <= 0;
    end
    else if(clk) begin
      if(shiftKey) key <= {uartDataOut, (key >> 32)};
    end
  end

  /* Cost Shift Register and Comparison */
  logic [31:0] cost;
  logic shiftCost, costIsZero, decrementCost;

  always_ff @(posedge clk, negedge reset_l) begin
    if(!reset_l) begin
      cost <= 0;
    end
    else if(clk) begin
      if(shiftCost) begin
	cost <= uartDataOut;
      end
      else if(decrementCost) begin
	cost <= cost - 1;
      end
    end
  end

  assign costIsZero = (cost == 0);
  

  /* Ciphertext Shift Register */
  logic [31:0] ct_Orph, ct_eanB, ct_ehol, ct_derS, ct_cryD, ct_oubt;
  logic shiftCtextL, shiftCtextR;
  logic selCT;

  always_ff @(posedge clk, negedge reset_l) begin
    if(!reset_l) begin
      ct_Orph <= 32'h4f727068;
      ct_eanB <= 32'h65616e42;
      ct_ehol <= 32'h65686f6c;
      ct_derS <= 32'h64657253;
      ct_cryD <= 32'h63727944;
      ct_oubt <= 32'h6f756274;
    end
    else if(clk) begin
      if(shiftCtextL) begin
	ct_Orph <= ct_ehol;
	ct_ehol <= ct_cryD;
	ct_cryD <= L;
      end
      if(shiftCtextR) begin
	ct_eanB <= ct_derS;
	ct_derS <= ct_oubt;
	ct_oubt <= R;
      end
    end
  end

  assign uartDataIn = selCT ? ct_eanB : ct_Orph;

  /* P-array Shift Register */
  logic [31:0] P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17;
  enum logic [1:0] {
    HOLD = 0,
    SHIFT = 1,
    XOR = 2,
    LOAD = 3
  } selp0p1, selp2p3, selp4p5, selp6p7, selp8p9, selp10p11, selp12p13, selp14p15, selp16p17;
  logic selPKey, shiftP;
  logic [575:0] pKeyOut;

  assign pKeyOut = selPKey ? {salt63, salt31, {4{salt127, salt95, salt63, salt31}}} : key;

  always_ff @(posedge clk, negedge reset_l) begin
    if(!reset_l) begin
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
    else if(clk) begin
      case(selp0p1)
	SHIFT: begin
	  P0 <= P1;
	  P1 <= P2;
	end
	XOR: begin
	  P0 <= P0 ^ pKeyOut[32*(0) +: 32];
	  P1 <= P1 ^ pKeyOut[32*(1) +: 32];
	end
	LOAD: begin
	  P0 <= L;
	  P1 <= R;
	end
      endcase
      case(selp2p3)
	SHIFT: begin
	  P2 <= P3;
	  P3 <= P4;
	end
	XOR: begin
	  P2 <= P2 ^ pKeyOut[32*(2) +: 32];
	  P3 <= P3 ^ pKeyOut[32*(3) +: 32];
	end
	LOAD: begin
	  P2 <= L;
	  P3 <= R;
	end
      endcase
      case(selp4p5)
	SHIFT: begin
	  P4 <= P5;
	  P5 <= P6;
	end
	XOR: begin
	  P4 <= P4 ^ pKeyOut[32*(4) +: 32];
	  P5 <= P5 ^ pKeyOut[32*(5) +: 32];
	end
	LOAD: begin
	  P4 <= L;
	  P5 <= R;
	end
      endcase
      case(selp6p7)
	SHIFT: begin
	  P6 <= P7;
	  P7 <= P8;
	end
	XOR: begin
	  P6 <= P6 ^ pKeyOut[32*(6) +: 32];
	  P7 <= P7 ^ pKeyOut[32*(7) +: 32];
	end
	LOAD: begin
	  P6 <= L;
	  P7 <= R;
	end
      endcase
      case(selp8p9)
	SHIFT: begin
	  P8 <= P9;
	  P9 <= P10;
	end
	XOR: begin
	  P8 <= P8 ^ pKeyOut[32*(8) +: 32];
	  P9 <= P9 ^ pKeyOut[32*(9) +: 32];
	end
	LOAD: begin
	  P8 <= L;
	  P9 <= R;
	end
      endcase
      case(selp10p11)
	SHIFT: begin
	  P10 <= P11;
	  P11 <= P12;
	end
	XOR: begin
	  P10 <= P10 ^ pKeyOut[32*(10) +: 32];
	  P11 <= P11 ^ pKeyOut[32*(11) +: 32];
	end
	LOAD: begin
	  P10 <= L;
	  P11 <= R;
	end
      endcase
      case(selp12p13)
	SHIFT: begin
	  P12 <= P13;
	  P13 <= P14;
	end
	XOR: begin
	  P12 <= P12 ^ pKeyOut[32*(12) +: 32];
	  P13 <= P13 ^ pKeyOut[32*(13) +: 32];
	end
	LOAD: begin
	  P12 <= L;
	  P13 <= R;
	end
      endcase
      case(selp14p15)
	SHIFT: begin
	  P14 <= P15;
	  P15 <= P16;
	end
	XOR: begin
	  P14 <= P14 ^ pKeyOut[32*(14) +: 32];
	  P15 <= P15 ^ pKeyOut[32*(15) +: 32];
	end
	LOAD: begin
	  P14 <= L;
	  P15 <= R;
	end
      endcase
      case(selp16p17)
	SHIFT: begin
	  P16 <= P17;
	  P17 <= P0;
	end
	XOR: begin
	  P16 <= P16 ^ pKeyOut[32*(16) +: 32];
	  P17 <= P17 ^ pKeyOut[32*(17) +: 32];
	end
	LOAD: begin
	  P16 <= L;
	  P17 <= R;
	end
      endcase
    end

  end

  /* Feistel */

  logic selFeistelMemOrZero;
  logic [31:0] feistelXorMem;
  logic shiftFeistel, loadFeistelCtext, loadFeistelSalt;

  always_comb begin
    if(selFeistelMemOrZero) begin
      if(L_p[0]) feistelXorMem = (((s1Data[63:32] + s2Data[63:32]) ^ s3Data[63:32]) + s4Data[63:32]);
      else feistelXorMem = (((s1Data[31:0] + s2Data[31:0]) ^ s3Data[31:0]) + s4Data[31:0]);
    end

    else feistelXorMem = 0;
  end

  always_ff @(posedge clk, negedge clk, negedge reset_l) begin
    if(!reset_l) begin
      L <= 0;
      L_p <= 0;
      R <= 0;
    end
    else begin
      if(!clk) begin
	L_p <= P0 ^ feistelXorMem ^ R;
      end
      if(clk) begin
	if(shiftFeistel) begin
	  L <= L_p;
	  R <= L;
	end
	else if(loadFeistelCtext) begin
	  L <= ct_Orph;
	  R <= ct_eanB;
	end
	else if(loadFeistelSalt) begin
	  L <= L ^ salt31;
	  R <= R ^ salt63;
	end
      end
    end
  end

  /* L_p to Memory Address Mapping */
  logic [6:0] feistel_s1_addr, feistel_s2_addr, feistel_s3_addr, feistel_s4_addr;
  assign feistel_s1_addr = L_p[31:25];
  assign feistel_s2_addr = L_p[23:17];
  assign feistel_s3_addr = L_p[15:9];
  assign feistel_s4_addr = L_p[7:1];

  /* Memory Address Write Counter */
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

  /* Memory Address Read/Write Mux */
  logic selS1Addr, selS2Addr, selS3Addr, selS4Addr;
  assign s1Addr = selS1Addr ? memWriteAddrCtr : feistel_s1_addr;
  assign s2Addr = selS2Addr ? memWriteAddrCtr : feistel_s2_addr;
  assign s3Addr = selS3Addr ? memWriteAddrCtr : feistel_s3_addr;
  assign s4Addr = selS4Addr ? memWriteAddrCtr : feistel_s4_addr;

endmodule: bcrypt
