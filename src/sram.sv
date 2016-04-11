
module sram(
	clk_1, clk_2, rst_l, wl, rw_sel, bl,
	dsel0, dsel1, dsel2, dsel3,
	cs0, cs1, cs2, cs3,
	data_ready, data0, data1, data2, data3
	);

	input bit clk_1, clk_2, rst_l;
	input bit dsel0, dsel1, dsel2, dsel3;
	input bit cs0, cs1, cs2, cs3;
	input bit [27:0] wl;
	input bit rw_sel;      //0 read, 1 write
	input bit [63:0] bl;

	output bit data_ready;
	output bit [31:0] data0, data1, data2, data3;

	bit bl_ready;
	bit wl_ready;
	bit op_in, buffer_thing;
	bit [63:0] bl_in;
	bit [31:0] dlatch0, dlatch1, dlatch2, dlatch3;
	bit [63:0] mem0[0:127];
	bit [63:0] mem1[0:127];
	bit [63:0] mem2[0:127];
	bit [63:0] mem3[0:127];
	bit re_en, wr_en;

	always_ff @(negedge clk_2, negedge clk_1, negedge rst_l) begin
	  if (~rst_l) begin
		dlatch0 <= 0;
		dlatch1 <= 0;
		dlatch2 <= 0;
		dlatch3 <= 0;
		mem0[0]   <= 64'hd1310ba6_98dfb5ac;
		mem0[1]   <= 64'h2ffd72db_d01adfb7;
		mem0[2]   <= 64'hb8e1afed_6a267e96;
		mem0[3]   <= 64'hba7c9045_f12c7f99;
		mem0[4]   <= 64'h24a19947_b3916cf7;
		mem0[5]   <= 64'h0801f2e2_858efc16;
		mem0[6]   <= 64'h636920d8_71574e69;
		mem0[7]   <= 64'ha458fea3_f4933d7e;
		mem0[8]   <= 64'h0d95748f_728eb658;
		mem0[9]   <= 64'h718bcd58_82154aee;
		mem0[10]  <= 64'h7b54a41d_c25a59b5;
		mem0[11]  <= 64'h9c30d539_2af26013;
		mem0[12]  <= 64'hc5d1b023_286085f0;
		mem0[13]  <= 64'hca417918_b8db38ef;
		mem0[14]  <= 64'h8e79dcb0_603a180e;
		mem0[15]  <= 64'h6c9e0e8b_b01e8a3e;
		mem0[16]  <= 64'hd71577c1_bd314b27;
		mem0[17]  <= 64'h78af2fda_55605c60;
		mem0[18]  <= 64'he65525f3_aa55ab94;
		mem0[19]  <= 64'h57489862_63e81440;
		mem0[20]  <= 64'h55ca396a_2aab10b6;
		mem0[21]  <= 64'hb4cc5c34_1141e8ce;
		mem0[22]  <= 64'ha15486af_7c72e993;
		mem0[23]  <= 64'hb3ee1411_636fbc2a;
		mem0[24]  <= 64'h2ba9c55d_741831f6;
		mem0[25]  <= 64'hce5c3e16_9b87931e;
		mem0[26]  <= 64'hafd6ba33_6c24cf5c;
		mem0[27]  <= 64'h7a325381_28958677;
		mem0[28]  <= 64'h3b8f4898_6b4bb9af;
		mem0[29]  <= 64'hc4bfe81b_66282193;
		mem0[30]  <= 64'h61d809cc_fb21a991;
		mem0[31]  <= 64'h487cac60_5dec8032;
		mem0[32]  <= 64'hef845d5d_e98575b1;
		mem0[33]  <= 64'hdc262302_eb651b88;
		mem0[34]  <= 64'h23893e81_d396acc5;
		mem0[35]  <= 64'h0f6d6ff3_83f44239;
		mem0[36]  <= 64'h2e0b4482_a4842004;
		mem0[37]  <= 64'h69c8f04a_9e1f9b5e;
		mem0[38]  <= 64'h21c66842_f6e96c9a;
		mem0[39]  <= 64'h670c9c61_abd388f0;
		mem0[40]  <= 64'h6a51a0d2_d8542f68;
		mem0[41]  <= 64'h960fa728_ab5133a3;
		mem0[42]  <= 64'h6eef0b6c_137a3be4;
		mem0[43]  <= 64'hba3bf050_7efb2a98;
		mem0[44]  <= 64'ha1f1651d_39af0176;
		mem0[45]  <= 64'h66ca593e_82430e88;
		mem0[46]  <= 64'h8cee8619_456f9fb4;
		mem0[47]  <= 64'h7d84a5c3_3b8b5ebe;
		mem0[48]  <= 64'he06f75d8_85c12073;
		mem0[49]  <= 64'h401a449f_56c16aa6;
		mem0[50]  <= 64'h4ed3aa62_363f7706;
		mem0[51]  <= 64'h1bfedf72_429b023d;
		mem0[52]  <= 64'h37d0d724_d00a1248;
		mem0[53]  <= 64'hdb0fead3_49f1c09b;
		mem0[54]  <= 64'h075372c9_80991b7b;
		mem0[55]  <= 64'h25d479d8_f6e8def7;
		mem0[56]  <= 64'he3fe501a_b6794c3b;
		mem0[57]  <= 64'h976ce0bd_04c006ba;
		mem0[58]  <= 64'hc1a94fb6_409f60c4;
		mem0[59]  <= 64'h5e5c9ec2_196a2463;
		mem0[60]  <= 64'h68fb6faf_3e6c53b5;
		mem0[61]  <= 64'h1339b2eb_3b52ec6f;
		mem0[62]  <= 64'h6dfc511f_9b30952c;
		mem0[63]  <= 64'hcc814544_af5ebd09;
		mem0[64]  <= 64'hbee3d004_de334afd;
		mem0[65]  <= 64'h660f2807_192e4bb3;
		mem0[66]  <= 64'hc0cba857_45c8740f;
		mem0[67]  <= 64'hd20b5f39_b9d3fbdb;
		mem0[68]  <= 64'h5579c0bd_1a60320a;
		mem0[69]  <= 64'hd6a100c6_402c7279;
		mem0[70]  <= 64'h679f25fe_fb1fa3cc;
		mem0[71]  <= 64'h8ea5e9f8_db3222f8;
		mem0[72]  <= 64'h3c7516df_fd616b15;
		mem0[73]  <= 64'h2f501ec8_ad0552ab;
		mem0[74]  <= 64'h323db5fa_fd238760;
		mem0[75]  <= 64'h53317b48_3e00df82;
		mem0[76]  <= 64'h9e5c57bb_ca6f8ca0;
		mem0[77]  <= 64'h1a87562e_df1769db;
		mem0[78]  <= 64'hd542a8f6_287effc3;
		mem0[79]  <= 64'hac6732c6_8c4f5573;
		mem0[80]  <= 64'h695b27b0_bbca58c8;
		mem0[81]  <= 64'he1ffa35d_b8f011a0;
		mem0[82]  <= 64'h10fa3d98_fd2183b8;
		mem0[83]  <= 64'h4afcb56c_2dd1d35b;
		mem0[84]  <= 64'h9a53e479_b6f84565;
		mem0[85]  <= 64'hd28e49bc_4bfb9790;
		mem0[86]  <= 64'he1ddf2da_a4cb7e33;
		mem0[87]  <= 64'h62fb1341_cee4c6e8;
		mem0[88]  <= 64'hef20cada_36774c01;
		mem0[89]  <= 64'hd07e9efe_2bf11fb4;
		mem0[90]  <= 64'h95dbda4d_ae909198;
		mem0[91]  <= 64'heaad8e71_6b93d5a0;
		mem0[92]  <= 64'hd08ed1d0_afc725e0;
		mem0[93]  <= 64'h8e3c5b2f_8e7594b7;
		mem0[94]  <= 64'h8ff6e2fb_f2122b64;
		mem0[95]  <= 64'h8888b812_900df01c;
		mem0[96]  <= 64'h4fad5ea0_688fc31c;
		mem0[97]  <= 64'hd1cff191_b3a8c1ad;
		mem0[98]  <= 64'h2f2f2218_be0e1777;
		mem0[99]  <= 64'hea752dfe_8b021fa1;
		mem0[100] <= 64'he5a0cc0f_b56f74e8;
		mem0[101] <= 64'h18acf3d6_ce89e299;
		mem0[102] <= 64'hb4a84fe0_fd13e0b7;
		mem0[103] <= 64'h7cc43b81_d2ada8d9;
		mem0[104] <= 64'h165fa266_80957705;
		mem0[105] <= 64'h93cc7314_211a1477;
		mem0[106] <= 64'he6ad2065_77b5fa86;
		mem0[107] <= 64'hc75442f5_fb9d35cf;
		mem0[108] <= 64'hebcdaf0c_7b3e89a0;
		mem0[109] <= 64'hd6411bd3_ae1e7e49;
		mem0[110] <= 64'h00250e2d_2071b35e;
		mem0[111] <= 64'h226800bb_57b8e0af;
		mem0[112] <= 64'h2464369b_f009b91e;
		mem0[113] <= 64'h5563911d_59dfa6aa;
		mem0[114] <= 64'h78c14389_d95a537f;
		mem0[115] <= 64'h207d5ba2_02e5b9c5;
		mem0[116] <= 64'h83260376_6295cfa9;
		mem0[117] <= 64'h11c81968_4e734a41;
		mem0[118] <= 64'hb3472dca_7b14a94a;
		mem0[119] <= 64'h1b510052_9a532915;
		mem0[120] <= 64'hd60f573f_bc9bc6e4;
		mem0[121] <= 64'h2b60a476_81e67400;
		mem0[122] <= 64'h08ba6fb5_571be91f;
		mem0[123] <= 64'hf296ec6b_2a0dd915;
		mem0[124] <= 64'hb6636521_e7b9f9b6;
		mem0[125] <= 64'hff34052e_c5855664;
		mem0[126] <= 64'h53b02d5d_a99f8fa1;
		mem0[127] <= 64'h08ba4799_6e85076a;

		mem1[0]   <= 64'h4b7a70e9_b5b32944;
		mem1[1]   <= 64'hdb75092e_c4192623;
		mem1[2]   <= 64'had6ea6b0_49a7df7d;
		mem1[3]   <= 64'h9cee60b8_8fedb266;
		mem1[4]   <= 64'hecaa8c71_699a17ff;
		mem1[5]   <= 64'h5664526c_c2b19ee1;
		mem1[6]   <= 64'h193602a5_75094c29;
		mem1[7]   <= 64'ha0591340_e4183a3e;
		mem1[8]   <= 64'h3f54989a_5b429d65;
		mem1[9]   <= 64'h6b8fe4d6_99f73fd6;
		mem1[10]  <= 64'ha1d29c07_efe830f5;
		mem1[11]  <= 64'h4d2d38e6_f0255dc1;
		mem1[12]  <= 64'h4cdd2086_8470eb26;
		mem1[13]  <= 64'h6382e9c6_021ecc5e;
		mem1[14]  <= 64'h09686b3f_3ebaefc9;
		mem1[15]  <= 64'h3c971814_6b6a70a1;
		mem1[16]  <= 64'h687f3584_52a0e286;
		mem1[17]  <= 64'hb79c5305_aa500737;
		mem1[18]  <= 64'h3e07841c_7fdeae5c;
		mem1[19]  <= 64'h8e7d44ec_5716f2b8;
		mem1[20]  <= 64'hb03ada37_f0500c0d;
		mem1[21]  <= 64'hf01c1f04_0200b3ff;
		mem1[22]  <= 64'hae0cf51a_3cb574b2;
		mem1[23]  <= 64'h25837a58_dc0921bd;
		mem1[24]  <= 64'hd19113f9_7ca92ff6;
		mem1[25]  <= 64'h94324773_22f54701;
		mem1[26]  <= 64'h3ae5e581_37c2dadc;
		mem1[27]  <= 64'hc8b57634_9af3dda7;
		mem1[28]  <= 64'ha9446146_0fd0030e;
		mem1[29]  <= 64'hecc8c73e_a4751e41;
		mem1[30]  <= 64'he238cd99_3bea0e2f;
		mem1[31]  <= 64'h3280bba1_183eb331;
		mem1[32]  <= 64'h4e548b38_4f6db908;
		mem1[33]  <= 64'h6f420d03_f60a04bf;
		mem1[34]  <= 64'h2cb81290_24977c79;
		mem1[35]  <= 64'h5679b072_bcaf89af;
		mem1[36]  <= 64'hde9a771f_d9930810;
		mem1[37]  <= 64'hb38bae12_dccf3f2e;
		mem1[38]  <= 64'h5512721f_2e6b7124;
		mem1[39]  <= 64'h501adde6_9f84cd87;
		mem1[40]  <= 64'h7a584718_7408da17;
		mem1[41]  <= 64'hbc9f9abc_e94b7d8c;
		mem1[42]  <= 64'hec7aec3a_db851dfa;
		mem1[43]  <= 64'h63094366_c464c3d2;
		mem1[44]  <= 64'hef1c1847_3215d908;
		mem1[45]  <= 64'hdd433b37_24c2ba16;
		mem1[46]  <= 64'h12a14d43_2a65c451;
		mem1[47]  <= 64'h50940002_133ae4dd;
		mem1[48]  <= 64'h71dff89e_10314e55;
		mem1[49]  <= 64'h81ac77d6_5f11199b;
		mem1[50]  <= 64'h043556f1_d7a3c76b;
		mem1[51]  <= 64'h3c11183b_5924a509;
		mem1[52]  <= 64'hf28fe6ed_97f1fbfa;
		mem1[53]  <= 64'h9ebabf2c_1e153c6e;
		mem1[54]  <= 64'h86e34570_eae96fb1;
		mem1[55]  <= 64'h860e5e0a_5a3e2ab3;
		mem1[56]  <= 64'h771fe71c_4e3d06fa;
		mem1[57]  <= 64'h2965dcb9_99e71d0f;
		mem1[58]  <= 64'h803e89d6_5266c825;
		mem1[59]  <= 64'h2e4cc978_9c10b36a;
		mem1[60]  <= 64'hc6150eba_94e2ea78;
		mem1[61]  <= 64'ha5fc3c53_1e0a2df4;
		mem1[62]  <= 64'hf2f74ea7_361d2b3d;
		mem1[63]  <= 64'h1939260f_19c27960;
		mem1[64]  <= 64'h5223a708_f71312b6;
		mem1[65]  <= 64'hebadfe6e_eac31f66;
		mem1[66]  <= 64'he3bc4595_a67bc883;
		mem1[67]  <= 64'hb17f37d1_018cff28;
		mem1[68]  <= 64'hc332ddef_be6c5aa5;
		mem1[69]  <= 64'h65582185_68ab9802;
		mem1[70]  <= 64'heecea50f_db2f953b;
		mem1[71]  <= 64'h2aef7dad_5b6e2f84;
		mem1[72]  <= 64'h1521b628_29076170;
		mem1[73]  <= 64'hecdd4775_619f1510;
		mem1[74]  <= 64'h13cca830_eb61bd96;
		mem1[75]  <= 64'h0334fe1e_aa0363cf;
		mem1[76]  <= 64'hb5735c90_4c70a239;
		mem1[77]  <= 64'hd59e9e0b_cbaade14;
		mem1[78]  <= 64'heecc86bc_60622ca7;
		mem1[79]  <= 64'h9cab5cab_b2f3846e;
		mem1[80]  <= 64'h648b1eaf_19bdf0ca;
		mem1[81]  <= 64'ha02369b9_655abb50;
		mem1[82]  <= 64'h40685a32_3c2ab4b3;
		mem1[83]  <= 64'h319ee9d5_c021b8f7;
		mem1[84]  <= 64'h9b540b19_875fa099;
		mem1[85]  <= 64'h95f7997e_623d7da8;
		mem1[86]  <= 64'hf837889a_97e32d77;
		mem1[87]  <= 64'h11ed935f_16681281;
		mem1[88]  <= 64'h0e358829_c7e61fd6;
		mem1[89]  <= 64'h96dedfa1_7858ba99;
		mem1[90]  <= 64'h57f584a5_1b227263;
		mem1[91]  <= 64'h9b83c3ff_1ac24696;
		mem1[92]  <= 64'hcdb30aeb_532e3054;
		mem1[93]  <= 64'h8fd948e4_6dbc3128;
		mem1[94]  <= 64'h58ebf2ef_34c6ffea;
		mem1[95]  <= 64'hfe28ed61_ee7c3c73;
		mem1[96]  <= 64'h5d4a14d9_e864b7e3;
		mem1[97]  <= 64'h42105d14_203e13e0;
		mem1[98]  <= 64'h45eee2b6_a3aaabea;
		mem1[99]  <= 64'hdb6c4f15_facb4fd0;
		mem1[100] <= 64'hc742f442_ef6abbb5;
		mem1[101] <= 64'h654f3b1d_41cd2105;
		mem1[102] <= 64'hd81e799e_86854dc7;
		mem1[103] <= 64'he44b476a_3d816250;
		mem1[104] <= 64'hcf62a1f2_5b8d2646;
		mem1[105] <= 64'hfc8883a0_c1c7b6a3;
		mem1[106] <= 64'h7f1524c3_69cb7492;
		mem1[107] <= 64'h47848a0b_5692b285;
		mem1[108] <= 64'h095bbf00_ad19489d;
		mem1[109] <= 64'h1462b174_23820e00;
		mem1[110] <= 64'h58428d2a_0c55f5ea;
		mem1[111] <= 64'h1dadf43e_233f7061;
		mem1[112] <= 64'h3372f092_8d937e41;
		mem1[113] <= 64'hd65fecf1_6c223bdb;
		mem1[114] <= 64'h7cde3759_cbee7460;
		mem1[115] <= 64'h4085f2a7_ce77326e;
		mem1[116] <= 64'ha6078084_19f8509e;
		mem1[117] <= 64'he8efd855_61d99735;
		mem1[118] <= 64'ha969a7aa_c50c06c2;
		mem1[119] <= 64'h5a04abfc_800bcadc;
		mem1[120] <= 64'h9e447a2e_c3453484;
		mem1[121] <= 64'hfdd56705_0e1e9ec9;
		mem1[122] <= 64'hdb73dbd3_105588cd;
		mem1[123] <= 64'h675fda79_e3674340;
		mem1[124] <= 64'hc5c43465_713e38d8;
		mem1[125] <= 64'h3d28f89e_f16dff20;
		mem1[126] <= 64'h153e21e7_8fb03d4a;
		mem1[127] <= 64'he6e39f2b_db83adf7;


		mem2[0]  <= 64'he93d5a68_948140f7;
		mem2[1]  <= 64'hf64c261c_94692934;
		mem2[2]  <= 64'h411520f7_7602d4f7;
		mem2[3]  <= 64'hbcf46b2e_d4a20068;
		mem2[4]  <= 64'hd4082471_3320f46a;
		mem2[5]  <= 64'h43b7d4b7_500061af;
		mem2[6]  <= 64'h1e39f62e_97244546;
		mem2[7]  <= 64'h14214f74_bf8b8840;
		mem2[8]  <= 64'h4d95fc1d_96b591af;
		mem2[9]  <= 64'h70f4ddd3_66a02f45;
		mem2[10]  <= 64'hbfbc09ec_03bd9785;
		mem2[11]  <= 64'h7fac6dd0_31cb8504;
		mem2[12]  <= 64'h96eb27b3_55fd3941;
		mem2[13]  <= 64'hda2547e6_abca0a9a;
		mem2[14]  <= 64'h28507825_530429f4;
		mem2[15]  <= 64'h0a2c86da_e9b66dfb;
		mem2[16]  <= 64'h68dc1462_d7486900;
		mem2[17]  <= 64'h680ec0a4_27a18dee;
		mem2[18]  <= 64'h4f3ffea2_e887ad8c;
		mem2[19]  <= 64'hb58ce006_7af4d6b6;
		mem2[20]  <= 64'haace1e7c_d3375fec;
		mem2[21]  <= 64'hce78a399_406b2a42;
		mem2[22]  <= 64'h20fe9e35_d9f385b9;
		mem2[23]  <= 64'hee39d7ab_3b124e8b;
		mem2[24]  <= 64'h1dc9faf7_4b6d1856;
		mem2[25]  <= 64'h26a36631_eae397b2;
		mem2[26]  <= 64'h3a6efa74_dd5b4332;
		mem2[27]  <= 64'h6841e7f7_ca7820fb;
		mem2[28]  <= 64'hfb0af54e_d8feb397;
		mem2[29]  <= 64'h454056ac_ba489527;
		mem2[30]  <= 64'h55533a3a_20838d87;
		mem2[31]  <= 64'hfe6ba9b7_d096954b;
		mem2[32]  <= 64'h55a867bc_a1159a58;
		mem2[33]  <= 64'hcca92963_99e1db33;
		mem2[34]  <= 64'ha62a4a56_3f3125f9;
		mem2[35]  <= 64'h5ef47e1c_9029317c;
		mem2[36]  <= 64'hfdf8e802_04272f70;
		mem2[37]  <= 64'h80bb155c_05282ce3;
		mem2[38]  <= 64'h95c11548_e4c66d22;
		mem2[39]  <= 64'h48c1133f_c70f86dc;
		mem2[40]  <= 64'h07f9c9ee_41041f0f;
		mem2[41]  <= 64'h404779a4_5d886e17;
		mem2[42]  <= 64'h325f51eb_d59bc0d1;
		mem2[43]  <= 64'hf2bcc18f_41113564;
		mem2[44]  <= 64'h257b7834_602a9c60;
		mem2[45]  <= 64'hdff8e8a3_1f636c1b;
		mem2[46]  <= 64'h0e12b4c2_02e1329e;
		mem2[47]  <= 64'haf664fd1_cad18115;
		mem2[48]  <= 64'h6b2395e0_333e92e1;
		mem2[49]  <= 64'h3b240b62_eebeb922;
		mem2[50]  <= 64'h85b2a20e_e6ba0d99;
		mem2[51]  <= 64'hde720c8c_2da2f728;
		mem2[52]  <= 64'hd0127845_95b794fd;
		mem2[53]  <= 64'h647d0862_e7ccf5f0;
		mem2[54]  <= 64'h5449a36f_877d48fa;
		mem2[55]  <= 64'hc39dfd27_f33e8d1e;
		mem2[56]  <= 64'h0a476341_992eff74;
		mem2[57]  <= 64'h3a6f6eab_f4f8fd37;
		mem2[58]  <= 64'ha812dc60_a1ebddf8;
		mem2[59]  <= 64'h991be14c_db6e6b0d;
		mem2[60]  <= 64'hc67b5510_6d672c37;
		mem2[61]  <= 64'h2765d43b_dcd0e804;
		mem2[62]  <= 64'hf1290dc7_cc00ffa3;
		mem2[63]  <= 64'hb5390f92_690fed0b;
		mem2[64]  <= 64'h667b9ffb_cedb7d9c;
		mem2[65]  <= 64'ha091cf0b_d9155ea3;
		mem2[66]  <= 64'hbb132f88_515bad24;
		mem2[67]  <= 64'h7b9479bf_763bd6eb;
		mem2[68]  <= 64'h37392eb3_cc115979;
		mem2[69]  <= 64'h8026e297_f42e312d;
		mem2[70]  <= 64'h6842ada7_c66a2b3b;
		mem2[71]  <= 64'h12754ccc_782ef11c;
		mem2[72]  <= 64'h6a124237_b79251e7;
		mem2[73]  <= 64'h06a1bbe6_4bfb6350;
		mem2[74]  <= 64'h1a6b1018_11caedfa;
		mem2[75]  <= 64'h3d25bdd8_e2e1c3c9;
		mem2[76]  <= 64'h44421659_0a121386;
		mem2[77]  <= 64'hd90cec6e_d5abea2a;
		mem2[78]  <= 64'h64af674e_da86a85f;
		mem2[79]  <= 64'hbebfe988_64e4c3fe;
		mem2[80]  <= 64'h9dbc8057_f0f7c086;
		mem2[81]  <= 64'h60787bf8_6003604d;
		mem2[82]  <= 64'hd1fd8346_f6381fb0;
		mem2[83]  <= 64'h7745ae04_d736fccc;
		mem2[84]  <= 64'h83426b33_f01eab71;
		mem2[85]  <= 64'hb0804187_3c005e5f;
		mem2[86]  <= 64'h77a057be_bde8ae24;
		mem2[87]  <= 64'h55464299_bf582e61;
		mem2[88]  <= 64'h4e58f48f_f2ddfda2;
		mem2[89]  <= 64'hf474ef38_8789bdc2;
		mem2[90]  <= 64'h5366f9c3_c8b38e74;
		mem2[91]  <= 64'hb475f255_46fcd9b9;
		mem2[92]  <= 64'h7aeb2661_8b1ddf84;
		mem2[93]  <= 64'h846a0e79_915f95e2;
		mem2[94]  <= 64'h466e598e_20b45770;
		mem2[95]  <= 64'h8cd55591_c902de4c;
		mem2[96]  <= 64'hb90bace1_bb8205d0;
		mem2[97]  <= 64'h11a86248_7574a99e;
		mem2[98]  <= 64'hb77f19b6_e0a9dc09;
		mem2[99]  <= 64'h662d09a1_c4324633;
		mem2[100] <= 64'he85a1f02_09f0be8c;
		mem2[101] <= 64'h4a99a025_1d6efe10;
		mem2[102] <= 64'h1ab93d1d_0ba5a4df;
		mem2[103] <= 64'ha186f20f_2868f169;
		mem2[104] <= 64'hdcb7da83_573906fe;
		mem2[105] <= 64'ha1e2ce9b_4fcd7f52;
		mem2[106] <= 64'h50115e01_a70683fa;
		mem2[107] <= 64'ha002b5c4_0de6d027;
		mem2[108] <= 64'h9af88c27_773f8641;
		mem2[109] <= 64'hc3604c06_61a806b5;
		mem2[110] <= 64'hf0177a28_c0f586e0;
		mem2[111] <= 64'h006058aa_30dc7d62;
		mem2[112] <= 64'h11e69ed7_2338ea63;
		mem2[113] <= 64'h53c2dd94_c2c21634;
		mem2[114] <= 64'hbbcbee56_90bcb6de;
		mem2[115] <= 64'hebfc7da1_ce591d76;
		mem2[116] <= 64'h6f05e409_4b7c0188;
		mem2[117] <= 64'h39720a3d_7c927c24;
		mem2[118] <= 64'h86e3725f_724d9db9;
		mem2[119] <= 64'h1ac15bb4_d39eb8fc;
		mem2[120] <= 64'hed545578_08fca5b5;
		mem2[121] <= 64'hd83d7cd3_4dad0fc4;
		mem2[122] <= 64'h1e50ef5e_b161e6f8;
		mem2[123] <= 64'ha28514d9_6c51133c;
		mem2[124] <= 64'h6fd5c7e7_56e14ec4;
		mem2[125] <= 64'h362abfce_ddc6c837;
		mem2[126] <= 64'hd79a3234_92638212;
		mem2[127] <= 64'h670efa8e_406000e0;
			

		mem3[0]  <= 64'h3a39ce37_d3faf5cf;
		mem3[1]  <= 64'habc27737_5ac52d1b;
		mem3[2]  <= 64'h5cb0679e_4fa33742;
		mem3[3]  <= 64'hd3822740_99bc9bbe;
		mem3[4]  <= 64'hd5118e9d_bf0f7315;
		mem3[5]  <= 64'hd62d1c7e_c700c47b;
		mem3[6]  <= 64'hb78c1b6b_21a19045;
		mem3[7]  <= 64'hb26eb1be_6a366eb4;
		mem3[8]  <= 64'h5748ab2f_bc946e79;
		mem3[9]  <= 64'hc6a376d2_6549c2c8;
		mem3[10]  <= 64'h530ff8ee_468dde7d;
		mem3[11]  <= 64'hd5730a1d_4cd04dc6;
		mem3[12]  <= 64'h2939bbdb_a9ba4650;
		mem3[13]  <= 64'hac9526e8_be5ee304;
		mem3[14]  <= 64'ha1fad5f0_6a2d519a;
		mem3[15]  <= 64'h63ef8ce2_9a86ee22;
		mem3[16]  <= 64'hc089c2b8_43242ef6;
		mem3[17]  <= 64'ha51e03aa_9cf2d0a4;
		mem3[18]  <= 64'h83c061ba_9be96a4d;
		mem3[19]  <= 64'h8fe51550_ba645bd6;
		mem3[20]  <= 64'h2826a2f9_a73a3ae1;
		mem3[21]  <= 64'h4ba99586_ef5562e9;
		mem3[22]  <= 64'hc72fefd3_f752f7da;
		mem3[23]  <= 64'h3f046f69_77fa0a59;
		mem3[24]  <= 64'h80e4a915_87b08601;
		mem3[25]  <= 64'h9b09e6ad_3b3ee593;
		mem3[26]  <= 64'he990fd5a_9e34d797;
		mem3[27]  <= 64'h2cf0b7d9_022b8b51;
		mem3[28]  <= 64'h96d5ac3a_017da67d;
		mem3[29]  <= 64'hd1cf3ed6_7c7d2d28;
		mem3[30]  <= 64'h1f9f25cf_adf2b89b;
		mem3[31]  <= 64'h5ad6b472_5a88f54c;
		mem3[32]  <= 64'he029ac71_e019a5e6;
		mem3[33]  <= 64'h47b0acfd_ed93fa9b;
		mem3[34]  <= 64'he8d3c48d_283b57cc;
		mem3[35]  <= 64'hf8d56629_79132e28;
		mem3[36]  <= 64'h785f0191_ed756055;
		mem3[37]  <= 64'hf7960e44_e3d35e8c;
		mem3[38]  <= 64'h15056dd4_88f46dba;
		mem3[39]  <= 64'h03a16125_0564f0bd;
		mem3[40]  <= 64'hc3eb9e15_3c9057a2;
		mem3[41]  <= 64'h97271aec_a93a072a;
		mem3[42]  <= 64'h1b3f6d9b_1e6321f5;
		mem3[43]  <= 64'hf59c66fb_26dcf319;
		mem3[44]  <= 64'h7533d928_b155fdf5;
		mem3[45]  <= 64'h03563482_8aba3cbb;
		mem3[46]  <= 64'h28517711_c20ad9f8;
		mem3[47]  <= 64'habcc5167_ccad925f;
		mem3[48]  <= 64'h4de81751_3830dc8e;
		mem3[49]  <= 64'h379d5862_9320f991;
		mem3[50]  <= 64'hea7a90c2_fb3e7bce;
		mem3[51]  <= 64'h5121ce64_774fbe32;
		mem3[52]  <= 64'ha8b6e37e_c3293d46;
		mem3[53]  <= 64'h48de5369_6413e680;
		mem3[54]  <= 64'ha2ae0810_dd6db224;
		mem3[55]  <= 64'h69852dfd_09072166;
		mem3[56]  <= 64'hb39a460a_6445c0dd;
		mem3[57]  <= 64'h586cdecf_1c20c8ae;
		mem3[58]  <= 64'h5bbef7dd_1b588d40;
		mem3[59]  <= 64'hccd2017f_6bb4e3bb;
		mem3[60]  <= 64'hdda26a7e_3a59ff45;
		mem3[61]  <= 64'h3e350a44_bcb4cdd5;
		mem3[62]  <= 64'h72eacea8_fa6484bb;
		mem3[63]  <= 64'h8d6612ae_bf3c6f47;
		mem3[64]  <= 64'hd29be463_542f5d9e;
		mem3[65]  <= 64'haec2771b_f64e6370;
		mem3[66]  <= 64'h740e0d8d_e75b1357;
		mem3[67]  <= 64'hf8721671_af537d5d;
		mem3[68]  <= 64'h4040cb08_4eb4e2cc;
		mem3[69]  <= 64'h34d2466a_0115af84;
		mem3[70]  <= 64'he1b00428_95983a1d;
		mem3[71]  <= 64'h06b89fb4_ce6ea048;
		mem3[72]  <= 64'h6f3f3b82_3520ab82;
		mem3[73]  <= 64'h011a1d4b_277227f8;
		mem3[74]  <= 64'h611560b1_e7933fdc;
		mem3[75]  <= 64'hbb3a792b_344525bd;
		mem3[76]  <= 64'ha08839e1_51ce794b;
		mem3[77]  <= 64'h2f32c9b7_a01fbac9;
		mem3[78]  <= 64'he01cc87e_bcc7d1f6;
		mem3[79]  <= 64'hcf0111c3_a1e8aac7;
		mem3[80]  <= 64'h1a908749_d44fbd9a;
		mem3[81]  <= 64'hd0dadecb_d50ada38;
		mem3[82]  <= 64'h0339c32a_c6913667;
		mem3[83]  <= 64'h8df9317c_e0b12b4f;
		mem3[84]  <= 64'hf79e59b7_43f5bb3a;
		mem3[85]  <= 64'hf2d519ff_27d9459c;
		mem3[86]  <= 64'hbf97222c_15e6fc2a;
		mem3[87]  <= 64'h0f91fc71_9b941525;
		mem3[88]  <= 64'hfae59361_ceb69ceb;
		mem3[89]  <= 64'hc2a86459_12baa8d1;
		mem3[90]  <= 64'hb6c1075e_e3056a0c;
		mem3[91]  <= 64'h10d25065_cb03a442;
		mem3[92]  <= 64'he0ec6e0e_1698db3b;
		mem3[93]  <= 64'h4c98a0be_3278e964;
		mem3[94]  <= 64'h9f1f9532_e0d392df;
		mem3[95]  <= 64'hd3a0342b_8971f21e;
		mem3[96]  <= 64'h1b0a7441_4ba3348c;
		mem3[97]  <= 64'hc5be7120_c37632d8;
		mem3[98]  <= 64'hdf359f8d_9b992f2e;
		mem3[99]  <= 64'he60b6f47_0fe3f11d;
		mem3[100] <= 64'he54cda54_1edad891;
		mem3[101] <= 64'hce6279cf_cd3e7e6f;
		mem3[102] <= 64'h1618b166_fd2c1d05;
		mem3[103] <= 64'h848fd2c5_f6fb2299;
		mem3[104] <= 64'hf523f357_a6327623;
		mem3[105] <= 64'h93a83531_56cccd02;
		mem3[106] <= 64'hacf08162_5a75ebb5;
		mem3[107] <= 64'h6e163697_88d273cc;
		mem3[108] <= 64'hde966292_81b949d0;
		mem3[109] <= 64'h4c50901b_71c65614;
		mem3[110] <= 64'he6c6c7bd_327a140a;
		mem3[111] <= 64'h45e1d006_c3f27b9a;
		mem3[112] <= 64'hc9aa53fd_62a80f00;
		mem3[113] <= 64'hbb25bfe2_35bdd2f6;
		mem3[114] <= 64'h71126905_b2040222;
		mem3[115] <= 64'hb6cbcf7c_cd769c2b;
		mem3[116] <= 64'h53113ec0_1640e3d3;
		mem3[117] <= 64'h38abbd60_2547adf0;
		mem3[118] <= 64'hba38209c_f746ce76;
		mem3[119] <= 64'h77afa1c5_20756060;
		mem3[120] <= 64'h85cbfe4e_8ae88dd8;
		mem3[121] <= 64'h7aaaf9b0_4cf9aa7e;
		mem3[122] <= 64'h1948c25c_02fb8a8c;
		mem3[123] <= 64'h01c36ae4_d6ebe1f9;
		mem3[124] <= 64'h90d4f869_a65cdea0;
		mem3[125] <= 64'h3f09252d_c208e69f;
		mem3[126] <= 64'hb74e6132_ce77e25b;
		mem3[127] <= 64'h578fdfe3_3ac372e6;
	  end
	  else if (wr_en) begin
	    if (cs0) begin
		  mem0[wl[6:0]] <= bl;
		end
	    else if (cs1) begin
		  mem1[wl[13:7]] <= bl;
		end
	    else if (cs2) begin
		  mem2[wl[20:14]] <= bl;
		end
	    else if (cs3) begin
		  mem3[wl[27:21]] <= bl;
		end
		dlatch0 <= 0;
		dlatch1 <= 0;
		dlatch2 <= 0;
		dlatch3 <= 0;
	  end
	  else if (re_en) begin
		dlatch0 <= ((dsel0) ? mem0[wl[6:0]][63:32]   : mem0[wl[6:0]][31:0]);
		dlatch1 <= ((dsel1) ? mem1[wl[13:7]][63:32]  : mem1[wl[13:7]][31:0]);
		dlatch2 <= ((dsel2) ? mem2[wl[20:14]][63:32] : mem2[wl[20:14]][31:0]);
		dlatch3 <= ((dsel3) ? mem3[wl[27:21]][63:32] : mem3[wl[27:21]][31:0]);
	  end
	  else begin
	    dlatch0 <= 0;
	    dlatch1 <= 0;
	    dlatch2 <= 0;
	    dlatch3 <= 0;
	  end
	end

	assign re_en = ((~rw_sel) && bl_ready);
	assign wr_en = (rw_sel && bl_ready);
	assign bl_ready = ((bl == {64{1'b1}}) || rw_sel);
	   
/* 
	always_ff @(posedge clk_1, posedge clk_2, negedge clk_1, negedge clk_2, negedge rst_l) begin
	  if (~rst_l) begin
	    buffer_thing <= 0;
	  end
	  else if (clk_1 || clk_2) begin
	    buffer_thing <= 1;
	  end
	  else begin
	    buffer_thing <= 0;
	  end
	end

	always_ff @(posedge buffer_thing, negedge rst_l) begin
	  if (~rst_l) begin
	    bl_ready <= 0;
	    op_in <= 0;
	  end
	  else if (buffer_thing)  begin
		bl_ready <= ((bl == {64{1'b1}}) || rw_sel);
	    op_in <= rw_sel;
	  end
	end
*/

	always_ff @(posedge clk_1, posedge clk_2, negedge clk_1, negedge clk_2, negedge rst_l) begin
	  if (~rst_l) begin
	    data_ready <= 0;
	  end
	  else if (clk_1 || clk_2) begin
	    data_ready <= 1;
	  end
	  else begin
	    data_ready <= 0;
	  end
	end

	always_ff @(posedge clk_1, posedge clk_2, negedge rst_l) begin
	  if (~rst_l) begin
		bl_in <= 0;
	  end
	  else begin
		bl_in <= bl;
	  end
	end

	always_ff @(posedge clk_1, negedge rst_l) begin
	  if (~rst_l) begin
	    data0 <= 0;
	    data1 <= 0;
	    data2 <= 0;
	    data3 <= 0;
	  end
	  else begin
	    data0 <= dlatch0;
	    data1 <= dlatch1;
	    data2 <= dlatch2;
	    data3 <= dlatch3;
	  end
	end

endmodule : sram

