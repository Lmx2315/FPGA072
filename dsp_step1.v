module dsp_step1 (

input wire clk,
input wire rst,
input wire         [15:0] in,
output wire        [31:0] data,
output wire valid
);

  parameter freq1=32'h64000000;//75 Mhz	 
//parameter freq1=32'h22222222;//32 Mhz	 для тестового синуса с АЦП

wire [19:0] w_i_nco;
wire [19:0] w_q_nco;
wire nco_valid;

reg [19:0] w_i_nco_temp;
reg [19:0] w_q_nco_temp;
reg nco_valid_temp;

reg n_rst0  =0;
reg n_rst1  =0;
reg n_rst2  =0;
reg n_rst3  =0;
reg n_rst4  =0;

always @(posedge clk) n_rst0   <=~rst;
always @(posedge clk) n_rst1   <=~rst;
always @(posedge clk) n_rst2   <=~rst;
always @(posedge clk) n_rst3   <=~rst;
always @(posedge clk) n_rst4   <=~rst;


//-------------------------------------
/*
GBUF_RST 
gbf1 (
		.inclk  (n_rst),  // 
		.outclk (wglob_rst)  // 
	);
*/	
//-------------------------------------

dds_dsp 
nc1 (
		.clk       (clk),        // clk.clk
		.reset_n   (n_rst0),      // rst.reset_n
		.clken     (1'h1),       //  in.clken
		.phi_inc_i (freq1),      //    .phi_inc_i
		.fsin_o    (w_i_nco),    // out.fsin_o
		.fcos_o    (w_q_nco),    //    .fcos_o
		.out_valid (nco_valid)   //    .out_valid
	);
	
reg [15:0] data_temp1=0;
	
always @(posedge clk)
if (nco_valid)
begin
w_i_nco_temp  <=w_i_nco;
w_q_nco_temp  <=w_q_nco;
		 nco_valid_temp<=1;
end else nco_valid_temp<=0;

always @(posedge clk) data_temp1<=in;//ввёл  для компенсации нарушения констрейнов в плис
	
wire [19:0] i_to_cic;
wire [19:0] q_to_cic; 
	
	
mixer_to_iq 
mix1(
	.o_i(i_to_cic),
	.o_q(q_to_cic),
	.clk(clk),
	.in(data_temp1),//s_i_nco[17:2]
	.nco_i(w_i_nco_temp),//w_i_nco
	.nco_q(w_q_nco_temp) //w_q_nco
);


wire [19:0] i_cic_to_fir;
wire i_fir_valid;
wire [19:0] q_cic_to_fir;
wire q_fir_valid;
	
cic 
cici (
		.clk       (clk),       //     clock.clk
		.reset_n   (n_rst1),   //     reset.reset_n
		.in_error  (0),  //  av_st_in.error
		.in_valid  (1),  //          .valid
		.in_ready  (),  //          .ready
		.in_data   (i_to_cic),   //          .in_data test_i_nco[17:2]
		.out_data  (i_cic_to_fir),  // av_st_out.out_data
		.out_error (), //          .error
		.out_valid (i_fir_valid), //          .valid
		.out_ready (1)  //          .ready  
	);
	
	
cic 
cicq (
		.clk       (clk),       //     clock.clk
		.reset_n   (n_rst2),   //     reset.reset_n
		.in_error  (0),  //  av_st_in.error
		.in_valid  (1),  //          .valid
		.in_ready  (),  //          .ready
		.in_data   (q_to_cic),   //          .in_data test_q_nco[17:2]
		.out_data  (q_cic_to_fir),  // av_st_out.out_data
		.out_error (), //          .error
		.out_valid (q_fir_valid), //          .valid
		.out_ready (1)  //          .ready  
	);

reg [19:0] r_i_cic_to_fir;
reg r_i_fir_valid;
reg [19:0] r_q_cic_to_fir;
reg r_q_fir_valid;	
	
always @(posedge clk)
if (i_fir_valid)
begin
r_i_cic_to_fir<=i_cic_to_fir;
r_i_fir_valid <=1;
end	else r_i_fir_valid <=0;

always @(posedge clk)
if (q_fir_valid)
begin
r_q_cic_to_fir<=q_cic_to_fir;
r_q_fir_valid <=1;
end	else r_q_fir_valid <=0;

wire [19:0] i_fir_data;	 
wire  i_sor_valid; 
 
wire [19:0] q_fir_data;	 
wire  q_sor_valid;

fir2
fir_i (
		.clk              (clk),              //                     clk.clk
		.reset_n          (n_rst3),          //                     rst.reset_n
		.ast_sink_data    (r_i_cic_to_fir),    //  {w_dec_dds_q,4'b0000} avalon_streaming_sink.data 
		.ast_sink_valid   (r_i_fir_valid),   //                        .valid  
		.ast_sink_error   (2'b00),   //                        .error
		.ast_source_data  (i_fir_data),  // avalon_streaming_source.data
		.ast_source_valid (i_sor_valid), //                        .valid
		.ast_source_error ()  //                        .error
	);


fir2
fir_q (
		.clk              (clk),              //                     clk.clk
		.reset_n          (n_rst4),          //                     rst.reset_n
		.ast_sink_data    (r_q_cic_to_fir),    //  {w_dec_dds_q,4'b0000} avalon_streaming_sink.data 
		.ast_sink_valid   (r_q_fir_valid),   //                        .valid  
		.ast_sink_error   (2'b00),   //                        .error
		.ast_source_data  (q_fir_data),  // avalon_streaming_source.data
		.ast_source_valid (q_sor_valid), //                        .valid
		.ast_source_error ()  //                        .error
	);	

reg [31:0] data_dsp=0;
reg [15:0] data_i=0;
reg [15:0] data_q=0;
reg data_valid=0;

always @(posedge clk)
if (q_sor_valid&i_sor_valid)
begin
data_i    <=i_fir_data[19:4];
data_q    <=q_fir_data[19:4];
data_dsp  <={data_i,data_q};
		 data_valid<=1;
end else data_valid<=0;
	
assign data=data_dsp;
assign valid=data_valid;	

	
endmodule
	