//-----------------------------------------------------------------------------
//
// Title       : time_control
// Design      : list
// Author      : Microsoft
// Company     : Microsoft
//
//-----------------------------------------------------------------------------
//
// File        : d:\My_Designs\project\list\src\time_control.v
// Generated   : Tue Apr  9 09:45:00 2019
// From        : interface description file
// By          : Itf2Vhdl ver. 1.22
//
//-----------------------------------------------------------------------------
//
// Description : 
//
//-----------------------------------------------------------------------------
`timescale 1 ns / 1 ps

//{{ Section below this comment is automatically maintained
//   and may be overwritten
//{module {time_control}}
module packet_counter (clk,clr,Numb_inter,n_ch,ev,q );

output [31:0] q ;
wire [31:0] q ;

input [15:0] Numb_inter ;
wire  [15:0] Numb_inter ;

input ev ;
wire ev ;
input clk ;
wire clk ;
input clr ;
wire clr ;
input [7:0] n_ch ;
wire  [7:0] n_ch ;
 
reg  [31:0] sch_int=0;
reg  [31:0] accum0=0;	 
reg  [31:0] accum1=0;
wire [31:0] data;
reg FLAG=0;

assign q={Numb_inter,data[15:0]};	//объеденяем и выдаём комбинацию номер интервала после секунднйо метки + номер пачки внутри интервала  

assign data=(FLAG)?accum1:accum0;
	
always @(posedge clk)
	if (clr)
		begin
		accum0<=0;	
		accum1<=0;
//		data  <=0;	
		end
	else
	if (ev)
	begin	
	if (n_ch==0) 
		begin
		FLAG  <=0;
		accum0<=accum0+1; 
//		data  <=accum0;
		end
		else
	if (n_ch==1) 
		begin
		FLAG  <=1;
		accum1<=accum1+1;
//		data  <=accum1;
	    end                  
	end	

endmodule
