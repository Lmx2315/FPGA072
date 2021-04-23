module work_clock (
	input clk,    // Clock
	input sec,    //вход секундной метки
	input rst,    // synchronous reset active low
	output [31:0] data   //выход подсчитанного времени, в секундах работы
);

logic [31:0] sch=0;
logic [ 2:0] frnt=0;

always_ff @(posedge clk) frnt<={frnt[1:0],sec};//ищем фронт секундной метки

always_ff @(posedge clk) 
	if(rst) 
	begin
	sch	 <= 0;
	end else 
	begin
	if (frnt==3'b001)	 sch<=sch+1;
	end

assign data=sch;

endmodule