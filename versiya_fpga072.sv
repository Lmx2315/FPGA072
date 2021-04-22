module versiya_fpga072 (
	output [47:0] data	
);

reg [47:0] ver_data=48'h220420211535;//29-03-2021 17-06

always_comb 
begin
data=ver_data;
end

endmodule