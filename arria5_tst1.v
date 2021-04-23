
//synthesis_resources = arriav_io_ibuf 1 
//synopsys translate_off
`timescale 1 ps / 1 ps
//synopsys translate_on

module top_module(

		input wire clk_100MHZ,		
//		input wire si570_clk,		
		input wire REF3,
	    input wire SYS_REF,//
		input wire SYSREF0,		
		input wire SYSREF5,
		input wire SYSREF6,	
	    input wire FPGA_SYNC1,			
		input wire FPGA_SYNC2,	
		input wire FPGA_SYNC3,			
//--------------------------------

output wire UPR_AT4_05,
output wire UPR_AT4_1,
output wire UPR_AT4_2,
output wire UPR_AT4_4,
output wire UPR_AT4_8,
output wire UPR_AT4_16,
	  
output wire UPR_AT1_05,
output wire UPR_AT1_1,
output wire UPR_AT1_2,
output wire UPR_AT1_4,
output wire UPR_AT1_8,
output wire UPR_AT1_16,
	  
output wire UPR_AT2_05,
output wire UPR_AT2_1,
output wire UPR_AT2_2,
output wire UPR_AT2_4,
output wire UPR_AT2_8,
output wire UPR_AT2_16,
	  
output wire UPR_AT3_05,
output wire UPR_AT3_1,
output wire UPR_AT3_2,
output wire UPR_AT3_4,
output wire UPR_AT3_8,
output wire UPR_AT3_16,
	  
output wire UPR_SWITCH1,
output wire UPR_SWITCH2,
output wire UPR_SWITCH3,
output wire UPR_SWITCH4,

output wire UPR_SWITCH1_OFF,
output wire UPR_SWITCH2_OFF,
output wire UPR_SWITCH3_OFF,
output wire UPR_SWITCH4_OFF,
	  
output wire LMK_STATUS_LD1_FPGA_3V3,
output wire LMK_STATUS_LD2_FPGA_3V3,
	  
input wire LMK_STATUS_LD1_3V3,
input wire LMK_STATUS_LD2_3V3,
	  
output wire D1_ALARM_FPGA,
output wire D2_ALARM_FPGA,
	  
output wire EN_2V5_VDA,
output wire REZERV1,
output wire FPGA_IND1,
output wire FPGA_IND2,
	  
input wire ADDR0,
input wire ADDR1,
input wire ADDR2,
input wire ADDR3,
 
input wire CS_ADC1,
input wire CS_ADC2,
input wire CS_DAC1,
input wire CS_DAC2,
input wire CS_LMK,
input wire CS_FPGA1,
input wire CS_FPGA2,
	  
//------------------------------------------------------
	
input  wire UART6_TX, 			//uart6 mk
output wire UART6_RX,
	 
input  wire UART1_TX, 			//uart1 mk
output wire UART1_RX,
	 
input  wire TX_FTDI_1, 			//FTDI1 mk
output wire RX_FTDI_1,
	 
input  wire TX_FTDI_2,	 		//FTDI2 mk
output wire RX_FTDI_2,
	 
output wire BOOT1,
output wire BOOT0,
	 
input wire BOOT_MK_FTDI,
input wire RESET_MK_FTDI,	 
	 
 input wire MR_RESET_MK_FPGA, 	// сигнал ресет для МК
output wire RESET_MK,
 input wire BOOT_MK_FPGA, 		// сигнал , буут для МК
output wire CLK_FOR_MK_3V3,     // clk на МК 
	  
output wire TNC_MK_1HZ, 		//1 сек к mk
output wire INT1_FPGA,			//прерывание MAC0
	  	  
input   wire WDATA_MK0, //от МК к ПЛИС 
output  wire WDATA_MK1, //
output  wire WDATA_MK2, //эта ветка в МК занята прерыванием от MAC1
output  wire WDATA_MK3, //прерывание от WCW
input   wire WDATA_MK4, //это будет сигнал DE/RE для 485 шины
output  wire WDATA_MK5, //
output  wire WDATA_MK6, //
output  wire WDATA_MK7, //

output wire OK_BUS,
input  wire WR_BUS,
input  wire RD_BUS,
	  
//--------------------------
input  wire SPI4_SCK_MK,  //SPI , слейв ПЛИС  (скоростной интерфейс для команд реального времени
input  wire SPI4_NSS_MK,  //передача идёт пачкой по 51 байт
input  wire SPI4_MOSI_MK,
output wire SPI4_MISO_MK,	  
	  
 input wire SPI3_SCK,  		//SPI где слейв ПЛИС
 input wire SPI3_CS,
output wire SPI3_MISO,
 input wire SPI3_MOSI,
	  
output wire SPI2_NSS_MK, 
	  
//-----------GTX---------------
	
     input wire RX_GTP, //ETH1
    output wire TX_GTP,

    input wire RX2_GTP, //ETH2
   output wire TX2_GTP,

//	  input wire FPGA_F48MHZ,
//	  input wire DCLK6,
//	  input wire DCLK5,
//	  input wire DCLK0,
//-----------------------
	 output wire D2_RX0,
	 output wire D2_RX1,

	 output wire D1_RX0,
	 output wire D1_RX1,	 
//-----------------------

	  input wire A2_TX0,
	  input wire A2_TX1,
      input wire A2_TX3,	  
 	  input wire A2_TX2,
	  
	  input wire A1_TX3, //
	  input wire A1_TX2,
	  input wire A1_TX1,
	  input wire A1_TX0,
 	  
//-----------DAC---------------
	  
output wire DAC_SYNC, //Сигнал идёт наружу кассеты к АЦ
	  
 input wire D2_SYNCB,
 input wire D1_SYNCB,
  
output wire D1_RESETB,
output wire D1_SCLK,
output wire D1_SDIO,
 input wire D1_SDO,
output wire D1_SDENB,
output wire D1_SLEEP,
input  wire D1_SYNC_N_AB,
input  wire D1_SYNC_N_CD,
output wire D1_TXENABLE,	 
 input wire D1_ALARM,
	 
output wire D2_RESETB,
output wire D2_SCLK,
output wire D2_SDIO,
 input wire D2_SDO,
output wire D2_SDENB,
output wire D2_SLEEP,
 input wire D2_SYNC_N_AB,
 input wire D2_SYNC_N_CD,
output wire D2_TXENABLE,
 input wire D2_ALARM,
	  
//-----------ADC---------------

	input wire A1_OVRA_1V8,
	input wire A1_OVRB_1V8,
   output wire A1_CTRL2_1V8,
   output wire A1_CTRL1_1V8,
   output wire A1_PWRDOWN_1V8, 
   output wire A1_RESET_1V8,
   output wire A1_SCLK_1V8,
   output wire A1_SDATA_1V8,
    input wire A1_SDOUT_1V8,
   output wire A1_SEN_1V8,
   output wire A1_STBY_1V8,
		  
    input wire A2_OVRA_1V8,
    input wire A2_OVRB_1V8,
   output wire A2_CTRL2_1V8,
   output wire A2_CTRL1_1V8,
   output wire A2_PWRDOWN_1V8, 
   output wire A2_RESET_1V8,
   output wire A2_SCLK_1V8,
   output wire A2_SDATA_1V8,
    input wire A2_SDOUT_1V8,
   output wire A2_SEN_1V8,
   output wire A2_STBY_1V8,
  
   output wire A1_SNC,
   output wire A2_SNC,  
	  
//-----------LMK---------------
	  output wire LMK_SEL0_3V3,
	  output wire LMK_SEL1_3V3,
	   input wire LMK_RESET_3V3,	//SPI readback from LMK
	  output wire LMK_SCK_3V3,
	  output wire LMK_SDIO_3V3,
	  output wire LMK_CS_3V3,
	  output wire LMK_SYNC_3V3,

//--------------------------
	  output wire RNE1_RS422,
	  output wire DE1RX_RS422,
	  input  wire RX1_RS422,
	  output wire TX1_RS422,	  
//--------------------------
	  input  wire CS_FLASH_FPGA, 		// сигнал чипсилекта с МК на флеш через ПЛИС (судя по всему уходит в РЕЗЕРВ! можно использовать для чего-то ещё!)
	  
	  output wire DE_MISO_LVDS_3V3,     //ноги управления буфером SPI
	  output wire SYNC_DA2,  			//sync для источника питания 
	 
	  input  wire CE_MO,
	  input  wire CLK_MO,
	  input  wire D_MOSI,
	  output wire D_MISO,
	  
	  output wire SEL_ETALON_3V3,
//------------SFP----------------	  
	  inout  wire SDATA1_I2C,
	  output wire SCLK1_I2C,
	  input  wire SFP1_TX_FAULT,
	  input  wire SFP1_PRESENT,
	  input  wire SFP1_LOS,
	  output wire RATE1_SELECTION,
	  output wire SFP1_TX_DISABLE,

	  inout  wire SDATA2_I2C,
	  output wire SCLK2_I2C,
	  input  wire SFP2_TX_FAULT,
	  input  wire SFP2_PRESENT,
	  input  wire SFP2_LOS,
	  output wire RATE2_SELECTION,
	  output wire SFP2_TX_DISABLE,

//-------------------------------
//sdram
	 output wire A1,
	 output wire A2,
	 output wire A3,
	 output wire A4,
	 output wire A5,
	 output wire A6,
	 output wire A7,
	 output wire A8,
	 output wire A9,
	 output wire A10,
	 output wire A12,
	 output wire A13,
	 
	 output wire SA10,
	 output wire A18,
	 output wire A19,
	 output wire ABE0,
	 output wire SWE,
	 output wire SCAS,
	 output wire SRAS,
	 output wire SMS,
	 output wire ABE1,
	 output wire CLK_OUT,
	 output wire SCKE,
	 
	 inout wire D0,
	 inout wire D1,
	 inout wire D2,
	 inout wire D3,
	 inout wire D4,
	 inout wire D5,
	 inout wire D6,
	 inout wire D7,
	 inout wire D8,
	 inout wire D9,
	 inout wire D10,
	 inout wire D11,
	 inout wire D12,
	 inout wire D13,
	 inout wire D14,
	 inout wire D15,	 
//-------------------------------
	 inout wire TEMP_DQ0,
	 inout wire TEMP_DQ1,
//-------------------------------	  
     output wire FPGA_LED1_3V3,
     output wire T_TEST1,
	 output wire T_TEST2,
//------------------------------
//  LED SFP
	output wire LED1_SFP,
	output wire LED2_SFP,
	output wire LED3_SFP,
	output wire LED4_SFP
     );
	  
	  wire xA1_PWRDOWN_upr_MK;
	  
	  wire xA2_PWRDOWN_upr_MK;	
	  
	  wire RX_GTP_o;
		 
	  wire reset_all;
	  
  	  wire signal_wire0 ;
	  wire signal_wire1 ;
	
	  wire [15:0] rx_parallel_data_out ;
	  wire [1:0]  rx_ctrldetect;
	  
	  wire xCE_MO;
	  wire xCLK_MO;
	  wire xD_MOSI;
	  wire xD_MISO;
	  wire xFPGA_SYNC1;
	  wire clk_hclk;
	  wire fboutclk;
	  wire [69:0] zreconfig_to_xcvr;
	  wire zreconfig_from_xcvr;
      wire clk_100MHZ_glob;
      wire clk_100MHz_eth;
	  wire clk_50MHz;
	  wire clk_50MHz_n;

	  wire clk360_locked;
	  wire reset_240;
	  wire locked_120_1;
	  wire locked_120_2;
	  wire locked_dac1;
	  wire locked_dac2;
	  wire clk_125_90;

	  wire clk_192_adc1;
	  wire clk_192_adc2;
	  wire clk_96_adc1;
	  wire clk_96_adc2;
/*
	  wire clk_240_adc1;
	  wire clk_240_adc2;
	  wire clk_120_adc1;
	  wire clk_120_adc2;
*/
	  wire clk_48_1;
	  wire clk_48_2;
	  wire clk_96_dac1;
	  wire clk_96_dac2;
	  wire locked_eth0;
	  wire locked_eth1;
	  wire REF360;
	  wire clk_125;
	  wire clk_125_n;
	  wire locked_125;
	  wire reset_125;
      wire clk_125_eth0;
      wire clk_125_eth1;

gbuf 
gref1 (
		.inclk  (REF3),  //  altclkctrl_input.inclk
		.outclk (REF360)  // altclkctrl_output.outclk
	);
	
rst rst_360(REF360,reset_all);

pll_0002 
pll_1(
		.refclk   (REF360),   //  refclk.clk
		.rst      (reset_all),      //   reset.reset
		.outclk_0 (clk_125), // outclk0.clk
		.outclk_1 (clk_125_n), // outclk1.clk
		.outclk_2 (clk_125_90), // outclk2.clk
		.locked   (locked_125)    //  locked.export
	);
	
pll96_48_from360_0002
pll_96_dac1 (
		.refclk   (REF360),   		//  refclk.clk
		.rst      (reset_all),      //   reset.reset
		.outclk_0 (clk_96_dac1), 	// outclk0.clk
		.outclk_1 (clk_48_1), 		// outclk1.clk
		.locked   (locked_dac1)    	//  locked.export
	);	

pll96_48_from360_0002
pll_96_dac2 (
		.refclk   (REF360),   		//  refclk.clk
		.rst      (reset_all),      //   reset.reset
		.outclk_0 (clk_96_dac2), 	// outclk0.clk
		.outclk_1 (clk_48_2), 		// outclk1.clk
		.locked   (locked_dac2)    	//  locked.export
	);	

pll_192_96_from360_0002 
pll192_96_ADC1 (
		.refclk   (REF360),   //  refclk.clk
		.rst      (reset_all),      //   reset.reset
		.outclk_0 (clk_192_adc1), // outclk0.clk
		.outclk_1 (clk_96_adc1), // outclk1.clk
		.locked   (locked_120_1)    //  locked.export
	);
	
pll_192_96_from360_0002 
pll192_96_ADC2 (
		.refclk   (REF360),   //  refclk.clk
		.rst      (reset_all),      //   reset.reset
		.outclk_0 (clk_192_adc2), // outclk0.clk
		.outclk_1 (clk_96_adc2), // outclk1.clk
		.locked   (locked_120_2)    //  locked.export
	);	

pll_125_from360_0002 
pll_125_eth0 (
		.refclk   (REF360),   //  refclk.clk
		.rst      (reset_all),      //   reset.reset
		.outclk_0 (clk_125_eth0), // outclk0.clk
		.locked   (locked_eth0)    //  locked.export
	);
	
pll_125_from360_0002 
pll_125_eth1 (
		.refclk   (REF360),   //  refclk.clk
		.rst      (reset_all),      //   reset.reset
		.outclk_0 (clk_125_eth1), // outclk0.clk
		.locked   (locked_eth1)    //  locked.export
	);	

/*
pll100_0002 
pll100_inst (
		.refclk   (clk_100MHZ),   //  refclk.clk
		.rst      (0),      //   reset.reset
		.outclk_0 (clk_100MHZ_glob), // outclk0.clk
		.locked   (clk100_locked)    //  locked.export
	);
*/	

wire reset_100eth;
rst rst8(clk_100MHz_eth,reset_100eth);
test_led tst_led2(clk_125 ,xT_TEST3);
	  
wire   xCS_ADC1;
wire   xCS_ADC2;
wire   xCS_DAC1;
wire   xCS_DAC2;
wire   xCS_LMK;
wire   xCS_FPGA1;
wire   xCS_FPGA2;

wire rzrv1;
wire rzrv2;
assign rzrv1 =  1'h0;
//-------------------------------------------------
assign xCS_ADC1=CS_ADC1;
assign xCS_ADC2=CS_ADC2;
assign xCS_DAC1=CS_DAC1;
assign xCS_DAC2=CS_DAC2;
assign xCS_LMK =CS_LMK;
assign xCS_FPGA1=CS_FPGA1;
assign xCS_FPGA2=CS_FPGA2;
	  
wire xEN_2V5_VDA;
wire xREZERV1;
	  
assign EN_2V5_VDA=xEN_2V5_VDA;
assign REZERV1   =xREZERV1;

wire xUART1_TX; 	 
wire xTX_FTDI_1;	 
	 
wire xBOOT_MK_FTDI;
wire xRESET_MK_FTDI;	  
wire xMR_RESET_MK_FPGA; 
wire xBOOT_MK_FPGA; 
		 
wire xWDATA_MK0;
wire xWDATA_MK2;   //прерывание от UDP2 (ETH2)
wire xWDATA_MK3;   //прерывание от WCM (сообщение что кончились записи в памяти команд реального времени)
wire xWDATA_MK4; 
wire xWDATA_MK5; 
wire xWDATA_MK6;
wire xWDATA_MK7; 

wire xOK_BUS;
wire xWR_BUS;
wire xRD_BUS;
   
assign  WDATA_MK3	        =xWDATA_MK3; //прерывание от WCM (сообщение что кончились записи в памяти команд реального времени)

assign  xUART1_TX 	  		=UART1_TX;
assign  xTX_FTDI_1	  		=TX_FTDI_1;

assign  xBOOT_MK_FTDI 		=BOOT_MK_FTDI;
assign  xRESET_MK_FTDI		=RESET_MK_FTDI;
assign  xMR_RESET_MK_FPGA	=MR_RESET_MK_FPGA;
assign  xBOOT_MK_FPGA		=0;//заменяем джампер на землю,BOOT_MK_FPGA


assign 	xWR_BUS		 =WR_BUS;
assign 	xRD_BUS		 =RD_BUS;

assign xWDATA_MK0    =WDATA_MK0; //<----
assign  WDATA_MK2	 =xWDATA_MK2;//----> прерывание от UDP2

wire xUART1_RX;
wire xRX_FTDI_1;

wire xBOOT1;
wire xBOOT0;
wire xRESET_MK;
wire xCLK_FOR_MK_3V3;
wire xINT1_FPGA;	

assign UART1_RX 	  =	xUART1_RX;
assign RX_FTDI_1 	  = xRX_FTDI_1;

assign BOOT1    	  = xBOOT1;
assign BOOT0    	  = xBOOT0;
assign RESET_MK 	  = xRESET_MK;
assign CLK_FOR_MK_3V3 = xCLK_FOR_MK_3V3;
assign INT1_FPGA      = xINT1_FPGA;
	
wire xSPI4_SCK_MK;
wire xSPI4_NSS_MK;
wire xSPI4_MOSI_MK;
wire xSPI3_SCK;
wire xSPI3_CS;
wire xSPI3_MISO;
wire xSPI2_NSS_MK;	

assign xSPI3_MOSI=SPI3_MOSI;
assign xSPI3_SCK =SPI3_SCK;
assign xSPI3_CS  =SPI3_CS;
assign SPI2_NSS_MK=xSPI2_NSS_MK;
	
 wire xSPI4_MISO_MK;
 wire xSPI3_MOSI;
	
assign xSPI4_MISO_MK=SPI4_MISO_MK;

wire xDAC_SYNC;
wire xD1_RESETB;
wire xD1_SCLK;
wire xD1_SDO;
wire xD1_SDENB;
wire xD1_SLEEP;
wire xD1_SYNC_N_AB;
wire xD1_SYNC_N_CD;
wire xD1_TXENABLE; 
wire xD2_RESETB;
wire xD2_SCLK;
wire xD2_SDO;
wire xD2_SDENB;
wire xD2_SLEEP;
wire xD2_SYNC_N_AB;
wire xD2_SYNC_N_CD;
wire xD2_TXENABLE;

wire xD1_SLEEP_upr_MK;
wire xD2_SLEEP_upr_MK;

wire xD1_RESETB_upr_MK;
wire xD2_RESETB_upr_MK;

//--------------DAC1----------------
assign xD1_RESETB = xD1_RESETB_upr_MK;
assign xD1_SLEEP  = xD1_SLEEP_upr_MK; 
assign xD1_TXENABLE=wEn_Iz;//1
//----------------------------------
//--------------DAC2----------------
assign xD2_RESETB = xD2_RESETB_upr_MK;
assign xD2_SLEEP  = xD2_SLEEP_upr_MK; 
assign xD2_TXENABLE=wEn_Iz;//11 //управление выходом ЦАП
//----------------------------------

assign xD1_SYNC_N_AB=D1_SYNC_N_AB;
assign xD1_SYNC_N_CD=D1_SYNC_N_CD;
assign xD2_SYNC_N_AB=D2_SYNC_N_AB;
assign xD2_SYNC_N_CD=D2_SYNC_N_CD;

assign    DAC_SYNC   =xDAC_SYNC;
assign	  D1_RESETB  =xD1_RESETB;
assign	  D1_SCLK    =xD1_SCLK;
assign	  D1_SDENB   =xD1_SDENB;
assign	  D1_SLEEP   =xD1_SLEEP;
assign	  D1_TXENABLE=xD1_TXENABLE;	  //управление выходом ЦАП
assign	  D2_RESETB  =xD2_RESETB;
assign	  D2_SCLK    =xD2_SCLK;
assign	  D2_SDENB   =xD2_SDENB;
assign	  D2_SLEEP   =xD2_SLEEP;
assign	  D2_TXENABLE=xD2_TXENABLE; //управление выходом ЦАП
	
assign xD1_SDO=D1_SDO;
assign xD2_SDO=D2_SDO;
	
wire xD2_SYNCB;	  
wire xD1_SYNCB;
wire xD1_SDIO;
wire xD2_SDIO;

wire xLMK_SDIO_3V3; 

assign      D1_SDIO=xD1_SDIO;
assign      D2_SDIO=xD2_SDIO;
assign LMK_SDIO_3V3=xLMK_SDIO_3V3;	
	
wire xA1_OVRA_1V8;
wire xA1_OVRB_1V8;
wire xA1_SDOUT_1V8;
wire xA2_OVRA_1V8;
wire xA2_OVRB_1V8;
wire xA2_SDOUT_1V8;

assign   xA1_OVRA_1V8 =A1_OVRA_1V8;
assign   xA1_OVRB_1V8 =A1_OVRB_1V8;
assign   xA1_SDOUT_1V8=A1_SDOUT_1V8;
assign   xA2_OVRA_1V8 =A2_OVRA_1V8;
assign   xA2_OVRB_1V8 =A2_OVRB_1V8;
assign   xA2_SDOUT_1V8=A2_SDOUT_1V8;

wire xA1_CTRL2_1V8;
wire xA1_CTRL1_1V8;
wire xA1_PWRDOWN_1V8; 
wire xA1_RESET_1V8;
wire xA1_SCLK_1V8;
wire xA1_SDATA_1V8;
wire xA1_SEN_1V8;
wire xA1_STBY_1V8;
wire xA2_CTRL2_1V8;
wire xA2_CTRL1_1V8;
wire xA2_PWRDOWN_1V8; 
wire xA2_RESET_1V8;
wire xA2_SCLK_1V8;
wire xA2_SDATA_1V8;
wire xA2_SEN_1V8;
wire xA2_STBY_1V8;
wire xA1_SNC;
wire xA2_SNC;

assign  A1_CTRL2_1V8  =xA1_CTRL2_1V8;
assign  A1_CTRL1_1V8  =xA1_CTRL1_1V8;
assign  A1_PWRDOWN_1V8=xA1_PWRDOWN_1V8; 
assign  A1_RESET_1V8  =xA1_RESET_1V8;
assign  A1_SCLK_1V8   =xA1_SCLK_1V8;
assign  A1_SDATA_1V8  =xA1_SDATA_1V8;
assign  A1_SEN_1V8    =xA1_SEN_1V8;
assign  A1_STBY_1V8   =xA1_STBY_1V8;
assign  A2_CTRL2_1V8  =xA2_CTRL2_1V8;
assign  A2_CTRL1_1V8  =xA2_CTRL1_1V8;
assign  A2_PWRDOWN_1V8=xA2_PWRDOWN_1V8;
assign  A2_RESET_1V8  =xA2_RESET_1V8;
assign  A2_SCLK_1V8   =xA2_SCLK_1V8;
assign  A2_SDATA_1V8  =xA2_SDATA_1V8;
assign  A2_SEN_1V8    =xA2_SEN_1V8;
assign  A2_STBY_1V8   =xA2_STBY_1V8;

assign A1_SNC=xA1_SNC;
assign A2_SNC=xA2_SNC;
	
wire xLMK_SEL0_3V3;
wire xLMK_SEL1_3V3;
wire xLMK_RESET_3V3;
wire xLMK_SCK_3V3;
wire xLMK_CS_3V3;
wire xLMK_SYNC_3V3;

wire xFLASH_CLK_3V3;  
wire xFLASH_MOSI_3V3;
wire xFLASH_CS_3V3;
wire xDE_MISO_LVDS_3V3;  
wire xSYNC_DA2;
wire xSEL_ETALON_3V3;
wire xT_TEST1;
wire xT_TEST2;
wire xT_TEST3;
		
//----------------------------SPI LMK---------------------------------------------------	   
assign xLMK_SCK_3V3 =xSPI3_SCK;
assign xLMK_CS_3V3  =xCS_LMK;
assign xLMK_SDIO_3V3=xSPI3_MOSI;
	
assign LMK_SEL0_3V3    =xLMK_SEL0_3V3;
assign LMK_SEL1_3V3    =xLMK_SEL1_3V3;
assign LMK_SCK_3V3     =xLMK_SCK_3V3;
assign LMK_CS_3V3      =xLMK_CS_3V3;
assign LMK_SYNC_3V3    =xLMK_SYNC_3V3;

assign FLASH_CLK_3V3   =xFLASH_CLK_3V3;  
assign FLASH_MOSI_3V3  =xFLASH_MOSI_3V3;
assign FLASH_CS_3V3    =xFLASH_CS_3V3;
assign SYNC_DA2        =xSYNC_DA2; 
assign SEL_ETALON_3V3  =xSEL_ETALON_3V3;
assign T_TEST1         =xT_TEST1;
assign T_TEST2         =xT_TEST2;

assign xLMK_RESET_3V3=LMK_RESET_3V3;

wire xFLASH_MISO_3V3;
wire xCS_FLASH_FPGA;	

assign xCS_FLASH_FPGA =CS_FLASH_FPGA;
	
//------------------------TEST SPI---------------------------------

wire xSPI3_MISO1;
wire xSPI3_MISO2;
wire xSPI3_MISO3;
wire xSPI3_MISO4;
wire xSPI3_MISO5;
wire xSPI3_MISO6;
wire xSPI3_MISO7;
wire xSPI3_MISO8;
wire xSPI3_MISO9;
wire xSPI3_MISO10;
wire xSPI3_MISO11;
wire xSPI3_MISO12;
wire xSPI3_MISO13;
wire xSPI3_MISO14;
wire xSPI3_MISO15;
wire xSPI3_MISO16;
wire xSPI3_MISO17;
wire xSPI3_MISO18;
wire xSPI3_MISO19;
wire xSPI3_MISO20;
wire xSPI3_MISO21;
wire xSPI3_MISO22;
wire xSPI3_MISO23;
wire xSPI3_MISO24;
wire xSPI3_MISO25;

wire xSPI3_MISO26;
wire xSPI3_MISO27;
wire xSPI3_MISO28;
wire xSPI3_MISO29;
wire xSPI3_MISO30;
wire xSPI3_MISO31;
wire xSPI3_MISO32;
wire xSPI3_MISO33;
wire xSPI3_MISO34;
wire xSPI3_MISO35;
wire xSPI3_MISO36;
wire xSPI3_MISO37;

wire  xSPI3_MISO_TEMP0;
wire  xSPI3_MISO_TEMP1;

wire [31:0] wTESTwr;
wire [ 3:0] wire_out;

//--------------------------REZERV--------------------------------
assign LMK_STATUS_LD1_FPGA_3V3=0;//тестовый вывод 							
assign LMK_STATUS_LD2_FPGA_3V3=0;//тестовый вывод 
assign D1_ALARM_FPGA=0;
assign D2_ALARM_FPGA=0;

//--------------------------SPI BACKPLANE--------------------------
wire [ 2:0] ADDR_SPI_BPL;//адресс кассеты относительно положения на бекплейне
wire [31:0] wTEST_bpl;
wire  xOE_DRV_ALL;
wire  xOE_DRV0;
wire  xOE_DRV1;

wire  xD_MISO0;
wire  xD_MISO1;

assign ADDR_SPI_BPL={ADDR2,ADDR1,ADDR0};

assign xOE_DRV_ALL=xOE_DRV0|
				   xOE_DRV1;

assign  D_MISO =xD_MISO0&
				xD_MISO1;



//assign DE_MISO_LVDS_3V3= (CE_MO==0)?1'b1:1'b0;//управление выходным драйвером lvds SPI (не будет работать с кучей кассет!)
assign DE_MISO_LVDS_3V3=xOE_DRV_ALL;

Block_read_spi_bpl 
 #(32) spi_test_bpl1   (.adr({ADDR_SPI_BPL,4'd1}),.clk(clk_125),.sclk(CLK_MO),.mosi(D_MOSI),.miso(xD_MISO0),.cs(CE_MO) ,.rst(0) ,
	.inport(32'hdeedbeef),.oe_drv(xOE_DRV0));	//чтение test
	
Block_read_spi_bpl 
 #(32) spi_TEST_rd_bpl1(.adr({ADDR_SPI_BPL,4'd3}),.clk(clk_125),.sclk(CLK_MO),.mosi(D_MOSI),.miso(xD_MISO1),.cs(CE_MO) ,.rst(0) ,
	.inport(wTEST_bpl),.oe_drv(xOE_DRV1));	//чтение контрольного регистра - проверка шины SPI	

Block_write_spi_bpl 
 #(32) spi_TEST_wr_bpl1(.adr({ADDR_SPI_BPL,4'd2}),.clk(clk_125),.sclk(CLK_MO),   .mosi(D_MOSI),.miso(),.cs(CE_MO) ,.rst(0) ,
	.out(wTEST_bpl));	  	//запись контрольного регистра - проверка шины SPI
//-----------------------------------------------------------------
wire xSPI3_MISO_ver;

logic [47:0] ver_data;
versiya_fpga072 
inst_versiya_fpga072 (.data(ver_data));

Block_read_spi 
 #(48,74) spi_read_versiya  (.clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO_ver),.cs(xCS_FPGA1) ,.rst(0) ,
	.inport(ver_data));//чтение test

Block_read_spi 
 #(32,30) spi_test       (.clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO1),.cs(xCS_FPGA1) ,.rst(0) ,
	.inport(32'hdeedbeef));//чтение test
	
Block_read_spi 
 #(32,35)     spi_TEST_rd(.clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO2),.cs(xCS_FPGA1) ,.rst(0) ,
	.inport(wTEST_bpl));//чтение контрольного регистра - проверка шины SPI	(wTESTwr) временно переделано под проверку шины SPI  с бекплейна

Block_write_spi 
 #(32,36) spi_TEST_wr( .clk(clk_125),.sclk(xSPI3_SCK),   .mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA1) ,.rst(0) ,
	.out(wTESTwr));	  //запись контрольного регистра - проверка шины SPI

//-----------------------------------------------------------------
wire rst_block000;
rst reset000(clk_125,rst_block000);	
	 Block_write_spi 
 #(8,1) spi_att1( .clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA1) ,.rst(rst_block000) ,
	.out
		 ({  UPR_AT1_05,
			 UPR_AT1_1,
			 UPR_AT1_2,
			 UPR_AT1_4,
			 UPR_AT1_8,
			 UPR_AT1_16}
			) );		
	
wire rst_block001;
rst reset001(clk_125,rst_block001);	
 Block_write_spi 
 #(8,2) spi_att2( .clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA1) ,.rst(rst_block001) ,
	.out
		 ({ UPR_AT2_05,
			UPR_AT2_1,
			UPR_AT2_2,
			UPR_AT2_4,
			UPR_AT2_8,
			UPR_AT2_16}
			) );

wire rst_block002;
rst reset002(clk_125,rst_block002);	
 Block_write_spi 
 #(8,3) spi_att3( .clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA1) ,.rst(rst_block002) ,
	.out
		 ({ UPR_AT3_05,
			UPR_AT3_1,
			UPR_AT3_2,
			UPR_AT3_4,
			UPR_AT3_8,
			UPR_AT3_16}
			) );

wire rst_block003;
rst reset003(clk_125,rst_block003);				
 Block_write_spi 
 #(8,4) spi_att4( .clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA1) ,.rst(rst_block003) ,
	.out
		 ({ UPR_AT4_05,
			UPR_AT4_1,
			UPR_AT4_2,
			UPR_AT4_4,
			UPR_AT4_8,
			UPR_AT4_16}
			) );

wire rst_block004;
rst reset004(clk_125,rst_block004);	
wire dummy_wire;	
	 Block_write_spi 
 #(8,5) spi_upr1( .clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA1) ,.rst(rst_block004) ,
	.out
		 ({ UPR_SWITCH1,   //передатчики
			UPR_SWITCH2,
			UPR_SWITCH3,
			UPR_SWITCH4,
			 UPR_SWITCH1_OFF,//приёмники
			 UPR_SWITCH2_OFF,
			 UPR_SWITCH3_OFF,
			 UPR_SWITCH4_OFF
			}  
			) );

//PWRDN для АЦП и ЦАП с МК			

wire rst_block005;
rst reset005(clk_125,rst_block005);			
	 Block_write_spi 
 #(8,99) spi_PWRDN( .clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA1) ,.rst(rst_block005) ,
	.out
		 ({ xD1_RESETB_upr_MK,
		    xD2_RESETB_upr_MK,
			 xA1_RESET_MK,
			 xA2_RESET_MK,
			 xD1_SLEEP_upr_MK,
			 xD2_SLEEP_upr_MK,
			 xA1_PWRDOWN_upr_MK,
			 xA2_PWRDOWN_upr_MK
			}  
			) );

wire rst_block006;
rst reset006(clk_125,rst_block006);	
 	 Block_write_spi 
 #(8,48) spi_LED( .clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA1) ,.rst(rst_block006) ,
	.out
		 ({
			FPGA_IND2,		  //индикация лицевой панели : "ИСПРАВ"
			FPGA_IND1,		  //индикация лицевой панели : "ЛИНИЯ СВЯЗИ" - GREEN
			xREZERV1		  //индикация лицевой панели : "ЛИНИЯ СВЯЗИ" - RED авария
			}  
			) );
//-----------------------------контроль входных сигналов через SPI --------------------------------------------------
			
	 Block_read_spi 
 #(8,7) spi_control2( .clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO4),.cs(xCS_FPGA1) ,.rst(0) ,
	.inport
		 ({ ADDR0,
			ADDR1,
			ADDR2,
			ADDR3,
			D1_ALARM,
			D2_ALARM,
			LMK_STATUS_LD1_3V3,  // 
			LMK_STATUS_LD2_3V3}  // 
			) ); 
			
//------------------TEMP signal-------------------
assign xLMK_SEL0_3V3  = 1'h00;//поменять управление в процессе отладки!!!
assign xLMK_SEL1_3V3  = 1'h00;
assign xLMK_SYNC_3V3  = xWDATA_MK0;
//------------------------------------------------

//------------------------------------------------
// контроль длительности входных SYNC
logic [31:0] cntr_min0;
logic [31:0] cntr_min1;
logic [31:0] cntr_min2;
logic [31:0] cntr_max0;
logic [31:0] cntr_max1;
logic [31:0] cntr_max2;
logic [31:0] cntr_T1hz;
logic rst_cntr;
logic xFLAG_1Hz;

logic TST_sync0;
logic TST_sync1;
logic TST_sync2;

/*
	test_sync #(
			.INTERV0(100_000),//задаётся в клоках 48 МГц
			.INTERV1(33_000),
			.INTERV2(48_000_000)
		) inst_test_sync (
			.clk   (clk_48_1),
			.sync0 (TST_sync0),
			.sync1 (TST_sync1),
			.sync2 (TST_sync2)
		);
*/

logic FLAG_1Hz_TST;

cntr_module inst_cntr_module
		(
			.clk   (clk_48_1),   //48 МГц
			.clk125(clk_125),
			.sync0 (FPGA_SYNC1),//
			.sync1 (FPGA_SYNC2),//
			.sync2 (FPGA_SYNC3),//	  //секундная метка!        SYS_REF
			.tst   (FLAG_1Hz_TST),    //тестовый вход для экстренного формирования секундной метки
			.rst   (rst_cntr),  	  //активный 1 !
			.FLAG_1Hz(xFLAG_1Hz),	  //флаг сообщает об источнике секундной метки 1 - внешняя , 0 - внутренняя
			.T1hz(w_T1HZ),			  //выход секундной метки, либо внешней либо внутренней
			.duration_T1hz(cntr_T1hz),//value of the duration of the current second mark
			.max0(cntr_max0),
			.max1(cntr_max1),
			.max2(cntr_max2),
			.min0(cntr_min0),
			.min1(cntr_min1),
			.min2(cntr_min2)
		);

Block_read_spi_v2 
 #(224,63) spi_cntr_fpga(.clk(clk_125),  .sclk   (xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO3),.cs(xCS_FPGA1)  ,.rst(0),
				  	  .clr(rst_cntr),.inport ({cntr_T1hz,  cntr_max2   ,cntr_min2   ,cntr_max1   ,cntr_min1   ,cntr_max0   ,cntr_min0}));//чтение min/max длительностей FPGA_SYNC0..2 

Block_read_spi_v2 
 #(8,68) spi_FLAG_1Hz_TST(.clk(clk_125),     .sclk   (xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA1)  ,.rst(0),
				  	      .clr(FLAG_1Hz_TST),.inport ());
//
//--------------TEMP SENSER-----------------------			
wire wtemp_sense_clr;
wire [7:0] temp_data;
wire  xSPI3_MISO_TEMP;
wire  xTEMP_REQ;
wire [23:0] DATA_FPGA_INFO;//следить за разрядностью!!!
wire [15:0] DATA_FPGA_PLL;

temp_sense
t0 
(
.clk48 (clk_48_1),
.clk125(clk_125),
.clr(xTEMP_REQ),   //
.data(temp_data)
);

assign DATA_FPGA_PLL ={7'b0000000,SYS_TIME_UPDATE_OK_1,xFLAG_1Hz,locked_125,locked_dac1,locked_dac2,locked_120_1,locked_120_2,locked_eth0,locked_eth1};
assign DATA_FPGA_INFO={DATA_FPGA_PLL,temp_data};

Block_read_spi_v2 
 #(24,52) spi_temp_fpga(.clk(clk_125),  .sclk   (xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO_TEMP),.cs(xCS_FPGA1)  ,.rst(0),
				  	  .clr(xTEMP_REQ),.inport (DATA_FPGA_INFO));//скачиваем показания Темп датчика и перезапускаем АЦП термометра

//------------------------------------------------------+
//               Подсчитываем время работы блока
//work_clock 
//inst_work_clock (.clk(clk), .sec(sec), .rst(rst), .data(data));
//------------------------------------------------------+
//Температурные датчики материнской платы
wire [71:0] TEMP0_DATA;
wire [71:0] TEMP1_DATA;
wire REQ_TEMP0;
wire REQ_TEMP1;

wire tmp_oe0;
wire tmp_o0;


temp_1wire
temp0(
.clk       (clk_125),	//тактирование
.rst       (REQ_TEMP0),	//запрос измерения температуры
.done      (),			//сигнал готовности результата
.T_data    (TEMP0_DATA),//результат
.tmp_oe	   (tmp_oe0),	//тестовый сигнал - говорит о направлении работы TEMP_DQ
.tmp_out   (tmp_o0),
.tmp_in    (TEMP_DQ0),
.TEMP_DQ   ()			//бинаправленный вывод
);

assign TEMP_DQ0 = tmp_oe0 ? tmp_o0 : 1'hz;

Block_read_spi_v2 
 #(48,57) spi_temp0(.clk(clk_125),  .sclk   (xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO_TEMP0),.cs(xCS_FPGA1)  ,.rst(0),
				  	  .clr(REQ_TEMP0),.inport (TEMP0_DATA[47:0]));//скачиваем показания Темп датчика и перезапускаем АЦП термометра	

wire tmp_oe1;
wire tmp_o1;

					  
temp_1wire
temp1(
.clk       (clk_125),	//тактирование
.rst       (REQ_TEMP1),	//запрос измерения температуры
.done      (),			//сигнал готовности результата
.T_data    (TEMP1_DATA),//результат
.tmp_oe	   (tmp_oe1),	//тестовый сигнал - говорит о направлении работы TEMP_DQ
.tmp_out   (tmp_o1),
.tmp_in    (TEMP_DQ1),
.TEMP_DQ   ()			//бинаправленный вывод
);

assign TEMP_DQ1 = tmp_oe1 ? tmp_o1 : 1'hz;

Block_read_spi_v2 
 #(48,58) spi_temp1(.clk(clk_125),  .sclk   (xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO_TEMP1),.cs(xCS_FPGA1)  ,.rst(0),
				  	  .clr(REQ_TEMP1),.inport (TEMP1_DATA[47:0]));//скачиваем показания Темп датчика и перезапускаем АЦП термометра						  
//-----------------------------------------------					  

//----------------------------------------------------------------			
assign xEN_2V5_VDA=1'h1; //0 - выключен источник питания для DAC и ADC			
//----------------------------------------------------------------
assign TNC_MK_1HZ     = w_T1HZ;  //направляем сигнал 1сек на мк
assign xSEL_ETALON_3V3 = 1'h1; //1 - включаем поступление сигнала эталона с ВЧ разъёма

assign xD1_SDENB=xCS_DAC1;
assign xD2_SDENB=xCS_DAC2;

assign xFLASH_CS_3V3 =xCS_FLASH_FPGA;
assign xFLASH_CLK_3V3=xSPI3_SCK;

assign xD2_SCLK		=xSPI3_SCK;
assign xD1_SCLK		=xSPI3_SCK; 

assign xD1_SDIO      = xSPI3_MOSI;
assign xD2_SDIO      = xSPI3_MOSI;

assign xFLASH_MOSI_3V3 = xSPI3_MOSI;

wire   xSPI3_MISO_AND_CS2;
wire   xSPI3_MISO_AND;
assign xSPI3_MISO_AND = 
                            xSPI3_MISO_ver&  //тут скачиваем новер версии прошивки
							xSPI3_MISO1&
							xSPI3_MISO2&
							xSPI3_MISO3&//min-max SYNC0..SYNC2
							xSPI3_MISO4&
					//		xSPI3_MISO5&
					//		xSPI3_MISO6&
					//		xSPI3_MISO7&
					//		xSPI3_MISO8&
							xSPI3_MISO_TEMP1& //датчик температуры 1
							xSPI3_MISO_TEMP0& //датчик температуры 0
							xSPI3_MISO_TEMP&  //датчик температуры ПЛИС
							xSPI3_MISO9 &     //eth1
							xSPI3_MISO10&
							xSPI3_MISO11&
							xSPI3_MISO12&

							xSPI3_MISO13&//adc2
							xSPI3_MISO14&//adc2
				//			xSPI3_MISO15&//
							xSPI3_MISO16&//adc2
				//			xSPI3_MISO17&//free
				//			xSPI3_MISO18&//free
							xSPI3_MISO19&//adc2
							xSPI3_MISO20&//adc2
							xSPI3_MISO21&//     eth2
				//			xSPI3_MISO22&//free

							xSPI3_MISO23&//DAC2
			  		    	xSPI3_MISO24&//DAC2
				    		xSPI3_MISO25&//DAC2

				//    		xSPI3_MISO26&//
				//     		xSPI3_MISO27&//free
				    		xSPI3_MISO28&//ADC1
				    		xSPI3_MISO29&//ADC1
				    		xSPI3_MISO30&//ADC1
				//    		xSPI3_MISO31&//free
				//    		xSPI3_MISO32&//free
				//    		xSPI3_MISO33&//free
				    		xSPI3_MISO34&//ADC1
				    		xSPI3_MISO35;//
				//    		xSPI3_MISO36;//free
			    //   		xSPI3_MISO37 //
									
//---------------------------------------------									
//assign xSPI3_MISO = xD1_SDO;//TEST!!!

assign SPI3_MISO =	xSPI3_MISO;						
 
assign xSPI3_MISO  = (!xCS_FPGA1)          ?xSPI3_MISO_AND:
					 (!xCS_FPGA2)          ?xSPI3_MISO_AND_CS2:  //данные из SDRAM
					 (!xCS_LMK  )	       ?xLMK_RESET_3V3:      //вывожу spi с LMK через её ногу reset
				  // (!xCS_FLASH_FPGA)   ?xFLASH_MISO_3V3:
			 		 (!xD1_SDENB)		   ?xD1_SDO:
					 (!xD2_SDENB)		   ?xD2_SDO:	
					 (!xA1_SEN_1V8)        ?xA1_SDOUT_1V8:
					 (!xA2_SEN_1V8)        ?xA2_SDOUT_1V8:1'h1;
 
//----------BOOT uart stm32---------- 
wire wRESET_MK;
wire wRESET_WDG;

reset_long
#(1000000) rst_mk (.clk(clk_125),.en(xBOOT_MK_FTDI & xMR_RESET_MK_FPGA & wRESET_WDG), .rst(wRESET_MK));  //модуль ресета для мк
//assign wRESET_MK=0;

assign xUART1_RX  = xTX_FTDI_1;
assign xRX_FTDI_1 = xUART1_TX;


assign xBOOT1    = 1'h0 ; //для stm32
assign xBOOT0    = (~xRESET_MK_FTDI)  | xBOOT_MK_FPGA;
assign xRESET_MK = wRESET_MK;

watchdog //вотчдог - следит чтобы МК не зависал , с учётом его программирования через FTDI (следим за чиспселектом SPI3)
#(250_000_000)inst_watchdog (.clk(clk_125), .event_mk(xCS_FPGA2), .boot0(xBOOT_MK_FTDI), .rst_n(wRESET_WDG));

//--------------rs485-------------------

assign TX1_RS422   =   UART6_TX;
assign UART6_RX    =   RX1_RS422;
assign RNE1_RS422  =   WDATA_MK4;
assign DE1RX_RS422 =   WDATA_MK4;
assign RX_FTDI_2   =   RX1_RS422;//тестовый вывод на консоль

//sdram

wire w_rd_req;
wire w_wr_req;
wire w_read_valid;
wire w_write_valid;
wire w_ready;

wire [12:0]           sdr_addr           ; // SDRAM ADRESS
wire [15:0]           Dq                 ; // SDRAM Read/Write Data Bus
wire 		sdram_buf_we;
wire [15:0] w_data_to_sdram;
wire [15:0] w_data_from_sdram;
wire [24:0] w_adr_sdram;
wire [15:0] sdram_data_o;

wire [1:0] w_sdram_ba;

wire w_scke;
wire w_sms;
wire w_sras;
wire w_scas;
wire w_swe;
wire [1:0] w_abe;


sdram_cntr
#(
     .SDRAM_ROW_WIDTH_BIT(13),
     .SDRAM_COL_WIDTH_BIT(10),
	 .SDRAM_BANK_WIDTH_BIT(2),
	 .SDRAM_DATA_WIDTH_BIT(16),
	 .SDRAM_CAS(3),
	 .tPOWERUP(16'd14285),
	 .tREF(64),
	 .tREF_COUNT(8192),
	 .tCK(8)
	 
)
SDRAM_CNTR0
(
     .i_clk                         (clk_125   ),
	 // interface for manager RAM
	 .i_ram_data					(w_data_to_sdram),//
	 .o_ram_data					(w_data_from_sdram),
	 .i_ram_addr					(w_adr_sdram),
	 .i_ram_len						(8           ),//работаем бурстом по 8 слов!!!
	 .i_ram_read_req				(w_rd_req    ),
	 .o_ram_read_valid				(w_read_valid),
	 .i_ram_write_req				(w_wr_req    ),
	 .o_ram_write_valid				(w_write_valid),
	 .o_ready						(w_ready     ),
	 // interface for SDRAM
	 .i_sdram_data					({D15,D14,D13,D12,D11,D10,D9,D8,D7,D6,D5,D4,D3,D2,D1,D0}),
	 .o_sdram_data					(sdram_data_o),
	 .o_sdram_addr					(sdr_addr    ),

//	 .o_sdram_clk(       o_sdram_clk        ),
//	 .o_sdram_clk(       o_sys_clk          ),

	 .o_sdram_cke					(w_scke),//SCKE
	 .o_sdram_cs_l					(w_sms ),//SMS
	 .o_sdram_ras_l					(w_sras),//SRAS
	 .o_sdram_cas_l					(w_scas),//SCAS
	 .o_sdram_we_l					(w_swe ),//SWE
	 .o_sdram_buf_we				(sdram_buf_we),
	 .o_sdram_ba  					(w_sdram_ba),
	 .o_sdram_dqml					(w_abe[0]),//ABE0
	 .o_sdram_dqmh					(w_abe[1]) //ABE1
);



assign {D15,D14,D13,D12,D11,D10,D9,D8,D7,D6,D5,D4,D3,D2,D1,D0} = sdram_buf_we ? sdram_data_o : 16'hz;
assign {A13,A12,SA10,A10,A9,A8,A7,A6,A5,A4,A3,A2,A1}           = sdr_addr;
assign {A19,A18} = w_sdram_ba;
assign SCKE = w_scke;
assign SMS  = w_sms;
assign SRAS = w_sras;
assign SCAS = w_scas;
assign SWE  = w_swe;
assign ABE0 = w_abe[0];
assign ABE1 = w_abe[1];

assign CLK_OUT=clk_125_n;//clk_125_90 clk_125_n


wire [15:0] data_from_mem_to_contr;
wire [15:0] data_sdram_to_mem;
wire [9:0]  adr_mem_wr;
wire [9:0]  adr_mem_rd;

wire w_wr;
wire w_rd;

wire [31:0] w_ADR_read_MEM;
wire [15:0] w_DATA_from_MEM;

wire w_write_imp;
wire r_write_imp;
wire [24:0] w_adr_wr;


test_wr 
t_wr( 
	.clk   (clk_125) ,
	.start (xWR_BUS) ,
	.wr_req(w_write_imp)
	 );

test_wr 
t_rd( 
	.clk   (clk_125) ,
	.start (xRD_BUS) ,
	.wr_req(r_write_imp)
	 );

sdram_mk_upr 
u1( 
	.ready          (w_ready),//
	.clk            (clk_125),
	.data_from_mem  (data_from_mem_to_contr),//data_from_mem_to_contr
	.adr_mem_read   (adr_mem_rd),
	.adr_mem_write  (adr_mem_wr),
	.data_to_mem    (data_sdram_to_mem),
	.data_to_sdram  (w_data_to_sdram),
	.data_from_sdram(w_data_from_sdram),
	.adr_sdram      (w_adr_sdram),
	.adr_from_mk    (w_ADR_read_MEM[24:0]),
	.adr_from_mk_wr (w_ADR_read_MEM[24:0]),
	.wr_req         (w_wr_req),
	.rd_req         (w_rd_req),
	.wr_valid       (w_write_valid),
	.rd_valid       (w_read_valid),
	.wr_bus         (w_write_imp),//сигнал записи в sdram от МК   xWR_BUS         
	.rd_bus         (r_write_imp) //xRD_BUS сигнал чтение из sdram в МЕМ           
	//--------------------
	 );

assign OK_BUS = w_ready;

mem_for_sdram_v1 //MEM для скачивания данных из sdram в мк
m_rd(   //MEM for read
	.clock(clk_125),
	.data(data_sdram_to_mem),
	.rdaddress(w_ADR_read_MEM[9:0]),//
	.wraddress(adr_mem_wr),//
	.wren(1'b1),
	.q(w_DATA_from_MEM)
	);

wire [31:0] w_ADR_DATA;


mem_for_sdram_v1 //MEM для записи данных в sdram из мк
m_wr(   //MEM for write
	.clock    (clk_125),
	.data     (w_ADR_DATA[15:0]),//
	.rdaddress(adr_mem_rd),//
	.wraddress(w_ADR_DATA[31:16]),
	.wren     (w_wr),//
	.q        (data_from_mem_to_contr)//
	);

wire rst_block007;
rst reset007(clk_125,rst_block007);	
//запись в промежуточную память МЕМ для пересылки оттуда в SDRAM , а ещё это адресс в sdram
Block_write_spi_mac //
 #(32,33) spi_test_sdram_wr(.clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA2) ,.rst(rst_block007) ,
	.out (w_ADR_DATA),.wr(w_wr),.wtreq(0));

//чтение из промежуточной памяти МЕМ данных скачаных из SDRAM
Block_read_spi 
 #(16,32) spi_read_data_MEM (.clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO_AND_CS2),.cs(xCS_FPGA2) ,.rst(0) ,
	.inport(w_DATA_from_MEM));//чтение данных 

wire rst_block008;
rst reset008(clk_125,rst_block008);	
Block_write_spi_mac //запись адреса в МЕМ откуда читаем слово данных, а ещё это адресс в sdram
 #(32,20) spi_adr_read_MEM  (.clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA2) ,.rst(rst_block008) ,
	.out (w_ADR_read_MEM),.wr(w_rd),.wtreq(0));

//----------------ADC-------------------

reg r_xA1_RESET_1V8 =0;
reg r_rst_adc1_align=0;

reg r_xA2_RESET_1V8 =0;
reg r_rst_adc2_align=0;

always @(posedge clk_192_adc1)
begin
r_xA1_RESET_1V8 <=xA1_RESET_MK;
r_rst_adc1_align<=xA1_RESET_MK;

r_xA2_RESET_1V8 <=xA2_RESET_MK;
r_rst_adc2_align<=xA2_RESET_MK;
end

wire xA1_RESET_MK;
wire xA2_RESET_MK;

assign xA2_RESET_1V8 =r_xA2_RESET_1V8;
assign xA1_RESET_1V8 =r_xA1_RESET_1V8;
assign rst_adc2_align=r_rst_adc2_align;
assign rst_adc1_align=r_rst_adc1_align;

assign xA1_SEN_1V8   =xCS_ADC1;
assign xA2_SEN_1V8   =xCS_ADC2;
assign xA2_SCLK_1V8  =xSPI3_SCK;
assign xA1_SCLK_1V8  =xSPI3_SCK;
assign xA1_SDATA_1V8 =xSPI3_MOSI; 
assign xA2_SDATA_1V8 =xSPI3_MOSI;

assign xA1_CTRL2_1V8  =0;//zero - Normal operation
assign xA1_CTRL1_1V8  =0;
assign xA1_PWRDOWN_1V8=xA1_PWRDOWN_upr_MK;
assign xA1_STBY_1V8   =0;

assign xA2_CTRL2_1V8  =0;//zero - Normal operation
assign xA2_CTRL1_1V8  =0;
assign xA2_PWRDOWN_1V8=xA2_PWRDOWN_upr_MK;
assign xA2_STBY_1V8   =0;

//---------------------ADC1-------------------------------------------

wire [ 7:0] xrx_runningdisp_adc1;
wire [ 7:0] xrx_disperr_adc1;
wire [ 7:0] xrx_errdetect_adc1;
wire [ 3:0] xrx_clkout_adc1;
wire [63:0] xrx_parallel_data_adc1;
wire [ 7:0] xrx_datak_adc1;
wire rst_adc1_align;
wire sync_n_adc1;
wire xrx_ready_adc1;
wire [7:0]  xrx_patterndetect_adc1;
wire [7:0]  xrx_syncstatus_adc1;
wire [19:0] rx_bitslipboundaryselectout_adc1;
wire [15:0] ch0_adc0_data;
wire [15:0] ch1_adc0_data;
wire [63:0] adc_data_tst1_1;
wire [63:0] adc_data_tst2_1;
wire [ 7:0] ALIAS_errdetect_adc1;
wire [ 7:0] ALIAS_datak_adc1;
wire        sync_n_adc1_ALIAS;
wire [ 7:0] ALIAS_syncstatus_adc1;
wire [ 7:0] ALIAS_disperr_adc1;


assign xA1_SNC=sync_n_adc1;//SYNC_N ADC1

//    21-05-19
custom_phy_4line 
adc1_phy(
		.phy_mgmt_clk(clk_125),         //       phy_mgmt_clk.clk
		.phy_mgmt_clk_reset(),   // phy_mgmt_clk_reset.reset
		.phy_mgmt_address(),     //           phy_mgmt.address
		.phy_mgmt_read(),        //                   .read
		.phy_mgmt_readdata(),    //                   .readdata
		.phy_mgmt_waitrequest(), //                   .waitrequest
		.phy_mgmt_write(),       //                   .write
		.phy_mgmt_writedata(),   //                   .writedata
		.rx_ready(xrx_ready_adc1),  //           rx_ready.export
		.pll_ref_clk(clk_96_adc1), //        pll_ref_clk.clk
		.rx_serial_data({A1_TX3,A1_TX2,A1_TX1,A1_TX0}),       //     rx_serial_data.export
		.rx_runningdisp(xrx_runningdisp_adc1),       //     rx_runningdisp.export
		.rx_disperr(xrx_disperr_adc1),           //         rx_disperr.export
		.rx_errdetect(xrx_errdetect_adc1),         //       rx_errdetect.export
		.rx_patterndetect(xrx_patterndetect_adc1),
		.rx_syncstatus(xrx_syncstatus_adc1),
        .rx_bitslipboundaryselectout(rx_bitslipboundaryselectout_adc1),		
		.rx_coreclkin({clk_96_adc1,clk_96_adc1,clk_96_adc1,clk_96_adc1}),
		.rx_clkout(xrx_clkout_adc1),            //          rx_clkout.export  клок для внешней user-s логики , мы не используем пока(?).
		.rx_parallel_data(xrx_parallel_data_adc1),     //   rx_parallel_data.export
		.rx_datak(xrx_datak_adc1),             //           rx_datak.export
		.reconfig_from_xcvr(),   // reconfig_from_xcvr.reconfig_from_xcvr
		.reconfig_to_xcvr()      //   reconfig_to_xcvr.reconfig_to_xcvr
	);


wire fifo_adc1_full_sig_1;

wire tst_adc1;
wire [3:0] w_ok0;
wire [3:0] w_ok1;
wire [7:0] walign_tst;
wire adc0_sysref_error; 

//    15-01-19
adc1_jesd_rcv
adc1
(
.clk_120(clk_96_adc1),//входная тактовая частота
.clk_240(clk_192_adc1),//выходная тактовая частота
.rst_adc_align(rst_adc1_align),//подать ресет от микроконтроллера для АЦП0
.SYSREF0(SYSREF0),
.xrx_syncstatus_adc(xrx_syncstatus_adc1),
.xrx_disperr_adc(xrx_disperr_adc1),
.xrx_errdetect_adc(xrx_errdetect_adc1),
.xrx_parallel_data_adc(xrx_parallel_data_adc1),
.xrx_datak_adc(xrx_datak_adc1),
.fifo2clk_tst_rd(tst_adc1),//сигнал чтения для тестового вывода слов на SPI
.walign_tst(walign_tst),//тестовый вывод кода смещения входного слова
.w_ok0(w_ok0),//выход кода смещения эластикбуфера0
.w_ok1(w_ok1),//выход кода смещения эластикбуфера1
.sync_n_adc(sync_n_adc1),//выход сигнала SYNC_N
.sysref_error(adc0_sysref_error),
.ch_adc0_data(ch0_adc0_data),
.ch_adc1_data(ch1_adc0_data),
.adc_data_tst2(adc_data_tst2_1),
.adc_data_tst1(adc_data_tst1_1),
.sync_n_adc_ALIAS(sync_n_adc1_ALIAS),
.ALIAS_datak_adc(ALIAS_datak_adc1),
.ALIAS_errdetect_adc(ALIAS_errdetect_adc1),
.ALIAS_syncstatus_adc(ALIAS_syncstatus_adc1),
.ALIAS_disperr_adc(ALIAS_disperr_adc1)
);

//---------------control error SYNC_N ADC1-------------
wire [15:0] error_adc1;
wire [15:0] error_sysref_adc1;
wire wclr1_adc1;
wire wclr2_adc1;
wire wclr3_adc1;

error_sch  
block_error_adc1 
 (
  	.out(error_adc1),
	.clk(clk_192_adc1),
	.event_int(~sync_n_adc1_ALIAS),
	.rst(wclr1_adc1)
);

error_sch  
block_error_sysref_adc1 
 (
  	.out(error_sysref_adc1),
	.clk(clk_96_adc1),
	.event_int(adc0_sysref_error),
	.rst(wclr2_adc1)
);

Block_read_spi_v2 
 #(16,110) spi_error_adc1(.clk(clk_96_adc1),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),
						 .miso(xSPI3_MISO28)  ,.cs(xCS_FPGA1)  ,.rst(0),
						 .clr(wclr1_adc1),           .inport (error_adc1));

 Block_read_spi_v2 
 #(16,112) spi_error_sysref_adc1(.clk(clk_96_adc1),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),
						      .miso(xSPI3_MISO30)  ,.cs(xCS_FPGA1)  ,.rst(0),
						      .clr(wclr2_adc1),      .inport (error_sysref_adc1));
//-------------------------------------------------
Block_read_spi 
 #(32,100) spi_custom_phy_reg1_adc1( .clk(clk_96_adc1),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO29),
 .cs(xCS_FPGA1) ,.rst(0) ,
	.inport
		 ({
		 xrx_runningdisp_adc1    [7:0],
		     ALIAS_datak_adc1    [7:0],//xrx_datak_adc1[7:0]
		     ALIAS_disperr_adc1  [7:0],//xrx_disperr_adc1[7:0]
		  	 ALIAS_errdetect_adc1[7:0]// xrx_errdetect_adc1[7:0]	   
			}));
		 
Block_read_spi 
 #(24,104) spi_custom_phy_reg6_adc1( .clk(clk_96_adc1),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO34),
 .cs(xCS_FPGA1) ,.rst(0) ,
	.inport
		 ({1'b0,
		 	w_ok0,//
	  	 xrx_ready_adc1,		  
		 sync_n_adc1_ALIAS,//sync_n_adc1
		 locked_120_1,
		 xrx_patterndetect_adc1[7:0],
		 ALIAS_syncstatus_adc1 [7:0]// xrx_syncstatus_adc1[7:0]		 
			}));
			
Block_read_spi 
 #(24,105) spi_custom_phy_reg7_adc1( .clk(clk_96_adc1),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO35),
 .cs(xCS_FPGA1) ,.rst(0) ,
	.inport
		 ({4'b0000,
	  	 rx_bitslipboundaryselectout_adc1[19:0]		 
			}));	
	
//---------------------ADC2-------------------------------------------

wire [ 7:0] xrx_runningdisp_adc2;
wire [ 7:0] xrx_disperr_adc2;
wire [ 7:0] xrx_errdetect_adc2;
wire [ 3:0] xrx_clkout_adc2;
wire [63:0] xrx_parallel_data_adc2;
wire [ 7:0] xrx_datak_adc2;
wire rst_adc2_align;
wire sync_n_adc2;
wire xrx_ready_adc2;
wire [7:0]  xrx_patterndetect_adc2;
wire [7:0]  xrx_syncstatus_adc2;
wire [19:0] rx_bitslipboundaryselectout_adc2;
wire [15:0] ch0_adc2_data;
wire [15:0] ch1_adc2_data;
wire [63:0] adc_data_tst1_2;
wire [63:0] adc_data_tst2_2;

wire [ 7:0] ALIAS_errdetect_adc2;
wire [ 7:0] ALIAS_datak_adc2;
wire        sync_n_adc2_ALIAS;
wire [ 7:0] ALIAS_syncstatus_adc2;
wire [ 7:0] ALIAS_disperr_adc2;

assign xA2_SNC=sync_n_adc2;//SYNC_N ADC2

//    21-05-19
custom_phy_4line 
adc2_phy(
		.phy_mgmt_clk(clk_125),         //       phy_mgmt_clk.clk
		.phy_mgmt_clk_reset(),   // phy_mgmt_clk_reset.reset
		.phy_mgmt_address(),     //           phy_mgmt.address
		.phy_mgmt_read(),        //                   .read
		.phy_mgmt_readdata(),    //                   .readdata
		.phy_mgmt_waitrequest(), //                   .waitrequest
		.phy_mgmt_write(),       //                   .write
		.phy_mgmt_writedata(),   //                   .writedata
		.rx_ready(xrx_ready_adc2),  //           rx_ready.export
		.pll_ref_clk(clk_96_adc2), //        pll_ref_clk.clk
		.rx_serial_data({A2_TX3,A2_TX2,A2_TX1,A2_TX0}),       //     rx_serial_data.export
		.rx_runningdisp(xrx_runningdisp_adc2),       //     rx_runningdisp.export
		.rx_disperr(xrx_disperr_adc2),           //         rx_disperr.export
		.rx_errdetect(xrx_errdetect_adc2),         //       rx_errdetect.export
		.rx_patterndetect(xrx_patterndetect_adc2),
		.rx_syncstatus(xrx_syncstatus_adc2),
        .rx_bitslipboundaryselectout(rx_bitslipboundaryselectout_adc2),		
		.rx_coreclkin({clk_96_adc2,clk_96_adc2,clk_96_adc2,clk_96_adc2}),
		.rx_clkout(xrx_clkout_adc2),            //          rx_clkout.export
		.rx_parallel_data(xrx_parallel_data_adc2),     //   rx_parallel_data.export
		.rx_datak(xrx_datak_adc2),             //           rx_datak.export
		.reconfig_from_xcvr(),   // reconfig_from_xcvr.reconfig_from_xcvr
		.reconfig_to_xcvr()      //   reconfig_to_xcvr.reconfig_to_xcvr
	);

wire fifo_adc2_full_sig_1;

wire tst_adc2;

wire [3:0] w2_ok0;
wire [3:0] w2_ok1;
wire [7:0] walign2_tst;
wire adc2_sysref_error; 

//    15-01-19
adc2_jesd_rcv
adc2
(
.clk_120(clk_96_adc2),//входная тактовая частота
.clk_240(clk_192_adc2),//выходная тактовая частота
.rst_adc_align(rst_adc2_align),//подать ресет от микроконтроллера для АЦП0
.SYSREF0(SYSREF0),
.xrx_syncstatus_adc(xrx_syncstatus_adc2),
.xrx_disperr_adc(xrx_disperr_adc2),
.xrx_errdetect_adc(xrx_errdetect_adc2),
.xrx_parallel_data_adc(xrx_parallel_data_adc2),
.xrx_datak_adc(xrx_datak_adc2),
.fifo2clk_tst_rd(tst_adc2),//сигнал чтения для тестового вывода слов на SPI
.walign_tst(walign2_tst),//тестовый вывод кода смещения входного слова
.w_ok0(w2_ok0),//выход кода смещения эластикбуфера0
.w_ok1(w2_ok1),//выход кода смещения эластикбуфера1
.sync_n_adc(sync_n_adc2),//выход сигнала SYNC_N
.sysref_error(adc2_sysref_error),
.ch_adc0_data(ch0_adc2_data),
.ch_adc1_data(ch1_adc2_data),
.adc_data_tst2(adc_data_tst2_2),
.adc_data_tst1(adc_data_tst1_2),
.sync_n_adc_ALIAS(sync_n_adc2_ALIAS),
.ALIAS_datak_adc(ALIAS_datak_adc2),
.ALIAS_errdetect_adc(ALIAS_errdetect_adc2),
.ALIAS_syncstatus_adc(ALIAS_syncstatus_adc2),
.ALIAS_disperr_adc(ALIAS_disperr_adc2)
);

//---------------control error SYNC_N ADC2-------------
wire [15:0] error_adc2;
wire [15:0] error_sysref_adc2;
wire wclr1_adc2;
wire wclr2_adc2;
wire wclr3_adc2;

error_sch  
block_error_adc2 
 (
  	.out(error_adc2),
	.clk(clk_192_adc2),
	.event_int(~sync_n_adc2_ALIAS),
	.rst(wclr1_adc2)
);

error_sch  
block_error_sysref_adc2 
 (
  	.out(error_sysref_adc2),
	.clk(clk_96_adc2),
	.event_int(adc2_sysref_error),
	.rst(wclr2_adc2)
);

Block_read_spi_v2 
 #(16,60) spi_error_adc2(.clk(clk_96_adc2),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),
						 .miso(xSPI3_MISO13)  ,.cs(xCS_FPGA1)  ,.rst(0),
						 .clr(wclr1_adc2),           .inport (error_adc2));
 Block_read_spi_v2 
 #(16,61) spi_error_sysref_adc2(.clk(clk_96_adc2),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),
						      .miso(xSPI3_MISO14)  ,.cs(xCS_FPGA1)  ,.rst(0),
						      .clr(wclr2_adc2),      .inport (error_sysref_adc2));
//-------------------------------------------------
Block_read_spi 
 #(32,50) spi_custom_phy_reg1_adc2( .clk(clk_96_adc2),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO16),
 .cs(xCS_FPGA1) ,.rst(0) ,
	.inport
		 ({
		 xrx_runningdisp_adc2[7:0],
		       ALIAS_datak_adc2[7:0],//
		     ALIAS_disperr_adc2[7:0],//
		   ALIAS_errdetect_adc2[7:0]//		   
			}));
		 
Block_read_spi 
 #(24,54) spi_custom_phy_reg6_adc2( .clk(clk_96_adc2),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO19),
 .cs(xCS_FPGA1) ,.rst(0) ,
	.inport
		 ({1'b0,
		 	w2_ok0,
	  	 xrx_ready_adc2,		  
		 sync_n_adc2_ALIAS,//
		 locked_120_2,
		 xrx_patterndetect_adc2[7:0],
		 ALIAS_syncstatus_adc2[7:0]// 		 
			}));
			
Block_read_spi 
 #(24,55) spi_custom_phy_reg7_adc2( .clk(clk_96_adc2),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO20),
 .cs(xCS_FPGA1) ,.rst(0) ,
	.inport
		 ({4'b0000,
	  	 rx_bitslipboundaryselectout_adc2[19:0]		 
			}));			

//------------------Синхронизатор1--------------------

wire 		 w_REQ_COMM 	;
wire [63:0] wTIME 			;
wire [63:0] wTIME_DSP1		;
wire [63:0] wTIME_DSP0		;
wire [47:0] FREQ     		;
wire [47:0] FREQ_STEP 		;
wire [31:0] FREQ_RATE 		;
wire [63:0] TIME_START		;
wire [63:0] TIME_INIT		;
wire 		 sSYS_TIME_UPDATE;
wire 		 w_T1HZ 		;//секундная метка или её замена
wire 		 w_spi_WR 		;//сигнал записи данных из вне в реестр реального времени
wire 		 mem_WR			;//сигнал записи данных из реестра реального времени в синхронизатор
wire [15:0] N_impuls 		;

wire [31:0] Interval_Ti 	;
wire [31:0] Interval_Tp 	;
wire [31:0] Tblank1 		;
wire [31:0] Tblank2 		;

//----------------------------
// вайры для между реестра и синхронизатора
wire [47:0] mFREQ     		;
wire [47:0] mFREQ_STEP 	;
wire [31:0] mFREQ_RATE 	;
wire [63:0] mTIME_START	;
wire [15:0] mN_impuls 		;
wire [ 1:0] mTYPE_impulse 	;
wire [31:0] mInterval_Ti 	;
wire [31:0] mInterval_Tp 	;
wire [31:0] mTblank1 		;
wire [31:0] mTblank2 		;
//----------------------------

wire [47:0] 	wFREQ 		;
wire [47:0] 	wFREQ_STEP 	;
wire [31:0] 	wFREQ_RATE	;
wire 			DDS_START 	;

wire 		SYS_TIME_UPDATE_OK;
wire 		SYS_TIME_UPDATE_OK_1;
wire 		wEn_Iz;
wire 		wEn_Pr;

wire wREQ;
wire wACK;

//----------DDS тактируется 96 МГц !!!---------------------------

wire [15:0] data_I;
wire [15:0] data_Q;
wire 		 dds_valid;

dds_chirp 
dds1(
	.clk_96 		(clk_96_dac1),    	// Clock
	.clk_48 		(clk_48_1 	),
	.REQ			(wREQ		),  	//запрос на передачу данных из 125 МГц в 96 МГц
    .ACK			(wACK		),		//подтверждение что данные переданы
	.DDS_freq 		(wFREQ 		),
	.DDS_delta_freq (wFREQ_STEP ),
	.DDS_delta_rate (wFREQ_RATE ),
	.start 			(DDS_START 	),
	.data_I 		(data_I 	),
	.data_Q 		(data_Q 	),
	.valid 			(dds_valid 	)	
);

//-------------Синхронизатор тактируется 48 МГц !!!-------------
wire rst_sync1;

wire [63:0] test_mstrt;
wire logic_En_ADC;


master_start 
sync1(
.DDS_freq 			(wFREQ 				),
.DDS_delta_freq 	(wFREQ_STEP 		),
.DDS_delta_rate 	(wFREQ_RATE 		),
.DDS_start 			(DDS_START 			),
.REQ				(wREQ				),	//запрос на передачу данных
.ACK				(wACK				),  //подтверждение принятых данных из DDS
.REQ_COMMAND 		(w_REQ_COMM 		),  //запрос новой команды из реестра реального времени
.RESET 				(rst_sync1			),
.CLK 				(clk_48_1 			),
.SYS_TIME 			(tmp_TIME			),	//код времени для предустановки по сигналу T1c
.SYS_TIME_UPDATE 	(sSYS_TIME_UPDATE 	),	//сигнал управления который включает готовность установки системного времени по сигналу T1hz 
.T1hz 				(w_T1HZ				),	//сигнал секундной метки
.WR_DATA 			(mem_WR 			),  //сигнал записи данных в синхронизатор
.MEM_DDS_freq 		(mFREQ 				),  //данные команды из реестра реального времени
.MEM_DDS_delta_freq (mFREQ_STEP  		),  //данные команды из реестра реального времени
.MEM_DDS_delta_rate (mFREQ_RATE			),  //данные команды из реестра реального времени
.MEM_TIME_START 	(mTIME_START 		),  //данные команды из реестра реального времени
.MEM_N_impuls 		(mN_impuls 			),  //данные команды из реестра реального времени
.MEM_TYPE_impulse 	(mTYPE_impulse   	),  //тип формируемой пачки  :0 - повторяющаяся (некогерентный),1 - когерентная (DDS не перепрограммируется)
.MEM_Interval_Ti 	(mInterval_Ti 		),  //данные команды из реестра реального времени
.MEM_Interval_Tp 	(mInterval_Tp 		),  //данные команды из реестра реального времени
.MEM_Tblank1		(mTblank1 			),  //данные команды из реестра реального времени
.MEM_Tblank2 		(mTblank2 			),  //данные команды из реестра реального времени
.SYS_TIME_UPDATE_OK      (SYS_TIME_UPDATE_OK ),	//флаг показывающий,что по секундной метке произошла установка системного времени
.SYS_TIME_UPDATE_OK_CONTR(SYS_TIME_UPDATE_OK_1),
.TIME 				(wTIME 				),  //выводим во вне текущее время
.TEST 				(test_mstrt         ),	//тестовый вывод внутренней информации
.En_ADC 			(logic_En_ADC 		),	//сигнал определяющий (интервал обработки АЦП)
.En_Iz 				(wEn_Iz 			),  //сформированый интервал Излучения
.En_Pr 				(wEn_Pr 			)   //сформированый интервал Приёма
);


Block_write_spi 
 #(8,69) spi_MSTRT_wr( .clk(clk_125),.sclk(xSPI3_SCK),   .mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA1) ,.rst(0) ,
	.out({7'b0000000,rst_sync1}));	


always @(posedge clk_48_1) //дублируем текущее время для блоков DSP , допускаем задержку на такт
 begin
 wTIME_DSP1<=wTIME;
 wTIME_DSP0<=wTIME;
 end


wire [31:0] TEST_wcw;
wire RST_WCW;

wcm 
wcm1(						  		  //блок записи и чтения команд реального времени в память и из.
.CLK 		    (clk_48_1),
.rst_n 	        (~RST_WCW),
.REQ_COMM 	    (w_REQ_COMM   		),//запрос новой команды для исполнения синхронизатором (тут вход)
.TIME 		    (wTIME 		 		),//текущее системное время 
.SYS_TIME_UPDATE(SYS_TIME_UPDATE_OK	),//сигнал сообщающий о перестановке системного времени!!!
.FREQ           (tmp_FREQ 		 	),//данные с интерфейса МК
.FREQ_STEP      (tmp_FREQ_STEP 	 	),//----------------------
.FREQ_RATE      (tmp_FREQ_RATE 	 	),//--------//------------ 
.TIME_START     (tmp_TIME_START  	),
.N_impulse 	    (tmp_N_impulse 		),
.TYPE_impulse   (tmp_TYPE_impulse	),
.Interval_Ti    (tmp_Interval_Ti 	),
.Interval_Tp    (tmp_Interval_Tp 	),
.Tblank1 	    (tmp_Tblank1 	 	),
.Tblank2        (tmp_Tblank2 	 	),
.SPI_WR		    (w_spi_WR 		 	),  //сигнал записи для данных из вне в реестр реального времени
.DATA_WR 	    (mem_WR		 		),  //сигнал записи для передачи данных в блок синхронизации
.FREQ_z         (mFREQ 		 		),  //части команды выводимые из модуля в блок синхронизации и исполнения
.FREQ_STEP_z    (mFREQ_STEP 	 	),
.FREQ_RATE_z    (mFREQ_RATE 	 	),
.TIME_START_z   (mTIME_START	 	),
.N_impuls_z     (mN_impuls 	 		),
.TYPE_impulse_z (mTYPE_impulse		), 	//2 бита шириной!!!
.Interval_Ti_z  (mInterval_Ti 		),
.Interval_Tp_z  (mInterval_Tp 		),
.Tblank1_z      (mTblank1 	 		),
.Tblank2_z      (mTblank2 	 		), 	//-----//-------	 
.FLAG_CMD_SEARCH_FAULT(xWDATA_MK3	),	//если в "1" то в памяти не найдено новой команды на исполнение, по этому сигналу подгружаются новые данные в память !!!
.SCH_BUSY_REG_MEM_port(             ),	//тут выводим количество занятых строк памяти - чтобы отслеживать утечку
.TEST 			(TEST_wcw 			)
);

//----------------SPI управление-----------------
wire [ 63:0]	 tmp_TIME 		  ;
wire [ 47:0] 	 tmp_FREQ 		  ;
wire [ 47:0] 	 tmp_FREQ_STEP 	  ;
wire [ 31:0] 	 tmp_FREQ_RATE	  ;
wire [ 63:0]    tmp_TIME_START   ;
wire [ 15:0]    tmp_N_impulse    ;
wire [  7:0]    tmp_TYPE_impulse ;
wire [ 31:0]    tmp_Interval_Ti  ;
wire [ 31:0]    tmp_Interval_Tp  ;
wire [ 31:0]    tmp_Tblank1	  ;
wire [ 31:0]    tmp_Tblank2	  ;

wire rst_spi1;	
rst reset_spi1_1(clk_48_1,rst_spi1);

assign xSPI4_SCK_MK =SPI4_SCK_MK ;
assign xSPI4_NSS_MK =SPI4_NSS_MK ;
assign xSPI4_MOSI_MK=SPI4_MOSI_MK;

DMA_SPI 
spi1
		(
			.clk             (clk_48_1),
			.clk_en          (1),
			.rst_n           (~rst_spi1),
			.MOSI            (xSPI4_MOSI_MK),
			.CS              (xSPI4_NSS_MK ),
			.SCLK            (xSPI4_SCK_MK ),
			.TIME            (tmp_TIME),
			.SYS_TIME_UPDATE (sSYS_TIME_UPDATE),
			.FREQ            (tmp_FREQ),
			.FREQ_STEP       (tmp_FREQ_STEP),
			.FREQ_RATE       (tmp_FREQ_RATE),
			.TIME_START      (tmp_TIME_START),
			.N_impulse       (tmp_N_impulse),
			.TYPE_impulse    (tmp_TYPE_impulse),
			.Interval_Ti     (tmp_Interval_Ti),//интервал измеряется в 1/48 МГц - еденицах
			.Interval_Tp     (tmp_Interval_Tp),
			.Tblank1         (tmp_Tblank1),
			.Tblank2         (tmp_Tblank2),

			.SPI_WR          (w_spi_WR),
			.RESET_WCW       (RST_WCW)  //очищает память команд каждую пересинхронизацию к текущему времени!!!!
		);
	
//---------------------JESD204b_dac1--------------------------		 

wire 			 dac1_phy_mgmt_clk;
wire 			 dac1_phy_mgmt_clk_reset;
wire [8:0]   dac1_phy_mgmt_address;
wire 			 dac1_phy_mgmt_read;
wire [31:0]  dac1_phy_mgmt_readdata;
wire 			 dac1_phy_mgmt_waitrequest;
wire 			 dac1_phy_mgmt_write;
wire [31:0]  dac1_phy_mgmt_writedata;

wire         dac1_tx_ready;
wire [ 1:0]  dac1_tx_serial_data;
wire         dac1_pll_locked;
wire [ 1:0]  dac1_tx_clkout;

wire [ 3:0]  dac1_tx_datak;
wire [ 3:0]  dac1_tx_dispval;
wire [ 3:0]  dac1_tx_forcedisp;
wire [183:0] dac1_reconfig_from_xcvr;
wire [279:0] dac1_reconfig_to_xcvr;

wire [31:0] dac1_tx_parallel_data_alias;
wire [ 3:0] dac1_datak_align;

wire wclr1;
wire wclr2;
wire [15:0] err_data1;
wire [15:0] err_data2;
wire JESD_DAC1_sync_n;

assign  JESD_DAC1_sync_n=D1_SYNCB;

error_sch  
error1 
 (
  	.out(err_data1),
	.clk(clk_125),
	.event_int(D1_ALARM),
	.rst(wclr1)
);

error_sch  
error2 
 (
  	.out(err_data2),
	.clk(clk_125),
	.event_int(~JESD_DAC1_sync_n),
	.rst(wclr2)
);

Block_read_spi_v2 
 #(16,70) spi_error_dac1(.clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),
						 .miso(xSPI3_MISO11)  ,.cs(xCS_FPGA1)  ,.rst(0),
						 .clr(wclr1),           .inport (err_data1));
Block_read_spi_v2 
 #(16,71) spi_error_dac2(.clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),
						 .miso(xSPI3_MISO12)  ,.cs(xCS_FPGA1)  ,.rst(0),
						 .clr(wclr2),           .inport (err_data2));

wire [31:0] data_dac;
wire [15:0] tst_data;

//-------BLOCK DELAY------------------------
wire rst_delay00;
rst reset_delay00(clk_96_dac1,rst_delay00);

wire delay_upr_wr;
wire [15:0] delay_upr;
wire [ 7:0] delay_upr_0;
wire [ 7:0] delay_upr_1;

logic [15:0] dat_I_0;
logic [15:0] dat_Q_0;

logic [15:0] dat_I_1;
logic [15:0] dat_Q_1;

assign {delay_upr_1,delay_upr_0} = delay_upr;

delay_96 #
(.size(20))
inst_delay_96_0 (
			.clk     (clk_96_dac1),
			.rst     (rst_delay00),
			.Idat    ({data_I,data_Q}),
			.wr_comm (delay_upr_wr),
			.upr     (delay_upr_0),
			.Odat    ({dat_I_0,dat_Q_0})
		);

delay_96 #
(.size(20))
inst_delay_96_1 (
			.clk     (clk_96_dac1),
			.rst     (rst_delay00),
			.Idat    ({data_I,data_Q}),
			.wr_comm (delay_upr_wr),
			.upr     (delay_upr_1),
			.Odat    ({dat_I_1,dat_Q_1})
		);

Block_write_spi_mac //
 #(16,65) spi_wr_data_delay(.clk(clk_96_dac1),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(),.cs(xCS_FPGA1) ,.rst(rst_delay00),
	.out (delay_upr),.wr(delay_upr_wr),.wtreq(0));
//-----------------------------------------------------
//----------------DAC1---------------------------------		 

//assign data_DAC0_i=data_I;
//assign data_DAC0_q=data_Q;

reg [15:0] temp_data_q=16'h0080;
reg [15:0] temp_data_i=16'h0080;

always @(posedge clk_96_dac1)
if (dds_valid)
begin
temp_data_q<={dat_I_0[7:0],dat_I_0[15:8]};
temp_data_i<={dat_Q_0[7:0],dat_Q_0[15:8]};
end else
begin
temp_data_q<=16'h0000;
temp_data_i<=16'h0000;
end

wire [15:0] scrambled_data1_i;
wire [15:0] scrambled_data1_q;


jesd204_scrambler #(
  .WIDTH(16),
  .DESCRAMBLE(0)
) i_scrambler1 (
  .clk(clk_96_dac1),
  .reset(~dac1_tx_ready),
  .enable(1),
  .data_in(temp_data_i),
  .data_out(scrambled_data1_i)
);

jesd204_scrambler #(
  .WIDTH(16),
  .DESCRAMBLE(0)
) q_scrambler1 (
  .clk(clk_96_dac1),
  .reset(~dac1_tx_ready),
  .enable(1),
  .data_in(temp_data_q),
  .data_out(scrambled_data1_q)
);

//    15-01-19		 
sync_align_ila #(32,4)
sa_ila(
	.dout   (dac1_tx_parallel_data_alias),
	.txdatak(dac1_datak_align),
	.clk    (clk_96_dac1),
	.sstatus(JESD_DAC1_sync_n),
	.sysref (SYSREF0),
	.data   ({scrambled_data1_q,scrambled_data1_i})	//{temp_data_q,temp_data_i}
);	


custom_phy_dac
dac1
(
		.phy_mgmt_clk                (clk_125),         //       phy_mgmt_clk.clk
		.phy_mgmt_clk_reset          (dac1_phy_mgmt_clk_reset),   // phy_mgmt_clk_reset.reset
		.phy_mgmt_address            (dac1_phy_mgmt_address),     //           phy_mgmt.address
		.phy_mgmt_read               (dac1_phy_mgmt_read),        //                   .read
		.phy_mgmt_readdata           (dac1_phy_mgmt_readdata),    //                   .readdata
		.phy_mgmt_waitrequest        (dac1_phy_mgmt_waitrequest), //                   .waitrequest
		.phy_mgmt_write              (dac1_phy_mgmt_write),       //                   .write
		.phy_mgmt_writedata          (dac1_phy_mgmt_writedata),   //                   .writedata
		.tx_ready                    (dac1_tx_ready),             //           tx_ready.export
		.pll_ref_clk                 (clk_96_dac1),          //        pll_ref_clk.clk clk_96Mhz
		.tx_serial_data              ({D1_RX1,D1_RX0}),       //     tx_serial_data.export
		.pll_locked                  (dac1_pll_locked),           //         pll_locked.export
		.tx_clkout                   (dac1_tx_clkout),            //          tx_clkout.export
		.tx_parallel_data            (dac1_tx_parallel_data_alias),     //   tx_parallel_data.export
		.tx_datak                    (dac1_datak_align),             //           tx_datak.export
		.reconfig_from_xcvr          (),   // reconfig_from_xcvr.reconfig_from_xcvr
		.reconfig_to_xcvr            ()     //   reconfig_to_xcvr.reconfig_to_xcvr
	);


	
Block_read_spi 
 #(8,40) spi_custom_phy_dac1( .clk(clk_96_dac1),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO10),
 .cs(xCS_FPGA1) ,.rst(0) ,
	.inport
		 ({
		   locked_dac1,
		   dac1_pll_locked,
		   dac1_tx_ready,
		   JESD_DAC1_sync_n,
				1'b0,   //dac1_datak_align[3]
				1'b0,   //dac1_datak_align[2] 
				1'b0,   //dac1_datak_align[1]
				1'b0}   //dac1_datak_align[0] 
			) );
			
//---------------------JESD204b_dac2--------------------------	
	 

wire 			 dac2_phy_mgmt_clk;
wire 			 dac2_phy_mgmt_clk_reset;
wire [8:0]   dac2_phy_mgmt_address;
wire 			 dac2_phy_mgmt_read;
wire [31:0]  dac2_phy_mgmt_readdata;
wire 			 dac2_phy_mgmt_waitrequest;
wire 			 dac2_phy_mgmt_write;
wire [31:0]  dac2_phy_mgmt_writedata;

wire         dac2_tx_ready;
wire [ 1:0]  dac2_tx_serial_data;
wire         dac2_pll_locked;
wire [ 1:0]  dac2_tx_clkout;

wire [ 3:0]  dac2_tx_datak;
wire [ 3:0]  dac2_tx_dispval;
wire [ 3:0]  dac2_tx_forcedisp;
wire [183:0] dac2_reconfig_from_xcvr;
wire [279:0] dac2_reconfig_to_xcvr;

wire [31:0] dac2_tx_parallel_data_alias;
wire [ 3:0] dac2_datak_align;

wire JESD_DAC2_sync_n;

assign  JESD_DAC2_sync_n=D2_SYNCB;// 

wire wclr1_D2;
wire wclr2_D2;
wire [15:0] err_data1_D2;
wire [15:0] err_data2_D2;

error_sch  
error1_D2 
 (
  	.out(err_data1_D2),
	.clk(clk_125),
	.event_int(xD2_ALARM),
	.rst(wclr1_D2)
);

error_sch  
error2_D2 
 (
  	.out(err_data2_D2),
	.clk(clk_125),
	.event_int(~JESD_DAC2_sync_n),
	.rst(wclr2_D2)
);


Block_read_spi_v2 
 #(16,72) spi_error_dac1_D2(.clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),
						 .miso(xSPI3_MISO23)  ,.cs(xCS_FPGA1)  ,.rst(0),
						 .clr(wclr1_D2),           .inport (err_data1_D2));
Block_read_spi_v2 
 #(16,73) spi_error_dac2_D2(.clk(clk_125),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),
						 .miso(xSPI3_MISO24)  ,.cs(xCS_FPGA1)  ,.rst(0),
						 .clr(wclr2_D2),           .inport (err_data2_D2));

wire [31:0] data_dac2;
wire [15:0] tst_data_D2;

//--------------------------------------

//assign data_DAC1_i=data_I;
//assign data_DAC1_q=data_Q;

reg [15:0] D2_temp_data_q=16'h0080;
reg [15:0] D2_temp_data_i=16'h0080;

always @(posedge clk_96_dac2)
if (dds_valid)
begin
D2_temp_data_q<={dat_I_1[7:0],dat_I_1[15:8]};
D2_temp_data_i<={dat_Q_1[7:0],dat_Q_1[15:8]};
end else
begin
D2_temp_data_q<=16'h0000;
D2_temp_data_i<=16'h0000;
end

wire [15:0] scrambled_data2_i;
wire [15:0] scrambled_data2_q;


jesd204_scrambler #(
  .WIDTH(16),
  .DESCRAMBLE(0)
) i_scrambler2 (
  .clk(clk_96_dac2),
  .reset(~dac2_tx_ready),
  .enable(1),
  .data_in(D2_temp_data_i),
  .data_out(scrambled_data2_i)
);

jesd204_scrambler #(
  .WIDTH(16),
  .DESCRAMBLE(0)
) q_scrambler2 (
  .clk(clk_96_dac2),
  .reset(~dac2_tx_ready),
  .enable(1),
  .data_in(D2_temp_data_q),
  .data_out(scrambled_data2_q)
);

//    15-01-19		 
sync_align_ila #(32,4)
sa_ila_D2(
	.dout   (dac2_tx_parallel_data_alias),
	.txdatak(dac2_datak_align),
	.clk    (clk_96_dac2),
	.sstatus(JESD_DAC2_sync_n),
	.sysref (SYSREF0),
	.data   ({scrambled_data2_q,scrambled_data2_i})//{D2_temp_data_q,D2_temp_data_i}	
);	



custom_phy_dac
dac2
(
		.phy_mgmt_clk                (clk_125),         //       phy_mgmt_clk.clk
		.phy_mgmt_clk_reset          (dac2_phy_mgmt_clk_reset),   // phy_mgmt_clk_reset.reset
		.phy_mgmt_address            (dac2_phy_mgmt_address),     //           phy_mgmt.address
		.phy_mgmt_read               (dac2_phy_mgmt_read),        //                   .read
		.phy_mgmt_readdata           (dac2_phy_mgmt_readdata),    //                   .readdata
		.phy_mgmt_waitrequest        (dac2_phy_mgmt_waitrequest), //                   .waitrequest
		.phy_mgmt_write              (dac2_phy_mgmt_write),       //                   .write
		.phy_mgmt_writedata          (dac2_phy_mgmt_writedata),   //                   .writedata
		.tx_ready                    (dac2_tx_ready),             //           tx_ready.export
		.pll_ref_clk                 (clk_96_dac2),          //        pll_ref_clk.clk
		.tx_serial_data              ({D2_RX1,D2_RX0}),       //     tx_serial_data.export
		.pll_locked                  (dac2_pll_locked),           //         pll_locked.export
		.tx_clkout                   (dac2_tx_clkout),            //          tx_clkout.export
		.tx_parallel_data            (dac2_tx_parallel_data_alias),     //   tx_parallel_data.export
		.tx_datak                    (dac2_datak_align),             //           tx_datak.export
		.reconfig_from_xcvr          (),   // reconfig_from_xcvr.reconfig_from_xcvr
		.reconfig_to_xcvr            ()     //   reconfig_to_xcvr.reconfig_to_xcvr
	);

	
	
Block_read_spi 
 #(8,42) spi_custom_phy_dac2_D2( .clk(clk_96_dac2),.sclk(xSPI3_SCK),.mosi(xSPI3_MOSI),.miso(xSPI3_MISO25),.cs(xCS_FPGA1) ,.rst(rst_spi_custom_phy_dac2_D2) ,
	.inport
		 ({
		   locked_dac2,
		   dac2_pll_locked,
		   dac2_tx_ready,
		   JESD_DAC2_sync_n,
		   1'b0,					//dac2_datak_align[3]
		   1'b0,  					//dac2_datak_align[2] 
		   1'b0,					//dac2_datak_align[1]
		   1'b0} 					//dac2_datak_align[0] 
			) );

//-------------------ETH1----------------------------

wire [31:0] data_dsp2_adc0_0;
wire [31:0] data_dsp2_adc0_1; 
wire [31:0] real_TIME0_0;	
wire [31:0] real_TIME0_1;	
wire 		 dsp2_0_0_valid;
wire 		 dsp2_0_1_valid;


wire rst_block_eth1;
rst reset_ETH1(clk_125,rst_block_eth1);
logic CLR_TIME_ETH0;

wire [15:0] Numb_inter0;
wire [15:0] Numb_inter1;


eth_1g_top //MAC0
#(
	18,//чтение 32 бит служебных данных из памяти UDP ресивера 
	17,//чтение 32 бит данных из памяти UDP ресивера 
	16,//запись адреса памяти для чтения,управление чтением пакета из фифо МАС
	15,//управление потоком в MAC 1 - вкл , 0 - выкл
	13,//записываем адресс чтения/записи (пишется первым)
	12,//запись в МАС данных
	14,//чтение 32 бит данных
	9, //чтение 32 бит данных TEST
	10,//32 записываем команду в блок i2c в формате: {<w/r>[6:0][7:0]}
	11,//24 записываем команду в блок i2c в формате: {<w/r>[6:0][7:0]}
	34,//адресс чтения памяти UDP
	37,//данные чтения из памяти UDP
	38,//адресс МЕМ3
	39,//данные МЕМ3
	44,//crc    MEM3
	21,//адрес по которому записываем наш IP и наш порт {32'ip,16'port}
	8, //адрес по которому записываем IP получателя потока с DSP {32'ip} (у главного МАК-а этот адрес совпадает с адресом управления)
	45,//адрес по которому записываем наш номер ПОРТа и ПОРТ абанента (dest)	{16'PORT_my,16'PORT_dest}
	46,//адрес по которому записываем MAC адрес в корку ETH
	56,//адрес по которому записываем MAC адрес получателя (куда будем слать) .
	67 //тут скачиваем MAC адрес нашего DEST-а (кто нами управляет)
	)
eth1(
	.Numb_inter(Numb_inter1),        //порядковый номер интервала полсе секундной метки
	.TIME_CLR(CLR_TIME_ETH0),       //сброс внутренних часов когда интервала приёма нет
	.data_1ch(data_dsp2_adc0_0),// data_test
	.data_2ch(data_dsp2_adc0_1),// data_test
	.wr_data_1ch(dsp2_0_0_valid),// wr_en
	.wr_data_2ch(dsp2_0_1_valid),// wr_en
	.wrclk(clk_192_adc2),		 //входной клок!!!!
	.clk_125  (clk_125),
	.clk_125_eth(clk_125_eth0),//ref для трансивера
	.reset_all(rst_block_eth1),
	.xCS_FPGA1(xCS_FPGA1),
	.xSPI3_SCK(xSPI3_SCK),
	.xSPI3_MOSI(xSPI3_MOSI),
	.xSPI3_MISO(xSPI3_MISO9),
	.RX_GTP(RX_GTP),
	.TX_GTP(TX_GTP),
	.SDATA1_I2C(SDATA1_I2C),
	.SCLK1_I2C(SCLK1_I2C),
	.SFP1_TX_FAULT(SFP1_TX_FAULT),
	.SFP1_PRESENT(SFP1_PRESENT),
	.SFP1_LOS(SFP1_LOS),
	.RATE1_SELECTION(RATE1_SELECTION),
	.SFP1_TX_DISABLE(SFP1_TX_DISABLE),
	.INT_MK(xINT1_FPGA),//прерывание на мк!!!
	.LED0(LED3_SFP),
	.LED1(LED4_SFP),
	.TST()
	);

//--------------------------------------

wire [31:0] data_dsp2_adc1_0;
wire [31:0] data_dsp2_adc1_1; 
wire [31:0] real_TIME1_0;	
wire [31:0] real_TIME1_1;	
wire 	    dsp2_1_0_valid;
wire 	    dsp2_1_1_valid;	

wire rst_block_eth2;
rst reset_ETH2(clk_125,rst_block_eth2);
logic CLR_TIME_ETH1;

eth_1g_top //MAC1 нижний разъём!
#(
	126,//чтение 32 бит служебных данных из памяти UDP ресивера 
	125,//чтение 32 бит данных из памяти UDP ресивера 
	124,//запись адреса памяти для чтения,управление чтением пакета из фифо МАС
	123,//управление потоком в MAC 1 - вкл , 0 - выкл
	122,//записываем адресс чтения/записи (пишется первым)
	121,//запись в МАС данных
	120,//чтение 32 бит данных
	119, //чтение 32 бит данных TEST
	118,//32 записываем команду в блок i2c в формате: {<w/r>[6:0][7:0]}
	117,//24 записываем команду в блок i2c в формате: {<w/r>[6:0][7:0]}
	116,//адресс чтения памяти UDP
	115,//данные чтения из памяти UDP
	108,//адресс МЕМ3
	102,//данные МЕМ3
	101,//crc    MEM3
	22, //адрес по которому записываем наш IP и наш порт 						{32'ip_my  ,32'ip_dest}
	 6, //адрес по которому записываем IP получателя потока с DSP  {32'ip}(у главного МАК-а этот адрес совпадает с адресом управления)
	23,	//адрес по которому записываем наш номер ПОРТа и ПОРТ абанента (dest)	{16'PORT_my,16'PORT_dest}
	47, //адрес по которому записываем MAC адрес в корку ETH
	53, //адрес по которому записываем MAC адрес получателя (куда будем слать) 
	66  //тут скачиваем MAC адрес нашего DEST-а (кто нами управляет)
	)
eth2(
	.Numb_inter(Numb_inter0),        //порядковый номер интервала полсе секундной метки
	.TIME_CLR(CLR_TIME_ETH1),       //сброс внутренних часов когда интервала приёма нет
	.data_1ch(data_dsp2_adc1_0),	// data_test
	.data_2ch(data_dsp2_adc1_1),	// data_test
	.wr_data_1ch(dsp2_1_0_valid),	// wr_en
	.wr_data_2ch(dsp2_1_0_valid),	// wr_en
	.wrclk(clk_192_adc1),			//входной клок!!!!
	.clk_125    (clk_125 ),//clk_125
	.clk_125_eth(clk_125_eth1),//ref для трансивера
	.reset_all(rst_block_eth2),
	.xCS_FPGA1(xCS_FPGA1),
	.xSPI3_SCK(xSPI3_SCK),
	.xSPI3_MOSI(xSPI3_MOSI),
	.xSPI3_MISO(xSPI3_MISO21),
	.RX_GTP(RX2_GTP),
	.TX_GTP(TX2_GTP),
	.SDATA1_I2C(SDATA2_I2C),
	.SCLK1_I2C(SCLK2_I2C),
	.SFP1_TX_FAULT(SFP2_TX_FAULT),
	.SFP1_PRESENT(SFP2_PRESENT),
	.SFP1_LOS(SFP2_LOS),
	.RATE1_SELECTION(RATE2_SELECTION),
	.SFP1_TX_DISABLE(SFP2_TX_DISABLE),
	.INT_MK(xWDATA_MK2),
	.LED0(LED1_SFP),
	.LED1(LED2_SFP),
	.TST(tst_mac)
	);

//-----DSP ADC2-----------

wire [31:0] data_dsp_adc0_0;
wire [31:0] data_dsp_adc0_1;
wire dsp0_0_valid;
wire dsp0_1_valid;

reg [15:0] reg_ch1_adc2_data=0;//вход0
reg [15:0] reg_ch0_adc2_data=0;//вход1

/*
wire rst_tst00;
rst reset_tst00(clk_192_adc2,rst_tst00);
wire [31:0] test0_dataIQ;
wire tst_valid;
test_UDP #(32'd1395864371) inst_test_UDP0 (.clk(clk_192_adc2), .clk_en(1), .rst_n(~rst_tst00), .out_dat(test0_dataIQ), .valid(tst_valid));//32'd1395864371 тестовый сигнал 78 МГц в домене 240 МГц
*/              


always @(posedge clk_192_adc2) reg_ch1_adc2_data<=ch1_adc2_data;// test0_dataIQ[31:16]
wire rst_00;
rst rst_dsp00(clk_192_adc2,rst_00);
dsp_step1 
dsp0_0(
.clk(clk_192_adc2),//
.rst(rst_00),
.in(reg_ch1_adc2_data),//q_data
.data(data_dsp_adc0_0),
.valid(dsp0_0_valid)
);


wire  rst_ds00;
rst reset_ds00(clk_192_adc2,rst_ds00);
dsp_step2 
ds0_0(
.T1sec      (w_T1HZ),           //Секундная метка
.clk 		(clk_192_adc2 	 ), //
.clk_en 	(logic_En_ADC),     // Clock Enable logic_En_ADC
.rst_n 		(~rst_ds00 		 ), // Asynchronous reset active low
.DATA 		(data_dsp_adc0_0 ),	// входные IQ данные test1_dataIQ
.data_en 	(dsp0_0_valid  	 ),	// valid входных данных tst2_valid
.real_TIME 	(wTIME_DSP0[31:0]), //вход тиков реального времени (младшие 32 бита)
.dat_IQ 	(data_dsp2_adc0_0), //выход совмещённых IQ данных
.dat_TIME 	(real_TIME0_0    ), //выход реального времени
.valid 		(dsp2_0_0_valid  ),	
.Numb_inter (Numb_inter1     ),
.CLR_TIME   (CLR_TIME_ETH0)	
);


wire rst_01;
rst rst_dsp01(clk_192_adc2,rst_01);
always @(posedge clk_192_adc2) reg_ch0_adc2_data<=ch0_adc2_data;// test0_dataIQ[15:0]
dsp_step1 
dsp0_1(
.clk(clk_192_adc2),//
.rst(rst_01),
.in(reg_ch0_adc2_data),//i_data
.data(data_dsp_adc0_1),
.valid(dsp0_1_valid)
);


wire  rst_ds01;
rst reset_ds01(clk_192_adc2,rst_ds01);
dsp_step2 
ds0_1(
.T1sec      (w_T1HZ),           //Секундная метка
.clk 		(clk_192_adc2 	 ), //
.clk_en 	(logic_En_ADC),     // Clock Enable logic_En_ADC
.rst_n 		(~rst_ds01 		 ), // Asynchronous reset active low
.DATA 		(data_dsp_adc0_1 ),	// входные IQ данные test1_dataIQ
.data_en 	(dsp0_1_valid    ),	// valid входных данных tst2_valid
.real_TIME 	(wTIME_DSP0[31:0]), //вход тиков реального времени (младшие 32 бита)
.dat_IQ 	(data_dsp2_adc0_1), //выход совмещённых IQ данных
.dat_TIME 	(real_TIME0_1    ), //выход реального времени
.valid 		(dsp2_0_1_valid  ),	
.Numb_inter (                ),
.CLR_TIME   (     )	
);

//-----DSP ADC1-----------

wire [31:0] data_dsp_adc1_0;
wire [31:0] data_dsp_adc1_1;
wire dsp1_0_valid;
wire dsp1_1_valid;

reg [15:0] reg_ch0_adc0_data=0;
reg [15:0] reg_ch1_adc0_data=0;

/*
wire rst_tst10;
rst reset_tst10(clk_192_adc1,rst_tst10);
wire tst2_valid;
wire [31:0] test1_dataIQ;
test_UDP #(32'd1413760068) inst_test_UDP1 (.clk(clk_192_adc1), .clk_en(1), .rst_n(~rst_tst10), .out_dat(test1_dataIQ), .valid(tst2_valid));//32'd1413760068 тестовый сигнал 78 МГц в домене 240 МГц
*/

always @(posedge clk_192_adc1) reg_ch0_adc0_data<=ch0_adc0_data;// test1_dataIQ[31:16]
wire rst_10;
rst rst_dsp10(clk_192_adc1,rst_10);
dsp_step1 
dsp1_0(
.clk(clk_192_adc1),		//
.rst(rst_10),
.in(reg_ch0_adc0_data),	//
.data(data_dsp_adc1_0),
.valid(dsp1_0_valid)
);

wire  rst_ds10;
rst reset_ds10(clk_192_adc1,rst_ds10);
dsp_step2 
ds1_0(
.T1sec      (w_T1HZ),           //Секундная метка
.clk 		(clk_192_adc1 	 ), //
.clk_en 	(logic_En_ADC),     // Clock Enable logic_En_ADC
.rst_n 		(~rst_ds10 		 ), // Asynchronous reset active low
.DATA 		(data_dsp_adc1_0 ),	// входные IQ данные test1_dataIQ
.data_en 	(dsp1_0_valid  	 ),	// valid входных данных tst2_valid
.real_TIME 	(wTIME_DSP1[31:0]), //вход тиков реального времени (младшие 32 бита)
.dat_IQ 	(data_dsp2_adc1_0), //выход совмещённых IQ данных
.dat_TIME 	(real_TIME1_0    ), //выход реального времени
.valid 		(dsp2_1_0_valid  ),	
.Numb_inter (Numb_inter0     ),
.CLR_TIME   (CLR_TIME_ETH1)	
);


wire rst_11;
rst rst_dsp11(clk_192_adc1,rst_11);
always @(posedge clk_192_adc1) reg_ch1_adc0_data<=ch1_adc0_data;// test1_dataIQ[15:0]
dsp_step1 
dsp1_1(
.clk(clk_192_adc1),//
.rst(rst_11),
.in(reg_ch1_adc0_data),//i_data
.data(data_dsp_adc1_1),
.valid(dsp1_1_valid)
);


wire  rst_ds11;
rst reset_ds11(clk_192_adc1,rst_ds11);
dsp_step2 
ds1_1(
.T1sec      (w_T1HZ),           //Секундная метка
.clk 		(clk_192_adc1 	 ), //
.clk_en 	(logic_En_ADC),     // Clock Enable logic_En_ADC
.rst_n 		(~rst_ds11 		 ), // Asynchronous reset active low
.DATA 		(data_dsp_adc1_1 ),	// входные IQ данные test1_dataIQ
.data_en 	(dsp1_1_valid    ),	// valid входных данных tst2_valid
.real_TIME 	(wTIME_DSP1[31:0]), //вход тиков реального времени (младшие 32 бита)
.dat_IQ 	(data_dsp2_adc1_1), //выход совмещённых IQ данных
.dat_TIME 	(real_TIME1_1    ), //выход реального времени
.valid 		(dsp2_1_1_valid  ),	
.Numb_inter (                ),
.CLR_TIME   (     )	
);

//------------тестовые выводы--------------------
logic tst_mac;
assign FPGA_LED1_3V3=wEn_Iz;//тестовый вывод на HL10  wEn_Iz
//-----------------------------------------------

endmodule

