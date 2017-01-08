{  All type declarations follow }
type
  message = packed array [1..5] of char;
  act_name = packed array[1..13] of char;
const
  current 	= 3B;		{ channel 3 A/D W/DMA }
  batt_volts 	= 2;		{ channel 1 A/D W/DMA }
  egr_vent 	= 2B;		{ HS02 }
  egr_press 	= 3B;		{ HS03 }
  therm_air_dmp = 4B;		{ HS05 }
  can_prg_sig 	= 6B;		{ HS06 }
  fuel_pmp 	= 7B;		{ HS07 }
  time_int 	= 17B;		{ INT #2 }
  off = 0;			{ HS0 off command }
  on = 20B;			{ HS0 on command }
{ Actuator errors }
  short		= 1;
  noise		= 2;
  normal	= 3;
  stuck		= 4;
  open		= 5;

{ New const }
  no_samples	= 4;
  sample_time	= 500;		{ 500 microsecond sample period }
  end_sample	= 127;		{ size of amps sample - compiler limit }
  one_millisec	= 417;
  twen_mil_delay = 8334;	{ 20 millisec delay }
  forty_millisec = 16667;
  fifty_usec_dly = 21;		{ ?????? 200 us. A/D switching time ????? }
  two_hund_udly = 209;		{ reduce when sample loop is optimized }
  LF = chr(12B);		{ ASCII line feed }
  CR = chr(15B);		{ ASCII carriage return }
  
  msg1 ='SHORT';
  msg2 ='NOISE';
  msg3 ='NORML';
  msg4 ='STUCK';
  msg5 ='OPEN ';
  msg6 ='EGR VENT     ';
  msg7 ='EGR PRESSURE ';
  msg8 ='THERM DUMP   ';
  msg9 ='THERM BYPASS ';
  msg10='CANISTER PRGE';
  msg11='FUEL PUMP    ';
  msg12='TEST ERROR   ';
  msg13='SYSTEM O.K.  ';

{EECIV interrupt 	Mask bit map	Vector address}
  spare 		= 2B;		 {300}
  hsi1			= 20B;		 {302}
  ad_done		= 200B;		 {304}
  hs02			= 10B;		 {306}
  clk_ovf		= 100B;		 {310}
  hs01			= 4B;		 {312}
  hsi0			= 40B;		 {314}
  hsi_in_avail		= 1B;		 {316}

var
  actuator	:0..fuel_pmp;
  sample_start,
  short_cntr,
  any_offset,
  amps_prev,
  time_dif,
  test_group	:integer;
  amps		:array [1..end_sample] of integer; {debug - should be 200 limit}
  errors	:array [0..fuel_pmp] of 0..open; {should be short..open}
  prev_error	:array [0..fuel_pmp] of 0..open;  { should be short..open }
  max_current,
  volts,
  epsilon,
  old_data,
  neg_slope,		{ counts when slope < 0 for normal detect }
  prev_theta,		{ previous value of theta }
  prev_sigh,		{ previous value of sigh }
  noise_cnt,		{ counter for noise in amps sample space }
  theta,		{ used to calculate slope and proj_point }
  sigh,			{ used to calculate slope and proj_point }
  slope,		{ slope of small portion of amps trace }
  last_i,		{ posn of amp array that is to be removed from
			  the rolling point predictor }
  calc_l,		{ calculated inductance }
  exp_l,		{ expected unloaded inductance }
  proj_point,		{ projected point }
  open_cntr,		{ counter for open actuator conditions }

{ Fixed point problem fixers }
  point5_x_exp_l,
  point2_x_pr_p	: integer;
  point05_prev_i: integer;
  point2_x_amppr: integer;

  j		:1..5;	{ message pointer }
  m		:1..13; { message pointer }
  exp_i		:array [1..end_sample] of integer;
  i		:1..end_sample;		{ debug - should be 200 limit }
  error		:short..open;
  short_mes	:message;
  noise_mes	:message;
  norm_mes	:message;
  stk_mes	:message;
  open_mes	:message;
  
  egr		:act_name;
  egr_p		:act_name;
  thrm_dmp	:act_name;
  thrm_byp	:act_name;
  can_prg	:act_name;
  fyool_pump	:act_name;
  tst_err	:act_name;
  sys_cool	:act_name;
  sys_ok	:boolean;
  a		:integer;	{ debug var }
  sample_stop:	1..end_sample;
  k		:integer;
