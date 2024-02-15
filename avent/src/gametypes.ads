
--
-- Copyright (C) 2024  <fastrgv@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You may read the full text of the GNU General Public License
-- at <http://www.gnu.org/licenses/>.
--


with ada.numerics.generic_elementary_functions;
with matutils;  use matutils;




with gl, gl.binding, gl.pointers;
with glu, glu.binding, glu.pointers;
with glext, glext.binding, glext.pointers;

-------------------------------------------------------------
with System;
with Interfaces.C;
use  type interfaces.c.unsigned;
use  type interfaces.c.c_float;
with Interfaces.C.Pointers;
with interfaces.c.strings;

use interfaces.c;
use interfaces.c.strings;

---------------------------------------------------------

with glfw3; use glfw3;
with zoomwheel;
---------------------------

with ada.unchecked_conversion;
with ada.text_io; use ada.text_io;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;

----------------------------------------------------------------


with shader;  use shader;

with particleobj;
with ribbonobj;

with cylobj;
with droomobj;
with bugobj;
with rectobj;
with rectxobj;
with rectxfineobj;
with cloudobj;
--with rectfineobj;
with cyl2texobj;
with rectsurfobj;
with avatarolay;
with pictobj;
with twictobj;
with xtreeobj;
--with wxtreeobj;
with w3treeobj;
with ztubeobj;


with cubemapobj;
with itemobj;
with ellipsoid;

with text_io;  use text_io;

with Tables;
with gnat.os_lib;







package gametypes is

	exestr: string := Ada.command_line.command_name;
	lastchar : constant character := exestr( exestr'last );
	olinux: constant boolean := (lastchar='u'); -- _gnu
	osx:   constant boolean := (lastchar='x'); -- _osx

	mswin: constant boolean := (gnat.os_lib.directory_separator='\');
	--linux: constant boolean := not mswin;
	linux: constant boolean := not mswin and not osx;

	--exestr: string := Ada.command_line.command_name;
	--lastchar : constant character := exestr( exestr'last );
	--linux: constant boolean := (lastchar='u'); -- _gnu
	--osx:   constant boolean := (lastchar='x'); -- _osx
	--mswin: constant boolean := (lastchar='e'); -- .exe

	hidpi: boolean;

	type mazeRec is record
		x,z,dir: float;
	end record;
	package maze_table is new Tables(mazeRec);

	mtab: maze_table.Table;


	use gl;
	use matutils;
	use type interfaces.c.c_float;




	type vec3 is array(1..3) of float;

	package fmath is new
			Ada.Numerics.generic_elementary_functions( float );



	onepi : constant float     := 3.14159_26535_89793;
	halfpi : constant float    := onepi/2.0;
	thirdpi : constant float   := onepi/3.0;
	fourthpi : constant float  := onepi/4.0;
	eighthpi : constant float  := onepi/8.0;
	twopi : constant float     := onepi*2.0;
	deg2rad : constant float   := onepi/180.0;
	rad2deg : constant float   := 180.0/onepi;



	package ellobj is new ellipsoid(20,20);
	jupit : ellobj.eloid;



	--package myparticles is new particleobj(40,80); --#hori, #vert
	package myparticles is new particleobj(20,40); --#hori, #vert
	wfallp: myparticles.partix;

	package myribbon is new ribbonobj(40); --# vert.parts
	wfallr: myribbon.ribbon;




---------- begin game controller ----------------------------------------

	cfgfile : string := "settings.txt";

	gamepad, joystik : boolean; --flags presence of game controllers

	btn0,btn1,btn2,btn3,btn4,btn5: aliased unsigned_char;

	axis_rx2,axis_ry3,axis_lx0,axis_ly1: aliased float;

	akount, bkount: aliased int;

	--axes : aliased array(0..5) of float; --see glfw3.ads::5226
	--buttons : aliased array(0..14) of unsigned_char;
	axes : access joyAxsArrayType;
	buttons : access joyBtnArrayType;


---------- end game controller ----------------------------------------





	normbuff,
	vertbuff, uvbuff, elembuff, rgbbuff, vertexarrayid : gluint;



----- time variables ---------------------------------------------------

	lazytime, --time @ lazyCamera toggle
	savetime, --time @ gameSave
	oldTimeKb --time @ KB key
		: gldouble := 0.0;


	currentTime, slewtime,
	lasttime,
	foldtime, --time @ moveFor
	boldtime, --time @ moveBak
	jumptime 
		: gldouble := -20.0;
----- time variables ---------------------------------------------------

	Fwid, Fhit,
	winwidth, winheight : aliased glint := 500;







	bmvp, bmm,
	satmm, satmvp, satmv,
	pm,mv,imv,  mmvp, imvp, mm :  mat44 := identity;



	nearpedestal: constant float := 0.8; --0.5;
	nearchalice: constant float := 1.0;

	rsword : constant float := 0.2;
	nearsword : constant float := 0.7; --0.5; --0.8;

	htobj: constant float := 0.02;
	rkey : constant float := 0.1;
	nearkey : constant float := 0.7; --0.5; --0.8;

	--signals nearby KO number if >0
	insertable: integer := 0;

	-- note that maze walls are composed of cubes
	-- of exactly 1 unit per side.  We don't want 
	-- to allow pickability of gateway when we are
	-- too close to center of cube.
	nearpgate : constant float :=1.2;
	toonearpgate : constant float := 0.6;

	-- closer than this to wall allows inserting gateway:
	mazeThreshold: constant float := 1.0;
	-- perhaps need a symbol to indicate insertability?

	neargate : constant float := 1.0;



	schalice : integer := 4;


	--associate gateway with KO#, scene#, row,col:
	kgate: integer := 0; -- => inactive, on ground
	sgate, rgate, cgate: integer := 0; --invalid values


	sgkey,
	sbkey,
	ssword,
	swkey: integer := 1; -- scene # that contains object

	xgate, ygate, zgate, -- purple gateway
	xgkey, ygkey, zgkey, -- green key to maze
	xbkey, ybkey, zbkey, -- black key to temple
	xsword,ysword,zsword,
	xwkey, ywkey, zwkey, -- white key to castle
	xme,yme,zme, 
	oxme,oyme,ozme: float := 0.0; -- virtual position


	fovrad : constant float := fourthpi;
	fovdeg : constant float := fourthpi*rad2deg;


	margin     : constant float := 0.2; --0.20; 18sep16
	aheight    : constant float := 0.5; --AvatarHeight

	oldstateExists,
	usersave,
	userexit : boolean := false;

	repassage: constant float := 2.0; --seconds between door crossings

	-- this must greater than margin
	here: constant float := 0.25;
	step: constant float := 0.28; --just beyond "here" threshold
	--threshold distance from maze transitions

	eps : constant float := 0.02; --dist maze gate from wall

	-- scene parms

	-- exterior dimensions
	xmax : constant float := 20.0;
	ymax : constant float := 20.0;
	zmax : constant float := 20.0;

	-- exterior User domain of (xme,zme), somewhat more limited
	-- so we don't encounter the edge of our flat world.
	xmaxu: constant float := 18.0;
	zmaxu: constant float := 18.0;

	ixmax : constant float :=10.0;
	iymax : constant float := 3.0; --1.5; -- 18nov19 (was 3.0;)
	izmax : constant float :=10.0;

	hmax: constant float := 0.0;
	wlev: constant float := 0.0;
	r1 : constant float := 10.0;
	r2 : constant float := 20.0;
	r3 : constant float := 30.0;



	skybox: cubemapobj.cubemap;



	hcup: constant float := 0.14;
	wcup: constant float := 0.07;

	xped,zped, yped : float;
	xchalice,ychalice,zchalice : float; --glfloat; 28oct19



	--fixed bug parms:
	beetrr: constant float := 0.1; -- x/z radius of 1 bug
	mxbo: constant integer := 60;
	mzbo: constant integer := 25;
	beetleobj : bugobj.bugsurf; -- 1 object for many bugs
	bugspeed : constant float := 0.5; --slow enough for eye to focus bugs





	--jumping : boolean := false;
	--pyjump : float := -ymax+aheight;
	--vyjump : float := 0.0;



	roq,roz : pictobj.pictangle; --skylite (frame) 24nov19
	rox: rectxobj.rectx; --maze-ceiling uses fancy frag.shader

	mz: integer := 0; -- # maze walls defined
	
	mxbloks : constant integer := 25;  -- to build castle, maze
	mwall, wallblok : array(1..mxbloks) of rectobj.rectangle; --wall

	--avatar,
	bugfloor1,bugfloor2,
	zpwall,zmwall,xpwall,xmwall,
	lfloor,mfloor,tfloor,floor, rceil,
	wallLf, wallBt, wallRt, wallTp, -- 26dec22 add
	wallxp, wallxm, wallzp, wallzm : rectobj.rectangle;

	chalice: xtreeobj.treeangle;
	tree: w3treeobj.treeangle;

	ixtmpl,iytmpl,iztmpl,
	 xtmpl, ytmpl, ztmpl : float;

	hwdoor: constant float := 0.8; --halfWidth
	fhdoor: constant float := 1.8; --fullHeight

	ymaze: float;
	xmaze: constant float := 7.0; --5.0;
	zmaze: constant float := 5.0; --4.4; --5.0-0.61;

	iymaze: constant float := -iymax+fhdoor/2.0;
	ixmaze: constant float :=  0.0;
	izmaze: constant float := -izmax;

	dmaze: constant float := 0.7; -- nearness defining "at-maze"
	ddoor: constant float := 0.4; -- nearness "atdoor"
	dtmpl: constant float := 0.9; -- nearness "attemple"



	xlab,ylab,zlab,
	xlion, ylion, zlion,
	ixdoor, iydoor, izdoor,
	xdoor,ydoor,zdoor: float;

	-- scene #5
	doorc, doord, doore, doorf, doorg,
	doory, doorz, doorv, doorw, doorx, ex5,

	-- scene #6
	doora1,doora2, doorb1, doorb2,
	doorv6,doorw6,doorx6,doory6,doorz6,
	doorc6,doord6,doore6,doorf6,doorg6,

	-- scene #7
	al7,ar7,bl7,br7,cl7,cr7,dl7,dr7,el7,er7,fl7,fr7, g7, h7,
	lg7,lh7, ex7, 
	
	-- scene #9
	lg9,

	-- scene #8
	g8,h8, eg8,eh8,

	-- scene #9
	al9,ar9, bl9,br9, cl9,cr9, fl9, fr9,
	dt9,db9, et9,eb9,

	imaze9door, imazedoor: pictobj.pictangle;

	beam12,
	dungdoor, sh7, --sg7,
	beam34,
	rsafe, rdoor,
	wallmoor,
	idung,
	omazedoor,
	bdragon,
	rdragon,

	minotaur,

	sword,
	sword0, --ghostSword
	pedestal,
	wallpicso, mat1,mat2,mat3,mat4,mat5,
	rug, ceil,
	porch, key1, key2, key3,
	key0, --ghostKey
	gatem1,gatem2, gateway,
	gatexp, gatexm, gatezp, gatezm, zoro : pictobj.pictangle;

	korla: twictobj.twictangle;

	zpmural,
	ihallway, ohallway : pictobj.pictangle;



	xaxis : constant vec3 := (1.0,0.0,0.0);
	yaxis : constant vec3 := (0.0,1.0,0.0);
	zaxis : constant vec3 := (0.0,0.0,1.0);
	axis : vec3 := ( others => 0.0 );




	rotx,roty,rotz : float := 0.0;

	maxnko : constant integer := 599;
	nko, level, oldlevel : integer := 0;


	korow, kocol,
	koscene: array(1..maxnko) of integer := (others=>100); 
	--100 denotes undefined

	mazewall: array(1..maxnko) of boolean := (others=>false);

	koxlo, koxhi, koylo,koyhi, kozlo,kozhi : array(1..maxnko) of float;
	onledge : array(1..maxnko) of boolean;



------------------------ begin stuff from main -----------------------------







	error : interfaces.c.int;








	use gl;
	use matutils;
	use type interfaces.c.c_float;



	iskylight,
	pillar11, pillar12, pillar13, pillar14,
	pillar5, pillar6, pillar7,
	pillar8, pillar9, pillar10,
	pillar3, pillar4,
	pillar1, pillar2 : cylobj.cylinder;


	tfile: text_io.file_type;


	intro: boolean := true;

	toxicplaying,
	wfplaying,
	hissbeat,
	drawchalice, chalicegone: boolean := false;


---------- begin texture pointers --------------------------------
	cistern_texid,
	white_texid, wfall_texid,
	rosequartz_texid, copper_texid,
	--bluewood_texid,
	skylite_texid,
	sgate_texid,
	bull_texid,
	pool_texid,
	ball_texid,
	gleaves_texid, fern_texid, sand_texid, -- 29dec17

	moss_texid,
	keyhole_texid,
	slime_texid, 
	roots_texid,

	korla_texid,
	kevin_texid,
	exit_texid,
	lab_texid,
	dmaze_texid,

	avatar_texid,
	snake_texid,

	tree1_texid, tree2_texid, tree3_texid,
	tree4_texid, tree5_texid, tree8_texid,
	spalm_texid, tpalm_texid,

	mural_texid,
	greekey_texid,
	adobe_texid,
	lion_texid,
	gmarble_texid,
	bmarble_texid,
	moorwall_texid,
	mazeouter_texid,
	mazeouter9_texid,
	mazex_texid,
	mazext_texid,
	frame_texid,
	doort_texid,
	doortw_texid,
	hedge_texid,
	deadbdragon_texid,
	deadrdragon_texid,
	deadminotaur_texid,
	bat1_texid, bat2_texid,
	chalice_texid, sword_texid,
	cherry_texid, room_texid, ceil_texid, floor_texid, 
	art_texid, mat_texid, rug_texid, bug_texid,
	wood_texid, 
	key_texid, 
	gkey_texid, bkey_texid,	gate_texid, zoro_texid,
	darkcubemap_texid, 
	sunnycubemap_texid,
	cloudycubemap_texid,
	mooncubemap_texid,
	grass1_texid,
	ava_texid,
	jup_texid,
	black_texid,
	grass_texid, stone_texid, slot_texid : gluint := 0;

	atree2_texid: array(0..3) of gluint;

------------ end texture pointers ------------------------------------




	playSecs : float;

	package myint_io is new text_io.integer_io(integer);
	package myfloat_io is new text_io.float_io(float);







-- begin string pointers for getUniformLocation: ---------------------



	--waterfall parms:
	wfxc: constant glfloat := -5.0;
	wfyc: constant glfloat :=  1.7;
	wfzc: constant glfloat :=  0.272;

	wfxr: constant glfloat :=  0.08;
	wfyr: constant glfloat :=  2.1;
	wfyv: constant glfloat :=  2.0;


	--cistern parms:
	cisxc: constant float := float(wfxc);
	cisyc: constant float := float(wfyc-wfyr) - 0.1;
	ciszc: constant float := 0.59;
	cisrr: constant float := 0.7;


-- begin string pointers for getUniformLocation:

	--for new waterfall:
	piflag: chars_ptr := new_string("iflag"&ascii.nul);




	phang : chars_ptr := new_string("horiAng"&ascii.nul);

	psand : chars_ptr := new_string("sandLevel"&ascii.nul);
	ppid : chars_ptr := new_string("palmID"&ascii.nul);
	pmytime : chars_ptr := new_string("mytime"&ascii.nul);

	pmyfade : chars_ptr := new_string("fade"&ascii.nul);
	pmydir : chars_ptr := new_string("direction"&ascii.nul);

	pwaterlevel : chars_ptr := new_string("waterlevel"&ascii.nul);

	pwPos : chars_ptr := new_string("wPos"&ascii.nul);
	pwRad : chars_ptr := new_string("wRad"&ascii.nul);

	--pnm : chars_ptr := new_string("NormalMatrix"&ascii.nul); --NOT needed

	pmv : chars_ptr := new_string("MV"&ascii.nul);
	pmvp : chars_ptr := new_string("MVP"&ascii.nul);
	pdark : chars_ptr := new_string("darkness"&ascii.nul);

	pmylev : chars_ptr := new_string("foglevl"&ascii.nul);
	pmyclr : chars_ptr := new_string("fogcolr"&ascii.nul);
	pme : chars_ptr := new_string("eyePos"&ascii.nul);


	psprit : chars_ptr := new_string("sprite"&ascii.nul);
	pmyts : chars_ptr := new_string("myTextureSampler"&ascii.nul);
	popac : chars_ptr := new_string("NormalOpacity"&ascii.nul);
	pcubemap : chars_ptr := new_string("CubeMap"&ascii.nul);

	ptime : chars_ptr := new_string("time"&ascii.nul);
	presol : chars_ptr := new_string("resolution"&ascii.nul);


	prad  : chars_ptr := new_string("rad"&ascii.nul);
	pangl : chars_ptr := new_string("angl"&ascii.nul);
	pwvel : chars_ptr := new_string("wvel"&ascii.nul);
	pwamp : chars_ptr := new_string("wamp"&ascii.nul);


---------- addendum for lighting

	pflag : chars_ptr := new_string("lightFlag"&ascii.nul);
	--pnormal : chars_ptr := new_string("normalMatrix"&ascii.nul);

	ppcolor : chars_ptr := new_string("vPtCol"&ascii.nul);
	ppos : chars_ptr := new_string("vPtPos"&ascii.nul);
	peye : chars_ptr := new_string("eyePos"&ascii.nul);
	pmspc : chars_ptr := new_string("fracMspc"&ascii.nul);
	pmdif : chars_ptr := new_string("fracMdif"&ascii.nul);

	pmyopac : chars_ptr := new_string("opacity"&ascii.nul);

	penvmap : chars_ptr := new_string("envMap"&ascii.nul);

-- end string pointers for getUniformLocation: -------------------------






--------- begin new PIDs/UIDs ---------------------


	--new waterfall, cistern
	pidwfall, pidrock36: gluint;
	ieye36,flev36,fcol36,
	imvp36,samp36,dark36,
	icen36,
	mvp43,time43,flag43,
	sprts43,ieye43,flev43,
	fcol43,dark43,icen43
	: glint;





	pidskyb01,
	pidterra02,
	pidfire03,
	pidfog03,
	pidstar04,
	pidtex05,
	pidcup06,
	pidava07, --avaTexShadID, 
	pidtree08,
	pidhole09,
	pidsky10, 
	pidsky11,
	pidsnake12,
	pidpool13, pidpool15,
	pidbull14: gluint; --PIDs

----------------------------------------

	cubemap_texid : gluint;

	mvpid15,

	ieye14, 
	envm14,irad14,wlev14,

	mvpid01, mapid01,fcolid01,flevid01, 

	fcolid02, flevid02, eyeid02,
	mvpid02, sampid02, darkid02,
	
	mvpid03, sampid03, opacid03, timeid03, cenid03, radid03, 
	radid03f, mvpid03f, cenid03f, ieye03,

	timeid04,resid04,mvpid04,

	ndc05,
	eawe05, noso05,
	xdc05,zdc05,
	mvpid05,sampid05,flevid05,fcolid05,darkid05,
	lflagid05,lcolrid05,lposid05,eyeid05,ldifid05,lspcid05,

	mvpid06, sampid06, darkid06, cenid06, radid06,
	lcolrid06, lposid06, eyeid06, lflagid06, hangid06, flevid06, fcolid06,

	darkid07,fadid07,sampid07,hangid07,
	cenid07,mvpid07,timeid07,dirid07,

	fcolid08,flevid08,eyeid08,
	mvpid08,sampid08,darkid08,
	timeid08,trid08,baseid08,
	radid08, cenid08,

	mvpid09,sampid09,radid09,cenid09,
	darkid09,
	lflagid09,lcolrid09,lposid09,eyeid09,ldifid09,lspcid09,

	mvpid10,resid10,timeid10,

	mvpid11,resid11,timeid11,

	mvpid12,sampid12,cenid12,radid12,
	anglid12,wvelid12,wampid12,
	darkid12,eyeid12, flevid12, fcolid12,

	eyeid13,darkid13,flevid13,fcolid13,
	mvpid13,timeid13,cenid13,radid13, wlevid13,
	
	imvp14, icen14, hang14, time14, samp14,idir14,

	ieye15,wlev15,irad15,icen15,time15,impv15,envm15

	: glint; --UIDs


--------- end new PIDs/UIDs ---------------------





	j1,j2,j3,j4,j5,j6 : float; -- junk bounds to ignore

	xgxp,zgxp, xgxm,zgxm, xgzp,zgzp, xgzm,zgzm: float;

	gateheld,
	gkeyheld,
	bkeyheld, lionnear, labnear,
	seen1already, batested9, batested7, bat7sent, bat9sent,
	batested1, batested5, batested6,
	bat1sent,  bat56sent, batfly, 
	bathaswkey, bathasbkey, bathaschalice,
	bathassword, -- 10feb23
	bdragondead, bdragonsent,
	rdragondead, rdragonsent, rdragonfly, bdragonfly,
	--minosent, 
	minorun, minotaurdead,

	bkeyseen, wkeyseen, 
	pedestalnear,
	swordnear, chalicenear, swordheld, chaliceheld,
	interior, gatewait, 
	mazewait, lionwait, labwait,
	gkeynear, bkeynear,wkeynear, wkeyheld: boolean := false;

	mazenear, pgatenear, gatenear : boolean := false;

	openlab, openlion, openmaze, opengate : boolean := false;

	gk1,gk2, -- exterior maze gates
	wlabko, elabko,
	lko, -- lion gate
	gzpk,gzmk, gxpk,gxmk : integer;

	labopening,
	mazegoingup,
	labopen, gateopen,
	liongoingup, lionopen, mazeopen,
	xpup,xmup, zpup,zmup : boolean := false;

	mazetime,
	labtime, liontime, lifttime : gldouble ;
	liftduration : constant gldouble := 8.3; --seconds


tt, pltime, yg, elapsed: gldouble := 0.0;
pickdwell : constant gldouble := 0.3;



------------------------------ end stuff from main -----------------------------
cdo, mdo5, mdo6, 
	mdo9, mdo7, mdo8, tdo : droomobj.room; --castle, maze, temple
--tdo7: troomobj.room; --topless room


	nuscene, scene: integer := 0;

dragonduration: constant gldouble := 7.0; --seconds
batduration: constant gldouble := 30.0; --seconds



bsdra, rsdra : integer; --scene# of dragon death

mxdra,mydra,mzdra,
bxdra,bydra,bzdra,
rxdra,rydra,rzdra,
xbat,ybat,zbat: float;

dragonstart, batstart: gldouble;

-- this must exclude other nearby connections
samequad: constant float := 1.5; --threshold distance to neighbor maze

heralded, success : boolean := false;

----- begin new mazes -------------------------------------

	-- make consistent with interiors -10..10, -3..3, -10..10

	subtype scenerng is integer range 5..9; -- 5 multiply-connected parts
	mrows, mcols : constant integer := 10;
	mzwall : array(scenerng, -mrows..mrows, -mcols..mcols) of pictobj.pictangle;


	--passthru, --most convenient way to identify "shortcuts"

	iswall : array(scenerng, -mrows..mrows, -mcols..mcols) of boolean
		:= (others=>(others=>(others=>false)));


	package longtube is new ztubeobj(32); --25);
	snake : longtube.ztube;

	snake7rad, snake7angl, x7snake, z7snake,
	snake8rad, snake8angl, x8snake, z8snake,

	snake5angl, snake6angl, snake5rad, snake6rad,
	x6snake,z6snake, x5snake,z5snake : float;


	warning1,
	imdead_toxicfog,
	imdead_fireball,
	imdead_minotaur,
	imdead_dragon, imdead_snake: boolean := false;

	snakeVSsword: integer := 0;


--------- family of moorish columns with cornices ---------------



	moorishpillar : array(0..4) of cylobj.cylinder;
	mpx,mpz : array(0..4) of float; -- pillar centers


	slab, edgezm,edgezp,edgexp,edgexm : rectobj.rectangle; 
	-- interior roof to pillars, pool
	-- and sides of pool

	rr: float; --hole radius






	package newrectsurf is new rectsurfobj(40);
	rso : newrectsurf.rectsurf;

	package reflsurf is new rectsurfobj(120); -- reflective water level 2
	rfo : reflsurf.rectsurf;



	nsortlimit: constant integer := 99;
	subtype nsortrng is integer range 0..nsortlimit;

	type sortarray is array(nsortrng) of integer;
	pfar2near, pnear2far, cf2n : sortarray;


	type limarray is array(nsortrng) of float;

	-- these describe locations of objects to be sorted:
	ox,oz : limarray := ( others => 0.0 );

	-- 1st subscript = no,so,ea,we
	-- 2nd subscript = pillar #
	hox, hoy, hoz : array(1..4,0..4) of float; --hole centers
	cornice : array(1..4,0..4) of rectobj.rectangle;

	bugEt, bugTimeStart: gldouble := 0.0;
	bugThreshold: constant gldouble := 15.0; --seconds
	warn_bug, imdead_bug, bugloop: boolean := false;

	-- soundIDs:
	evilBugs, introsong,
	wind1, atmos8, dang8, tmpl4,
	stone, down, up, die, eat, roar, won,
	hiss, misr, turk, angv, ibn, smallwf, toxic,
	water, medusascream, girlyscream,
	monsterScream, womanScream, gameover: glint;


	resfile : string := "./data/resume_av.txt";

	major, minor, profile, flags, mtu :  glint;

-----------------------------------------------------------

	veldir, horiang, vertang : float := 0.0; -- avatar actual
	choriang : float := 0.0; -- cam actual
	ahoriang : float := 0.0; -- cam looking @ avatar

	ixcam,iycam,izcam,        -- ideal cam.pos
	xcam, ycam, zcam: float;  -- actual cam.pos

	cxlook,cylook,czlook,       -- actual camera orientation
	xlook,ylook,zlook : float := 0.0; -- avatar orientation = ideal camera

	thirdPerson, showingHand, showingGlyph: boolean := false;

	direction: integer;

	ava : avatarolay.avatar;

	--28may18:
	bull: avatarolay.avatar;
	bulltime: gldouble := -20.0;
	xbull: float := +5.0;
	zbull: float := -5.0;
	ybull,
	hbull: float := -fourthpi; --0.0; --horiAng of bull
	dbull: integer;



	extdarkness,	
	extfogclr,
	extfoglev -- 14dec17
		: glint;

	okmultisamp: boolean := false;


-- lev: 0=>none, 1=>normal, 2=>dense, 3=>extreme fog
-- clr: 0=>true color, 1=>white, 2=>brown, 3=>purple, 4=>gray

-- glint == interfaces.c.int
--darkness : glint := 2; -- 0=none ... 4=darkest


-- castle2:
	castledarkness : constant glint := 3; --3; 
	pooldarkness : constant glint := 2; --2;
	foglev2 : constant glint := 0;
	fogclr2 : constant glint := 1;

	type dctype is array(1..14) of float;

	xdc,zdc: dctype := (others=>99.0);
	noso, eawe : glint := 0;

	chapter: integer; --moved here 30oct19

--================chapter 2 below:

-- maze9:
	mazedarkness9 : glint := 4; --3; 
	foglev9: constant glint := 0;
	fogclr9: constant glint := 1;
	ndc9: constant glint:=13;
	xdc9,zdc9: dctype; --discard centers

--lab8:
	darkness8: constant glint := 2;
	foglev8 : constant glint := 1;
	fogclr8 : constant glint := 3;

--maze7
	darkness7 : constant glint := 2;
	foglev7 : constant glint := 2; --3;
	fogclr7 : constant glint := 1;
	ndc7: constant glint:=13;
	xdc7,zdc7: dctype; --discard centers


--================chapter 1 below:

--maze6
	darkness6 : constant glint := 2;
	foglev6 : constant glint := 2;
	fogclr6 : constant glint := 1;
	ndc6: constant glint:=14;
	xdc6,zdc6: dctype; --discard centers

--maze5
	darkness5 : constant glint := 2;
	foglev5 : constant glint := 2;
	fogclr5 : constant glint := 1;
	ndc5: constant glint:=10;
	xdc5,zdc5: dctype; --discard centers

	ndc8: constant glint:=2;
	xdc8,zdc8: dctype; --discard centers

--temple4
	darkness4 : constant glint := 2;
	foglev4 : constant glint := 1;
	fogclr4 : constant glint := 3;

--==========chapter 3/4 (dark chapter 1/2):
-- foglev:=3; --extreme (dense)
-- fogclr:=4; --gray color
-- darkness := 2;


	-- bounds for secret room (see avent-setup_maze9.adb)
	xmin9, xmax9, zmin9, zmax9: float; 

	stop9, stop4,
	play9, play4  --signal after 1st playing of chalice choir
		: boolean := false;	

-- castle bounds:
	x2c, z2c: constant float := -5.0;
	x2r, z2r: constant float := +5.0;
	y2c, y2r: constant float := iymax/2.0;



-------------------------------------------------

	package fireballobj is new rectxfineobj(16);
	fireball : fireballobj.rectfine;
	barrad: constant float := 0.5; --radius of fireball

	package fogballobj is new cloudobj(16);
	fogball: fogballobj.cloud;

-----------------------------------------------------------

	-- 4 is nice but very round, 
	-- 2 is angular & interesting
	--package myrockobj is new rectfineobj(4); -- must now be EVEN
	--rococo : myrockobj.rectfine; --used here for cistern @ rear castle
	rococo: cyl2texobj.ball;

-----------------------------------------------------------


	-- Camera Standoff Parms:

	hoffset: constant float := 0.8; --4.0*margin; --0.8
	voffset: constant float := 0.1; --0.5*margin; --0.1

	--ocamdist, camdist: float := 1.0;
	-- zoom offset factor:
	ocamdist, camdist: float := float(zoomwheel.zdefault); --9feb22


	--linear move speed factor
	speed : constant float := 2.0;

	--keyboard angular speed factor:
	angrate1: constant float := 1.0; -- 1st person
	angrate3: constant float := 2.0; -- 3rd person

	lazyCam: boolean := true; --toggle between 2 camera types




-- begin Mappings of special characters in Font-Dictionary:
	bat1   : constant integer := 16; --123; ----"{"
	bat2   : constant integer := 18; --125; ----"}"

	blakey : constant integer := 17; --124; ----"|"
	whikey : constant integer := 19; --126; ----"~"
	grekey : constant integer := 20; -- 94; ----"^"

	hand   : constant integer :=  0; --0=normalHand, 3=splayedHand
	spider : constant integer := 11; -- 58; ----":"
	csword : constant integer := 14; -- 61; ----"="
	chalis : constant integer := 12; -- 59; ----";"
	gnu    : constant integer :=  1; -- 33; ----"!"

	bladra : constant integer := 15; -- 62; ----">"
	blakil : constant integer := 13; -- 60; ----"<"

	reddra : constant integer :=  9; -- 41; ----")"
	redkil : constant integer :=  8; -- 40; ----"("

	gatfar : constant integer :=  4; -- 36; ----"$"
	gatner : constant integer :=  5; -- 37; ----"%"

	-- addenda 20may20:
	lfbeet : constant integer := 21; -- nwBeetle
	rtbeet : constant integer := 22; -- neBeetle
	upbeet : constant integer := 23; -- noBeetle

-- end Mappings of special characters in Font-Dictionary:

	--statefile : constant string := "data/gamestate.txt";
	statefilex : constant string := "data/gamestatex.txt"; --for aborts only


	--controller sensitivity settings
	fspd, --ForwardSpeed
	mslu, --MouseSlew
	kslu, --KeySlew
	gslu, --GamepadSlew
	jslu  --JoystickSlew
		: float := 1.0; -- defaulted to 1.0

	setfile : string := "./data/settings.txt";

	bugstarted: boolean := false;







-- begin rusty addendum -------------------------------------------
		-- vertex-shader:
		lproj : chars_ptr := new_string("projection"&ascii.nul);
		lview : chars_ptr := new_string("view"&ascii.nul);
		lmodl : chars_ptr := new_string("model"&ascii.nul);

		-- fragshader:

	phrad  : chars_ptr := new_string("hrad"&ascii.nul);
	phole  : chars_ptr := new_string("hole"&ascii.nul);


		lcampos : chars_ptr := new_string("camPos"&ascii.nul);
		lpos0 : chars_ptr := new_string("lightPos0"&ascii.nul);
		lpos1 : chars_ptr := new_string("lightPos1"&ascii.nul);
		lpos2 : chars_ptr := new_string("lightPos2"&ascii.nul);
		lpcolor : chars_ptr := new_string("lightColor"&ascii.nul);

		lalbm : chars_ptr := new_string("albedoMap"&ascii.nul);
		lnrmm : chars_ptr := new_string("normalMap"&ascii.nul);
		lmetm : chars_ptr := new_string("metallicMap"&ascii.nul);
		lrufm : chars_ptr := new_string("roughnessMap"&ascii.nul);
		laom : chars_ptr := new_string("aoMap"&ascii.nul);

		albm34a,nrmm34a,metm34a,rufm34a,aom34a,
		campos34a, pcolr34a, 
		phrad34a, phole34a,
		proj34a,view34a,modl34a,
		pos0_34a,pos1_34a,pos2_34a
			:glint;

		pidzpm34a,
		albedo34b, normal34b, metallic34b, roughness34b,
		albedo34a, normal34a, metallic34a, roughness34a, ao34a: gluint;

-- end rusty addendum-------------------------------------------


-------- begin exterior tree centers/radii -----------------
	xt1c,yt1c,zt1c, 
	xt2c,yt2c,zt2c, 
	xt3c,yt3c,zt3c,

	xt4c,yt4c,zt4c, 
	xt5c,yt5c,zt5c, 
	xt6c,yt6c,zt6c,

	xt1r,yt1r,zt1r, 
	xt2r,yt2r,zt2r, 
	xt3r,yt3r,zt3r,

	xt4r,yt4r,zt4r, 
	xt5r,yt5r,zt5r, 
	xt6r,yt6r,zt6r 

		: glfloat;






end gametypes;

