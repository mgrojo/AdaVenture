
separate(avent)

procedure setup_textures is
	bipedal: constant boolean := true; --  minotaur (false=>quadriped)
begin 

--------- castle begin --------------------------------------------




--copper arches begin----------------------------------------------------
	pidzpm34a:= LoadShaders( "data/rusty.vs", "data/rusty.fs" );

		-- vertex shader uniforms:
		proj34a   := glgetuniformlocation(pidzpm34a, lproj);--projection
		view34a   := glgetuniformlocation(pidzpm34a, lview);--view
		modl34a   := glgetuniformlocation(pidzpm34a, lmodl);--model

		-- fragment shader uniforms:

phrad34a:=glGetUniformLocation( pidzpm34a, phrad);
phole34a:=glGetUniformLocation( pidzpm34a, phole);

		pcolr34a := glGetUniformLocation( pidzpm34a, lpcolor );
		pos0_34a := glGetUniformLocation( pidzpm34a, lpos0 );
		pos1_34a := glGetUniformLocation( pidzpm34a, lpos1 );
		pos2_34a := glGetUniformLocation( pidzpm34a, lpos2 );

		albm34a := glGetUniformLocation( pidzpm34a, lalbm ); --tex0
		nrmm34a := glGetUniformLocation( pidzpm34a, lnrmm ); --tex1
		metm34a := glGetUniformLocation( pidzpm34a, lmetm ); --tex2
		rufm34a := glGetUniformLocation( pidzpm34a, lrufm ); --tex3
		--aom34a := glGetUniformLocation( pidzpm34a, laom );   --tex4

		campos34a := glGetUniformLocation( pidzpm34a, lcampos );

	normal34a := loadPng(repeat,"data/normal.png"); -- uses XYZ
	--simply blue
	--ao34a := loadPng(repeat,"data/ao.png"); -- uses R i
	--(simply white)

	albedo34a := loadPng(repeat,"data/diffuse.png"); -- uses RGB
	metallic34a := loadPng(repeat,"data/specular.png"); -- uses R
	roughness34a := loadPng(repeat,"data/aspecular.png"); -- uses R

--copper arches end----------------------------------------------------


-- begin alcove ------------------------------------------------
	normal34b := loadPng(repeat,"data/alnorm.png"); -- uses XYZ
	--normal34b := loadPng(repeat,"data/alnorm4.png"); -- uses XYZ
	albedo34b := loadPng(repeat,"data/alcove.png"); -- uses RGB
	metallic34b := loadPng(repeat,"data/almet.png"); -- uses R
	--metallic34b := loadPng(repeat,"data/almet3.png"); -- flat wings
	roughness34b := loadPng(repeat,"data/alrough.png"); -- uses R
-- end alcove ------------------------------------------------









	--dark clouds:
	darkcubemap_texid := loadCubePng(
	"data/skyBoxes/stormy/px.png",	"data/skyBoxes/stormy/nx.png",
	"data/skyBoxes/stormy/py.png",	"data/skyBoxes/stormy/ny.png",
	"data/skyBoxes/stormy/pz.png",	"data/skyBoxes/stormy/nz.png"	);

	--brighter, but still cloudy:
	cloudycubemap_texid := loadCubePng(
	"data/skyBoxes/clouds/bluft.png",	"data/skyBoxes/clouds/blubk.png",
	"data/skyBoxes/clouds/bluup.png",	"data/skyBoxes/clouds/bludn.png",
	"data/skyBoxes/clouds/blurt.png",	"data/skyBoxes/clouds/blulf.png"	);

	--sunny day:
	--sunnycubemap_texid := loadCubePng(
	--"data/skyBoxes/tropicalsun/slt.png","data/skyBoxes/tropicalsun/srt.png",
	--"data/skyBoxes/tropicalsun/sup.png","data/skyBoxes/tropicalsun/sdn.png",
	--"data/skyBoxes/tropicalsun/sfr.png","data/skyBoxes/tropicalsun/sbk.png");

	--sunny day (awesome new clouds):
	sunnycubemap_texid := loadCubePng(
	"data/skyBoxes/thicksky/lflo.png","data/skyBoxes/thicksky/rtlo.png",
	"data/skyBoxes/thicksky/up.png","data/skyBoxes/thicksky/dn.png",
	"data/skyBoxes/thicksky/ftlo.png","data/skyBoxes/thicksky/bklo.png");


	mooncubemap_texid := loadCubePng(
	"data/skyBoxes/moon/lf.png",	"data/skyBoxes/moon/rt.png",
	"data/skyBoxes/moon/up.png",	"data/skyBoxes/moon/dn.png",
	"data/skyBoxes/moon/fr.png",	"data/skyBoxes/moon/bk.png");



	pidskyb01 := LoadShaders( "./data/oskyFog.vs", "./data/oskyFog.fs" );
	mvpid01 := glGetUniformLocation( pidskyb01, pmvp );     
	mapid01 := glGetUniformLocation( pidskyb01, pcubemap );     
	flevid01 := glGetUniformLocation( pidskyb01, pmylev ); --14dec17
	fcolid01 := glGetUniformLocation( pidskyb01, pmyclr ); --19dec17



	pidterra02 := 
		LoadShaders( "./data/yislandobjFog.vs", "./data/islandobjFog.fs" );
	mvpid02 := glGetUniformLocation( pidterra02, pmvp );     

	eyeid02 := glGetUniformLocation( pidterra02, peye );

	sampid02 := glGetUniformLocation( pidterra02, pmyts );     
	darkid02 := glGetUniformLocation( pidterra02, pdark );     
	flevid02 := glGetUniformLocation( pidterra02, pmylev ); --14dec17
	fcolid02 := glGetUniformLocation( pidterra02, pmyclr ); --19dec17

	terrain.setrect(grounds, 0.0,0.0, xmax, zmax );


	black_texid := loadPng(pngloader.mirror,"data/black.png");

	grass1_texid := loadPng(pngloader.mirror,"data/grass1.png");

	grass_texid := loadPng(pngloader.mirror,"data/grasss.png");
	-- note that loadPng uses mirrored_repeat texture wrapping

	-- Note that when (xme,zme) is near the edge of the domain,
	-- we don't want the far edge to get clipped by the skybox.
	cubemapobj.setrect( skybox, 0.0,0.0,0.0, xmax*2.0,ymax*2.0,zmax*2.0 );

	stone_texid  := loadPng(mirror,"data/darkstonewall.png");
	slot_texid  := loadPng(mirror,"data/midslot.png");
	gate_texid  := loadPng(mirror,"data/pbayGate.png");
	zoro_texid  := loadPng(mirror,"data/zoroaster.png");
	gkey_texid  := loadPng(mirror,"data/gkey.png");
	bkey_texid  := loadPng(mirror,"data/bkey.png");
	key_texid  := loadPng(mirror,"data/wkey.png");
	sword_texid  := loadPng(mirror,"data/swordw.png");
	wood_texid  := loadPng(mirror,"data/wood.png");

	jup_texid  := loadPng(mirror,"data/jupitermap.png");

	--gleaves_texid  := loadPng(mirror,"data/greenleaves0.png");
	--gleaves_texid  := loadPng(repeat,"data/greenleavess.png");--seamless(Mz9)

	--looks great but too light:
	gleaves_texid  := loadPng(repeat,"data/tinyLeavesSeamlessd.png");--(Mz9)

	sand_texid  := loadPng(mirror,"data/Sand.png");



	keyhole_texid  := loadPng(mirror,"data/keyhole.png");

	slime_texid  := loadPng(mirror,"data/slime.png");
	moss_texid  := loadPng(mirror,"data/moss.png");
	roots_texid  := loadPng(mirror,"data/dry_roots.png");

	moorwall_texid  := loadPng(mirror,"data/moorishWall.png");
	gmarble_texid  := loadPng(mirror,"data/marblegray.png");
	bmarble_texid  := loadPng(mirror,"data/marblebrown.png");

	--25mar21 addendum:
	rosequartz_texid:=loadPng(mirror,"data/roseQuartz.png"); --unused fttb
	copper_texid:=loadPng(mirror,"data/copper.png");
	--bluewood_texid:=loadPng(mirror,"data/blueWood.png");

	deadbdragon_texid  := loadPng(mirror,"data/dbd.png");
	deadrdragon_texid  := loadPng(mirror,"data/drd.png");
	--deadminotaur_texid  := loadPng(mirror,"data/deadgnu.png");
	--deadminotaur_texid  := loadPng(mirror,"data/deadMino.png");
	deadminotaur_texid  := loadPng(mirror,"data/minotx.png");

	skylite_texid  := loadPng(mirror,"data/skylite.png");
	sgate_texid  := loadPng(mirror,"data/stargate_off.png");


	doort_texid  := loadPng(mirror,"data/doort.png");
	--doort_texid  := loadPng(mirror,"data/doortFrame.png");

	--doortw_texid  := loadPng(mirror,"data/doortw.png");
	doortw_texid  := loadPng(mirror,"data/doortf3.png");

	--hedge_texid  := loadPng(mirror,"data/hedge2Pbay.png");
	hedge_texid  := loadPng(repeat,"data/hedge2Pbay.png");


	lion_texid  := loadPng(mirror,"data/lion.png");


	korla_texid := loadPng(mirror,"data/pandit.png");
	kevin_texid := loadPng(mirror,"data/kmacleodFez.png");




	pidfire03:=loadshaders("./data/fireball.vs", "./data/fireball.fs");
	mvpid03 := glgetuniformlocation(pidfire03, pmvp);
	sampid03  := glgetuniformlocation(pidfire03, pmyts);
	opacid03  := glgetuniformlocation(pidfire03, pmyopac);
	timeid03  := glgetuniformlocation(pidfire03, ptime);
	cenid03  := glgetuniformlocation(pidfire03, pwPos);
	radid03 := glgetuniformlocation(pidfire03, pwRad);

	--ball_texid:= loadPng(mirror,"data/explosion.png"); --radial color
	ball_texid:= loadPng(mirror,"data/explosion2.png"); --radial color

	fireballobj.setrect(fireball,j1,j2,j3,j4,j5,j6);




------------ chalice room (M9) begin -----------------------------------

	--very nice black&white clouds in a black sky (lightweight)
	--pidstar04 := loadshaders("./data/skyX.vs", "./data/bw2clouds.fs");

	-- nice lightweight clouds in deep blue sky
	pidstar04 := loadshaders("./data/skyX.vs", "./data/lightweightclouds.fs");
	timeid04 := glgetuniformlocation(pidstar04, ptime);
	resid04  := glgetuniformlocation(pidstar04, presol);
	mvpid04  := glgetuniformlocation(pidstar04, pmvp);





	pidtex05 := loadshaders("./data/texobjFog.vs", "./data/texobjShine.fs");

ndc05:=glgetuniformlocation(pidtex05, new_string("numDC") );
eawe05:=glgetuniformlocation(pidtex05, new_string("EW") );
noso05:=glgetuniformlocation(pidtex05, new_string("NS") );
xdc05:=glgetuniformlocation(pidtex05, new_string("xdc") );
zdc05:=glgetuniformlocation(pidtex05, new_string("zdc") );

	mvpid05 := glgetuniformlocation(pidtex05, pmvp); --MVP
	sampid05  := glgetuniformlocation(pidtex05, pmyts); --myTextureSampler
	darkid05 := glGetUniformLocation(pidtex05, pdark );--darkness
	flevid05  := glgetuniformlocation(pidtex05, pmylev);--foglevel
	fcolid05  := glgetuniformlocation(pidtex05, pmyclr);--fogcolor


	-- lighting effects
	lflagid05 := glGetUniformLocation( pidtex05, pflag );
	lcolrid05 := glGetUniformLocation( pidtex05, ppcolor );
	lposid05 := glGetUniformLocation( pidtex05, ppos );
	eyeid05 := glGetUniformLocation( pidtex05, peye );
	ldifid05 := glGetUniformLocation( pidtex05, pmdif );
	lspcid05 := glGetUniformLocation( pidtex05, pmspc );




	mural_texid := loadPng(mirror,"data/mural.png");
	greekey_texid := loadPng(mirror,"data/greek.png");
	adobe_texid := loadPng(mirror,"data/darkadobe.png");
	cherry_texid := loadPng(mirror,"data/wood2_texture.png"); --cherry wood
	room_texid := loadPng(mirror,"data/darkstonewall.png"); 

	mazext_texid  := loadPng(mirror,"data/wall-pbay.png"); --stoneFence
	mazex_texid  := loadPng(mirror,"data/fern1a.png"); --nice 10mar18
	--mazeouter9_texid  := loadPng(repeat,"data/hedge3Pbay.png"); --rotatedOrig
	mazeouter9_texid  := loadPng(repeat,"data/hedge3seamless.png"); --rotatedOrig
	mazeouter_texid  := loadPng(repeat,"data/foliageSeamless.png"); --NICE!

	frame_texid  := loadPng(mirror,"data/gate.png");

	ceil_texid := loadPng(mirror,"data/mosqueceilSQ.png"); 
	floor_texid  := loadPng(mirror,"data/bricksE.png");

	rug_texid  := loadPng(mirror,"data/persiancarpetdark2.png");

	--Bad Beetles:
	--bug_texid  := loadPng(mirror,"data/beetleHorned.png");
	bug_texid  := loadPng(mirror,"data/beetleBlack.png");
	--bug_texid  := loadPng(mirror,"data/DeathwatchBeetle.png");
	-- beetleHorned.png, beetleGreen.png, beetleBlack.png,
	-- DeathwatchBeetle.png, FalsePPBeetle.png


	chalice_texid  := loadPng(mirror,"data/chalice.png");


	bat1_texid := loadPng(mirror,"data/bat0b.png");
	bat2_texid := loadPng(mirror,"data/bat1b.png");



	pidcup06:=LoadShaders("./data/rotexnew.vs","./data/texobjShine.fs");
	mvpid06 := glGetUniformLocation( pidcup06, pmvp );     
	sampid06 := glGetUniformLocation( pidcup06, pmyts );     
	darkid06 := glGetUniformLocation( pidcup06, pdark );     
	radid06 := glGetUniformLocation(pidcup06, pwrad); --// vec3
	cenid06 := glGetUniformLocation(pidcup06, pwpos); --// vec3

	flevid06  := glgetuniformlocation(pidcup06, pmylev);--foglevel
	fcolid06  := glgetuniformlocation(pidcup06, pmyclr);--fogcolor

	hangid06 := glgetuniformlocation(pidcup06, phang); --horiAng


	-- lighting effects
	lflagid06 := glGetUniformLocation( pidcup06, pflag );
	lcolrid06 := glGetUniformLocation( pidcup06, ppcolor ); --vPtCol
	lposid06 := glGetUniformLocation( pidcup06, ppos );
	eyeid06 := glGetUniformLocation( pidcup06, peye );







---- ava begin ----------------------------

	--pidava07
	pidava07:=LoadShaders("./data/avatarobj.vs","./data/avatarobj.fs");
	mvpid07 := glGetUniformLocation( pidava07, pmvp );     
	cenid07 := glGetUniformLocation(pidava07, pwpos); --// vec3
	hangid07 := glgetuniformlocation(pidava07, phang); --horiAng
	timeid07 := glGetUniformLocation( pidava07, pmytime );

	dirid07 := glGetUniformLocation( pidava07, pmydir );
	fadid07 := glGetUniformLocation( pidava07, pmyfade );

	sampid07 := glGetUniformLocation( pidava07, pmyts );

	darkid07 := glGetUniformLocation( pidava07, pdark ); --29jun19 (0..4)

	ava_texid := loadPng(mirror,"data/skin.png",false); --very good, dark clothes


	avatarolay.setrect(ava);


------- 24aug23  Minotaur begin --------------------------------

if bipedal then 

	-- newer upright bipedal minotaur:

	pidbull14:=LoadShaders("./data/minotaur2obj.vs","./data/minotaur2obj.fs");
	imvp14 := glGetUniformLocation( pidbull14, pmvp );     
	icen14 := glGetUniformLocation( pidbull14, pwpos); --// vec3
	hang14 := glgetuniformlocation( pidbull14, phang); --horiAng
	time14 := glGetUniformLocation( pidbull14, pmytime );
	idir14 := glGetUniformLocation( pidbull14, pmydir );

	samp14 := glGetUniformLocation( pidbull14, pmyts );
	bull_texid := loadPng(mirror,"data/minotaurSkin2.png",false); --noInvert

	avatarolay.setrect(bull);

else -- fierce quadripedal minotaur


	------- improved quadripedal minotaur:

	pidbull14:=LoadShaders("./data/minotaur4obj.vs","./data/minotaur4obj.fs");
	imvp14 := glGetUniformLocation( pidbull14, pmvp );     
	icen14 := glGetUniformLocation( pidbull14, pwpos); --// vec3
	hang14 := glgetuniformlocation( pidbull14, phang); --horiAng
	time14 := glGetUniformLocation( pidbull14, pmytime );
	idir14 := glGetUniformLocation( pidbull14, pmydir );

	samp14 := glGetUniformLocation( pidbull14, pmyts );
	bull_texid := loadPng(mirror,"data/minotaurSkin2.png",false); --noInvert

	avatarolay.setrect(bull); -- ? use simpler avatarobj instead ?

end if;

------- 24aug23  Minotaur end --------------------------------









--------- trees begin ---------------------------------------------

	spalm_texid := loadPng(mirror,"data/palm1.png");
	tpalm_texid := loadPng(mirror,"data/tallpalm1.png");

	tree1_texid := loadPng(mirror,"data/tree1.png");
	tree2_texid := loadPng(mirror,"data/tree2.png");
	tree3_texid := loadPng(mirror,"data/tree3.png");
	tree4_texid := loadPng(mirror,"data/tree4.png");
	tree5_texid := loadPng(mirror,"data/tree5.png");

	tree8_texid := loadPng(mirror,"data/tree-51363.png");

	exit_texid := loadPng(mirror,"data/wexit.png");
	lab_texid := loadPng(mirror,"data/wlab.png");
	dmaze_texid := loadPng(mirror,"data/wdmaze.png");




	--treeTexShadID
	pidtree08:=LoadShaders("./data/windtexFog.vs","./data/otexFog.fs");

	eyeid08 := glGetUniformLocation( pidtree08, peye );

	flevid08 := glGetUniformLocation( pidtree08, pmylev );     
	fcolid08 := glGetUniformLocation( pidtree08, pmyclr );     

	mvpid08 := glGetUniformLocation( pidtree08, pmvp );
	sampid08 := glGetUniformLocation( pidtree08, pmyts );
	darkid08 := glGetUniformLocation( pidtree08, pdark );
	timeid08 := glGetUniformLocation( pidtree08, pmytime );
	trid08 := glGetUniformLocation( pidtree08, ppid );
	baseid08 := glGetUniformLocation( pidtree08, psand );

	radID08 := glGetUniformLocation(pidtree08, pwrad); --// vec3
	cenid08 := glGetUniformLocation(pidtree08, pwpos); --// vec3


------------hole-shader begin ----------------------

	--holeTexShadID
	pidhole09:=LoadShaders("./data/texobjFog.vs","./data/texnewHole.fs");
	mvpid09 := glGetUniformLocation(pidhole09, pmvp);
	sampid09 := glGetUniformLocation(pidhole09, pmyts);

	-- hole parameters:
	radID09 := glGetUniformLocation(pidhole09, phrad); --// float hrad
	cenid09 := glGetUniformLocation(pidhole09, phole); --// vec3 hole

	darkid09 := glGetUniformLocation( pidhole09, pdark );
	flevid09 := glGetUniformLocation( pidhole09, pmylev );
	fcolid09 := glGetUniformLocation( pidhole09, pmyclr );

	-- lighting effects
	lFlagid09 := glGetUniformLocation( pidhole09, pflag );
	lcolrid09 := glGetUniformLocation( pidhole09, ppcolor );
	lposid09 := glGetUniformLocation( pidhole09, ppos );

	eyeid09 := glGetUniformLocation( pidhole09, peye );

	ldifid09 := glGetUniformLocation( pidhole09, pmdif ); --diffuse
	lspcid09 := glGetUniformLocation( pidhole09, pmspc ); --specular


--------------- maze room begin -----------------------------------
	-- M7 (off fttb)
	pidsky10:= loadshaders("./data/skyX.vs", "./data/lightweightclouds.fs");
	-- nice lightweight clouds in deep blue sky

	mvpid10 := glgetuniformlocation(pidsky10, pmvp);
	resid10 := glgetuniformlocation(pidsky10, presol);
	timeid10 := glgetuniformlocation(pidsky10, ptime);



	-- M5,M6 (off fttb)
	pidsky11:= loadshaders("./data/skyX.vs", "./data/tunnelstars.fs");
	-- ok, pretty awesome night sky...twinkling stars with liminous colors

	mvpid11 := glgetuniformlocation(pidsky11, pmvp);
	resid11 := glgetuniformlocation(pidsky11, presol);
	timeid11 := glgetuniformlocation(pidsky11, ptime);



	snake_texid := loadPng(pngloader.clamp,"data/mamba3.png");
	longtube.setrect( snake, 0.03, 0.02, 0.6);


	--snakeTexShadID 
	pidsnake12:= LoadShaders( "./data/snakeFogHiHead.vs", "./data/snakeFog.fs" );

	eyeid12 := glGetUniformLocation( pidsnake12, peye );

	mvpid12 := glGetUniformLocation( pidsnake12, pmvp );

	sampid12 := glGetUniformLocation( pidsnake12, pmyts );     
	cenid12 := glGetUniformLocation(pidsnake12, pwpos); --// vec3
	radid12 := glGetUniformLocation( pidsnake12, prad );     
	anglid12 := glGetUniformLocation( pidsnake12, pangl );     
	wvelid12 := glGetUniformLocation( pidsnake12, pwvel );     
	wampid12 := glGetUniformLocation( pidsnake12, pwamp );

	darkid12 := glGetUniformLocation( pidsnake12, pdark );
	flevid12 := glGetUniformLocation( pidsnake12, pmylev );
	fcolid12 := glGetUniformLocation( pidsnake12, pmyclr );






	-- pool:
--poolshadid
pidpool13:=loadshaders("./data/poolobjFog.vs","./data/myBlueWaterFog.fs");
	eyeid13 := glgetuniformlocation(pidpool13, peye);
	mvpid13 := glgetuniformlocation(pidpool13, pmvp);
	timeid13    := glgetuniformlocation(pidpool13, pmytime);
	wlevid13    := glgetuniformlocation(pidpool13, pwaterlevel);
	radid13    := glgetuniformlocation(pidpool13, pwRad);
	cenid13    := glgetuniformlocation(pidpool13, pwPos);

	darkid13   := glgetuniformlocation(pidpool13, pdark);
	flevid13   := glgetuniformlocation(pidpool13, pmylev); --19dec17
	fcolid13   := glgetuniformlocation(pidpool13, pmyclr);

--pool_texid := loadPng(pngloader.clamp,"data/poolWater.png");


------------- begin alternative to pool above ------------------

	-- level 2 reflective pool:
	--rpoolshadid  (lev#2)
	pidpool15:= loadshaders("data/reflwater.vs", "data/reflwater.fs");
	--note:  fogcolr,foglevl use defaulted values
	envm15 := glgetuniformlocation(pidpool15, penvmap);
	ieye15 := glgetuniformlocation(pidpool15, peye);
	mvpid15 := glgetuniformlocation(pidpool15, pmvp);
	time15 := glgetuniformlocation(pidpool15, pmytime);
	wlev15 := glgetuniformlocation(pidpool15, pwaterlevel);
	irad15 := glgetuniformlocation(pidpool15, pwRad);
	icen15 := glgetuniformlocation(pidpool15, pwPos);

	-- level 2 reflective pool;  
	-- Here, only used as reflective environment, but not drawn.
	cubemap_texid := loadCubePng(
	--"data/red.png", "data/red.png",
	--"data/blue.png", "data/blue.png",
	--"data/green.png", "data/green.png");
	"data/mardbrn.png", "data/mardbrn.png",
	"data/mardbrn.png", "data/mardbrn.png",
	"data/mardbrn.png", "data/mardbrn.png");
	--"data/marblebrown.png", "data/marblebrown.png",
	--"data/marblebrown.png", "data/marblebrown.png",
	--"data/marblebrown.png", "data/marblebrown.png");




	



end setup_textures; ---------------------------------------------------------




