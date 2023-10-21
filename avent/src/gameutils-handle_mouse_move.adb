separate(gameutils)



procedure handle_mouse_move( nowTime : gldouble ) is

	mousedx, mousedy : gldouble;
	dt : gldouble := nowTime-oldMdTime;
	et : float := float(dt);
	ff : float := 1.0; --factor quicker response @ 3rd person
	xpos,ypos : aliased gldouble;

begin

	if thirdperson then ff:=ff*1.2; end if; --add 20% response
	--if hidpi then ff:=ff*2.0; end if; 

	if et>0.1 then et:=0.0; end if; 
	--prohibit large jumps due to stale oldMdTime.

	glfwgetcursorpos(mainWindow,xpos'access,ypos'access);

	mousedx:=xpos-xold;
	mousedy:=ypos-yold;
	xold:=xpos;
	yold:=ypos;
	oldMdTime:=nowTime;

	--horiAng := horiAng - 0.0007 * float(mousedx); old way
	--vertAng := vertAng - 0.0007 * float(mousedy);

	-- should include factor (et) that makes response
	-- proportional to time since last update.
	horiAng := horiAng - 0.05 *et*ff*mslu* float(mousedx);
	vertAng := vertAng - 0.05 *et*ff*mslu* float(mousedy);

	slewToAv; --27jan18

	xlook := fmath.cos(vertAng)*fmath.sin(horiAng);
	ylook := fmath.sin(vertAng);
	zlook := fmath.cos(vertAng)*fmath.cos(horiAng);

	updategamestate;


end handle_mouse_move;



