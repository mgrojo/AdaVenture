separate( avent )

procedure getKeyInputs( 
	mainWin : access GLFWwindow ) is

	nowTime,
	deltaTime,
	slewsec: gldouble;
	keydlay: constant gldouble := 0.20;

--use zoomwheel;

begin

	nowTime := glfwGetTime;
	deltaTime := nowTime - oldTimeKb;


-- First, we process continuous key-press responses: quit, Nearer, Further, Pan, Move

--Quit
if glfwgetkey( mainWin, glfw_key_escape ) = Glfw_Press then
		userexit:=true;
		return;

--Quit
elsif glfwgetkey( mainWin, glfw_key_q ) = Glfw_Press then
		userexit:=true;
		return;
end if;

--Nearer
if glfwgetkey( mainWin, glfw_key_n ) = Glfw_Press then
			zoomwheel.zdist:=0.99*zoomwheel.zdist;
			--if zoomwheel.zdist<0.1 then zoomwheel.zdist:=0.1; end if;
			if zoomwheel.zdist<zoomwheel.zmin then zoomwheel.zdist:=zoomwheel.zmin; end if; --9feb22
			camdist:=float(zoomwheel.zdist); --
		return;

--Further
elsif glfwgetkey( mainWin, glfw_key_f ) = Glfw_Press then
			zoomwheel.zdist:=1.01*zoomwheel.zdist;
			--if zoomwheel.zdist>10.0 then zoomwheel.zdist:=10.0; end if;
			if zoomwheel.zdist>zoomwheel.zmax then zoomwheel.zdist:=zoomwheel.zmax; end if; --9feb22
			camdist:=float(zoomwheel.zdist); --
		return;

--zoomNormalize
elsif glfwgetkey( mainWin, glfw_key_z ) = Glfw_Press then

			if zoomwheel.zdist>zoomwheel.zdefault then
				zoomwheel.zdist := 0.99*zoomwheel.zdist;
			elsif zoomwheel.zdist<zoomwheel.zdefault then
				zoomwheel.zdist := 1.01*zoomwheel.zdist;
			end if;
			camdist:=float(zoomwheel.zdist);

			return;

end if;



--MoveFor
if 
	glfwgetkey( mainWin, glfw_key_up ) = Glfw_Press 
	or glfwgetkey( mainWin, glfw_key_w ) = Glfw_Press 
then
	moveForward(currentTime);

--MoveBak
elsif 
	glfwgetkey( mainWin, glfw_key_down ) = Glfw_Press 
	or glfwgetkey( mainWin, glfw_key_s ) = Glfw_Press 
then
	moveBackward(currentTime);

end if;


--panLeft
if 
	glfwgetkey( mainWin, glfw_key_left ) = Glfw_Press 
	or glfwgetkey( mainWin, glfw_key_a ) = Glfw_Press 
then 

	slewsec:=currentTime-slewtime;

	if slewsec<0.1 then
		roty := +0.5*kslu*float(slewsec);
		if thirdperson then
			hdang := angrate3*roty;
		else
			hdang := angrate1*roty;
		end if;
		horiang := horiang + hdang;
		slewToAv; --27jan18

		roty:=0.0;
		xlook := fmath.cos(vertang)*fmath.sin(horiang);
		ylook := fmath.sin(vertang);
		zlook := fmath.cos(vertang)*fmath.cos(horiang);

		updategamestate; --automatic in moveforward
	end if;
	slewtime := currentTime;


--panRight
elsif 
	glfwgetkey( mainWin, glfw_key_right ) = Glfw_Press 
	or glfwgetkey( mainWin, glfw_key_d ) = Glfw_Press 
then 

	slewsec:=currentTime-slewtime;

	if slewsec<0.1 then
		roty := -0.5*kslu*float(slewsec);
		if thirdperson then
			hdang := angrate3*roty;
		else
			hdang := angrate1*roty;
		end if;
		horiang := horiang + hdang;
		slewToAv; --27jan18

		roty:=0.0;
		xlook := fmath.cos(vertang)*fmath.sin(horiang);
		ylook := fmath.sin(vertang);
		zlook := fmath.cos(vertang)*fmath.cos(horiang);

		updategamestate; --automatic in moveforward
	end if;
	slewtime := currentTime;
end if;







-- Second, we process discrete key-press events -----------------------

if deltaTime > keydlay then


	if    glfwgetkey( mainWin, glfw_key_h ) = Glfw_Press then --toggleIntro (h=help)
		intro:=not intro;
		if intro then
			snd4ada.playLoop(introsong); --16sep23 addendum
		else
			snd4ada.stopLoop(introsong);
		end if;
		oldTimeKb := currentTime;

	elsif glfwgetkey( mainWin, glfw_key_i ) = Glfw_Press then --toggleIntro (i=intro)
		intro:=not intro;
		if intro then
			snd4ada.playLoop(introsong); --19jan23 addendum
		else
			snd4ada.stopLoop(introsong);
		end if;
		oldTimeKb := currentTime;

	elsif glfwgetkey( mainWin, glfw_key_v ) = Glfw_Press then --saveGame
		usersave:=true;
		writeState;
		savetime:=currentTime;
		oldTimeKb := currentTime;

	elsif glfwgetkey( mainWin, glfw_key_l ) = Glfw_Press then --toggleLazyCam
		lazyCam:=not lazyCam;
		lazytime:=currentTime;

		--camdist:=1.0; --default
		zoomwheel.zdist := zoomwheel.zdefault;
		camdist := float(zoomwheel.zdist);

		oldTimeKb := currentTime;

	elsif glfwgetkey( mainWin, glfw_key_m ) = Glfw_Press then --toggleAvatar
		thirdPerson:=not thirdPerson;
		oldTimeKb := currentTime;


	elsif glfwgetkey( mainWin, glfw_key_space ) = Glfw_Press then --pickDrop
		pickOrDrop;
		oldTimeKb := currentTime;





	elsif glfwgetkey( mainWin, glfw_key_x ) = Glfw_Press then --debugInfo

			put("scene="&integer'image(scene));
			put(", horiangDeg="&float'image(horiang*rad2deg));
			put_line(", me: "
				&float'image(xme)&","
				&float'image(yme)&","&float'image(zme));
			new_line;

		oldTimeKb := currentTime;


--	elsif glfwgetkey( mainWin, glfw_key_z ) = Glfw_Press then --zoomNormalize
--				if camdist>1.01 then
--					camdist := 0.99*camdist;
--				elsif camdist<0.99 then
--					camdist := 1.01*camdist;
--				end if;

--		oldTimeKb := currentTime;



	end if;

end if;

end getKeyInputs;




