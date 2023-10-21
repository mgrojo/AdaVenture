separate(avent)


procedure handle_mouse_click( nowTime : gldouble ) is

	msr,msl: glint;
	dt : gldouble := nowTime - pltime;

begin

	if dt>pickdwell then

		msr := glfwGetMouseButton(mainWindow, glfw_mouse_button_2); -- ?3?
		msl := glfwGetMouseButton(mainWindow, glfw_mouse_button_1);
		-- msl,msr: glint;

		if
			msl=glfw_press or msr=glfw_press
		then

			pickOrDrop;
			pltime:=nowtime;

		end if;

	end if;

end handle_mouse_click;


