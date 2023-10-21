separate(gameutils)

procedure InitGlfwFs( name: string ) is

	use system;
	use ada.strings.fixed;

	title : interfaces.c.strings.chars_ptr := new_string(name&ascii.nul);

	refrate, maj,min,rev : aliased glint;

begin

	put_line("...using fastrgv's Ada Binding to GLFW3...");

	GlfwGetVersion(maj'access,min'access,rev'access); --naturals
	put("GLFW ver: ");
	put(glint'image(maj));
	put(":"&glint'image(min));
	put(":"&glint'image(rev));
	New_Line;



	if GlfwInit /= gl_true then
		new_line;
		put_line("glfwInit failed");
		raise program_error;
	end if;

	-- use version here that your graphics card would support:
	GlfwWindowHint( glfw_context_version_major, 3);
	GlfwWindowHint( glfw_context_version_minor, 3);
	GlfwWindowHint( glfw_opengl_forward_compat, gl_true);
	GlfwWindowHint( glfw_opengl_profile, glfw_opengl_core_profile);

	GlfwWindowHint( glfw_samples, 4);
	GlfwWindowHint( glfw_client_api, glfw_opengl_api);




	declare
		monit: access glfwMonitor := glfwGetPrimaryMonitor;
		vmode: access constant glfwVidMode 
			:= glfwGetVideoMode(monit);
	begin

		if monit=null then 
		put_line("null monitor");
		else 
		put_line("NoN-null monitor"); --mint
		end if;

		--get current video mode size [full screen]
		refrate:=vmode.refreshRate;

		mainWindow := glfwcreatewindow(
			vmode.width, vmode.height,
			title, monit, null );

	end;

	if mainWindow = null then
		new_line;
		put_line("glfwCreateWindow failed");
		raise program_error;
	end if;

	glfwmakecontextcurrent( mainWindow );

	--delay 1.0; --X11 needs extra time to set window before queries


	if glfwJoystickPresent(glfw_joystick_1)=glfw_true then

		if glfwJoystickIsGamepad(glfw_joystick_1)=glfw_true then
			put_line("Gamepad is present");
			gamepad:=true;
		else
			put_line("Joystick is present");
			joystik:=true;
		end if;

	end if;

	glfwSetInputMode(mainWindow, glfw_cursor, glfw_cursor_disabled);

	if    linux then put_line("system is linux");
	elsif mswin then put_line("system is MsWin");
	--elsif   osx then put_line("system is OSX");
	end if;

end InitGlfwFs;





