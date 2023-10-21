separate(gameutils)

procedure InitGlfw( width, height : glint; name: string ) is

	use system;

	title : interfaces.c.strings.chars_ptr := new_string(name&ascii.nul);

	maj,min,rev : aliased glint;


	awidth:  aliased glint := width;
	aheight: aliased glint := height;

	azero : aliased glint := 0;

	axs, ays : aliased float;
	awwid,awhit, afwid, afhit : aliased glint;

	forcelowdpi : constant boolean := true;

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

	if osx and forcelowdpi then --prevent jerky hidpi
		GlfwWindowHint( glfw_cocoa_retina_framebuffer, glfw_false);
	end if;

	mainWindow := glfwcreatewindow(
		width, height,	title, null, null );


		

	if mainWindow = null then
		new_line;
		put_line("glfwCreateWindow failed");
		raise program_error;
	end if;

	glfwmakecontextcurrent( mainWindow );


--HiDpi queries:
	glfwGetWindowSize(mainWindow, awwid'access, awhit'access);
	glfwGetFramebufferSize(mainWindow, afwid'access,afhit'access);
	glfwGetWindowContentScale(mainWindow, axs'access,ays'access);

	put_line("HighDpi Queries:");
	put_line("WI: "&glint'image(awwid)&","&glint'image(awhit));
	put_line("FB: "&glint'image(afwid)&","&glint'image(afhit));
	put_line("Sc: "&float'image(axs)&","&float'image(ays));


	if glfwJoystickPresent(glfw_joystick_1)=glfw_true then

		if glfwJoystickIsGamepad(glfw_joystick_1)=glfw_true then
			put_line("Gamepad is present");
			gamepad:=true;
		else
			put_line("Joystick is present");
			joystik:=true;
		end if;

	end if;


end InitGlfw;



