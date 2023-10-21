separate(avent)

procedure drawAvatar( mytime : gldouble ) is

begin

	if bugstarted then direction:=2; end if;
	
	glUseProgram(pidava07);
	glUniformMatrix4fv(mvpid07, 1, GL_FALSE, imvp(1,1)'address);
	glUniform1f(hangid07, glfloat(horiang));
	glUniform1i(dirid07, glint(direction));

	if showingHand then
		glUniform1i(fadid07, 1); -- fade avatar
	else
		glUniform1i(fadid07, 0); -- no fade
	end if;

	if 
		(chapter=4 and scene>5)
		or (chapter=3 and scene>3)
	then
		glUniform1i(darkid07, 3); --dark

	elsif chapter>2 then
		glUniform1i(darkid07, 1);

	else
		glUniform1i(darkid07, 0); --normal

	end if;

	glUniform1f(timeid07, glfloat(mytime));

	glUniform3f(cenid07, 
		glfloat(xme),glfloat(yme-aheight),glfloat(zme) );

	glUniform1i(sampid07, 0);

	glbindtexture(gl_texture_2d, ava_texid);
	avatarolay.draw(ava, vertbuff,uvbuff,elembuff);

end drawAvatar;


