separate(gameutils)


procedure handle_gc_move( nowtime: gldouble; gcx,gcy:float ) is
	ux : float := gcx; --left_x gamepad axis
	uy : float := gcy; --left_y gamepad axis
begin

	if    uy < -0.05 then
		moveforward(nowTime);

	elsif uy > +0.05 then
		movebackward(nowTime);

	end if;

	handle_gc_look(ux,0.0,gslu); -- turns left/right

end handle_gc_move;



