separate(gameutils)

procedure handle_gc_look( gcx,gcy,slu:float ) is
	ux : float := gcx; --right_x gamepad axis
	uy : float := gcy; --right_y gamepad axis
begin

	if abs(ux)<0.05 then ux:=0.0; end if;
	if abs(uy)<0.05 then uy:=0.0; end if;

	horiAng := horiAng - 0.02 *slu* ux; --need better response
	vertAng := vertAng + 0.01 *slu* uy;

	slewToAv;

	xlook := fmath.cos(vertAng)*fmath.sin(horiAng);
	zlook := fmath.cos(vertAng)*fmath.cos(horiAng);
	ylook := fmath.sin(vertAng);

	updategamestate;

end handle_gc_look;


