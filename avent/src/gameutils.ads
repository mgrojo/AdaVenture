
--
-- Copyright (C) 2023  <fastrgv@gmail.com>
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


with matutils;  use matutils;
with gametypes;  use gametypes;



with glfw3; use glfw3;


with interfaces.c;
with interfaces.c.strings;
with glext;
with glext.pointers;
with glext.binding;
with gl;
with gl.binding;
with gl.pointers;





package gameutils is


	use interfaces.c;
	use interfaces.c.strings;
	use glext;
	use glext.pointers;
	use glext.binding;
	use gl;
	use gl.binding;
	use gl.pointers;


	mainWindow : access GLFWwindow;

	procedure myassert( 
		condition : boolean;  
		flag: integer:=0;
		msg: string := "");

	-- calculate floatingPoint remainder of floor(val/ub)
	-- returns value in [0..ub]
	function fmod( val, ub : float ) return float;

	function odd( i: integer ) return boolean;

	function sqr( x: float ) return float;
	function min( x,y: float ) return float;
	function mini( i,j: integer ) return integer;



	function hordistance( x1,y1, x2,y2 : float ) return float;

	procedure moveforward( currenttime: gldouble );
	procedure movebackward( currenttime: gldouble );

	procedure first_prep(
		HiRes: boolean := false);


	procedure InitGLFW( width, height : glint; name: string );
	procedure InitGLFWfs( name: string );




	procedure updategamestate;
	procedure updateCamera(init: boolean := false);

	procedure updateMVPs( 
		et: gldouble;
		wid,hit : float; upd8: boolean:=false );

	procedure slewToAvLook;
	procedure slewToAv;




	procedure handle_mouse_move( nowTime : gldouble );

	procedure handle_gc_look(gcx,gcy,slu:float);
	procedure handle_gc_move(nowTime:gldouble; gcx,gcy:float);





	procedure liftmaze;
	procedure slidelab;
	procedure liftlion;
	procedure liftgate( n: integer );



	function land_alt( x,z : float ) return float;

	procedure sendBat;
	procedure drawbat( dt: gldouble );
	procedure drawRdragon( dt: gldouble );
	procedure drawBdragon( dt: gldouble );
	procedure drawMinotaur( dt: gldouble );
	function angl( x1,y1,z1, x2,y2,z2 : float ) return float;

	function nearfog return boolean;
	function nearwaterfall return boolean;

	function snakehiss return boolean;
	function nearsnake return boolean;

	procedure drawspider( dt: gldouble );


	procedure sort( 
		f2n: in out sortarray;  -- output permutations
		ox,oz : limarray;       -- pos of each object
		lo, hi : integer;       -- bounds of sort
		eyex,eyez : float       -- xme, zme
		);


	procedure emptyGLerrorQueue;

	function dumpGLerrorQueue(id: string) return integer;
--
-- 16#0#   =    0 = no_error
-- 16#500# = 1280 = invalid_enum
-- 16#501# = 1281 = invalid_value
-- 16#502# = 1282 = invalid_operation ?reusing uniformID?
-- 16#503# = 1283 = stack_overflow
-- 16#504# = 1284 = stack_underflow
-- 16#505# = 1285 = out_of_memory
--



	function atThreshold(	
		timenow: gldouble; 
		str: string;
		sene: integer ) return boolean;


	procedure showWhatIsHeld;


	procedure insertgate( k: integer );


	procedure updateBull( --28may18
		mytime: gldouble;
		xme,zme: float;
		xm,ym,zm,am: in out float;
		idir: out integer;
		collide: out boolean
		);
	procedure drawBull( 
		mytime: gldouble;
		xb,yb,zb,hang : float;
		idir: integer
		);

	procedure writeState( aborting: boolean := false );
	procedure readState( statefile: in string );
	procedure setState;

	procedure drawBugs( nowtime: float );
	procedure screenBugs( nowtime: float );


	procedure initializeNewMazes;

-- note transition table below

-- scene		no		so		ea									we

-- 	5		x		in		6Y(5,-10,9)->(6,+10,-4)		6C(5,10,9)->(6,-10,9)
-- 	5		x		in		6Z(5,-10,4)->(6,+10,-9)		6D(5,10,4)->(6,-10,4)

-- 	5		x		in		6V(5,-10,2)->(6,+10,2)		6E(5,+10,2)->(6,-10,2)
-- 	5		x		in		6W(5,-10,0)->(6,+10,0)		6F(5,+10,0)->(6,-10,0)
-- 	5		x		in		6X(5,-10,-2)->(6,+10,-2)	6G(5,+10,-2)->(6,-10,-2)



--		6		out	x		5C(6,-10,9)->(5,+10,9)		6A(6,+10,+9)->(6,-10,-4)
--		6		out	x		5D(6,-10,4)->(5,+10,4)		6B(6,+10,+4)->(6,-10,-9)

--		6		out	x		5E(6,-10,2)->(5,+10,2)		5V(6,+10,+2)->(5,-10,+2)
--		6		out	x		5F(6,-10,0)->(5,+10,0)		5W(6,+10,0)->(5,-10,0)
--		6		out	x		5G(6,-10,-2)->(5,+10,-2)	5X(6,+10,-2)->(5,-10,-2)

--		6		out	x		6A(6,-10,-4)->(6,+10,+9)	5Y(6,+10,-4)->(5,-10,+9)
--		6		out	x		6B(6,-10,-9)->(6,+10,+4)	5Z(6,+10,-9)->(5,-10,+4)

-- denote transition ID by (ea/we, initial-scene, letter):
--
-- {ea,we} + {5,6} + {a,b,c,d,e,f,g, v,w,x,y,z}
-- so ...
-----------------------------------------
-- ea5y: x+20, z-13
-- ea5z: x+20, z-13
-- ea5v: x+20
-- ea5w: x+20
-- ea5x: x+20
--
-- we5c: x-20
-- we5d: x-20
-- we5e: x-20
-- we5f: x-20
-- we5g: x-20
----------------------------------
-- ea6c: x+20
-- ea6d: x+20
-- ea6e: x+20
-- ea6f: x+20
-- ea6g: x+20
-- ea6a: x+20, z+13
-- ea6b: x+20, z+13
--
-- we6a: x-20, z-13
-- we6b: x-20, z-13
-- we6v: x-20
-- we6w: x-20
-- we6x: x-20
-- we6y: x-20, z+13
-- we6z: x-20, z+13


end gameutils;
