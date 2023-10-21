
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

with gl;  use gl;
with ada.finalization;
with unchecked_deallocation;


generic nsides, nradials : integer;

package ellipsoid is 

type eloid is tagged private;

procedure seteli( eli: eloid; rx,ry,rz: float );  --TxCen==>Npols
procedure mseteli( eli: eloid; rx,ry,rz: float ); --equirect +Z=Npole
procedure fseteli( eli: eloid; rx,ry,rz: float ); --face map
procedure yseteli( eli: eloid; rx,ry,rz: float ); --equirect +Y=Npole

procedure setPos( eli: in out eloid; x,y,z: float);
procedure getPos( eli: eloid; x,y,z: out float);


procedure ldraw( eli: eloid; vertbuff, uvbuff, normbuff, elembuff : gluint );
procedure draw( eli: eloid; vertbuff, uvbuff, elembuff : gluint );

private


nfaces: constant integer := nsides*nradials; --
nvert : constant integer := nfaces*4*3; --
nuv   : constant integer := nfaces*2*4; --
nelm  : constant integer := nfaces*2*3; --

type varray is array(1..nvert) of float;
type vap is access varray;

type tarray is array(1..nuv) of float;
type tap is access tarray;

type earray is array(1..nelm) of glushort;
type eap is access earray;

type eloid is new ada.finalization.controlled with record
	vert,norm : vap;
	txuv : tap;
	elem : eap;
	oldx, oldy, oldz : float := 0.0;
end record;


procedure initialize(eli: in out eloid);
procedure finalize(eli: in out eloid);

end ellipsoid;

