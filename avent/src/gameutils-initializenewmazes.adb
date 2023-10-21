separate(gameutils)


procedure initializeNewMazes is
begin

	-- The maze interconnections in this game are complex.
	-- Begin by organizing the many doorways that
	-- transition within & between mazes.  This includes
	-- the ideal velocity direction required to pass thru,
	-- as well as the precise doorway locations, using
	-- coordinates PRIOR to traversal.
	-- The encoding string-keys come from "map.txt", a
	-- design document that can be found in ./mazeDesign/.
	-- The maze table is a structure using the "Tables"
	-- library of Dmitry Kazakov, that allows the
	-- association of a data record (x,z,dir) to a 
	-- given string-key.
	

	-- scene5 transitions:
	maze_table.add(mtab,"5c", (+10.0,+9.0,+halfpi));
	maze_table.add(mtab,"5y", (-10.0,+9.0,-halfpi));
	maze_table.add(mtab,"5d", (+10.0,+4.0,+halfpi));
	maze_table.add(mtab,"5z", (-10.0,+4.0,-halfpi));
	maze_table.add(mtab,"5e", (+10.0,+2.0,+halfpi));
	maze_table.add(mtab,"5v", (-10.0,+2.0,-halfpi));
	maze_table.add(mtab,"5f", (+10.0,+0.0,+halfpi));
	maze_table.add(mtab,"5w", (-10.0,+0.0,-halfpi));
	maze_table.add(mtab,"5g", (+10.0,-2.0,+halfpi));
	maze_table.add(mtab,"5x", (-10.0,-2.0,-halfpi));
	maze_table.add(mtab,"5ex", (0.0,-10.0,+onepi));

	-- scene6 transitions:
	maze_table.add(mtab,"6al", (+10.0,+9.0,+halfpi));
	maze_table.add(mtab,"6c", (-10.0,+9.0,-halfpi));
	maze_table.add(mtab,"6bl", (+10.0,+4.0,+halfpi));
	maze_table.add(mtab,"6d", (-10.0,+4.0,-halfpi));
	maze_table.add(mtab,"6v", (+10.0,+2.0,+halfpi));
	maze_table.add(mtab,"6e", (-10.0,+2.0,-halfpi));
	maze_table.add(mtab,"6w", (+10.0,+0.0,+halfpi));
	maze_table.add(mtab,"6f", (-10.0,+0.0,-halfpi));
	maze_table.add(mtab,"6x", (+10.0,-2.0,+halfpi));
	maze_table.add(mtab,"6g", (-10.0,-2.0,-halfpi));
	maze_table.add(mtab,"6y", (+10.0,-4.0,+halfpi));
	maze_table.add(mtab,"6ar", (-10.0,-4.0,-halfpi));
	maze_table.add(mtab,"6z", (+10.0,-9.0,+halfpi));
	maze_table.add(mtab,"6br", (-10.0,-9.0,-halfpi));
	maze_table.add(mtab,"6ex", (+0.0,+10.0, 0.0));


	-- scene7 transitions:
	maze_table.add(mtab,"7al", (-10.0,-9.0,-halfpi));
	maze_table.add(mtab,"7dr", (+10.0,-9.0,+halfpi));
	maze_table.add(mtab,"7bl", (-10.0,-7.0,-halfpi));
	maze_table.add(mtab,"7er", (+10.0,-7.0,+halfpi));
	maze_table.add(mtab,"7cl", (-10.0,-4.0,-halfpi));
	maze_table.add(mtab,"7fr", (+10.0,-4.0,+halfpi));
	maze_table.add(mtab,"7dl", (-10.0,-2.0,-halfpi));
	maze_table.add(mtab,"7ar", (+10.0,-2.0,+halfpi));
	maze_table.add(mtab,"7el", (-10.0, 0.0,-halfpi));
	maze_table.add(mtab,"7br", (+10.0, 0.0,+halfpi));
	maze_table.add(mtab,"7fl", (-10.0,+3.0,-halfpi));
	maze_table.add(mtab,"7cr", (+10.0,+3.0,+halfpi));
	maze_table.add(mtab,"7h", (+10.0,+7.0,+halfpi));
	maze_table.add(mtab,"7ex", (0.0,-10.0,+onepi));


	-- scene8 transitions:
	maze_table.add(mtab,"8h", (-10.0,+7.0,-halfpi));
	maze_table.add(mtab,"8g", (+10.0,+7.0,+halfpi));


	-- scene9 transitions:
	maze_table.add(mtab,"9fl", (+8.0,-10.0,+onepi));
	maze_table.add(mtab,"9cr", (+6.0,+10.0, 0.0));
	maze_table.add(mtab,"9bl", (+5.0,-10.0,+onepi));
	maze_table.add(mtab,"9al", (+1.0,-10.0,+onepi));
	maze_table.add(mtab,"9fr", (-2.0,+10.0, 0.0));
	maze_table.add(mtab,"9cl", (-4.0,-10.0,+onepi));
	maze_table.add(mtab,"9br", (-5.0,+10.0, 0.0));
	maze_table.add(mtab,"9ar", (-9.0,+10.0, 0.0));
	maze_table.add(mtab,"9dt", (+10.0,-8.0,+halfpi));
	maze_table.add(mtab,"9et", (+10.0,-1.0,+halfpi));
	maze_table.add(mtab,"9db", (-10.0,-8.0,-halfpi));
	maze_table.add(mtab,"9eb", (-10.0,-1.0,-halfpi));
	maze_table.add(mtab,"9x",  (-10.0,+7.0, -halfpi));


	-- scene2 transitions:
	maze_table.add(mtab,"2e", (-5.0,-10.0, 0.0));
	maze_table.add(mtab,"2x", (-5.0,-10.0,+onepi));

	-- scene1 transitions:
	maze_table.add(mtab,"1e", (0.0,-10.0,+onepi));
	maze_table.add(mtab,"1x", (+7.0,+4.6, 0.0));

	-- scene4 transitions:
	maze_table.add(mtab,"4e", (0.0,+10.0, 0.0));
	maze_table.add(mtab,"4x", (0.0,-10.0,+onepi));


-- Now, we initialize the array that defines the mazes:

	for im in scenerng loop
	for row in -mrows..mrows loop
	for col in -mcols..mcols loop
		--passthru(im,row,col):=false;
		iswall(im,row,col):=false;
	end loop; --col
	end loop; --row
	end loop; --im



-- begin scene #9 =======================

	iswall(9,+10,-6):=true;
	iswall(9,+10,-2):=true;
	iswall(9,+10,+5):=true;

	iswall(9,+9,-6):=true;
	iswall(9,+9,-2):=true;
	iswall(9,+9,+5):=true;

	iswall(9,+8,-6):=true;
	iswall(9,+8,-4):=true;
	iswall(9,+8,-3):=true;
	iswall(9,+8,-2):=true;
	iswall(9,+8,-1):=true;
	iswall(9,+8, 0):=true;
	iswall(9,+8,+1):=true;
	iswall(9,+8,+2):=true;
	iswall(9,+8,+3):=true;
	iswall(9,+8,+5):=true;
	iswall(9,+8,+6):=true;
	iswall(9,+8,+7):=true;
	iswall(9,+8,+8):=true;

	iswall(9,+7,-6):=true;
	iswall(9,+7,-4):=true;
	iswall(9,+7,+3):=true;
	iswall(9,+7,+8):=true;

	iswall(9,+6,-10):=true;
	iswall(9,+6,-9):=true;
	iswall(9,+6,-8):=true;
	iswall(9,+6,-7):=true;
	iswall(9,+6,-6):=true;
	iswall(9,+6,-4):=true;
	iswall(9,+6,-3):=true;
	iswall(9,+6,-2):=true;
	iswall(9,+6,-1):=true;
	iswall(9,+6, 0):=true;
	iswall(9,+6,+1):=true;
	iswall(9,+6,+3):=true;
	iswall(9,+6,+6):=true;
	iswall(9,+6,+7):=true;
	iswall(9,+6,+8):=true;

	iswall(9,+5,+3):=true;
	iswall(9,+5,+6):=true;
	iswall(9,+5,+8):=true;
	iswall(9,+5,+10):=true;

	iswall(9,+4,-8):=true;
	iswall(9,+4,-7):=true;
	iswall(9,+4,-6):=true;
	iswall(9,+4,-5):=true;
	iswall(9,+4,-4):=true;
	iswall(9,+4,-3):=true;
	iswall(9,+4,-2):=true;
	iswall(9,+4, 0):=true;
	iswall(9,+4,+3):=true;
	iswall(9,+4,+6):=true;
	iswall(9,+4,+10):=true;

	iswall(9,+3,-8):=true;
	iswall(9,+3,-2):=true;
	iswall(9,+3, 0):=true;
	iswall(9,+3,+3):=true;
	iswall(9,+3,+6):=true;

	iswall(9,+2,-8):=true;
	iswall(9,+2,-2):=true;
	iswall(9,+2, 0):=true;
	iswall(9,+2,+3):=true;
	iswall(9,+2,+6):=true;
	iswall(9,+2,+8):=true;

	iswall(9,+1,-8):=true;
	iswall(9,+1,-2):=true;
	iswall(9,+1, 0):=true;
	iswall(9,+1,+1):=true;
	iswall(9,+1,+2):=true;
	iswall(9,+1,+3):=true;
	iswall(9,+1,+6):=true;
	iswall(9,+1,+8):=true;







	iswall(9,-0,-10):=true;
	iswall(9,-0,-9):=true;
	iswall(9,-0,-8):=true;
	iswall(9,-0,-6):=true;
	iswall(9,-0,-5):=true;
	iswall(9,-0,-4):=true;
	iswall(9,-0,-2):=true;
	iswall(9,-0,+2):=true;
	iswall(9,-0,+3):=true;
	iswall(9,-0,+4):=true;
	iswall(9,-0,+6):=true;
	iswall(9,-0,+8):=true;

	iswall(9,-1,-4):=true;
	iswall(9,-1,-2):=true;
	iswall(9,-1, 0):=true;
	iswall(9,-1,+2):=true;
	iswall(9,-1,+4):=true;
	iswall(9,-1,+6):=true;
	iswall(9,-1,+8):=true;

	iswall(9,-2,-4):=true;
	iswall(9,-2, 0):=true;
	iswall(9,-2,+2):=true;
	iswall(9,-2,+4):=true;
	iswall(9,-2,+6):=true;
	iswall(9,-2,+8):=true;

	iswall(9,-3,-8):=true;
	iswall(9,-3,-7):=true;
	iswall(9,-3,-6):=true;
	iswall(9,-3,-5):=true;
	iswall(9,-3,-4):=true;
	iswall(9,-3, 0):=true;
	iswall(9,-3,+2):=true;
	iswall(9,-3,+4):=true;
	iswall(9,-3,+8):=true;
	iswall(9,-3,+9):=true;
	iswall(9,-3,+10):=true;

	iswall(9,-4,-8):=true;
	iswall(9,-4,-4):=true;
	iswall(9,-4,-2):=true;
	iswall(9,-4,-1):=true;
	iswall(9,-4, 0):=true;
	iswall(9,-4,+4):=true;
	iswall(9,-4,+5):=true;
	iswall(9,-4,+6):=true;
	iswall(9,-4,+7):=true;
	iswall(9,-4,+8):=true;
	iswall(9,-4,+9):=true;
	iswall(9,-4,+10):=true;

	iswall(9,-5,-8):=true;
	iswall(9,-5,-4):=true;
	iswall(9,-5,-2):=true;
	iswall(9,-5,+4):=true; --2jan18

	iswall(9,-6,-10):=true;
	iswall(9,-6,-9):=true;
	iswall(9,-6,-8):=true;
	iswall(9,-6,-7):=true;
	iswall(9,-6,-6):=true;
	iswall(9,-6,-5):=true;
	iswall(9,-6,-4):=true;
	iswall(9,-6,-2):=true;
	iswall(9,-6, 0):=true;
	iswall(9,-6,+1):=true;
	iswall(9,-6,+2):=true;
	iswall(9,-6,+3):=true;
	iswall(9,-6,+4):=true;
	iswall(9,-6,+5):=true;
	iswall(9,-6,+8):=true;
	iswall(9,-6,+9):=true;
	iswall(9,-6,+10):=true;

	iswall(9,-7,-2):=true;
	iswall(9,-7, 0):=true;
	iswall(9,-7,+5):=true;
	iswall(9,-7,+8):=true;
	iswall(9,-7,+10):=true;

	iswall(9,-8,-7):=true;
	iswall(9,-8,-6):=true;
	iswall(9,-8,-5):=true;
	iswall(9,-8,-4):=true;
	iswall(9,-8,-3):=true;
	iswall(9,-8,-2):=true;
	iswall(9,-8, 0):=true;
	iswall(9,-8,+2):=true;
	iswall(9,-8,+3):=true;
	iswall(9,-8,+4):=true;
	iswall(9,-8,+5):=true;
	iswall(9,-8,+8):=true;
	iswall(9,-8,+9):=true;
	iswall(9,-8,+10):=true;

	iswall(9,-9,-7):=true;
	iswall(9,-9,-2):=true;
	iswall(9,-9, 0):=true;

	iswall(9,-10,-7):=true;
	iswall(9,-10,-2):=true;
	iswall(9,-10, 0):=true;



-- end of scene #9 ==============================



-- scene # 6 ========================================================
	-- topexit ------------

	--iswall(6,10,10):=true;
	--iswall(6,9,10):=true;
	--iswall(6,8,10):=true;
	--iswall(6,7,10):=true;
	--iswall(6,6,10):=true;
	--iswall(6,5,10):=true;
	--iswall(6,4,10):=true;
	--iswall(6,3,10):=true;

	iswall(6,5,10):=true;
	iswall(6,3,10):=true;

	iswall(6,5,9):=true;
	iswall(6,3,9):=true;

	iswall(6,10,8):=true;
	iswall(6,9,8):=true;
	iswall(6,8,8):=true;
	iswall(6,7,8):=true;
	iswall(6,5,8):=true;
	iswall(6,3,8):=true;

	iswall(6,10,7):=true;
	iswall(6,7,7):=true;
	iswall(6,5,7):=true;
	iswall(6,3,7):=true;
	iswall(6,2,7):=true;

	iswall(6,10,6):=true;
	iswall(6,7,6):=true;
	iswall(6,5,6):=true;

	iswall(6,10,5):=true;
	iswall(6,9,5):=true;
	iswall(6,8,5):=true;
	iswall(6,7,5):=true;
	iswall(6,6,5):=true;
	iswall(6,5,5):=true;
	iswall(6,4,5):=true;
	iswall(6,3,5):=true;
	iswall(6,2,5):=true; --passthru(6,2,5):=true; --symmetric pos
	iswall(6,1,5):=true;

	iswall(6,5,4):=true;
	iswall(6,1,4):=true;

	iswall(6,10,3):=true;
	iswall(6,9,3):=true;
	iswall(6,8,3):=true;
	iswall(6,7,3):=true;
	iswall(6,5,3):=true;
	iswall(6,3,3):=true;
	iswall(6,2,3):=true;
	iswall(6,1,3):=true;
-------------------------- repeat negating 1st coord:

	--iswall(6,-10,10):=true;
	--iswall(6,-9,10):=true;
	--iswall(6,-8,10):=true;
	--iswall(6,-7,10):=true;
	--iswall(6,-6,10):=true;
	--iswall(6,-5,10):=true;
	--iswall(6,-4,10):=true;
	--iswall(6,-3,10):=true;

	iswall(6,-5,10):=true;
	iswall(6,-3,10):=true;

	iswall(6,-5,9):=true;
	iswall(6,-3,9):=true;

	iswall(6,-10,8):=true;
	iswall(6,-9,8):=true;
	iswall(6,-8,8):=true;
	iswall(6,-7,8):=true;
	iswall(6,-5,8):=true;
	iswall(6,-3,8):=true;

	iswall(6,-10,7):=true;
	iswall(6,-7,7):=true;
	iswall(6,-5,7):=true;
	iswall(6,-3,7):=true;
	iswall(6,-2,7):=true;

	iswall(6,-10,6):=true;
	iswall(6,-7,6):=true;
	iswall(6,-5,6):=true;

	iswall(6,-10,5):=true;
	iswall(6,-9,5):=true;
	iswall(6,-8,5):=true;
	iswall(6,-7,5):=true;
	iswall(6,-6,5):=true;
	iswall(6,-5,5):=true;
	iswall(6,-4,5):=true;
	iswall(6,-3,5):=true;
	iswall(6,-2,5):=true;  --passthru(6,-2,5):=true; --original pos
	iswall(6,-1,5):=true;

	iswall(6,-5,4):=true;
	iswall(6,-1,4):=true;

	iswall(6,-10,3):=true;
	iswall(6,-9,3):=true;
	iswall(6,-8,3):=true;
	iswall(6,-7,3):=true;
	iswall(6,-5,3):=true;
	iswall(6,-3,3):=true;
	iswall(6,-2,3):=true;
	iswall(6,-1,3):=true;

--==========================================

	-- mid -------------------------------------

	iswall(6,10,3):=true;
	iswall(6,9,3):=true;
	iswall(6,8,3):=true;
	iswall(6,7,3):=true;
	iswall(6,5,3):=true;
	iswall(6,3,3):=true;
	iswall(6,2,3):=true;
	iswall(6,1,3):=true;

	iswall(6,5,2):=true;
	iswall(6,2,2):=true;
	iswall(6,1,2):=true;

	iswall(6,10,1):=true;
	iswall(6,9,1):=true;
	iswall(6,8,1):=true;
	iswall(6,7,1):=true;
	iswall(6,6,1):=true;
	iswall(6,5,1):=true;
	iswall(6,4,1):=true;
	iswall(6,2,1):=true;
	iswall(6,1,1):=true;

	iswall(6,4,0):=true;
	iswall(6,2,0):=true;
	iswall(6,1,0):=true;

	iswall(6,10,-1):=true;
	iswall(6,9,-1):=true;
	iswall(6,8,-1):=true;
	iswall(6,6,-1):=true;
	iswall(6,4,-1):=true;
	iswall(6,2,-1):=true;
	iswall(6,1,-1):=true;

	iswall(6,6,-2):=true;
	iswall(6,4,-2):=true;
	iswall(6,2,-2):=true;

	iswall(6,10,-3):=true;
	iswall(6,9,-3):=true;
	iswall(6,8,-3):=true;
	iswall(6,7,-3):=true;
	iswall(6,6,-3):=true;
	iswall(6,4,-3):=true;
	iswall(6,2,-3):=true;

-------------------------- repeat negating 1st coord:

	iswall(6,-10,3):=true;
	iswall(6,-9,3):=true;
	iswall(6,-8,3):=true;
	iswall(6,-7,3):=true;
	iswall(6,-5,3):=true;
	iswall(6,-3,3):=true;
	iswall(6,-2,3):=true;
	iswall(6,-1,3):=true;

	iswall(6,-5,2):=true;
	iswall(6,-2,2):=true;
	iswall(6,-1,2):=true;

	iswall(6,-10,1):=true;
	iswall(6,-9,1):=true;
	iswall(6,-8,1):=true;
	iswall(6,-7,1):=true;
	iswall(6,-6,1):=true;
	iswall(6,-5,1):=true;
	iswall(6,-4,1):=true;
	iswall(6,-2,1):=true;
	iswall(6,-1,1):=true;

	iswall(6,-4,0):=true;
	iswall(6,-2,0):=true;
	iswall(6,-1,0):=true;

	iswall(6,-10,-1):=true;
	iswall(6,-9,-1):=true;
	iswall(6,-8,-1):=true;
	iswall(6,-6,-1):=true;
	iswall(6,-4,-1):=true;
	iswall(6,-2,-1):=true;
	iswall(6,-1,-1):=true;

	iswall(6,-6,-2):=true;
	iswall(6,-4,-2):=true;
	iswall(6,-2,-2):=true;

	iswall(6,-10,-3):=true;
	iswall(6,-9,-3):=true;
	iswall(6,-8,-3):=true;
	iswall(6,-7,-3):=true;
	iswall(6,-6,-3):=true;
	iswall(6,-4,-3):=true;
	iswall(6,-2,-3):=true;



--==========================================

	-- bot -------------------------------------

	iswall(6,10,-3):=true;
	iswall(6,9,-3):=true;
	iswall(6,8,-3):=true;
	iswall(6,7,-3):=true;
	iswall(6,6,-3):=true;
	iswall(6,4,-3):=true;
	iswall(6,2,-3):=true;

	iswall(6,6,-4):=true;
	iswall(6,2,-4):=true;

	iswall(6,10,-5):=true;
	iswall(6,9,-5):=true;
	iswall(6,8,-5):=true;
	iswall(6,6,-5):=true;
	iswall(6,5,-5):=true;
	iswall(6,4,-5):=true;
	iswall(6,3,-5):=true;
	iswall(6,2,-5):=true;

	iswall(6,8,-6):=true;

	iswall(6,8,-7):=true;
	iswall(6,7,-7):=true;
	iswall(6,6,-7):=true;

	iswall(6,10,-8):=true;
	iswall(6,9,-8):=true;
	iswall(6,8,-8):=true;
	iswall(6,6,-8):=true;

	iswall(6,6,-9):=true;

	iswall(6,6,-10):=true;

	--iswall(6,10,-10):=true;
	--iswall(6,9,-10):=true;
	--iswall(6,8,-10):=true;
	--iswall(6,7,-10):=true;
	--iswall(6,6,-10):=true;
	--iswall(6,5,-10):=true;
	--iswall(6,4,-10):=true;
	--iswall(6,3,-10):=true;
	--iswall(6,2,-10):=true;
	--iswall(6,1,-10):=true;
	--iswall(6,0,-10):=true;

-------------------------- repeat negating 1st coord:

	iswall(6,-10,-3):=true;
	iswall(6,-9,-3):=true;
	iswall(6,-8,-3):=true;
	iswall(6,-7,-3):=true;
	iswall(6,-6,-3):=true;
	iswall(6,-4,-3):=true;
	iswall(6,-2,-3):=true;

	iswall(6,-6,-4):=true;
	iswall(6,-2,-4):=true;

	iswall(6,-10,-5):=true;
	iswall(6,-9,-5):=true;
	iswall(6,-8,-5):=true;
	iswall(6,-6,-5):=true;
	iswall(6,-5,-5):=true;
	iswall(6,-4,-5):=true;
	iswall(6,-3,-5):=true;
	iswall(6,-2,-5):=true;

	iswall(6,-8,-6):=true;

	iswall(6,-8,-7):=true;
	iswall(6,-7,-7):=true;
	iswall(6,-6,-7):=true;

	iswall(6,-10,-8):=true;
	iswall(6,-9,-8):=true;
	iswall(6,-8,-8):=true;
	iswall(6,-6,-8):=true;

	iswall(6,-6,-9):=true;

	iswall(6,-6,-10):=true;

--=========== done with scene #6;  begin scene #5 ================

-- midLR = maze entry @ bottom = Zmin


	iswall(5,10,-1):=true;
	iswall(5,9,-1):=true;
	iswall(5,8,-1):=true;
	iswall(5,7,-1):=true;
	iswall(5,6,-1):=true;

	iswall(5,4,-1):=true;
	iswall(5,3,-1):=true;
	iswall(5,2,-1):=true;
	iswall(5,1,-1):=true;
	iswall(5,0,-1):=true;

	iswall(5,6,0):=true;
	iswall(5,4,0):=true;

	iswall(5,10,1):=true;
	iswall(5,9,1):=true;
	iswall(5,8,1):=true;
	iswall(5,6,1):=true;
	iswall(5,4,1):=true;
	iswall(5,2,1):=true;
iswall(5,1,1):=true; --2jul19
	iswall(5,0,1):=true;

	iswall(5,6,2):=true;
	iswall(5,4,2):=true;
	iswall(5,2,2):=true;

	iswall(5,10,3):=true;
	iswall(5,9,3):=true;
	iswall(5,8,3):=true;
	iswall(5,7,3):=true;
	iswall(5,6,3):=true;
	iswall(5,4,3):=true;
	iswall(5,2,3):=true;
	iswall(5,0,3):=true;

-------------------------- repeat negating 1st coord:

	iswall(5,-10,-1):=true;
	iswall(5,-9,-1):=true;
	iswall(5,-8,-1):=true;
	iswall(5,-7,-1):=true;
	iswall(5,-6,-1):=true;

	iswall(5,-4,-1):=true;
	iswall(5,-3,-1):=true;
	iswall(5,-2,-1):=true;
	iswall(5,-1,-1):=true;

	iswall(5,-6,0):=true;
	iswall(5,-4,0):=true;

	iswall(5,-10,1):=true;
	iswall(5,-9,1):=true;
	iswall(5,-8,1):=true;
	iswall(5,-6,1):=true;
	iswall(5,-4,1):=true;
	iswall(5,-2,1):=true;
iswall(5,-1,1):=true; --2jul19

	iswall(5,-6,2):=true;
	iswall(5,-4,2):=true;
	iswall(5,-2,2):=true;

	iswall(5,-10,3):=true;
	iswall(5,-9,3):=true;
	iswall(5,-8,3):=true;
	iswall(5,-7,3):=true;
	iswall(5,-6,3):=true;
	iswall(5,-4,3):=true;
	iswall(5,-2,3):=true;



--==========================================

	-- botL-exitR -------------------------------------

	iswall(5,10,8):=true;
	iswall(5,9,8):=true;
	iswall(5,8,8):=true;
	iswall(5,7,8):=true;
	iswall(5,6,8):=true;
	iswall(5,5,8):=true;
	iswall(5,3,8):=true;
	iswall(5,2,8):=true;
	iswall(5,1,8):=true;
	iswall(5,0,8):=true;

	iswall(5,10,7):=true;
	iswall(5,8,7):=true;
	iswall(5,0,7):=true;

	iswall(5,10,6):=true;
	iswall(5,8,6):=true;
	iswall(5,6,6):=true;
	iswall(5,5,6):=true;
	iswall(5,4,6):=true;
	iswall(5,3,6):=true;
	iswall(5,2,6):=true;
	iswall(5,0,6):=true;

	iswall(5,10,5):=true;
	iswall(5,9,5):=true;
	iswall(5,8,5):=true;
	iswall(5,6,5):=true;
	iswall(5,2,5):=true;
	iswall(5,0,5):=true;

	iswall(5,6,4):=true;
	iswall(5,4,4):=true;
	iswall(5,2,4):=true;
	iswall(5,0,4):=true;


-------------------------- repeat negating 1st coord:


	iswall(5,-10,8):=true;
	iswall(5,-9,8):=true;
	iswall(5,-8,8):=true;
	iswall(5,-7,8):=true;
	iswall(5,-6,8):=true;
	iswall(5,-5,8):=true;
	iswall(5,-3,8):=true;
	iswall(5,-2,8):=true;
	iswall(5,-1,8):=true;

	iswall(5,-10,7):=true;
	iswall(5,-8,7):=true;

	iswall(5,-10,6):=true;
	iswall(5,-8,6):=true;
	iswall(5,-6,6):=true;
	iswall(5,-5,6):=true;
	iswall(5,-4,6):=true;
	iswall(5,-3,6):=true;
	iswall(5,-2,6):=true;

	iswall(5,-10,5):=true;
	iswall(5,-9,5):=true;
	iswall(5,-8,5):=true;
	iswall(5,-6,5):=true;
	iswall(5,-2,5):=true;

	iswall(5,-6,4):=true;
	iswall(5,-4,4):=true;
	iswall(5,-2,4):=true;








------------- begin maze #7 = orange maze to white castle -------------

----- warning:  x,z coords here are interchanged ------------

-- 1st define left half of scene7.txt:


	iswall(7,-10,-7):=true; -- z=-10, x=-7 => (-7,-10)
	iswall(7,-9,-7):=true; --(-7,-9)

	iswall(7,-8,-0):=true; --( 0,-8)
	iswall(7,-8,-1):=true; --(-1,-8)
	iswall(7,-8,-2):=true; --(-2,-8)
	iswall(7,-8,-3):=true;
	iswall(7,-8,-4):=true;
	iswall(7,-8,-7):=true;
	iswall(7,-8,-9):=true;
	iswall(7,-8,-10):=true;

	iswall(7,-7,0):=true;
	iswall(7,-7,-7):=true;

	iswall(7,-6,-0):=true;
	iswall(7,-6,-4):=true;
	iswall(7,-6,-5):=true;
	iswall(7,-6,-7):=true;
	iswall(7,-6,-8):=true;
	iswall(7,-6,-9):=true;
	iswall(7,-6,-10):=true;

	iswall(7,-5,-0):=true; --( 0,-5)
	iswall(7,-5,-4):=true; --(-4,-5)
	iswall(7,-5,-5):=true; --(-5,-5)
	iswall(7,-5,-7):=true;
	iswall(7,-5,-8):=true;
	iswall(7,-5,-9):=true;
	iswall(7,-5,-10):=true;

	iswall(7,-4,-0):=true; --( 0,-4)
	iswall(7,-4,-4):=true; --(-4,-4)
	iswall(7,-4,-5):=true; --(-5,-4)


	iswall(7,-3,-0):=true; --( 0,-3)
	iswall(7,-3,-2):=true; --(-2,-3)
	iswall(7,-3,-4):=true;
	iswall(7,-3,-5):=true;
	iswall(7,-3,-6):=true;
	iswall(7,-3,-7):=true;
	iswall(7,-3,-8):=true;
	iswall(7,-3,-9):=true;
	iswall(7,-3,-10):=true;

	iswall(7,-2,0):=true;
	iswall(7,-2,-2):=true;

	iswall(7,-1,-0):=true;
	iswall(7,-1,-2):=true;
	iswall(7,-1,-3):=true;
	iswall(7,-1,-4):=true;
	iswall(7,-1,-8):=true;
	iswall(7,-1,-9):=true;
	iswall(7,-1,-10):=true;

	iswall(7,0,-4):=true; --last of upper half

	iswall(7,1,0):=true;
	iswall(7,1,-1):=true;
	iswall(7,1,-2):=true;
	iswall(7,1,-4):=true;
	iswall(7,1,-6):=true;
	iswall(7,1,-7):=true;
	iswall(7,1,-8):=true;
	iswall(7,1,-9):=true;
	iswall(7,1,-10):=true;

	iswall(7,2,-2):=true;
	iswall(7,2,-4):=true;
	iswall(7,2,-6):=true;
	iswall(7,2,-7):=true;
	iswall(7,2,-8):=true;
	iswall(7,2,-9):=true;
	iswall(7,2,-10):=true;

	iswall(7,3,0):=true;
	iswall(7,3,-2):=true;
	iswall(7,3,-4):=true;
	iswall(7,3,-6):=true;

	iswall(7,4,0):=true;
	iswall(7,4,-2):=true;
	iswall(7,4,-4):=true;
	iswall(7,4,-6):=true;
	iswall(7,4,-8):=true;
	iswall(7,4,-9):=true;
	iswall(7,4,-10):=true;

	iswall(7,5,0):=true;
	iswall(7,5,-2):=true;
	iswall(7,5,-6):=true;
	iswall(7,5,-8):=true;
	iswall(7,5,-9):=true;
	iswall(7,5,-10):=true;


	iswall(7,6,0):=true;
	iswall(7,6,-2):=true;
	iswall(7,6,-3):=true;
	iswall(7,6,-4):=true;
	iswall(7,6,-5):=true;
	iswall(7,6,-6):=true;

	iswall(7,7,0):=true;

	iswall(7,8,0):=true;
	iswall(7,8,-2):=true;
	iswall(7,8,-3):=true;
	iswall(7,8,-4):=true;
iswall(7,8,-5):=true; --3jul19
	iswall(7,8,-6):=true;


	iswall(7,9,0):=true;
	iswall(7,9,-6):=true;

	iswall(7,10, 0):=true;
	iswall(7,10,-6):=true;


------ now, repeat maze#7 but negating 3rd coord ----------------------


	iswall(7,-10,7):=true;

	iswall(7,-9,7):=true;

	iswall(7,-8,1):=true;
	iswall(7,-8,2):=true;
	iswall(7,-8,3):=true;
	iswall(7,-8,4):=true;
	iswall(7,-8,7):=true;
	iswall(7,-8,9):=true;
	iswall(7,-8,10):=true;

	iswall(7,-7,7):=true;

	iswall(7,-6,4):=true;
	iswall(7,-6,5):=true;
	iswall(7,-6,7):=true;
	iswall(7,-6,8):=true;
	iswall(7,-6,9):=true;
	iswall(7,-6,10):=true;

	iswall(7,-5,4):=true;
	iswall(7,-5,5):=true;
	iswall(7,-5,7):=true;
	iswall(7,-5,8):=true;
	iswall(7,-5,9):=true;
	iswall(7,-5,10):=true;

	iswall(7,-4,4):=true;
	iswall(7,-4,5):=true;

	iswall(7,-3,2):=true;
	iswall(7,-3,4):=true;
	iswall(7,-3,5):=true;
	iswall(7,-3,6):=true;
	iswall(7,-3,7):=true;
	iswall(7,-3,8):=true;
	iswall(7,-3,9):=true;
	iswall(7,-3,10):=true;

	iswall(7,-2,2):=true;

	iswall(7,-1,2):=true;
	iswall(7,-1,3):=true;
	iswall(7,-1,4):=true;
	iswall(7,-1,8):=true;
	iswall(7,-1,9):=true;
	iswall(7,-1,10):=true;

	iswall(7,0,4):=true; --last of upper half

	iswall(7,1,1):=true;
	iswall(7,1,2):=true;
	iswall(7,1,4):=true;
	iswall(7,1,6):=true;
	iswall(7,1,7):=true;
	iswall(7,1,8):=true;
	iswall(7,1,9):=true;
	iswall(7,1,10):=true;

	iswall(7,2,2):=true;
	iswall(7,2,4):=true;
	iswall(7,2,6):=true;
	iswall(7,2,7):=true;
	iswall(7,2,8):=true;
	iswall(7,2,9):=true;
	iswall(7,2,10):=true;

	iswall(7,3,2):=true;
	iswall(7,3,4):=true;
	iswall(7,3,6):=true;

	iswall(7,4,2):=true;
	iswall(7,4,4):=true;
	iswall(7,4,6):=true;
	iswall(7,4,8):=true;
	iswall(7,4,9):=true;
	iswall(7,4,10):=true;

	iswall(7,5,2):=true;
	iswall(7,5,6):=true;
	iswall(7,5,8):=true;
	iswall(7,5,9):=true;
	iswall(7,5,10):=true;


	iswall(7,6,2):=true;
	iswall(7,6,3):=true;
	iswall(7,6,4):=true;
	iswall(7,6,5):=true;
	iswall(7,6,6):=true;


	iswall(7,8,2):=true;
	iswall(7,8,3):=true;
	iswall(7,8,4):=true;
iswall(7,8,5):=true; --3jul19
	iswall(7,8,6):=true;


	iswall(7,9,6):=true;

	iswall(7,10,6):=true;


-------- scene#7 transitions (letter,z,x) ------------------------
-- (-9,-10)  7A  (-2,+10)		dxme:=+(20+step)/-(20+step);
-- (-7,-10)  7B  ( 0,+10)
-- (-4,-10)  7C  (+3,+10)
-- (-2,-10)  7D  (-9,+10)
-- ( 0,-10)  7E  (-7,+10)
-- (+3,-10)  7F  (-4,+10)

-- entry @ (-10,0)
-- exit  @ (+7,-10) to room8 @ (7,+10)
-- exit  @ (+7,+10) to room8 @ (7,-10)
-------------------------------------------



------------ begin scene #8 labyrinth ---------------------------
--
-- entry @ (x,z)=(-10,7) & (+10,7)  minotaur@(0,-1)=center
-----------------------------------------------------------

-- note:  these x,z coords are NOT reversed

	-- -X wall:
	iswall(8,-8,7):=true;

	iswall(8,-8,-8):=true;
	iswall(8,-8,-7):=true;
	iswall(8,-8,-6):=true;
	iswall(8,-8,-5):=true;
	iswall(8,-8,-4):=true;
	iswall(8,-8,-3):=true;
	iswall(8,-8,-2):=true;
	iswall(8,-8,-1):=true; --passthru(8,-8,-1):=true;
	iswall(8,-8,0):=true;
	iswall(8,-8,1):=true;
	iswall(8,-8,2):=true;
	iswall(8,-8,3):=true;
	iswall(8,-8,4):=true;

	iswall(8,-6,-6):=true;
	iswall(8,-6,-5):=true;
	iswall(8,-6,-4):=true;
	iswall(8,-6,-3):=true;
	iswall(8,-6,-2):=true;
	iswall(8,-6,-1):=true;
	iswall(8,-6,0):=true;
	iswall(8,-6,1):=true;
	iswall(8,-6,2):=true;


	-- +X wall:
	iswall(8,6,-6):=true;
	iswall(8,6,-5):=true;
	iswall(8,6,-4):=true;
	iswall(8,6,-3):=true;
	iswall(8,6,-2):=true;
	iswall(8,6,-1):=true;
	iswall(8,6,0):=true;
	iswall(8,6,1):=true;
	iswall(8,6,2):=true;
	iswall(8,6,3):=true;
	iswall(8,6,4):=true;
	iswall(8,6,5):=true;
	iswall(8,6,6):=true;
	iswall(8,6,7):=true;
	iswall(8,6,8):=true;


	iswall(8,8,-8):=true;
	iswall(8,8,-7):=true;
	iswall(8,8,-6):=true;
	iswall(8,8,-5):=true;
	iswall(8,8,-4):=true;
	iswall(8,8,-3):=true;
	iswall(8,8,-2):=true;
	iswall(8,8,-1):=true;
	iswall(8,8,0):=true;
	iswall(8,8,1):=true;
	iswall(8,8,2):=true;
	iswall(8,8,3):=true;
	iswall(8,8,4):=true;
	iswall(8,8,5):=true;
	iswall(8,8,6):=true;
	iswall(8,8,7):=true;
	iswall(8,8,8):=true;
	iswall(8,8,9):=true;
	iswall(8,8,10):=true;

----- end vertical walls;  begin horizontal -------------

	iswall(8,-8,-8):=true;
	iswall(8,-7,-8):=true;
	iswall(8,-6,-8):=true;
	iswall(8,-5,-8):=true;
	iswall(8,-4,-8):=true;
	iswall(8,-3,-8):=true;
	iswall(8,-2,-8):=true;
	iswall(8,-1,-8):=true;
	iswall(8,0,-8):=true;
	iswall(8,1,-8):=true;
	iswall(8,2,-8):=true;
	iswall(8,3,-8):=true;
	iswall(8,4,-8):=true;
	iswall(8,5,-8):=true;
	iswall(8,6,-8):=true;
	iswall(8,7,-8):=true;
	iswall(8,8,-8):=true;

	iswall(8,-6,-6):=true;
	iswall(8,-5,-6):=true;
	iswall(8,-4,-6):=true;
	iswall(8,-3,-6):=true;
	iswall(8,-2,-6):=true;
	iswall(8,-1,-6):=true;
	iswall(8,0,-6):=true;
	iswall(8,1,-6):=true;
	iswall(8,2,-6):=true;
	iswall(8,3,-6):=true;
	iswall(8,4,-6):=true;
	iswall(8,5,-6):=true;
	iswall(8,6,-6):=true;


	iswall(8,-8,4):=true;
	iswall(8,-7,4):=true;
	iswall(8,-6,4):=true;
	iswall(8,-5,4):=true;
	iswall(8,-4,4):=true;
	iswall(8,-3,4):=true;
	iswall(8,-2,4):=true;
	iswall(8,-1,4):=true;
	iswall(8, 0,4):=true;
	iswall(8, 1,4):=true;
	iswall(8, 2,4):=true;
	iswall(8, 3,4):=true;
	iswall(8, 4,4):=true;


	iswall(8,-10,6):=true;
	iswall(8,-9,6):=true;
	iswall(8,-8,6):=true;
	iswall(8,-7,6):=true;
	iswall(8,-6,6):=true;
	iswall(8,-5,6):=true;
	iswall(8,-4,6):=true;
	iswall(8,-3,6):=true;
	iswall(8,-2,6):=true;
	iswall(8,-1,6):=true;
	iswall(8, 0,6):=true;
	iswall(8, 1,6):=true;
	iswall(8, 2,6):=true;
	iswall(8, 3,6):=true;
	iswall(8, 4,6):=true;
	iswall(8, 5,6):=true;
	iswall(8, 6,6):=true;


	iswall(8,-8,8):=true;
	iswall(8,-7,8):=true;
	iswall(8,-6,8):=true;
	iswall(8,-5,8):=true;
	iswall(8,-4,8):=true;
	iswall(8,-3,8):=true;
	iswall(8,-2,8):=true;
	iswall(8,-1,8):=true;
	iswall(8, 0,8):=true;
	iswall(8, 1,8):=true;
	iswall(8, 2,8):=true;
	iswall(8, 3,8):=true;
	iswall(8, 4,8):=true;
	iswall(8, 5,8):=true;
	iswall(8, 6,8):=true;

	iswall(8, 8,8):=true;
	iswall(8, 9,8):=true;
	iswall(8,10,8):=true;




end initializeNewMazes;


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





