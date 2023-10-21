separate(avent)

procedure pickOrDrop is
begin

---------------------------------------------------------------
		-- perhaps an item is being picked...

			if sgate=scene and pgatenear then --pgate
				pickLeft(pgate);

			-- green key logic:
			elsif sgkey=scene and gkeynear  then
				pickLeft(gkee);

			-- black key logic:
			elsif sbkey=scene and bkeynear  then
				pickLeft(bkee);

			-- white key logic:
			elsif swkey=scene and wkeynear  then
				pickLeft(wkee);

			-- sword logic:
			elsif ssword=scene and swordnear  then
				pickLeft(srd);

			-- chalice logic:
			elsif schalice=scene and chalicenear  then
				pickLeft(cup);


		-- or maybe an item is being dropped...


			--elsif gateheld and insertable>0 then
			elsif gateheld then
				gateheld:=false;
				dropitem(pgate);
				snd4ada.playSnd(down); --putdown


			elsif wkeyheld and not gatewait then
				wkeyheld:=false;
				dropitem(wkee);
				snd4ada.playSnd(down); --putdown


			elsif gkeyheld and not mazewait then
				gkeyheld:=false;
				dropitem(gkee);
				snd4ada.playSnd(down); --putdown

			elsif bkeyheld and not lionwait and not labwait then
				bkeyheld:=false;
				dropitem(bkee);
				snd4ada.playSnd(down); --putdown


			elsif swordheld then
				swordheld:=false;
				dropitem(srd);
				snd4ada.playSnd(down); --putdown


			elsif chaliceheld then
				chaliceheld:=false;
				dropitem(cup);
				snd4ada.playSnd(down); --putdown

				if interior and scene=2 and pedestalnear then
					xchalice:=xped;
					ychalice:=yped+hcup;
					zchalice:=zped;
					success:=true;
				end if;

			end if;
-----------------------------------------------

end pickOrDrop;



