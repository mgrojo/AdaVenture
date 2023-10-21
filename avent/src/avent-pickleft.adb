separate(avent)

procedure pickLeft( item: itemtype ) is
begin

if 
	not gatewait and
	not lionwait and
	not labwait and
	not mazewait 
then

	if item=pgate then

		if swordheld then dropitem(srd);
		elsif wkeyheld then dropitem(wkee);
		elsif gkeyheld then dropitem(gkee);
		elsif bkeyheld then dropitem(bkee);
		elsif chaliceheld then dropitem(cup);
		end if;

		kgate:=0; --=> gateway inactive (on ground or held)

		gateheld:=true;
		gkeyheld:=false; 
		swordheld:=false; 
		chaliceheld:=false; 
		bkeyheld:=false;
		wkeyheld:=false;
		snd4ada.playSnd(up); --pickup


	elsif item=gkee then

		if swordheld then dropitem(srd);
		elsif gateheld then dropitem(pgate);
		elsif wkeyheld then dropitem(wkee);
		elsif bkeyheld then dropitem(bkee);
		elsif chaliceheld then dropitem(cup);
		end if;

		gkeyheld:=true; 
		swordheld:=false; 
		chaliceheld:=false; 
		bkeyheld:=false;
		wkeyheld:=false;
		gateheld:=false;
		snd4ada.playSnd(up); --pickup


	elsif item=wkee then

		if swordheld then dropitem(srd);
		elsif gateheld then dropitem(pgate);
		elsif gkeyheld then dropitem(gkee);
		elsif bkeyheld then dropitem(bkee);
		elsif chaliceheld then dropitem(cup);
		end if;

		wkeyheld:=true; swordheld:=false; chaliceheld:=false; 
		bkeyheld:=false;
		gkeyheld:=false;
		gateheld:=false;
		snd4ada.playSnd(up); --pickup


	elsif item=bkee then

		if swordheld then dropitem(srd);
		elsif gateheld then dropitem(pgate);
		elsif wkeyheld then dropitem(wkee);
		elsif gkeyheld then dropitem(gkee);
		elsif chaliceheld then dropitem(cup);
		end if;

		bkeyheld:=true; swordheld:=false; 
		chaliceheld:=false; wkeyheld:=false;
		gkeyheld:=false;
		gateheld:=false;
		snd4ada.playSnd(up); --pickup


	elsif item=srd then

		if wkeyheld then dropitem(wkee);
		elsif gateheld then dropitem(pgate);
		elsif bkeyheld then dropitem(bkee);
		elsif gkeyheld then dropitem(gkee);
		elsif chaliceheld then dropitem(cup);
		end if;

		swordheld:=true; wkeyheld:=false; 
		chaliceheld:=false; bkeyheld:=false;
		gkeyheld:=false;
		gateheld:=false;
		snd4ada.playSnd(up); --pickup

	elsif item=cup then

		if swordheld then dropitem(srd);
		elsif gateheld then dropitem(pgate);
		elsif bkeyheld then dropitem(bkee);
		elsif gkeyheld then dropitem(gkee);
		elsif wkeyheld then dropitem(wkee);
		end if;

		chaliceheld:=true; swordheld:=false; 
		wkeyheld:=false; bkeyheld:=false;
		gkeyheld:=false;
		gateheld:=false;
		chalicegone:=true; --do NOT show inside rocksafe
		drawchalice:=true; --...but DO draw it as soon as dropped

		if play9 and not stop9 then
			snd4ada.stopLoop(tmpl4);
			stop9:=true;
		elsif play4 and not stop4 then
			snd4ada.stopLoop(tmpl4);
			stop4:=true;
		end if;
		snd4ada.playSnd(up); --pickup

	end if; -- cup


end if; -- not wait

end pickLeft;



