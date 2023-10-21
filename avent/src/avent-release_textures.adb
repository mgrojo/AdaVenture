
separate(avent)






procedure release_textures is -- prepare to close down
begin

	glext.binding.glDeleteBuffers(1, vertbuff'address);
	glext.binding.glDeleteBuffers(1, normbuff'address);
	glext.binding.glDeleteBuffers(1, rgbbuff'address);
	glext.binding.glDeleteBuffers(1, uvbuff'address);
	glext.binding.glDeleteBuffers(1, elembuff'address);
	glext.binding.glDeleteVertexArrays(1, vertexarrayid'address);



------- begin copper ---------------------------------
	glext.binding.glDeleteProgram( pidzpm34a );
	gldeletetextures(1, albedo34a'address);
	gldeletetextures(1, normal34a'address);
	gldeletetextures(1, metallic34a'address);
	gldeletetextures(1, roughness34a'address);
	--gldeletetextures(1, ao34a'address);
	gldeletetextures(1, albedo34b'address);
	gldeletetextures(1, normal34b'address);
	gldeletetextures(1, metallic34b'address);
	gldeletetextures(1, roughness34b'address);
------- end copper ---------------------------------



	glext.binding.glDeleteProgram( pidskyb01 );
	glext.binding.glDeleteProgram( pidterra02 );
	glext.binding.glDeleteProgram( pidfire03 );
	glext.binding.glDeleteProgram( pidstar04 );
	glext.binding.glDeleteProgram( pidtex05 );
	glext.binding.glDeleteProgram( pidcup06 );
	glext.binding.glDeleteProgram( pidava07 );
	glext.binding.glDeleteProgram( pidtree08 );
	glext.binding.glDeleteProgram( pidhole09 );
	glext.binding.glDeleteProgram( pidsky10 );
	glext.binding.glDeleteProgram( pidsky11 );
	glext.binding.glDeleteProgram( pidsnake12 );
	glext.binding.glDeleteProgram( pidpool13 );

----------------------------------------------------------------
	gldeletetextures(1, darkcubemap_texid'address);
	gldeletetextures(1, cloudycubemap_texid'address);
	gldeletetextures(1, sunnycubemap_texid'address);
	gldeletetextures(1, mooncubemap_texid'address);

----------------------------------------------------------------
	gldeletetextures(1, ava_texid'address);
	gldeletetextures(1, adobe_texid'address);

	gldeletetextures(1, bmarble_texid'address);
	gldeletetextures(1, bkey_texid'address);
	gldeletetextures(1, bat1_texid'address);
	gldeletetextures(1, bat2_texid'address);
	gldeletetextures(1, ball_texid'address);

	gldeletetextures(1, ceil_texid'address);
	gldeletetextures(1, cherry_texid'address);
	gldeletetextures(1, chalice_texid'address);

	gldeletetextures(1, deadbdragon_texid'address);
	gldeletetextures(1, deadrdragon_texid'address);
	gldeletetextures(1, deadminotaur_texid'address);

	gldeletetextures(1, dmaze_texid'address);
	gldeletetextures(1, doort_texid'address);
	gldeletetextures(1, doortw_texid'address);

	gldeletetextures(1, exit_texid'address);

	gldeletetextures(1, floor_texid'address);
	gldeletetextures(1, frame_texid'address);

	gldeletetextures(1, gmarble_texid'address);
	gldeletetextures(1, gkey_texid'address);
	gldeletetextures(1, gleaves_texid'address);
	gldeletetextures(1, greekey_texid'address);
	gldeletetextures(1, gate_texid'address);
	gldeletetextures(1, grass_texid'address);
	gldeletetextures(1, grass1_texid'address);

	gldeletetextures(1, hedge_texid'address);

	gldeletetextures(1, jup_texid'address);

	gldeletetextures(1, keyhole_texid'address);
	gldeletetextures(1, korla_texid'address);
	gldeletetextures(1, kevin_texid'address);
	gldeletetextures(1, key_texid'address);

	gldeletetextures(1, lab_texid'address);
	gldeletetextures(1, lion_texid'address);

	gldeletetextures(1, moss_texid'address);
	gldeletetextures(1, mural_texid'address);
	gldeletetextures(1, mazex_texid'address);
	gldeletetextures(1, mazext_texid'address);
	gldeletetextures(1, mazeouter_texid'address);
	gldeletetextures(1, moorwall_texid'address);

	gldeletetextures(1, roots_texid'address);
	gldeletetextures(1, room_texid'address);
	gldeletetextures(1, rug_texid'address);

	gldeletetextures(1, stone_texid'address);
	gldeletetextures(1, sword_texid'address);
	gldeletetextures(1, slime_texid'address);
	gldeletetextures(1, sand_texid'address);
	gldeletetextures(1, spalm_texid'address);
	gldeletetextures(1, snake_texid'address);

	gldeletetextures(1, tpalm_texid'address);
	gldeletetextures(1, tree1_texid'address);
	gldeletetextures(1, tree2_texid'address);
	gldeletetextures(1, tree3_texid'address);
	gldeletetextures(1, tree4_texid'address);
	gldeletetextures(1, tree5_texid'address);
	gldeletetextures(1, tree8_texid'address);

	gldeletetextures(1, wood_texid'address);

	gldeletetextures(1, zoro_texid'address);

end release_textures;


