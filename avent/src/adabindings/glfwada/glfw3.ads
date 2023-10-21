
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


pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

-- fastrgv jan2020:
-- minor edits to gamepad stuff for usability...search "fastrgv"
-- 1st 4 types below allow self-containment within this single file.


package glfw3 is


   subtype uu_uint8_t is unsigned_char;
   subtype uu_uint16_t is unsigned_short;
   subtype uu_uint32_t is unsigned;
   subtype uu_uint64_t is unsigned_long;

--18jun2020: let's make size intensions explicit:

   type uint8_t is new uu_uint8_t;
	for uint8_t'size use 8;

   type uint16_t is new uu_uint16_t;
	for uint16_t'size use 16;

   type uint32_t is new uu_uint32_t;
	for uint32_t'size use 32;

   type uint64_t is new uu_uint64_t;
	for uint64_t'size use 64;

	--fastrgv:  this works fine too (in case of trouble):
	--type uint8_t  is mod 2**8;
	--for uint8_t'size use 8;
	--type uint16_t is mod 2**16;
	--for uint16_t'size use 16;
	--type uint32_t is mod 2**32;
	--for uint32_t'size use 32;
	--type uint64_t is mod 2**64;
	--for uint64_t'size use 64;




   GLFW_VERSION_MAJOR : constant := 3;  --  ../include/GLFW/glfw3.h:259

   GLFW_VERSION_MINOR : constant := 3;  --  ../include/GLFW/glfw3.h:266

   GLFW_VERSION_REVISION : constant := 4;  --  ../include/GLFW/glfw3.h:273

   GLFW_TRUE : constant := 1;  --  ../include/GLFW/glfw3.h:284

   GLFW_FALSE : constant := 0;  --  ../include/GLFW/glfw3.h:293

   GLFW_RELEASE : constant := 0;  --  ../include/GLFW/glfw3.h:303

   GLFW_PRESS : constant := 1;  --  ../include/GLFW/glfw3.h:310

   GLFW_REPEAT : constant := 2;  --  ../include/GLFW/glfw3.h:317

   GLFW_HAT_CENTERED : constant := 0;  --  ../include/GLFW/glfw3.h:327
   GLFW_HAT_UP : constant := 1;  --  ../include/GLFW/glfw3.h:328
   GLFW_HAT_RIGHT : constant := 2;  --  ../include/GLFW/glfw3.h:329
   GLFW_HAT_DOWN : constant := 4;  --  ../include/GLFW/glfw3.h:330
   GLFW_HAT_LEFT : constant := 8;  --  ../include/GLFW/glfw3.h:331
   --  unsupported macro: GLFW_HAT_RIGHT_UP (GLFW_HAT_RIGHT | GLFW_HAT_UP)
   --  unsupported macro: GLFW_HAT_RIGHT_DOWN (GLFW_HAT_RIGHT | GLFW_HAT_DOWN)
   --  unsupported macro: GLFW_HAT_LEFT_UP (GLFW_HAT_LEFT | GLFW_HAT_UP)
   --  unsupported macro: GLFW_HAT_LEFT_DOWN (GLFW_HAT_LEFT | GLFW_HAT_DOWN)

   GLFW_KEY_UNKNOWN : constant := -1;  --  ../include/GLFW/glfw3.h:363

   GLFW_KEY_SPACE : constant := 32;  --  ../include/GLFW/glfw3.h:366
   GLFW_KEY_APOSTROPHE : constant := 39;  --  ../include/GLFW/glfw3.h:367
   GLFW_KEY_COMMA : constant := 44;  --  ../include/GLFW/glfw3.h:368
   GLFW_KEY_MINUS : constant := 45;  --  ../include/GLFW/glfw3.h:369
   GLFW_KEY_PERIOD : constant := 46;  --  ../include/GLFW/glfw3.h:370
   GLFW_KEY_SLASH : constant := 47;  --  ../include/GLFW/glfw3.h:371
   GLFW_KEY_0 : constant := 48;  --  ../include/GLFW/glfw3.h:372
   GLFW_KEY_1 : constant := 49;  --  ../include/GLFW/glfw3.h:373
   GLFW_KEY_2 : constant := 50;  --  ../include/GLFW/glfw3.h:374
   GLFW_KEY_3 : constant := 51;  --  ../include/GLFW/glfw3.h:375
   GLFW_KEY_4 : constant := 52;  --  ../include/GLFW/glfw3.h:376
   GLFW_KEY_5 : constant := 53;  --  ../include/GLFW/glfw3.h:377
   GLFW_KEY_6 : constant := 54;  --  ../include/GLFW/glfw3.h:378
   GLFW_KEY_7 : constant := 55;  --  ../include/GLFW/glfw3.h:379
   GLFW_KEY_8 : constant := 56;  --  ../include/GLFW/glfw3.h:380
   GLFW_KEY_9 : constant := 57;  --  ../include/GLFW/glfw3.h:381
   GLFW_KEY_SEMICOLON : constant := 59;  --  ../include/GLFW/glfw3.h:382
   GLFW_KEY_EQUAL : constant := 61;  --  ../include/GLFW/glfw3.h:383
   GLFW_KEY_A : constant := 65;  --  ../include/GLFW/glfw3.h:384
   GLFW_KEY_B : constant := 66;  --  ../include/GLFW/glfw3.h:385
   GLFW_KEY_C : constant := 67;  --  ../include/GLFW/glfw3.h:386
   GLFW_KEY_D : constant := 68;  --  ../include/GLFW/glfw3.h:387
   GLFW_KEY_E : constant := 69;  --  ../include/GLFW/glfw3.h:388
   GLFW_KEY_F : constant := 70;  --  ../include/GLFW/glfw3.h:389
   GLFW_KEY_G : constant := 71;  --  ../include/GLFW/glfw3.h:390
   GLFW_KEY_H : constant := 72;  --  ../include/GLFW/glfw3.h:391
   GLFW_KEY_I : constant := 73;  --  ../include/GLFW/glfw3.h:392
   GLFW_KEY_J : constant := 74;  --  ../include/GLFW/glfw3.h:393
   GLFW_KEY_K : constant := 75;  --  ../include/GLFW/glfw3.h:394
   GLFW_KEY_L : constant := 76;  --  ../include/GLFW/glfw3.h:395
   GLFW_KEY_M : constant := 77;  --  ../include/GLFW/glfw3.h:396
   GLFW_KEY_N : constant := 78;  --  ../include/GLFW/glfw3.h:397
   GLFW_KEY_O : constant := 79;  --  ../include/GLFW/glfw3.h:398
   GLFW_KEY_P : constant := 80;  --  ../include/GLFW/glfw3.h:399
   GLFW_KEY_Q : constant := 81;  --  ../include/GLFW/glfw3.h:400
   GLFW_KEY_R : constant := 82;  --  ../include/GLFW/glfw3.h:401
   GLFW_KEY_S : constant := 83;  --  ../include/GLFW/glfw3.h:402
   GLFW_KEY_T : constant := 84;  --  ../include/GLFW/glfw3.h:403
   GLFW_KEY_U : constant := 85;  --  ../include/GLFW/glfw3.h:404
   GLFW_KEY_V : constant := 86;  --  ../include/GLFW/glfw3.h:405
   GLFW_KEY_W : constant := 87;  --  ../include/GLFW/glfw3.h:406
   GLFW_KEY_X : constant := 88;  --  ../include/GLFW/glfw3.h:407
   GLFW_KEY_Y : constant := 89;  --  ../include/GLFW/glfw3.h:408
   GLFW_KEY_Z : constant := 90;  --  ../include/GLFW/glfw3.h:409
   GLFW_KEY_LEFT_BRACKET : constant := 91;  --  ../include/GLFW/glfw3.h:410
   GLFW_KEY_BACKSLASH : constant := 92;  --  ../include/GLFW/glfw3.h:411
   GLFW_KEY_RIGHT_BRACKET : constant := 93;  --  ../include/GLFW/glfw3.h:412
   GLFW_KEY_GRAVE_ACCENT : constant := 96;  --  ../include/GLFW/glfw3.h:413
   GLFW_KEY_WORLD_1 : constant := 161;  --  ../include/GLFW/glfw3.h:414
   GLFW_KEY_WORLD_2 : constant := 162;  --  ../include/GLFW/glfw3.h:415

   GLFW_KEY_ESCAPE : constant := 256;  --  ../include/GLFW/glfw3.h:418
   GLFW_KEY_ENTER : constant := 257;  --  ../include/GLFW/glfw3.h:419
   GLFW_KEY_TAB : constant := 258;  --  ../include/GLFW/glfw3.h:420
   GLFW_KEY_BACKSPACE : constant := 259;  --  ../include/GLFW/glfw3.h:421
   GLFW_KEY_INSERT : constant := 260;  --  ../include/GLFW/glfw3.h:422
   GLFW_KEY_DELETE : constant := 261;  --  ../include/GLFW/glfw3.h:423
   GLFW_KEY_RIGHT : constant := 262;  --  ../include/GLFW/glfw3.h:424
   GLFW_KEY_LEFT : constant := 263;  --  ../include/GLFW/glfw3.h:425
   GLFW_KEY_DOWN : constant := 264;  --  ../include/GLFW/glfw3.h:426
   GLFW_KEY_UP : constant := 265;  --  ../include/GLFW/glfw3.h:427
   GLFW_KEY_PAGE_UP : constant := 266;  --  ../include/GLFW/glfw3.h:428
   GLFW_KEY_PAGE_DOWN : constant := 267;  --  ../include/GLFW/glfw3.h:429
   GLFW_KEY_HOME : constant := 268;  --  ../include/GLFW/glfw3.h:430
   GLFW_KEY_END : constant := 269;  --  ../include/GLFW/glfw3.h:431
   GLFW_KEY_CAPS_LOCK : constant := 280;  --  ../include/GLFW/glfw3.h:432
   GLFW_KEY_SCROLL_LOCK : constant := 281;  --  ../include/GLFW/glfw3.h:433
   GLFW_KEY_NUM_LOCK : constant := 282;  --  ../include/GLFW/glfw3.h:434
   GLFW_KEY_PRINT_SCREEN : constant := 283;  --  ../include/GLFW/glfw3.h:435
   GLFW_KEY_PAUSE : constant := 284;  --  ../include/GLFW/glfw3.h:436
   GLFW_KEY_F1 : constant := 290;  --  ../include/GLFW/glfw3.h:437
   GLFW_KEY_F2 : constant := 291;  --  ../include/GLFW/glfw3.h:438
   GLFW_KEY_F3 : constant := 292;  --  ../include/GLFW/glfw3.h:439
   GLFW_KEY_F4 : constant := 293;  --  ../include/GLFW/glfw3.h:440
   GLFW_KEY_F5 : constant := 294;  --  ../include/GLFW/glfw3.h:441
   GLFW_KEY_F6 : constant := 295;  --  ../include/GLFW/glfw3.h:442
   GLFW_KEY_F7 : constant := 296;  --  ../include/GLFW/glfw3.h:443
   GLFW_KEY_F8 : constant := 297;  --  ../include/GLFW/glfw3.h:444
   GLFW_KEY_F9 : constant := 298;  --  ../include/GLFW/glfw3.h:445
   GLFW_KEY_F10 : constant := 299;  --  ../include/GLFW/glfw3.h:446
   GLFW_KEY_F11 : constant := 300;  --  ../include/GLFW/glfw3.h:447
   GLFW_KEY_F12 : constant := 301;  --  ../include/GLFW/glfw3.h:448
   GLFW_KEY_F13 : constant := 302;  --  ../include/GLFW/glfw3.h:449
   GLFW_KEY_F14 : constant := 303;  --  ../include/GLFW/glfw3.h:450
   GLFW_KEY_F15 : constant := 304;  --  ../include/GLFW/glfw3.h:451
   GLFW_KEY_F16 : constant := 305;  --  ../include/GLFW/glfw3.h:452
   GLFW_KEY_F17 : constant := 306;  --  ../include/GLFW/glfw3.h:453
   GLFW_KEY_F18 : constant := 307;  --  ../include/GLFW/glfw3.h:454
   GLFW_KEY_F19 : constant := 308;  --  ../include/GLFW/glfw3.h:455
   GLFW_KEY_F20 : constant := 309;  --  ../include/GLFW/glfw3.h:456
   GLFW_KEY_F21 : constant := 310;  --  ../include/GLFW/glfw3.h:457
   GLFW_KEY_F22 : constant := 311;  --  ../include/GLFW/glfw3.h:458
   GLFW_KEY_F23 : constant := 312;  --  ../include/GLFW/glfw3.h:459
   GLFW_KEY_F24 : constant := 313;  --  ../include/GLFW/glfw3.h:460
   GLFW_KEY_F25 : constant := 314;  --  ../include/GLFW/glfw3.h:461
   GLFW_KEY_KP_0 : constant := 320;  --  ../include/GLFW/glfw3.h:462
   GLFW_KEY_KP_1 : constant := 321;  --  ../include/GLFW/glfw3.h:463
   GLFW_KEY_KP_2 : constant := 322;  --  ../include/GLFW/glfw3.h:464
   GLFW_KEY_KP_3 : constant := 323;  --  ../include/GLFW/glfw3.h:465
   GLFW_KEY_KP_4 : constant := 324;  --  ../include/GLFW/glfw3.h:466
   GLFW_KEY_KP_5 : constant := 325;  --  ../include/GLFW/glfw3.h:467
   GLFW_KEY_KP_6 : constant := 326;  --  ../include/GLFW/glfw3.h:468
   GLFW_KEY_KP_7 : constant := 327;  --  ../include/GLFW/glfw3.h:469
   GLFW_KEY_KP_8 : constant := 328;  --  ../include/GLFW/glfw3.h:470
   GLFW_KEY_KP_9 : constant := 329;  --  ../include/GLFW/glfw3.h:471
   GLFW_KEY_KP_DECIMAL : constant := 330;  --  ../include/GLFW/glfw3.h:472
   GLFW_KEY_KP_DIVIDE : constant := 331;  --  ../include/GLFW/glfw3.h:473
   GLFW_KEY_KP_MULTIPLY : constant := 332;  --  ../include/GLFW/glfw3.h:474
   GLFW_KEY_KP_SUBTRACT : constant := 333;  --  ../include/GLFW/glfw3.h:475
   GLFW_KEY_KP_ADD : constant := 334;  --  ../include/GLFW/glfw3.h:476
   GLFW_KEY_KP_ENTER : constant := 335;  --  ../include/GLFW/glfw3.h:477
   GLFW_KEY_KP_EQUAL : constant := 336;  --  ../include/GLFW/glfw3.h:478
   GLFW_KEY_LEFT_SHIFT : constant := 340;  --  ../include/GLFW/glfw3.h:479
   GLFW_KEY_LEFT_CONTROL : constant := 341;  --  ../include/GLFW/glfw3.h:480
   GLFW_KEY_LEFT_ALT : constant := 342;  --  ../include/GLFW/glfw3.h:481
   GLFW_KEY_LEFT_SUPER : constant := 343;  --  ../include/GLFW/glfw3.h:482
   GLFW_KEY_RIGHT_SHIFT : constant := 344;  --  ../include/GLFW/glfw3.h:483
   GLFW_KEY_RIGHT_CONTROL : constant := 345;  --  ../include/GLFW/glfw3.h:484
   GLFW_KEY_RIGHT_ALT : constant := 346;  --  ../include/GLFW/glfw3.h:485
   GLFW_KEY_RIGHT_SUPER : constant := 347;  --  ../include/GLFW/glfw3.h:486
   GLFW_KEY_MENU : constant := 348;  --  ../include/GLFW/glfw3.h:487
   --  unsupported macro: GLFW_KEY_LAST GLFW_KEY_MENU

   GLFW_MOD_SHIFT : constant := 16#0001#;  --  ../include/GLFW/glfw3.h:505

   GLFW_MOD_CONTROL : constant := 16#0002#;  --  ../include/GLFW/glfw3.h:510

   GLFW_MOD_ALT : constant := 16#0004#;  --  ../include/GLFW/glfw3.h:515

   GLFW_MOD_SUPER : constant := 16#0008#;  --  ../include/GLFW/glfw3.h:520

   GLFW_MOD_CAPS_LOCK : constant := 16#0010#;  --  ../include/GLFW/glfw3.h:526

   GLFW_MOD_NUM_LOCK : constant := 16#0020#;  --  ../include/GLFW/glfw3.h:532

   GLFW_MOUSE_BUTTON_1 : constant := 0;  --  ../include/GLFW/glfw3.h:543
   GLFW_MOUSE_BUTTON_2 : constant := 1;  --  ../include/GLFW/glfw3.h:544
   GLFW_MOUSE_BUTTON_3 : constant := 2;  --  ../include/GLFW/glfw3.h:545
   GLFW_MOUSE_BUTTON_4 : constant := 3;  --  ../include/GLFW/glfw3.h:546
   GLFW_MOUSE_BUTTON_5 : constant := 4;  --  ../include/GLFW/glfw3.h:547
   GLFW_MOUSE_BUTTON_6 : constant := 5;  --  ../include/GLFW/glfw3.h:548
   GLFW_MOUSE_BUTTON_7 : constant := 6;  --  ../include/GLFW/glfw3.h:549
   GLFW_MOUSE_BUTTON_8 : constant := 7;  --  ../include/GLFW/glfw3.h:550
   --  unsupported macro: GLFW_MOUSE_BUTTON_LAST GLFW_MOUSE_BUTTON_8
   --  unsupported macro: GLFW_MOUSE_BUTTON_LEFT GLFW_MOUSE_BUTTON_1
   --  unsupported macro: GLFW_MOUSE_BUTTON_RIGHT GLFW_MOUSE_BUTTON_2
   --  unsupported macro: GLFW_MOUSE_BUTTON_MIDDLE GLFW_MOUSE_BUTTON_3

   GLFW_JOYSTICK_1 : constant := 0;  --  ../include/GLFW/glfw3.h:564
   GLFW_JOYSTICK_2 : constant := 1;  --  ../include/GLFW/glfw3.h:565
   GLFW_JOYSTICK_3 : constant := 2;  --  ../include/GLFW/glfw3.h:566
   GLFW_JOYSTICK_4 : constant := 3;  --  ../include/GLFW/glfw3.h:567
   GLFW_JOYSTICK_5 : constant := 4;  --  ../include/GLFW/glfw3.h:568
   GLFW_JOYSTICK_6 : constant := 5;  --  ../include/GLFW/glfw3.h:569
   GLFW_JOYSTICK_7 : constant := 6;  --  ../include/GLFW/glfw3.h:570
   GLFW_JOYSTICK_8 : constant := 7;  --  ../include/GLFW/glfw3.h:571
   GLFW_JOYSTICK_9 : constant := 8;  --  ../include/GLFW/glfw3.h:572
   GLFW_JOYSTICK_10 : constant := 9;  --  ../include/GLFW/glfw3.h:573
   GLFW_JOYSTICK_11 : constant := 10;  --  ../include/GLFW/glfw3.h:574
   GLFW_JOYSTICK_12 : constant := 11;  --  ../include/GLFW/glfw3.h:575
   GLFW_JOYSTICK_13 : constant := 12;  --  ../include/GLFW/glfw3.h:576
   GLFW_JOYSTICK_14 : constant := 13;  --  ../include/GLFW/glfw3.h:577
   GLFW_JOYSTICK_15 : constant := 14;  --  ../include/GLFW/glfw3.h:578
   GLFW_JOYSTICK_16 : constant := 15;  --  ../include/GLFW/glfw3.h:579
   --  unsupported macro: GLFW_JOYSTICK_LAST GLFW_JOYSTICK_16

   GLFW_GAMEPAD_BUTTON_A : constant := 0;  --  ../include/GLFW/glfw3.h:590
   GLFW_GAMEPAD_BUTTON_B : constant := 1;  --  ../include/GLFW/glfw3.h:591
   GLFW_GAMEPAD_BUTTON_X : constant := 2;  --  ../include/GLFW/glfw3.h:592
   GLFW_GAMEPAD_BUTTON_Y : constant := 3;  --  ../include/GLFW/glfw3.h:593
   GLFW_GAMEPAD_BUTTON_LEFT_BUMPER : constant := 4;  --  ../include/GLFW/glfw3.h:594
   GLFW_GAMEPAD_BUTTON_RIGHT_BUMPER : constant := 5;  --  ../include/GLFW/glfw3.h:595
   GLFW_GAMEPAD_BUTTON_BACK : constant := 6;  --  ../include/GLFW/glfw3.h:596
   GLFW_GAMEPAD_BUTTON_START : constant := 7;  --  ../include/GLFW/glfw3.h:597
   GLFW_GAMEPAD_BUTTON_GUIDE : constant := 8;  --  ../include/GLFW/glfw3.h:598
   GLFW_GAMEPAD_BUTTON_LEFT_THUMB : constant := 9;  --  ../include/GLFW/glfw3.h:599
   GLFW_GAMEPAD_BUTTON_RIGHT_THUMB : constant := 10;  --  ../include/GLFW/glfw3.h:600
   GLFW_GAMEPAD_BUTTON_DPAD_UP : constant := 11;  --  ../include/GLFW/glfw3.h:601
   GLFW_GAMEPAD_BUTTON_DPAD_RIGHT : constant := 12;  --  ../include/GLFW/glfw3.h:602
   GLFW_GAMEPAD_BUTTON_DPAD_DOWN : constant := 13;  --  ../include/GLFW/glfw3.h:603
   GLFW_GAMEPAD_BUTTON_DPAD_LEFT : constant := 14;  --  ../include/GLFW/glfw3.h:604

	--fastrgv:
   GLFW_GAMEPAD_BUTTON_CROSS : constant := GLFW_GAMEPAD_BUTTON_A; --fastrgv
   GLFW_GAMEPAD_BUTTON_CIRCLE : constant := GLFW_GAMEPAD_BUTTON_B;
   GLFW_GAMEPAD_BUTTON_SQUARE : constant := GLFW_GAMEPAD_BUTTON_X;
   GLFW_GAMEPAD_BUTTON_TRIANGLE : constant := GLFW_GAMEPAD_BUTTON_Y;

   --  unsupported macro: GLFW_GAMEPAD_BUTTON_LAST GLFW_GAMEPAD_BUTTON_DPAD_LEFT
   --  unsupported macro: GLFW_GAMEPAD_BUTTON_CROSS GLFW_GAMEPAD_BUTTON_A
   --  unsupported macro: GLFW_GAMEPAD_BUTTON_CIRCLE GLFW_GAMEPAD_BUTTON_B
   --  unsupported macro: GLFW_GAMEPAD_BUTTON_SQUARE GLFW_GAMEPAD_BUTTON_X
   --  unsupported macro: GLFW_GAMEPAD_BUTTON_TRIANGLE GLFW_GAMEPAD_BUTTON_Y

   GLFW_GAMEPAD_AXIS_LEFT_X : constant := 0;  --  ../include/GLFW/glfw3.h:620
   GLFW_GAMEPAD_AXIS_LEFT_Y : constant := 1;  --  ../include/GLFW/glfw3.h:621
   GLFW_GAMEPAD_AXIS_RIGHT_X : constant := 2;  --  ../include/GLFW/glfw3.h:622
   GLFW_GAMEPAD_AXIS_RIGHT_Y : constant := 3;  --  ../include/GLFW/glfw3.h:623
   GLFW_GAMEPAD_AXIS_LEFT_TRIGGER : constant := 4;  --  ../include/GLFW/glfw3.h:624
   GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER : constant := 5;  --  ../include/GLFW/glfw3.h:625
   --  unsupported macro: GLFW_GAMEPAD_AXIS_LAST GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER

   GLFW_NO_ERROR : constant := 0;  --  ../include/GLFW/glfw3.h:642

   GLFW_NOT_INITIALIZED : constant := 16#00010001#;  --  ../include/GLFW/glfw3.h:651

   GLFW_NO_CURRENT_CONTEXT : constant := 16#00010002#;  --  ../include/GLFW/glfw3.h:661

   GLFW_INVALID_ENUM : constant := 16#00010003#;  --  ../include/GLFW/glfw3.h:669

   GLFW_INVALID_VALUE : constant := 16#00010004#;  --  ../include/GLFW/glfw3.h:680

   GLFW_OUT_OF_MEMORY : constant := 16#00010005#;  --  ../include/GLFW/glfw3.h:688

   GLFW_API_UNAVAILABLE : constant := 16#00010006#;  --  ../include/GLFW/glfw3.h:704

   GLFW_VERSION_UNAVAILABLE : constant := 16#00010007#;  --  ../include/GLFW/glfw3.h:721

   GLFW_PLATFORM_ERROR : constant := 16#00010008#;  --  ../include/GLFW/glfw3.h:732

   GLFW_FORMAT_UNAVAILABLE : constant := 16#00010009#;  --  ../include/GLFW/glfw3.h:751

   GLFW_NO_WINDOW_CONTEXT : constant := 16#0001000A#;  --  ../include/GLFW/glfw3.h:759

   GLFW_FOCUSED : constant := 16#00020001#;  --  ../include/GLFW/glfw3.h:769

   GLFW_ICONIFIED : constant := 16#00020002#;  --  ../include/GLFW/glfw3.h:774

   GLFW_RESIZABLE : constant := 16#00020003#;  --  ../include/GLFW/glfw3.h:780

   GLFW_VISIBLE : constant := 16#00020004#;  --  ../include/GLFW/glfw3.h:786

   GLFW_DECORATED : constant := 16#00020005#;  --  ../include/GLFW/glfw3.h:792

   GLFW_AUTO_ICONIFY : constant := 16#00020006#;  --  ../include/GLFW/glfw3.h:798

   GLFW_FLOATING : constant := 16#00020007#;  --  ../include/GLFW/glfw3.h:804

   GLFW_MAXIMIZED : constant := 16#00020008#;  --  ../include/GLFW/glfw3.h:810

   GLFW_CENTER_CURSOR : constant := 16#00020009#;  --  ../include/GLFW/glfw3.h:815

   GLFW_TRANSPARENT_FRAMEBUFFER : constant := 16#0002000A#;  --  ../include/GLFW/glfw3.h:822

   GLFW_HOVERED : constant := 16#0002000B#;  --  ../include/GLFW/glfw3.h:827

   GLFW_FOCUS_ON_SHOW : constant := 16#0002000C#;  --  ../include/GLFW/glfw3.h:833

   GLFW_RED_BITS : constant := 16#00021001#;  --  ../include/GLFW/glfw3.h:839

   GLFW_GREEN_BITS : constant := 16#00021002#;  --  ../include/GLFW/glfw3.h:844

   GLFW_BLUE_BITS : constant := 16#00021003#;  --  ../include/GLFW/glfw3.h:849

   GLFW_ALPHA_BITS : constant := 16#00021004#;  --  ../include/GLFW/glfw3.h:854

   GLFW_DEPTH_BITS : constant := 16#00021005#;  --  ../include/GLFW/glfw3.h:859

   GLFW_STENCIL_BITS : constant := 16#00021006#;  --  ../include/GLFW/glfw3.h:864

   GLFW_ACCUM_RED_BITS : constant := 16#00021007#;  --  ../include/GLFW/glfw3.h:869

   GLFW_ACCUM_GREEN_BITS : constant := 16#00021008#;  --  ../include/GLFW/glfw3.h:874

   GLFW_ACCUM_BLUE_BITS : constant := 16#00021009#;  --  ../include/GLFW/glfw3.h:879

   GLFW_ACCUM_ALPHA_BITS : constant := 16#0002100A#;  --  ../include/GLFW/glfw3.h:884

   GLFW_AUX_BUFFERS : constant := 16#0002100B#;  --  ../include/GLFW/glfw3.h:889

   GLFW_STEREO : constant := 16#0002100C#;  --  ../include/GLFW/glfw3.h:894

   GLFW_SAMPLES : constant := 16#0002100D#;  --  ../include/GLFW/glfw3.h:899

   GLFW_SRGB_CAPABLE : constant := 16#0002100E#;  --  ../include/GLFW/glfw3.h:904

   GLFW_REFRESH_RATE : constant := 16#0002100F#;  --  ../include/GLFW/glfw3.h:909

   GLFW_DOUBLEBUFFER : constant := 16#00021010#;  --  ../include/GLFW/glfw3.h:914

   GLFW_CLIENT_API : constant := 16#00022001#;  --  ../include/GLFW/glfw3.h:921

   GLFW_CONTEXT_VERSION_MAJOR : constant := 16#00022002#;  --  ../include/GLFW/glfw3.h:927

   GLFW_CONTEXT_VERSION_MINOR : constant := 16#00022003#;  --  ../include/GLFW/glfw3.h:933

   GLFW_CONTEXT_REVISION : constant := 16#00022004#;  --  ../include/GLFW/glfw3.h:939

   GLFW_CONTEXT_ROBUSTNESS : constant := 16#00022005#;  --  ../include/GLFW/glfw3.h:945

   GLFW_OPENGL_FORWARD_COMPAT : constant := 16#00022006#;  --  ../include/GLFW/glfw3.h:951

   GLFW_OPENGL_DEBUG_CONTEXT : constant := 16#00022007#;  --  ../include/GLFW/glfw3.h:957

   GLFW_OPENGL_PROFILE : constant := 16#00022008#;  --  ../include/GLFW/glfw3.h:963

   GLFW_CONTEXT_RELEASE_BEHAVIOR : constant := 16#00022009#;  --  ../include/GLFW/glfw3.h:969

   GLFW_CONTEXT_NO_ERROR : constant := 16#0002200A#;  --  ../include/GLFW/glfw3.h:975

   GLFW_CONTEXT_CREATION_API : constant := 16#0002200B#;  --  ../include/GLFW/glfw3.h:981

   GLFW_SCALE_TO_MONITOR : constant := 16#0002200C#;  --  ../include/GLFW/glfw3.h:985

   GLFW_COCOA_RETINA_FRAMEBUFFER : constant := 16#00023001#;  --  ../include/GLFW/glfw3.h:989

   GLFW_COCOA_FRAME_NAME : constant := 16#00023002#;  --  ../include/GLFW/glfw3.h:993

   GLFW_COCOA_GRAPHICS_SWITCHING : constant := 16#00023003#;  --  ../include/GLFW/glfw3.h:997

   GLFW_X11_CLASS_NAME : constant := 16#00024001#;  --  ../include/GLFW/glfw3.h:1001

   GLFW_X11_INSTANCE_NAME : constant := 16#00024002#;  --  ../include/GLFW/glfw3.h:1005

   GLFW_NO_API : constant := 0;  --  ../include/GLFW/glfw3.h:1008
   GLFW_OPENGL_API : constant := 16#00030001#;  --  ../include/GLFW/glfw3.h:1009
   GLFW_OPENGL_ES_API : constant := 16#00030002#;  --  ../include/GLFW/glfw3.h:1010

   GLFW_NO_ROBUSTNESS : constant := 0;  --  ../include/GLFW/glfw3.h:1012
   GLFW_NO_RESET_NOTIFICATION : constant := 16#00031001#;  --  ../include/GLFW/glfw3.h:1013
   GLFW_LOSE_CONTEXT_ON_RESET : constant := 16#00031002#;  --  ../include/GLFW/glfw3.h:1014

   GLFW_OPENGL_ANY_PROFILE : constant := 0;  --  ../include/GLFW/glfw3.h:1016
   GLFW_OPENGL_CORE_PROFILE : constant := 16#00032001#;  --  ../include/GLFW/glfw3.h:1017
   GLFW_OPENGL_COMPAT_PROFILE : constant := 16#00032002#;  --  ../include/GLFW/glfw3.h:1018

   GLFW_CURSOR : constant := 16#00033001#;  --  ../include/GLFW/glfw3.h:1020
   GLFW_STICKY_KEYS : constant := 16#00033002#;  --  ../include/GLFW/glfw3.h:1021
   GLFW_STICKY_MOUSE_BUTTONS : constant := 16#00033003#;  --  ../include/GLFW/glfw3.h:1022
   GLFW_LOCK_KEY_MODS : constant := 16#00033004#;  --  ../include/GLFW/glfw3.h:1023
   GLFW_RAW_MOUSE_MOTION : constant := 16#00033005#;  --  ../include/GLFW/glfw3.h:1024

   GLFW_CURSOR_NORMAL : constant := 16#00034001#;  --  ../include/GLFW/glfw3.h:1026
   GLFW_CURSOR_HIDDEN : constant := 16#00034002#;  --  ../include/GLFW/glfw3.h:1027
   GLFW_CURSOR_DISABLED : constant := 16#00034003#;  --  ../include/GLFW/glfw3.h:1028

   GLFW_ANY_RELEASE_BEHAVIOR : constant := 0;  --  ../include/GLFW/glfw3.h:1030
   GLFW_RELEASE_BEHAVIOR_FLUSH : constant := 16#00035001#;  --  ../include/GLFW/glfw3.h:1031
   GLFW_RELEASE_BEHAVIOR_NONE : constant := 16#00035002#;  --  ../include/GLFW/glfw3.h:1032

   GLFW_NATIVE_CONTEXT_API : constant := 16#00036001#;  --  ../include/GLFW/glfw3.h:1034
   GLFW_EGL_CONTEXT_API : constant := 16#00036002#;  --  ../include/GLFW/glfw3.h:1035
   GLFW_OSMESA_CONTEXT_API : constant := 16#00036003#;  --  ../include/GLFW/glfw3.h:1036

   GLFW_ARROW_CURSOR : constant := 16#00036001#;  --  ../include/GLFW/glfw3.h:1050

   GLFW_IBEAM_CURSOR : constant := 16#00036002#;  --  ../include/GLFW/glfw3.h:1055

   GLFW_CROSSHAIR_CURSOR : constant := 16#00036003#;  --  ../include/GLFW/glfw3.h:1060

   GLFW_HAND_CURSOR : constant := 16#00036004#;  --  ../include/GLFW/glfw3.h:1065

   GLFW_HRESIZE_CURSOR : constant := 16#00036005#;  --  ../include/GLFW/glfw3.h:1070

   GLFW_VRESIZE_CURSOR : constant := 16#00036006#;  --  ../include/GLFW/glfw3.h:1075

   GLFW_CONNECTED : constant := 16#00040001#;  --  ../include/GLFW/glfw3.h:1078
   GLFW_DISCONNECTED : constant := 16#00040002#;  --  ../include/GLFW/glfw3.h:1079

   GLFW_JOYSTICK_HAT_BUTTONS : constant := 16#00050001#;  --  ../include/GLFW/glfw3.h:1087

   GLFW_COCOA_CHDIR_RESOURCES : constant := 16#00051001#;  --  ../include/GLFW/glfw3.h:1092

   GLFW_COCOA_MENUBAR : constant := 16#00051002#;  --  ../include/GLFW/glfw3.h:1097

   GLFW_DONT_CARE : constant := -1;  --  ../include/GLFW/glfw3.h:1100

  --************************************************************************
  -- * GLFW 3.3 - www.glfw.org
  -- * A library for OpenGL, window and input
  -- *------------------------------------------------------------------------
  -- * Copyright (c) 2002-2006 Marcus Geelnard
  -- * Copyright (c) 2006-2019 Camilla Löwy <elmindreda@glfw.org>
  -- *
  -- * This software is provided 'as-is', without any express or implied
  -- * warranty. In no event will the authors be held liable for any damages
  -- * arising from the use of this software.
  -- *
  -- * Permission is granted to anyone to use this software for any purpose,
  -- * including commercial applications, and to alter it and redistribute it
  -- * freely, subject to the following restrictions:
  -- *
  -- * 1. The origin of this software must not be misrepresented; you must not
  -- *    claim that you wrote the original software. If you use this software
  -- *    in a product, an acknowledgment in the product documentation would
  -- *    be appreciated but is not required.
  -- *
  -- * 2. Altered source versions must be plainly marked as such, and must not
  -- *    be misrepresented as being the original software.
  -- *
  -- * 3. This notice may not be removed or altered from any source
  -- *    distribution.
  -- *
  -- ************************************************************************ 

  --************************************************************************
  -- * Doxygen documentation
  -- ************************************************************************ 

  --! @file glfw3.h
  -- *  @brief The header of the GLFW 3 API.
  -- *
  -- *  This is the header file of the GLFW 3 API.  It defines all its types and
  -- *  declares all its functions.
  -- *
  -- *  For more information about how to use this file, see @ref build_include.
  --  

  --! @defgroup context Context reference
  -- *  @brief Functions and types related to OpenGL and OpenGL ES contexts.
  -- *
  -- *  This is the reference documentation for OpenGL and OpenGL ES context related
  -- *  functions.  For more task-oriented information, see the @ref context_guide.
  --  

  --! @defgroup vulkan Vulkan reference
  -- *  @brief Functions and types related to Vulkan.
  -- *
  -- *  This is the reference documentation for Vulkan related functions and types.
  -- *  For more task-oriented information, see the @ref vulkan_guide.
  --  

  --! @defgroup init Initialization, version and error reference
  -- *  @brief Functions and types related to initialization and error handling.
  -- *
  -- *  This is the reference documentation for initialization and termination of
  -- *  the library, version management and error handling.  For more task-oriented
  -- *  information, see the @ref intro_guide.
  --  

  --! @defgroup input Input reference
  -- *  @brief Functions and types related to input handling.
  -- *
  -- *  This is the reference documentation for input related functions and types.
  -- *  For more task-oriented information, see the @ref input_guide.
  --  

  --! @defgroup monitor Monitor reference
  -- *  @brief Functions and types related to monitors.
  -- *
  -- *  This is the reference documentation for monitor related functions and types.
  -- *  For more task-oriented information, see the @ref monitor_guide.
  --  

  --! @defgroup window Window reference
  -- *  @brief Functions and types related to windows.
  -- *
  -- *  This is the reference documentation for window related functions and types,
  -- *  including creation, deletion and event polling.  For more task-oriented
  -- *  information, see the @ref window_guide.
  --  

  --************************************************************************
  -- * Compiler- and platform-specific preprocessor work
  -- ************************************************************************ 

  -- If we are we on Windows, we want a single define for it.
  --  

  -- Include because most Windows GLU headers need wchar_t and
  -- * the macOS OpenGL header blocks the definition of ptrdiff_t by glext.h.
  -- * Include it unconditionally to avoid surprising side-effects.
  --  

  -- Include because it is needed by Vulkan and related functions.
  -- * Include it unconditionally to avoid surprising side-effects.
  --  

  -- The Vulkan header may have indirectly included windows.h (because of
  -- * VK_USE_PLATFORM_WIN32_KHR) so we offer our replacement symbols after it.
  --  

  -- It is customary to use APIENTRY for OpenGL function pointer declarations on
  -- * all platforms.  Additionally, the Windows OpenGL header needs APIENTRY.
  --  

  -- Some Windows OpenGL headers need this.
  --  

  -- Some Windows GLU headers need this.
  --  

  -- Include the chosen OpenGL or OpenGL ES headers.
  --  

  -- GLFW_DLL must be defined by applications that are linking against the DLL
  --  * version of the GLFW library.  _GLFW_BUILD_DLL is defined by the GLFW
  --  * configuration header when compiling the DLL version of the library.
  --   

  -- GLFWAPI is used to declare public API functions for export
  -- * from the DLL / shared library / dynamic library.
  --  

  -- We are building GLFW as a Win32 DLL  
  -- We are calling GLFW as a Win32 DLL  
  -- We are building GLFW as a shared / dynamic library  
  -- We are building or calling GLFW as a static library  
  --************************************************************************
  -- * GLFW API tokens
  -- ************************************************************************ 

  --! @name GLFW version macros
  -- *  @{  

  --! @brief The major version number of the GLFW library.
  -- *
  -- *  This is incremented when the API is changed in non-compatible ways.
  -- *  @ingroup init
  --  

  --! @brief The minor version number of the GLFW library.
  -- *
  -- *  This is incremented when features are added to the API but it remains
  -- *  backward-compatible.
  -- *  @ingroup init
  --  

  --! @brief The revision number of the GLFW library.
  -- *
  -- *  This is incremented when a bug fix release is made that does not contain any
  -- *  API changes.
  -- *  @ingroup init
  --  

  --! @}  
  --! @brief One.
  -- *
  -- *  This is only semantic sugar for the number 1.  You can instead use `1` or
  -- *  `true` or `_True` or `GL_TRUE` or `VK_TRUE` or anything else that is equal
  -- *  to one.
  -- *
  -- *  @ingroup init
  --  

  --! @brief Zero.
  -- *
  -- *  This is only semantic sugar for the number 0.  You can instead use `0` or
  -- *  `false` or `_False` or `GL_FALSE` or `VK_FALSE` or anything else that is
  -- *  equal to zero.
  -- *
  -- *  @ingroup init
  --  

  --! @name Key and button actions
  -- *  @{  

  --! @brief The key or mouse button was released.
  -- *
  -- *  The key or mouse button was released.
  -- *
  -- *  @ingroup input
  --  

  --! @brief The key or mouse button was pressed.
  -- *
  -- *  The key or mouse button was pressed.
  -- *
  -- *  @ingroup input
  --  

  --! @brief The key was held down until it repeated.
  -- *
  -- *  The key was held down until it repeated.
  -- *
  -- *  @ingroup input
  --  

  --! @}  
  --! @defgroup hat_state Joystick hat states
  -- *  @brief Joystick hat states.
  -- *
  -- *  See [joystick hat input](@ref joystick_hat) for how these are used.
  -- *
  -- *  @ingroup input
  -- *  @{  

  --! @}  
  --! @defgroup keys Keyboard keys
  -- *  @brief Keyboard key IDs.
  -- *
  -- *  See [key input](@ref input_key) for how these are used.
  -- *
  -- *  These key codes are inspired by the _USB HID Usage Tables v1.12_ (p. 53-60),
  -- *  but re-arranged to map to 7-bit ASCII for printable keys (function keys are
  -- *  put in the 256+ range).
  -- *
  -- *  The naming of the key codes follow these rules:
  -- *   - The US keyboard layout is used
  -- *   - Names of printable alpha-numeric characters are used (e.g. "A", "R",
  -- *     "3", etc.)
  -- *   - For non-alphanumeric characters, Unicode:ish names are used (e.g.
  -- *     "COMMA", "LEFT_SQUARE_BRACKET", etc.). Note that some names do not
  -- *     correspond to the Unicode standard (usually for brevity)
  -- *   - Keys that lack a clear US mapping are named "WORLD_x"
  -- *   - For non-printable keys, custom names are used (e.g. "F4",
  -- *     "BACKSPACE", etc.)
  -- *
  -- *  @ingroup input
  -- *  @{
  --  

  -- The unknown key  
  -- Printable keys  
  -- Function keys  
  --! @}  
  --! @defgroup mods Modifier key flags
  -- *  @brief Modifier key flags.
  -- *
  -- *  See [key input](@ref input_key) for how these are used.
  -- *
  -- *  @ingroup input
  -- *  @{  

  --! @brief If this bit is set one or more Shift keys were held down.
  -- *
  -- *  If this bit is set one or more Shift keys were held down.
  --  

  --! @brief If this bit is set one or more Control keys were held down.
  -- *
  -- *  If this bit is set one or more Control keys were held down.
  --  

  --! @brief If this bit is set one or more Alt keys were held down.
  -- *
  -- *  If this bit is set one or more Alt keys were held down.
  --  

  --! @brief If this bit is set one or more Super keys were held down.
  -- *
  -- *  If this bit is set one or more Super keys were held down.
  --  

  --! @brief If this bit is set the Caps Lock key is enabled.
  -- *
  -- *  If this bit is set the Caps Lock key is enabled and the @ref
  -- *  GLFW_LOCK_KEY_MODS input mode is set.
  --  

  --! @brief If this bit is set the Num Lock key is enabled.
  -- *
  -- *  If this bit is set the Num Lock key is enabled and the @ref
  -- *  GLFW_LOCK_KEY_MODS input mode is set.
  --  

  --! @}  
  --! @defgroup buttons Mouse buttons
  -- *  @brief Mouse button IDs.
  -- *
  -- *  See [mouse button input](@ref input_mouse_button) for how these are used.
  -- *
  -- *  @ingroup input
  -- *  @{  

  --! @}  
  --! @defgroup joysticks Joysticks
  -- *  @brief Joystick IDs.
  -- *
  -- *  See [joystick input](@ref joystick) for how these are used.
  -- *
  -- *  @ingroup input
  -- *  @{  

  --! @}  
  --! @defgroup gamepad_buttons Gamepad buttons
  -- *  @brief Gamepad buttons.
  -- *
  -- *  See @ref gamepad for how these are used.
  -- *
  -- *  @ingroup input
  -- *  @{  

  --! @}  
  --! @defgroup gamepad_axes Gamepad axes
  -- *  @brief Gamepad axes.
  -- *
  -- *  See @ref gamepad for how these are used.
  -- *
  -- *  @ingroup input
  -- *  @{  

  --! @}  
  --! @defgroup errors Error codes
  -- *  @brief Error codes.
  -- *
  -- *  See [error handling](@ref error_handling) for how these are used.
  -- *
  -- *  @ingroup init
  -- *  @{  

  --! @brief No error has occurred.
  -- *
  -- *  No error has occurred.
  -- *
  -- *  @analysis Yay.
  --  

  --! @brief GLFW has not been initialized.
  -- *
  -- *  This occurs if a GLFW function was called that must not be called unless the
  -- *  library is [initialized](@ref intro_init).
  -- *
  -- *  @analysis Application programmer error.  Initialize GLFW before calling any
  -- *  function that requires initialization.
  --  

  --! @brief No context is current for this thread.
  -- *
  -- *  This occurs if a GLFW function was called that needs and operates on the
  -- *  current OpenGL or OpenGL ES context but no context is current on the calling
  -- *  thread.  One such function is @ref glfwSwapInterval.
  -- *
  -- *  @analysis Application programmer error.  Ensure a context is current before
  -- *  calling functions that require a current context.
  --  

  --! @brief One of the arguments to the function was an invalid enum value.
  -- *
  -- *  One of the arguments to the function was an invalid enum value, for example
  -- *  requesting @ref GLFW_RED_BITS with @ref glfwGetWindowAttrib.
  -- *
  -- *  @analysis Application programmer error.  Fix the offending call.
  --  

  --! @brief One of the arguments to the function was an invalid value.
  -- *
  -- *  One of the arguments to the function was an invalid value, for example
  -- *  requesting a non-existent OpenGL or OpenGL ES version like 2.7.
  -- *
  -- *  Requesting a valid but unavailable OpenGL or OpenGL ES version will instead
  -- *  result in a @ref GLFW_VERSION_UNAVAILABLE error.
  -- *
  -- *  @analysis Application programmer error.  Fix the offending call.
  --  

  --! @brief A memory allocation failed.
  -- *
  -- *  A memory allocation failed.
  -- *
  -- *  @analysis A bug in GLFW or the underlying operating system.  Report the bug
  -- *  to our [issue tracker](https://github.com/glfw/glfw/issues).
  --  

  --! @brief GLFW could not find support for the requested API on the system.
  -- *
  -- *  GLFW could not find support for the requested API on the system.
  -- *
  -- *  @analysis The installed graphics driver does not support the requested
  -- *  API, or does not support it via the chosen context creation backend.
  -- *  Below are a few examples.
  -- *
  -- *  @par
  -- *  Some pre-installed Windows graphics drivers do not support OpenGL.  AMD only
  -- *  supports OpenGL ES via EGL, while Nvidia and Intel only support it via
  -- *  a WGL or GLX extension.  macOS does not provide OpenGL ES at all.  The Mesa
  -- *  EGL, OpenGL and OpenGL ES libraries do not interface with the Nvidia binary
  -- *  driver.  Older graphics drivers do not support Vulkan.
  --  

  --! @brief The requested OpenGL or OpenGL ES version is not available.
  -- *
  -- *  The requested OpenGL or OpenGL ES version (including any requested context
  -- *  or framebuffer hints) is not available on this machine.
  -- *
  -- *  @analysis The machine does not support your requirements.  If your
  -- *  application is sufficiently flexible, downgrade your requirements and try
  -- *  again.  Otherwise, inform the user that their machine does not match your
  -- *  requirements.
  -- *
  -- *  @par
  -- *  Future invalid OpenGL and OpenGL ES versions, for example OpenGL 4.8 if 5.0
  -- *  comes out before the 4.x series gets that far, also fail with this error and
  -- *  not @ref GLFW_INVALID_VALUE, because GLFW cannot know what future versions
  -- *  will exist.
  --  

  --! @brief A platform-specific error occurred that does not match any of the
  -- *  more specific categories.
  -- *
  -- *  A platform-specific error occurred that does not match any of the more
  -- *  specific categories.
  -- *
  -- *  @analysis A bug or configuration error in GLFW, the underlying operating
  -- *  system or its drivers, or a lack of required resources.  Report the issue to
  -- *  our [issue tracker](https://github.com/glfw/glfw/issues).
  --  

  --! @brief The requested format is not supported or available.
  -- *
  -- *  If emitted during window creation, the requested pixel format is not
  -- *  supported.
  -- *
  -- *  If emitted when querying the clipboard, the contents of the clipboard could
  -- *  not be converted to the requested format.
  -- *
  -- *  @analysis If emitted during window creation, one or more
  -- *  [hard constraints](@ref window_hints_hard) did not match any of the
  -- *  available pixel formats.  If your application is sufficiently flexible,
  -- *  downgrade your requirements and try again.  Otherwise, inform the user that
  -- *  their machine does not match your requirements.
  -- *
  -- *  @par
  -- *  If emitted when querying the clipboard, ignore the error or report it to
  -- *  the user, as appropriate.
  --  

  --! @brief The specified window does not have an OpenGL or OpenGL ES context.
  -- *
  -- *  A window that does not have an OpenGL or OpenGL ES context was passed to
  -- *  a function that requires it to have one.
  -- *
  -- *  @analysis Application programmer error.  Fix the offending call.
  --  

  --! @}  
  --! @addtogroup window
  -- *  @{  

  --! @brief Input focus window hint and attribute
  -- *
  -- *  Input focus [window hint](@ref GLFW_FOCUSED_hint) or
  -- *  [window attribute](@ref GLFW_FOCUSED_attrib).
  --  

  --! @brief Window iconification window attribute
  -- *
  -- *  Window iconification [window attribute](@ref GLFW_ICONIFIED_attrib).
  --  

  --! @brief Window resize-ability window hint and attribute
  -- *
  -- *  Window resize-ability [window hint](@ref GLFW_RESIZABLE_hint) and
  -- *  [window attribute](@ref GLFW_RESIZABLE_attrib).
  --  

  --! @brief Window visibility window hint and attribute
  -- *
  -- *  Window visibility [window hint](@ref GLFW_VISIBLE_hint) and
  -- *  [window attribute](@ref GLFW_VISIBLE_attrib).
  --  

  --! @brief Window decoration window hint and attribute
  -- *
  -- *  Window decoration [window hint](@ref GLFW_DECORATED_hint) and
  -- *  [window attribute](@ref GLFW_DECORATED_attrib).
  --  

  --! @brief Window auto-iconification window hint and attribute
  -- *
  -- *  Window auto-iconification [window hint](@ref GLFW_AUTO_ICONIFY_hint) and
  -- *  [window attribute](@ref GLFW_AUTO_ICONIFY_attrib).
  --  

  --! @brief Window decoration window hint and attribute
  -- *
  -- *  Window decoration [window hint](@ref GLFW_FLOATING_hint) and
  -- *  [window attribute](@ref GLFW_FLOATING_attrib).
  --  

  --! @brief Window maximization window hint and attribute
  -- *
  -- *  Window maximization [window hint](@ref GLFW_MAXIMIZED_hint) and
  -- *  [window attribute](@ref GLFW_MAXIMIZED_attrib).
  --  

  --! @brief Cursor centering window hint
  -- *
  -- *  Cursor centering [window hint](@ref GLFW_CENTER_CURSOR_hint).
  --  

  --! @brief Window framebuffer transparency hint and attribute
  -- *
  -- *  Window framebuffer transparency
  -- *  [window hint](@ref GLFW_TRANSPARENT_FRAMEBUFFER_hint) and
  -- *  [window attribute](@ref GLFW_TRANSPARENT_FRAMEBUFFER_attrib).
  --  

  --! @brief Mouse cursor hover window attribute.
  -- *
  -- *  Mouse cursor hover [window attribute](@ref GLFW_HOVERED_attrib).
  --  

  --! @brief Input focus on calling show window hint and attribute
  -- *
  -- *  Input focus [window hint](@ref GLFW_FOCUS_ON_SHOW_hint) or
  -- *  [window attribute](@ref GLFW_FOCUS_ON_SHOW_attrib).
  --  

  --! @brief Framebuffer bit depth hint.
  -- *
  -- *  Framebuffer bit depth [hint](@ref GLFW_RED_BITS).
  --  

  --! @brief Framebuffer bit depth hint.
  -- *
  -- *  Framebuffer bit depth [hint](@ref GLFW_GREEN_BITS).
  --  

  --! @brief Framebuffer bit depth hint.
  -- *
  -- *  Framebuffer bit depth [hint](@ref GLFW_BLUE_BITS).
  --  

  --! @brief Framebuffer bit depth hint.
  -- *
  -- *  Framebuffer bit depth [hint](@ref GLFW_ALPHA_BITS).
  --  

  --! @brief Framebuffer bit depth hint.
  -- *
  -- *  Framebuffer bit depth [hint](@ref GLFW_DEPTH_BITS).
  --  

  --! @brief Framebuffer bit depth hint.
  -- *
  -- *  Framebuffer bit depth [hint](@ref GLFW_STENCIL_BITS).
  --  

  --! @brief Framebuffer bit depth hint.
  -- *
  -- *  Framebuffer bit depth [hint](@ref GLFW_ACCUM_RED_BITS).
  --  

  --! @brief Framebuffer bit depth hint.
  -- *
  -- *  Framebuffer bit depth [hint](@ref GLFW_ACCUM_GREEN_BITS).
  --  

  --! @brief Framebuffer bit depth hint.
  -- *
  -- *  Framebuffer bit depth [hint](@ref GLFW_ACCUM_BLUE_BITS).
  --  

  --! @brief Framebuffer bit depth hint.
  -- *
  -- *  Framebuffer bit depth [hint](@ref GLFW_ACCUM_ALPHA_BITS).
  --  

  --! @brief Framebuffer auxiliary buffer hint.
  -- *
  -- *  Framebuffer auxiliary buffer [hint](@ref GLFW_AUX_BUFFERS).
  --  

  --! @brief OpenGL stereoscopic rendering hint.
  -- *
  -- *  OpenGL stereoscopic rendering [hint](@ref GLFW_STEREO).
  --  

  --! @brief Framebuffer MSAA samples hint.
  -- *
  -- *  Framebuffer MSAA samples [hint](@ref GLFW_SAMPLES).
  --  

  --! @brief Framebuffer sRGB hint.
  -- *
  -- *  Framebuffer sRGB [hint](@ref GLFW_SRGB_CAPABLE).
  --  

  --! @brief Monitor refresh rate hint.
  -- *
  -- *  Monitor refresh rate [hint](@ref GLFW_REFRESH_RATE).
  --  

  --! @brief Framebuffer double buffering hint.
  -- *
  -- *  Framebuffer double buffering [hint](@ref GLFW_DOUBLEBUFFER).
  --  

  --! @brief Context client API hint and attribute.
  -- *
  -- *  Context client API [hint](@ref GLFW_CLIENT_API_hint) and
  -- *  [attribute](@ref GLFW_CLIENT_API_attrib).
  --  

  --! @brief Context client API major version hint and attribute.
  -- *
  -- *  Context client API major version [hint](@ref GLFW_CONTEXT_VERSION_MAJOR_hint)
  -- *  and [attribute](@ref GLFW_CONTEXT_VERSION_MAJOR_attrib).
  --  

  --! @brief Context client API minor version hint and attribute.
  -- *
  -- *  Context client API minor version [hint](@ref GLFW_CONTEXT_VERSION_MINOR_hint)
  -- *  and [attribute](@ref GLFW_CONTEXT_VERSION_MINOR_attrib).
  --  

  --! @brief Context client API revision number hint and attribute.
  -- *
  -- *  Context client API revision number
  -- *  [attribute](@ref GLFW_CONTEXT_REVISION_attrib).
  --  

  --! @brief Context robustness hint and attribute.
  -- *
  -- *  Context client API revision number [hint](@ref GLFW_CONTEXT_ROBUSTNESS_hint)
  -- *  and [attribute](@ref GLFW_CONTEXT_ROBUSTNESS_attrib).
  --  

  --! @brief OpenGL forward-compatibility hint and attribute.
  -- *
  -- *  OpenGL forward-compatibility [hint](@ref GLFW_OPENGL_FORWARD_COMPAT_hint)
  -- *  and [attribute](@ref GLFW_OPENGL_FORWARD_COMPAT_attrib).
  --  

  --! @brief OpenGL debug context hint and attribute.
  -- *
  -- *  OpenGL debug context [hint](@ref GLFW_OPENGL_DEBUG_CONTEXT_hint) and
  -- *  [attribute](@ref GLFW_OPENGL_DEBUG_CONTEXT_attrib).
  --  

  --! @brief OpenGL profile hint and attribute.
  -- *
  -- *  OpenGL profile [hint](@ref GLFW_OPENGL_PROFILE_hint) and
  -- *  [attribute](@ref GLFW_OPENGL_PROFILE_attrib).
  --  

  --! @brief Context flush-on-release hint and attribute.
  -- *
  -- *  Context flush-on-release [hint](@ref GLFW_CONTEXT_RELEASE_BEHAVIOR_hint) and
  -- *  [attribute](@ref GLFW_CONTEXT_RELEASE_BEHAVIOR_attrib).
  --  

  --! @brief Context error suppression hint and attribute.
  -- *
  -- *  Context error suppression [hint](@ref GLFW_CONTEXT_NO_ERROR_hint) and
  -- *  [attribute](@ref GLFW_CONTEXT_NO_ERROR_attrib).
  --  

  --! @brief Context creation API hint and attribute.
  -- *
  -- *  Context creation API [hint](@ref GLFW_CONTEXT_CREATION_API_hint) and
  -- *  [attribute](@ref GLFW_CONTEXT_CREATION_API_attrib).
  --  

  --! @brief Window content area scaling window
  -- *  [window hint](@ref GLFW_SCALE_TO_MONITOR).
  --  

  --! @brief macOS specific
  -- *  [window hint](@ref GLFW_COCOA_RETINA_FRAMEBUFFER_hint).
  --  

  --! @brief macOS specific
  -- *  [window hint](@ref GLFW_COCOA_FRAME_NAME_hint).
  --  

  --! @brief macOS specific
  -- *  [window hint](@ref GLFW_COCOA_GRAPHICS_SWITCHING_hint).
  --  

  --! @brief X11 specific
  -- *  [window hint](@ref GLFW_X11_CLASS_NAME_hint).
  --  

  --! @brief X11 specific
  -- *  [window hint](@ref GLFW_X11_CLASS_NAME_hint).
  --  

  --! @}  
  --! @defgroup shapes Standard cursor shapes
  -- *  @brief Standard system cursor shapes.
  -- *
  -- *  See [standard cursor creation](@ref cursor_standard) for how these are used.
  -- *
  -- *  @ingroup input
  -- *  @{  

  --! @brief The regular arrow cursor shape.
  -- *
  -- *  The regular arrow cursor.
  --  

  --! @brief The text input I-beam cursor shape.
  -- *
  -- *  The text input I-beam cursor shape.
  --  

  --! @brief The crosshair shape.
  -- *
  -- *  The crosshair shape.
  --  

  --! @brief The hand shape.
  -- *
  -- *  The hand shape.
  --  

  --! @brief The horizontal resize arrow shape.
  -- *
  -- *  The horizontal resize arrow shape.
  --  

  --! @brief The vertical resize arrow shape.
  -- *
  -- *  The vertical resize arrow shape.
  --  

  --! @}  
  --! @addtogroup init
  -- *  @{  

  --! @brief Joystick hat buttons init hint.
  -- *
  -- *  Joystick hat buttons [init hint](@ref GLFW_JOYSTICK_HAT_BUTTONS).
  --  

  --! @brief macOS specific init hint.
  -- *
  -- *  macOS specific [init hint](@ref GLFW_COCOA_CHDIR_RESOURCES_hint).
  --  

  --! @brief macOS specific init hint.
  -- *
  -- *  macOS specific [init hint](@ref GLFW_COCOA_MENUBAR_hint).
  --  

  --! @}  
  --************************************************************************
  -- * GLFW API types
  -- ************************************************************************ 

  --! @brief Client API function pointer type.
  -- *
  -- *  Generic function pointer used for returning client API function pointers
  -- *  without forcing a cast from a regular pointer.
  -- *
  -- *  @sa @ref context_glext
  -- *  @sa @ref glfwGetProcAddress
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup context
  --  

   type GLFWglproc is access procedure
   with Convention => C;  -- ../include/GLFW/glfw3.h:1119

  --! @brief Vulkan API function pointer type.
  -- *
  -- *  Generic function pointer used for returning Vulkan API function pointers
  -- *  without forcing a cast from a regular pointer.
  -- *
  -- *  @sa @ref vulkan_proc
  -- *  @sa @ref glfwGetInstanceProcAddress
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup vulkan
  --  

   type GLFWvkproc is access procedure
   with Convention => C;  -- ../include/GLFW/glfw3.h:1133

  --! @brief Opaque monitor object.
  -- *
  -- *  Opaque monitor object.
  -- *
  -- *  @see @ref monitor_object
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

   type GLFWmonitor is null record;   -- incomplete struct

  --! @brief Opaque window object.
  -- *
  -- *  Opaque window object.
  -- *
  -- *  @see @ref window_object
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   type GLFWwindow is null record;   -- incomplete struct

  --! @brief Opaque cursor object.
  -- *
  -- *  Opaque cursor object.
  -- *
  -- *  @see @ref cursor_object
  -- *
  -- *  @since Added in version 3.1.
  -- *
  -- *  @ingroup input
  --  

   type GLFWcursor is null record;   -- incomplete struct

  --! @brief The function pointer type for error callbacks.
  -- *
  -- *  This is the function pointer type for error callbacks.  An error callback
  -- *  function has the following signature:
  -- *  @code
  -- *  void callback_name(int error_code, const char* description)
  -- *  @endcode
  -- *
  -- *  @param[in] error_code An [error code](@ref errors).  Future releases may add
  -- *  more error codes.
  -- *  @param[in] description A UTF-8 encoded string describing the error.
  -- *
  -- *  @pointer_lifetime The error description string is valid until the callback
  -- *  function returns.
  -- *
  -- *  @sa @ref error_handling
  -- *  @sa @ref glfwSetErrorCallback
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup init
  --  

   type GLFWerrorfun is access procedure (arg1 : int; arg2 : Interfaces.C.Strings.chars_ptr)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1193

  --! @brief The function pointer type for window position callbacks.
  -- *
  -- *  This is the function pointer type for window position callbacks.  A window
  -- *  position callback function has the following signature:
  -- *  @code
  -- *  void callback_name(GLFWwindow* window, int xpos, int ypos)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that was moved.
  -- *  @param[in] xpos The new x-coordinate, in screen coordinates, of the
  -- *  upper-left corner of the content area of the window.
  -- *  @param[in] ypos The new y-coordinate, in screen coordinates, of the
  -- *  upper-left corner of the content area of the window.
  -- *
  -- *  @sa @ref window_pos
  -- *  @sa @ref glfwSetWindowPosCallback
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   type GLFWwindowposfun is access procedure
        (arg1 : access GLFWwindow;
         arg2 : int;
         arg3 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1216

  --! @brief The function pointer type for window size callbacks.
  -- *
  -- *  This is the function pointer type for window size callbacks.  A window size
  -- *  callback function has the following signature:
  -- *  @code
  -- *  void callback_name(GLFWwindow* window, int width, int height)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that was resized.
  -- *  @param[in] width The new width, in screen coordinates, of the window.
  -- *  @param[in] height The new height, in screen coordinates, of the window.
  -- *
  -- *  @sa @ref window_size
  -- *  @sa @ref glfwSetWindowSizeCallback
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup window
  --  

   type GLFWwindowsizefun is access procedure
        (arg1 : access GLFWwindow;
         arg2 : int;
         arg3 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1238

  --! @brief The function pointer type for window close callbacks.
  -- *
  -- *  This is the function pointer type for window close callbacks.  A window
  -- *  close callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that the user attempted to close.
  -- *
  -- *  @sa @ref window_close
  -- *  @sa @ref glfwSetWindowCloseCallback
  -- *
  -- *  @since Added in version 2.5.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup window
  --  

   type GLFWwindowclosefun is access procedure (arg1 : access GLFWwindow)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1258

  --! @brief The function pointer type for window content refresh callbacks.
  -- *
  -- *  This is the function pointer type for window content refresh callbacks.
  -- *  A window content refresh callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window);
  -- *  @endcode
  -- *
  -- *  @param[in] window The window whose content needs to be refreshed.
  -- *
  -- *  @sa @ref window_refresh
  -- *  @sa @ref glfwSetWindowRefreshCallback
  -- *
  -- *  @since Added in version 2.5.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup window
  --  

   type GLFWwindowrefreshfun is access procedure (arg1 : access GLFWwindow)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1278

  --! @brief The function pointer type for window focus callbacks.
  -- *
  -- *  This is the function pointer type for window focus callbacks.  A window
  -- *  focus callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int focused)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that gained or lost input focus.
  -- *  @param[in] focused `GLFW_TRUE` if the window was given input focus, or
  -- *  `GLFW_FALSE` if it lost it.
  -- *
  -- *  @sa @ref window_focus
  -- *  @sa @ref glfwSetWindowFocusCallback
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   type GLFWwindowfocusfun is access procedure (arg1 : access GLFWwindow; arg2 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1299

  --! @brief The function pointer type for window iconify callbacks.
  -- *
  -- *  This is the function pointer type for window iconify callbacks.  A window
  -- *  iconify callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int iconified)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that was iconified or restored.
  -- *  @param[in] iconified `GLFW_TRUE` if the window was iconified, or
  -- *  `GLFW_FALSE` if it was restored.
  -- *
  -- *  @sa @ref window_iconify
  -- *  @sa @ref glfwSetWindowIconifyCallback
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   type GLFWwindowiconifyfun is access procedure (arg1 : access GLFWwindow; arg2 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1320

  --! @brief The function pointer type for window maximize callbacks.
  -- *
  -- *  This is the function pointer type for window maximize callbacks.  A window
  -- *  maximize callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int maximized)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that was maximized or restored.
  -- *  @param[in] iconified `GLFW_TRUE` if the window was maximized, or
  -- *  `GLFW_FALSE` if it was restored.
  -- *
  -- *  @sa @ref window_maximize
  -- *  @sa glfwSetWindowMaximizeCallback
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup window
  --  

   type GLFWwindowmaximizefun is access procedure (arg1 : access GLFWwindow; arg2 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1341

  --! @brief The function pointer type for framebuffer size callbacks.
  -- *
  -- *  This is the function pointer type for framebuffer size callbacks.
  -- *  A framebuffer size callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int width, int height)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window whose framebuffer was resized.
  -- *  @param[in] width The new width, in pixels, of the framebuffer.
  -- *  @param[in] height The new height, in pixels, of the framebuffer.
  -- *
  -- *  @sa @ref window_fbsize
  -- *  @sa @ref glfwSetFramebufferSizeCallback
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   type GLFWframebuffersizefun is access procedure
        (arg1 : access GLFWwindow;
         arg2 : int;
         arg3 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1362

  --! @brief The function pointer type for window content scale callbacks.
  -- *
  -- *  This is the function pointer type for window content scale callbacks.
  -- *  A window content scale callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, float xscale, float yscale)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window whose content scale changed.
  -- *  @param[in] xscale The new x-axis content scale of the window.
  -- *  @param[in] yscale The new y-axis content scale of the window.
  -- *
  -- *  @sa @ref window_scale
  -- *  @sa @ref glfwSetWindowContentScaleCallback
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup window
  --  

   type GLFWwindowcontentscalefun is access procedure
        (arg1 : access GLFWwindow;
         arg2 : float;
         arg3 : float)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1383

  --! @brief The function pointer type for mouse button callbacks.
  -- *
  -- *  This is the function pointer type for mouse button callback functions.
  -- *  A mouse button callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int button, int action, int mods)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that received the event.
  -- *  @param[in] button The [mouse button](@ref buttons) that was pressed or
  -- *  released.
  -- *  @param[in] action One of `GLFW_PRESS` or `GLFW_RELEASE`.  Future releases
  -- *  may add more actions.
  -- *  @param[in] mods Bit field describing which [modifier keys](@ref mods) were
  -- *  held down.
  -- *
  -- *  @sa @ref input_mouse_button
  -- *  @sa @ref glfwSetMouseButtonCallback
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle and modifier mask parameters.
  -- *
  -- *  @ingroup input
  --  

   type GLFWmousebuttonfun is access procedure
        (arg1 : access GLFWwindow;
         arg2 : int;
         arg3 : int;
         arg4 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1409

  --! @brief The function pointer type for cursor position callbacks.
  -- *
  -- *  This is the function pointer type for cursor position callbacks.  A cursor
  -- *  position callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, double xpos, double ypos);
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that received the event.
  -- *  @param[in] xpos The new cursor x-coordinate, relative to the left edge of
  -- *  the content area.
  -- *  @param[in] ypos The new cursor y-coordinate, relative to the top edge of the
  -- *  content area.
  -- *
  -- *  @sa @ref cursor_pos
  -- *  @sa @ref glfwSetCursorPosCallback
  -- *
  -- *  @since Added in version 3.0.  Replaces `GLFWmouseposfun`.
  -- *
  -- *  @ingroup input
  --  

   type GLFWcursorposfun is access procedure
        (arg1 : access GLFWwindow;
         arg2 : double;
         arg3 : double)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1432

  --! @brief The function pointer type for cursor enter/leave callbacks.
  -- *
  -- *  This is the function pointer type for cursor enter/leave callbacks.
  -- *  A cursor enter/leave callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int entered)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that received the event.
  -- *  @param[in] entered `GLFW_TRUE` if the cursor entered the window's content
  -- *  area, or `GLFW_FALSE` if it left it.
  -- *
  -- *  @sa @ref cursor_enter
  -- *  @sa @ref glfwSetCursorEnterCallback
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup input
  --  

   type GLFWcursorenterfun is access procedure (arg1 : access GLFWwindow; arg2 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1453

  --! @brief The function pointer type for scroll callbacks.
  -- *
  -- *  This is the function pointer type for scroll callbacks.  A scroll callback
  -- *  function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, double xoffset, double yoffset)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that received the event.
  -- *  @param[in] xoffset The scroll offset along the x-axis.
  -- *  @param[in] yoffset The scroll offset along the y-axis.
  -- *
  -- *  @sa @ref scrolling
  -- *  @sa @ref glfwSetScrollCallback
  -- *
  -- *  @since Added in version 3.0.  Replaces `GLFWmousewheelfun`.
  -- *
  -- *  @ingroup input
  --  

   type GLFWscrollfun is access procedure
        (arg1 : access GLFWwindow;
         arg2 : double;
         arg3 : double)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1474

  --! @brief The function pointer type for keyboard key callbacks.
  -- *
  -- *  This is the function pointer type for keyboard key callbacks.  A keyboard
  -- *  key callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int key, int scancode, int action, int mods)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that received the event.
  -- *  @param[in] key The [keyboard key](@ref keys) that was pressed or released.
  -- *  @param[in] scancode The system-specific scancode of the key.
  -- *  @param[in] action `GLFW_PRESS`, `GLFW_RELEASE` or `GLFW_REPEAT`.  Future
  -- *  releases may add more actions.
  -- *  @param[in] mods Bit field describing which [modifier keys](@ref mods) were
  -- *  held down.
  -- *
  -- *  @sa @ref input_key
  -- *  @sa @ref glfwSetKeyCallback
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle, scancode and modifier mask parameters.
  -- *
  -- *  @ingroup input
  --  

   type GLFWkeyfun is access procedure
        (arg1 : access GLFWwindow;
         arg2 : int;
         arg3 : int;
         arg4 : int;
         arg5 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1500

  --! @brief The function pointer type for Unicode character callbacks.
  -- *
  -- *  This is the function pointer type for Unicode character callbacks.
  -- *  A Unicode character callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, unsigned int codepoint)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that received the event.
  -- *  @param[in] codepoint The Unicode code point of the character.
  -- *
  -- *  @sa @ref input_char
  -- *  @sa @ref glfwSetCharCallback
  -- *
  -- *  @since Added in version 2.4.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup input
  --  

   type GLFWcharfun is access procedure (arg1 : access GLFWwindow; arg2 : unsigned)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1521

  --! @brief The function pointer type for Unicode character with modifiers
  -- *  callbacks.
  -- *
  -- *  This is the function pointer type for Unicode character with modifiers
  -- *  callbacks.  It is called for each input character, regardless of what
  -- *  modifier keys are held down.  A Unicode character with modifiers callback
  -- *  function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, unsigned int codepoint, int mods)
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that received the event.
  -- *  @param[in] codepoint The Unicode code point of the character.
  -- *  @param[in] mods Bit field describing which [modifier keys](@ref mods) were
  -- *  held down.
  -- *
  -- *  @sa @ref input_char
  -- *  @sa @ref glfwSetCharModsCallback
  -- *
  -- *  @deprecated Scheduled for removal in version 4.0.
  -- *
  -- *  @since Added in version 3.1.
  -- *
  -- *  @ingroup input
  --  

   type GLFWcharmodsfun is access procedure
        (arg1 : access GLFWwindow;
         arg2 : unsigned;
         arg3 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1548

  --! @brief The function pointer type for path drop callbacks.
  -- *
  -- *  This is the function pointer type for path drop callbacks.  A path drop
  -- *  callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int path_count, const char* paths[])
  -- *  @endcode
  -- *
  -- *  @param[in] window The window that received the event.
  -- *  @param[in] path_count The number of dropped paths.
  -- *  @param[in] paths The UTF-8 encoded file and/or directory path names.
  -- *
  -- *  @pointer_lifetime The path array and its strings are valid until the
  -- *  callback function returns.
  -- *
  -- *  @sa @ref path_drop
  -- *  @sa @ref glfwSetDropCallback
  -- *
  -- *  @since Added in version 3.1.
  -- *
  -- *  @ingroup input
  --  

   type GLFWdropfun is access procedure
        (arg1 : access GLFWwindow;
         arg2 : int;
         arg3 : System.Address)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1572

  --! @brief The function pointer type for monitor configuration callbacks.
  -- *
  -- *  This is the function pointer type for monitor configuration callbacks.
  -- *  A monitor callback function has the following signature:
  -- *  @code
  -- *  void function_name(GLFWmonitor* monitor, int event)
  -- *  @endcode
  -- *
  -- *  @param[in] monitor The monitor that was connected or disconnected.
  -- *  @param[in] event One of `GLFW_CONNECTED` or `GLFW_DISCONNECTED`.  Future
  -- *  releases may add more events.
  -- *
  -- *  @sa @ref monitor_event
  -- *  @sa @ref glfwSetMonitorCallback
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

   type GLFWmonitorfun is access procedure (arg1 : access GLFWmonitor; arg2 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1593

  --! @brief The function pointer type for joystick configuration callbacks.
  -- *
  -- *  This is the function pointer type for joystick configuration callbacks.
  -- *  A joystick configuration callback function has the following signature:
  -- *  @code
  -- *  void function_name(int jid, int event)
  -- *  @endcode
  -- *
  -- *  @param[in] jid The joystick that was connected or disconnected.
  -- *  @param[in] event One of `GLFW_CONNECTED` or `GLFW_DISCONNECTED`.  Future
  -- *  releases may add more events.
  -- *
  -- *  @sa @ref joystick_event
  -- *  @sa @ref glfwSetJoystickCallback
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup input
  --  

   type GLFWjoystickfun is access procedure (arg1 : int; arg2 : int)
   with Convention => C;  -- ../include/GLFW/glfw3.h:1614

  --! @brief Video mode type.
  -- *
  -- *  This describes a single video mode.
  -- *
  -- *  @sa @ref monitor_modes
  -- *  @sa @ref glfwGetVideoMode
  -- *  @sa @ref glfwGetVideoModes
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added refresh rate member.
  -- *
  -- *  @ingroup monitor
  --  

  --! The width, in screen coordinates, of the video mode.
  --      

   type GLFWvidmode is record
      width : aliased int;  -- ../include/GLFW/glfw3.h:1633
      height : aliased int;  -- ../include/GLFW/glfw3.h:1636
      redBits : aliased int;  -- ../include/GLFW/glfw3.h:1639
      greenBits : aliased int;  -- ../include/GLFW/glfw3.h:1642
      blueBits : aliased int;  -- ../include/GLFW/glfw3.h:1645
      refreshRate : aliased int;  -- ../include/GLFW/glfw3.h:1648
   end record
   with Convention => C_Pass_By_Copy;  -- ../include/GLFW/glfw3.h:1629

  --! The height, in screen coordinates, of the video mode.
  --      

  --! The bit depth of the red channel of the video mode.
  --      

  --! The bit depth of the green channel of the video mode.
  --      

  --! The bit depth of the blue channel of the video mode.
  --      

  --! The refresh rate, in Hz, of the video mode.
  --      

  --! @brief Gamma ramp.
  -- *
  -- *  This describes the gamma ramp for a monitor.
  -- *
  -- *  @sa @ref monitor_gamma
  -- *  @sa @ref glfwGetGammaRamp
  -- *  @sa @ref glfwSetGammaRamp
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

  --! An array of value describing the response of the red channel.
  --      

   type GLFWgammaramp is record
      red : access unsigned_short;  -- ../include/GLFW/glfw3.h:1667
      green : access unsigned_short;  -- ../include/GLFW/glfw3.h:1670
      blue : access unsigned_short;  -- ../include/GLFW/glfw3.h:1673
      size : aliased unsigned;  -- ../include/GLFW/glfw3.h:1676
   end record
   with Convention => C_Pass_By_Copy;  -- ../include/GLFW/glfw3.h:1663

  --! An array of value describing the response of the green channel.
  --      

  --! An array of value describing the response of the blue channel.
  --      

  --! The number of elements in each array.
  --      

  --! @brief Image data.
  -- *
  -- *  This describes a single 2D image.  See the documentation for each related
  -- *  function what the expected pixel format is.
  -- *
  -- *  @sa @ref cursor_custom
  -- *  @sa @ref window_icon
  -- *
  -- *  @since Added in version 2.1.
  -- *  @glfw3 Removed format and bytes-per-pixel members.
  -- *
  -- *  @ingroup window
  --  

  --! The width, in pixels, of this image.
  --      

   type GLFWimage is record
      width : aliased int;  -- ../include/GLFW/glfw3.h:1696
      height : aliased int;  -- ../include/GLFW/glfw3.h:1699
      pixels : access unsigned_char;  -- ../include/GLFW/glfw3.h:1702
   end record
   with Convention => C_Pass_By_Copy;  -- ../include/GLFW/glfw3.h:1692

  --! The height, in pixels, of this image.
  --      

  --! The pixel data of this image, arranged left-to-right, top-to-bottom.
  --      

  --! @brief Gamepad input state
  -- *
  -- *  This describes the input state of a gamepad.
  -- *
  -- *  @sa @ref gamepad
  -- *  @sa @ref glfwGetGamepadState
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup input
  --  

  --! The states of each [gamepad button](@ref gamepad_buttons), `GLFW_PRESS`
  --     *  or `GLFW_RELEASE`.
  --      

   type GLFWgamepadstate_buttons_array is array (0 .. 14) of aliased unsigned_char;
   type GLFWgamepadstate_axes_array is array (0 .. 5) of aliased float;
   type GLFWgamepadstate is record
      buttons : aliased GLFWgamepadstate_buttons_array;  -- ../include/GLFW/glfw3.h:1721
      axes : aliased GLFWgamepadstate_axes_array;  -- ../include/GLFW/glfw3.h:1725
   end record
   with Convention => C_Pass_By_Copy;  -- ../include/GLFW/glfw3.h:1716

  --! The states of each [gamepad axis](@ref gamepad_axes), in the range -1.0
  --     *  to 1.0 inclusive.
  --      

  --************************************************************************
  -- * GLFW API functions
  -- ************************************************************************ 

  --! @brief Initializes the GLFW library.
  -- *
  -- *  This function initializes the GLFW library.  Before most GLFW functions can
  -- *  be used, GLFW must be initialized, and before an application terminates GLFW
  -- *  should be terminated in order to free any resources allocated during or
  -- *  after initialization.
  -- *
  -- *  If this function fails, it calls @ref glfwTerminate before returning.  If it
  -- *  succeeds, you should call @ref glfwTerminate before the application exits.
  -- *
  -- *  Additional calls to this function after successful initialization but before
  -- *  termination will return `GLFW_TRUE` immediately.
  -- *
  -- *  @return `GLFW_TRUE` if successful, or `GLFW_FALSE` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @macos This function will change the current directory of the
  -- *  application to the `Contents/Resources` subdirectory of the application's
  -- *  bundle, if present.  This can be disabled with the @ref
  -- *  GLFW_COCOA_CHDIR_RESOURCES init hint.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref intro_init
  -- *  @sa @ref glfwTerminate
  -- *
  -- *  @since Added in version 1.0.
  -- *
  -- *  @ingroup init
  --  

   function glfwInit return int  -- ../include/GLFW/glfw3.h:1765
   with Import => True, 
        Convention => C, 
        External_Name => "glfwInit";

  --! @brief Terminates the GLFW library.
  -- *
  -- *  This function destroys all remaining windows and cursors, restores any
  -- *  modified gamma ramps and frees any other allocated resources.  Once this
  -- *  function is called, you must again call @ref glfwInit successfully before
  -- *  you will be able to use most GLFW functions.
  -- *
  -- *  If GLFW has been successfully initialized, this function should be called
  -- *  before the application exits.  If initialization fails, there is no need to
  -- *  call this function, as it is called by @ref glfwInit before it returns
  -- *  failure.
  -- *
  -- *  @errors Possible errors include @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark This function may be called before @ref glfwInit.
  -- *
  -- *  @warning The contexts of any remaining windows must not be current on any
  -- *  other thread when this function is called.
  -- *
  -- *  @reentrancy This function must not be called from a callback.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref intro_init
  -- *  @sa @ref glfwInit
  -- *
  -- *  @since Added in version 1.0.
  -- *
  -- *  @ingroup init
  --  

   procedure glfwTerminate  -- ../include/GLFW/glfw3.h:1797
   with Import => True, 
        Convention => C, 
        External_Name => "glfwTerminate";

  --! @brief Sets the specified init hint to the desired value.
  -- *
  -- *  This function sets hints for the next initialization of GLFW.
  -- *
  -- *  The values you set hints to are never reset by GLFW, but they only take
  -- *  effect during initialization.  Once GLFW has been initialized, any values
  -- *  you set will be ignored until the library is terminated and initialized
  -- *  again.
  -- *
  -- *  Some hints are platform specific.  These may be set on any platform but they
  -- *  will only affect their specific platform.  Other platforms will ignore them.
  -- *  Setting these hints requires no platform specific headers or functions.
  -- *
  -- *  @param[in] hint The [init hint](@ref init_hints) to set.
  -- *  @param[in] value The new value of the init hint.
  -- *
  -- *  @errors Possible errors include @ref GLFW_INVALID_ENUM and @ref
  -- *  GLFW_INVALID_VALUE.
  -- *
  -- *  @remarks This function may be called before @ref glfwInit.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa init_hints
  -- *  @sa glfwInit
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup init
  --  

   procedure glfwInitHint (hint : int; value : int)  -- ../include/GLFW/glfw3.h:1829
   with Import => True, 
        Convention => C, 
        External_Name => "glfwInitHint";

  --! @brief Retrieves the version of the GLFW library.
  -- *
  -- *  This function retrieves the major, minor and revision numbers of the GLFW
  -- *  library.  It is intended for when you are using GLFW as a shared library and
  -- *  want to ensure that you are using the minimum required version.
  -- *
  -- *  Any or all of the version arguments may be `NULL`.
  -- *
  -- *  @param[out] major Where to store the major version number, or `NULL`.
  -- *  @param[out] minor Where to store the minor version number, or `NULL`.
  -- *  @param[out] rev Where to store the revision number, or `NULL`.
  -- *
  -- *  @errors None.
  -- *
  -- *  @remark This function may be called before @ref glfwInit.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref intro_version
  -- *  @sa @ref glfwGetVersionString
  -- *
  -- *  @since Added in version 1.0.
  -- *
  -- *  @ingroup init
  --  

   procedure glfwGetVersion
     (major : access int;
      minor : access int;
      rev : access int)  -- ../include/GLFW/glfw3.h:1856
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetVersion";

  --! @brief Returns a string describing the compile-time configuration.
  -- *
  -- *  This function returns the compile-time generated
  -- *  [version string](@ref intro_version_string) of the GLFW library binary.  It
  -- *  describes the version, platform, compiler and any platform-specific
  -- *  compile-time options.  It should not be confused with the OpenGL or OpenGL
  -- *  ES version string, queried with `glGetString`.
  -- *
  -- *  __Do not use the version string__ to parse the GLFW library version.  The
  -- *  @ref glfwGetVersion function provides the version of the running library
  -- *  binary in numerical format.
  -- *
  -- *  @return The ASCII encoded GLFW version string.
  -- *
  -- *  @errors None.
  -- *
  -- *  @remark This function may be called before @ref glfwInit.
  -- *
  -- *  @pointer_lifetime The returned string is static and compile-time generated.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref intro_version
  -- *  @sa @ref glfwGetVersion
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup init
  --  

   function glfwGetVersionString return Interfaces.C.Strings.chars_ptr  -- ../include/GLFW/glfw3.h:1887
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetVersionString";

  --! @brief Returns and clears the last error for the calling thread.
  -- *
  -- *  This function returns and clears the [error code](@ref errors) of the last
  -- *  error that occurred on the calling thread, and optionally a UTF-8 encoded
  -- *  human-readable description of it.  If no error has occurred since the last
  -- *  call, it returns @ref GLFW_NO_ERROR (zero) and the description pointer is
  -- *  set to `NULL`.
  -- *
  -- *  @param[in] description Where to store the error description pointer, or `NULL`.
  -- *  @return The last error code for the calling thread, or @ref GLFW_NO_ERROR
  -- *  (zero).
  -- *
  -- *  @errors None.
  -- *
  -- *  @pointer_lifetime The returned string is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is guaranteed to be valid only until the
  -- *  next error occurs or the library is terminated.
  -- *
  -- *  @remark This function may be called before @ref glfwInit.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref error_handling
  -- *  @sa @ref glfwSetErrorCallback
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup init
  --  

   function glfwGetError (description : System.Address) return int  -- ../include/GLFW/glfw3.h:1918
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetError";

  --! @brief Sets the error callback.
  -- *
  -- *  This function sets the error callback, which is called with an error code
  -- *  and a human-readable description each time a GLFW error occurs.
  -- *
  -- *  The error code is set before the callback is called.  Calling @ref
  -- *  glfwGetError from the error callback will return the same value as the error
  -- *  code argument.
  -- *
  -- *  The error callback is called on the thread where the error occurred.  If you
  -- *  are using GLFW from multiple threads, your error callback needs to be
  -- *  written accordingly.
  -- *
  -- *  Because the description string may have been generated specifically for that
  -- *  error, it is not guaranteed to be valid after the callback has returned.  If
  -- *  you wish to use it after the callback returns, you need to make a copy.
  -- *
  -- *  Once set, the error callback remains set even after the library has been
  -- *  terminated.
  -- *
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set.
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void callback_name(int error_code, const char* description)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [callback pointer type](@ref GLFWerrorfun).
  -- *
  -- *  @errors None.
  -- *
  -- *  @remark This function may be called before @ref glfwInit.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref error_handling
  -- *  @sa @ref glfwGetError
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup init
  --  

   function glfwSetErrorCallback (callback : GLFWerrorfun) return GLFWerrorfun  -- ../include/GLFW/glfw3.h:1964
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetErrorCallback";

  --! @brief Returns the currently connected monitors.
  -- *
  -- *  This function returns an array of handles for all currently connected
  -- *  monitors.  The primary monitor is always first in the returned array.  If no
  -- *  monitors were found, this function returns `NULL`.
  -- *
  -- *  @param[out] count Where to store the number of monitors in the returned
  -- *  array.  This is set to zero if an error occurred.
  -- *  @return An array of monitor handles, or `NULL` if no monitors were found or
  -- *  if an [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @pointer_lifetime The returned array is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is guaranteed to be valid only until the
  -- *  monitor configuration changes or the library is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_monitors
  -- *  @sa @ref monitor_event
  -- *  @sa @ref glfwGetPrimaryMonitor
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

   function glfwGetMonitors (count : access int) return System.Address  -- ../include/GLFW/glfw3.h:1993
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetMonitors";

  --! @brief Returns the primary monitor.
  -- *
  -- *  This function returns the primary monitor.  This is usually the monitor
  -- *  where elements like the task bar or global menu bar are located.
  -- *
  -- *  @return The primary monitor, or `NULL` if no monitors were found or if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @remark The primary monitor is always first in the array returned by @ref
  -- *  glfwGetMonitors.
  -- *
  -- *  @sa @ref monitor_monitors
  -- *  @sa @ref glfwGetMonitors
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

   function glfwGetPrimaryMonitor return access GLFWmonitor  -- ../include/GLFW/glfw3.h:2017
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetPrimaryMonitor";

  --! @brief Returns the position of the monitor's viewport on the virtual screen.
  -- *
  -- *  This function returns the position, in screen coordinates, of the upper-left
  -- *  corner of the specified monitor.
  -- *
  -- *  Any or all of the position arguments may be `NULL`.  If an error occurs, all
  -- *  non-`NULL` position arguments will be set to zero.
  -- *
  -- *  @param[in] monitor The monitor to query.
  -- *  @param[out] xpos Where to store the monitor x-coordinate, or `NULL`.
  -- *  @param[out] ypos Where to store the monitor y-coordinate, or `NULL`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_properties
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

   procedure glfwGetMonitorPos
     (monitor : access GLFWmonitor;
      xpos : access int;
      ypos : access int)  -- ../include/GLFW/glfw3.h:2042
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetMonitorPos";

  --! @brief Retrieves the work area of the monitor.
  -- *
  -- *  This function returns the position, in screen coordinates, of the upper-left
  -- *  corner of the work area of the specified monitor along with the work area
  -- *  size in screen coordinates. The work area is defined as the area of the
  -- *  monitor not occluded by the operating system task bar where present. If no
  -- *  task bar exists then the work area is the monitor resolution in screen
  -- *  coordinates.
  -- *
  -- *  Any or all of the position and size arguments may be `NULL`.  If an error
  -- *  occurs, all non-`NULL` position and size arguments will be set to zero.
  -- *
  -- *  @param[in] monitor The monitor to query.
  -- *  @param[out] xpos Where to store the monitor x-coordinate, or `NULL`.
  -- *  @param[out] ypos Where to store the monitor y-coordinate, or `NULL`.
  -- *  @param[out] width Where to store the monitor width, or `NULL`.
  -- *  @param[out] height Where to store the monitor height, or `NULL`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_workarea
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup monitor
  --  

   procedure glfwGetMonitorWorkarea
     (monitor : access GLFWmonitor;
      xpos : access int;
      ypos : access int;
      width : access int;
      height : access int)  -- ../include/GLFW/glfw3.h:2073
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetMonitorWorkarea";

  --! @brief Returns the physical size of the monitor.
  -- *
  -- *  This function returns the size, in millimetres, of the display area of the
  -- *  specified monitor.
  -- *
  -- *  Some systems do not provide accurate monitor size information, either
  -- *  because the monitor
  -- *  [EDID](https://en.wikipedia.org/wiki/Extended_display_identification_data)
  -- *  data is incorrect or because the driver does not report it accurately.
  -- *
  -- *  Any or all of the size arguments may be `NULL`.  If an error occurs, all
  -- *  non-`NULL` size arguments will be set to zero.
  -- *
  -- *  @param[in] monitor The monitor to query.
  -- *  @param[out] widthMM Where to store the width, in millimetres, of the
  -- *  monitor's display area, or `NULL`.
  -- *  @param[out] heightMM Where to store the height, in millimetres, of the
  -- *  monitor's display area, or `NULL`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @remark @win32 calculates the returned physical size from the
  -- *  current resolution and system DPI instead of querying the monitor EDID data.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_properties
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

   procedure glfwGetMonitorPhysicalSize
     (monitor : access GLFWmonitor;
      widthMM : access int;
      heightMM : access int)  -- ../include/GLFW/glfw3.h:2107
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetMonitorPhysicalSize";

  --! @brief Retrieves the content scale for the specified monitor.
  -- *
  -- *  This function retrieves the content scale for the specified monitor.  The
  -- *  content scale is the ratio between the current DPI and the platform's
  -- *  default DPI.  This is especially important for text and any UI elements.  If
  -- *  the pixel dimensions of your UI scaled by this look appropriate on your
  -- *  machine then it should appear at a reasonable size on other machines
  -- *  regardless of their DPI and scaling settings.  This relies on the system DPI
  -- *  and scaling settings being somewhat correct.
  -- *
  -- *  The content scale may depend on both the monitor resolution and pixel
  -- *  density and on user settings.  It may be very different from the raw DPI
  -- *  calculated from the physical size and current resolution.
  -- *
  -- *  @param[in] monitor The monitor to query.
  -- *  @param[out] xscale Where to store the x-axis content scale, or `NULL`.
  -- *  @param[out] yscale Where to store the y-axis content scale, or `NULL`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_scale
  -- *  @sa @ref glfwGetWindowContentScale
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup monitor
  --  

   procedure glfwGetMonitorContentScale
     (monitor : access GLFWmonitor;
      xscale : access float;
      yscale : access float)  -- ../include/GLFW/glfw3.h:2139
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetMonitorContentScale";

  --! @brief Returns the name of the specified monitor.
  -- *
  -- *  This function returns a human-readable name, encoded as UTF-8, of the
  -- *  specified monitor.  The name typically reflects the make and model of the
  -- *  monitor and is not guaranteed to be unique among the connected monitors.
  -- *
  -- *  @param[in] monitor The monitor to query.
  -- *  @return The UTF-8 encoded name of the monitor, or `NULL` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @pointer_lifetime The returned string is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is valid until the specified monitor is
  -- *  disconnected or the library is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_properties
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

   function glfwGetMonitorName (monitor : access GLFWmonitor) return Interfaces.C.Strings.chars_ptr  -- ../include/GLFW/glfw3.h:2165
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetMonitorName";

  --! @brief Sets the user pointer of the specified monitor.
  -- *
  -- *  This function sets the user-defined pointer of the specified monitor.  The
  -- *  current value is retained until the monitor is disconnected.  The initial
  -- *  value is `NULL`.
  -- *
  -- *  This function may be called from the monitor callback, even for a monitor
  -- *  that is being disconnected.
  -- *
  -- *  @param[in] monitor The monitor whose pointer to set.
  -- *  @param[in] pointer The new value.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.  Access is not
  -- *  synchronized.
  -- *
  -- *  @sa @ref monitor_userptr
  -- *  @sa @ref glfwGetMonitorUserPointer
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup monitor
  --  

   procedure glfwSetMonitorUserPointer (monitor : access GLFWmonitor; pointer : System.Address)  -- ../include/GLFW/glfw3.h:2191
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetMonitorUserPointer";

  --! @brief Returns the user pointer of the specified monitor.
  -- *
  -- *  This function returns the current value of the user-defined pointer of the
  -- *  specified monitor.  The initial value is `NULL`.
  -- *
  -- *  This function may be called from the monitor callback, even for a monitor
  -- *  that is being disconnected.
  -- *
  -- *  @param[in] monitor The monitor whose pointer to return.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.  Access is not
  -- *  synchronized.
  -- *
  -- *  @sa @ref monitor_userptr
  -- *  @sa @ref glfwSetMonitorUserPointer
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup monitor
  --  

   function glfwGetMonitorUserPointer (monitor : access GLFWmonitor) return System.Address  -- ../include/GLFW/glfw3.h:2215
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetMonitorUserPointer";

  --! @brief Sets the monitor configuration callback.
  -- *
  -- *  This function sets the monitor configuration callback, or removes the
  -- *  currently set callback.  This is called when a monitor is connected to or
  -- *  disconnected from the system.
  -- *
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWmonitor* monitor, int event)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWmonitorfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_event
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

   function glfwSetMonitorCallback (callback : GLFWmonitorfun) return GLFWmonitorfun  -- ../include/GLFW/glfw3.h:2245
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetMonitorCallback";

  --! @brief Returns the available video modes for the specified monitor.
  -- *
  -- *  This function returns an array of all video modes supported by the specified
  -- *  monitor.  The returned array is sorted in ascending order, first by color
  -- *  bit depth (the sum of all channel depths) and then by resolution area (the
  -- *  product of width and height).
  -- *
  -- *  @param[in] monitor The monitor to query.
  -- *  @param[out] count Where to store the number of video modes in the returned
  -- *  array.  This is set to zero if an error occurred.
  -- *  @return An array of video modes, or `NULL` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The returned array is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is valid until the specified monitor is
  -- *  disconnected, this function is called again for that monitor or the library
  -- *  is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_modes
  -- *  @sa @ref glfwGetVideoMode
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Changed to return an array of modes for a specific monitor.
  -- *
  -- *  @ingroup monitor
  --  

   function glfwGetVideoModes (monitor : access GLFWmonitor; count : access int) return access constant GLFWvidmode  -- ../include/GLFW/glfw3.h:2278
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetVideoModes";

  --! @brief Returns the current mode of the specified monitor.
  -- *
  -- *  This function returns the current video mode of the specified monitor.  If
  -- *  you have created a full screen window for that monitor, the return value
  -- *  will depend on whether that window is iconified.
  -- *
  -- *  @param[in] monitor The monitor to query.
  -- *  @return The current mode of the monitor, or `NULL` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The returned array is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is valid until the specified monitor is
  -- *  disconnected or the library is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_modes
  -- *  @sa @ref glfwGetVideoModes
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwGetDesktopMode`.
  -- *
  -- *  @ingroup monitor
  --  

   function glfwGetVideoMode (monitor : access GLFWmonitor) return access constant GLFWvidmode  -- ../include/GLFW/glfw3.h:2306
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetVideoMode";

  --! @brief Generates a gamma ramp and sets it for the specified monitor.
  -- *
  -- *  This function generates an appropriately sized gamma ramp from the specified
  -- *  exponent and then calls @ref glfwSetGammaRamp with it.  The value must be
  -- *  a finite number greater than zero.
  -- *
  -- *  The software controlled gamma ramp is applied _in addition_ to the hardware
  -- *  gamma correction, which today is usually an approximation of sRGB gamma.
  -- *  This means that setting a perfectly linear ramp, or gamma 1.0, will produce
  -- *  the default (usually sRGB-like) behavior.
  -- *
  -- *  For gamma correct rendering with OpenGL or OpenGL ES, see the @ref
  -- *  GLFW_SRGB_CAPABLE hint.
  -- *
  -- *  @param[in] monitor The monitor whose gamma ramp to set.
  -- *  @param[in] gamma The desired exponent.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_VALUE and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @wayland Gamma handling is a privileged protocol, this function
  -- *  will thus never be implemented and emits @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_gamma
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

   procedure glfwSetGamma (monitor : access GLFWmonitor; gamma : float)  -- ../include/GLFW/glfw3.h:2339
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetGamma";

  --! @brief Returns the current gamma ramp for the specified monitor.
  -- *
  -- *  This function returns the current gamma ramp of the specified monitor.
  -- *
  -- *  @param[in] monitor The monitor to query.
  -- *  @return The current gamma ramp, or `NULL` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @wayland Gamma handling is a privileged protocol, this function
  -- *  will thus never be implemented and emits @ref GLFW_PLATFORM_ERROR while
  -- *  returning `NULL`.
  -- *
  -- *  @pointer_lifetime The returned structure and its arrays are allocated and
  -- *  freed by GLFW.  You should not free them yourself.  They are valid until the
  -- *  specified monitor is disconnected, this function is called again for that
  -- *  monitor or the library is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_gamma
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

   function glfwGetGammaRamp (monitor : access GLFWmonitor) return access constant GLFWgammaramp  -- ../include/GLFW/glfw3.h:2369
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetGammaRamp";

  --! @brief Sets the current gamma ramp for the specified monitor.
  -- *
  -- *  This function sets the current gamma ramp for the specified monitor.  The
  -- *  original gamma ramp for that monitor is saved by GLFW the first time this
  -- *  function is called and is restored by @ref glfwTerminate.
  -- *
  -- *  The software controlled gamma ramp is applied _in addition_ to the hardware
  -- *  gamma correction, which today is usually an approximation of sRGB gamma.
  -- *  This means that setting a perfectly linear ramp, or gamma 1.0, will produce
  -- *  the default (usually sRGB-like) behavior.
  -- *
  -- *  For gamma correct rendering with OpenGL or OpenGL ES, see the @ref
  -- *  GLFW_SRGB_CAPABLE hint.
  -- *
  -- *  @param[in] monitor The monitor whose gamma ramp to set.
  -- *  @param[in] ramp The gamma ramp to use.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark The size of the specified gamma ramp should match the size of the
  -- *  current ramp for that monitor.
  -- *
  -- *  @remark @win32 The gamma ramp size must be 256.
  -- *
  -- *  @remark @wayland Gamma handling is a privileged protocol, this function
  -- *  will thus never be implemented and emits @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The specified gamma ramp is copied before this function
  -- *  returns.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref monitor_gamma
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup monitor
  --  

   procedure glfwSetGammaRamp (monitor : access GLFWmonitor; ramp : access constant GLFWgammaramp)  -- ../include/GLFW/glfw3.h:2410
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetGammaRamp";

  --! @brief Resets all window hints to their default values.
  -- *
  -- *  This function resets all window hints to their
  -- *  [default values](@ref window_hints_values).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_hints
  -- *  @sa @ref glfwWindowHint
  -- *  @sa @ref glfwWindowHintString
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwDefaultWindowHints  -- ../include/GLFW/glfw3.h:2429
   with Import => True, 
        Convention => C, 
        External_Name => "glfwDefaultWindowHints";

  --! @brief Sets the specified window hint to the desired value.
  -- *
  -- *  This function sets hints for the next call to @ref glfwCreateWindow.  The
  -- *  hints, once set, retain their values until changed by a call to this
  -- *  function or @ref glfwDefaultWindowHints, or until the library is terminated.
  -- *
  -- *  Only integer value hints can be set with this function.  String value hints
  -- *  are set with @ref glfwWindowHintString.
  -- *
  -- *  This function does not check whether the specified hint values are valid.
  -- *  If you set hints to invalid values this will instead be reported by the next
  -- *  call to @ref glfwCreateWindow.
  -- *
  -- *  Some hints are platform specific.  These may be set on any platform but they
  -- *  will only affect their specific platform.  Other platforms will ignore them.
  -- *  Setting these hints requires no platform specific headers or functions.
  -- *
  -- *  @param[in] hint The [window hint](@ref window_hints) to set.
  -- *  @param[in] value The new value of the window hint.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_INVALID_ENUM.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_hints
  -- *  @sa @ref glfwWindowHintString
  -- *  @sa @ref glfwDefaultWindowHints
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwOpenWindowHint`.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwWindowHint (hint : int; value : int)  -- ../include/GLFW/glfw3.h:2464
   with Import => True, 
        Convention => C, 
        External_Name => "glfwWindowHint";

  --! @brief Sets the specified window hint to the desired value.
  -- *
  -- *  This function sets hints for the next call to @ref glfwCreateWindow.  The
  -- *  hints, once set, retain their values until changed by a call to this
  -- *  function or @ref glfwDefaultWindowHints, or until the library is terminated.
  -- *
  -- *  Only string type hints can be set with this function.  Integer value hints
  -- *  are set with @ref glfwWindowHint.
  -- *
  -- *  This function does not check whether the specified hint values are valid.
  -- *  If you set hints to invalid values this will instead be reported by the next
  -- *  call to @ref glfwCreateWindow.
  -- *
  -- *  Some hints are platform specific.  These may be set on any platform but they
  -- *  will only affect their specific platform.  Other platforms will ignore them.
  -- *  Setting these hints requires no platform specific headers or functions.
  -- *
  -- *  @param[in] hint The [window hint](@ref window_hints) to set.
  -- *  @param[in] value The new value of the window hint.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_INVALID_ENUM.
  -- *
  -- *  @pointer_lifetime The specified string is copied before this function
  -- *  returns.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_hints
  -- *  @sa @ref glfwWindowHint
  -- *  @sa @ref glfwDefaultWindowHints
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwWindowHintString (hint : int; value : Interfaces.C.Strings.chars_ptr)  -- ../include/GLFW/glfw3.h:2502
   with Import => True, 
        Convention => C, 
        External_Name => "glfwWindowHintString";

  --! @brief Creates a window and its associated context.
  -- *
  -- *  This function creates a window and its associated OpenGL or OpenGL ES
  -- *  context.  Most of the options controlling how the window and its context
  -- *  should be created are specified with [window hints](@ref window_hints).
  -- *
  -- *  Successful creation does not change which context is current.  Before you
  -- *  can use the newly created context, you need to
  -- *  [make it current](@ref context_current).  For information about the `share`
  -- *  parameter, see @ref context_sharing.
  -- *
  -- *  The created window, framebuffer and context may differ from what you
  -- *  requested, as not all parameters and hints are
  -- *  [hard constraints](@ref window_hints_hard).  This includes the size of the
  -- *  window, especially for full screen windows.  To query the actual attributes
  -- *  of the created window, framebuffer and context, see @ref
  -- *  glfwGetWindowAttrib, @ref glfwGetWindowSize and @ref glfwGetFramebufferSize.
  -- *
  -- *  To create a full screen window, you need to specify the monitor the window
  -- *  will cover.  If no monitor is specified, the window will be windowed mode.
  -- *  Unless you have a way for the user to choose a specific monitor, it is
  -- *  recommended that you pick the primary monitor.  For more information on how
  -- *  to query connected monitors, see @ref monitor_monitors.
  -- *
  -- *  For full screen windows, the specified size becomes the resolution of the
  -- *  window's _desired video mode_.  As long as a full screen window is not
  -- *  iconified, the supported video mode most closely matching the desired video
  -- *  mode is set for the specified monitor.  For more information about full
  -- *  screen windows, including the creation of so called _windowed full screen_
  -- *  or _borderless full screen_ windows, see @ref window_windowed_full_screen.
  -- *
  -- *  Once you have created the window, you can switch it between windowed and
  -- *  full screen mode with @ref glfwSetWindowMonitor.  This will not affect its
  -- *  OpenGL or OpenGL ES context.
  -- *
  -- *  By default, newly created windows use the placement recommended by the
  -- *  window system.  To create the window at a specific position, make it
  -- *  initially invisible using the [GLFW_VISIBLE](@ref GLFW_VISIBLE_hint) window
  -- *  hint, set its [position](@ref window_pos) and then [show](@ref window_hide)
  -- *  it.
  -- *
  -- *  As long as at least one full screen window is not iconified, the screensaver
  -- *  is prohibited from starting.
  -- *
  -- *  Window systems put limits on window sizes.  Very large or very small window
  -- *  dimensions may be overridden by the window system on creation.  Check the
  -- *  actual [size](@ref window_size) after creation.
  -- *
  -- *  The [swap interval](@ref buffer_swap) is not set during window creation and
  -- *  the initial value may vary depending on driver settings and defaults.
  -- *
  -- *  @param[in] width The desired width, in screen coordinates, of the window.
  -- *  This must be greater than zero.
  -- *  @param[in] height The desired height, in screen coordinates, of the window.
  -- *  This must be greater than zero.
  -- *  @param[in] title The initial, UTF-8 encoded window title.
  -- *  @param[in] monitor The monitor to use for full screen mode, or `NULL` for
  -- *  windowed mode.
  -- *  @param[in] share The window whose context to share resources with, or `NULL`
  -- *  to not share resources.
  -- *  @return The handle of the created window, or `NULL` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM, @ref GLFW_INVALID_VALUE, @ref GLFW_API_UNAVAILABLE, @ref
  -- *  GLFW_VERSION_UNAVAILABLE, @ref GLFW_FORMAT_UNAVAILABLE and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @win32 Window creation will fail if the Microsoft GDI software
  -- *  OpenGL implementation is the only one available.
  -- *
  -- *  @remark @win32 If the executable has an icon resource named `GLFW_ICON,` it
  -- *  will be set as the initial icon for the window.  If no such icon is present,
  -- *  the `IDI_APPLICATION` icon will be used instead.  To set a different icon,
  -- *  see @ref glfwSetWindowIcon.
  -- *
  -- *  @remark @win32 The context to share resources with must not be current on
  -- *  any other thread.
  -- *
  -- *  @remark @macos The OS only supports forward-compatible core profile contexts
  -- *  for OpenGL versions 3.2 and later.  Before creating an OpenGL context of
  -- *  version 3.2 or later you must set the
  -- *  [GLFW_OPENGL_FORWARD_COMPAT](@ref GLFW_OPENGL_FORWARD_COMPAT_hint) and
  -- *  [GLFW_OPENGL_PROFILE](@ref GLFW_OPENGL_PROFILE_hint) hints accordingly.
  -- *  OpenGL 3.0 and 3.1 contexts are not supported at all on macOS.
  -- *
  -- *  @remark @macos The GLFW window has no icon, as it is not a document
  -- *  window, but the dock icon will be the same as the application bundle's icon.
  -- *  For more information on bundles, see the
  -- *  [Bundle Programming Guide](https://developer.apple.com/library/mac/documentation/CoreFoundation/Conceptual/CFBundles/)
  -- *  in the Mac Developer Library.
  -- *
  -- *  @remark @macos The first time a window is created the menu bar is created.
  -- *  If GLFW finds a `MainMenu.nib` it is loaded and assumed to contain a menu
  -- *  bar.  Otherwise a minimal menu bar is created manually with common commands
  -- *  like Hide, Quit and About.  The About entry opens a minimal about dialog
  -- *  with information from the application's bundle.  Menu bar creation can be
  -- *  disabled entirely with the @ref GLFW_COCOA_MENUBAR init hint.
  -- *
  -- *  @remark @macos On OS X 10.10 and later the window frame will not be rendered
  -- *  at full resolution on Retina displays unless the
  -- *  [GLFW_COCOA_RETINA_FRAMEBUFFER](@ref GLFW_COCOA_RETINA_FRAMEBUFFER_hint)
  -- *  hint is `GLFW_TRUE` and the `NSHighResolutionCapable` key is enabled in the
  -- *  application bundle's `Info.plist`.  For more information, see
  -- *  [High Resolution Guidelines for OS X](https://developer.apple.com/library/mac/documentation/GraphicsAnimation/Conceptual/HighResolutionOSX/Explained/Explained.html)
  -- *  in the Mac Developer Library.  The GLFW test and example programs use
  -- *  a custom `Info.plist` template for this, which can be found as
  -- *  `CMake/MacOSXBundleInfo.plist.in` in the source tree.
  -- *
  -- *  @remark @macos When activating frame autosaving with
  -- *  [GLFW_COCOA_FRAME_NAME](@ref GLFW_COCOA_FRAME_NAME_hint), the specified
  -- *  window size and position may be overridden by previously saved values.
  -- *
  -- *  @remark @x11 Some window managers will not respect the placement of
  -- *  initially hidden windows.
  -- *
  -- *  @remark @x11 Due to the asynchronous nature of X11, it may take a moment for
  -- *  a window to reach its requested state.  This means you may not be able to
  -- *  query the final size, position or other attributes directly after window
  -- *  creation.
  -- *
  -- *  @remark @x11 The class part of the `WM_CLASS` window property will by
  -- *  default be set to the window title passed to this function.  The instance
  -- *  part will use the contents of the `RESOURCE_NAME` environment variable, if
  -- *  present and not empty, or fall back to the window title.  Set the
  -- *  [GLFW_X11_CLASS_NAME](@ref GLFW_X11_CLASS_NAME_hint) and
  -- *  [GLFW_X11_INSTANCE_NAME](@ref GLFW_X11_INSTANCE_NAME_hint) window hints to
  -- *  override this.
  -- *
  -- *  @remark @wayland Compositors should implement the xdg-decoration protocol
  -- *  for GLFW to decorate the window properly.  If this protocol isn't
  -- *  supported, or if the compositor prefers client-side decorations, a very
  -- *  simple fallback frame will be drawn using the wp_viewporter protocol.  A
  -- *  compositor can still emit close, maximize or fullscreen events, using for
  -- *  instance a keybind mechanism.  If neither of these protocols is supported,
  -- *  the window won't be decorated.
  -- *
  -- *  @remark @wayland A full screen window will not attempt to change the mode,
  -- *  no matter what the requested size or refresh rate.
  -- *
  -- *  @remark @wayland Screensaver inhibition requires the idle-inhibit protocol
  -- *  to be implemented in the user's compositor.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_creation
  -- *  @sa @ref glfwDestroyWindow
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwOpenWindow`.
  -- *
  -- *  @ingroup window
  --  

   function glfwCreateWindow
     (width : int;
      height : int;
      title : Interfaces.C.Strings.chars_ptr;
      monitor : access GLFWmonitor;
      share : access GLFWwindow) return access GLFWwindow  -- ../include/GLFW/glfw3.h:2656
   with Import => True, 
        Convention => C, 
        External_Name => "glfwCreateWindow";

  --! @brief Destroys the specified window and its context.
  -- *
  -- *  This function destroys the specified window and its context.  On calling
  -- *  this function, no further callbacks will be called for that window.
  -- *
  -- *  If the context of the specified window is current on the main thread, it is
  -- *  detached before being destroyed.
  -- *
  -- *  @param[in] window The window to destroy.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @note The context of the specified window must not be current on any other
  -- *  thread when this function is called.
  -- *
  -- *  @reentrancy This function must not be called from a callback.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_creation
  -- *  @sa @ref glfwCreateWindow
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwCloseWindow`.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwDestroyWindow (window : access GLFWwindow)  -- ../include/GLFW/glfw3.h:2685
   with Import => True, 
        Convention => C, 
        External_Name => "glfwDestroyWindow";

  --! @brief Checks the close flag of the specified window.
  -- *
  -- *  This function returns the value of the close flag of the specified window.
  -- *
  -- *  @param[in] window The window to query.
  -- *  @return The value of the close flag.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.  Access is not
  -- *  synchronized.
  -- *
  -- *  @sa @ref window_close
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   function glfwWindowShouldClose (window : access GLFWwindow) return int  -- ../include/GLFW/glfw3.h:2705
   with Import => True, 
        Convention => C, 
        External_Name => "glfwWindowShouldClose";

  --! @brief Sets the close flag of the specified window.
  -- *
  -- *  This function sets the value of the close flag of the specified window.
  -- *  This can be used to override the user's attempt to close the window, or
  -- *  to signal that it should be closed.
  -- *
  -- *  @param[in] window The window whose flag to change.
  -- *  @param[in] value The new value.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.  Access is not
  -- *  synchronized.
  -- *
  -- *  @sa @ref window_close
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSetWindowShouldClose (window : access GLFWwindow; value : int)  -- ../include/GLFW/glfw3.h:2727
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowShouldClose";

  --! @brief Sets the title of the specified window.
  -- *
  -- *  This function sets the window title, encoded as UTF-8, of the specified
  -- *  window.
  -- *
  -- *  @param[in] window The window whose title to change.
  -- *  @param[in] title The UTF-8 encoded window title.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @macos The window title will not be updated until the next time you
  -- *  process events.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_title
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSetWindowTitle (window : access GLFWwindow; title : Interfaces.C.Strings.chars_ptr)  -- ../include/GLFW/glfw3.h:2752
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowTitle";

  --! @brief Sets the icon for the specified window.
  -- *
  -- *  This function sets the icon of the specified window.  If passed an array of
  -- *  candidate images, those of or closest to the sizes desired by the system are
  -- *  selected.  If no images are specified, the window reverts to its default
  -- *  icon.
  -- *
  -- *  The pixels are 32-bit, little-endian, non-premultiplied RGBA, i.e. eight
  -- *  bits per channel with the red channel first.  They are arranged canonically
  -- *  as packed sequential rows, starting from the top-left corner.
  -- *
  -- *  The desired image sizes varies depending on platform and system settings.
  -- *  The selected images will be rescaled as needed.  Good sizes include 16x16,
  -- *  32x32 and 48x48.
  -- *
  -- *  @param[in] window The window whose icon to set.
  -- *  @param[in] count The number of images in the specified array, or zero to
  -- *  revert to the default window icon.
  -- *  @param[in] images The images to create the icon from.  This is ignored if
  -- *  count is zero.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The specified image data is copied before this function
  -- *  returns.
  -- *
  -- *  @remark @macos The GLFW window has no icon, as it is not a document
  -- *  window, so this function does nothing.  The dock icon will be the same as
  -- *  the application bundle's icon.  For more information on bundles, see the
  -- *  [Bundle Programming Guide](https://developer.apple.com/library/mac/documentation/CoreFoundation/Conceptual/CFBundles/)
  -- *  in the Mac Developer Library.
  -- *
  -- *  @remark @wayland There is no existing protocol to change an icon, the
  -- *  window will thus inherit the one defined in the application's desktop file.
  -- *  This function always emits @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_icon
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSetWindowIcon
     (window : access GLFWwindow;
      count : int;
      images : access constant GLFWimage)  -- ../include/GLFW/glfw3.h:2799
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowIcon";

  --! @brief Retrieves the position of the content area of the specified window.
  -- *
  -- *  This function retrieves the position, in screen coordinates, of the
  -- *  upper-left corner of the content area of the specified window.
  -- *
  -- *  Any or all of the position arguments may be `NULL`.  If an error occurs, all
  -- *  non-`NULL` position arguments will be set to zero.
  -- *
  -- *  @param[in] window The window to query.
  -- *  @param[out] xpos Where to store the x-coordinate of the upper-left corner of
  -- *  the content area, or `NULL`.
  -- *  @param[out] ypos Where to store the y-coordinate of the upper-left corner of
  -- *  the content area, or `NULL`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @wayland There is no way for an application to retrieve the global
  -- *  position of its windows, this function will always emit @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_pos
  -- *  @sa @ref glfwSetWindowPos
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwGetWindowPos
     (window : access GLFWwindow;
      xpos : access int;
      ypos : access int)  -- ../include/GLFW/glfw3.h:2831
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetWindowPos";

  --! @brief Sets the position of the content area of the specified window.
  -- *
  -- *  This function sets the position, in screen coordinates, of the upper-left
  -- *  corner of the content area of the specified windowed mode window.  If the
  -- *  window is a full screen window, this function does nothing.
  -- *
  -- *  __Do not use this function__ to move an already visible window unless you
  -- *  have very good reasons for doing so, as it will confuse and annoy the user.
  -- *
  -- *  The window manager may put limits on what positions are allowed.  GLFW
  -- *  cannot and should not override these limits.
  -- *
  -- *  @param[in] window The window to query.
  -- *  @param[in] xpos The x-coordinate of the upper-left corner of the content area.
  -- *  @param[in] ypos The y-coordinate of the upper-left corner of the content area.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @wayland There is no way for an application to set the global
  -- *  position of its windows, this function will always emit @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_pos
  -- *  @sa @ref glfwGetWindowPos
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSetWindowPos
     (window : access GLFWwindow;
      xpos : int;
      ypos : int)  -- ../include/GLFW/glfw3.h:2866
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowPos";

  --! @brief Retrieves the size of the content area of the specified window.
  -- *
  -- *  This function retrieves the size, in screen coordinates, of the content area
  -- *  of the specified window.  If you wish to retrieve the size of the
  -- *  framebuffer of the window in pixels, see @ref glfwGetFramebufferSize.
  -- *
  -- *  Any or all of the size arguments may be `NULL`.  If an error occurs, all
  -- *  non-`NULL` size arguments will be set to zero.
  -- *
  -- *  @param[in] window The window whose size to retrieve.
  -- *  @param[out] width Where to store the width, in screen coordinates, of the
  -- *  content area, or `NULL`.
  -- *  @param[out] height Where to store the height, in screen coordinates, of the
  -- *  content area, or `NULL`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_size
  -- *  @sa @ref glfwSetWindowSize
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwGetWindowSize
     (window : access GLFWwindow;
      width : access int;
      height : access int)  -- ../include/GLFW/glfw3.h:2896
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetWindowSize";

  --! @brief Sets the size limits of the specified window.
  -- *
  -- *  This function sets the size limits of the content area of the specified
  -- *  window.  If the window is full screen, the size limits only take effect
  -- *  once it is made windowed.  If the window is not resizable, this function
  -- *  does nothing.
  -- *
  -- *  The size limits are applied immediately to a windowed mode window and may
  -- *  cause it to be resized.
  -- *
  -- *  The maximum dimensions must be greater than or equal to the minimum
  -- *  dimensions and all must be greater than or equal to zero.
  -- *
  -- *  @param[in] window The window to set limits for.
  -- *  @param[in] minwidth The minimum width, in screen coordinates, of the content
  -- *  area, or `GLFW_DONT_CARE`.
  -- *  @param[in] minheight The minimum height, in screen coordinates, of the
  -- *  content area, or `GLFW_DONT_CARE`.
  -- *  @param[in] maxwidth The maximum width, in screen coordinates, of the content
  -- *  area, or `GLFW_DONT_CARE`.
  -- *  @param[in] maxheight The maximum height, in screen coordinates, of the
  -- *  content area, or `GLFW_DONT_CARE`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_VALUE and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark If you set size limits and an aspect ratio that conflict, the
  -- *  results are undefined.
  -- *
  -- *  @remark @wayland The size limits will not be applied until the window is
  -- *  actually resized, either by the user or by the compositor.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_sizelimits
  -- *  @sa @ref glfwSetWindowAspectRatio
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSetWindowSizeLimits
     (window : access GLFWwindow;
      minwidth : int;
      minheight : int;
      maxwidth : int;
      maxheight : int)  -- ../include/GLFW/glfw3.h:2939
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowSizeLimits";

  --! @brief Sets the aspect ratio of the specified window.
  -- *
  -- *  This function sets the required aspect ratio of the content area of the
  -- *  specified window.  If the window is full screen, the aspect ratio only takes
  -- *  effect once it is made windowed.  If the window is not resizable, this
  -- *  function does nothing.
  -- *
  -- *  The aspect ratio is specified as a numerator and a denominator and both
  -- *  values must be greater than zero.  For example, the common 16:9 aspect ratio
  -- *  is specified as 16 and 9, respectively.
  -- *
  -- *  If the numerator and denominator is set to `GLFW_DONT_CARE` then the aspect
  -- *  ratio limit is disabled.
  -- *
  -- *  The aspect ratio is applied immediately to a windowed mode window and may
  -- *  cause it to be resized.
  -- *
  -- *  @param[in] window The window to set limits for.
  -- *  @param[in] numer The numerator of the desired aspect ratio, or
  -- *  `GLFW_DONT_CARE`.
  -- *  @param[in] denom The denominator of the desired aspect ratio, or
  -- *  `GLFW_DONT_CARE`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_VALUE and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark If you set size limits and an aspect ratio that conflict, the
  -- *  results are undefined.
  -- *
  -- *  @remark @wayland The aspect ratio will not be applied until the window is
  -- *  actually resized, either by the user or by the compositor.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_sizelimits
  -- *  @sa @ref glfwSetWindowSizeLimits
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSetWindowAspectRatio
     (window : access GLFWwindow;
      numer : int;
      denom : int)  -- ../include/GLFW/glfw3.h:2982
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowAspectRatio";

  --! @brief Sets the size of the content area of the specified window.
  -- *
  -- *  This function sets the size, in screen coordinates, of the content area of
  -- *  the specified window.
  -- *
  -- *  For full screen windows, this function updates the resolution of its desired
  -- *  video mode and switches to the video mode closest to it, without affecting
  -- *  the window's context.  As the context is unaffected, the bit depths of the
  -- *  framebuffer remain unchanged.
  -- *
  -- *  If you wish to update the refresh rate of the desired video mode in addition
  -- *  to its resolution, see @ref glfwSetWindowMonitor.
  -- *
  -- *  The window manager may put limits on what sizes are allowed.  GLFW cannot
  -- *  and should not override these limits.
  -- *
  -- *  @param[in] window The window to resize.
  -- *  @param[in] width The desired width, in screen coordinates, of the window
  -- *  content area.
  -- *  @param[in] height The desired height, in screen coordinates, of the window
  -- *  content area.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @wayland A full screen window will not attempt to change the mode,
  -- *  no matter what the requested size.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_size
  -- *  @sa @ref glfwGetWindowSize
  -- *  @sa @ref glfwSetWindowMonitor
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSetWindowSize
     (window : access GLFWwindow;
      width : int;
      height : int)  -- ../include/GLFW/glfw3.h:3023
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowSize";

  --! @brief Retrieves the size of the framebuffer of the specified window.
  -- *
  -- *  This function retrieves the size, in pixels, of the framebuffer of the
  -- *  specified window.  If you wish to retrieve the size of the window in screen
  -- *  coordinates, see @ref glfwGetWindowSize.
  -- *
  -- *  Any or all of the size arguments may be `NULL`.  If an error occurs, all
  -- *  non-`NULL` size arguments will be set to zero.
  -- *
  -- *  @param[in] window The window whose framebuffer to query.
  -- *  @param[out] width Where to store the width, in pixels, of the framebuffer,
  -- *  or `NULL`.
  -- *  @param[out] height Where to store the height, in pixels, of the framebuffer,
  -- *  or `NULL`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_fbsize
  -- *  @sa @ref glfwSetFramebufferSizeCallback
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwGetFramebufferSize
     (window : access GLFWwindow;
      width : access int;
      height : access int)  -- ../include/GLFW/glfw3.h:3052
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetFramebufferSize";

  --! @brief Retrieves the size of the frame of the window.
  -- *
  -- *  This function retrieves the size, in screen coordinates, of each edge of the
  -- *  frame of the specified window.  This size includes the title bar, if the
  -- *  window has one.  The size of the frame may vary depending on the
  -- *  [window-related hints](@ref window_hints_wnd) used to create it.
  -- *
  -- *  Because this function retrieves the size of each window frame edge and not
  -- *  the offset along a particular coordinate axis, the retrieved values will
  -- *  always be zero or positive.
  -- *
  -- *  Any or all of the size arguments may be `NULL`.  If an error occurs, all
  -- *  non-`NULL` size arguments will be set to zero.
  -- *
  -- *  @param[in] window The window whose frame size to query.
  -- *  @param[out] left Where to store the size, in screen coordinates, of the left
  -- *  edge of the window frame, or `NULL`.
  -- *  @param[out] top Where to store the size, in screen coordinates, of the top
  -- *  edge of the window frame, or `NULL`.
  -- *  @param[out] right Where to store the size, in screen coordinates, of the
  -- *  right edge of the window frame, or `NULL`.
  -- *  @param[out] bottom Where to store the size, in screen coordinates, of the
  -- *  bottom edge of the window frame, or `NULL`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_size
  -- *
  -- *  @since Added in version 3.1.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwGetWindowFrameSize
     (window : access GLFWwindow;
      left : access int;
      top : access int;
      right : access int;
      bottom : access int)  -- ../include/GLFW/glfw3.h:3089
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetWindowFrameSize";

  --! @brief Retrieves the content scale for the specified window.
  -- *
  -- *  This function retrieves the content scale for the specified window.  The
  -- *  content scale is the ratio between the current DPI and the platform's
  -- *  default DPI.  This is especially important for text and any UI elements.  If
  -- *  the pixel dimensions of your UI scaled by this look appropriate on your
  -- *  machine then it should appear at a reasonable size on other machines
  -- *  regardless of their DPI and scaling settings.  This relies on the system DPI
  -- *  and scaling settings being somewhat correct.
  -- *
  -- *  On systems where each monitors can have its own content scale, the window
  -- *  content scale will depend on which monitor the system considers the window
  -- *  to be on.
  -- *
  -- *  @param[in] window The window to query.
  -- *  @param[out] xscale Where to store the x-axis content scale, or `NULL`.
  -- *  @param[out] yscale Where to store the y-axis content scale, or `NULL`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_scale
  -- *  @sa @ref glfwSetWindowContentScaleCallback
  -- *  @sa @ref glfwGetMonitorContentScale
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwGetWindowContentScale
     (window : access GLFWwindow;
      xscale : access float;
      yscale : access float)  -- ../include/GLFW/glfw3.h:3122
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetWindowContentScale";

  --! @brief Returns the opacity of the whole window.
  -- *
  -- *  This function returns the opacity of the window, including any decorations.
  -- *
  -- *  The opacity (or alpha) value is a positive finite number between zero and
  -- *  one, where zero is fully transparent and one is fully opaque.  If the system
  -- *  does not support whole window transparency, this function always returns one.
  -- *
  -- *  The initial opacity value for newly created windows is one.
  -- *
  -- *  @param[in] window The window to query.
  -- *  @return The opacity value of the specified window.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_transparency
  -- *  @sa @ref glfwSetWindowOpacity
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup window
  --  

   function glfwGetWindowOpacity (window : access GLFWwindow) return float  -- ../include/GLFW/glfw3.h:3149
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetWindowOpacity";

  --! @brief Sets the opacity of the whole window.
  -- *
  -- *  This function sets the opacity of the window, including any decorations.
  -- *
  -- *  The opacity (or alpha) value is a positive finite number between zero and
  -- *  one, where zero is fully transparent and one is fully opaque.
  -- *
  -- *  The initial opacity value for newly created windows is one.
  -- *
  -- *  A window created with framebuffer transparency may not use whole window
  -- *  transparency.  The results of doing this are undefined.
  -- *
  -- *  @param[in] window The window to set the opacity for.
  -- *  @param[in] opacity The desired opacity of the specified window.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_transparency
  -- *  @sa @ref glfwGetWindowOpacity
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSetWindowOpacity (window : access GLFWwindow; opacity : float)  -- ../include/GLFW/glfw3.h:3178
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowOpacity";

  --! @brief Iconifies the specified window.
  -- *
  -- *  This function iconifies (minimizes) the specified window if it was
  -- *  previously restored.  If the window is already iconified, this function does
  -- *  nothing.
  -- *
  -- *  If the specified window is a full screen window, the original monitor
  -- *  resolution is restored until the window is restored.
  -- *
  -- *  @param[in] window The window to iconify.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @wayland There is no concept of iconification in wl_shell, this
  -- *  function will emit @ref GLFW_PLATFORM_ERROR when using this deprecated
  -- *  protocol.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_iconify
  -- *  @sa @ref glfwRestoreWindow
  -- *  @sa @ref glfwMaximizeWindow
  -- *
  -- *  @since Added in version 2.1.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwIconifyWindow (window : access GLFWwindow)  -- ../include/GLFW/glfw3.h:3209
   with Import => True, 
        Convention => C, 
        External_Name => "glfwIconifyWindow";

  --! @brief Restores the specified window.
  -- *
  -- *  This function restores the specified window if it was previously iconified
  -- *  (minimized) or maximized.  If the window is already restored, this function
  -- *  does nothing.
  -- *
  -- *  If the specified window is a full screen window, the resolution chosen for
  -- *  the window is restored on the selected monitor.
  -- *
  -- *  @param[in] window The window to restore.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_iconify
  -- *  @sa @ref glfwIconifyWindow
  -- *  @sa @ref glfwMaximizeWindow
  -- *
  -- *  @since Added in version 2.1.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwRestoreWindow (window : access GLFWwindow)  -- ../include/GLFW/glfw3.h:3236
   with Import => True, 
        Convention => C, 
        External_Name => "glfwRestoreWindow";

  --! @brief Maximizes the specified window.
  -- *
  -- *  This function maximizes the specified window if it was previously not
  -- *  maximized.  If the window is already maximized, this function does nothing.
  -- *
  -- *  If the specified window is a full screen window, this function does nothing.
  -- *
  -- *  @param[in] window The window to maximize.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @par Thread Safety
  -- *  This function may only be called from the main thread.
  -- *
  -- *  @sa @ref window_iconify
  -- *  @sa @ref glfwIconifyWindow
  -- *  @sa @ref glfwRestoreWindow
  -- *
  -- *  @since Added in GLFW 3.2.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwMaximizeWindow (window : access GLFWwindow)  -- ../include/GLFW/glfw3.h:3261
   with Import => True, 
        Convention => C, 
        External_Name => "glfwMaximizeWindow";

  --! @brief Makes the specified window visible.
  -- *
  -- *  This function makes the specified window visible if it was previously
  -- *  hidden.  If the window is already visible or is in full screen mode, this
  -- *  function does nothing.
  -- *
  -- *  By default, windowed mode windows are focused when shown
  -- *  Set the [GLFW_FOCUS_ON_SHOW](@ref GLFW_FOCUS_ON_SHOW_hint) window hint
  -- *  to change this behavior for all newly created windows, or change the
  -- *  behavior for an existing window with @ref glfwSetWindowAttrib.
  -- *
  -- *  @param[in] window The window to make visible.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_hide
  -- *  @sa @ref glfwHideWindow
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwShowWindow (window : access GLFWwindow)  -- ../include/GLFW/glfw3.h:3288
   with Import => True, 
        Convention => C, 
        External_Name => "glfwShowWindow";

  --! @brief Hides the specified window.
  -- *
  -- *  This function hides the specified window if it was previously visible.  If
  -- *  the window is already hidden or is in full screen mode, this function does
  -- *  nothing.
  -- *
  -- *  @param[in] window The window to hide.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_hide
  -- *  @sa @ref glfwShowWindow
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwHideWindow (window : access GLFWwindow)  -- ../include/GLFW/glfw3.h:3310
   with Import => True, 
        Convention => C, 
        External_Name => "glfwHideWindow";

  --! @brief Brings the specified window to front and sets input focus.
  -- *
  -- *  This function brings the specified window to front and sets input focus.
  -- *  The window should already be visible and not iconified.
  -- *
  -- *  By default, both windowed and full screen mode windows are focused when
  -- *  initially created.  Set the [GLFW_FOCUSED](@ref GLFW_FOCUSED_hint) to
  -- *  disable this behavior.
  -- *
  -- *  Also by default, windowed mode windows are focused when shown
  -- *  with @ref glfwShowWindow. Set the
  -- *  [GLFW_FOCUS_ON_SHOW](@ref GLFW_FOCUS_ON_SHOW_hint) to disable this behavior.
  -- *
  -- *  __Do not use this function__ to steal focus from other applications unless
  -- *  you are certain that is what the user wants.  Focus stealing can be
  -- *  extremely disruptive.
  -- *
  -- *  For a less disruptive way of getting the user's attention, see
  -- *  [attention requests](@ref window_attention).
  -- *
  -- *  @param[in] window The window to give input focus.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @wayland It is not possible for an application to bring its windows
  -- *  to front, this function will always emit @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_focus
  -- *  @sa @ref window_attention
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwFocusWindow (window : access GLFWwindow)  -- ../include/GLFW/glfw3.h:3349
   with Import => True, 
        Convention => C, 
        External_Name => "glfwFocusWindow";

  --! @brief Requests user attention to the specified window.
  -- *
  -- *  This function requests user attention to the specified window.  On
  -- *  platforms where this is not supported, attention is requested to the
  -- *  application as a whole.
  -- *
  -- *  Once the user has given attention, usually by focusing the window or
  -- *  application, the system will end the request automatically.
  -- *
  -- *  @param[in] window The window to request attention to.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @macos Attention is requested to the application as a whole, not the
  -- *  specific window.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_attention
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwRequestWindowAttention (window : access GLFWwindow)  -- ../include/GLFW/glfw3.h:3376
   with Import => True, 
        Convention => C, 
        External_Name => "glfwRequestWindowAttention";

  --! @brief Returns the monitor that the window uses for full screen mode.
  -- *
  -- *  This function returns the handle of the monitor that the specified window is
  -- *  in full screen on.
  -- *
  -- *  @param[in] window The window to query.
  -- *  @return The monitor, or `NULL` if the window is in windowed mode or an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_monitor
  -- *  @sa @ref glfwSetWindowMonitor
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   function glfwGetWindowMonitor (window : access GLFWwindow) return access GLFWmonitor  -- ../include/GLFW/glfw3.h:3398
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetWindowMonitor";

  --! @brief Sets the mode, monitor, video mode and placement of a window.
  -- *
  -- *  This function sets the monitor that the window uses for full screen mode or,
  -- *  if the monitor is `NULL`, makes it windowed mode.
  -- *
  -- *  When setting a monitor, this function updates the width, height and refresh
  -- *  rate of the desired video mode and switches to the video mode closest to it.
  -- *  The window position is ignored when setting a monitor.
  -- *
  -- *  When the monitor is `NULL`, the position, width and height are used to
  -- *  place the window content area.  The refresh rate is ignored when no monitor
  -- *  is specified.
  -- *
  -- *  If you only wish to update the resolution of a full screen window or the
  -- *  size of a windowed mode window, see @ref glfwSetWindowSize.
  -- *
  -- *  When a window transitions from full screen to windowed mode, this function
  -- *  restores any previous window settings such as whether it is decorated,
  -- *  floating, resizable, has size or aspect ratio limits, etc.
  -- *
  -- *  @param[in] window The window whose monitor, size or video mode to set.
  -- *  @param[in] monitor The desired monitor, or `NULL` to set windowed mode.
  -- *  @param[in] xpos The desired x-coordinate of the upper-left corner of the
  -- *  content area.
  -- *  @param[in] ypos The desired y-coordinate of the upper-left corner of the
  -- *  content area.
  -- *  @param[in] width The desired with, in screen coordinates, of the content
  -- *  area or video mode.
  -- *  @param[in] height The desired height, in screen coordinates, of the content
  -- *  area or video mode.
  -- *  @param[in] refreshRate The desired refresh rate, in Hz, of the video mode,
  -- *  or `GLFW_DONT_CARE`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark The OpenGL or OpenGL ES context will not be destroyed or otherwise
  -- *  affected by any resizing or mode switching, although you may need to update
  -- *  your viewport if the framebuffer size has changed.
  -- *
  -- *  @remark @wayland The desired window position is ignored, as there is no way
  -- *  for an application to set this property.
  -- *
  -- *  @remark @wayland Setting the window to full screen will not attempt to
  -- *  change the mode, no matter what the requested size or refresh rate.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_monitor
  -- *  @sa @ref window_full_screen
  -- *  @sa @ref glfwGetWindowMonitor
  -- *  @sa @ref glfwSetWindowSize
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSetWindowMonitor
     (window : access GLFWwindow;
      monitor : access GLFWmonitor;
      xpos : int;
      ypos : int;
      width : int;
      height : int;
      refreshRate : int)  -- ../include/GLFW/glfw3.h:3457
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowMonitor";

  --! @brief Returns an attribute of the specified window.
  -- *
  -- *  This function returns the value of an attribute of the specified window or
  -- *  its OpenGL or OpenGL ES context.
  -- *
  -- *  @param[in] window The window to query.
  -- *  @param[in] attrib The [window attribute](@ref window_attribs) whose value to
  -- *  return.
  -- *  @return The value of the attribute, or zero if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark Framebuffer related hints are not window attributes.  See @ref
  -- *  window_attribs_fb for more information.
  -- *
  -- *  @remark Zero is a valid value for many window and context related
  -- *  attributes so you cannot use a return value of zero as an indication of
  -- *  errors.  However, this function should not fail as long as it is passed
  -- *  valid arguments and the library has been [initialized](@ref intro_init).
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_attribs
  -- *  @sa @ref glfwSetWindowAttrib
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwGetWindowParam` and
  -- *  `glfwGetGLVersion`.
  -- *
  -- *  @ingroup window
  --  

   function glfwGetWindowAttrib (window : access GLFWwindow; attrib : int) return int  -- ../include/GLFW/glfw3.h:3491
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetWindowAttrib";

  --! @brief Sets an attribute of the specified window.
  -- *
  -- *  This function sets the value of an attribute of the specified window.
  -- *
  -- *  The supported attributes are [GLFW_DECORATED](@ref GLFW_DECORATED_attrib),
  -- *  [GLFW_RESIZABLE](@ref GLFW_RESIZABLE_attrib),
  -- *  [GLFW_FLOATING](@ref GLFW_FLOATING_attrib),
  -- *  [GLFW_AUTO_ICONIFY](@ref GLFW_AUTO_ICONIFY_attrib) and
  -- *  [GLFW_FOCUS_ON_SHOW](@ref GLFW_FOCUS_ON_SHOW_attrib).
  -- *
  -- *  Some of these attributes are ignored for full screen windows.  The new
  -- *  value will take effect if the window is later made windowed.
  -- *
  -- *  Some of these attributes are ignored for windowed mode windows.  The new
  -- *  value will take effect if the window is later made full screen.
  -- *
  -- *  @param[in] window The window to set the attribute for.
  -- *  @param[in] attrib A supported window attribute.
  -- *  @param[in] value `GLFW_TRUE` or `GLFW_FALSE`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM, @ref GLFW_INVALID_VALUE and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark Calling @ref glfwGetWindowAttrib will always return the latest
  -- *  value, even if that value is ignored by the current mode of the window.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_attribs
  -- *  @sa @ref glfwGetWindowAttrib
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSetWindowAttrib
     (window : access GLFWwindow;
      attrib : int;
      value : int)  -- ../include/GLFW/glfw3.h:3528
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowAttrib";

  --! @brief Sets the user pointer of the specified window.
  -- *
  -- *  This function sets the user-defined pointer of the specified window.  The
  -- *  current value is retained until the window is destroyed.  The initial value
  -- *  is `NULL`.
  -- *
  -- *  @param[in] window The window whose pointer to set.
  -- *  @param[in] pointer The new value.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.  Access is not
  -- *  synchronized.
  -- *
  -- *  @sa @ref window_userptr
  -- *  @sa @ref glfwGetWindowUserPointer
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSetWindowUserPointer (window : access GLFWwindow; pointer : System.Address)  -- ../include/GLFW/glfw3.h:3551
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowUserPointer";

  --! @brief Returns the user pointer of the specified window.
  -- *
  -- *  This function returns the current value of the user-defined pointer of the
  -- *  specified window.  The initial value is `NULL`.
  -- *
  -- *  @param[in] window The window whose pointer to return.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.  Access is not
  -- *  synchronized.
  -- *
  -- *  @sa @ref window_userptr
  -- *  @sa @ref glfwSetWindowUserPointer
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   function glfwGetWindowUserPointer (window : access GLFWwindow) return System.Address  -- ../include/GLFW/glfw3.h:3572
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetWindowUserPointer";

  --! @brief Sets the position callback for the specified window.
  -- *
  -- *  This function sets the position callback of the specified window, which is
  -- *  called when the window is moved.  The callback is provided with the
  -- *  position, in screen coordinates, of the upper-left corner of the content
  -- *  area of the window.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int xpos, int ypos)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWwindowposfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @remark @wayland This callback will never be called, as there is no way for
  -- *  an application to know its global position.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_pos
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   function glfwSetWindowPosCallback (window : access GLFWwindow; callback : GLFWwindowposfun) return GLFWwindowposfun  -- ../include/GLFW/glfw3.h:3607
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowPosCallback";

  --! @brief Sets the size callback for the specified window.
  -- *
  -- *  This function sets the size callback of the specified window, which is
  -- *  called when the window is resized.  The callback is provided with the size,
  -- *  in screen coordinates, of the content area of the window.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int width, int height)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWwindowsizefun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_size
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle parameter and return value.
  -- *
  -- *  @ingroup window
  --  

   function glfwSetWindowSizeCallback (window : access GLFWwindow; callback : GLFWwindowsizefun) return GLFWwindowsizefun  -- ../include/GLFW/glfw3.h:3639
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowSizeCallback";

  --! @brief Sets the close callback for the specified window.
  -- *
  -- *  This function sets the close callback of the specified window, which is
  -- *  called when the user attempts to close the window, for example by clicking
  -- *  the close widget in the title bar.
  -- *
  -- *  The close flag is set before this callback is called, but you can modify it
  -- *  at any time with @ref glfwSetWindowShouldClose.
  -- *
  -- *  The close callback is not triggered by @ref glfwDestroyWindow.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWwindowclosefun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @remark @macos Selecting Quit from the application menu will trigger the
  -- *  close callback for all windows.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_close
  -- *
  -- *  @since Added in version 2.5.
  -- *  @glfw3 Added window handle parameter and return value.
  -- *
  -- *  @ingroup window
  --  

   function glfwSetWindowCloseCallback (window : access GLFWwindow; callback : GLFWwindowclosefun) return GLFWwindowclosefun  -- ../include/GLFW/glfw3.h:3679
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowCloseCallback";

  --! @brief Sets the refresh callback for the specified window.
  -- *
  -- *  This function sets the refresh callback of the specified window, which is
  -- *  called when the content area of the window needs to be redrawn, for example
  -- *  if the window has been exposed after having been covered by another window.
  -- *
  -- *  On compositing window systems such as Aero, Compiz, Aqua or Wayland, where
  -- *  the window contents are saved off-screen, this callback may be called only
  -- *  very infrequently or never at all.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window);
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWwindowrefreshfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_refresh
  -- *
  -- *  @since Added in version 2.5.
  -- *  @glfw3 Added window handle parameter and return value.
  -- *
  -- *  @ingroup window
  --  

   function glfwSetWindowRefreshCallback (window : access GLFWwindow; callback : GLFWwindowrefreshfun) return GLFWwindowrefreshfun  -- ../include/GLFW/glfw3.h:3715
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowRefreshCallback";

  --! @brief Sets the focus callback for the specified window.
  -- *
  -- *  This function sets the focus callback of the specified window, which is
  -- *  called when the window gains or loses input focus.
  -- *
  -- *  After the focus callback is called for a window that lost input focus,
  -- *  synthetic key and mouse button release events will be generated for all such
  -- *  that had been pressed.  For more information, see @ref glfwSetKeyCallback
  -- *  and @ref glfwSetMouseButtonCallback.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int focused)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWwindowfocusfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_focus
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   function glfwSetWindowFocusCallback (window : access GLFWwindow; callback : GLFWwindowfocusfun) return GLFWwindowfocusfun  -- ../include/GLFW/glfw3.h:3750
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowFocusCallback";

  --! @brief Sets the iconify callback for the specified window.
  -- *
  -- *  This function sets the iconification callback of the specified window, which
  -- *  is called when the window is iconified or restored.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int iconified)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWwindowiconifyfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @remark @wayland The wl_shell protocol has no concept of iconification,
  -- *  this callback will never be called when using this deprecated protocol.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_iconify
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   function glfwSetWindowIconifyCallback (window : access GLFWwindow; callback : GLFWwindowiconifyfun) return GLFWwindowiconifyfun  -- ../include/GLFW/glfw3.h:3783
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowIconifyCallback";

  --! @brief Sets the maximize callback for the specified window.
  -- *
  -- *  This function sets the maximization callback of the specified window, which
  -- *  is called when the window is maximized or restored.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int maximized)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWwindowmaximizefun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_maximize
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup window
  --  

   function glfwSetWindowMaximizeCallback (window : access GLFWwindow; callback : GLFWwindowmaximizefun) return GLFWwindowmaximizefun  -- ../include/GLFW/glfw3.h:3813
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowMaximizeCallback";

  --! @brief Sets the framebuffer resize callback for the specified window.
  -- *
  -- *  This function sets the framebuffer resize callback of the specified window,
  -- *  which is called when the framebuffer of the specified window is resized.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int width, int height)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWframebuffersizefun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_fbsize
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup window
  --  

   function glfwSetFramebufferSizeCallback (window : access GLFWwindow; callback : GLFWframebuffersizefun) return GLFWframebuffersizefun  -- ../include/GLFW/glfw3.h:3843
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetFramebufferSizeCallback";

  --! @brief Sets the window content scale callback for the specified window.
  -- *
  -- *  This function sets the window content scale callback of the specified window,
  -- *  which is called when the content scale of the specified window changes.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, float xscale, float yscale)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWwindowcontentscalefun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref window_scale
  -- *  @sa @ref glfwGetWindowContentScale
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup window
  --  

   function glfwSetWindowContentScaleCallback (window : access GLFWwindow; callback : GLFWwindowcontentscalefun) return GLFWwindowcontentscalefun  -- ../include/GLFW/glfw3.h:3874
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetWindowContentScaleCallback";

  --! @brief Processes all pending events.
  -- *
  -- *  This function processes only those events that are already in the event
  -- *  queue and then returns immediately.  Processing events will cause the window
  -- *  and input callbacks associated with those events to be called.
  -- *
  -- *  On some platforms, a window move, resize or menu operation will cause event
  -- *  processing to block.  This is due to how event processing is designed on
  -- *  those platforms.  You can use the
  -- *  [window refresh callback](@ref window_refresh) to redraw the contents of
  -- *  your window when necessary during such operations.
  -- *
  -- *  Do not assume that callbacks you set will _only_ be called in response to
  -- *  event processing functions like this one.  While it is necessary to poll for
  -- *  events, window systems that require GLFW to register callbacks of its own
  -- *  can pass events to GLFW in response to many window system function calls.
  -- *  GLFW will pass those events on to the application callbacks before
  -- *  returning.
  -- *
  -- *  Event processing is not required for joystick input to work.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @reentrancy This function must not be called from a callback.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref events
  -- *  @sa @ref glfwWaitEvents
  -- *  @sa @ref glfwWaitEventsTimeout
  -- *
  -- *  @since Added in version 1.0.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwPollEvents  -- ../include/GLFW/glfw3.h:3912
   with Import => True, 
        Convention => C, 
        External_Name => "glfwPollEvents";

  --! @brief Waits until events are queued and processes them.
  -- *
  -- *  This function puts the calling thread to sleep until at least one event is
  -- *  available in the event queue.  Once one or more events are available,
  -- *  it behaves exactly like @ref glfwPollEvents, i.e. the events in the queue
  -- *  are processed and the function then returns immediately.  Processing events
  -- *  will cause the window and input callbacks associated with those events to be
  -- *  called.
  -- *
  -- *  Since not all events are associated with callbacks, this function may return
  -- *  without a callback having been called even if you are monitoring all
  -- *  callbacks.
  -- *
  -- *  On some platforms, a window move, resize or menu operation will cause event
  -- *  processing to block.  This is due to how event processing is designed on
  -- *  those platforms.  You can use the
  -- *  [window refresh callback](@ref window_refresh) to redraw the contents of
  -- *  your window when necessary during such operations.
  -- *
  -- *  Do not assume that callbacks you set will _only_ be called in response to
  -- *  event processing functions like this one.  While it is necessary to poll for
  -- *  events, window systems that require GLFW to register callbacks of its own
  -- *  can pass events to GLFW in response to many window system function calls.
  -- *  GLFW will pass those events on to the application callbacks before
  -- *  returning.
  -- *
  -- *  Event processing is not required for joystick input to work.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @reentrancy This function must not be called from a callback.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref events
  -- *  @sa @ref glfwPollEvents
  -- *  @sa @ref glfwWaitEventsTimeout
  -- *
  -- *  @since Added in version 2.5.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwWaitEvents  -- ../include/GLFW/glfw3.h:3957
   with Import => True, 
        Convention => C, 
        External_Name => "glfwWaitEvents";

  --! @brief Waits with timeout until events are queued and processes them.
  -- *
  -- *  This function puts the calling thread to sleep until at least one event is
  -- *  available in the event queue, or until the specified timeout is reached.  If
  -- *  one or more events are available, it behaves exactly like @ref
  -- *  glfwPollEvents, i.e. the events in the queue are processed and the function
  -- *  then returns immediately.  Processing events will cause the window and input
  -- *  callbacks associated with those events to be called.
  -- *
  -- *  The timeout value must be a positive finite number.
  -- *
  -- *  Since not all events are associated with callbacks, this function may return
  -- *  without a callback having been called even if you are monitoring all
  -- *  callbacks.
  -- *
  -- *  On some platforms, a window move, resize or menu operation will cause event
  -- *  processing to block.  This is due to how event processing is designed on
  -- *  those platforms.  You can use the
  -- *  [window refresh callback](@ref window_refresh) to redraw the contents of
  -- *  your window when necessary during such operations.
  -- *
  -- *  Do not assume that callbacks you set will _only_ be called in response to
  -- *  event processing functions like this one.  While it is necessary to poll for
  -- *  events, window systems that require GLFW to register callbacks of its own
  -- *  can pass events to GLFW in response to many window system function calls.
  -- *  GLFW will pass those events on to the application callbacks before
  -- *  returning.
  -- *
  -- *  Event processing is not required for joystick input to work.
  -- *
  -- *  @param[in] timeout The maximum amount of time, in seconds, to wait.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_VALUE and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @reentrancy This function must not be called from a callback.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref events
  -- *  @sa @ref glfwPollEvents
  -- *  @sa @ref glfwWaitEvents
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwWaitEventsTimeout (timeout : double)  -- ../include/GLFW/glfw3.h:4006
   with Import => True, 
        Convention => C, 
        External_Name => "glfwWaitEventsTimeout";

  --! @brief Posts an empty event to the event queue.
  -- *
  -- *  This function posts an empty event from the current thread to the event
  -- *  queue, causing @ref glfwWaitEvents or @ref glfwWaitEventsTimeout to return.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref events
  -- *  @sa @ref glfwWaitEvents
  -- *  @sa @ref glfwWaitEventsTimeout
  -- *
  -- *  @since Added in version 3.1.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwPostEmptyEvent  -- ../include/GLFW/glfw3.h:4026
   with Import => True, 
        Convention => C, 
        External_Name => "glfwPostEmptyEvent";

  --! @brief Returns the value of an input option for the specified window.
  -- *
  -- *  This function returns the value of an input option for the specified window.
  -- *  The mode must be one of @ref GLFW_CURSOR, @ref GLFW_STICKY_KEYS,
  -- *  @ref GLFW_STICKY_MOUSE_BUTTONS, @ref GLFW_LOCK_KEY_MODS or
  -- *  @ref GLFW_RAW_MOUSE_MOTION.
  -- *
  -- *  @param[in] window The window to query.
  -- *  @param[in] mode One of `GLFW_CURSOR`, `GLFW_STICKY_KEYS`,
  -- *  `GLFW_STICKY_MOUSE_BUTTONS`, `GLFW_LOCK_KEY_MODS` or
  -- *  `GLFW_RAW_MOUSE_MOTION`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_INVALID_ENUM.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref glfwSetInputMode
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetInputMode (window : access GLFWwindow; mode : int) return int  -- ../include/GLFW/glfw3.h:4051
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetInputMode";

  --! @brief Sets an input option for the specified window.
  -- *
  -- *  This function sets an input mode option for the specified window.  The mode
  -- *  must be one of @ref GLFW_CURSOR, @ref GLFW_STICKY_KEYS,
  -- *  @ref GLFW_STICKY_MOUSE_BUTTONS, @ref GLFW_LOCK_KEY_MODS or
  -- *  @ref GLFW_RAW_MOUSE_MOTION.
  -- *
  -- *  If the mode is `GLFW_CURSOR`, the value must be one of the following cursor
  -- *  modes:
  -- *  - `GLFW_CURSOR_NORMAL` makes the cursor visible and behaving normally.
  -- *  - `GLFW_CURSOR_HIDDEN` makes the cursor invisible when it is over the
  -- *    content area of the window but does not restrict the cursor from leaving.
  -- *  - `GLFW_CURSOR_DISABLED` hides and grabs the cursor, providing virtual
  -- *    and unlimited cursor movement.  This is useful for implementing for
  -- *    example 3D camera controls.
  -- *
  -- *  If the mode is `GLFW_STICKY_KEYS`, the value must be either `GLFW_TRUE` to
  -- *  enable sticky keys, or `GLFW_FALSE` to disable it.  If sticky keys are
  -- *  enabled, a key press will ensure that @ref glfwGetKey returns `GLFW_PRESS`
  -- *  the next time it is called even if the key had been released before the
  -- *  call.  This is useful when you are only interested in whether keys have been
  -- *  pressed but not when or in which order.
  -- *
  -- *  If the mode is `GLFW_STICKY_MOUSE_BUTTONS`, the value must be either
  -- *  `GLFW_TRUE` to enable sticky mouse buttons, or `GLFW_FALSE` to disable it.
  -- *  If sticky mouse buttons are enabled, a mouse button press will ensure that
  -- *  @ref glfwGetMouseButton returns `GLFW_PRESS` the next time it is called even
  -- *  if the mouse button had been released before the call.  This is useful when
  -- *  you are only interested in whether mouse buttons have been pressed but not
  -- *  when or in which order.
  -- *
  -- *  If the mode is `GLFW_LOCK_KEY_MODS`, the value must be either `GLFW_TRUE` to
  -- *  enable lock key modifier bits, or `GLFW_FALSE` to disable them.  If enabled,
  -- *  callbacks that receive modifier bits will also have the @ref
  -- *  GLFW_MOD_CAPS_LOCK bit set when the event was generated with Caps Lock on,
  -- *  and the @ref GLFW_MOD_NUM_LOCK bit when Num Lock was on.
  -- *
  -- *  If the mode is `GLFW_RAW_MOUSE_MOTION`, the value must be either `GLFW_TRUE`
  -- *  to enable raw (unscaled and unaccelerated) mouse motion when the cursor is
  -- *  disabled, or `GLFW_FALSE` to disable it.  If raw motion is not supported,
  -- *  attempting to set this will emit @ref GLFW_PLATFORM_ERROR.  Call @ref
  -- *  glfwRawMouseMotionSupported to check for support.
  -- *
  -- *  @param[in] window The window whose input mode to set.
  -- *  @param[in] mode One of `GLFW_CURSOR`, `GLFW_STICKY_KEYS`,
  -- *  `GLFW_STICKY_MOUSE_BUTTONS`, `GLFW_LOCK_KEY_MODS` or
  -- *  `GLFW_RAW_MOUSE_MOTION`.
  -- *  @param[in] value The new value of the specified input mode.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref glfwGetInputMode
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwEnable` and `glfwDisable`.
  -- *
  -- *  @ingroup input
  --  

   procedure glfwSetInputMode
     (window : access GLFWwindow;
      mode : int;
      value : int)  -- ../include/GLFW/glfw3.h:4113
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetInputMode";

  --! @brief Returns whether raw mouse motion is supported.
  -- *
  -- *  This function returns whether raw mouse motion is supported on the current
  -- *  system.  This status does not change after GLFW has been initialized so you
  -- *  only need to check this once.  If you attempt to enable raw motion on
  -- *  a system that does not support it, @ref GLFW_PLATFORM_ERROR will be emitted.
  -- *
  -- *  Raw mouse motion is closer to the actual motion of the mouse across
  -- *  a surface.  It is not affected by the scaling and acceleration applied to
  -- *  the motion of the desktop cursor.  That processing is suitable for a cursor
  -- *  while raw motion is better for controlling for example a 3D camera.  Because
  -- *  of this, raw mouse motion is only provided when the cursor is disabled.
  -- *
  -- *  @return `GLFW_TRUE` if raw mouse motion is supported on the current machine,
  -- *  or `GLFW_FALSE` otherwise.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref raw_mouse_motion
  -- *  @sa @ref glfwSetInputMode
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup input
  --  

   function glfwRawMouseMotionSupported return int  -- ../include/GLFW/glfw3.h:4142
   with Import => True, 
        Convention => C, 
        External_Name => "glfwRawMouseMotionSupported";

  --! @brief Returns the layout-specific name of the specified printable key.
  -- *
  -- *  This function returns the name of the specified printable key, encoded as
  -- *  UTF-8.  This is typically the character that key would produce without any
  -- *  modifier keys, intended for displaying key bindings to the user.  For dead
  -- *  keys, it is typically the diacritic it would add to a character.
  -- *
  -- *  __Do not use this function__ for [text input](@ref input_char).  You will
  -- *  break text input for many languages even if it happens to work for yours.
  -- *
  -- *  If the key is `GLFW_KEY_UNKNOWN`, the scancode is used to identify the key,
  -- *  otherwise the scancode is ignored.  If you specify a non-printable key, or
  -- *  `GLFW_KEY_UNKNOWN` and a scancode that maps to a non-printable key, this
  -- *  function returns `NULL` but does not emit an error.
  -- *
  -- *  This behavior allows you to always pass in the arguments in the
  -- *  [key callback](@ref input_key) without modification.
  -- *
  -- *  The printable keys are:
  -- *  - `GLFW_KEY_APOSTROPHE`
  -- *  - `GLFW_KEY_COMMA`
  -- *  - `GLFW_KEY_MINUS`
  -- *  - `GLFW_KEY_PERIOD`
  -- *  - `GLFW_KEY_SLASH`
  -- *  - `GLFW_KEY_SEMICOLON`
  -- *  - `GLFW_KEY_EQUAL`
  -- *  - `GLFW_KEY_LEFT_BRACKET`
  -- *  - `GLFW_KEY_RIGHT_BRACKET`
  -- *  - `GLFW_KEY_BACKSLASH`
  -- *  - `GLFW_KEY_WORLD_1`
  -- *  - `GLFW_KEY_WORLD_2`
  -- *  - `GLFW_KEY_0` to `GLFW_KEY_9`
  -- *  - `GLFW_KEY_A` to `GLFW_KEY_Z`
  -- *  - `GLFW_KEY_KP_0` to `GLFW_KEY_KP_9`
  -- *  - `GLFW_KEY_KP_DECIMAL`
  -- *  - `GLFW_KEY_KP_DIVIDE`
  -- *  - `GLFW_KEY_KP_MULTIPLY`
  -- *  - `GLFW_KEY_KP_SUBTRACT`
  -- *  - `GLFW_KEY_KP_ADD`
  -- *  - `GLFW_KEY_KP_EQUAL`
  -- *
  -- *  Names for printable keys depend on keyboard layout, while names for
  -- *  non-printable keys are the same across layouts but depend on the application
  -- *  language and should be localized along with other user interface text.
  -- *
  -- *  @param[in] key The key to query, or `GLFW_KEY_UNKNOWN`.
  -- *  @param[in] scancode The scancode of the key to query.
  -- *  @return The UTF-8 encoded, layout-specific name of the key, or `NULL`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark The contents of the returned string may change when a keyboard
  -- *  layout change event is received.
  -- *
  -- *  @pointer_lifetime The returned string is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is valid until the library is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref input_key_name
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetKeyName (key : int; scancode : int) return Interfaces.C.Strings.chars_ptr  -- ../include/GLFW/glfw3.h:4210
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetKeyName";

  --! @brief Returns the platform-specific scancode of the specified key.
  -- *
  -- *  This function returns the platform-specific scancode of the specified key.
  -- *
  -- *  If the key is `GLFW_KEY_UNKNOWN` or does not exist on the keyboard this
  -- *  method will return `-1`.
  -- *
  -- *  @param[in] key Any [named key](@ref keys).
  -- *  @return The platform-specific scancode for the key, or `-1` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref input_key
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetKeyScancode (key : int) return int  -- ../include/GLFW/glfw3.h:4234
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetKeyScancode";

  --! @brief Returns the last reported state of a keyboard key for the specified
  -- *  window.
  -- *
  -- *  This function returns the last state reported for the specified key to the
  -- *  specified window.  The returned state is one of `GLFW_PRESS` or
  -- *  `GLFW_RELEASE`.  The higher-level action `GLFW_REPEAT` is only reported to
  -- *  the key callback.
  -- *
  -- *  If the @ref GLFW_STICKY_KEYS input mode is enabled, this function returns
  -- *  `GLFW_PRESS` the first time you call it for a key that was pressed, even if
  -- *  that key has already been released.
  -- *
  -- *  The key functions deal with physical keys, with [key tokens](@ref keys)
  -- *  named after their use on the standard US keyboard layout.  If you want to
  -- *  input text, use the Unicode character callback instead.
  -- *
  -- *  The [modifier key bit masks](@ref mods) are not key tokens and cannot be
  -- *  used with this function.
  -- *
  -- *  __Do not use this function__ to implement [text input](@ref input_char).
  -- *
  -- *  @param[in] window The desired window.
  -- *  @param[in] key The desired [keyboard key](@ref keys).  `GLFW_KEY_UNKNOWN` is
  -- *  not a valid key for this function.
  -- *  @return One of `GLFW_PRESS` or `GLFW_RELEASE`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_INVALID_ENUM.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref input_key
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetKey (window : access GLFWwindow; key : int) return int  -- ../include/GLFW/glfw3.h:4274
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetKey";

  --! @brief Returns the last reported state of a mouse button for the specified
  -- *  window.
  -- *
  -- *  This function returns the last state reported for the specified mouse button
  -- *  to the specified window.  The returned state is one of `GLFW_PRESS` or
  -- *  `GLFW_RELEASE`.
  -- *
  -- *  If the @ref GLFW_STICKY_MOUSE_BUTTONS input mode is enabled, this function
  -- *  returns `GLFW_PRESS` the first time you call it for a mouse button that was
  -- *  pressed, even if that mouse button has already been released.
  -- *
  -- *  @param[in] window The desired window.
  -- *  @param[in] button The desired [mouse button](@ref buttons).
  -- *  @return One of `GLFW_PRESS` or `GLFW_RELEASE`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_INVALID_ENUM.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref input_mouse_button
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetMouseButton (window : access GLFWwindow; button : int) return int  -- ../include/GLFW/glfw3.h:4303
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetMouseButton";

  --! @brief Retrieves the position of the cursor relative to the content area of
  -- *  the window.
  -- *
  -- *  This function returns the position of the cursor, in screen coordinates,
  -- *  relative to the upper-left corner of the content area of the specified
  -- *  window.
  -- *
  -- *  If the cursor is disabled (with `GLFW_CURSOR_DISABLED`) then the cursor
  -- *  position is unbounded and limited only by the minimum and maximum values of
  -- *  a `double`.
  -- *
  -- *  The coordinate can be converted to their integer equivalents with the
  -- *  `floor` function.  Casting directly to an integer type works for positive
  -- *  coordinates, but fails for negative ones.
  -- *
  -- *  Any or all of the position arguments may be `NULL`.  If an error occurs, all
  -- *  non-`NULL` position arguments will be set to zero.
  -- *
  -- *  @param[in] window The desired window.
  -- *  @param[out] xpos Where to store the cursor x-coordinate, relative to the
  -- *  left edge of the content area, or `NULL`.
  -- *  @param[out] ypos Where to store the cursor y-coordinate, relative to the to
  -- *  top edge of the content area, or `NULL`.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref cursor_pos
  -- *  @sa @ref glfwSetCursorPos
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwGetMousePos`.
  -- *
  -- *  @ingroup input
  --  

   procedure glfwGetCursorPos
     (window : access GLFWwindow;
      xpos : access double;
      ypos : access double)  -- ../include/GLFW/glfw3.h:4341
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetCursorPos";

  --! @brief Sets the position of the cursor, relative to the content area of the
  -- *  window.
  -- *
  -- *  This function sets the position, in screen coordinates, of the cursor
  -- *  relative to the upper-left corner of the content area of the specified
  -- *  window.  The window must have input focus.  If the window does not have
  -- *  input focus when this function is called, it fails silently.
  -- *
  -- *  __Do not use this function__ to implement things like camera controls.  GLFW
  -- *  already provides the `GLFW_CURSOR_DISABLED` cursor mode that hides the
  -- *  cursor, transparently re-centers it and provides unconstrained cursor
  -- *  motion.  See @ref glfwSetInputMode for more information.
  -- *
  -- *  If the cursor mode is `GLFW_CURSOR_DISABLED` then the cursor position is
  -- *  unconstrained and limited only by the minimum and maximum values of
  -- *  a `double`.
  -- *
  -- *  @param[in] window The desired window.
  -- *  @param[in] xpos The desired x-coordinate, relative to the left edge of the
  -- *  content area.
  -- *  @param[in] ypos The desired y-coordinate, relative to the top edge of the
  -- *  content area.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @wayland This function will only work when the cursor mode is
  -- *  `GLFW_CURSOR_DISABLED`, otherwise it will do nothing.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref cursor_pos
  -- *  @sa @ref glfwGetCursorPos
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwSetMousePos`.
  -- *
  -- *  @ingroup input
  --  

   procedure glfwSetCursorPos
     (window : access GLFWwindow;
      xpos : double;
      ypos : double)  -- ../include/GLFW/glfw3.h:4381
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetCursorPos";

  --! @brief Creates a custom cursor.
  -- *
  -- *  Creates a new custom cursor image that can be set for a window with @ref
  -- *  glfwSetCursor.  The cursor can be destroyed with @ref glfwDestroyCursor.
  -- *  Any remaining cursors are destroyed by @ref glfwTerminate.
  -- *
  -- *  The pixels are 32-bit, little-endian, non-premultiplied RGBA, i.e. eight
  -- *  bits per channel with the red channel first.  They are arranged canonically
  -- *  as packed sequential rows, starting from the top-left corner.
  -- *
  -- *  The cursor hotspot is specified in pixels, relative to the upper-left corner
  -- *  of the cursor image.  Like all other coordinate systems in GLFW, the X-axis
  -- *  points to the right and the Y-axis points down.
  -- *
  -- *  @param[in] image The desired cursor image.
  -- *  @param[in] xhot The desired x-coordinate, in pixels, of the cursor hotspot.
  -- *  @param[in] yhot The desired y-coordinate, in pixels, of the cursor hotspot.
  -- *  @return The handle of the created cursor, or `NULL` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The specified image data is copied before this function
  -- *  returns.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref cursor_object
  -- *  @sa @ref glfwDestroyCursor
  -- *  @sa @ref glfwCreateStandardCursor
  -- *
  -- *  @since Added in version 3.1.
  -- *
  -- *  @ingroup input
  --  

   function glfwCreateCursor
     (image : access constant GLFWimage;
      xhot : int;
      yhot : int) return access GLFWcursor  -- ../include/GLFW/glfw3.h:4419
   with Import => True, 
        Convention => C, 
        External_Name => "glfwCreateCursor";

  --! @brief Creates a cursor with a standard shape.
  -- *
  -- *  Returns a cursor with a [standard shape](@ref shapes), that can be set for
  -- *  a window with @ref glfwSetCursor.
  -- *
  -- *  @param[in] shape One of the [standard shapes](@ref shapes).
  -- *  @return A new cursor ready to use or `NULL` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref cursor_object
  -- *  @sa @ref glfwCreateCursor
  -- *
  -- *  @since Added in version 3.1.
  -- *
  -- *  @ingroup input
  --  

   function glfwCreateStandardCursor (shape : int) return access GLFWcursor  -- ../include/GLFW/glfw3.h:4442
   with Import => True, 
        Convention => C, 
        External_Name => "glfwCreateStandardCursor";

  --! @brief Destroys a cursor.
  -- *
  -- *  This function destroys a cursor previously created with @ref
  -- *  glfwCreateCursor.  Any remaining cursors will be destroyed by @ref
  -- *  glfwTerminate.
  -- *
  -- *  If the specified cursor is current for any window, that window will be
  -- *  reverted to the default cursor.  This does not affect the cursor mode.
  -- *
  -- *  @param[in] cursor The cursor object to destroy.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @reentrancy This function must not be called from a callback.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref cursor_object
  -- *  @sa @ref glfwCreateCursor
  -- *
  -- *  @since Added in version 3.1.
  -- *
  -- *  @ingroup input
  --  

   procedure glfwDestroyCursor (cursor : access GLFWcursor)  -- ../include/GLFW/glfw3.h:4469
   with Import => True, 
        Convention => C, 
        External_Name => "glfwDestroyCursor";

  --! @brief Sets the cursor for the window.
  -- *
  -- *  This function sets the cursor image to be used when the cursor is over the
  -- *  content area of the specified window.  The set cursor will only be visible
  -- *  when the [cursor mode](@ref cursor_mode) of the window is
  -- *  `GLFW_CURSOR_NORMAL`.
  -- *
  -- *  On some platforms, the set cursor may not be visible unless the window also
  -- *  has input focus.
  -- *
  -- *  @param[in] window The window to set the cursor for.
  -- *  @param[in] cursor The cursor to set, or `NULL` to switch back to the default
  -- *  arrow cursor.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref cursor_object
  -- *
  -- *  @since Added in version 3.1.
  -- *
  -- *  @ingroup input
  --  

   procedure glfwSetCursor (window : access GLFWwindow; cursor : access GLFWcursor)  -- ../include/GLFW/glfw3.h:4496
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetCursor";

  --! @brief Sets the key callback.
  -- *
  -- *  This function sets the key callback of the specified window, which is called
  -- *  when a key is pressed, repeated or released.
  -- *
  -- *  The key functions deal with physical keys, with layout independent
  -- *  [key tokens](@ref keys) named after their values in the standard US keyboard
  -- *  layout.  If you want to input text, use the
  -- *  [character callback](@ref glfwSetCharCallback) instead.
  -- *
  -- *  When a window loses input focus, it will generate synthetic key release
  -- *  events for all pressed keys.  You can tell these events from user-generated
  -- *  events by the fact that the synthetic ones are generated after the focus
  -- *  loss event has been processed, i.e. after the
  -- *  [window focus callback](@ref glfwSetWindowFocusCallback) has been called.
  -- *
  -- *  The scancode of a key is specific to that platform or sometimes even to that
  -- *  machine.  Scancodes are intended to allow users to bind keys that don't have
  -- *  a GLFW key token.  Such keys have `key` set to `GLFW_KEY_UNKNOWN`, their
  -- *  state is not saved and so it cannot be queried with @ref glfwGetKey.
  -- *
  -- *  Sometimes GLFW needs to generate synthetic key events, in which case the
  -- *  scancode may be zero.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new key callback, or `NULL` to remove the currently
  -- *  set callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int key, int scancode, int action, int mods)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWkeyfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref input_key
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle parameter and return value.
  -- *
  -- *  @ingroup input
  --  

   function glfwSetKeyCallback (window : access GLFWwindow; callback : GLFWkeyfun) return GLFWkeyfun  -- ../include/GLFW/glfw3.h:4546
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetKeyCallback";

  --! @brief Sets the Unicode character callback.
  -- *
  -- *  This function sets the character callback of the specified window, which is
  -- *  called when a Unicode character is input.
  -- *
  -- *  The character callback is intended for Unicode text input.  As it deals with
  -- *  characters, it is keyboard layout dependent, whereas the
  -- *  [key callback](@ref glfwSetKeyCallback) is not.  Characters do not map 1:1
  -- *  to physical keys, as a key may produce zero, one or more characters.  If you
  -- *  want to know whether a specific physical key was pressed or released, see
  -- *  the key callback instead.
  -- *
  -- *  The character callback behaves as system text input normally does and will
  -- *  not be called if modifier keys are held down that would prevent normal text
  -- *  input on that platform, for example a Super (Command) key on macOS or Alt key
  -- *  on Windows.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, unsigned int codepoint)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWcharfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref input_char
  -- *
  -- *  @since Added in version 2.4.
  -- *  @glfw3 Added window handle parameter and return value.
  -- *
  -- *  @ingroup input
  --  

   function glfwSetCharCallback (window : access GLFWwindow; callback : GLFWcharfun) return GLFWcharfun  -- ../include/GLFW/glfw3.h:4589
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetCharCallback";

  --! @brief Sets the Unicode character with modifiers callback.
  -- *
  -- *  This function sets the character with modifiers callback of the specified
  -- *  window, which is called when a Unicode character is input regardless of what
  -- *  modifier keys are used.
  -- *
  -- *  The character with modifiers callback is intended for implementing custom
  -- *  Unicode character input.  For regular Unicode text input, see the
  -- *  [character callback](@ref glfwSetCharCallback).  Like the character
  -- *  callback, the character with modifiers callback deals with characters and is
  -- *  keyboard layout dependent.  Characters do not map 1:1 to physical keys, as
  -- *  a key may produce zero, one or more characters.  If you want to know whether
  -- *  a specific physical key was pressed or released, see the
  -- *  [key callback](@ref glfwSetKeyCallback) instead.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, unsigned int codepoint, int mods)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWcharmodsfun).
  -- *
  -- *  @deprecated Scheduled for removal in version 4.0.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref input_char
  -- *
  -- *  @since Added in version 3.1.
  -- *
  -- *  @ingroup input
  --  

   function glfwSetCharModsCallback (window : access GLFWwindow; callback : GLFWcharmodsfun) return GLFWcharmodsfun  -- ../include/GLFW/glfw3.h:4631
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetCharModsCallback";

  --! @brief Sets the mouse button callback.
  -- *
  -- *  This function sets the mouse button callback of the specified window, which
  -- *  is called when a mouse button is pressed or released.
  -- *
  -- *  When a window loses input focus, it will generate synthetic mouse button
  -- *  release events for all pressed mouse buttons.  You can tell these events
  -- *  from user-generated events by the fact that the synthetic ones are generated
  -- *  after the focus loss event has been processed, i.e. after the
  -- *  [window focus callback](@ref glfwSetWindowFocusCallback) has been called.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int button, int action, int mods)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWmousebuttonfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref input_mouse_button
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle parameter and return value.
  -- *
  -- *  @ingroup input
  --  

   function glfwSetMouseButtonCallback (window : access GLFWwindow; callback : GLFWmousebuttonfun) return GLFWmousebuttonfun  -- ../include/GLFW/glfw3.h:4668
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetMouseButtonCallback";

  --! @brief Sets the cursor position callback.
  -- *
  -- *  This function sets the cursor position callback of the specified window,
  -- *  which is called when the cursor is moved.  The callback is provided with the
  -- *  position, in screen coordinates, relative to the upper-left corner of the
  -- *  content area of the window.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, double xpos, double ypos);
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWcursorposfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref cursor_pos
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwSetMousePosCallback`.
  -- *
  -- *  @ingroup input
  --  

   function glfwSetCursorPosCallback (window : access GLFWwindow; callback : GLFWcursorposfun) return GLFWcursorposfun  -- ../include/GLFW/glfw3.h:4700
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetCursorPosCallback";

  --! @brief Sets the cursor enter/leave callback.
  -- *
  -- *  This function sets the cursor boundary crossing callback of the specified
  -- *  window, which is called when the cursor enters or leaves the content area of
  -- *  the window.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int entered)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWcursorenterfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref cursor_enter
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup input
  --  

   function glfwSetCursorEnterCallback (window : access GLFWwindow; callback : GLFWcursorenterfun) return GLFWcursorenterfun  -- ../include/GLFW/glfw3.h:4731
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetCursorEnterCallback";

  --! @brief Sets the scroll callback.
  -- *
  -- *  This function sets the scroll callback of the specified window, which is
  -- *  called when a scrolling device is used, such as a mouse wheel or scrolling
  -- *  area of a touchpad.
  -- *
  -- *  The scroll callback receives all scrolling input, like that from a mouse
  -- *  wheel or a touchpad scrolling area.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new scroll callback, or `NULL` to remove the
  -- *  currently set callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, double xoffset, double yoffset)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWscrollfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref scrolling
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwSetMouseWheelCallback`.
  -- *
  -- *  @ingroup input
  --  

   function glfwSetScrollCallback (window : access GLFWwindow; callback : GLFWscrollfun) return GLFWscrollfun  -- ../include/GLFW/glfw3.h:4765
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetScrollCallback";

  --! @brief Sets the path drop callback.
  -- *
  -- *  This function sets the path drop callback of the specified window, which is
  -- *  called when one or more dragged paths are dropped on the window.
  -- *
  -- *  Because the path array and its strings may have been generated specifically
  -- *  for that event, they are not guaranteed to be valid after the callback has
  -- *  returned.  If you wish to use them after the callback returns, you need to
  -- *  make a deep copy.
  -- *
  -- *  @param[in] window The window whose callback to set.
  -- *  @param[in] callback The new file drop callback, or `NULL` to remove the
  -- *  currently set callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(GLFWwindow* window, int path_count, const char* paths[])
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWdropfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @remark @wayland File drop is currently unimplemented.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref path_drop
  -- *
  -- *  @since Added in version 3.1.
  -- *
  -- *  @ingroup input
  --  

   function glfwSetDropCallback (window : access GLFWwindow; callback : GLFWdropfun) return GLFWdropfun  -- ../include/GLFW/glfw3.h:4802
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetDropCallback";

  --! @brief Returns whether the specified joystick is present.
  -- *
  -- *  This function returns whether the specified joystick is present.
  -- *
  -- *  There is no need to call this function before other functions that accept
  -- *  a joystick ID, as they all check for presence before performing any other
  -- *  work.
  -- *
  -- *  @param[in] jid The [joystick](@ref joysticks) to query.
  -- *  @return `GLFW_TRUE` if the joystick is present, or `GLFW_FALSE` otherwise.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref joystick
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwGetJoystickParam`.
  -- *
  -- *  @ingroup input
  --  

   function glfwJoystickPresent (jid : int) return int  -- ../include/GLFW/glfw3.h:4826
   with Import => True, 
        Convention => C, 
        External_Name => "glfwJoystickPresent";

  --! @brief Returns the values of all axes of the specified joystick.
  -- *
  -- *  This function returns the values of all axes of the specified joystick.
  -- *  Each element in the array is a value between -1.0 and 1.0.
  -- *
  -- *  If the specified joystick is not present this function will return `NULL`
  -- *  but will not generate an error.  This can be used instead of first calling
  -- *  @ref glfwJoystickPresent.
  -- *
  -- *  @param[in] jid The [joystick](@ref joysticks) to query.
  -- *  @param[out] count Where to store the number of axis values in the returned
  -- *  array.  This is set to zero if the joystick is not present or an error
  -- *  occurred.
  -- *  @return An array of axis values, or `NULL` if the joystick is not present or
  -- *  an [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The returned array is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is valid until the specified joystick is
  -- *  disconnected or the library is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref joystick_axis
  -- *
  -- *  @since Added in version 3.0.  Replaces `glfwGetJoystickPos`.
  -- *
  -- *  @ingroup input
  --  

   --function glfwGetJoystickAxes (jid : int; count : access int) return access float
   --with Import => True, 
   --     Convention => C, 
   --     External_Name => "glfwGetJoystickAxes";

	--fastrgv:
	subtype axsrange is integer range 0..3; 
	--gamepad: akount=4 => 0..3 is Ok
	--joystik: akount=3 => 0..2 is Ok
	type joyAxsArrayType is array(axsrange) of float;

   function glfwGetJoystickAxes (jid : int; count : access int) 
		return access joyAxsArrayType
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetJoystickAxes";



  --! @brief Returns the state of all buttons of the specified joystick.
  -- *
  -- *  This function returns the state of all buttons of the specified joystick.
  -- *  Each element in the array is either `GLFW_PRESS` or `GLFW_RELEASE`.
  -- *
  -- *  For backward compatibility with earlier versions that did not have @ref
  -- *  glfwGetJoystickHats, the button array also includes all hats, each
  -- *  represented as four buttons.  The hats are in the same order as returned by
  -- *  __glfwGetJoystickHats__ and are in the order _up_, _right_, _down_ and
  -- *  _left_.  To disable these extra buttons, set the @ref
  -- *  GLFW_JOYSTICK_HAT_BUTTONS init hint before initialization.
  -- *
  -- *  If the specified joystick is not present this function will return `NULL`
  -- *  but will not generate an error.  This can be used instead of first calling
  -- *  @ref glfwJoystickPresent.
  -- *
  -- *  @param[in] jid The [joystick](@ref joysticks) to query.
  -- *  @param[out] count Where to store the number of button states in the returned
  -- *  array.  This is set to zero if the joystick is not present or an error
  -- *  occurred.
  -- *  @return An array of button states, or `NULL` if the joystick is not present
  -- *  or an [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The returned array is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is valid until the specified joystick is
  -- *  disconnected or the library is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref joystick_button
  -- *
  -- *  @since Added in version 2.2.
  -- *  @glfw3 Changed to return a dynamic array.
  -- *
  -- *  @ingroup input
  --  

   --function glfwGetJoystickButtons (jid : int; count : access int) return access unsigned_char
   --with Import => True, 
   --     Convention => C, 
   --     External_Name => "glfwGetJoystickButtons";



	--fastrgv:
	subtype btnrange is integer range 0..15; 
	--gamepad: bkount=16 => 0..15 is Ok
	--joystik: bkount=8 => 0..7 is Ok
	type joyBtnArrayType is array(btnrange) of unsigned_char;

   function glfwGetJoystickButtons (jid : int; count : access int) 
		return access joyBtnArrayType
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetJoystickButtons";






  --! @brief Returns the state of all hats of the specified joystick.
  -- *
  -- *  This function returns the state of all hats of the specified joystick.
  -- *  Each element in the array is one of the following values:
  -- *
  -- *  Name                  | Value
  -- *  ----                  | -----
  -- *  `GLFW_HAT_CENTERED`   | 0
  -- *  `GLFW_HAT_UP`         | 1
  -- *  `GLFW_HAT_RIGHT`      | 2
  -- *  `GLFW_HAT_DOWN`       | 4
  -- *  `GLFW_HAT_LEFT`       | 8
  -- *  `GLFW_HAT_RIGHT_UP`   | `GLFW_HAT_RIGHT` \| `GLFW_HAT_UP`
  -- *  `GLFW_HAT_RIGHT_DOWN` | `GLFW_HAT_RIGHT` \| `GLFW_HAT_DOWN`
  -- *  `GLFW_HAT_LEFT_UP`    | `GLFW_HAT_LEFT` \| `GLFW_HAT_UP`
  -- *  `GLFW_HAT_LEFT_DOWN`  | `GLFW_HAT_LEFT` \| `GLFW_HAT_DOWN`
  -- *
  -- *  The diagonal directions are bitwise combinations of the primary (up, right,
  -- *  down and left) directions and you can test for these individually by ANDing
  -- *  it with the corresponding direction.
  -- *
  -- *  @code
  -- *  if (hats[2] & GLFW_HAT_RIGHT)
  -- *  {
  -- *      // State of hat 2 could be right-up, right or right-down
  -- *  }
  -- *  @endcode
  -- *
  -- *  If the specified joystick is not present this function will return `NULL`
  -- *  but will not generate an error.  This can be used instead of first calling
  -- *  @ref glfwJoystickPresent.
  -- *
  -- *  @param[in] jid The [joystick](@ref joysticks) to query.
  -- *  @param[out] count Where to store the number of hat states in the returned
  -- *  array.  This is set to zero if the joystick is not present or an error
  -- *  occurred.
  -- *  @return An array of hat states, or `NULL` if the joystick is not present
  -- *  or an [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The returned array is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is valid until the specified joystick is
  -- *  disconnected, this function is called again for that joystick or the library
  -- *  is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref joystick_hat
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetJoystickHats (jid : int; count : access int) return access unsigned_char  -- ../include/GLFW/glfw3.h:4957
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetJoystickHats";

  --! @brief Returns the name of the specified joystick.
  -- *
  -- *  This function returns the name, encoded as UTF-8, of the specified joystick.
  -- *  The returned string is allocated and freed by GLFW.  You should not free it
  -- *  yourself.
  -- *
  -- *  If the specified joystick is not present this function will return `NULL`
  -- *  but will not generate an error.  This can be used instead of first calling
  -- *  @ref glfwJoystickPresent.
  -- *
  -- *  @param[in] jid The [joystick](@ref joysticks) to query.
  -- *  @return The UTF-8 encoded name of the joystick, or `NULL` if the joystick
  -- *  is not present or an [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The returned string is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is valid until the specified joystick is
  -- *  disconnected or the library is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref joystick_name
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetJoystickName (jid : int) return Interfaces.C.Strings.chars_ptr  -- ../include/GLFW/glfw3.h:4988
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetJoystickName";

  --! @brief Returns the SDL compatible GUID of the specified joystick.
  -- *
  -- *  This function returns the SDL compatible GUID, as a UTF-8 encoded
  -- *  hexadecimal string, of the specified joystick.  The returned string is
  -- *  allocated and freed by GLFW.  You should not free it yourself.
  -- *
  -- *  The GUID is what connects a joystick to a gamepad mapping.  A connected
  -- *  joystick will always have a GUID even if there is no gamepad mapping
  -- *  assigned to it.
  -- *
  -- *  If the specified joystick is not present this function will return `NULL`
  -- *  but will not generate an error.  This can be used instead of first calling
  -- *  @ref glfwJoystickPresent.
  -- *
  -- *  The GUID uses the format introduced in SDL 2.0.5.  This GUID tries to
  -- *  uniquely identify the make and model of a joystick but does not identify
  -- *  a specific unit, e.g. all wired Xbox 360 controllers will have the same
  -- *  GUID on that platform.  The GUID for a unit may vary between platforms
  -- *  depending on what hardware information the platform specific APIs provide.
  -- *
  -- *  @param[in] jid The [joystick](@ref joysticks) to query.
  -- *  @return The UTF-8 encoded GUID of the joystick, or `NULL` if the joystick
  -- *  is not present or an [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_INVALID_ENUM and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The returned string is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is valid until the specified joystick is
  -- *  disconnected or the library is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref gamepad
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetJoystickGUID (jid : int) return Interfaces.C.Strings.chars_ptr  -- ../include/GLFW/glfw3.h:5029
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetJoystickGUID";

  --! @brief Sets the user pointer of the specified joystick.
  -- *
  -- *  This function sets the user-defined pointer of the specified joystick.  The
  -- *  current value is retained until the joystick is disconnected.  The initial
  -- *  value is `NULL`.
  -- *
  -- *  This function may be called from the joystick callback, even for a joystick
  -- *  that is being disconnected.
  -- *
  -- *  @param[in] jid The joystick whose pointer to set.
  -- *  @param[in] pointer The new value.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.  Access is not
  -- *  synchronized.
  -- *
  -- *  @sa @ref joystick_userptr
  -- *  @sa @ref glfwGetJoystickUserPointer
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup input
  --  

   procedure glfwSetJoystickUserPointer (jid : int; pointer : System.Address)  -- ../include/GLFW/glfw3.h:5055
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetJoystickUserPointer";

  --! @brief Returns the user pointer of the specified joystick.
  -- *
  -- *  This function returns the current value of the user-defined pointer of the
  -- *  specified joystick.  The initial value is `NULL`.
  -- *
  -- *  This function may be called from the joystick callback, even for a joystick
  -- *  that is being disconnected.
  -- *
  -- *  @param[in] jid The joystick whose pointer to return.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.  Access is not
  -- *  synchronized.
  -- *
  -- *  @sa @ref joystick_userptr
  -- *  @sa @ref glfwSetJoystickUserPointer
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetJoystickUserPointer (jid : int) return System.Address  -- ../include/GLFW/glfw3.h:5079
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetJoystickUserPointer";

  --! @brief Returns whether the specified joystick has a gamepad mapping.
  -- *
  -- *  This function returns whether the specified joystick is both present and has
  -- *  a gamepad mapping.
  -- *
  -- *  If the specified joystick is present but does not have a gamepad mapping
  -- *  this function will return `GLFW_FALSE` but will not generate an error.  Call
  -- *  @ref glfwJoystickPresent to check if a joystick is present regardless of
  -- *  whether it has a mapping.
  -- *
  -- *  @param[in] jid The [joystick](@ref joysticks) to query.
  -- *  @return `GLFW_TRUE` if a joystick is both present and has a gamepad mapping,
  -- *  or `GLFW_FALSE` otherwise.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_INVALID_ENUM.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref gamepad
  -- *  @sa @ref glfwGetGamepadState
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup input
  --  

   function glfwJoystickIsGamepad (jid : int) return int  -- ../include/GLFW/glfw3.h:5107
   with Import => True, 
        Convention => C, 
        External_Name => "glfwJoystickIsGamepad";

  --! @brief Sets the joystick configuration callback.
  -- *
  -- *  This function sets the joystick configuration callback, or removes the
  -- *  currently set callback.  This is called when a joystick is connected to or
  -- *  disconnected from the system.
  -- *
  -- *  For joystick connection and disconnection events to be delivered on all
  -- *  platforms, you need to call one of the [event processing](@ref events)
  -- *  functions.  Joystick disconnection may also be detected and the callback
  -- *  called by joystick functions.  The function will then return whatever it
  -- *  returns if the joystick is not present.
  -- *
  -- *  @param[in] callback The new callback, or `NULL` to remove the currently set
  -- *  callback.
  -- *  @return The previously set callback, or `NULL` if no callback was set or the
  -- *  library had not been [initialized](@ref intro_init).
  -- *
  -- *  @callback_signature
  -- *  @code
  -- *  void function_name(int jid, int event)
  -- *  @endcode
  -- *  For more information about the callback parameters, see the
  -- *  [function pointer type](@ref GLFWjoystickfun).
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref joystick_event
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup input
  --  

   function glfwSetJoystickCallback (callback : GLFWjoystickfun) return GLFWjoystickfun  -- ../include/GLFW/glfw3.h:5143
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetJoystickCallback";

  --! @brief Adds the specified SDL_GameControllerDB gamepad mappings.
  -- *
  -- *  This function parses the specified ASCII encoded string and updates the
  -- *  internal list with any gamepad mappings it finds.  This string may
  -- *  contain either a single gamepad mapping or many mappings separated by
  -- *  newlines.  The parser supports the full format of the `gamecontrollerdb.txt`
  -- *  source file including empty lines and comments.
  -- *
  -- *  See @ref gamepad_mapping for a description of the format.
  -- *
  -- *  If there is already a gamepad mapping for a given GUID in the internal list,
  -- *  it will be replaced by the one passed to this function.  If the library is
  -- *  terminated and re-initialized the internal list will revert to the built-in
  -- *  default.
  -- *
  -- *  @param[in] string The string containing the gamepad mappings.
  -- *  @return `GLFW_TRUE` if successful, or `GLFW_FALSE` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_INVALID_VALUE.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref gamepad
  -- *  @sa @ref glfwJoystickIsGamepad
  -- *  @sa @ref glfwGetGamepadName
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup input
  --  

   function glfwUpdateGamepadMappings (string : Interfaces.C.Strings.chars_ptr) return int  -- ../include/GLFW/glfw3.h:5177
   with Import => True, 
        Convention => C, 
        External_Name => "glfwUpdateGamepadMappings";

  --! @brief Returns the human-readable gamepad name for the specified joystick.
  -- *
  -- *  This function returns the human-readable name of the gamepad from the
  -- *  gamepad mapping assigned to the specified joystick.
  -- *
  -- *  If the specified joystick is not present or does not have a gamepad mapping
  -- *  this function will return `NULL` but will not generate an error.  Call
  -- *  @ref glfwJoystickPresent to check whether it is present regardless of
  -- *  whether it has a mapping.
  -- *
  -- *  @param[in] jid The [joystick](@ref joysticks) to query.
  -- *  @return The UTF-8 encoded name of the gamepad, or `NULL` if the
  -- *  joystick is not present, does not have a mapping or an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @pointer_lifetime The returned string is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is valid until the specified joystick is
  -- *  disconnected, the gamepad mappings are updated or the library is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref gamepad
  -- *  @sa @ref glfwJoystickIsGamepad
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetGamepadName (jid : int) return Interfaces.C.Strings.chars_ptr  -- ../include/GLFW/glfw3.h:5207
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetGamepadName";

  --! @brief Retrieves the state of the specified joystick remapped as a gamepad.
  -- *
  -- *  This function retrieves the state of the specified joystick remapped to
  -- *  an Xbox-like gamepad.
  -- *
  -- *  If the specified joystick is not present or does not have a gamepad mapping
  -- *  this function will return `GLFW_FALSE` but will not generate an error.  Call
  -- *  @ref glfwJoystickPresent to check whether it is present regardless of
  -- *  whether it has a mapping.
  -- *
  -- *  The Guide button may not be available for input as it is often hooked by the
  -- *  system or the Steam client.
  -- *
  -- *  Not all devices have all the buttons or axes provided by @ref
  -- *  GLFWgamepadstate.  Unavailable buttons and axes will always report
  -- *  `GLFW_RELEASE` and 0.0 respectively.
  -- *
  -- *  @param[in] jid The [joystick](@ref joysticks) to query.
  -- *  @param[out] state The gamepad input state of the joystick.
  -- *  @return `GLFW_TRUE` if successful, or `GLFW_FALSE` if no joystick is
  -- *  connected, it has no gamepad mapping or an [error](@ref error_handling)
  -- *  occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_INVALID_ENUM.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref gamepad
  -- *  @sa @ref glfwUpdateGamepadMappings
  -- *  @sa @ref glfwJoystickIsGamepad
  -- *
  -- *  @since Added in version 3.3.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetGamepadState (jid : int; state : access GLFWgamepadstate) return int  -- ../include/GLFW/glfw3.h:5245
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetGamepadState";

  --! @brief Sets the clipboard to the specified string.
  -- *
  -- *  This function sets the system clipboard to the specified, UTF-8 encoded
  -- *  string.
  -- *
  -- *  @param[in] window Deprecated.  Any valid window or `NULL`.
  -- *  @param[in] string A UTF-8 encoded string.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The specified string is copied before this function
  -- *  returns.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref clipboard
  -- *  @sa @ref glfwGetClipboardString
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup input
  --  

   procedure glfwSetClipboardString (window : access GLFWwindow; string : Interfaces.C.Strings.chars_ptr)  -- ../include/GLFW/glfw3.h:5270
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetClipboardString";

  --! @brief Returns the contents of the clipboard as a string.
  -- *
  -- *  This function returns the contents of the system clipboard, if it contains
  -- *  or is convertible to a UTF-8 encoded string.  If the clipboard is empty or
  -- *  if its contents cannot be converted, `NULL` is returned and a @ref
  -- *  GLFW_FORMAT_UNAVAILABLE error is generated.
  -- *
  -- *  @param[in] window Deprecated.  Any valid window or `NULL`.
  -- *  @return The contents of the clipboard as a UTF-8 encoded string, or `NULL`
  -- *  if an [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @pointer_lifetime The returned string is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is valid until the next call to @ref
  -- *  glfwGetClipboardString or @ref glfwSetClipboardString, or until the library
  -- *  is terminated.
  -- *
  -- *  @thread_safety This function must only be called from the main thread.
  -- *
  -- *  @sa @ref clipboard
  -- *  @sa @ref glfwSetClipboardString
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetClipboardString (window : access GLFWwindow) return Interfaces.C.Strings.chars_ptr  -- ../include/GLFW/glfw3.h:5300
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetClipboardString";

  --! @brief Returns the GLFW time.
  -- *
  -- *  This function returns the current GLFW time, in seconds.  Unless the time
  -- *  has been set using @ref glfwSetTime it measures time elapsed since GLFW was
  -- *  initialized.
  -- *
  -- *  This function and @ref glfwSetTime are helper functions on top of @ref
  -- *  glfwGetTimerFrequency and @ref glfwGetTimerValue.
  -- *
  -- *  The resolution of the timer is system dependent, but is usually on the order
  -- *  of a few micro- or nanoseconds.  It uses the highest-resolution monotonic
  -- *  time source on each supported platform.
  -- *
  -- *  @return The current time, in seconds, or zero if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.  Reading and
  -- *  writing of the internal base time is not atomic, so it needs to be
  -- *  externally synchronized with calls to @ref glfwSetTime.
  -- *
  -- *  @sa @ref time
  -- *
  -- *  @since Added in version 1.0.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetTime return double  -- ../include/GLFW/glfw3.h:5330
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetTime";

  --! @brief Sets the GLFW time.
  -- *
  -- *  This function sets the current GLFW time, in seconds.  The value must be
  -- *  a positive finite number less than or equal to 18446744073.0, which is
  -- *  approximately 584.5 years.
  -- *
  -- *  This function and @ref glfwGetTime are helper functions on top of @ref
  -- *  glfwGetTimerFrequency and @ref glfwGetTimerValue.
  -- *
  -- *  @param[in] time The new value, in seconds.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_INVALID_VALUE.
  -- *
  -- *  @remark The upper limit of GLFW time is calculated as
  -- *  floor((2<sup>64</sup> - 1) / 10<sup>9</sup>) and is due to implementations
  -- *  storing nanoseconds in 64 bits.  The limit may be increased in the future.
  -- *
  -- *  @thread_safety This function may be called from any thread.  Reading and
  -- *  writing of the internal base time is not atomic, so it needs to be
  -- *  externally synchronized with calls to @ref glfwGetTime.
  -- *
  -- *  @sa @ref time
  -- *
  -- *  @since Added in version 2.2.
  -- *
  -- *  @ingroup input
  --  

   procedure glfwSetTime (time : double)  -- ../include/GLFW/glfw3.h:5360
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSetTime";

  --! @brief Returns the current value of the raw timer.
  -- *
  -- *  This function returns the current value of the raw timer, measured in
  -- *  1&nbsp;/&nbsp;frequency seconds.  To get the frequency, call @ref
  -- *  glfwGetTimerFrequency.
  -- *
  -- *  @return The value of the timer, or zero if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref time
  -- *  @sa @ref glfwGetTimerFrequency
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetTimerValue return uint64_t  -- ../include/GLFW/glfw3.h:5382
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetTimerValue";

  --! @brief Returns the frequency, in Hz, of the raw timer.
  -- *
  -- *  This function returns the frequency, in Hz, of the raw timer.
  -- *
  -- *  @return The frequency of the timer, in Hz, or zero if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref time
  -- *  @sa @ref glfwGetTimerValue
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup input
  --  

   function glfwGetTimerFrequency return uint64_t  -- ../include/GLFW/glfw3.h:5402
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetTimerFrequency";

  --! @brief Makes the context of the specified window current for the calling
  -- *  thread.
  -- *
  -- *  This function makes the OpenGL or OpenGL ES context of the specified window
  -- *  current on the calling thread.  A context must only be made current on
  -- *  a single thread at a time and each thread can have only a single current
  -- *  context at a time.
  -- *
  -- *  When moving a context between threads, you must make it non-current on the
  -- *  old thread before making it current on the new one.
  -- *
  -- *  By default, making a context non-current implicitly forces a pipeline flush.
  -- *  On machines that support `GL_KHR_context_flush_control`, you can control
  -- *  whether a context performs this flush by setting the
  -- *  [GLFW_CONTEXT_RELEASE_BEHAVIOR](@ref GLFW_CONTEXT_RELEASE_BEHAVIOR_hint)
  -- *  hint.
  -- *
  -- *  The specified window must have an OpenGL or OpenGL ES context.  Specifying
  -- *  a window without a context will generate a @ref GLFW_NO_WINDOW_CONTEXT
  -- *  error.
  -- *
  -- *  @param[in] window The window whose context to make current, or `NULL` to
  -- *  detach the current context.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_NO_WINDOW_CONTEXT and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref context_current
  -- *  @sa @ref glfwGetCurrentContext
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup context
  --  

   procedure glfwMakeContextCurrent (window : access GLFWwindow)  -- ../include/GLFW/glfw3.h:5440
   with Import => True, 
        Convention => C, 
        External_Name => "glfwMakeContextCurrent";

  --! @brief Returns the window whose context is current on the calling thread.
  -- *
  -- *  This function returns the window whose OpenGL or OpenGL ES context is
  -- *  current on the calling thread.
  -- *
  -- *  @return The window whose context is current, or `NULL` if no window's
  -- *  context is current.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref context_current
  -- *  @sa @ref glfwMakeContextCurrent
  -- *
  -- *  @since Added in version 3.0.
  -- *
  -- *  @ingroup context
  --  

   function glfwGetCurrentContext return access GLFWwindow  -- ../include/GLFW/glfw3.h:5461
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetCurrentContext";

  --! @brief Swaps the front and back buffers of the specified window.
  -- *
  -- *  This function swaps the front and back buffers of the specified window when
  -- *  rendering with OpenGL or OpenGL ES.  If the swap interval is greater than
  -- *  zero, the GPU driver waits the specified number of screen updates before
  -- *  swapping the buffers.
  -- *
  -- *  The specified window must have an OpenGL or OpenGL ES context.  Specifying
  -- *  a window without a context will generate a @ref GLFW_NO_WINDOW_CONTEXT
  -- *  error.
  -- *
  -- *  This function does not apply to Vulkan.  If you are rendering with Vulkan,
  -- *  see `vkQueuePresentKHR` instead.
  -- *
  -- *  @param[in] window The window whose buffers to swap.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_NO_WINDOW_CONTEXT and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark __EGL:__ The context of the specified window must be current on the
  -- *  calling thread.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref buffer_swap
  -- *  @sa @ref glfwSwapInterval
  -- *
  -- *  @since Added in version 1.0.
  -- *  @glfw3 Added window handle parameter.
  -- *
  -- *  @ingroup window
  --  

   procedure glfwSwapBuffers (window : access GLFWwindow)  -- ../include/GLFW/glfw3.h:5495
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSwapBuffers";

  --! @brief Sets the swap interval for the current context.
  -- *
  -- *  This function sets the swap interval for the current OpenGL or OpenGL ES
  -- *  context, i.e. the number of screen updates to wait from the time @ref
  -- *  glfwSwapBuffers was called before swapping the buffers and returning.  This
  -- *  is sometimes called _vertical synchronization_, _vertical retrace
  -- *  synchronization_ or just _vsync_.
  -- *
  -- *  A context that supports either of the `WGL_EXT_swap_control_tear` and
  -- *  `GLX_EXT_swap_control_tear` extensions also accepts _negative_ swap
  -- *  intervals, which allows the driver to swap immediately even if a frame
  -- *  arrives a little bit late.  You can check for these extensions with @ref
  -- *  glfwExtensionSupported.
  -- *
  -- *  A context must be current on the calling thread.  Calling this function
  -- *  without a current context will cause a @ref GLFW_NO_CURRENT_CONTEXT error.
  -- *
  -- *  This function does not apply to Vulkan.  If you are rendering with Vulkan,
  -- *  see the present mode of your swapchain instead.
  -- *
  -- *  @param[in] interval The minimum number of screen updates to wait for
  -- *  until the buffers are swapped by @ref glfwSwapBuffers.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_NO_CURRENT_CONTEXT and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark This function is not called during context creation, leaving the
  -- *  swap interval set to whatever is the default on that platform.  This is done
  -- *  because some swap interval extensions used by GLFW do not allow the swap
  -- *  interval to be reset to zero once it has been set to a non-zero value.
  -- *
  -- *  @remark Some GPU drivers do not honor the requested swap interval, either
  -- *  because of a user setting that overrides the application's request or due to
  -- *  bugs in the driver.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref buffer_swap
  -- *  @sa @ref glfwSwapBuffers
  -- *
  -- *  @since Added in version 1.0.
  -- *
  -- *  @ingroup context
  --  

   procedure glfwSwapInterval (interval : int)  -- ../include/GLFW/glfw3.h:5541
   with Import => True, 
        Convention => C, 
        External_Name => "glfwSwapInterval";

  --! @brief Returns whether the specified extension is available.
  -- *
  -- *  This function returns whether the specified
  -- *  [API extension](@ref context_glext) is supported by the current OpenGL or
  -- *  OpenGL ES context.  It searches both for client API extension and context
  -- *  creation API extensions.
  -- *
  -- *  A context must be current on the calling thread.  Calling this function
  -- *  without a current context will cause a @ref GLFW_NO_CURRENT_CONTEXT error.
  -- *
  -- *  As this functions retrieves and searches one or more extension strings each
  -- *  call, it is recommended that you cache its results if it is going to be used
  -- *  frequently.  The extension strings will not change during the lifetime of
  -- *  a context, so there is no danger in doing this.
  -- *
  -- *  This function does not apply to Vulkan.  If you are using Vulkan, see @ref
  -- *  glfwGetRequiredInstanceExtensions, `vkEnumerateInstanceExtensionProperties`
  -- *  and `vkEnumerateDeviceExtensionProperties` instead.
  -- *
  -- *  @param[in] extension The ASCII encoded name of the extension.
  -- *  @return `GLFW_TRUE` if the extension is available, or `GLFW_FALSE`
  -- *  otherwise.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_NO_CURRENT_CONTEXT, @ref GLFW_INVALID_VALUE and @ref
  -- *  GLFW_PLATFORM_ERROR.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref context_glext
  -- *  @sa @ref glfwGetProcAddress
  -- *
  -- *  @since Added in version 1.0.
  -- *
  -- *  @ingroup context
  --  

   function glfwExtensionSupported (extension : Interfaces.C.Strings.chars_ptr) return int  -- ../include/GLFW/glfw3.h:5579
   with Import => True, 
        Convention => C, 
        External_Name => "glfwExtensionSupported";

  --! @brief Returns the address of the specified function for the current
  -- *  context.
  -- *
  -- *  This function returns the address of the specified OpenGL or OpenGL ES
  -- *  [core or extension function](@ref context_glext), if it is supported
  -- *  by the current context.
  -- *
  -- *  A context must be current on the calling thread.  Calling this function
  -- *  without a current context will cause a @ref GLFW_NO_CURRENT_CONTEXT error.
  -- *
  -- *  This function does not apply to Vulkan.  If you are rendering with Vulkan,
  -- *  see @ref glfwGetInstanceProcAddress, `vkGetInstanceProcAddr` and
  -- *  `vkGetDeviceProcAddr` instead.
  -- *
  -- *  @param[in] procname The ASCII encoded name of the function.
  -- *  @return The address of the function, or `NULL` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_NO_CURRENT_CONTEXT and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark The address of a given function is not guaranteed to be the same
  -- *  between contexts.
  -- *
  -- *  @remark This function may return a non-`NULL` address despite the
  -- *  associated version or extension not being available.  Always check the
  -- *  context version or extension string first.
  -- *
  -- *  @pointer_lifetime The returned function pointer is valid until the context
  -- *  is destroyed or the library is terminated.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref context_glext
  -- *  @sa @ref glfwExtensionSupported
  -- *
  -- *  @since Added in version 1.0.
  -- *
  -- *  @ingroup context
  --  

   function glfwGetProcAddress (procname : Interfaces.C.Strings.chars_ptr) return GLFWglproc  -- ../include/GLFW/glfw3.h:5621
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetProcAddress";

  --! @brief Returns whether the Vulkan loader and an ICD have been found.
  -- *
  -- *  This function returns whether the Vulkan loader and any minimally functional
  -- *  ICD have been found.
  -- *
  -- *  The availability of a Vulkan loader and even an ICD does not by itself
  -- *  guarantee that surface creation or even instance creation is possible.
  -- *  For example, on Fermi systems Nvidia will install an ICD that provides no
  -- *  actual Vulkan support.  Call @ref glfwGetRequiredInstanceExtensions to check
  -- *  whether the extensions necessary for Vulkan surface creation are available
  -- *  and @ref glfwGetPhysicalDevicePresentationSupport to check whether a queue
  -- *  family of a physical device supports image presentation.
  -- *
  -- *  @return `GLFW_TRUE` if Vulkan is minimally available, or `GLFW_FALSE`
  -- *  otherwise.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref vulkan_support
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup vulkan
  --  

   function glfwVulkanSupported return int  -- ../include/GLFW/glfw3.h:5649
   with Import => True, 
        Convention => C, 
        External_Name => "glfwVulkanSupported";

  --! @brief Returns the Vulkan instance extensions required by GLFW.
  -- *
  -- *  This function returns an array of names of Vulkan instance extensions required
  -- *  by GLFW for creating Vulkan surfaces for GLFW windows.  If successful, the
  -- *  list will always contain `VK_KHR_surface`, so if you don't require any
  -- *  additional extensions you can pass this list directly to the
  -- *  `VkInstanceCreateInfo` struct.
  -- *
  -- *  If Vulkan is not available on the machine, this function returns `NULL` and
  -- *  generates a @ref GLFW_API_UNAVAILABLE error.  Call @ref glfwVulkanSupported
  -- *  to check whether Vulkan is at least minimally available.
  -- *
  -- *  If Vulkan is available but no set of extensions allowing window surface
  -- *  creation was found, this function returns `NULL`.  You may still use Vulkan
  -- *  for off-screen rendering and compute work.
  -- *
  -- *  @param[out] count Where to store the number of extensions in the returned
  -- *  array.  This is set to zero if an error occurred.
  -- *  @return An array of ASCII encoded extension names, or `NULL` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_API_UNAVAILABLE.
  -- *
  -- *  @remark Additional extensions may be required by future versions of GLFW.
  -- *  You should check if any extensions you wish to enable are already in the
  -- *  returned array, as it is an error to specify an extension more than once in
  -- *  the `VkInstanceCreateInfo` struct.
  -- *
  -- *  @remark @macos This function currently only supports the
  -- *  `VK_MVK_macos_surface` extension from MoltenVK.
  -- *
  -- *  @pointer_lifetime The returned array is allocated and freed by GLFW.  You
  -- *  should not free it yourself.  It is guaranteed to be valid only until the
  -- *  library is terminated.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref vulkan_ext
  -- *  @sa @ref glfwCreateWindowSurface
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup vulkan
  --  

   function glfwGetRequiredInstanceExtensions (count : access uint32_t) return System.Address  -- ../include/GLFW/glfw3.h:5696
   with Import => True, 
        Convention => C, 
        External_Name => "glfwGetRequiredInstanceExtensions";

  --! @brief Returns the address of the specified Vulkan instance function.
  -- *
  -- *  This function returns the address of the specified Vulkan core or extension
  -- *  function for the specified instance.  If instance is set to `NULL` it can
  -- *  return any function exported from the Vulkan loader, including at least the
  -- *  following functions:
  -- *
  -- *  - `vkEnumerateInstanceExtensionProperties`
  -- *  - `vkEnumerateInstanceLayerProperties`
  -- *  - `vkCreateInstance`
  -- *  - `vkGetInstanceProcAddr`
  -- *
  -- *  If Vulkan is not available on the machine, this function returns `NULL` and
  -- *  generates a @ref GLFW_API_UNAVAILABLE error.  Call @ref glfwVulkanSupported
  -- *  to check whether Vulkan is at least minimally available.
  -- *
  -- *  This function is equivalent to calling `vkGetInstanceProcAddr` with
  -- *  a platform-specific query of the Vulkan loader as a fallback.
  -- *
  -- *  @param[in] instance The Vulkan instance to query, or `NULL` to retrieve
  -- *  functions related to instance creation.
  -- *  @param[in] procname The ASCII encoded name of the function.
  -- *  @return The address of the function, or `NULL` if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED and @ref
  -- *  GLFW_API_UNAVAILABLE.
  -- *
  -- *  @pointer_lifetime The returned function pointer is valid until the library
  -- *  is terminated.
  -- *
  -- *  @thread_safety This function may be called from any thread.
  -- *
  -- *  @sa @ref vulkan_proc
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup vulkan
  --  

  --! @brief Returns whether the specified queue family can present images.
  -- *
  -- *  This function returns whether the specified queue family of the specified
  -- *  physical device supports presentation to the platform GLFW was built for.
  -- *
  -- *  If Vulkan or the required window surface creation instance extensions are
  -- *  not available on the machine, or if the specified instance was not created
  -- *  with the required extensions, this function returns `GLFW_FALSE` and
  -- *  generates a @ref GLFW_API_UNAVAILABLE error.  Call @ref glfwVulkanSupported
  -- *  to check whether Vulkan is at least minimally available and @ref
  -- *  glfwGetRequiredInstanceExtensions to check what instance extensions are
  -- *  required.
  -- *
  -- *  @param[in] instance The instance that the physical device belongs to.
  -- *  @param[in] device The physical device that the queue family belongs to.
  -- *  @param[in] queuefamily The index of the queue family to query.
  -- *  @return `GLFW_TRUE` if the queue family supports presentation, or
  -- *  `GLFW_FALSE` otherwise.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_API_UNAVAILABLE and @ref GLFW_PLATFORM_ERROR.
  -- *
  -- *  @remark @macos This function currently always returns `GLFW_TRUE`, as the
  -- *  `VK_MVK_macos_surface` extension does not provide
  -- *  a `vkGetPhysicalDevice*PresentationSupport` type function.
  -- *
  -- *  @thread_safety This function may be called from any thread.  For
  -- *  synchronization details of Vulkan objects, see the Vulkan specification.
  -- *
  -- *  @sa @ref vulkan_present
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup vulkan
  --  

  --! @brief Creates a Vulkan surface for the specified window.
  -- *
  -- *  This function creates a Vulkan surface for the specified window.
  -- *
  -- *  If the Vulkan loader or at least one minimally functional ICD were not found,
  -- *  this function returns `VK_ERROR_INITIALIZATION_FAILED` and generates a @ref
  -- *  GLFW_API_UNAVAILABLE error.  Call @ref glfwVulkanSupported to check whether
  -- *  Vulkan is at least minimally available.
  -- *
  -- *  If the required window surface creation instance extensions are not
  -- *  available or if the specified instance was not created with these extensions
  -- *  enabled, this function returns `VK_ERROR_EXTENSION_NOT_PRESENT` and
  -- *  generates a @ref GLFW_API_UNAVAILABLE error.  Call @ref
  -- *  glfwGetRequiredInstanceExtensions to check what instance extensions are
  -- *  required.
  -- *
  -- *  The window surface cannot be shared with another API so the window must
  -- *  have been created with the [client api hint](@ref GLFW_CLIENT_API_attrib)
  -- *  set to `GLFW_NO_API` otherwise it generates a @ref GLFW_INVALID_VALUE error
  -- *  and returns `VK_ERROR_NATIVE_WINDOW_IN_USE_KHR`.
  -- *
  -- *  The window surface must be destroyed before the specified Vulkan instance.
  -- *  It is the responsibility of the caller to destroy the window surface.  GLFW
  -- *  does not destroy it for you.  Call `vkDestroySurfaceKHR` to destroy the
  -- *  surface.
  -- *
  -- *  @param[in] instance The Vulkan instance to create the surface in.
  -- *  @param[in] window The window to create the surface for.
  -- *  @param[in] allocator The allocator to use, or `NULL` to use the default
  -- *  allocator.
  -- *  @param[out] surface Where to store the handle of the surface.  This is set
  -- *  to `VK_NULL_HANDLE` if an error occurred.
  -- *  @return `VK_SUCCESS` if successful, or a Vulkan error code if an
  -- *  [error](@ref error_handling) occurred.
  -- *
  -- *  @errors Possible errors include @ref GLFW_NOT_INITIALIZED, @ref
  -- *  GLFW_API_UNAVAILABLE, @ref GLFW_PLATFORM_ERROR and @ref GLFW_INVALID_VALUE
  -- *
  -- *  @remark If an error occurs before the creation call is made, GLFW returns
  -- *  the Vulkan error code most appropriate for the error.  Appropriate use of
  -- *  @ref glfwVulkanSupported and @ref glfwGetRequiredInstanceExtensions should
  -- *  eliminate almost all occurrences of these errors.
  -- *
  -- *  @remark @macos This function currently only supports the
  -- *  `VK_MVK_macos_surface` extension from MoltenVK.
  -- *
  -- *  @remark @macos This function creates and sets a `CAMetalLayer` instance for
  -- *  the window content view, which is required for MoltenVK to function.
  -- *
  -- *  @thread_safety This function may be called from any thread.  For
  -- *  synchronization details of Vulkan objects, see the Vulkan specification.
  -- *
  -- *  @sa @ref vulkan_surface
  -- *  @sa @ref glfwGetRequiredInstanceExtensions
  -- *
  -- *  @since Added in version 3.2.
  -- *
  -- *  @ingroup vulkan
  --  

  --************************************************************************
  -- * Global definition cleanup
  -- ************************************************************************ 

  -- ------------------- BEGIN SYSTEM/COMPILER SPECIFIC --------------------  
  -- Some OpenGL related headers need GLAPIENTRY, but it is unconditionally
  -- * defined by some gl.h variants (OpenBSD) so define it after if needed.
  --  

  -- -------------------- END SYSTEM/COMPILER SPECIFIC ---------------------  
end glfw3;
