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


with interfaces.C;
with System;


package GL is

   pragma Pure;

   use Interfaces;


   -- GLenum
   --
   subtype GLenum is C.unsigned;

   type GLenum_array is
     array (C.size_t range <>) of aliased GLenum;

   -- GLboolean
   --
   subtype GLboolean is C.unsigned_char;

   type GLboolean_array is
     array (C.size_t range <>) of aliased GLboolean;

   -- GLbitfield
   --
   subtype GLbitfield is C.unsigned;

   type GLbitfield_array is
     array (C.size_t range <>) of aliased GLbitfield;


   -- GLvoid_Pointer
   --
   subtype GLvoid_Pointer is system.Address;

   type GLvoid_Pointer_array is
     array (C.size_t range <>)
            of aliased GLvoid_Pointer;

   -- GLvoid_Pointer_Pointer
   --
   subtype GLvoid_Pointer_Pointer is system.Address;


   -- GLbyte
   --
   subtype GLbyte is C.signed_char;

   type GLbyte_array is
     array (C.size_t range <>) of aliased GLbyte;

   -- GLshort
   --
   subtype GLshort is C.short;

   type GLshort_array is
     array (C.size_t range <>) of aliased GLshort;

   -- GLint
   --
   subtype GLint is C.int;

   type GLint_array is
     array (C.size_t range <>) of aliased GLint;

   -- GLubyte
   --
   subtype GLubyte is C.unsigned_char;

   type GLubyte_array is
     array (C.size_t range <>) of aliased GLubyte;

   -- GLushort
   --
   subtype GLushort is C.unsigned_short;

   type GLushort_array is
     array (C.size_t range <>) of aliased GLushort;

   -- GLuint
   --
   subtype GLuint is C.unsigned;

   type GLuint_array is
     array (C.size_t range <>) of aliased GLuint;

   -- GLsizei
   --
   subtype GLsizei is C.int;

   type GLsizei_array is
     array (C.size_t range <>) of aliased GLsizei;

   -- GLfloat
   --
   subtype GLfloat is C.C_float;

   type GLfloat_array is
     array (C.size_t range <>) of aliased GLfloat;

   -- GLclampf
   --
   subtype GLclampf is C.C_float;

   type GLclampf_array is
     array (C.size_t range <>) of aliased GLclampf;

   -- GLdouble
   --
   subtype GLdouble is C.double;

   type GLdouble_array is
     array (C.size_t range <>) of aliased GLdouble;

   -- GLclampd
   --
   subtype GLclampd is C.double;

   type GLclampd_array is
     array (C.size_t range <>) of aliased GLclampd;


   GL_VERSION_1_1                              : constant := 1;
   GL_VERSION_1_2                              : constant := 1;
   GL_VERSION_1_3                              : constant := 1;
   GL_ARB_imaging                              : constant := 1;
   GL_FALSE                                    : constant := 16#0#;
   GL_TRUE                                     : constant := 16#1#;
   GL_BYTE                                     : constant := 16#1400#;
   GL_UNSIGNED_BYTE                            : constant := 16#1401#;
   GL_SHORT                                    : constant := 16#1402#;
   GL_UNSIGNED_SHORT                           : constant := 16#1403#;
   GL_INT                                      : constant := 16#1404#;
   GL_UNSIGNED_INT                             : constant := 16#1405#;
   GL_FLOAT                                    : constant := 16#1406#;
   GL_2_BYTES                                  : constant := 16#1407#;
   GL_3_BYTES                                  : constant := 16#1408#;
   GL_4_BYTES                                  : constant := 16#1409#;
   GL_DOUBLE                                   : constant := 16#140a#;
   GL_POINTS                                   : constant := 16#0#;
   GL_LINES                                    : constant := 16#1#;
   GL_LINE_LOOP                                : constant := 16#2#;
   GL_LINE_STRIP                               : constant := 16#3#;
   GL_TRIANGLES                                : constant := 16#4#;
   GL_TRIANGLE_STRIP                           : constant := 16#5#;
   GL_TRIANGLE_FAN                             : constant := 16#6#;
   GL_QUADS                                    : constant := 16#7#;
   GL_QUAD_STRIP                               : constant := 16#8#;
   GL_POLYGON                                  : constant := 16#9#;
   GL_VERTEX_ARRAY                             : constant := 16#8074#;
   GL_NORMAL_ARRAY                             : constant := 16#8075#;
   GL_COLOR_ARRAY                              : constant := 16#8076#;
   GL_INDEX_ARRAY                              : constant := 16#8077#;
   GL_TEXTURE_COORD_ARRAY                      : constant := 16#8078#;
   GL_EDGE_FLAG_ARRAY                          : constant := 16#8079#;
   GL_VERTEX_ARRAY_SIZE                        : constant := 16#807a#;
   GL_VERTEX_ARRAY_TYPE                        : constant := 16#807b#;
   GL_VERTEX_ARRAY_STRIDE                      : constant := 16#807c#;
   GL_NORMAL_ARRAY_TYPE                        : constant := 16#807e#;
   GL_NORMAL_ARRAY_STRIDE                      : constant := 16#807f#;
   GL_COLOR_ARRAY_SIZE                         : constant := 16#8081#;
   GL_COLOR_ARRAY_TYPE                         : constant := 16#8082#;
   GL_COLOR_ARRAY_STRIDE                       : constant := 16#8083#;
   GL_INDEX_ARRAY_TYPE                         : constant := 16#8085#;
   GL_INDEX_ARRAY_STRIDE                       : constant := 16#8086#;
   GL_TEXTURE_COORD_ARRAY_SIZE                 : constant := 16#8088#;
   GL_TEXTURE_COORD_ARRAY_TYPE                 : constant := 16#8089#;
   GL_TEXTURE_COORD_ARRAY_STRIDE               : constant := 16#808a#;
   GL_EDGE_FLAG_ARRAY_STRIDE                   : constant := 16#808c#;
   GL_VERTEX_ARRAY_POINTER                     : constant := 16#808e#;
   GL_NORMAL_ARRAY_POINTER                     : constant := 16#808f#;
   GL_COLOR_ARRAY_POINTER                      : constant := 16#8090#;
   GL_INDEX_ARRAY_POINTER                      : constant := 16#8091#;
   GL_TEXTURE_COORD_ARRAY_POINTER              : constant := 16#8092#;
   GL_EDGE_FLAG_ARRAY_POINTER                  : constant := 16#8093#;
   GL_V2F                                      : constant := 16#2a20#;
   GL_V3F                                      : constant := 16#2a21#;
   GL_C4UB_V2F                                 : constant := 16#2a22#;
   GL_C4UB_V3F                                 : constant := 16#2a23#;
   GL_C3F_V3F                                  : constant := 16#2a24#;
   GL_N3F_V3F                                  : constant := 16#2a25#;
   GL_C4F_N3F_V3F                              : constant := 16#2a26#;
   GL_T2F_V3F                                  : constant := 16#2a27#;
   GL_T4F_V4F                                  : constant := 16#2a28#;
   GL_T2F_C4UB_V3F                             : constant := 16#2a29#;
   GL_T2F_C3F_V3F                              : constant := 16#2a2a#;
   GL_T2F_N3F_V3F                              : constant := 16#2a2b#;
   GL_T2F_C4F_N3F_V3F                          : constant := 16#2a2c#;
   GL_T4F_C4F_N3F_V4F                          : constant := 16#2a2d#;
   GL_MATRIX_MODE                              : constant := 16#ba0#;
   GL_MODELVIEW                                : constant := 16#1700#;
   GL_PROJECTION                               : constant := 16#1701#;
   GL_TEXTURE                                  : constant := 16#1702#;
   GL_POINT_SMOOTH                             : constant := 16#b10#;
   GL_POINT_SIZE                               : constant := 16#b11#;
   GL_POINT_SIZE_GRANULARITY                   : constant := 16#b13#;
   GL_POINT_SIZE_RANGE                         : constant := 16#b12#;
   GL_LINE_SMOOTH                              : constant := 16#b20#;
   GL_LINE_STIPPLE                             : constant := 16#b24#;
   GL_LINE_STIPPLE_PATTERN                     : constant := 16#b25#;
   GL_LINE_STIPPLE_REPEAT                      : constant := 16#b26#;
   GL_LINE_WIDTH                               : constant := 16#b21#;
   GL_LINE_WIDTH_GRANULARITY                   : constant := 16#b23#;
   GL_LINE_WIDTH_RANGE                         : constant := 16#b22#;
   GL_POINT                                    : constant := 16#1b00#;
   GL_LINE                                     : constant := 16#1b01#;
   GL_FILL                                     : constant := 16#1b02#;
   GL_CW                                       : constant := 16#900#;
   GL_CCW                                      : constant := 16#901#;
   GL_FRONT                                    : constant := 16#404#;
   GL_BACK                                     : constant := 16#405#;
   GL_POLYGON_MODE                             : constant := 16#b40#;
   GL_POLYGON_SMOOTH                           : constant := 16#b41#;
   GL_POLYGON_STIPPLE                          : constant := 16#b42#;
   GL_EDGE_FLAG                                : constant := 16#b43#;
   GL_CULL_FACE                                : constant := 16#b44#;
   GL_CULL_FACE_MODE                           : constant := 16#b45#;
   GL_FRONT_FACE                               : constant := 16#b46#;
   GL_POLYGON_OFFSET_FACTOR                    : constant := 16#8038#;
   GL_POLYGON_OFFSET_UNITS                     : constant := 16#2a00#;
   GL_POLYGON_OFFSET_POINT                     : constant := 16#2a01#;
   GL_POLYGON_OFFSET_LINE                      : constant := 16#2a02#;
   GL_POLYGON_OFFSET_FILL                      : constant := 16#8037#;
   GL_COMPILE                                  : constant := 16#1300#;
   GL_COMPILE_AND_EXECUTE                      : constant := 16#1301#;
   GL_LIST_BASE                                : constant := 16#b32#;
   GL_LIST_INDEX                               : constant := 16#b33#;
   GL_LIST_MODE                                : constant := 16#b30#;
   GL_NEVER                                    : constant := 16#200#;
   GL_LESS                                     : constant := 16#201#;
   GL_EQUAL                                    : constant := 16#202#;
   GL_LEQUAL                                   : constant := 16#203#;
   GL_GREATER                                  : constant := 16#204#;
   GL_NOTEQUAL                                 : constant := 16#205#;
   GL_GEQUAL                                   : constant := 16#206#;
   GL_ALWAYS                                   : constant := 16#207#;
   GL_DEPTH_TEST                               : constant := 16#b71#;
   GL_DEPTH_BITS                               : constant := 16#d56#;
   GL_DEPTH_CLEAR_VALUE                        : constant := 16#b73#;
   GL_DEPTH_FUNC                               : constant := 16#b74#;
   GL_DEPTH_RANGE                              : constant := 16#b70#;
   GL_DEPTH_WRITEMASK                          : constant := 16#b72#;
   GL_DEPTH_COMPONENT                          : constant := 16#1902#;
   GL_LIGHTING                                 : constant := 16#b50#;
   GL_LIGHT0                                   : constant := 16#4000#;
   GL_LIGHT1                                   : constant := 16#4001#;
   GL_LIGHT2                                   : constant := 16#4002#;
   GL_LIGHT3                                   : constant := 16#4003#;
   GL_LIGHT4                                   : constant := 16#4004#;
   GL_LIGHT5                                   : constant := 16#4005#;
   GL_LIGHT6                                   : constant := 16#4006#;
   GL_LIGHT7                                   : constant := 16#4007#;
   GL_SPOT_EXPONENT                            : constant := 16#1205#;
   GL_SPOT_CUTOFF                              : constant := 16#1206#;
   GL_CONSTANT_ATTENUATION                     : constant := 16#1207#;
   GL_LINEAR_ATTENUATION                       : constant := 16#1208#;
   GL_QUADRATIC_ATTENUATION                    : constant := 16#1209#;
   GL_AMBIENT                                  : constant := 16#1200#;
   GL_DIFFUSE                                  : constant := 16#1201#;
   GL_SPECULAR                                 : constant := 16#1202#;
   GL_SHININESS                                : constant := 16#1601#;
   GL_EMISSION                                 : constant := 16#1600#;
   GL_POSITION                                 : constant := 16#1203#;
   GL_SPOT_DIRECTION                           : constant := 16#1204#;
   GL_AMBIENT_AND_DIFFUSE                      : constant := 16#1602#;
   GL_COLOR_INDEXES                            : constant := 16#1603#;
   GL_LIGHT_MODEL_TWO_SIDE                     : constant := 16#b52#;
   GL_LIGHT_MODEL_LOCAL_VIEWER                 : constant := 16#b51#;
   GL_LIGHT_MODEL_AMBIENT                      : constant := 16#b53#;
   GL_FRONT_AND_BACK                           : constant := 16#408#;
   GL_SHADE_MODEL                              : constant := 16#b54#;
   GL_FLAT                                     : constant := 16#1d00#;
   GL_SMOOTH                                   : constant := 16#1d01#;
   GL_COLOR_MATERIAL                           : constant := 16#b57#;
   GL_COLOR_MATERIAL_FACE                      : constant := 16#b55#;
   GL_COLOR_MATERIAL_PARAMETER                 : constant := 16#b56#;
   GL_NORMALIZE                                : constant := 16#ba1#;
   GL_CLIP_PLANE0                              : constant := 16#3000#;
   GL_CLIP_PLANE1                              : constant := 16#3001#;
   GL_CLIP_PLANE2                              : constant := 16#3002#;
   GL_CLIP_PLANE3                              : constant := 16#3003#;
   GL_CLIP_PLANE4                              : constant := 16#3004#;
   GL_CLIP_PLANE5                              : constant := 16#3005#;
   GL_ACCUM_RED_BITS                           : constant := 16#d58#;
   GL_ACCUM_GREEN_BITS                         : constant := 16#d59#;
   GL_ACCUM_BLUE_BITS                          : constant := 16#d5a#;
   GL_ACCUM_ALPHA_BITS                         : constant := 16#d5b#;
   GL_ACCUM_CLEAR_VALUE                        : constant := 16#b80#;
   GL_ACCUM                                    : constant := 16#100#;
   GL_ADD                                      : constant := 16#104#;
   GL_LOAD                                     : constant := 16#101#;
   GL_MULT                                     : constant := 16#103#;
   GL_RETURN                                   : constant := 16#102#;
   GL_ALPHA_TEST                               : constant := 16#bc0#;
   GL_ALPHA_TEST_REF                           : constant := 16#bc2#;
   GL_ALPHA_TEST_FUNC                          : constant := 16#bc1#;
   GL_BLEND                                    : constant := 16#be2#;
   GL_BLEND_SRC                                : constant := 16#be1#;
   GL_BLEND_DST                                : constant := 16#be0#;
   GL_ZERO                                     : constant := 16#0#;
   GL_ONE                                      : constant := 16#1#;
   GL_SRC_COLOR                                : constant := 16#300#;
   GL_ONE_MINUS_SRC_COLOR                      : constant := 16#301#;
   GL_SRC_ALPHA                                : constant := 16#302#;
   GL_ONE_MINUS_SRC_ALPHA                      : constant := 16#303#;
   GL_DST_ALPHA                                : constant := 16#304#;
   GL_ONE_MINUS_DST_ALPHA                      : constant := 16#305#;
   GL_DST_COLOR                                : constant := 16#306#;
   GL_ONE_MINUS_DST_COLOR                      : constant := 16#307#;
   GL_SRC_ALPHA_SATURATE                       : constant := 16#308#;
   GL_FEEDBACK                                 : constant := 16#1c01#;
   GL_RENDER                                   : constant := 16#1c00#;
   GL_SELECT                                   : constant := 16#1c02#;
   GL_2D                                       : constant := 16#600#;
   GL_3D                                       : constant := 16#601#;
   GL_3D_COLOR                                 : constant := 16#602#;
   GL_3D_COLOR_TEXTURE                         : constant := 16#603#;
   GL_4D_COLOR_TEXTURE                         : constant := 16#604#;
   GL_POINT_TOKEN                              : constant := 16#701#;
   GL_LINE_TOKEN                               : constant := 16#702#;
   GL_LINE_RESET_TOKEN                         : constant := 16#707#;
   GL_POLYGON_TOKEN                            : constant := 16#703#;
   GL_BITMAP_TOKEN                             : constant := 16#704#;
   GL_DRAW_PIXEL_TOKEN                         : constant := 16#705#;
   GL_COPY_PIXEL_TOKEN                         : constant := 16#706#;
   GL_PASS_THROUGH_TOKEN                       : constant := 16#700#;
   GL_FEEDBACK_BUFFER_POINTER                  : constant := 16#df0#;
   GL_FEEDBACK_BUFFER_SIZE                     : constant := 16#df1#;
   GL_FEEDBACK_BUFFER_TYPE                     : constant := 16#df2#;
   GL_SELECTION_BUFFER_POINTER                 : constant := 16#df3#;
   GL_SELECTION_BUFFER_SIZE                    : constant := 16#df4#;
   GL_FOG                                      : constant := 16#b60#;
   GL_FOG_MODE                                 : constant := 16#b65#;
   GL_FOG_DENSITY                              : constant := 16#b62#;
   GL_FOG_COLOR                                : constant := 16#b66#;
   GL_FOG_INDEX                                : constant := 16#b61#;
   GL_FOG_START                                : constant := 16#b63#;
   GL_FOG_END                                  : constant := 16#b64#;
   GL_LINEAR                                   : constant := 16#2601#;
   GL_EXP                                      : constant := 16#800#;
   GL_EXP2                                     : constant := 16#801#;
   GL_LOGIC_OP                                 : constant := 16#bf1#;
   GL_INDEX_LOGIC_OP                           : constant := 16#bf1#;
   GL_COLOR_LOGIC_OP                           : constant := 16#bf2#;
   GL_LOGIC_OP_MODE                            : constant := 16#bf0#;
   GL_CLEAR                                    : constant := 16#1500#;
   GL_SET                                      : constant := 16#150f#;
   GL_COPY                                     : constant := 16#1503#;
   GL_COPY_INVERTED                            : constant := 16#150c#;
   GL_NOOP                                     : constant := 16#1505#;
   GL_INVERT                                   : constant := 16#150a#;
   GL_AND                                      : constant := 16#1501#;
   GL_NAND                                     : constant := 16#150e#;
   GL_OR                                       : constant := 16#1507#;
   GL_NOR                                      : constant := 16#1508#;
   GL_XOR                                      : constant := 16#1506#;
   GL_EQUIV                                    : constant := 16#1509#;
   GL_AND_REVERSE                              : constant := 16#1502#;
   GL_AND_INVERTED                             : constant := 16#1504#;
   GL_OR_REVERSE                               : constant := 16#150b#;
   GL_OR_INVERTED                              : constant := 16#150d#;
   GL_STENCIL_BITS                             : constant := 16#d57#;
   GL_STENCIL_TEST                             : constant := 16#b90#;
   GL_STENCIL_CLEAR_VALUE                      : constant := 16#b91#;
   GL_STENCIL_FUNC                             : constant := 16#b92#;
   GL_STENCIL_VALUE_MASK                       : constant := 16#b93#;
   GL_STENCIL_FAIL                             : constant := 16#b94#;
   GL_STENCIL_PASS_DEPTH_FAIL                  : constant := 16#b95#;
   GL_STENCIL_PASS_DEPTH_PASS                  : constant := 16#b96#;
   GL_STENCIL_REF                              : constant := 16#b97#;
   GL_STENCIL_WRITEMASK                        : constant := 16#b98#;
   GL_STENCIL_INDEX                            : constant := 16#1901#;
   GL_KEEP                                     : constant := 16#1e00#;
   GL_REPLACE                                  : constant := 16#1e01#;
   GL_INCR                                     : constant := 16#1e02#;
   GL_DECR                                     : constant := 16#1e03#;
   GL_NONE                                     : constant := 16#0#;
   GL_LEFT                                     : constant := 16#406#;
   GL_RIGHT                                    : constant := 16#407#;
   GL_FRONT_LEFT                               : constant := 16#400#;
   GL_FRONT_RIGHT                              : constant := 16#401#;
   GL_BACK_LEFT                                : constant := 16#402#;
   GL_BACK_RIGHT                               : constant := 16#403#;
   GL_AUX0                                     : constant := 16#409#;
   GL_AUX1                                     : constant := 16#40a#;
   GL_AUX2                                     : constant := 16#40b#;
   GL_AUX3                                     : constant := 16#40c#;
   GL_COLOR_INDEX                              : constant := 16#1900#;
   GL_RED                                      : constant := 16#1903#;
   GL_GREEN                                    : constant := 16#1904#;
   GL_BLUE                                     : constant := 16#1905#;
   GL_ALPHA                                    : constant := 16#1906#;
   GL_LUMINANCE                                : constant := 16#1909#;
   GL_LUMINANCE_ALPHA                          : constant := 16#190a#;
   GL_ALPHA_BITS                               : constant := 16#d55#;
   GL_RED_BITS                                 : constant := 16#d52#;
   GL_GREEN_BITS                               : constant := 16#d53#;
   GL_BLUE_BITS                                : constant := 16#d54#;
   GL_INDEX_BITS                               : constant := 16#d51#;
   GL_SUBPIXEL_BITS                            : constant := 16#d50#;
   GL_AUX_BUFFERS                              : constant := 16#c00#;
   GL_READ_BUFFER                              : constant := 16#c02#;
   GL_DRAW_BUFFER                              : constant := 16#c01#;
   GL_DOUBLEBUFFER                             : constant := 16#c32#;
   GL_STEREO                                   : constant := 16#c33#;
   GL_BITMAP                                   : constant := 16#1a00#;
   GL_COLOR                                    : constant := 16#1800#;
   GL_DEPTH                                    : constant := 16#1801#;
   GL_STENCIL                                  : constant := 16#1802#;
   GL_DITHER                                   : constant := 16#bd0#;
   GL_RGB                                      : constant := 16#1907#;
   GL_RGBA                                     : constant := 16#1908#;
   GL_MAX_LIST_NESTING                         : constant := 16#b31#;
   GL_MAX_EVAL_ORDER                           : constant := 16#d30#;
   GL_MAX_LIGHTS                               : constant := 16#d31#;
   GL_MAX_CLIP_PLANES                          : constant := 16#d32#;
   GL_MAX_TEXTURE_SIZE                         : constant := 16#d33#;
   GL_MAX_PIXEL_MAP_TABLE                      : constant := 16#d34#;
   GL_MAX_ATTRIB_STACK_DEPTH                   : constant := 16#d35#;
   GL_MAX_MODELVIEW_STACK_DEPTH                : constant := 16#d36#;
   GL_MAX_NAME_STACK_DEPTH                     : constant := 16#d37#;
   GL_MAX_PROJECTION_STACK_DEPTH               : constant := 16#d38#;
   GL_MAX_TEXTURE_STACK_DEPTH                  : constant := 16#d39#;
   GL_MAX_VIEWPORT_DIMS                        : constant := 16#d3a#;
   GL_MAX_CLIENT_ATTRIB_STACK_DEPTH            : constant := 16#d3b#;
   GL_ATTRIB_STACK_DEPTH                       : constant := 16#bb0#;
   GL_CLIENT_ATTRIB_STACK_DEPTH                : constant := 16#bb1#;
   GL_COLOR_CLEAR_VALUE                        : constant := 16#c22#;
   GL_COLOR_WRITEMASK                          : constant := 16#c23#;
   GL_CURRENT_INDEX                            : constant := 16#b01#;
   GL_CURRENT_COLOR                            : constant := 16#b00#;
   GL_CURRENT_NORMAL                           : constant := 16#b02#;
   GL_CURRENT_RASTER_COLOR                     : constant := 16#b04#;
   GL_CURRENT_RASTER_DISTANCE                  : constant := 16#b09#;
   GL_CURRENT_RASTER_INDEX                     : constant := 16#b05#;
   GL_CURRENT_RASTER_POSITION                  : constant := 16#b07#;
   GL_CURRENT_RASTER_TEXTURE_COORDS            : constant := 16#b06#;
   GL_CURRENT_RASTER_POSITION_VALID            : constant := 16#b08#;
   GL_CURRENT_TEXTURE_COORDS                   : constant := 16#b03#;
   GL_INDEX_CLEAR_VALUE                        : constant := 16#c20#;
   GL_INDEX_MODE                               : constant := 16#c30#;
   GL_INDEX_WRITEMASK                          : constant := 16#c21#;
   GL_MODELVIEW_MATRIX                         : constant := 16#ba6#;
   GL_MODELVIEW_STACK_DEPTH                    : constant := 16#ba3#;
   GL_NAME_STACK_DEPTH                         : constant := 16#d70#;
   GL_PROJECTION_MATRIX                        : constant := 16#ba7#;
   GL_PROJECTION_STACK_DEPTH                   : constant := 16#ba4#;
   GL_RENDER_MODE                              : constant := 16#c40#;
   GL_RGBA_MODE                                : constant := 16#c31#;
   GL_TEXTURE_MATRIX                           : constant := 16#ba8#;
   GL_TEXTURE_STACK_DEPTH                      : constant := 16#ba5#;
   GL_VIEWPORT                                 : constant := 16#ba2#;
   GL_AUTO_NORMAL                              : constant := 16#d80#;
   GL_MAP1_COLOR_4                             : constant := 16#d90#;
   GL_MAP1_INDEX                               : constant := 16#d91#;
   GL_MAP1_NORMAL                              : constant := 16#d92#;
   GL_MAP1_TEXTURE_COORD_1                     : constant := 16#d93#;
   GL_MAP1_TEXTURE_COORD_2                     : constant := 16#d94#;
   GL_MAP1_TEXTURE_COORD_3                     : constant := 16#d95#;
   GL_MAP1_TEXTURE_COORD_4                     : constant := 16#d96#;
   GL_MAP1_VERTEX_3                            : constant := 16#d97#;
   GL_MAP1_VERTEX_4                            : constant := 16#d98#;
   GL_MAP2_COLOR_4                             : constant := 16#db0#;
   GL_MAP2_INDEX                               : constant := 16#db1#;
   GL_MAP2_NORMAL                              : constant := 16#db2#;
   GL_MAP2_TEXTURE_COORD_1                     : constant := 16#db3#;
   GL_MAP2_TEXTURE_COORD_2                     : constant := 16#db4#;
   GL_MAP2_TEXTURE_COORD_3                     : constant := 16#db5#;
   GL_MAP2_TEXTURE_COORD_4                     : constant := 16#db6#;
   GL_MAP2_VERTEX_3                            : constant := 16#db7#;
   GL_MAP2_VERTEX_4                            : constant := 16#db8#;
   GL_MAP1_GRID_DOMAIN                         : constant := 16#dd0#;
   GL_MAP1_GRID_SEGMENTS                       : constant := 16#dd1#;
   GL_MAP2_GRID_DOMAIN                         : constant := 16#dd2#;
   GL_MAP2_GRID_SEGMENTS                       : constant := 16#dd3#;
   GL_COEFF                                    : constant := 16#a00#;
   GL_ORDER                                    : constant := 16#a01#;
   GL_DOMAIN                                   : constant := 16#a02#;
   GL_PERSPECTIVE_CORRECTION_HINT              : constant := 16#c50#;
   GL_POINT_SMOOTH_HINT                        : constant := 16#c51#;
   GL_LINE_SMOOTH_HINT                         : constant := 16#c52#;
   GL_POLYGON_SMOOTH_HINT                      : constant := 16#c53#;
   GL_FOG_HINT                                 : constant := 16#c54#;
   GL_DONT_CARE                                : constant := 16#1100#;
   GL_FASTEST                                  : constant := 16#1101#;
   GL_NICEST                                   : constant := 16#1102#;
   GL_SCISSOR_BOX                              : constant := 16#c10#;
   GL_SCISSOR_TEST                             : constant := 16#c11#;
   GL_MAP_COLOR                                : constant := 16#d10#;
   GL_MAP_STENCIL                              : constant := 16#d11#;
   GL_INDEX_SHIFT                              : constant := 16#d12#;
   GL_INDEX_OFFSET                             : constant := 16#d13#;
   GL_RED_SCALE                                : constant := 16#d14#;
   GL_RED_BIAS                                 : constant := 16#d15#;
   GL_GREEN_SCALE                              : constant := 16#d18#;
   GL_GREEN_BIAS                               : constant := 16#d19#;
   GL_BLUE_SCALE                               : constant := 16#d1a#;
   GL_BLUE_BIAS                                : constant := 16#d1b#;
   GL_ALPHA_SCALE                              : constant := 16#d1c#;
   GL_ALPHA_BIAS                               : constant := 16#d1d#;
   GL_DEPTH_SCALE                              : constant := 16#d1e#;
   GL_DEPTH_BIAS                               : constant := 16#d1f#;
   GL_PIXEL_MAP_S_TO_S_SIZE                    : constant := 16#cb1#;
   GL_PIXEL_MAP_I_TO_I_SIZE                    : constant := 16#cb0#;
   GL_PIXEL_MAP_I_TO_R_SIZE                    : constant := 16#cb2#;
   GL_PIXEL_MAP_I_TO_G_SIZE                    : constant := 16#cb3#;
   GL_PIXEL_MAP_I_TO_B_SIZE                    : constant := 16#cb4#;
   GL_PIXEL_MAP_I_TO_A_SIZE                    : constant := 16#cb5#;
   GL_PIXEL_MAP_R_TO_R_SIZE                    : constant := 16#cb6#;
   GL_PIXEL_MAP_G_TO_G_SIZE                    : constant := 16#cb7#;
   GL_PIXEL_MAP_B_TO_B_SIZE                    : constant := 16#cb8#;
   GL_PIXEL_MAP_A_TO_A_SIZE                    : constant := 16#cb9#;
   GL_PIXEL_MAP_S_TO_S                         : constant := 16#c71#;
   GL_PIXEL_MAP_I_TO_I                         : constant := 16#c70#;
   GL_PIXEL_MAP_I_TO_R                         : constant := 16#c72#;
   GL_PIXEL_MAP_I_TO_G                         : constant := 16#c73#;
   GL_PIXEL_MAP_I_TO_B                         : constant := 16#c74#;
   GL_PIXEL_MAP_I_TO_A                         : constant := 16#c75#;
   GL_PIXEL_MAP_R_TO_R                         : constant := 16#c76#;
   GL_PIXEL_MAP_G_TO_G                         : constant := 16#c77#;
   GL_PIXEL_MAP_B_TO_B                         : constant := 16#c78#;
   GL_PIXEL_MAP_A_TO_A                         : constant := 16#c79#;
   GL_PACK_ALIGNMENT                           : constant := 16#d05#;
   GL_PACK_LSB_FIRST                           : constant := 16#d01#;
   GL_PACK_ROW_LENGTH                          : constant := 16#d02#;
   GL_PACK_SKIP_PIXELS                         : constant := 16#d04#;
   GL_PACK_SKIP_ROWS                           : constant := 16#d03#;
   GL_PACK_SWAP_BYTES                          : constant := 16#d00#;
   GL_UNPACK_ALIGNMENT                         : constant := 16#cf5#;
   GL_UNPACK_LSB_FIRST                         : constant := 16#cf1#;
   GL_UNPACK_ROW_LENGTH                        : constant := 16#cf2#;
   GL_UNPACK_SKIP_PIXELS                       : constant := 16#cf4#;
   GL_UNPACK_SKIP_ROWS                         : constant := 16#cf3#;
   GL_UNPACK_SWAP_BYTES                        : constant := 16#cf0#;
   GL_ZOOM_X                                   : constant := 16#d16#;
   GL_ZOOM_Y                                   : constant := 16#d17#;
   GL_TEXTURE_ENV                              : constant := 16#2300#;
   GL_TEXTURE_ENV_MODE                         : constant := 16#2200#;
   GL_TEXTURE_1D                               : constant := 16#de0#;
   GL_TEXTURE_2D                               : constant := 16#de1#;
   GL_TEXTURE_WRAP_S                           : constant := 16#2802#;
   GL_TEXTURE_WRAP_T                           : constant := 16#2803#;
   GL_TEXTURE_MAG_FILTER                       : constant := 16#2800#;
   GL_TEXTURE_MIN_FILTER                       : constant := 16#2801#;
   GL_TEXTURE_ENV_COLOR                        : constant := 16#2201#;
   GL_TEXTURE_GEN_S                            : constant := 16#c60#;
   GL_TEXTURE_GEN_T                            : constant := 16#c61#;
   GL_TEXTURE_GEN_MODE                         : constant := 16#2500#;
   GL_TEXTURE_BORDER_COLOR                     : constant := 16#1004#;
   GL_TEXTURE_WIDTH                            : constant := 16#1000#;
   GL_TEXTURE_HEIGHT                           : constant := 16#1001#;
   GL_TEXTURE_BORDER                           : constant := 16#1005#;
   GL_TEXTURE_COMPONENTS                       : constant := 16#1003#;
   GL_TEXTURE_RED_SIZE                         : constant := 16#805c#;
   GL_TEXTURE_GREEN_SIZE                       : constant := 16#805d#;
   GL_TEXTURE_BLUE_SIZE                        : constant := 16#805e#;
   GL_TEXTURE_ALPHA_SIZE                       : constant := 16#805f#;
   GL_TEXTURE_LUMINANCE_SIZE                   : constant := 16#8060#;
   GL_TEXTURE_INTENSITY_SIZE                   : constant := 16#8061#;
   GL_NEAREST_MIPMAP_NEAREST                   : constant := 16#2700#;
   GL_NEAREST_MIPMAP_LINEAR                    : constant := 16#2702#;
   GL_LINEAR_MIPMAP_NEAREST                    : constant := 16#2701#;
   GL_LINEAR_MIPMAP_LINEAR                     : constant := 16#2703#;
   GL_OBJECT_LINEAR                            : constant := 16#2401#;
   GL_OBJECT_PLANE                             : constant := 16#2501#;
   GL_EYE_LINEAR                               : constant := 16#2400#;
   GL_EYE_PLANE                                : constant := 16#2502#;
   GL_SPHERE_MAP                               : constant := 16#2402#;
   GL_DECAL                                    : constant := 16#2101#;
   GL_MODULATE                                 : constant := 16#2100#;
   GL_NEAREST                                  : constant := 16#2600#;
   GL_REPEAT                                   : constant := 16#2901#;

   GL_MIRRORED_REPEAT                          : constant := 16#8370#; --25nov14 fastrgv

   GL_CLAMP                                    : constant := 16#2900#;
   GL_S                                        : constant := 16#2000#;
   GL_T                                        : constant := 16#2001#;
   GL_R                                        : constant := 16#2002#;
   GL_Q                                        : constant := 16#2003#;
   GL_TEXTURE_GEN_R                            : constant := 16#c62#;
   GL_TEXTURE_GEN_Q                            : constant := 16#c63#;
   GL_VENDOR                                   : constant := 16#1f00#;
   GL_RENDERER                                 : constant := 16#1f01#;
   GL_VERSION                                  : constant := 16#1f02#;
   GL_EXTENSIONS                               : constant := 16#1f03#;
   GL_NO_ERROR                                 : constant := 16#0#;
   GL_INVALID_ENUM                             : constant := 16#500#;
   GL_INVALID_VALUE                            : constant := 16#501#;
   GL_INVALID_OPERATION                        : constant := 16#502#;
   GL_STACK_OVERFLOW                           : constant := 16#503#;
   GL_STACK_UNDERFLOW                          : constant := 16#504#;
   GL_OUT_OF_MEMORY                            : constant := 16#505#;
   GL_CURRENT_BIT                              : constant := 16#1#;
   GL_POINT_BIT                                : constant := 16#2#;
   GL_LINE_BIT                                 : constant := 16#4#;
   GL_POLYGON_BIT                              : constant := 16#8#;
   GL_POLYGON_STIPPLE_BIT                      : constant := 16#10#;
   GL_PIXEL_MODE_BIT                           : constant := 16#20#;
   GL_LIGHTING_BIT                             : constant := 16#40#;
   GL_FOG_BIT                                  : constant := 16#80#;
   GL_DEPTH_BUFFER_BIT                         : constant := 16#100#;
   GL_ACCUM_BUFFER_BIT                         : constant := 16#200#;
   GL_STENCIL_BUFFER_BIT                       : constant := 16#400#;
   GL_VIEWPORT_BIT                             : constant := 16#800#;
   GL_TRANSFORM_BIT                            : constant := 16#1000#;
   GL_ENABLE_BIT                               : constant := 16#2000#;
   GL_COLOR_BUFFER_BIT                         : constant := 16#4000#;
   GL_HINT_BIT                                 : constant := 16#8000#;
   GL_EVAL_BIT                                 : constant := 16#10000#;
   GL_LIST_BIT                                 : constant := 16#20000#;
   GL_TEXTURE_BIT                              : constant := 16#40000#;
   GL_SCISSOR_BIT                              : constant := 16#80000#;
   GL_ALL_ATTRIB_BITS                          : constant := 16#fffff#;
   GL_PROXY_TEXTURE_1D                         : constant := 16#8063#;
   GL_PROXY_TEXTURE_2D                         : constant := 16#8064#;
   GL_TEXTURE_PRIORITY                         : constant := 16#8066#;
   GL_TEXTURE_RESIDENT                         : constant := 16#8067#;
   GL_TEXTURE_BINDING_1D                       : constant := 16#8068#;
   GL_TEXTURE_BINDING_2D                       : constant := 16#8069#;
   GL_TEXTURE_INTERNAL_FORMAT                  : constant := 16#1003#;
   GL_ALPHA4                                   : constant := 16#803b#;
   GL_ALPHA8                                   : constant := 16#803c#;
   GL_ALPHA12                                  : constant := 16#803d#;
   GL_ALPHA16                                  : constant := 16#803e#;
   GL_LUMINANCE4                               : constant := 16#803f#;
   GL_LUMINANCE8                               : constant := 16#8040#;
   GL_LUMINANCE12                              : constant := 16#8041#;
   GL_LUMINANCE16                              : constant := 16#8042#;
   GL_LUMINANCE4_ALPHA4                        : constant := 16#8043#;
   GL_LUMINANCE6_ALPHA2                        : constant := 16#8044#;
   GL_LUMINANCE8_ALPHA8                        : constant := 16#8045#;
   GL_LUMINANCE12_ALPHA4                       : constant := 16#8046#;
   GL_LUMINANCE12_ALPHA12                      : constant := 16#8047#;
   GL_LUMINANCE16_ALPHA16                      : constant := 16#8048#;
   GL_INTENSITY                                : constant := 16#8049#;
   GL_INTENSITY4                               : constant := 16#804a#;
   GL_INTENSITY8                               : constant := 16#804b#;
   GL_INTENSITY12                              : constant := 16#804c#;
   GL_INTENSITY16                              : constant := 16#804d#;
   GL_R3_G3_B2                                 : constant := 16#2a10#;
   GL_RGB4                                     : constant := 16#804f#;
   GL_RGB5                                     : constant := 16#8050#;
   GL_RGB8                                     : constant := 16#8051#;
   GL_RGB10                                    : constant := 16#8052#;
   GL_RGB12                                    : constant := 16#8053#;
   GL_RGB16                                    : constant := 16#8054#;
   GL_RGBA2                                    : constant := 16#8055#;
   GL_RGBA4                                    : constant := 16#8056#;
   GL_RGB5_A1                                  : constant := 16#8057#;
   GL_RGBA8                                    : constant := 16#8058#;
   GL_RGB10_A2                                 : constant := 16#8059#;
   GL_RGBA12                                   : constant := 16#805a#;
   GL_RGBA16                                   : constant := 16#805b#;
   GL_CLIENT_PIXEL_STORE_BIT                   : constant := 16#1#;
   GL_CLIENT_VERTEX_ARRAY_BIT                  : constant := 16#2#;
   GL_ALL_CLIENT_ATTRIB_BITS                   : constant := 16#ffffffff#;
   GL_CLIENT_ALL_ATTRIB_BITS                   : constant := 16#ffffffff#;
   GL_RESCALE_NORMAL                           : constant := 16#803a#;
   GL_CLAMP_TO_EDGE                            : constant := 16#812f#;
   GL_MAX_ELEMENTS_VERTICES                    : constant := 16#80e8#;
   GL_MAX_ELEMENTS_INDICES                     : constant := 16#80e9#;
   GL_BGR                                      : constant := 16#80e0#;
   GL_BGRA                                     : constant := 16#80e1#;
   GL_UNSIGNED_BYTE_3_3_2                      : constant := 16#8032#;
   GL_UNSIGNED_BYTE_2_3_3_REV                  : constant := 16#8362#;
   GL_UNSIGNED_SHORT_5_6_5                     : constant := 16#8363#;
   GL_UNSIGNED_SHORT_5_6_5_REV                 : constant := 16#8364#;
   GL_UNSIGNED_SHORT_4_4_4_4                   : constant := 16#8033#;
   GL_UNSIGNED_SHORT_4_4_4_4_REV               : constant := 16#8365#;
   GL_UNSIGNED_SHORT_5_5_5_1                   : constant := 16#8034#;
   GL_UNSIGNED_SHORT_1_5_5_5_REV               : constant := 16#8366#;
   GL_UNSIGNED_INT_8_8_8_8                     : constant := 16#8035#;
   GL_UNSIGNED_INT_8_8_8_8_REV                 : constant := 16#8367#;
   GL_UNSIGNED_INT_10_10_10_2                  : constant := 16#8036#;
   GL_UNSIGNED_INT_2_10_10_10_REV              : constant := 16#8368#;
   GL_LIGHT_MODEL_COLOR_CONTROL                : constant := 16#81f8#;
   GL_SINGLE_COLOR                             : constant := 16#81f9#;
   GL_SEPARATE_SPECULAR_COLOR                  : constant := 16#81fa#;
   GL_TEXTURE_MIN_LOD                          : constant := 16#813a#;
   GL_TEXTURE_MAX_LOD                          : constant := 16#813b#;
   GL_TEXTURE_BASE_LEVEL                       : constant := 16#813c#;
   GL_TEXTURE_MAX_LEVEL                        : constant := 16#813d#;
   GL_SMOOTH_POINT_SIZE_RANGE                  : constant := 16#b12#;
   GL_SMOOTH_POINT_SIZE_GRANULARITY            : constant := 16#b13#;
   GL_SMOOTH_LINE_WIDTH_RANGE                  : constant := 16#b22#;
   GL_SMOOTH_LINE_WIDTH_GRANULARITY            : constant := 16#b23#;
   GL_ALIASED_POINT_SIZE_RANGE                 : constant := 16#846d#;
   GL_ALIASED_LINE_WIDTH_RANGE                 : constant := 16#846e#;
   GL_PACK_SKIP_IMAGES                         : constant := 16#806b#;
   GL_PACK_IMAGE_HEIGHT                        : constant := 16#806c#;
   GL_UNPACK_SKIP_IMAGES                       : constant := 16#806d#;
   GL_UNPACK_IMAGE_HEIGHT                      : constant := 16#806e#;
   GL_TEXTURE_3D                               : constant := 16#806f#;
   GL_PROXY_TEXTURE_3D                         : constant := 16#8070#;
   GL_TEXTURE_DEPTH                            : constant := 16#8071#;
   GL_TEXTURE_WRAP_R                           : constant := 16#8072#;
   GL_MAX_3D_TEXTURE_SIZE                      : constant := 16#8073#;
   GL_TEXTURE_BINDING_3D                       : constant := 16#806a#;
   GL_CONSTANT_COLOR                           : constant := 16#8001#;
   GL_ONE_MINUS_CONSTANT_COLOR                 : constant := 16#8002#;
   GL_CONSTANT_ALPHA                           : constant := 16#8003#;
   GL_ONE_MINUS_CONSTANT_ALPHA                 : constant := 16#8004#;
   GL_COLOR_TABLE                              : constant := 16#80d0#;
   GL_POST_CONVOLUTION_COLOR_TABLE             : constant := 16#80d1#;
   GL_POST_COLOR_MATRIX_COLOR_TABLE            : constant := 16#80d2#;
   GL_PROXY_COLOR_TABLE                        : constant := 16#80d3#;
   GL_PROXY_POST_CONVOLUTION_COLOR_TABLE       : constant := 16#80d4#;
   GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE      : constant := 16#80d5#;
   GL_COLOR_TABLE_SCALE                        : constant := 16#80d6#;
   GL_COLOR_TABLE_BIAS                         : constant := 16#80d7#;
   GL_COLOR_TABLE_FORMAT                       : constant := 16#80d8#;
   GL_COLOR_TABLE_WIDTH                        : constant := 16#80d9#;
   GL_COLOR_TABLE_RED_SIZE                     : constant := 16#80da#;
   GL_COLOR_TABLE_GREEN_SIZE                   : constant := 16#80db#;
   GL_COLOR_TABLE_BLUE_SIZE                    : constant := 16#80dc#;
   GL_COLOR_TABLE_ALPHA_SIZE                   : constant := 16#80dd#;
   GL_COLOR_TABLE_LUMINANCE_SIZE               : constant := 16#80de#;
   GL_COLOR_TABLE_INTENSITY_SIZE               : constant := 16#80df#;
   GL_CONVOLUTION_1D                           : constant := 16#8010#;
   GL_CONVOLUTION_2D                           : constant := 16#8011#;
   GL_SEPARABLE_2D                             : constant := 16#8012#;
   GL_CONVOLUTION_BORDER_MODE                  : constant := 16#8013#;
   GL_CONVOLUTION_FILTER_SCALE                 : constant := 16#8014#;
   GL_CONVOLUTION_FILTER_BIAS                  : constant := 16#8015#;
   GL_REDUCE                                   : constant := 16#8016#;
   GL_CONVOLUTION_FORMAT                       : constant := 16#8017#;
   GL_CONVOLUTION_WIDTH                        : constant := 16#8018#;
   GL_CONVOLUTION_HEIGHT                       : constant := 16#8019#;
   GL_MAX_CONVOLUTION_WIDTH                    : constant := 16#801a#;
   GL_MAX_CONVOLUTION_HEIGHT                   : constant := 16#801b#;
   GL_POST_CONVOLUTION_RED_SCALE               : constant := 16#801c#;
   GL_POST_CONVOLUTION_GREEN_SCALE             : constant := 16#801d#;
   GL_POST_CONVOLUTION_BLUE_SCALE              : constant := 16#801e#;
   GL_POST_CONVOLUTION_ALPHA_SCALE             : constant := 16#801f#;
   GL_POST_CONVOLUTION_RED_BIAS                : constant := 16#8020#;
   GL_POST_CONVOLUTION_GREEN_BIAS              : constant := 16#8021#;
   GL_POST_CONVOLUTION_BLUE_BIAS               : constant := 16#8022#;
   GL_POST_CONVOLUTION_ALPHA_BIAS              : constant := 16#8023#;
   GL_CONSTANT_BORDER                          : constant := 16#8151#;
   GL_REPLICATE_BORDER                         : constant := 16#8153#;
   GL_CONVOLUTION_BORDER_COLOR                 : constant := 16#8154#;
   GL_COLOR_MATRIX                             : constant := 16#80b1#;
   GL_COLOR_MATRIX_STACK_DEPTH                 : constant := 16#80b2#;
   GL_MAX_COLOR_MATRIX_STACK_DEPTH             : constant := 16#80b3#;
   GL_POST_COLOR_MATRIX_RED_SCALE              : constant := 16#80b4#;
   GL_POST_COLOR_MATRIX_GREEN_SCALE            : constant := 16#80b5#;
   GL_POST_COLOR_MATRIX_BLUE_SCALE             : constant := 16#80b6#;
   GL_POST_COLOR_MATRIX_ALPHA_SCALE            : constant := 16#80b7#;
   GL_POST_COLOR_MATRIX_RED_BIAS               : constant := 16#80b8#;
   GL_POST_COLOR_MATRIX_GREEN_BIAS             : constant := 16#80b9#;
   GL_POST_COLOR_MATRIX_BLUE_BIAS              : constant := 16#80ba#;
   GL_POST_COLOR_MATRIX_ALPHA_BIAS             : constant := 16#80bb#;
   GL_HISTOGRAM                                : constant := 16#8024#;
   GL_PROXY_HISTOGRAM                          : constant := 16#8025#;
   GL_HISTOGRAM_WIDTH                          : constant := 16#8026#;
   GL_HISTOGRAM_FORMAT                         : constant := 16#8027#;
   GL_HISTOGRAM_RED_SIZE                       : constant := 16#8028#;
   GL_HISTOGRAM_GREEN_SIZE                     : constant := 16#8029#;
   GL_HISTOGRAM_BLUE_SIZE                      : constant := 16#802a#;
   GL_HISTOGRAM_ALPHA_SIZE                     : constant := 16#802b#;
   GL_HISTOGRAM_LUMINANCE_SIZE                 : constant := 16#802c#;
   GL_HISTOGRAM_SINK                           : constant := 16#802d#;
   GL_MINMAX                                   : constant := 16#802e#;
   GL_MINMAX_FORMAT                            : constant := 16#802f#;
   GL_MINMAX_SINK                              : constant := 16#8030#;
   GL_TABLE_TOO_LARGE                          : constant := 16#8031#;
   GL_BLEND_EQUATION                           : constant := 16#8009#;
   GL_MIN                                      : constant := 16#8007#;
   GL_MAX                                      : constant := 16#8008#;
   GL_FUNC_ADD                                 : constant := 16#8006#;
   GL_FUNC_SUBTRACT                            : constant := 16#800a#;
   GL_FUNC_REVERSE_SUBTRACT                    : constant := 16#800b#;
   GL_BLEND_COLOR                              : constant := 16#8005#;
   GL_TEXTURE0                                 : constant := 16#84c0#;
   GL_TEXTURE1                                 : constant := 16#84c1#;
   GL_TEXTURE2                                 : constant := 16#84c2#;
   GL_TEXTURE3                                 : constant := 16#84c3#;
   GL_TEXTURE4                                 : constant := 16#84c4#;
   GL_TEXTURE5                                 : constant := 16#84c5#;
   GL_TEXTURE6                                 : constant := 16#84c6#;
   GL_TEXTURE7                                 : constant := 16#84c7#;
   GL_TEXTURE8                                 : constant := 16#84c8#;
   GL_TEXTURE9                                 : constant := 16#84c9#;
   GL_TEXTURE10                                : constant := 16#84ca#;
   GL_TEXTURE11                                : constant := 16#84cb#;
   GL_TEXTURE12                                : constant := 16#84cc#;
   GL_TEXTURE13                                : constant := 16#84cd#;
   GL_TEXTURE14                                : constant := 16#84ce#;
   GL_TEXTURE15                                : constant := 16#84cf#;
   GL_TEXTURE16                                : constant := 16#84d0#;
   GL_TEXTURE17                                : constant := 16#84d1#;
   GL_TEXTURE18                                : constant := 16#84d2#;
   GL_TEXTURE19                                : constant := 16#84d3#;
   GL_TEXTURE20                                : constant := 16#84d4#;
   GL_TEXTURE21                                : constant := 16#84d5#;
   GL_TEXTURE22                                : constant := 16#84d6#;
   GL_TEXTURE23                                : constant := 16#84d7#;
   GL_TEXTURE24                                : constant := 16#84d8#;
   GL_TEXTURE25                                : constant := 16#84d9#;
   GL_TEXTURE26                                : constant := 16#84da#;
   GL_TEXTURE27                                : constant := 16#84db#;
   GL_TEXTURE28                                : constant := 16#84dc#;
   GL_TEXTURE29                                : constant := 16#84dd#;
   GL_TEXTURE30                                : constant := 16#84de#;
   GL_TEXTURE31                                : constant := 16#84df#;
   GL_ACTIVE_TEXTURE                           : constant := 16#84e0#;
   GL_CLIENT_ACTIVE_TEXTURE                    : constant := 16#84e1#;
   GL_MAX_TEXTURE_UNITS                        : constant := 16#84e2#;
   GL_NORMAL_MAP                               : constant := 16#8511#;
   GL_REFLECTION_MAP                           : constant := 16#8512#;
   GL_TEXTURE_CUBE_MAP                         : constant := 16#8513#;
   GL_TEXTURE_BINDING_CUBE_MAP                 : constant := 16#8514#;
   GL_TEXTURE_CUBE_MAP_POSITIVE_X              : constant := 16#8515#;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X              : constant := 16#8516#;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y              : constant := 16#8517#;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y              : constant := 16#8518#;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z              : constant := 16#8519#;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z              : constant := 16#851a#;
   GL_PROXY_TEXTURE_CUBE_MAP                   : constant := 16#851b#;
   GL_MAX_CUBE_MAP_TEXTURE_SIZE                : constant := 16#851c#;
   GL_COMPRESSED_ALPHA                         : constant := 16#84e9#;
   GL_COMPRESSED_LUMINANCE                     : constant := 16#84ea#;
   GL_COMPRESSED_LUMINANCE_ALPHA               : constant := 16#84eb#;
   GL_COMPRESSED_INTENSITY                     : constant := 16#84ec#;
   GL_COMPRESSED_RGB                           : constant := 16#84ed#;
   GL_COMPRESSED_RGBA                          : constant := 16#84ee#;
   GL_TEXTURE_COMPRESSION_HINT                 : constant := 16#84ef#;
   GL_TEXTURE_COMPRESSED_IMAGE_SIZE            : constant := 16#86a0#;
   GL_TEXTURE_COMPRESSED                       : constant := 16#86a1#;
   GL_NUM_COMPRESSED_TEXTURE_FORMATS           : constant := 16#86a2#;
   GL_COMPRESSED_TEXTURE_FORMATS               : constant := 16#86a3#;
   GL_MULTISAMPLE                              : constant := 16#809d#;
   GL_SAMPLE_ALPHA_TO_COVERAGE                 : constant := 16#809e#;
   GL_SAMPLE_ALPHA_TO_ONE                      : constant := 16#809f#;
   GL_SAMPLE_COVERAGE                          : constant := 16#80a0#;
   GL_SAMPLE_BUFFERS                           : constant := 16#80a8#;
   GL_SAMPLES                                  : constant := 16#80a9#;
   GL_SAMPLE_COVERAGE_VALUE                    : constant := 16#80aa#;
   GL_SAMPLE_COVERAGE_INVERT                   : constant := 16#80ab#;
   GL_MULTISAMPLE_BIT                          : constant := 16#20000000#;
   GL_TRANSPOSE_MODELVIEW_MATRIX               : constant := 16#84e3#;
   GL_TRANSPOSE_PROJECTION_MATRIX              : constant := 16#84e4#;
   GL_TRANSPOSE_TEXTURE_MATRIX                 : constant := 16#84e5#;
   GL_TRANSPOSE_COLOR_MATRIX                   : constant := 16#84e6#;
   GL_COMBINE                                  : constant := 16#8570#;
   GL_COMBINE_RGB                              : constant := 16#8571#;
   GL_COMBINE_ALPHA                            : constant := 16#8572#;
   GL_SOURCE0_RGB                              : constant := 16#8580#;
   GL_SOURCE1_RGB                              : constant := 16#8581#;
   GL_SOURCE2_RGB                              : constant := 16#8582#;
   GL_SOURCE0_ALPHA                            : constant := 16#8588#;
   GL_SOURCE1_ALPHA                            : constant := 16#8589#;
   GL_SOURCE2_ALPHA                            : constant := 16#858a#;
   GL_OPERAND0_RGB                             : constant := 16#8590#;
   GL_OPERAND1_RGB                             : constant := 16#8591#;
   GL_OPERAND2_RGB                             : constant := 16#8592#;
   GL_OPERAND0_ALPHA                           : constant := 16#8598#;
   GL_OPERAND1_ALPHA                           : constant := 16#8599#;
   GL_OPERAND2_ALPHA                           : constant := 16#859a#;
   GL_RGB_SCALE                                : constant := 16#8573#;
   GL_ADD_SIGNED                               : constant := 16#8574#;
   GL_INTERPOLATE                              : constant := 16#8575#;
   GL_SUBTRACT                                 : constant := 16#84e7#;
   GL_CONSTANT                                 : constant := 16#8576#;
   GL_PRIMARY_COLOR                            : constant := 16#8577#;
   GL_PREVIOUS                                 : constant := 16#8578#;
   GL_DOT3_RGB                                 : constant := 16#86ae#;
   GL_DOT3_RGBA                                : constant := 16#86af#;
   GL_CLAMP_TO_BORDER                          : constant := 16#812d#;
   GL_ARB_multitexture                         : constant := 1;
   GL_TEXTURE0_ARB                             : constant := 16#84c0#;
   GL_TEXTURE1_ARB                             : constant := 16#84c1#;
   GL_TEXTURE2_ARB                             : constant := 16#84c2#;
   GL_TEXTURE3_ARB                             : constant := 16#84c3#;
   GL_TEXTURE4_ARB                             : constant := 16#84c4#;
   GL_TEXTURE5_ARB                             : constant := 16#84c5#;
   GL_TEXTURE6_ARB                             : constant := 16#84c6#;
   GL_TEXTURE7_ARB                             : constant := 16#84c7#;
   GL_TEXTURE8_ARB                             : constant := 16#84c8#;
   GL_TEXTURE9_ARB                             : constant := 16#84c9#;
   GL_TEXTURE10_ARB                            : constant := 16#84ca#;
   GL_TEXTURE11_ARB                            : constant := 16#84cb#;
   GL_TEXTURE12_ARB                            : constant := 16#84cc#;
   GL_TEXTURE13_ARB                            : constant := 16#84cd#;
   GL_TEXTURE14_ARB                            : constant := 16#84ce#;
   GL_TEXTURE15_ARB                            : constant := 16#84cf#;
   GL_TEXTURE16_ARB                            : constant := 16#84d0#;
   GL_TEXTURE17_ARB                            : constant := 16#84d1#;
   GL_TEXTURE18_ARB                            : constant := 16#84d2#;
   GL_TEXTURE19_ARB                            : constant := 16#84d3#;
   GL_TEXTURE20_ARB                            : constant := 16#84d4#;
   GL_TEXTURE21_ARB                            : constant := 16#84d5#;
   GL_TEXTURE22_ARB                            : constant := 16#84d6#;
   GL_TEXTURE23_ARB                            : constant := 16#84d7#;
   GL_TEXTURE24_ARB                            : constant := 16#84d8#;
   GL_TEXTURE25_ARB                            : constant := 16#84d9#;
   GL_TEXTURE26_ARB                            : constant := 16#84da#;
   GL_TEXTURE27_ARB                            : constant := 16#84db#;
   GL_TEXTURE28_ARB                            : constant := 16#84dc#;
   GL_TEXTURE29_ARB                            : constant := 16#84dd#;
   GL_TEXTURE30_ARB                            : constant := 16#84de#;
   GL_TEXTURE31_ARB                            : constant := 16#84df#;
   GL_ACTIVE_TEXTURE_ARB                       : constant := 16#84e0#;
   GL_CLIENT_ACTIVE_TEXTURE_ARB                : constant := 16#84e1#;
   GL_MAX_TEXTURE_UNITS_ARB                    : constant := 16#84e2#;
   GL_MESA_packed_depth_stencil                : constant := 1;
   GL_DEPTH_STENCIL_MESA                       : constant := 16#8750#;
   GL_UNSIGNED_INT_24_8_MESA                   : constant := 16#8751#;
   GL_UNSIGNED_INT_8_24_REV_MESA               : constant := 16#8752#;
   GL_UNSIGNED_SHORT_15_1_MESA                 : constant := 16#8753#;
   GL_UNSIGNED_SHORT_1_15_REV_MESA             : constant := 16#8754#;
   GL_MESA_program_debug                       : constant := 1;
   GL_FRAGMENT_PROGRAM_POSITION_MESA           : constant := 16#8bb0#;
   GL_FRAGMENT_PROGRAM_CALLBACK_MESA           : constant := 16#8bb1#;
   GL_FRAGMENT_PROGRAM_CALLBACK_FUNC_MESA      : constant := 16#8bb2#;
   GL_FRAGMENT_PROGRAM_CALLBACK_DATA_MESA      : constant := 16#8bb3#;
   GL_VERTEX_PROGRAM_POSITION_MESA             : constant := 16#8bb4#;
   GL_VERTEX_PROGRAM_CALLBACK_MESA             : constant := 16#8bb5#;
   GL_VERTEX_PROGRAM_CALLBACK_FUNC_MESA        : constant := 16#8bb6#;
   GL_VERTEX_PROGRAM_CALLBACK_DATA_MESA        : constant := 16#8bb7#;
   GL_MESA_texture_array                       : constant := 1;
   GL_TEXTURE_1D_ARRAY_EXT                     : constant := 16#8c18#;
   GL_PROXY_TEXTURE_1D_ARRAY_EXT               : constant := 16#8c19#;
   GL_TEXTURE_2D_ARRAY_EXT                     : constant := 16#8c1a#;
   GL_PROXY_TEXTURE_2D_ARRAY_EXT               : constant := 16#8c1b#;
   GL_TEXTURE_BINDING_1D_ARRAY_EXT             : constant := 16#8c1c#;
   GL_TEXTURE_BINDING_2D_ARRAY_EXT             : constant := 16#8c1d#;
   GL_MAX_ARRAY_TEXTURE_LAYERS_EXT             : constant := 16#88ff#;
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT : constant := 16#8cd4#;
   GL_ATI_blend_equation_separate              : constant := 1;
   GL_ALPHA_BLEND_EQUATION_ATI                 : constant := 16#883d#;

end GL;
