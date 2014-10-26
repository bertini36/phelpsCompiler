with phelps_dfa, phelps_io; 
use phelps_dfa, phelps_io;

package body analitzador_lexic is
function YYLex return Token is
subtype short is integer range -32768..32767;
    yy_act : integer;
    yy_c : short;

-- returned upon end-of-file
YY_END_TOK : constant integer := 0;
YY_END_OF_BUFFER : constant := 48;
subtype yy_state_type is integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
yy_accept : constant array(0..147) of short :=
    (   0,
        0,    0,   48,   46,   43,   45,   46,   32,   46,   33,
       34,   30,   28,   37,   29,   36,   31,   42,   27,   38,
       25,   25,   25,   39,   39,   39,   39,   39,   39,   39,
       39,   39,   39,   39,   39,   39,   39,   43,    0,   40,
        0,   35,   44,   25,   42,   26,   39,    0,   39,   39,
       39,   12,   39,    2,   39,    4,   39,   39,   39,   39,
       39,   39,   39,   22,   39,   39,   18,   39,   39,   41,
       44,   39,   39,   39,   39,   39,   39,   16,   39,   39,
       21,   39,   39,   39,   39,    9,   39,   39,   39,   39,
       39,   39,   39,   39,   39,   39,   39,   39,   39,   39,

       39,   39,   39,   39,   20,   39,   39,   11,    1,   39,
       39,   39,   39,   39,   39,   39,   39,   39,   24,   39,
       39,    7,   39,   39,   39,   39,   39,    3,   23,   39,
       15,   39,   39,   39,   39,    5,   17,   39,   19,   39,
        6,   10,    8,   39,   13,   14,    0
    ) ;

yy_ec : constant array(ASCII.NUL..ASCII.DEL) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    2,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    4,    5,    6,    5,    5,    7,    5,    8,    9,
       10,   11,   12,   13,   14,   15,   16,   17,   17,   17,
       17,   17,   17,   17,   17,   17,   17,   18,   19,   20,
       21,   22,    5,    5,   23,   23,   23,   23,   23,   23,
       23,   23,   23,   23,   23,   23,   23,   23,   23,   23,
       23,   23,   23,   23,   23,   23,   23,   23,   23,   23,
        5,    5,    5,    5,   24,    5,   25,   26,   27,   28,

       29,   30,   31,   23,   32,   23,   23,   33,   34,   35,
       36,   37,   23,   38,   39,   40,   41,   42,   23,   23,
       23,   23,    5,    5,    5,    5,    1
    ) ;

yy_meta : constant array(0..42) of short :=
    (   0,
        1,    1,    2,    3,    3,    3,    3,    3,    3,    3,
        3,    3,    3,    3,    3,    3,    4,    3,    3,    3,
        3,    3,    4,    5,    4,    4,    4,    4,    4,    4,
        4,    4,    4,    4,    4,    4,    4,    4,    4,    4,
        4,    4
    ) ;

yy_base : constant array(0..152) of short :=
    (   0,
        0,    0,  278,  279,   41,  279,  271,  279,    0,  279,
      279,  279,  279,  279,  279,  261,   28,  258,  253,  279,
      252,  279,  251,  247,   24,   22,   23,   29,   31,   32,
       33,   30,   37,   38,   47,   50,   51,   76,  264,  263,
      260,  279,    0,  279,  250,  279,  242,    0,   60,   63,
       64,  241,   55,  240,   65,  239,   67,   69,   68,   70,
       80,   82,   77,   84,   85,   87,   86,   89,   91,  279,
        0,  238,   90,   92,  102,   95,   98,  237,  106,  108,
      236,   96,  109,  111,  114,  235,  113,  118,  116,  120,
      121,  123,  122,  132,  125,  142,  131,  137,  140,  144,

      149,  150,  151,  152,  234,  154,  153,  233,  232,  155,
      156,  165,  159,  160,  170,  172,  166,  174,  231,  176,
      178,  230,  181,  183,  184,  186,  187,  229,  228,  190,
      224,  188,  195,  197,  198,  213,  212,  193,  211,  199,
      210,  208,  200,  201,  133,  127,  279,  236,  239,  241,
      246,   77
    ) ;

yy_def : constant array(0..152) of short :=
    (   0,
      147,    1,  147,  147,  147,  147,  148,  147,  149,  147,
      147,  147,  147,  147,  147,  147,  147,  147,  147,  147,
      147,  147,  147,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  147,  148,  147,
      147,  147,  151,  147,  147,  147,  150,  152,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  147,
      151,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,

      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,    0,  147,  147,  147,
      147,  147
    ) ;

yy_nxt : constant array(0..321) of short :=
    (   0,
        4,    5,    6,    5,    4,    7,    8,    9,   10,   11,
       12,   13,   14,   15,   16,   17,   18,   19,   20,   21,
       22,   23,   24,    4,   25,   24,   26,   27,   28,   29,
       24,   30,   31,   32,   33,   34,   24,   35,   36,   37,
       24,   24,   38,   43,   38,   48,   48,   48,   44,   49,
       50,   52,   48,   48,   48,   48,   48,   51,   63,   55,
       48,   48,   56,   53,   57,   62,   60,   54,   58,   59,
       48,   61,   64,   48,   48,   66,   65,   38,   48,   38,
       72,   67,   69,   48,   73,   68,   48,   48,   48,   74,
       48,   48,   48,   48,   77,   79,   75,   80,   76,   81,

       48,   82,   78,   48,   83,   48,   84,   48,   48,   48,
       48,   85,   48,   48,   48,   48,   87,   88,   48,   48,
       89,   48,   99,   93,   86,   48,   90,   91,   92,   48,
       94,   48,   48,   95,   48,   96,   48,   48,   98,   48,
       97,   48,  100,   48,   48,   48,   48,  103,   48,  104,
       48,  105,  101,  102,   48,   48,   48,  109,  110,  106,
       48,  107,  108,   48,  111,   48,  112,   48,  114,  116,
      113,  115,   48,   48,   48,   48,   48,   48,   48,   48,
      124,  123,   48,   48,  117,  121,  119,  118,   48,   48,
      120,  122,  125,   48,  128,   48,  126,   48,  127,   48,

      129,   48,  131,  130,   48,  133,   48,   48,  136,   48,
       48,   48,  134,   48,  137,  132,   48,  135,   48,  141,
       48,   48,   48,   48,   48,  140,  138,  145,  139,  146,
      144,   48,  142,   48,   48,   48,   48,  143,   39,   39,
       39,   41,   41,   41,   47,   47,   71,   48,   71,   71,
       71,   48,   48,   48,   48,   48,   48,   48,   48,   48,
       48,   48,   48,   48,   48,   48,   45,   70,   39,   40,
       48,   44,   44,   46,   45,   42,   40,  147,    3,  147,
      147,  147,  147,  147,  147,  147,  147,  147,  147,  147,
      147,  147,  147,  147,  147,  147,  147,  147,  147,  147,

      147,  147,  147,  147,  147,  147,  147,  147,  147,  147,
      147,  147,  147,  147,  147,  147,  147,  147,  147,  147,
      147
    ) ;

yy_chk : constant array(0..321) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    5,   17,    5,   26,   27,   25,   17,   25,
       25,   27,   28,   32,   29,   30,   31,   26,   32,   29,
       33,   34,   29,   28,   29,   31,   30,   28,   29,   29,
       35,   30,   33,   36,   37,   35,   34,   38,   53,   38,
      152,   36,   37,   49,   49,   36,   50,   51,   55,   50,
       57,   59,   58,   60,   53,   57,   51,   58,   51,   59,

       63,   60,   55,   61,   61,   62,   62,   64,   65,   67,
       66,   63,   68,   73,   69,   74,   65,   66,   76,   82,
       67,   77,   82,   74,   64,   75,   68,   69,   73,   79,
       75,   80,   83,   76,   84,   77,   87,   85,   80,   89,
       79,   88,   83,   90,   91,   93,   92,   87,   95,   88,
      146,   89,   84,   85,   97,   94,  145,   93,   94,   90,
       98,   91,   92,   99,   95,   96,   96,  100,   98,  100,
       97,   99,  101,  102,  103,  104,  107,  106,  110,  111,
      111,  110,  113,  114,  101,  106,  103,  102,  112,  117,
      104,  107,  112,  115,  115,  116,  113,  118,  114,  120,

      116,  121,  118,  117,  123,  121,  124,  125,  125,  126,
      127,  132,  123,  130,  126,  120,  138,  124,  133,  133,
      134,  135,  140,  143,  144,  132,  127,  140,  130,  144,
      138,  142,  134,  141,  139,  137,  136,  135,  148,  148,
      148,  149,  149,  149,  150,  150,  151,  131,  151,  151,
      151,  129,  128,  122,  119,  109,  108,  105,   86,   81,
       78,   72,   56,   54,   52,   47,   45,   41,   40,   39,
       24,   23,   21,   19,   18,   16,    7,    3,  147,  147,
      147,  147,  147,  147,  147,  147,  147,  147,  147,  147,
      147,  147,  147,  147,  147,  147,  147,  147,  147,  147,

      147,  147,  147,  147,  147,  147,  147,  147,  147,  147,
      147,  147,  147,  147,  147,  147,  147,  147,  147,  147,
      147
    ) ;


-- copy whatever the last rule matched to the standard output

procedure ECHO is
begin
   if (text_io.is_open(user_output_file)) then
     text_io.put( user_output_file, yytext );
   else
     text_io.put( yytext );
   end if;
end ECHO;

-- enter a start condition.
-- Using procedure requires a () after the ENTER, but makes everything
-- much neater.

procedure ENTER( state : integer ) is
begin
     yy_start := 1 + 2 * state;
end ENTER;

-- action number for EOF rule of a given start state
function YY_STATE_EOF(state : integer) return integer is
begin
     return YY_END_OF_BUFFER + state + 1;
end YY_STATE_EOF;

-- return all but the first 'n' matched characters back to the input stream
procedure yyless(n : integer) is
begin
        yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
        yy_cp := yy_bp + n;
        yy_c_buf_p := yy_cp;
        YY_DO_BEFORE_ACTION; -- set up yytext again
end yyless;

-- redefine this if you have something you want each time.
procedure YY_USER_ACTION is
begin
        null;
end;

-- yy_get_previous_state - get the state just before the EOB char was reached

function yy_get_previous_state return yy_state_type is
    yy_current_state : yy_state_type;
    yy_c : short;
begin
    yy_current_state := yy_start;

    for yy_cp in yytext_ptr..yy_c_buf_p - 1 loop
	yy_c := yy_ec(yy_ch_buf(yy_cp));
	if ( yy_accept(yy_current_state) /= 0 ) then
	    yy_last_accepting_state := yy_current_state;
	    yy_last_accepting_cpos := yy_cp;
	end if;
	while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
	    yy_current_state := yy_def(yy_current_state);
	    if ( yy_current_state >= 148 ) then
		yy_c := yy_meta(yy_c);
	    end if;
	end loop;
	yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
    end loop;

    return yy_current_state;
end yy_get_previous_state;

procedure yyrestart( input_file : file_type ) is
begin
   open_input(text_io.name(input_file));
end yyrestart;

begin -- of YYLex
<<new_file>>
        -- this is where we enter upon encountering an end-of-file and
        -- yywrap() indicating that we should continue processing

    if ( yy_init ) then
        if ( yy_start = 0 ) then
            yy_start := 1;      -- first start state
        end if;

        -- we put in the '\n' and start reading from [1] so that an
        -- initial match-at-newline will be true.

        yy_ch_buf(0) := ASCII.LF;
        yy_n_chars := 1;

        -- we always need two end-of-buffer characters. The first causes
        -- a transition to the end-of-buffer state. The second causes
        -- a jam in that state.

        yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
        yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

        yy_eof_has_been_seen := false;

        yytext_ptr := 1;
        yy_c_buf_p := yytext_ptr;
        yy_hold_char := yy_ch_buf(yy_c_buf_p);
        yy_init := false;
-- UMASS CODES :
--   Initialization
        tok_begin_line := 1;
        tok_end_line := 1;
        tok_begin_col := 0;
        tok_end_col := 0;
        token_at_end_of_line := false;
        line_number_of_saved_tok_line1 := 0;
        line_number_of_saved_tok_line2 := 0;
-- END OF UMASS CODES.
    end if; -- yy_init

    loop                -- loops until end-of-file is reached

-- UMASS CODES :
--    if last matched token is end_of_line, we must
--    update the token_end_line and reset tok_end_col.
    if Token_At_End_Of_Line then
      Tok_End_Line := Tok_End_Line + 1;
      Tok_End_Col := 0;
      Token_At_End_Of_Line := False;
    end if;
-- END OF UMASS CODES.

        yy_cp := yy_c_buf_p;

        -- support of yytext
        yy_ch_buf(yy_cp) := yy_hold_char;

        -- yy_bp points to the position in yy_ch_buf of the start of the
        -- current run.
	yy_bp := yy_cp;
	yy_current_state := yy_start;
	loop
		yy_c := yy_ec(yy_ch_buf(yy_cp));
		if ( yy_accept(yy_current_state) /= 0 ) then
		    yy_last_accepting_state := yy_current_state;
		    yy_last_accepting_cpos := yy_cp;
		end if;
		while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
		    yy_current_state := yy_def(yy_current_state);
		    if ( yy_current_state >= 148 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 147 ) then
    exit;
end if;
	end loop;
	yy_cp := yy_last_accepting_cpos;
	yy_current_state := yy_last_accepting_state;

<<next_action>>
	    yy_act := yy_accept(yy_current_state);
            YY_DO_BEFORE_ACTION;
            YY_USER_ACTION;

        if aflex_debug then  -- output acceptance info. for (-d) debug mode
            text_io.put( Standard_Error, "--accepting rule #" );
            text_io.put( Standard_Error, INTEGER'IMAGE(yy_act) );
            text_io.put_line( Standard_Error, "(""" & yytext & """)");
        end if;

-- UMASS CODES :
--   Update tok_begin_line, tok_end_line, tok_begin_col and tok_end_col
--   after matching the token.
        if yy_act /= YY_END_OF_BUFFER and then yy_act /= 0 then
-- Token are matched only when yy_act is not yy_end_of_buffer or 0.
          Tok_Begin_Line := Tok_End_Line;
          Tok_Begin_Col := Tok_End_Col + 1;
          Tok_End_Col := Tok_Begin_Col + yy_cp - yy_bp - 1;
          if yy_ch_buf ( yy_bp ) = ASCII.LF then
            Token_At_End_Of_Line := True;
          end if;
        end if;
-- END OF UMASS CODES.

<<do_action>>   -- this label is used only to access EOF actions
            case yy_act is
		when 0 => -- must backtrack
		-- undo the effects of YY_DO_BEFORE_ACTION
		yy_ch_buf(yy_cp) := yy_hold_char;
		yy_cp := yy_last_accepting_cpos;
		yy_current_state := yy_last_accepting_state;
		goto next_action;



--Paraules clau
when 1 => 
--# line 16 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_accio;

when 2 => 
--# line 17 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_es;

when 3 => 
--# line 18 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_inicia;

when 4 => 
--# line 19 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_fi;

when 5 => 
--# line 20 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_entrada;

when 6 => 
--# line 21 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_sortida;

when 7 => 
--# line 22 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_tipus;

when 8 => 
--# line 23 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_constant;

when 9 => 
--# line 24 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_nou;

when 10 => 
--# line 25 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_coleccio;

when 11 => 
--# line 26 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_abast;

when 12 => 
--# line 27 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_de;

when 13 => 
--# line 28 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_registre;

when 14 => 
--# line 29 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_fregistre;

when 15 => 
--# line 30 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_mentre;

when 16 => 
--# line 31 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_fer;

when 17 => 
--# line 32 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_fmentre;						

when 18 => 
--# line 33 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_si;

when 19 => 
--# line 34 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_llavors;

when 20 => 
--# line 35 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_sino;

when 21 => 
--# line 36 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return pc_fsi;

--Operacions
when 22 => 
--# line 39 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return op_no;

when 23 => 
--# line 40 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return op_i;

when 24 => 
--# line 41 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return op_o;

--Simbols	
when 25 => 
--# line 44 "phelps.l"
rl_oprel(yylval, yytext, yy_line_number, yy_begin_column); return s_oprel;	

when 26 => 
--# line 45 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_assig;	

when 27 => 
--# line 46 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_dospunts;										

when 28 => 
--# line 47 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_mes;

when 29 => 
--# line 48 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_menys;

when 30 => 
--# line 49 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_prod;

when 31 => 
--# line 50 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_div;

when 32 => 
--# line 51 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_mod;

when 33 => 
--# line 52 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_parobert;

when 34 => 
--# line 53 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_partancat;

when 35 => 
--# line 54 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_abast;

when 36 => 
--# line 55 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_punt;

when 37 => 
--# line 56 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_coma;

when 38 => 
--# line 57 "phelps.l"
rl_atom(yylval, yy_line_number, yy_begin_column); return s_punticoma;

--Identificadors
when 39 => 
--# line 60 "phelps.l"
rl_identificador(yylval, yytext, yy_line_number, yy_begin_column); return id;

--Literals
when 40 => 
--# line 63 "phelps.l"
rl_lit_cad(yylval, yytext, yy_line_number, yy_begin_column); return literal;

when 41 => 
--# line 64 "phelps.l"
rl_lit_car(yylval, yytext, yy_line_number, yy_begin_column); return literal;							

when 42 => 
--# line 65 "phelps.l"
rl_lit_ent(yylval, yytext, yy_line_number, yy_begin_column); return literal;

--Separadors i comentaris
when 43 => 
--# line 68 "phelps.l"
null;	

when 44 => 
--# line 69 "phelps.l"
null;

when 45 => 
--# line 70 "phelps.l"
null;

--Qualsevol altra
when 46 => 
--# line 73 "phelps.l"
return Error;

when 47 => 
--# line 75 "phelps.l"
ECHO;
when YY_END_OF_BUFFER + INITIAL + 1 => 
    return End_Of_Input;
                when YY_END_OF_BUFFER =>
                    -- undo the effects of YY_DO_BEFORE_ACTION
                    yy_ch_buf(yy_cp) := yy_hold_char;

                    yytext_ptr := yy_bp;

                    case yy_get_next_buffer is
                        when EOB_ACT_END_OF_FILE =>
                            begin
                            if ( yywrap ) then
                                -- note: because we've taken care in
                                -- yy_get_next_buffer() to have set up yytext,
                                -- we can now set up yy_c_buf_p so that if some
                                -- total hoser (like aflex itself) wants
                                -- to call the scanner after we return the
                                -- End_Of_Input, it'll still work - another
                                -- End_Of_Input will get returned.

                                yy_c_buf_p := yytext_ptr;

                                yy_act := YY_STATE_EOF((yy_start - 1) / 2);

                                goto do_action;
                            else
                                --  start processing a new file
                                yy_init := true;
                                goto new_file;
                            end if;
                            end;
                        when EOB_ACT_RESTART_SCAN =>
                            yy_c_buf_p := yytext_ptr;
                            yy_hold_char := yy_ch_buf(yy_c_buf_p);
                        when EOB_ACT_LAST_MATCH =>
                            yy_c_buf_p := yy_n_chars;
                            yy_current_state := yy_get_previous_state;

                            yy_cp := yy_c_buf_p;
                            yy_bp := yytext_ptr;
                            goto next_action;
                        when others => null;
                        end case; -- case yy_get_next_buffer()
                when others =>
                    text_io.put( "action # " );
                    text_io.put( INTEGER'IMAGE(yy_act) );
                    text_io.new_line;
                    raise AFLEX_INTERNAL_ERROR;
            end case; -- case (yy_act)
        end loop; -- end of loop waiting for end of file
end YYLex;
--# line 75 "phelps.l"
end analitzador_lexic;