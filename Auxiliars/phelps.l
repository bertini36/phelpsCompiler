lletra			[A-Za-z]
digit			[0-9]
identificador 	{lletra}(({lletra}|{digit})|_({lletra}|{digit}))*
car_rep			[\040-\176]
car_rep_c		[\040-\041\043-\176]
cadena			\"({car_rep_c}|\"\")*\"	
caracter		'{car_rep}'
enter			{digit}({digit})*
comentari		"//"[^\n]*
separador		[ \t\r]
salt			[\n]

%%

--Paraules clau
accio													{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_accio;}
es														{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_es;}
inicia													{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_inicia;}
fi														{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_fi;}
entrada													{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_entrada;}
sortida													{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_sortida;}
tipus													{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_tipus;}
constant												{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_constant;}
nou														{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_nou;}
coleccio												{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_coleccio;}
abast													{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_abast;}
de														{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_de;}
registre												{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_registre;}
fregistre												{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_fregistre;}
mentre													{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_mentre;}
fer														{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_fer;}
fmentre													{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_fmentre;}						
si														{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_si;}
llavors													{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_llavors;}
sino													{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_sino;}
fsi														{rl_atom(yylval, yy_line_number, yy_begin_column); return pc_fsi;}

--Operacions
no														{rl_atom(yylval, yy_line_number, yy_begin_column); return op_no;}
itambe													{rl_atom(yylval, yy_line_number, yy_begin_column); return op_i;}
osino													{rl_atom(yylval, yy_line_number, yy_begin_column); return op_o;}

--Simbols	
">"=|"<"=|">"|"<"|"/"=|=								{rl_oprel(yylval, yytext, yy_line_number, yy_begin_column); return s_oprel;}	
:=														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_assig;}	
:														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_dospunts;}										
"+"														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_mes;}
"-"														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_menys;}
"*"														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_prod;}
"/"														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_div;}
"%"														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_mod;}
"("														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_parobert;}
")"														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_partancat;}
".."													{rl_atom(yylval, yy_line_number, yy_begin_column); return s_abast;}
"."														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_punt;}
,														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_coma;}
;														{rl_atom(yylval, yy_line_number, yy_begin_column); return s_punticoma;}

--Identificadors
{identificador}											{rl_identificador(yylval, yytext, yy_line_number, yy_begin_column); return id;}

--Literals
{cadena}												{rl_lit_cad(yylval, yytext, yy_line_number, yy_begin_column); return literal;}
{caracter}												{rl_lit_car(yylval, yytext, yy_line_number, yy_begin_column); return literal;}							
{enter}													{rl_lit_ent(yylval, yytext, yy_line_number, yy_begin_column); return literal;}

--Separadors i comentaris
{separador}+											{null;}	
{comentari}												{null;}
{salt}													{null;}

--Qualsevol altra
. 														{return Error;}

%%

with phelps_tokens, declaracions.arbre, semantica.construccio_arbre;
use phelps_tokens, declaracions.arbre, semantica.construccio_arbre;
package analitzador_lexic is
	function yylex return token;
end analitzador_lexic;

with phelps_dfa, phelps_io; 
use phelps_dfa, phelps_io;
package body analitzador_lexic is
##
end analitzador_lexic;