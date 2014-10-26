%token id
%token literal
%token pc_accio
%token pc_es
%token pc_inicia
%token pc_fi
%token pc_entrada
%token pc_sortida
%token pc_tipus
%token pc_constant
%token pc_nou
%token pc_coleccio
%token pc_abast 
%token pc_de
%token pc_registre
%token pc_fregistre
%token pc_mentre
%token pc_fer
%token pc_fmentre
%token pc_si
%token pc_llavors
%token pc_sino
%token pc_fsi
%token op_no
%token op_i
%token op_o
%token s_dospunts
%token s_assig
%token s_oprel
%token s_mes
%token s_menys
%token s_prod
%token s_div
%token s_mod
%token s_parobert
%token s_partancat
%token s_abast
%token s_punt
%token s_coma
%token s_punticoma

%left op_o
%left op_i
%left op_no
%nonassoc s_oprel
%left s_mes, s_menys
%left s_menys_unitari
%left s_prod, s_div, s_mod

%with d_arbre;
{subtype yystype is d_arbre.pnode;}

%%

PROG:
     DPROC                                                                       	 {rs_prog($$, $1);}
  ;
DPROC:
     pc_accio ENCAP pc_es DECLS pc_inicia SENTS pc_fi id s_punticoma             	 {rs_dproc($$, $2, $4, $6, $8);}
  ;
ENCAP:
     id ARGS                                                                     	 {rs_encap($$, $1, $2);}
  ;
ARGS:
     s_parobert LARGS s_partancat                                                	 {rs_args($$, $2);}
|																				 	 {rs_args($$);}
  ;
LARGS:
     LARGS s_punticoma ARG                                                       	 {rs_largs($$, $1, $3);}
|    ARG                                                                         	 {rs_largs($$, $1);}
  ;
ARG:
     id s_dospunts MODE id                                                       	 {rs_arg($$, $1, $3, $4);}
  ;
MODE:
     pc_entrada                                                                  	 {rs_entrada($$);}       
|    pc_sortida                                                                  	 {rs_entradasortida($$);}
|    pc_entrada pc_sortida                                                       	 {rs_entradasortida($$);}
  ;
DECLS:
     DECLS DECL                                                                  	 {rs_decls($$, $1, $2);}
|			                                                                       	 {rs_decls($$);}
  ;
DECL:
     DVAR                                                                        	 {rs_ddvar($$, $1);}
|    DCONS           														     	 {rs_ddcons($$, $1);}
|    DTIPUS																		 	 {rs_ddtipus($$, $1);}
|    DPROC																		 	 {rs_ddproc($$, $1);}
  ;
DVAR:
     LID s_dospunts id s_punticoma                                               	 {rs_dvar($$, $1, $3);}
  ;
LID:
	 LID s_coma id                                                               	 {rs_lid($$, $1, $3);}
|    id																			 	 {rs_lid($$, $1);}
  ;
DCONS:
     LID s_dospunts pc_constant id s_assig VALOR s_punticoma                     	 {rs_dcons($$, $1, $4, $6);}
  ;
VALOR:
     s_menys VAL_ELEM  					                                         	 {rs_nvalor($$, $2);}
|    VAL_ELEM                                                                    	 {rs_valor($$, $1);}
  ;
VAL_ELEM:
     literal                                                                     	 {rs_lval($$, $1);}
|    id           																 	 {rs_idval($$, $1);}
  ;
DTIPUS:
     DSUBRANG                  													 	 {rs_dtipus_sub($$, $1);}
|    DREGISTRE																	 	 {rs_dtipus_reg($$, $1);}
|    DCOLECCIO																     	 {rs_dtipus_col($$, $1);}
  ;
DSUBRANG:
     pc_tipus id pc_es pc_nou id pc_abast VALOR s_abast VALOR s_punticoma        	 {rs_dsubrang($$, $2, $5, $7, $9);}
  ;
DREGISTRE:
     pc_tipus id pc_es pc_registre DCMPS pc_fregistre s_punticoma                	 {rs_dregistre($$, $2, $5);}
  ;
DCOLECCIO:
     pc_tipus id pc_es pc_coleccio s_parobert LID s_partancat pc_de id s_punticoma   {rs_dcoleccio($$, $2, $6, $9);}
  ;
DCMPS:
     DCMPS DCMP 																 	 {rs_dcmps_dd($$, $1, $2);}
|    DCMP                                                                        	 {rs_dcmps_d($$, $1);}
  ;
DCMP:
     id s_dospunts id s_punticoma												 	 {rs_dcmp($$, $1, $3);}
  ;
SENTS:
     SENTS SENT                                                                      {rs_sents($$, $1, $2);}
|    SENT																		  	 {rs_sents_s($$, $1);}
  ;
SENT:
     IF								  												 {rs_sent_if($$, $1);}
|    ITERACIO                        												 {rs_sent_it($$, $1);}
|    ASSIG                           			                                     {rs_sent_as($$, $1);}
|    CRIDA_PROC                      												 {rs_sent_cp($$, $1);} 
  ;
IF:
     pc_si E pc_llavors SENTS pc_fsi s_punticoma								  	 {rs_if($$, $2, $4);}
|    pc_si E pc_llavors SENTS pc_sino SENTS pc_fsi s_punticoma                       {rs_if($$, $2, $4, $6);}
  ;
ITERACIO:
     pc_mentre E pc_fer SENTS pc_fmentre s_punticoma                                 {rs_iteracio($$, $2, $4);}
  ;
ASSIG:
     REF s_assig E s_punticoma                                                       {rs_assig($$, $1, $3);}
  ; 
CRIDA_PROC:
     REF s_punticoma															     {rs_cridapr($$, $1);}
  ;
REF:
     id QS																		  	 {rs_ref($$, $1, $2);}
  ;
QS:
     QS Q                                                                            {rs_qs($$, $1, $2);}
|																				  	 {rs_qs($$);}
  ;
Q:
     s_punt id                                                                       {rs_q($$, $2);}
|    s_parobert LEXPS s_partancat												  	 {rs_q_lexps($$, $2);}
  ;
LEXPS:
     LEXPS s_coma E 															  	 {rs_lexps($$, $1, $3);}
|    E																			  	 {rs_lexps($$, $1);}
  ;
E:
     s_menys E %prec s_menys_unitari                                                 {rs_e_neg($$, $2);}
|    E s_mes E                                                                       {rs_e_mes($$, $1, $3);}
|    E s_menys E                                                                     {rs_e_menys($$, $1, $3);}
|    E s_prod E                                                                      {rs_e_prod($$, $1, $3);}
|    E s_div E                                                                       {rs_e_div($$, $1, $3);}
|    E s_mod E																	  	 {rs_e_mod($$, $1, $3);}
|    op_no E																	  	 {rs_e_no($$, $2);}
|    E op_i E                                                                        {rs_e_i($$, $1, $3);}
|    E op_o E                                                                        {rs_e_o($$, $1, $3);}
|    E s_oprel E																  	 {rs_e_oprel($$, $1, $2, $3);} 
|    s_parobert E s_partancat													  	 {rs_e_pars($$, $2);}
|    literal																	  	 {rs_e_lit($$, $1);}
|    REF																		  	 {rs_e_ref($$, $1);}
  ;
  
%%

package analitzador_sintactic is
   procedure yyparse;
end analitzador_sintactic;

with Text_io, phelps_tokens, phelps_goto, phelps_shift_reduce, phelps_error_report, analitzador_lexic, phelps_io, declaracions.arbre, semantica.construccio_arbre;
use phelps_tokens, phelps_goto, phelps_shift_reduce, phelps_error_report, analitzador_lexic, phelps_io, declaracions.arbre, semantica.construccio_arbre;

package body analitzador_sintactic is
	procedure yyerror(msg: string) is
	begin
		null;
	end yyerror;
##
end analitzador_sintactic;