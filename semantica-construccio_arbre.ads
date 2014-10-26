with declaracions, declaracions.arbre, declaracions.desc, 
     declaracions.t_noms, declaracions.t_simbols, declaracions.c3a; 

use declaracions, declaracions.arbre, declaracions.desc, 
    declaracions.t_noms, declaracions.t_simbols, declaracions.c3a;

package semantica.construccio_arbre is
   
   --Rutines lèxiques
   procedure rl_atom(pp: out pnode; li, co: in natural);
   procedure rl_lit_cad(pp: out pnode; c: in string; li, co: in natural);
   procedure rl_lit_car(pp: out pnode; c: in string; li, co: in natural);
   procedure rl_lit_ent(pp: out pnode; c: in string; li, co: in natural);
   procedure rl_identificador(pp: out pnode; c: in string; li, co: in natural);
   procedure rl_oprel(pp: out pnode; c: in string; li, co: in natural);
   
   --Rutines semàntiques
   procedure rs_prog(pp: out pnode; p: in pnode);
   procedure rs_dproc(pp: out pnode; p1, p2, p3, p4: in pnode);
   procedure rs_encap(pp: out pnode; p1, p2: in pnode);
   procedure rs_args(pp: out pnode; p: in pnode);
   procedure rs_args(pp: out pnode);
   procedure rs_largs(pp: out pnode; p1, p2: in pnode);
   procedure rs_largs(pp: out pnode; p: in pnode);
   procedure rs_arg(pp: out pnode; p1, p2, p3: in pnode);
   procedure rs_entrada(pp: out pnode);
   procedure rs_entradasortida(pp: out pnode);
   procedure rs_decls(pp: out pnode; p1, p2: in pnode);
   procedure rs_decls(pp: out pnode);
   procedure rs_ddvar(pp: out pnode; p: in pnode);
   procedure rs_ddcons(pp: out pnode; p: in pnode);
   procedure rs_ddtipus(pp: out pnode; p: in pnode);
   procedure rs_ddproc(pp: out pnode; p: in pnode);
   procedure rs_dvar(pp: out pnode; p1, p2: in pnode);
   procedure rs_lid(pp: out pnode; p1, p2: in pnode);
   procedure rs_lid(pp: out pnode; p: in pnode);
   procedure rs_dcons(pp: out pnode; p1, p2, p3: in pnode);
   procedure rs_nvalor(pp: out pnode; p: in pnode);
   procedure rs_valor(pp: out pnode; p: in pnode);
   procedure rs_lval(pp: out pnode; p: in pnode);
   procedure rs_idval(pp: out pnode; p: in pnode);
   procedure rs_dtipus_sub(pp: out pnode; p: in pnode);
   procedure rs_dtipus_reg(pp: out pnode; p: in pnode);
   procedure rs_dtipus_col(pp: out pnode; p: in pnode);
   procedure rs_dsubrang(pp: out pnode; p1, p2, p3, p4: in pnode);
   procedure rs_dregistre(pp: out pnode; p1, p2: in pnode);
   procedure rs_dcoleccio(pp: out pnode; p1, p2, p3: in pnode);
   procedure rs_dcmps_dd(pp: out pnode; p1, p2: in pnode);
   procedure rs_dcmps_d(pp: out pnode; p: in pnode);
   procedure rs_dcmp(pp: out pnode; p1, p2: in pnode);
   procedure rs_sents(pp: out pnode; p1, p2: in pnode);
   procedure rs_sents_s(pp: out pnode; p: in pnode);
   procedure rs_sent_if(pp: out pnode; p: in pnode);
   procedure rs_sent_it(pp: out pnode; p: in pnode);
   procedure rs_sent_as(pp: out pnode; p: in pnode);
   procedure rs_sent_cp(pp: out pnode; p: in pnode);
   procedure rs_if(pp: out pnode; p1, p2: in pnode);
   procedure rs_if(pp: out pnode; p1, p2, p3: in pnode);
   procedure rs_iteracio(pp: out pnode; p1, p2: in pnode);
   procedure rs_assig(pp: out pnode; p1, p2: in pnode);
   procedure rs_cridapr(pp: out pnode; p: in pnode);
   procedure rs_ref(pp: out pnode; p1, p2: in pnode);
   procedure rs_qs(pp: out pnode; p1, p2: in pnode);
   procedure rs_qs(pp: out pnode);
   procedure rs_q(pp: out pnode; p: in pnode);
   procedure rs_q_lexps(pp: out pnode; p: in pnode);
   procedure rs_lexps(pp: out pnode; p1, p2: in pnode);
   procedure rs_lexps(pp: out pnode; p: in pnode);
   procedure rs_e_neg(pp: out pnode; p: in pnode);
   procedure rs_e_mes(pp: out pnode; p1, p2: in pnode);
   procedure rs_e_menys(pp: out pnode; p1, p2: in pnode);
   procedure rs_e_prod(pp: out pnode; p1, p2: in pnode);
   procedure rs_e_div(pp: out pnode; p1, p2: in pnode);
   procedure rs_e_mod(pp: out pnode; p1, p2: in pnode);
   procedure rs_e_no(pp: out pnode; p: in pnode);
   procedure rs_e_i(pp: out pnode; p1, p2: in pnode);
   procedure rs_e_o(pp: out pnode; p1, p2: in pnode);
   procedure rs_e_oprel(pp: out pnode; p1, p2, p3: in pnode);
   procedure rs_e_pars(pp: out pnode; p: in pnode);
   procedure rs_e_lit(pp: out pnode; p: in pnode);
   procedure rs_e_ref(pp: out pnode; p: in pnode);

end semantica.construccio_arbre;