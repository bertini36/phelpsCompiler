package body semantica.construccio_arbre is
     
   --Rutines lèxiques
   procedure rl_atom(pp: out pnode; li,co: in natural) is
      pos : posicio;
   begin
      pos := (li,co);
      pp := new node'(nd_atom,pos);
   end rl_atom;

   procedure rl_lit_cad(pp: out pnode; c: in string; li,co: in natural) is
      pos : posicio;
      sid : idstring;
      val : valor;
      s : string(1..c'Length-2);
   begin
      pos := (li,co);
      s := c(c'First+1..c'Last-1);
      posaString(tn,s,sid);
      val := Valor(sid);
      pp := new node'(nd_lit,ts_arr,val,pos);
   end rl_lit_cad;

   procedure rl_lit_car(pp: out pnode; c: in string; li,co: in natural) is
      pos : posicio;
      val : valor;
   begin
      pos := (li,co);
      val := Valor(Character'pos(c(c'First+1)));
      pp := new node'(nd_lit,ts_car,val,pos);
   end rl_lit_car;

   procedure rl_lit_ent(pp: out pnode; c: in string; li,co: in natural) is
      pos : posicio;
      val : valor;
   begin
      pos := (li,co);
      val := Valor(Integer'Value(c));
      pp := new node'(nd_lit,ts_ent,val,pos);
   end rl_lit_ent;

   procedure rl_identificador(pp: out pnode; c: in string; li,co: in natural) is
      pos : posicio;
      nid : idnom;
   begin
      pos := (li,co);
      posaNom(tn,c,nid);
      pp := new node'(nd_id,nid,pos);
   end rl_identificador;

   procedure rl_oprel(pp: out pnode; c: in string; li,co: in natural) is
      pos : posicio;
      op: op_oprel;
   begin
      pos := (li,co);
      if c = "=" then op := op_igual;
      elsif c = "/=" then op := op_diferent;
      elsif c = ">" then op := op_major;
      elsif c = "<" then op := op_menor;
      elsif c = ">=" then op := op_majorigual;
      elsif c = "<=" then op := op_menorigual;
      end if;
      pp:= new node'(nd_oprel,op,pos);
   end rl_oprel;

   --Rutines semàntiques
   procedure rs_prog(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_prog, p);
   end rs_prog;

   procedure rs_dproc(pp: out pnode; p1, p2, p3, p4: in pnode) is
   begin
      pp:= new node'(nd_dproc, p1, p2, p3, p4);
   end rs_dproc;

   procedure rs_encap(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_encap, p1, p2, procNul);
   end rs_encap;

   procedure rs_args(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_args, p);
   end rs_args;

   procedure rs_args(pp: out pnode) is
   begin
      pp:= new node'(nd_args, null);
   end rs_args;

   procedure rs_largs(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_largs, p1, p2);
   end rs_largs;

   procedure rs_largs(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_largs, p, null);
   end rs_largs;

   procedure rs_arg(pp: out pnode; p1, p2, p3: in pnode) is
   begin
      pp:= new node'(nd_arg, p1, p2, p3);
   end rs_arg;

   procedure rs_entrada(pp: out pnode) is
   begin
      pp:= new node'(nd_mode,m_in);
   end rs_entrada;

   procedure rs_entradasortida(pp: out pnode) is
   begin
      pp:= new node'(nd_mode,m_in_out);
   end rs_entradasortida;

   procedure rs_decls(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_decls, p1, p2);
   end rs_decls;

   procedure rs_decls(pp: out pnode) is
   begin
      pp:= new node'(nd_decls, null, null);
   end rs_decls;

   procedure rs_ddvar(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_decl, p);
   end rs_ddvar;

   procedure rs_ddcons(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_decl, p);
   end rs_ddcons;

   procedure rs_ddtipus(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_decl, p);
   end rs_ddtipus;

   procedure rs_ddproc(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_decl, p);
   end rs_ddproc;

   procedure rs_dvar(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_dvar, p1, p2);
   end rs_dvar;

   procedure rs_lid(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_lid, p1, p2);
   end rs_lid;

   procedure rs_lid(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_lid, p, null);
   end rs_lid;

   procedure rs_dcons(pp: out pnode; p1, p2, p3: in pnode) is
   begin
      pp:= new node'(nd_dcons, p1, p2, p3);
   end rs_dcons;

   procedure rs_nvalor(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_val, p, null);
   end rs_nvalor;

   procedure rs_valor(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_val, null, p);
   end rs_valor;

   procedure rs_lval(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_valelem, p);
   end rs_lval;

   procedure rs_idval(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_valelem, p);
   end rs_idval;

   procedure rs_dtipus_sub(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_dtipus, p);
   end rs_dtipus_sub;

   procedure rs_dtipus_reg(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_dtipus, p);
   end rs_dtipus_reg;

   procedure rs_dtipus_col(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_dtipus, p);
   end rs_dtipus_col;

   procedure rs_dsubrang(pp: out pnode; p1, p2, p3, p4: in pnode) is
   begin
      pp:= new node'(nd_dsub, p1, p2, p3, p4);
   end rs_dsubrang;

   procedure rs_dregistre(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_dreg, p1, p2);
   end rs_dregistre;
   
   procedure rs_dcoleccio(pp: out pnode; p1, p2, p3: in pnode) is
   begin
      pp:= new node'(nd_dcol, p1, p2, p3);
   end rs_dcoleccio;

   procedure rs_dcmps_dd(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_dcmps, p1, p2);
   end rs_dcmps_dd;

   procedure rs_dcmps_d(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_dcmps, p, null);
   end rs_dcmps_d;

   procedure rs_dcmp(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_dcmp, p1, p2);
   end rs_dcmp;

   procedure rs_sents(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_sents, p1, p2);
   end rs_sents;

   procedure rs_sents_s(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_sents, p, null);
   end rs_sents_s;
   
   procedure rs_sent_if(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_sent, p);
   end rs_sent_if;
   
   procedure rs_sent_it(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_sent, p);
   end rs_sent_it;
   
   procedure rs_sent_as(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_sent, p);
   end rs_sent_as;
   
   procedure rs_sent_cp(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_sent, p);
   end rs_sent_cp;

   procedure rs_if(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_if, p1, p2, null);
   end rs_if;

   procedure rs_if(pp: out pnode; p1, p2, p3: in pnode) is
   begin
      pp:= new node'(nd_if, p1, p2, p3);
   end rs_if;

   procedure rs_iteracio(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_it, p1, p2);
   end rs_iteracio;

   procedure rs_assig(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_assig, p1, p2);
   end rs_assig;

   procedure rs_cridapr(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_cp, p);
   end rs_cridapr;

   procedure rs_ref(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_ref, p1, p2, false, procNul, varNula);
   end rs_ref;

   procedure rs_qs(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_qs, p1, p2);
   end rs_qs;

   procedure rs_qs(pp: out pnode) is
   begin
      pp:= new node'(nd_qs, null, null);
   end rs_qs;

   procedure rs_q(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_q, p, 0, 0, 0);
   end rs_q;

   procedure rs_q_lexps(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_q, p, 0, 0, 0);
   end rs_q_lexps;
   
   procedure rs_lexps(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_lexps, p1, p2);
   end rs_lexps;

   procedure rs_lexps(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_lexps, p, null);
   end rs_lexps;

   procedure rs_e_neg(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_e, e_neg, p, null, null, 0, ts_nul, false);
   end rs_e_neg;

   procedure rs_e_mes(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_e, e_mes, p1, p2, null, 0, ts_nul, false);
   end rs_e_mes;

   procedure rs_e_menys(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_e, e_menys, p1, p2, null, 0, ts_nul, false);
   end rs_e_menys;

   procedure rs_e_prod(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_e, e_prod, p1, p2, null, 0, ts_nul, false);
   end rs_e_prod;

   procedure rs_e_div(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_e, e_div, p1, p2, null, 0, ts_nul, false);
   end rs_e_div;

   procedure rs_e_mod(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_e, e_mod, p1, p2, null, 0, ts_nul, false);
   end rs_e_mod;

   procedure rs_e_no(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_e, e_no, p, null, null, 0, ts_nul, false);
   end rs_e_no;

   procedure rs_e_i(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_e, e_i, p1, p2, null, 0, ts_nul, false);
   end rs_e_i;

   procedure rs_e_o(pp: out pnode; p1, p2: in pnode) is
   begin
      pp:= new node'(nd_e, e_o, p1, p2, null, 0, ts_nul, false);
   end rs_e_o;

   procedure rs_e_oprel(pp: out pnode; p1, p2, p3: in pnode) is
   begin
      pp:= new node'(nd_e, e_oprel, p1, p3, p2, 0, ts_nul, false);
   end rs_e_oprel;

   procedure rs_e_pars(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_e, e_pars, p, null, null, 0, ts_nul, false);
   end rs_e_pars;

   procedure rs_e_lit(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_e, e_lit, p, null, null, 0, ts_nul, false);
   end rs_e_lit;

   procedure rs_e_ref(pp: out pnode; p: in pnode) is
   begin
      pp:= new node'(nd_e, e_ref, p, null, null, 0, ts_nul, false);
   end rs_e_ref;

end semantica.construccio_arbre;