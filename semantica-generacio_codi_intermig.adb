package body semantica.generacio_codi_intermig is

   procedure creaFitxerC3a(nom: string) is
   begin
      Seq_IO.Create(file, seq_IO.Out_file, nom & ".c3a");
      Ada.Text_IO.Create(llegible, Ada.Text_IO.Out_File, nom & ".c3as");
   end creaFitxerC3a;

   procedure tancaFitxerC3a is
   begin
      Seq_IO.Close(file);
      Ada.Text_IO.Close(llegible);
   end tancaFitxerC3a;

   procedure prepara(nom: in string) is
   begin
      d_pilaEtiq.pbuida(pcert);
      d_pilaEtiq.pbuida(pfals);
      creaFitxerC3a(nom);
   end prepara;

   procedure conclou is
   begin
      tancaFitxerC3a;
   end conclou;

   procedure gc_prog;

   procedure gen_codi_intermig is
   begin
      d_pilaProc.pbuida(pilaProc);
      gc_prog;
      mg_codiIntermigCorrecte;
      conclou;
   exception
      when others =>
         raise error_gen_codi_int;
   end gen_codi_intermig;

   --Funció auxiliar per escriure les variables a la versió llegible
   function escriu_var(nv: in numVar) return string is
      id: idnom;
   begin
      id := tv(nv).idvar;
      if id = null_idnom then
         return "_t"&nv'Img(2..nv'Img'Length);
      else
         return consultaNom(tn, id);
      end if;
   end escriu_var;

   procedure genera(i3a: in instr3a) is
      id: idnom;
   begin
      --Versió binaria
      Seq_IO.write(file, i3a);
      --Versió llegible
      case i3a.op is
         when op3_copia =>
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a1) & ":= " & escriu_var(i3a.b1));

         when op3_menysU =>
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a1) & ":= -" & escriu_var(i3a.b1));

         when op3_not =>
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a1) & ":= not" & escriu_var(i3a.b1));

         when op3_consIndx => --a2:= b2[c2];
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a2) & ":= " & escriu_var(i3a.b2) & "[" & escriu_var(i3a.c2) & "]");

         when op3_modIndx => --a2[b2]:= c2;
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a2) & "[" & escriu_var(i3a.b2) & "]:= " & escriu_var(i3a.c2));

         when op3_suma =>
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a2) & ":= " & escriu_var(i3a.b2) & " + " & escriu_var(i3a.c2));

         when op3_resta =>
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a2) & ":= " & escriu_var(i3a.b2) & " - " & escriu_var(i3a.c2));

         when op3_prod =>
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a2) & ":= " & escriu_var(i3a.b2) & " * " & escriu_var(i3a.c2));

         when op3_div =>
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a2) & ":= " & escriu_var(i3a.b2) & " / " & escriu_var(i3a.c2));

         when op3_mod =>
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a2) & ":= " & escriu_var(i3a.b2) & " mod " & escriu_var(i3a.c2));

         when op3_and =>
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a2) & ":= " & escriu_var(i3a.b2) & " and " & escriu_var(i3a.c2));

         when op3_or =>
            Ada.Text_IO.Put_Line (llegible, escriu_var(i3a.a2) & ":= " & escriu_var(i3a.b2) & " or " & escriu_var(i3a.c2));

         when op3_etiq =>
            Ada.Text_IO.Put_Line (llegible, "e"&i3a.e1'Img(2..i3a.e1'Img'Length) & ": skip");

         when op3_goto =>
            Ada.Text_IO.Put_Line (llegible, "goto " & "e"&i3a.e1'Img(2..i3a.e1'Img'Length));

         when op3_lt =>
            Ada.Text_IO.Put_Line (llegible, "if " & escriu_var(i3a.a3) & " < " & escriu_var(i3a.b3) & " then goto " & "e"&i3a.e2'Img(2..i3a.e2'Img'Length));

         when op3_le =>
            Ada.Text_IO.Put_Line (llegible, "if " & escriu_var(i3a.a3) & " <= " & escriu_var(i3a.b3) & " then goto " & "e"&i3a.e2'Img(2..i3a.e2'Img'Length));

         when op3_ig =>
            Ada.Text_IO.Put_Line (llegible, "if " & escriu_var(i3a.a3) & " = " & escriu_var(i3a.b3) & " then goto " & "e"&i3a.e2'Img(2..i3a.e2'Img'Length));

         when op3_dif =>
            Ada.Text_IO.Put_Line (llegible, "if " & escriu_var(i3a.a3) & " /= " & escriu_var(i3a.b3) & " then goto " & "e"&i3a.e2'Img(2..i3a.e2'Img'Length));

         when op3_ge =>
            Ada.Text_IO.Put_Line (llegible, "if " & escriu_var(i3a.a3) & " >= " & escriu_var(i3a.b3) & " then goto " & "e"&i3a.e2'Img(2..i3a.e2'Img'Length));

         when op3_gt =>
            Ada.Text_IO.Put_Line (llegible, "if " & escriu_var(i3a.a3) & " > " & escriu_var(i3a.b3) & " then goto " & "e"&i3a.e2'Img(2..i3a.e2'Img'Length));

         when op3_pmb =>
            id := tp(i3a.np).idProc;
            Ada.Text_IO.Put_Line (llegible, "pmb " & consultaNom(tn, id));

         when op3_rtn =>
            id := tp(i3a.np).idProc;
            Ada.Text_IO.Put_Line (llegible, "rtn " & consultaNom(tn, id));

         when op3_call =>
            id := tp(i3a.np).idProc;
            Ada.Text_IO.Put_Line (llegible, "call " & consultaNom(tn, id));

         when op3_params =>
            Ada.Text_IO.Put_Line (llegible, "params " & escriu_var(i3a.a4));

         when op3_paramc =>
            Ada.Text_IO.Put_Line (llegible, "paramc " & escriu_var(i3a.a5) & "[" & escriu_var(i3a.b5) & "]");

         when op3_noop =>
            null;
      end case;
   end genera;

   function novaVar return numVar is
      infV: infVar;
   begin
      nv := nv + 1;
      infV := (t_var, null_idnom, d_pilaProc.cim(pilaProc), false, 4, 0);
      tv(nv) := infV;
      return nv;
   end novaVar;

   function novaConst (val: in valor; tsb: in tSub) return numVar is
      infC: infVar;
      nVar: numVar;
      trobat: boolean;
   begin
      nVar := numVar'First;
      trobat := false;
      while not trobat and then nVar < nv loop
         nVar := nVar + 1;
         if tv(nVar).tv = t_const and then tv(nVar).tsb = tsb and then tv(nVar).val = val then
            trobat := true;
         end if;
      end loop;
      if not trobat then
         nv := nv + 1;
      	 infC := (t_const, null_idnom, val, tsb, enula);
         tv(nv) := infC;
         nVar := nv;
      end if;
      return nVar;
   end novaConst;

   function novaEtiqueta return etiqueta is
   begin
      ne := ne + 1;
      return ne;
   end novaEtiqueta;

   procedure desreferencia(r, d: in numVar; t: out numVar) is
      i3a: instr3a;
   begin
      if d = varNula then
         t := r;
      else
         t := novaVar;
         i3a := (op3_consIndx, t, r, d);
         genera(i3a);
      end if;
   end desreferencia;

   procedure gc_dproc(p: in out pnode);
   procedure gc_encap(p: in out pnode; np: out numProc);
   procedure gc_decls(p: in out pnode);
   procedure gc_sents(p: in out pnode);
   procedure gc_sent(p: in out pnode);
   procedure gc_si(p: in out pnode);
   procedure gc_iteracio(p: in out pnode);
   procedure gc_assignacio(p: in out pnode);
   procedure gc_cridaProc (p: in out pnode);
   procedure gc_expressio(p: in out pnode; r, d: out numVar; param_out: boolean);
   procedure gc_expressio_arit(p: in out pnode; r1, d1, r2, d2: in numVar; r, d: out numVar);
   procedure gc_expressio_log (p: in out pnode);
   procedure gc_expressio_rel(op: in op_oprel; r1, d1, r2, d2: in numvar);
   procedure gc_expressio_mu(r1, d1: in numVar;  r, d: out numVar);
   procedure gc_expressio_not;
   procedure gc_expressio_parent(r1, d1: in numVar; r, d: out numVar);
   procedure gc_expressio_ref(p: in out pnode; r, d: out numVar; tsb: in tSub; param_out: boolean);
   procedure gc_expressio_lit(p: in out pnode; r, d: out numVar);
   procedure gc_referencia(p: in out pnode; np: out numProc; rr, dr: out numVar);
   procedure gc_lexps_proc(p: in out pnode);
   procedure gc_qs(p: in out pnode; dr: in out numVar);
   procedure gc_q(p: in out pnode; dr: in out numVar);
   procedure gc_q_record(p: in out pnode; dr: in out numVar);
   procedure gc_q_array(p: in out pnode; dr: in out numVar);
   procedure gc_lexps(p: in out pnode; dr: in out numVar);

   procedure gc_prog is
   begin
      gc_dproc(arrel.all.f_prog);
   end gc_prog;

   procedure gc_dproc(p: in out pnode) is
      f1_dp: pnode renames p.all.f1_dp;
      f2_dp: pnode renames p.all.f2_dp;
      f3_dp: pnode renames p.all.f3_dp;
      i3a: instr3a;
      np: numProc;
   begin
      gc_decls(f2_dp);
      gc_encap(f1_dp, np);
      gc_sents(f3_dp);
      i3a := (op3_rtn, np);
      genera(i3a);
      d_pilaProc.desempila(pilaProc);
   end gc_dproc;

   procedure gc_encap(p: in out pnode; np: out numProc) is
      i3a: instr3a;
      e: etiqueta;
   begin
      np := p.all.np;
      if not tp(np).principal then --No es el principal
         e := novaEtiqueta;
      	 tp(np).e := e;
      	 i3a := (op3_etiq, e);
         genera(i3a);
      end if;
      if tp(np).principal then --Es el principal
         tp(np).e := eNula;
         i3a := (op3_etiq, eNula);
         genera(i3a);
      end if;
      i3a := (op3_pmb, np);
      genera(i3a);
      d_pilaProc.empila(pilaProc, np);
   end gc_encap;

   procedure gc_decls(p: in out pnode) is
      f1_decls: pnode renames p.all.f1_decls;
      f2_decls: pnode renames p.all.f2_decls;
   begin
      if f1_decls /= null then
         gc_decls(f1_decls);
         --Si es una declaració de procediment hem de generar codi
         if f2_decls.all.f_decl.all.tnd = nd_dproc then
            gc_dproc(f2_decls.all.f_decl);
         end if;
      end if;
   end gc_decls;

   procedure gc_sents(p: in out pnode) is
      f1_sents: pnode renames p.all.f1_sents;
      f2_sents: pnode renames p.all.f2_sents;
   begin
      if f2_sents /= null then
         gc_sents(f1_sents);
         gc_sent(f2_sents);
      else
         gc_sent(f1_sents);
      end if;
   end gc_sents;

   procedure gc_sent(p: in out pnode) is
      f_sent: pnode renames p.all.f_sent;
      tnd: tnode renames f_sent.all.tnd;
   begin
      case tnd is
         when nd_if =>
            gc_si(f_sent);
         when nd_it =>
            gc_iteracio(f_sent);
         when nd_assig =>
            gc_assignacio(f_sent);
         when nd_cp =>
            gc_cridaproc(f_sent);
         when others =>
      	    null;
      end case;
   end gc_sent;

   procedure gc_si(p: in out pnode) is
      f1_if: pnode renames p.all.f1_if;
      f2_if: pnode renames p.all.f2_if;
      f3_if: pnode renames p.all.f3_if;
      e, e2: etiqueta;
      r, d: numVar;
      i3a: instr3a;
   begin
      e := novaEtiqueta;
      d_pilaEtiq.empila(pcert, e);
      e := novaEtiqueta;
      d_pilaEtiq.empila(pfals, e);
      gc_expressio(f1_if, r, d, false);
      e := d_pilaEtiq.cim(pcert);
      i3a := (op3_etiq, e);
      genera(i3a);
      d_pilaEtiq.desempila(pcert);
      gc_sents(f2_if);
      if f3_if = null then
         e := d_pilaEtiq.cim(pfals);
         i3a := (op3_etiq, e);
         genera(i3a);
         d_pilaEtiq.desempila(pfals);
      else
         e := novaEtiqueta;
         i3a := (op3_goto, e);
         genera(i3a);
         e2 := d_pilaEtiq.cim(pfals);
         i3a := (op3_etiq, e2);
         genera(i3a);
         d_pilaEtiq.desempila(pfals);
         gc_sents(f3_if);
         i3a := (op3_etiq, e);
         genera(i3a);
      end if;
   end gc_si;

   procedure gc_iteracio(p: in out pnode) is
      f1_it: pnode renames p.all.f1_it;
      f2_it: pnode renames p.all.f2_it;
      e, e2: etiqueta;
      r, d: numVar;
      i3a: instr3a;
   begin
      e := novaEtiqueta;
      i3a := (op3_etiq, e);
      genera(i3a);
      e2 := novaEtiqueta;
      d_pilaEtiq.empila(pcert, e2);
      e2 := novaEtiqueta;
      d_pilaEtiq.empila(pfals, e2);
      gc_expressio(f1_it, r, d, false);
      e2 := d_pilaEtiq.cim(pcert);
      i3a := (op3_etiq, e2);
      genera(i3a);
      d_pilaEtiq.desempila(pcert);
      gc_sents(f2_it);
      i3a := (op3_goto, e);
      genera(i3a);
      e2 := d_pilaEtiq.cim(pfals);
      i3a := (op3_etiq, e2);
      genera(i3a);
      d_pilaEtiq.desempila(pfals);
   end gc_iteracio;

   procedure gc_assignacio(p: in out pnode) is
      f1_assig: pnode renames p.all.f1_assig;
      f2_assig: pnode renames p.all.f2_assig;
      rr, dr, re, de, t, vm1, v0: numVar;
      ec, ef, efi: etiqueta;
      np: numProc;
      i3a: instr3a;
   begin
      gc_referencia(f1_assig, np, rr, dr);
      --Necessari per una assignació de booleà correcta
      if f2_assig.all.etsb = ts_bool then
         ec := novaEtiqueta;
         d_pilaEtiq.empila(pcert, ec);
         ef := novaEtiqueta;
         d_pilaEtiq.empila(pfals, ef);
      end if;
      gc_expressio(f2_assig, re, de, false);
      if f2_assig.all.etsb = ts_bool then
         ec := d_pilaEtiq.cim(pcert);
         d_pilaEtiq.desempila(pcert);
         ef := d_pilaEtiq.cim(pfals);
         d_pilaEtiq.desempila(pfals);
         efi := novaEtiqueta;
         t := novaVar;
         vm1 := novaConst(-1, ts_bool);
         v0 := novaConst(0, ts_bool);
         i3a := (op3_etiq, ec);
         genera(i3a);
         i3a := (op3_copia, t, vm1);
         genera(i3a);
         i3a := (op3_goto, efi);
         genera(i3a);
         i3a := (op3_etiq, ef);
         genera(i3a);
         i3a := (op3_copia, t, v0);
         genera(i3a);
         i3a := (op3_etiq, efi);
         genera(i3a);
         if dr = varnula then
            i3a := (op3_copia, rr, t);
         else
            i3a := (op3_modIndx, rr, dr, t);
         end if;
         genera(i3a);
      else
      	 desreferencia(re, de, t);
      	 if dr = varnula then
            i3a := (op3_copia, rr, t);
         else
            i3a := (op3_modIndx, rr, dr, t);
         end if;
         genera(i3a);
      end if;
   end gc_assignacio;

   procedure gc_cridaProc (p: in out pnode) is
      f_cp: pnode renames p.all.f_cp;
      nproc: numProc;
      rr, dr: numvar;
      par: infPar;
      i3a: instr3a;
   begin
      d_pilaPars.pbuida(pilaPars);
      gc_referencia(f_cp, nproc, rr, dr);
      while not d_pilaPars.esBuida(pilaPars) loop
         par := d_pilaPars.cim(pilaPars);
         d_pilaPars.desempila(pilaPars);
         if par.d = varnula then
            i3a := (op3_params, par.r);
         else
            i3a := (op3_paramc, par.r, par.d);
         end if;
         genera(i3a);
      end loop;
      i3a := (op3_call, nproc);
      genera(i3a);
   end gc_cridaProc;

   procedure gc_expressio(p: in out pnode; r, d: out numVar; param_out: in boolean) is
      f1_e: pnode renames p.all.f1_e;
      f2_e: pnode renames p.all.f2_e;
      r1, d1, r2, d2: numVar;
   begin
      r := varnula;
      d := varnula;
      case p.all.te is
         when e_mes..e_mod =>
            gc_expressio(f1_e, r1, d1, false);
            gc_expressio(f2_e, r2, d2, false);
            gc_expressio_arit(p, r1, d1, r2, d2, r, d);
         when e_i..e_o =>
            gc_expressio_log(p);
         when e_oprel =>
            gc_expressio(f1_e, r1, d1, false);
            gc_expressio(f2_e, r2, d2, false);
            gc_expressio_rel(p.all.f_oprel.all.op, r1, d1, r2, d2);
         when e_neg =>
            gc_expressio(f1_e, r1, d1, false);
            gc_expressio_mu(r1, d1, r, d);
         when e_no =>
            gc_expressio(f1_e, r, d, false);
            gc_expressio_not;
         when e_pars =>
            gc_expressio(f1_e, r1, d1, param_out);
            gc_expressio_parent(r1, d1, r, d);
         when e_ref =>
            gc_expressio_ref(f1_e, r, d, p.all.etsb, param_out);
         when e_lit =>
            gc_expressio_lit(f1_e, r, d);
         when others =>
            null;
      end case;
   end gc_expressio;

   procedure gc_expressio_arit(p: in out pnode; r1, d1, r2, d2: in numVar; r, d: out numVar) is
      te: t_exp renames p.all.te;
      t, t1, t2: numvar;
      i3a: instr3a;
   begin
      desreferencia(r1, d1, t1);
      desreferencia(r2, d2, t2);
      t := novaVar;
      case te is
         when e_mes =>
            i3a := (op3_suma, t, t1, t2);
         when e_menys =>
            i3a := (op3_resta, t, t1, t2);
         when e_prod =>
            i3a := (op3_prod, t, t1, t2);
         when e_div =>
            i3a := (op3_div, t, t1, t2);
         when e_mod =>
            i3a := (op3_mod, t, t1, t2);
         when others =>
            null;
      end case;
      genera(i3a);
      r := t;
      d := varnula;
   end gc_expressio_arit;

   procedure gc_expressio_log (p: in out pnode) is
      f1_e: pnode renames p.all.f1_e;
      f2_e: pnode renames p.all.f2_e;
      te: t_exp renames p.all.te;
      r1, d1, r2, d2: numVar;
      e: etiqueta;
      i3a: instr3a;
   begin
      case te is
         when e_i =>
            e := novaEtiqueta;
            d_pilaEtiq.empila(pcert, e);
            gc_expressio(f1_e, r1, d1, false);
            e := d_pilaEtiq.cim(pcert);
            i3a := (op3_etiq, e);
            genera(i3a);
            d_pilaEtiq.desempila(pcert);
            gc_expressio(f2_e, r2, d2, false);
         when e_o =>
            e := novaEtiqueta;
            d_pilaEtiq.empila(pfals, e);
            gc_expressio(f1_e, r1, d1, false);
            e := d_pilaEtiq.cim(pfals);
            i3a := (op3_etiq, e);
            genera(i3a);
            d_pilaEtiq.desempila(pfals);
            gc_expressio(f2_e, r2, d2, false);
         when others =>
            null;
      end case;
   end gc_expressio_log;

   procedure gc_expressio_rel(op: in op_oprel; r1, d1, r2, d2: in numvar) is
      t1, t2: numVar;
      i3a: instr3a;
   begin
      desreferencia(r1, d1, t1);
      desreferencia(r2, d2, t2);
      case op is
         when op_igual =>
            i3a := (op3_ig, d_pilaEtiq.cim(pcert), t1, t2);
            genera(i3a);
            i3a := (op3_goto, d_pilaEtiq.cim(pfals));
            genera(i3a);
         when op_diferent =>
            i3a := (op3_dif, d_pilaEtiq.cim(pcert), t1, t2);
            genera(i3a);
            i3a := (op3_goto, d_pilaEtiq.cim(pfals));
            genera(i3a);
         when op_major =>
            i3a := (op3_gt, d_pilaEtiq.cim(pcert), t1, t2);
            genera(i3a);
            i3a := (op3_goto, d_pilaEtiq.cim(pfals));
            genera(i3a);
         when op_menor =>
            i3a := (op3_lt, d_pilaEtiq.cim(pcert), t1, t2);
            genera(i3a);
            i3a := (op3_goto, d_pilaEtiq.cim(pfals));
            genera(i3a);
         when op_majorigual =>
            i3a := (op3_ge, d_pilaEtiq.cim(pcert), t1, t2);
            genera(i3a);
            i3a := (op3_goto, d_pilaEtiq.cim(pfals));
            genera(i3a);
         when op_menorigual =>
            i3a := (op3_le, d_pilaEtiq.cim(pcert), t1, t2);
            genera(i3a);
            i3a := (op3_goto, d_pilaEtiq.cim(pfals));
            genera(i3a);
         when others =>
            null;
      end case;
   end gc_expressio_rel;

   procedure gc_expressio_mu(r1, d1: in numVar;  r, d: out numVar) is
      t, t1: numVar;
      i3a: instr3a;
   begin
      desreferencia(r1, d1, t1);
      t := novaVar;
      i3a := (op3_menysU, t, t1);
      genera(i3a);
      r := t;
      d := varNula;
   end gc_expressio_mu;

   procedure gc_expressio_not is
      ec, ef: etiqueta;
   begin
      ec := d_pilaEtiq.cim(pcert);
      d_pilaEtiq.desempila(pcert);
      ef := d_pilaEtiq.cim(pfals);
      d_pilaEtiq.desempila(pfals);
      d_pilaEtiq.empila(pcert, ef);
      d_pilaEtiq.empila(pfals, ec);
   end gc_expressio_not;

   procedure gc_expressio_parent(r1, d1: in numVar; r, d: out numVar) is
   begin
      r := r1;
      d := d1;
   end gc_expressio_parent;

   procedure gc_expressio_ref(p: in out pnode; r, d: out numVar; tsb: in tSub; param_out: in boolean) is
      np: numProc;
      t, vm1: numVar;
      i3a: instr3a;
   begin
      gc_referencia(p, np, r, d);
      if tsb = ts_bool and not param_out then
         desreferencia(r, d, t);
         r := varNula;
         d := varNula;
         vm1 := novaConst(-1, ts_bool);
         i3a := (op3_ig, d_pilaEtiq.cim(pcert), t, vm1);
         genera(i3a);
         i3a := (op3_goto, d_pilaEtiq.cim(pfals));
         genera(i3a);
      end if;
   end gc_expressio_ref;

   procedure gc_expressio_lit(p: in out pnode; r, d: out numVar) is
      val: valor renames p.all.val_lit;
      tsb: tSub renames p.all.tsb;
   begin
      r := novaConst(val, tsb);
      d := varnula;
   end gc_expressio_lit;

   procedure gc_referencia(p: in out pnode; np: out numProc; rr, dr: out numVar) is
      f2_ref: pnode renames p.all.f2_ref;
   begin
      if p.all.esProc then
         if f2_ref.all.f1_qs /= null then   --Si te paràmetres...
            gc_lexps_proc(f2_ref.all.f2_qs.all.f_q);
         end if;
         np := p.all.numP;
      else
         rr := p.all.nv;
         dr := varNula;
      	 gc_qs(f2_ref, dr);
      end if;
   end gc_referencia;

   procedure gc_lexps_proc(p: in out pnode) is
      f1_lexps: pnode renames p.all.f1_lexps;
      f2_lexps: pnode renames p.all.f2_lexps;
      r, d, v0, vm1, t: numVar;
      ec, ef, efi: etiqueta;
      i3a: instr3a;
   begin
      if f2_lexps /= null then
         gc_lexps_proc(f1_lexps);
         if f2_lexps.all.etsb = ts_bool then   --Quan trobam un paràmetre booleà...
            ec := novaEtiqueta;
            d_pilaEtiq.empila(pcert, ec);
            ef := novaEtiqueta;
            d_pilaEtiq.empila(pfals, ef);
         end if;
         gc_expressio(f2_lexps, r, d, f2_lexps.param_out);
      else
         if f1_lexps.all.etsb = ts_bool then   --Quan trobam un paràmetre booleà...
            ec := novaEtiqueta;
            d_pilaEtiq.empila(pcert, ec);
            ef := novaEtiqueta;
            d_pilaEtiq.empila(pfals, ef);
         end if;
         gc_expressio(f1_lexps, r, d, f1_lexps.param_out);
      end if;
      if r = varnula and d = varnula then
         ec := d_pilaEtiq.cim(pcert);
         d_pilaEtiq.desempila(pcert);
         ef := d_pilaEtiq.cim(pfals);
         d_pilaEtiq.desempila(pfals);
         efi := novaEtiqueta;
         t := novaVar;
         vm1 := novaConst(-1, ts_bool);
         v0 := novaConst(0, ts_bool);
         i3a := (op3_etiq, ec);
         genera(i3a);
         i3a := (op3_copia, t, vm1);
         genera(i3a);
         i3a := (op3_goto, efi);
         genera(i3a);
         i3a := (op3_etiq, ef);
         genera(i3a);
         i3a := (op3_copia, t, v0);
         genera(i3a);
         i3a := (op3_etiq, efi);
         genera(i3a);
         r := t;
      end if;
      d_pilaPars.empila(pilaPars, (r, d));
   end gc_lexps_proc;

   procedure gc_qs(p: in out pnode; dr: in out numVar) is
      f1_qs: pnode renames p.all.f1_qs;
      f2_qs: pnode renames p.all.f2_qs;
   begin
      if f1_qs /= null then
         gc_qs(f1_qs, dr);
         gc_q(f2_qs, dr);
      end if;
   end gc_qs;

   procedure gc_q(p: in out pnode; dr: in out numVar) is
      f_q: pnode renames p.all.f_q;
   begin
      if f_q.all.tnd = nd_id then
         gc_q_record(p, dr);
      else
         gc_q_array(p, dr);
      end if;
   end gc_q;

   procedure gc_q_record(p: in out pnode; dr: in out numVar) is
      desp: despl renames p.all.desp;
      t, td: numVar;
      i3a: instr3a;
   begin
      t := novaConst(valor(desp), ts_ent);
      if dr = varnula then
         dr := t;
      else
         td := novaVar;
         i3a := (op3_suma, td, dr, t);
         genera(i3a);
         dr := td;
      end if;
   end gc_q_record;

   procedure gc_q_array(p: in out pnode; dr: in out numVar) is
      f_q: pnode renames p.all.f_q;
      w: despl renames p.all.w;
      b: valor renames p.all.b;
      d, t1, t2, t3, tb, tw: numVar;
      i3a: instr3a;
   begin
      d := varNula;
      gc_lexps(f_q, d);
      t1 := novaVar;
      tb := novaConst(b, ts_ent);
      i3a := (op3_resta, t1, d, tb);
      genera(i3a);
      tw := novaConst(valor(w), ts_ent);
      t2 := novaVar;
      i3a := (op3_prod, t2, t1, tw);
      genera(i3a);
      if dr = varnula then
         dr := t2;
      else
         t3 := novaVar;
         i3a := (op3_suma, t3, t2, dr);
         genera(i3a);
         dr := t3;
      end if;
   end gc_q_array;

   procedure gc_lexps(p: in out pnode; dr: in out numvar) is
      f1_lexps: pnode renames p.all.f1_lexps;
      f2_lexps: pnode renames p.all.f2_lexps;
      t, t1, t2, te, rn, r, d: numVar;
      i3a: instr3a;
   begin
      if f2_lexps /= null then
         gc_lexps(f1_lexps, dr);
         gc_expressio(f2_lexps, r, d, false);
         t1 := novaVar;
         rn := novaConst(f2_lexps.all.n, ts_ent);
         i3a := (op3_prod, t1, dr, rn);
         genera(i3a);
         desreferencia(r, d, te);
         t2 := novaVar;
         i3a := (op3_suma, t2, t1, te);
         genera(i3a);
         dr := t2;
      else
         gc_expressio(f1_lexps, r, d, false);
         desreferencia(r, d, t);
         dr := t;
      end if;
   end gc_lexps;

end semantica.generacio_codi_intermig;
