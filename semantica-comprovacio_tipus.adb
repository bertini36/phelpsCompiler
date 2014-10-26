package body semantica.comprovacio_tipus is

   -- Definició de tipus enumerats
   type mode_exp is (mode_var, mode_res);
   type mode_ref is (r_var, r_cons, r_proc);

   procedure posa_entorn_standard is
      id, id_ent, id_char, id_bool, id_cad, id_proc, id_par: idnom;
      d: descripcio;
      error: boolean;
      infV: infVar;
      infP: infProc;
   begin
      --ENTER
      posaNom(tn,"enter",id_ent);
      d := (d_tipus,(ts_ent,4,valor(integer'first),valor(integer'last)));
      posa(ts,id_ent,d,error);

      --CARACTER
      posaNom(tn,"caracter",id_char);
      d := (d_tipus,(ts_car,4,character'pos(character'first),character'pos(character'last)));
      posa(ts,id_char,d,error);

      --BOOLEA
      posaNom(tn,"boolea",id_bool);
      d := (d_tipus,(ts_bool,4,-1,0));
      posa(ts,id_bool,d,error);

      --CADENA
      posaNom(tn,"cadena",id_cad);
      d := (d_tipus,(ts_arr,despl(max_ch*4),id_char));
      posa(ts,id_cad,d,error);

      --CERT
      posaNom(tn,"cert",id);
      nv := nv + 1;
      infV := (t_const, id, -1, ts_bool, enula);
      tv(nv) := infV;
      d := (d_const,id_bool,-1,nv);
      posa(ts,id,d,error);

      --FALS
      posaNom(tn,"fals",id);
      nv := nv + 1;
      infV := (t_const, id, 0, ts_bool, enula);
      tv(nv) := infV;
      d := (d_const,id_bool,0,nv);
      posa(ts,id,d,error);

      --PUTI
      posaNom(tn, "puti", id_proc);
      posaNom(tn, "i_puti", id_par);
      infP:= (p_extern, id_proc);
      np := np + 1;
      tp(np) := infP;
      infV := (t_var, id_par, np, true, 4, 12);
      nv := nv + 1;
      tv(nv) := infV;
      d := (d_proc, np);
      posa(ts, id_proc, d, error);
      d := (d_argc, id_ent, nv);
      posaArg(ts, id_proc, id_par, d, error);

      --GETI
      posaNom(tn, "geti", id_proc);
      posaNom(tn, "i_geti", id_par);
      infP := (p_extern, id_proc);
      np := np + 1;
      tp(np) := infP;
      infV := (t_var, id_par, np, true, 4, 12);
      nv := nv + 1;
      tv(nv) := infV;
      d := (d_proc, np);
      posa(ts, id_proc, d, error);
      d := (d_var,id_ent,nv);
      posaArg(ts, id_proc, id_par, d, error);

      --PUTC
      posaNom(tn, "putc", id_proc);
      posaNom(tn, "c_putc", id_par);
      infP := (p_extern, id_proc);
      np := np + 1;
      tp(np) := infP;
      infV := (t_var, id_par, np, true, 4, 12);
      nv := nv + 1;
      tv(nv) := infV;
      d := (d_proc, np);
      posa(ts, id_proc, d, error);
      d := (d_argc, id_char, nv);
      posaArg(ts, id_proc, id_par, d, error);

      --GETC
      posaNom(tn, "getc", id_proc);
      posaNom(tn, "c_getc", id_par);
      infP := (p_extern, id_proc);
      np := np + 1;
      tp(np) := infP;
      infV := (t_var, id_par, np, true, 4, 12);
      nv := nv + 1;
      tv(nv) := infV;
      d := (d_proc, np);
      posa(ts, id_proc, d, error);
      d := (d_var, id_char, nv);
      posaArg(ts, id_proc, id_par, d, error);

      --PUTS
      posaNom(tn, "puts", id_proc);
      posaNom(tn, "s_puts", id_par);
      infP := (p_extern, id_proc);
      np := np + 1;
      tp(np) := infP;
      infV := (t_var, id_par, np, true, 4, 12);
      nv := nv + 1;
      tv(nv) := infV;
      d := (d_proc, np);
      posa(ts, id_proc, d, error);
      d := (d_argc, id_cad, nv);
      posaArg(ts, id_proc, id_par, d, error);

      --NEW_LINE
      posaNom(tn, "new_line", id_proc);
      infP := (p_extern, id_proc);
      np := np + 1;
      tp(np) := infP;
      d := (d_proc, np);
      posa(ts, id_proc, d, error);
   end posa_entorn_standard;

   procedure prepara is
   begin
      posa_entorn_standard;
      error_ct := false;
   end prepara;

   procedure ct_prog;

   procedure compr_tipus is
   begin
      arrel := yyval;
      d_pilaProc.pbuida(pilaProc);
      ct_prog;
      if error_ct then
         raise error_compr_tipus;
      else
         mg_comprTipusCorrecte;
      end if;
   end compr_tipus;

   procedure ct_dproc(p: in out pnode; idencap: out idnom);
   procedure ct_encap(p: in out pnode; idencap: out idnom);
   procedure ct_largs(p: in out pnode; idp: in idnom);
   procedure ct_arg(p: in out pnode; idp: in idnom);
   procedure ct_mode(p: in out pnode; mode: out t_mode);
   procedure ct_dvar(p: in out pnode);
   procedure ct_lid_var(p: in out pnode; idt: in idnom);
   procedure ct_dcons(p: in out pnode);
   procedure ct_valor(p: in out pnode; idt: out idnom; tsb: out tSub; vc: out valor);
   procedure ct_val_elem(p: in out pnode; idt: out idnom; tsb: out tSub; vc: out valor);
   procedure ct_lid_cons(p: in out pnode; idtc: in idnom; vc: in valor);
   procedure ct_subrang(p: in pnode);
   procedure ct_registre(p: in pnode);
   procedure ct_dcmps(p: in pnode; idr: in idnom; ocup: out despl);
   procedure ct_dcmp(p: in pnode; idr: in idnom; desp: in despl; ocup: out despl);
   procedure ct_dcoleccio(p: in out pnode);
   procedure ct_lid_col(p: in out pnode; nc: out despl; ida: in idnom);
   procedure ct_decls(p: in out pnode);
   procedure ct_decl(p: in out pnode);
   procedure ct_sents(p: in out pnode);
   procedure ct_sent(p: in out pnode);
   procedure ct_dtipus(p:in out pnode);
   procedure ct_si(p: in out pnode);
   procedure ct_iteracio(p: in out pnode);
   procedure ct_assignacio(p: in out pnode);
   procedure ct_cridaproc(p: in out pnode);
   procedure ct_e(p: in out pnode; eidt: out idnom; etsb: out tSub; emode: out mode_exp; li, co: out natural);
   procedure ct_ref(p: in out pnode; ridt: out idnom; rmode: out mode_ref; rtsb: out tSub; li, co: out natural);
   procedure ct_qs_proc(p: in out pnode; idp: in idnom; li, co: in natural);
   procedure ct_lexps_proc(p: in out pnode; idp: in idnom; it: in out iterator_arg; li, co: in natural);
   procedure ct_qs(p: in out pnode; idt: in out idnom; li,co: in natural);
   procedure ct_q (p: in out pnode; idt: in out idnom; li, co: in natural);
   procedure ct_camp_q(p: in out pnode; idt: in out idnom; li, co: in natural);
   procedure ct_lexps_q(p: in out pnode; it: in out iterator_indx; li, co: in natural);

   procedure test_compatibilitat(idt1: in idnom; idt2: in idnom; tsb1: in tSub; tsb2: in tSub; idtr: out idnom; tsbr: out tSub; li, co: in natural) is
   begin
      if (idt1 /= null_idnom) and (idt2 /= null_idnom) then
         if idt1 /= idt2 then
            me_tipusDistints(li, co, idt1, idt2);
            error_ct := true;
         end if;
         idtr := idt1;
         tsbr := tsb1;
      elsif (idt1 = null_idnom) and (idt2 /= null_idnom) then
         if tsb1 /= tsb2 then
            me_tipusNoCompatibles(li, co, tsb1, tsb2);
            error_ct := true;
         end if;
         idtr := idt2;
         tsbr := tsb2;
      elsif (idt1 /= null_idnom) and (idt2 = null_idnom) then
         if tsb1 /= tsb2 then
            me_tipusNoCompatibles(li, co, tsb1, tsb2);
            error_ct := true;
         end if;
         idtr := idt1;
         tsbr := tsb1;
      else
         if tsb1 /= tsb2 then
            me_tipusNoCompatibles(li, co, tsb1, tsb2);
            error_ct := true;
         end if;
         idtr := null_idnom; -- Típus universal
         tsbr := tsb1;
      end if;
   end test_compatibilitat;

   procedure ct_prog is
      f_prog: pnode renames arrel.all.f_prog;
      idproc: idnom;
      it: iterator_arg;
   begin
      ct_dproc(f_prog, idproc);
      it := primer(ts, idproc);
      if esValid(it) then
         me_argumentsProgPrincipal;
      	 error_ct := true;
      end if;
   end ct_prog;

   procedure ct_dproc(p: in out pnode; idencap: out idnom) is
      f1_dp: pnode renames p.all.f1_dp;
      f2_dp: pnode renames p.all.f2_dp;
      f3_dp: pnode renames p.all.f3_dp;
      f4_dp: pnode renames p.all.f4_dp;
      li: natural renames f4_dp.all.pos_id.li;
      co: natural renames f4_dp.all.pos_id.co;
      dprocid: idnom;
   begin
      ct_encap(f1_dp, idencap);
      ct_decls(f2_dp);
      ct_sents(f3_dp);
      dprocid := f4_dp.all.id;
      if idencap /= dprocid then
         me_idProcDif(li, co, idencap, dprocid);
         error_ct := true;
      end if;
      d_pilaProc.desempila(pilaProc);
      surtBloc(ts);
   end ct_dproc;

   procedure ct_encap(p: in out pnode; idencap: out idnom) is
      f1_encap: pnode renames p.all.f1_encap;
      f2_encap: pnode renames p.all.f2_encap;
      encap_id: idnom renames f1_encap.all.id;
      li: natural renames f1_encap.all.pos_id.li;
      co: natural renames f1_encap.all.pos_id.co;
      d, da, dp: descripcio;
      infV: infVar;
      infP: infProc;
      it: iterator_arg;
      nparam: integer;
      ida: idnom;
      e: boolean;
   begin
      np := np + 1;
      p.all.np := np; --Necessari per GC
      dp := (d_proc, np);
      posa (ts, encap_id, dp, e);
      if e then
         me_jaExisteix(li,co,encap_id,"el procediment");
         error_ct := true;
      end if;
      if d_pilaProc.esBuida(pilaProc) then   --Es el programa principal
         infP := (p_intern, encap_id, 0, 0, enula, 0, true);
      else
         infP := (p_intern, encap_id, tp(d_pilaProc.cim(pilaProc)).prof+1, 0, enula, 0, false);
      end if;
      tp(np) := infP;
      idencap := encap_id;
      d_pilaProc.empila(pilaProc,np);
      if f2_encap.all.f1_args /= null then
         ct_largs(f2_encap.all.f1_args, encap_id);
         entraBloc(ts);
         it := primer(ts, encap_id);
         nparam := 0;
         while esValid(it) loop
            nparam := nparam + 1;
            consulta(ts, it, ida, da);
            nv := nv + 1;
            infV := (t_var,ida,d_pilaProc.cim(pilaProc),true, 4,despl(8+nparam*4));
            tv(nv) := infV;
            if da.td = d_var then
               da.nv := nv;
            else
               da.na := nv;
            end if;
            posa(ts, ida, da, e);
            it := successor(ts, it);
         end loop;
         tp(np).numParam := nparam;
      else
         entraBloc(ts);
      end if;
   end ct_encap;

   procedure ct_largs(p: in out pnode; idp: in idnom) is
      f1_largs: pnode renames p.all.f1_largs;
      f2_largs: pnode renames p.all.f2_largs;
   begin
      if f2_largs /= null then
         ct_largs(f1_largs, idp);
         ct_arg(f2_largs, idp);
      else
         ct_arg(f1_largs, idp);
      end if;
   end ct_largs;

   procedure ct_arg(p: in out pnode; idp: in idnom) is
      f1_arg: pnode renames p.all.f1_arg;
      f2_arg: pnode renames p.all.f2_arg;
      f3_arg: pnode renames p.all.f3_arg;
      ida: idnom renames f1_arg.all.id;
      idta: idnom renames f3_arg.all.id;
      li: natural renames f3_arg.all.pos_id.li;
      co: natural renames f3_arg.all.pos_id.co;
      li2: natural renames f1_arg.all.pos_id.li;
      co2: natural renames f1_arg.all.pos_id.co;
      m: t_mode;
      dta, da: descripcio;
      e: boolean;
   begin
      ct_mode(f2_arg, m);
      dta := consulta(ts, idta);
      if dta.td /= d_tipus then
         me_noEsTipus(li,co,idta);
         error_ct := true;
      end if;
      case m is
         when m_in => da := (d_argc, idta, 0);
         when m_in_out => da := (d_var, idta, 0);
      end case;
      posaArg(ts, idp, ida, da, e);
      if e then
         me_jaExisteix(li2,co2,ida,"l'argument");
         error_ct := true;
      end if;
   end ct_arg;

   procedure ct_mode(p: in out pnode; mode: out t_mode) is
   begin
      mode := p.all.tm;
   end ct_mode;

   procedure ct_dvar(p: in out pnode) is
      f1_dv: pnode renames p.all.f1_dv;
      f2_dv: pnode renames p.all.f2_dv;
      li: natural renames f2_dv.all.pos_id.li;
      co: natural renames f2_dv.all.pos_id.co;
      idt: idnom;
      dt: descripcio;
   begin
      idt := f2_dv.all.id;
      dt := consulta(ts, idt);
      if dt.td /= d_tipus then
         me_noEsTipus(li,co,idt);
         error_ct := true;
      end if;
      ct_lid_var(f1_dv, idt);
   end ct_dvar;

   procedure ct_lid_var(p: in out pnode; idt: in idnom) is
      f1_lid: pnode renames p.all.f1_lid;
      f2_lid: pnode renames p.all.f2_lid;
      li, co: natural;
      dv, dt: descripcio;
      id: idnom;
      infV: infVar;
      e: boolean;
   begin
      if f2_lid /= null then
         ct_lid_var(f1_lid, idt);
         id := f2_lid.all.id;
         li := f2_lid.all.pos_id.li;
         co := f2_lid.all.pos_id.co;
      else
         id := f1_lid.all.id;
         li := f1_lid.all.pos_id.li;
         co := f1_lid.all.pos_id.co;
      end if;
      nv := nv + 1;
      dv := (d_var, idt, nv);
      posa(ts, id, dv, e);
      if e then
         me_jaExisteix(li,co,id,"la variable");
         error_ct := true;
      end if;
      dt := consulta(ts, idt);
      if dt.td /= d_nula then
         infV := (t_var, id, d_pilaProc.cim(pilaProc), false, dt.dt.ocup, 0);
         tv(nv) := infV;
      end if;
   end ct_lid_var;

   procedure ct_dcons(p: in out pnode) is
      f1_dc: pnode renames p.all.f1_dc;
      f2_dc: pnode renames p.all.f2_dc;
      f3_dc: pnode renames p.all.f3_dc;
      idtc: idnom renames f2_dc.all.id;
      li: natural renames f2_dc.all.pos_id.li;
      co: natural renames f2_dc.all.pos_id.co;
      idt: idnom;
      tsb: tSub;
      vc: valor;
      dc, dtc: descripcio;
   begin
      ct_valor(f3_dc, idt, tsb, vc);
      dtc := consulta(ts, idtc);
      if dtc.td = d_nula then
         me_noExisteixTipusConstant(li, co);
         error_ct := true;
      else
         if dtc.td /= d_tipus then
            me_noEsTipus(li,co,idtc);
      	    error_ct := true;
         end if;
         if dtc.dt.tsb > ts_ent then
            me_constantTsbNoEscalar(li,co,idtc);
      	    error_ct := true;
         end if;
         case idt is
            when null_idnom =>  --Universal
               if tsb /= dtc.dt.tsb then
                  me_constantTipusNoCompatibles(li,co,dtc.dt.tsb,tsb);
                  error_ct := true;
               end if;
            when error_idnom =>  --Necessari quan s'ha produit un error de ct
               null;
            when others =>
               if idt /= idtc then
                  me_constantTipusDistints(li,co,idtc,idt);
                  error_ct := true;
               end if;
         end case;
         ct_lid_cons(f1_dc, idtc, vc);
      end if;
   end ct_dcons;

   procedure ct_valor(p: in out pnode; idt: out idnom; tsb: out tSub; vc: out valor) is
      f_val_pos: pnode renames p.all.f_val_pos;
      f_val_neg: pnode renames p.all.f_val_neg;
      li, co: natural;
   begin
      if f_val_neg = null then
         ct_val_elem(f_val_pos, idt, tsb, vc);
      else
         ct_val_elem(f_val_neg, idt, tsb, vc);
         if f_val_neg.all.f_valelem.all.tnd = nd_id then
            li := f_val_neg.all.f_valelem.all.pos_id.li;
            co := f_val_neg.all.f_valelem.all.pos_id.co;
         else
            li := f_val_neg.all.f_valelem.all.pos_lit.li;
            co := f_val_neg.all.f_valelem.all.pos_lit.co;
         end if;
         if tsb /= ts_ent then me_noEsEnter(li,co,tsb); end if;
         vc := -vc;
      end if;
   end ct_valor;

   procedure ct_val_elem(p: in out pnode; idt: out idnom; tsb: out tSub; vc: out valor)is
      f_valelem: pnode renames p.all.f_valelem;
      dc, dt: descripcio;
      li, co: natural;
   begin
      case f_valelem.all.tnd is
         when nd_lit =>
            idt := null_idnom; --Universal
            tsb := f_valelem.all.tsb;
            vc := f_valelem.all.val_lit;
         when nd_id =>
            li := f_valelem.pos_id.li;
            co := f_valelem.pos_id.co;
            dc := consulta(ts, f_valelem.all.id);
            if dc.td = d_nula then
               me_referenciaNoDeclarada(li, co);
               error_ct := true;
               idt := error_idnom;
            elsif dc.td /= d_const then
               me_noEsConstant(li,co,dc.td);
               error_ct := true;
               idt := error_idnom;
            else
               idt := dc.tc;
               vc := dc.vc;
               dt := consulta(ts, dc.tc);
               tsb := dt.dt.tsb;
            end if;
         when others =>
            raise error_comp_ct;
      end case;
   end ct_val_elem;

   procedure ct_lid_cons(p: in out pnode; idtc: in idnom; vc: in valor) is
      f1_lid: pnode renames p.all.f1_lid;
      f2_lid: pnode renames p.all.f2_lid;
      idc: idnom;
      li, co: natural;
      infC: infVar;
      d, dc, dtc: descripcio;
      e: boolean;
   begin
      if f2_lid /= null then
         ct_lid_cons(f1_lid, idtc, vc);
         idc := f2_lid.all.id;
         li := f2_lid.all.pos_id.li;
         co := f2_lid.all.pos_id.co;
      else
         idc := f1_lid.all.id;
         li := f1_lid.all.pos_id.li;
         co := f1_lid.all.pos_id.co;
      end if;
      nv := nv + 1;
      dc := (d_const, idtc, vc, nv);
      posa(ts, idc, dc, e);
      if e then
         me_jaExisteix(li,co,idc,"la constant");
      	 error_ct := true;
      end if;
      dtc := consulta(ts, idtc);
      if dtc.td /= d_nula then
      	infC := (t_const, idc, vc, dtc.dt.tsb, enula);
        tv(nv) := infC;
      end if;
   end ct_lid_cons;

   procedure ct_subrang(p: in pnode) is
      f1_sub: pnode renames p.all.f1_sub;
      f2_sub: pnode renames p.all.f2_sub;
      f3_sub: pnode renames p.all.f3_sub;
      f4_sub: pnode renames p.all.f4_sub;
      ids: idnom renames f1_sub.all.id;
      idts: idnom renames f2_sub.all.id;
      li: natural renames f2_sub.all.pos_id.li;
      co: natural renames f2_sub.all.pos_id.co;
      idt1, idt2: idnom;
      tsb1, tsb2: tSub;
      vc1, vc2: valor;
      dt, dtsr: descripcio;
      e: boolean;
   begin
      ct_valor(f3_sub, idt1, tsb1, vc1);
      ct_valor(f4_sub, idt2, tsb2, vc2);
      dt := consulta(ts, idts);
      if dt.td /= d_tipus then
         me_noEsTipus(li,co,idts);
      	 error_ct := true;
      end if;
      if dt.dt.tsb > ts_ent then
         me_tsbNoEscalar(li,co,idts);
      	 error_ct := true;
      end if;
      if idt1 = null_idnom then
         if tsb1 /= dt.dt.tsb then
            me_tipusNoCompatibles(li,co,dt.dt.tsb,tsb1);
            error_ct := true;
         end if;
      else
         if idt1 /= idts then
            me_tipusDistints(li,co,idts,idt1);
            error_ct := true;
         end if;
      end if;
      if idt2 = null_idnom then
         if tsb2 /= dt.dt.tsb then
            me_tipusNoCompatibles(li,co,dt.dt.tsb,tsb2);
            error_ct := true;
         end if;
      else
         if idt2 /= idts then
            me_tipusDistints(li,co,idts,idt2);
            error_ct := true;
         end if;
      end if;
      if vc1 > vc2 then
         me_infMajorSup(li,co,idts);
         error_ct := true;
      end if;
      case dt.dt.tsb is
         when ts_ent => dtsr := (d_tipus, dt => (ts_ent,4,vc1,vc2));
         when ts_car => dtsr := (d_tipus, dt => (ts_car,4,vc1,vc2));
         when ts_bool => dtsr := (d_tipus, dt => (ts_bool,4,vc1,vc2));
         when others => raise error_comp_ct;
      end case;
      posa(ts, ids, dtsr, e);
      if e then
         me_jaExisteix(li,co,ids,"el tipus");
      	 error_ct := true;
      end if;
   end ct_subrang;

   procedure ct_registre(p: in pnode) is
      f1_reg: pnode renames p.all.f1_reg;
      f2_reg: pnode renames p.all.f2_reg;
      idr: idnom renames f1_reg.all.id;
      li: natural renames f1_reg.all.pos_id.li;
      co: natural renames f1_reg.all.pos_id.co;
      dr: descripcio;
      ocup: despl;
      e: boolean;
   begin
      dr := (d_tipus, dt => (ts_rec,0));
      posa(ts, idr, dr, e);
      if e then
         me_jaExisteix(li,co,idr,"el tipus");
      	 error_ct := true;
      end if;
      ct_dcmps(f2_reg, idr, ocup);
      dr := (d_tipus, dt => (ts_rec, ocup));
      actualitza(ts, idr, dr);
   end ct_registre;

   procedure ct_dcmps(p: in pnode; idr: in idnom; ocup: out despl) is
      f1_dcmps: pnode renames p.all.f1_dcmps;
      f2_dcmps: pnode renames p.all.f2_dcmps;
      ocup1, ocup2: despl;
   begin
      if f2_dcmps /= null then
         ct_dcmps(f1_dcmps, idr, ocup1);
	 ct_dcmp(f2_dcmps, idr, ocup1, ocup2);
         ocup := ocup1 + ocup2;
      else
         ct_dcmp(f1_dcmps, idr, 0, ocup);
      end if;
   end ct_dcmps;

   procedure ct_dcmp(p: in pnode; idr: in idnom; desp: in despl; ocup: out despl) is
      f1_dcmp: pnode renames p.all.f1_dcmp;
      f2_dcmp: pnode renames p.all.f2_dcmp;
      idc: idnom renames f1_dcmp.all.id;
      idtc: idnom renames f2_dcmp.all.id;
      li: natural renames f1_dcmp.all.pos_id.li;
      co: natural renames f1_dcmp.all.pos_id.co;
      li2: natural renames f2_dcmp.all.pos_id.li;
      co2: natural renames f2_dcmp.all.pos_id.co;
      dtcmp, dcmp: descripcio;
      e: boolean;
   begin
      dtcmp := consulta(ts, idtc);
      if dtcmp.td /= d_tipus then
         me_noEsTipus(li2,co2,idtc);
      	 error_ct := true;
      end if;
      dcmp := (d_camp, idtc, desp);
      posaCamp(ts, idr, idc, dcmp, e);
      if e then
         me_jaExisteix(li,co,idc,"el camp");
      	 error_ct := true;
      end if;
      ocup := dtcmp.dt.ocup;
   end ct_dcmp;

   procedure ct_dcoleccio(p: in out pnode) is
      f1_col: pnode renames p.all.f1_col;
      f2_col: pnode renames p.all.f2_col;
      f3_col: pnode renames p.all.f3_col;
      ida: idnom renames f1_col.all.id;
      idta: idnom renames f3_col.all.id;
      li: natural renames f1_col.all.pos_id.li;
      co: natural renames f1_col.all.pos_id.co;
      li2: natural renames f3_col.all.pos_id.li;
      co2: natural renames f3_col.all.pos_id.co;
      dta, da, di, dti: descripcio;
      nc: despl;
      e: boolean;
   begin
      dta := consulta(ts, idta);
      if dta.td /= d_tipus then
         me_noEsTipus(li2,co2,idta);
      	 error_ct := true;
      end if;
      da := (d_tipus, (ts_arr, 0, idta));
      posa(ts, ida, da ,e);
      if e then
         me_jaExisteix(li,co,ida,"el tipus");
      	 error_ct := true;
      end if;
      ct_lid_col(f2_col, nc, ida);
      da.dt.ocup := nc*dta.dt.ocup;
      actualitza(ts, ida, da);
   end ct_dcoleccio;

   procedure ct_lid_col(p: in out pnode; nc: out despl; ida: in idnom) is
      f1_lid: pnode renames p.all.f1_lid;
      f2_lid: pnode renames p.all.f2_lid;
      li, co: natural;
      idt: idnom;
      di, dindx: descripcio;
   begin
      if f2_lid = null then
         idt := f1_lid.all.id;
         li := f1_lid.all.pos_id.li;
         co := f1_lid.all.pos_id.co;
         nc := 1;
      else
         idt := f2_lid.all.id;
         li := f2_lid.all.pos_id.li;
         co := f2_lid.all.pos_id.co;
         ct_lid_col(f1_lid, nc, ida);
      end if;
      di := consulta(ts, idt);
      if di.td /= d_tipus then
         me_noEsTipus(li,co,idt);
      	 error_ct := true;
      end if;
      if di.dt.tsb > ts_ent then
         me_tsbNoEscalar(li,co,idt);
      	 error_ct := true;
      end if;
      dindx := (d_index, idt);
      posaIndex(ts, ida, dindx);
      nc := nc*despl(di.dt.lsup-di.dt.linf+1);
   end ct_lid_col;

   procedure ct_decls(p: in out pnode) is
      f1_decls: pnode renames p.all.f1_decls;
      f2_decls: pnode renames p.all.f2_decls;
   begin
      if f1_decls /= null then
         ct_decls(f1_decls);
         ct_decl(f2_decls);
      end if;
   end ct_decls;

   procedure ct_decl(p: in out pnode) is
      f_decl: pnode renames p.all.f_decl;
      tnd: tnode renames f_decl.all.tnd;
      dproc_id: idnom;
   begin
      case tnd is
         when nd_dvar =>
            ct_dvar(f_decl);
         when nd_dcons =>
            ct_dcons(f_decl);
         when nd_dtipus =>
            ct_dtipus(f_decl);
         when nd_dproc =>
            ct_dproc(f_decl, dproc_id);
         when others =>
            raise error_comp_ct;
      end case;
   end ct_decl;

   procedure ct_sents(p: in out pnode) is
      f1_sents: pnode renames p.all.f1_sents;
      f2_sents: pnode renames p.all.f2_sents;
   begin
      if p.f2_sents /= null then
         ct_sents(f1_sents);
         ct_sent(f2_sents);
      else
         ct_sent(f1_sents);
      end if;
   end ct_sents;

   procedure ct_sent(p: in out pnode) is
      f_sent: pnode renames p.all.f_sent;
      tnd: tnode renames f_sent.all.tnd;
   begin
      case tnd is
         when nd_if =>
            ct_si(f_sent);
         when nd_it =>
            ct_iteracio(f_sent);
         when nd_assig =>
            ct_assignacio(f_sent);
         when nd_cp =>
            ct_cridaproc(f_sent);
         when others =>
      	    raise error_comp_ct;
      end case;
   end ct_sent;

   procedure ct_dtipus(p:in out pnode) is
      f_dt: pnode renames p.all.f_dt;
      tnd: tnode renames f_dt.all.tnd;
   begin
      case tnd is
         when nd_dsub =>
            ct_subrang(f_dt);
         when nd_dreg =>
            ct_registre(f_dt);
         when nd_dcol =>
            ct_dcoleccio(f_dt);
         when others =>
            raise error_comp_ct;
      end case;
   end ct_dtipus;

   procedure ct_si(p: in out pnode) is
      f1_if: pnode renames p.all.f1_if;
      f2_if: pnode renames p.all.f2_if;
      f3_if: pnode renames p.all.f3_if;
      eidt: idnom;
      etsb: tSub;
      emode: mode_exp;
      li, co: natural;
   begin
      ct_e(f1_if, eidt, etsb, emode, li, co);
      if etsb /= ts_bool then
         me_tsbNoBoolea(li,co,etsb);
      	 error_ct := true;
      end if;
      ct_sents(f2_if);
      if f3_if /= null then
         ct_sents(f3_if);
      end if;
   end ct_si;

   procedure ct_iteracio(p: in out pnode) is
      f1_it: pnode renames p.all.f1_it;
      f2_it: pnode renames p.all.f2_it;
      eidt: idnom;
      etsb: tSub;
      emode: mode_exp;
      li, co: natural;
   begin
      ct_e(f1_it, eidt, etsb, emode, li, co);
      if etsb /= ts_bool then
         me_tsbNoBoolea(li,co,etsb);
      	 error_ct := true;
      end if;
      ct_sents(f2_it);
   end ct_iteracio;

   procedure ct_assignacio(p: in out pnode) is
      f1_assig: pnode renames p.all.f1_assig;
      f2_assig: pnode renames p.all.f2_assig;
      emode: mode_exp;
      rmode: mode_ref;
      eidt, ridt: idnom;
      etsb, rtsb: tSub;
      lir, cor: natural;
      lie, coe: natural;
   begin
      ct_ref(f1_assig, ridt, rmode, rtsb, lir, cor);
      if rtsb = ts_rec then
         me_assignacioTipusEstructurat(lir, cor);
         error_ct := true;
      end if;
      if rmode /= r_var then
          me_noEsVar(lir, cor);
      	  error_ct := true;
      end if;
      ct_e(f2_assig, eidt, etsb, emode, lie, coe);
      if eidt = null_idnom then
         if rtsb /= etsb then
            if etsb /= ts_nul then
               me_variableTipusNoCompatibles(lie,coe,rtsb,etsb);
               error_ct := true;
            end if;
         end if;
      elsif ridt /= eidt then
         if ridt /= error_idnom then
            me_variableTipusDistints(lie,coe,ridt,eidt);
            error_ct := true;
         end if;
      end if;
   end ct_assignacio;

   procedure ct_cridaproc(p: in out pnode) is
      f_cp: pnode renames p.all.f_cp;
      ridt: idnom;
      rtsb: tSub;
      rmode: mode_ref;
      li, co: natural;
   begin
      ct_ref(f_cp, ridt, rmode, rtsb, li, co);
      if rmode /= r_proc then
         me_noEsProc(li,co);
      	 error_ct := true;
      end if;
   end ct_cridaproc;

   procedure ct_e(p: in out pnode; eidt: out idnom; etsb: out tSub; emode: out mode_exp; li, co: out natural) is
      f1_e: pnode renames p.all.f1_e;
      f2_e: pnode renames p.all.f2_e;
      e1idt, e2idt: idnom;
      e1tsb, e2tsb: tSub;
      e1mode, e2mode: mode_exp;
      e1moder: mode_ref;
   begin
      case p.te is
         when e_neg =>
            ct_e(f1_e, e1idt, e1tsb, e1mode, li, co);
            if e1tsb /= ts_ent then
               me_noEsEnter(li,co,e1tsb);
               error_ct := true;
            end if;
            eidt := e1idt;
            etsb := e1tsb;
            emode := e1mode;
         when e_mes =>
            ct_e(f2_e, e2idt, e2tsb, e2mode, li, co);
            ct_e(f1_e, e1idt, e1tsb, e1mode, li, co);
            test_compatibilitat(e1idt, e2idt, e1tsb, e2tsb, eidt, etsb, li, co);
            if etsb /= ts_ent then
               me_operandsNoEnters(li,co,"la suma");
               error_ct := true;
            end if;
            emode := mode_res;
         when e_menys =>
            ct_e(f2_e, e2idt, e2tsb, e2mode, li, co);
            ct_e(f1_e, e1idt, e1tsb, e1mode, li, co);
            test_compatibilitat(e1idt, e2idt, e1tsb, e2tsb, eidt, etsb, li, co);
            if etsb /= ts_ent then
               me_operandsNoEnters(li,co,"la resta");
               error_ct := true;
            end if;
            emode := mode_res;
         when e_prod =>
            ct_e(f2_e, e2idt, e2tsb, e2mode, li, co);
            ct_e(f1_e, e1idt, e1tsb, e1mode, li, co);
            test_compatibilitat(e1idt, e2idt, e1tsb, e2tsb, eidt, etsb, li, co);
            if etsb /= ts_ent then
               me_operandsNoEnters(li,co,"la multiplicació");
               error_ct := true;
            end if;
            emode := mode_res;
         when e_div =>
            ct_e(f2_e, e2idt, e2tsb, e2mode, li, co);
            ct_e(f1_e, e1idt, e1tsb, e1mode, li, co);
            test_compatibilitat(e1idt, e2idt, e1tsb, e2tsb, eidt, etsb, li, co);
            if etsb /= ts_ent then
               me_operandsNoEnters(li,co,"la divisió");
               error_ct := true;
            end if;
            emode := mode_res;
         when e_mod =>
            ct_e(f2_e, e2idt, e2tsb, e2mode, li, co);
            ct_e(f1_e, e1idt, e1tsb, e1mode, li, co);
            test_compatibilitat(e1idt, e2idt, e1tsb, e2tsb, eidt, etsb, li, co);
            if etsb /= ts_ent then
               me_operandsNoEnters(li,co,"el modul");
               error_ct := true;
            end if;
            emode := mode_res;
         when e_no =>
            ct_e(f1_e, e1idt, e1tsb, e1mode, li, co);
            if e1tsb /= ts_bool then
               me_operandsNoBooleans(li,co,"el not");
               error_ct := true;
            end if;
            eidt := e1idt;
            etsb := e1tsb;
            emode := e1mode;
         when e_i =>
            ct_e(f2_e, e2idt, e2tsb, e2mode, li, co);
            ct_e(f1_e, e1idt, e1tsb, e1mode, li, co);
            test_compatibilitat(e1idt, e2idt, e1tsb, e2tsb, eidt, etsb, li, co);
            if etsb /= ts_bool then
               me_operandsNoBooleans(li,co,"l'and");
               error_ct := true;
            end if;
            emode := mode_res;
         when e_o =>
            ct_e(f2_e, e2idt, e2tsb, e2mode, li, co);
            ct_e(f1_e, e1idt, e1tsb, e1mode, li, co);
            test_compatibilitat(e1idt, e2idt, e1tsb, e2tsb, eidt, etsb, li, co);
            if etsb /= ts_bool then
               me_operandsNoBooleans(li,co,"l'or");
               error_ct := true;
            end if;
            emode := mode_res;
         when e_oprel =>
            ct_e(f2_e, e2idt, e2tsb, e2mode, li, co);
            ct_e(f1_e, e1idt, e1tsb, e1mode, li, co);
            test_compatibilitat(e1idt, e2idt, e1tsb, e2tsb, eidt, etsb, li, co);
            eidt := null_idnom;
            etsb := ts_bool;
            emode := mode_res;
         when e_pars =>
            ct_e(f1_e, e1idt, e1tsb, e1mode, li, co);
            eidt := e1idt;
            etsb := e1tsb;
            emode := e1mode;
         when e_lit =>
            eidt := null_idnom;
            etsb := f1_e.all.tsb;
            emode := mode_res;
            li := f1_e.all.pos_lit.li;
            co := f1_e.all.pos_lit.co;
         when e_ref =>
            ct_ref(f1_e, e1idt, e1moder, e1tsb, li, co);
            if (e1moder = r_proc) then
               me_assigProc(li,co);
               error_ct := true;
            end if;
            eidt := e1idt;
            etsb := e1tsb;
            if e1moder = r_var then
               emode := mode_var;
            else
               emode := mode_res;
            end if;
      end case;
      p.all.etsb := etsb; --Necessari per GC
   end ct_e;

   procedure ct_ref(p: in out pnode; ridt: out idnom; rmode: out mode_ref; rtsb: out tSub; li, co: out natural) is
      f1_ref: pnode renames p.all.f1_ref;
      f2_ref: pnode renames p.all.f2_ref;
      d, d1, dt: descripcio;
   begin
      d := consulta(ts, f1_ref.all.id);
      li := f1_ref.all.pos_id.li;
      co := f1_ref.all.pos_id.co;
      if d.td = d_nula then
         me_referenciaNoDeclarada(li, co);
         error_ct := true;
      end if;
      p.all.numP := procNul; --Necessari per GC
      case d.td is
         when d_proc =>
            p.all.esProc := true; --Necessari per GC
            rmode := r_proc;
            ridt := null_idnom;
            rtsb := ts_nul;
            ct_qs_proc(f2_ref, f1_ref.all.id, li, co);
            p.all.numP := d.np; --Necessari per GC
         when d_var =>
            p.all.nv := d.nv; --Necessari per GC
            rmode := r_var;
            ridt := d.tv;
            ct_qs(f2_ref, ridt, li, co);
            if ridt /= error_idnom then
               d1 := consulta(ts, ridt);
               if d1.td = d_tipus then
                  rtsb := d1.dt.tsb;
               else
                  rtsb := ts_nul;
               end if;
            else
               rtsb := ts_nul;
            end if;
         when d_argc =>
            p.all.nv := d.na; --Necessari per GC
            rmode := r_cons;
            ridt := d.ta;
            ct_qs(f2_ref, ridt, li, co);
            d1 := consulta(ts, ridt);
            if d1.td = d_tipus then
               rtsb := d1.dt.tsb;
            else
               rtsb := ts_nul;
            end if;
         when d_const =>
            p.all.nv := d.nc; --Necessari per GC
            rmode := r_cons;
            ridt := d.tc;
            d1 := consulta(ts, d.tc);
            if d1.td = d_tipus then
               rtsb := d1.dt.tsb;
            else
               rtsb := ts_nul;
            end if;
            if f2_ref.all.f1_qs /= null then
               me_constantEsEscalar(li,co);
               error_ct := true;
            end if;
         when others =>
            ridt := error_idnom; --Necessari per evitar problemes
      end case;
   end ct_ref;

   procedure ct_qs_proc(p: in out pnode; idp: in idnom; li, co: in natural) is
      f1_qs: pnode renames p.all.f1_qs;
      f2_qs: pnode renames p.all.f2_qs;
      it: iterator_arg;
   begin
      it:= primer(ts, idp);
      if esValid(it) then    --Requereix parametres
         if f2_qs = null then
            me_procRequereixParametres(li, co);
            error_ct := true;
         else
            ct_lexps_proc(f2_qs.all.f_q, idp, it, li, co);
            if esValid(it) then
               me_procFaltenParametres(li, co);
               error_ct := true;
            end if;
         end if;
      else
         if f2_qs /= null then
            me_procMassaParametres(li, co);
            error_ct := true;
         end if;
      end if;
   end ct_qs_proc;

   procedure ct_lexps_proc(p: in out pnode; idp: in idnom; it: in out iterator_arg; li, co: in natural) is
      f1_lexps: pnode renames p.all.f1_lexps;
      f2_lexps: pnode renames p.all.f2_lexps;
      eidt, ida: idnom;
      etsb: tSub;
      emode: mode_exp;
      da, dta: descripcio;
      lie, coe: natural;
      fill: natural;
   begin
      if f2_lexps /= null then
         ct_lexps_proc(f1_lexps, idp, it, li, co);
         ct_e(f2_lexps, eidt, etsb, emode, lie, coe);
         fill := 2;
      else
         ct_e(f1_lexps, eidt, etsb, emode, lie, coe);
         fill := 1;
      end if;
      if not esValid(it) then
         me_procMassaParametres(li, co);
         error_ct := true;
      end if;
      consulta(ts, it, ida, da);
      if da.td = d_var then
         if fill = 1 then
            f1_lexps.param_out := true; --Necessari per GC
         else
            f2_lexps.param_out := true; --Necessari per GC
         end if;
      end if;
      if eidt = null_idnom then
         if da.td = d_var then
            dta := consulta(ts, da.tv);
         elsif da.td = d_argc then
            dta := consulta(ts, da.ta);
         end if;
         if dta.td /= d_nula then
            if dta.dt.tsb /= etsb then
               me_procTipusNoCompatibles(lie, coe, dta.dt.tsb, etsb);
               error_ct := true;
            end if;
         end if;
      else
         if da.td = d_argc then
            if da.ta /= eidt then
            	me_procTipusDistints(lie, coe, da.ta, eidt);
            	error_ct := true;
            end if;
         else
            if da.td /= d_nula then
               if da.tv /= eidt then
            	  me_procTipusDistints(lie, coe, da.tv, eidt);
                  error_ct := true;
               end if;
            end if;
	end if;
      end if;
      if da.td = d_var then
         if emode = mode_res then
            me_modeParametreIncorrecte(lie, coe);
            error_ct := true;
         end if;
      end if;
      it := successor(ts, it);
   end ct_lexps_proc;

   procedure ct_qs(p: in out pnode; idt: in out idnom; li, co: in natural) is
      f1_qs: pnode renames p.all.f1_qs;
      f2_qs: pnode renames p.all.f2_qs;
   begin
      if f1_qs /= null then
         ct_qs(f1_qs, idt, li, co);
         ct_q(f2_qs, idt, li, co);
      end if;
   end ct_qs;

   procedure ct_q (p: in out pnode; idt: in out idnom; li, co: in natural) is
      f_q: pnode renames p.all.f_q;
      dt, di, dti, dc: descripcio;
      it: iterator_indx;
      aux: valor;
      ida: idnom;
   begin
      case f_q.all.tnd is
         when nd_id =>
              ct_camp_q(p, idt, li, co);
         when nd_lexps =>
            dt := consulta(ts, idt);
            if dt.td = d_tipus and then dt.dt.tsb /= ts_arr then
               me_noEsArray(li, co);
               idt := error_idnom;
               error_ct := true;
            end if;
            if dt.td = d_tipus and then idt /= error_idnom then
               it := primer(ts, idt);
               ida := idt;
               idt := dt.dt.tc;
               ct_lexps_q(f_q, it, li, co);
               if esValid(it) then
                  me_faltenIndexosArray(li, co);
                  error_ct := true;
               end if;
               it := primer(ts, ida);
               consulta(ts, it, di);
               dti := consulta(ts, di.tindx);
               aux := dti.dt.linf;
               it := successor(ts, it);
               while(esValid(it)) loop
                  consulta(ts, it, di);
                  it := successor(ts, it);
                  dti := consulta(ts, di.tindx);
                  aux := aux * (dti.dt.lsup - dti.dt.linf + 1) + dti.dt.linf;
               end loop;
               p.all.b := aux; -- Necessari per GC
               dc := consulta(ts, dt.dt.tc);
               if dc.td /= d_nula then
               	  p.all.w := dc.dt.ocup; --Necessari per GC
               end if;
            end if;
         when others =>
            raise error_comp_ct;
      end case;
   end ct_q;

   procedure ct_camp_q(p: in out pnode; idt: in out idnom; li, co: in natural) is
      f_q: pnode renames p.all.f_q;
      dt, dc: descripcio;
   begin
      dt := consulta(ts, idt);
      if dt.dt.tsb /= ts_rec then
         me_noEsRecord(li, co);
         error_ct := true;
      end if;
      dc := consulta(ts, idt, f_q.all.id);
      if dc.td = d_nula then
         me_noExisteixAquestCamp(f_q.all.pos_id.li, f_q.all.pos_id.co);
         error_ct := true;
         idt := error_idnom;
      else
         idt := dc.tcmp;
         p.all.desp := dc.dcmp; --Necessari per GC
      end if;
   end ct_camp_q;

   procedure ct_lexps_q(p: in out pnode; it: in out iterator_indx; li, co: in natural) is
      f1_lexps: pnode renames p.all.f1_lexps;
      f2_lexps: pnode renames p.all.f2_lexps;
      di, dt, dti: descripcio;
      eidt: idnom;
      etsb: tSub;
      emode: mode_exp;
      lie, coe: natural;
   begin
      if f2_lexps /= null then
         ct_lexps_q(f1_lexps, it, li, co);
         ct_e(f2_lexps, eidt, etsb, emode, lie, coe);
      else
         ct_e(f1_lexps, eidt, etsb, emode, lie, coe);
      end if;
      if not esValid(it) then
         me_massaIndexosArray(li, co);
         error_ct := true;
         eidt := error_idnom;
      end if;
      consulta(ts, it, di);
      case eidt is
         when error_idnom => --Quan s'ha produit un error previament i no tenim id
            null;
         when null_idnom =>
            dti := consulta(ts, di.tindx);
            if dti.dt.tsb /= etsb then
               me_indexTSubIncorrecte(lie, coe, dti.dt.tsb, etsb);
               error_ct := true;
            end if;
            if f2_lexps /= null then
            	f2_lexps.all.n := dti.dt.lsup - dti.dt.linf + 1; --Necessari per GC
            end if;
         when others =>
            if di.tindx /= eidt then
               me_indexTipusIncorrecte(lie, coe, di.tindx, eidt);
               error_ct := true;
            end if;
            if f2_lexps /= null then
            	dt := consulta(ts, eidt);
            	f2_lexps.all.n := dt.dt.lsup - dt.dt.linf + 1; --Necessari per GC
            end if;
      end case;
      it:= successor(ts, it);
   end ct_lexps_q;

end semantica.comprovacio_tipus;
