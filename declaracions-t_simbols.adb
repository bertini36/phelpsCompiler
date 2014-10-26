package body declaracions.t_simbols is

   procedure tbuida(ts: out tsimbols) is
      td: table_d renames ts.td;
      prof: prof_real renames ts.prof;
      ta: table_a renames ts.ta;
      d: descripcio;
   begin
      for id in idnom loop td(id) := (0, d, 0); end loop;
      prof := 1;
      ta(prof) := 0;
   end tbuida;

   procedure posa(ts: in out tsimbols; id: in idnom; d: in descripcio; e: out boolean) is
      td: table_d renames ts.td;
      te: table_e renames ts.te;
      ta: table_a renames ts.ta;
      prof: prof_real renames ts.prof;
      ie: indx_exp;
   begin
      if td(id).prof = prof then raise error_p; end if; --Error si estas intentant posar una amb el mateix nom a la mateixa profunditat
      ie := ta(prof); ie := ie+1; ta(prof) := ie; --Ocupació te y registre ta
      te(ie).prof := td(id).prof; te(ie).d := td(id).d; te(ie).id := id; --Trasllat de td a te
      td(id).prof := prof; td(id).d := d; --Inclusio en td de la nova descripció
      e := false;
   exception
      when error_p => e := true;
   end posa;

   function consulta(ts: in tsimbols; id: in idnom) return descripcio is
      td: table_d renames ts.td;
   begin
      return td(id).d;
   end consulta;

   procedure entraBloc(ts: in out tsimbols) is
      ta: table_a renames ts.ta;
      prof: prof_real renames ts.prof;
   begin
      prof := prof + 1;
      ta(prof) := ta(prof-1);
   end entraBloc;

   procedure surtBloc(ts: in out tsimbols) is
      td: table_d renames ts.td;
      te: table_e renames ts.te;
      ta: table_a renames ts.ta;
      prof: prof_real renames ts.prof;
      ie, nf: indx_exp;
      id: idnom;
   begin
      ie := ta(prof); prof := prof - 1; nf := ta(prof);
      while ie > nf loop
         if te(ie).prof /= -1 then
            id := te(ie).id;
            td(id).prof := te(ie).prof;
            td(id).d := te(ie).d;
            td(id).s := te(ie).s;
         end if;
         ie := ie-1;
      end loop;
   end surtBloc;

   procedure actualitza(ts: in out tsimbols; id: in idnom; d: in descripcio) is
      td: table_d renames ts.td;
   begin
      td(id).d := d;
   end actualitza;

   procedure posaCamp(ts: in out tsimbols; idr: in idnom; idc: in idnom; dc: in descripcio; e: out boolean) is
      td: table_d renames ts.td;
      te: table_e renames ts.te;
      ta: table_a renames ts.ta;
      prof: prof_real renames ts.prof;
      d: descripcio;
      p: indx_exp;
   begin
      d := td(idr).d;
      if d.td /= d_tipus and then d.dt.tsb /= ts_rec then raise error_c; end if; --Comprovam que idr correspon a un tipus record
      p := td(idr).s;
      while p /= 0 and then te(p).id /= idc loop --Comprovar que no hi ha cap altre camp amb el mateix nom
         p := te(p).s;
      end loop;
      if p /= 0 then raise error_p; end if; --Vol dir que ha trobat un camp amb el mateix nom que el que volem inserir
      p := ta(prof); p := p + 1; ta(prof) := p; --Reserva de memoria per el nou camp
      te(p).id := idc; te(p).prof := - 1; te(p).d := dc; --Guarda la informació
      te(p).s := td(idr).s; td(idr).s := p; --Inserció a la llista
      e := false;
   exception
      when error_p => e := true;
   end posaCamp;

   function consulta(ts: in tsimbols; idr: in idnom; idc: in idnom) return descripcio is
      td: table_d renames ts.td;
      te: table_e renames ts.te;
      d, dc: descripcio;
      p: indx_exp;
   begin
      d := td(idr).d;
      if d.td /= d_tipus and then d.dt.tsb /= ts_rec then raise error_c; end if; --Primer miram que sigui un tipus record
      p := td(idr).s; --Agafa el primer dels seus camps
      while p /= 0 and then te(p).id /= idc loop
         p := te(p).s;
      end loop;
      if p = 0 then --No l'hem trobat
         dc := (td => d_nula);
      else
         dc := te(p).d;
      end if;
      return dc;
   end consulta;

   procedure posaArg(ts: in out tsimbols; idp: in idnom; ida: in idnom; da: in descripcio; e: out boolean) is
      td: table_d renames ts.td;
      te: table_e renames ts.te;
      ta: table_a renames ts.ta;
      prof: prof_real renames ts.prof;
      d: descripcio;
      p, pp: indx_exp;
   begin
      d := td(idp).d;
      if d.td /= d_proc then raise error_c; end if;
      p := td(idp).s; pp := 0;
      while p /= 0 and then te(p).id /= ida loop
         pp := p; p := te(p).s;
      end loop;
      if p /= 0 then raise error_p; end if;
      p := ta(prof); p := p+1; ta(prof) := p; --Reserva d'espai
      te(p).id := ida; te(p).prof := -1; te(p).d := da; --Gurdar-hi info
      if pp = 0 then --Inserir-ho a la llista
         td(idp).s := p;
      else
         te(pp).s := p;
      end if;
      te(p).s := 0;
      e := false;
   exception
      when error_p => e := true;
   end posaArg;

   function primer(ts: in tsimbols; idp: idnom) return iterator_arg is
      td: table_d renames ts.td;
      it: iterator_arg;
   begin
      it.s := td(idp).s;
      return it;
   end primer;

   function successor(ts: in tsimbols; it: in iterator_arg) return iterator_arg is
      te: table_e renames ts.te;
      sit: iterator_arg;
   begin
      sit.s := te(it.s).s;
      return sit;
   end successor;

   function esValid(it: in iterator_arg) return boolean is
   begin
      return it.s /= 0;
   end esValid;

   procedure consulta(ts: in tsimbols; it: in iterator_arg; ida: out idnom; da: out descripcio) is
      te: table_e renames ts.te;
   begin
      ida := te(it.s).id;
      da := te(it.s).d;
   end consulta;

   procedure posaIndex(ts: in out tsimbols; ida: in idnom; dindx: in descripcio) is
      td: table_d renames ts.td;
      te: table_e renames ts.te;
      ta: table_a renames ts.ta;
      prof: prof_real renames ts.prof;
      d: descripcio;
      p, pp: indx_exp;
   begin
      d := td(ida).d;
      if d.td /= d_tipus and then d.dt.tsb /= ts_arr then raise error_c; end if;
      pp := 0; p := td(ida).s; --Ens situam a la fi de la llista
      while p /= 0 loop
         pp := p; p := te(p).s;
      end loop;
      p := ta(prof); p := p + 1; ta(prof) := p;
      te(p).id := null_idnom; te(p).prof := -1; te(p).d := dindx;
      if pp = 0 then
         td(ida).s := p;
      else
         te(pp).s := p;
      end if;
      te(p).s := 0;
   end posaIndex;

   function primer (ts: in tsimbols; ida: in idnom) return iterator_indx is
      td: table_d renames ts.td;
      it: iterator_indx;
   begin
      it.s := td(ida).s;
      return it;
   end primer;

   function successor (ts: in tsimbols; it: in iterator_indx) return iterator_indx is
      te: table_e renames ts.te;
      sit: iterator_indx;
   begin
      sit.s := te(it.s).s;
      return sit;
   end successor;

   function esValid (it: in iterator_indx) return boolean is
   begin
      return it.s /= 0;
   end esValid;

   procedure consulta (ts: in tsimbols; it: in iterator_indx; di: out descripcio) is
      te: table_e renames ts.te;
   begin
      di := te(it.s).d;
   end consulta;

end declaracions.t_simbols;
