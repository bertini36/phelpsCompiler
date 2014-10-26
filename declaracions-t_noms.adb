with Ada.Strings.Hash;
use Ada.Strings;

package body declaracions.t_noms is

   procedure tbuida(tn: out tnoms) is
      tdn: disp_table renames tn.tdn;
      tnid: idnom_table renames tn.tnid;
      tsid: idstring_table renames tn.tsid;
      nid: idnom renames tn.nid;
      sid: idstring renames tn.sid;
      nc: natural renames tn.nc;
      nes: natural renames tn.nes;
   begin
      for i in hash_index loop
         tdn(i) := null_idnom;
      end loop;
      nid := 0;
      sid := 0;
      nc := 0;
      nes := max_ch+1;
      tnid(null_idnom) := (null_idnom, nc);
      tsid(null_idstring) := nes;
   end tbuida;

   procedure posaNom(tn: in out tnoms; nom: in string; id: out idnom) is
      tdn: disp_table renames tn.tdn;
      tnid: idnom_table renames tn.tnid;
      tc: characters_table renames tn.tc;
      nid: idnom renames tn.nid;
      nc: natural renames tn.nc;
      nes: natural renames tn.nes;
      i: hash_type;
      p: idnom;

      function equal(nom: in string; tn: in tnoms; p: in idnom) return boolean is
         tnid: idnom_table renames tn.tnid;
         tc: characters_table renames tn.tc;
         pi, pf: natural;
         i, j: natural;
      begin
         pi := tnid(p-1).ptc+1; pf := tnid(p).ptc;
         i := nom'First; j := pi;
         while nom(i) = tc(j) and i < nom'Last and j < pf loop
            i := i + 1; j := j + 1;
      	 end loop;
      	 return nom(i) = tc(j) and i = nom'Last and j = pf;
      end equal;

      procedure save_name(nom: in string; tc: in out characters_table; nc: in out natural) is
      begin
         for i in nom'Range loop
            nc := nc + 1; tc(nc) := nom(i);
         end loop;
      end save_name;

   begin
      i := hash(nom) mod b; p := tdn(i);
      while p /= null_idnom and then not equal(nom, tn, p) loop
         p := tnid(p).psh;
      end loop;
      if p = null_idnom then
         if nid = idnom(max_noms) then raise desb_memoria; end if;
         if nc + nom'Length > nes - 1 then raise desb_memoria; end if;
         save_name(nom, tc, nc);
         nid := nid + 1; tnid(nid) := (tdn(i), nc);
         tdn(i) := nid; p := nid;
      end if;
      id := p;
   end posaNom;

   function consultaNom(tn: in tnoms; id: in idnom) return string is
      tnid: idnom_table renames tn.tnid;
      tc: characters_table renames tn.tc;
      nid: idnom renames tn.nid;
      pi, pf: natural;
   begin
      if id = null_idnom or id > nid then raise mal_us; end if;
      pi := tnid(id-1).ptc + 1;
      pf := tnid(id).ptc;
      return tc(pi..pf);
   end consultaNom;

   procedure posaString(tn: in out tnoms; s: in string; ids: out idstring) is
      tsid: idstring_table renames tn.tsid;
      tc: characters_table renames tn.tc;
      sid: idstring renames tn.sid;
      nc: natural renames tn.nc;
      nes: natural renames tn.nes;

      procedure save_name(s: in string; tc: in out characters_table; nes: in out natural) is
      begin
         for i in s'Range loop
            nes := nes-1; tc(nes) := s(s'Last-(i-1));
         end loop;
      end save_name;

   begin
      if sid = idstring(max_str) then raise desb_memoria; end if;
      if nc + s'Length > nes - 1 then raise desb_memoria; end if;
      save_name(s, tc, nes);
      sid := sid + 1; tsid(sid) := nes;
      ids := sid;
   end posaString;

   function consultaString(tn: in tnoms; ids: in idstring) return string is
      tsid: idstring_table renames tn.tsid;
      tc: characters_table renames tn.tc;
      sid: idstring renames tn.sid;
      pi, pf: natural;
   begin
      if ids = null_idstring or ids > sid then raise mal_us; end if;
      pi := tsid(ids);
      pf := tsid(ids-1) - 1;
      return tc(pi..pf);
   end consultaString;

end declaracions.t_noms;
