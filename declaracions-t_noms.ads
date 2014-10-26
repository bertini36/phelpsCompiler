with Ada.Containers;
use Ada.Containers;

package declaracions.t_noms is

   max_ch: constant integer := max_noms * long_mitja_nom
                             + max_str * long_mitja_str;

   type tnoms is limited private;

   procedure tbuida(tn: out tnoms);

   procedure posaNom(tn: in out tnoms; nom: in string; id: out idnom);
   function consultaNom(tn: in tnoms; id: in idnom) return string;

   procedure posaString(tn: in out tnoms; s: in string; ids: out idstring);
   function consultaString(tn: in tnoms; ids: in idstring) return string;

   desb_memoria: exception;
   mal_us: exception;

private

   b: constant Hash_type := Hash_Type(max_noms);
   subtype hash_index is Hash_Type range 0..b-1;

   type list_item is
      record
         psh: idnom;
         ptc: natural;
      end record;

   type idnom_table is array(idnom) of list_item;
   type idstring_table is array(idstring) of natural;

   type disp_table is array(hash_index) of idnom;

   subtype characters_table is string(1..max_ch);

   type tnoms is
      record
         tdn: disp_table;
         tnid: idnom_table;
         tsid: idstring_table;
         tc: characters_table;
         nid: idnom;
         sid: idstring;
         nc: natural;
         nes: natural;
      end record;

end declaracions.t_noms;
