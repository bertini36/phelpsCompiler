with Ada.Text_IO, Ada.Containers, declaracions.desc;
use Ada.Text_IO, Ada.Containers, declaracions.desc;

package declaracions.t_simbols is

   type tsimbols is limited private;

   --Accions i funcions generals
   procedure tbuida(ts: out tsimbols);
   procedure posa(ts: in out tsimbols; id: in idnom; d: in descripcio; e: out boolean);
   function consulta(ts: in tsimbols; id: in idnom) return descripcio;
   procedure entraBloc(ts: in out tsimbols);
   procedure surtBloc(ts: in out tsimbols);
   procedure actualitza(ts: in out tsimbols; id: in idnom; d: in descripcio);

   --Records. No requereix iterador.
   procedure posaCamp(ts: in out tsimbols; idr: in idnom; idc: in idnom; dc: in descripcio; e:out boolean);
   function consulta(ts: in tsimbols; idr: in idnom; idc: in idnom) return descripcio;

   --Procedures
   procedure posaArg(ts: in out tsimbols; idp: in idnom; ida: in idnom; da: in descripcio; e: out boolean);

   type iterator_arg is private;
   function primer(ts: in tsimbols; idp: in idnom) return iterator_arg;
   function successor(ts: in tsimbols; it: in iterator_arg) return iterator_arg;
   function esValid(it: in iterator_arg) return boolean;
   procedure consulta(ts: in tsimbols; it: in iterator_arg; ida: out idnom; da: out descripcio);

   --Arrays
   procedure posaIndex(ts: in out tsimbols; ida: in idnom; dindx: in descripcio);

   type iterator_indx is private;
   function primer(ts: in tsimbols; ida: in idnom) return iterator_indx;
   function successor(ts: in tsimbols; it: in iterator_indx) return iterator_indx;
   function esValid(it: in iterator_indx) return boolean;
   procedure consulta(ts: in tsimbols; it: in iterator_indx; di: out descripcio);

   space_overflow: exception;
   bad_use: exception;
   error_p: exception; --Error del programador
   error_c: exception; --Error del compilador

private

   max_exp: constant integer := max_noms * max_prof;

   type profunditat is new integer range -1..max_prof;
   subtype prof_real is profunditat range profunditat'First+2..profunditat'Last;

   type indx_exp is new integer range 0..max_exp;

   type node_td is
      record
         prof: profunditat;
         d: descripcio;
         s: indx_exp;
      end record;

   type table_d is array(idnom) of node_td;

   type node_te is
      record
         id: idnom;
         prof: profunditat;
         d: descripcio;
         s: indx_exp;
      end record;

   type table_e is array(indx_exp) of node_te;

   type table_a is array(prof_real) of indx_exp;

   type tsimbols is
      record
         td: table_d;
         te: table_e;
         ta: table_a;
         prof: prof_real; --Profunditat actual
      end record;

   type iterator_arg is
      record
         s: indx_exp;
      end record;

   type iterator_indx is
      record
         s: indx_exp;
      end record;

end declaracions.t_simbols;
