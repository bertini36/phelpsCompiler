with llibreria, Ada.Text_IO, declaracions.c3a, declaracions, semantica,
     semantica.generacio_codi_intermig, declaracions.desc, semantica.missatges;

use llibreria, Ada.Text_IO, declaracions.c3a, declaracions, semantica,
    semantica.generacio_codi_intermig, declaracions.desc, semantica.missatges;

package semantica.generacio_codi is

   procedure prepara(nom: in string);
   procedure conclou;
   procedure gen_codi(fname: in string);

   error_gen_codi: exception;

private

   file: seq_IO.File_Type;                  --Codi intermig
   assemblador: Ada.Text_IO.File_Type;      --Codi assemblador
   pProc: d_pilaProc.pila;                  --Pila de procediments

end semantica.generacio_codi;
