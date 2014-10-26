with Ada.Sequential_IO, Ada.Text_IO, d_pila, declaracions.c3a,
     declaracions, declaracions.arbre, declaracions.desc,
     declaracions.t_noms, semantica.missatges, llibreria;

use declaracions.c3a, declaracions, declaracions.arbre, declaracions.desc,
    declaracions.t_noms, semantica.missatges, llibreria;

package semantica.generacio_codi_intermig is

   procedure prepara(nom: in string);
   procedure conclou;
   procedure gen_codi_intermig;

   error_gen_codi_int: exception;

private

   file: seq_IO.File_Type;
   llegible: Ada.Text_IO.File_Type;

   --Piles necessaries
   pcert: d_pilaEtiq.pila;
   pfals: d_pilaEtiq.pila;
   pilaPars: d_pilaPars.pila;

end semantica.generacio_codi_intermig;
