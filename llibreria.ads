with Ada.Sequential_IO, declaracions, declaracions.c3a, d_pila;
use declaracions, declaracions.c3a;

package llibreria is

   package Seq_IO is new Ada.Sequential_IO(instr3a);
   package d_pilaEtiq is new d_pila(etiqueta);
   package d_pilaPars is new d_pila(infPar);
   package d_pilaProc is new d_pila(numProc);

end llibreria;
