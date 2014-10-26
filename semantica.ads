with declaracions, declaracions.t_noms, declaracions.t_simbols, 
     declaracions.arbre, declaracions.c3a, llibreria;

use declaracions, declaracions.t_noms, declaracions.t_simbols, 
    declaracions.arbre, declaracions.c3a, llibreria;

package semantica is

   procedure prepara_analisi(fname: in string);
   procedure conclou_analisi;

   pilaProc: d_pilaProc.pila;
   
private
     
   arrel: pnode;
   
   tn: tnoms;
   ts: tsimbols;
   
   nv: numVar;
   np: numProc;
   ne: etiqueta;
   
   tp: taulaProc;
   tv: taulaVar;
   
end semantica;
