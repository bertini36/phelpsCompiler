with semantica.comprovacio_tipus, semantica.generacio_codi_intermig,
     semantica.missatges, semantica.generacio_codi;

package body semantica is

   procedure prepara_analisi(fname: in string) is
      nom: string(fname'First..fname'Last-4);
   begin
      tbuida(tn);
      tbuida(ts);
      nv := 0;
      np := 0;
      ne := 0;
      nom := fname(fname'First..fname'Last-4);
      comprovacio_tipus.prepara;
      generacio_codi_intermig.prepara(nom);
      generacio_codi.prepara(nom);
      missatges.prepara(nom);
   end prepara_analisi;

   procedure conclou_analisi is
   begin
      generacio_codi.conclou;
      missatges.conclou;
   end conclou_analisi;

end semantica;
