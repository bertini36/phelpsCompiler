with Ada.text_io, analitzador_sintactic, phelps_tokens, declaracions.arbre,
     phelps_io, semantica, semantica.construccio_arbre,
     semantica.comprovacio_tipus, semantica.missatges,
     semantica.generacio_codi_intermig, Ada.Command_Line, lectura_fitxer,
     phelps_error_report, semantica.generacio_codi;

use Ada.text_io, analitzador_sintactic, phelps_tokens, declaracions.arbre,
    phelps_io, semantica, semantica.construccio_arbre,
    semantica.comprovacio_tipus, semantica.missatges,
    semantica.generacio_codi_intermig, Ada.Command_Line, lectura_fitxer,
    semantica.generacio_codi;

procedure phelps is
begin
   llegeixNom(Argument(1));
   prepara_analisi(Argument(1));
   analisi_sintactic(Argument(1));
   compr_tipus;
   gen_codi_intermig;
   gen_codi(Argument(1));
   conclou_analisi;
exception
   when nom_incorrecte => mg_nomFitxerIncorrecte;
   when phelps_error_report.syntax_error => mg_errorSintactic;
   when error_comp_ct => mg_errorCompiladorComprTipus;
   when error_compr_tipus => mg_errorComprTipus;
   when error_gen_codi_int => mg_errorCodiIntermig;
   when error_gen_codi => mg_errorCodiAssemblador;
end phelps;
