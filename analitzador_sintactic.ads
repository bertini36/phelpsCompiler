with text_io, phelps_tokens, phelps_goto, phelps_shift_reduce,
     phelps_error_report, analitzador_lexic, phelps_io, declaracions.arbre,
     semantica.construccio_arbre, semantica.missatges;

use phelps_tokens, phelps_goto, phelps_shift_reduce, phelps_error_report,
    analitzador_lexic, phelps_io, declaracions.arbre,
    semantica.construccio_arbre, semantica.missatges;

package analitzador_sintactic is
   procedure analisi_sintactic(fname: in string);
end analitzador_sintactic;