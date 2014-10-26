with declaracions, declaracions.desc, declaracions.arbre,
     declaracions.t_simbols, declaracions.c3a, semantica.missatges,
     phelps_tokens;

use declaracions, declaracions.desc, declaracions.arbre,
    declaracions.t_simbols, declaracions.c3a, semantica.missatges,
    phelps_tokens;

package semantica.comprovacio_tipus is

   procedure prepara;
   procedure compr_tipus;

   error_compr_tipus: exception;
   error_comp_ct: exception; --Error del compilador

private

   error_ct: boolean; --Error durant la comprovació de tipus

end semantica.comprovacio_tipus;
