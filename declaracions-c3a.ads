with declaracions.desc;
use declaracions.desc;

package declaracions.c3a is

   --Taula de procediments
   type tProc is (p_intern, p_extern);

   type infProc(tp: tProc := p_intern) is record
      idProc: idnom;
      case tp is
         when p_intern =>
            prof: natural;
            ocupVL: despl;
            e: etiqueta;
            numParam: natural;
            principal: boolean;
         when p_extern =>
            null;
      end case;
   end record;

   type taulaProc is array (numProc) of infProc;

   --Taula de variables
   type tVar is (t_var, t_const);

   type infVar(tv: tVar := t_var) is record
      idvar: idnom; --Info per depurar i per distingir variables GC
      case tv is
         when t_var =>
            np: numProc;
            esParam: boolean;
            ocup: despl;
            desp: despl;
         when t_const =>
            val: valor;
            tsb: tSub;
            etiq: etiqueta;  --Per a la generació de codi assemblador
      end case;
   end record;

   type taulaVar is array (numVar) of infVar;

   type infPar is
      record
         r, d: numVar;
      end record;

   --Codi 3@
   type op_3a is (op3_noop, op3_copia, op3_consIndx, op3_modIndx, op3_suma,
                  op3_resta, op3_prod, op3_div, op3_mod, op3_and, op3_or,
                  op3_menysU, op3_not, op3_etiq, op3_goto, op3_lt, op3_le,
                  op3_ig, op3_dif, op3_ge, op3_gt, op3_pmb, op3_rtn, op3_call,
                  op3_params, op3_paramc);

   type instr3a(op: op_3a := op3_noop) is record
      case op is
         when op3_copia | op3_menysU | op3_not =>
            a1, b1: numVar;
         when op3_consIndx | op3_modIndx | op3_suma | op3_resta | op3_prod
              | op3_div | op3_mod | op3_and | op3_or =>
            a2, b2, c2: numVar;
         when op3_etiq | op3_goto =>
            e1: etiqueta;
         when op3_lt | op3_le | op3_ig | op3_dif | op3_ge | op3_gt =>
            e2: etiqueta;
            a3, b3: numVar;
         when op3_pmb | op3_rtn | op3_call =>
            np: numProc;
         when op3_params =>
            a4: numVar;
         when op3_paramc =>
            a5, b5: numVar;
         when op3_noop =>
            null;
      end case;
   end record;

end declaracions.c3a;
