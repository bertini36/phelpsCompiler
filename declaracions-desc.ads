package declaracions.desc is
   
   type tSub is (ts_bool, ts_car, ts_ent, ts_rec, ts_arr, ts_nul);
	  
   type descrTipus (tsb: tSub := ts_nul) is
      record
         ocup: despl;
         case tsb is
            when ts_bool | ts_car | ts_ent =>
               linf, lsup: valor;
            when ts_arr =>
               tc: idnom;
            when ts_rec | ts_nul =>
               null;
         end case;
      end record;
   
   type tDescr is (d_var, d_const, d_tipus, d_proc, d_argc, d_camp, d_index, d_nula);

   type descripcio (td: tDescr := d_nula) is
      record
         case td is
            when d_var =>
               tv: idnom;
               nv: numVar;
            when d_const =>
               tc: idnom;
               vc: valor;
               nc: numVar;
            when d_tipus =>
               dt: descrTipus;
            when d_proc =>
               np: numProc;
            when d_argc =>
               ta: idnom;
               na: numVar;
            when d_camp =>
               tcmp: idnom;
               dcmp: despl;
            when d_index =>
               tindx: idnom;
            when d_nula =>
               null;
         end case;
      end record;

end declaracions.desc;
