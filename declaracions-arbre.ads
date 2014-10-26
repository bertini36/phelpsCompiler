with declaracions.desc;
use declaracions.desc;

package declaracions.arbre is

   type node;
   type pnode is access node;
   type tnode is (nd_prog, nd_id, nd_lit, nd_encap, nd_args, nd_largs,
                  nd_arg, nd_mode, nd_decls, nd_decl, nd_dvar, nd_dcons,
                  nd_dtipus, nd_dproc, nd_lid, nd_val, nd_valelem, nd_dsub,
                  nd_dreg, nd_dcol, nd_dcmps, nd_dcmp, nd_sents, nd_sent,
                  nd_if, nd_it, nd_assig, nd_cp, nd_ref, nd_e, nd_qs, nd_q,
                  nd_lexps, nd_atom, nd_oprel);

   type t_mode is (m_in, m_in_out);

   type t_exp is (e_neg, e_mes, e_menys, e_prod, e_div, e_mod, e_no,
                  e_i, e_o, e_oprel, e_pars, e_lit, e_ref);

   type op_oprel is (op_igual, op_diferent, op_major, op_menor, op_majorigual,
                     op_menorigual);

   type posicio is
      record
         li, co: natural;
      end record;

   type node (tnd: tnode) is record
      case tnd is
         when nd_prog => f_prog: pnode;

         when nd_dproc => f1_dp: pnode;
            		  f2_dp: pnode;
                          f3_dp: pnode;
            		  f4_dp: pnode;

         when nd_encap => f1_encap: pnode;
                          f2_encap: pnode;
                          --Info GC
                          np: numProc;

         when nd_args => f1_args: pnode;

         when nd_largs => f1_largs: pnode;
                          f2_largs: pnode;

         when nd_arg => f1_arg: pnode;
                        f2_arg: pnode;
                        f3_arg: pnode;

         when nd_mode => tm: t_mode;

         when nd_decls => f1_decls: pnode;
                          f2_decls: pnode;

         when nd_decl => f_decl: pnode;

         when nd_dvar => f1_dv: pnode;
                         f2_dv: pnode;

         when nd_lid => f1_lid: pnode;
                        f2_lid: pnode;

         when nd_dcons => f1_dc: pnode;
                          f2_dc: pnode;
                          f3_dc: pnode;

         when nd_val => f_val_neg: pnode;
                        f_val_pos: pnode;

         when nd_valelem => f_valelem: pnode;

         when nd_dtipus => f_dt: pnode;

         when nd_dsub => f1_sub: pnode;
                         f2_sub: pnode;
                         f3_sub: pnode;
                         f4_sub: pnode;

         when nd_dreg => f1_reg: pnode;
                         f2_reg: pnode;

         when nd_dcol => f1_col: pnode;
                         f2_col: pnode;
                         f3_col: pnode;

         when nd_dcmps => f1_dcmps: pnode;
                          f2_dcmps: pnode;

         when nd_dcmp => f1_dcmp: pnode;
                         f2_dcmp: pnode;

         when nd_sents => f1_sents: pnode;
                          f2_sents: pnode;

         when nd_sent => f_sent: pnode;

         when nd_if => f1_if: pnode;
                       f2_if: pnode;
                       f3_if: pnode;

         when nd_it => f1_it: pnode;
                       f2_it: pnode;

    	 when nd_assig => f1_assig: pnode;
                          f2_assig: pnode;

         when nd_cp => f_cp: pnode;

         when nd_ref => f1_ref: pnode;
                        f2_ref: pnode;
                        --Info GC
            	    	esProc: boolean;
                        numP: numProc;
                        nv: numVar;

         when nd_qs => f1_qs: pnode;
                       f2_qs: pnode;

         when nd_q => f_q: pnode;
            	      --Info GC
            	      desp: despl;
                      w: despl;
                      b: valor;

         when nd_lexps => f1_lexps: pnode;
                          f2_lexps: pnode;

         when nd_e => te: t_exp;
                      f1_e: pnode;
            	      f2_e: pnode;
                      f_oprel: pnode;
                      --Info GC
            	      n: valor;
                      etsb: tSub;
                      param_out: boolean;

         when nd_id => id: idnom;
                       pos_id: posicio;

         when nd_lit => tsb: tSub;
                        val_lit: valor;
                        pos_lit: posicio;

         when nd_oprel => op: op_oprel;
                          pos_oprel: posicio;

         when nd_atom => pos_atom: posicio;
      end case;
   end record;

end declaracions.arbre;