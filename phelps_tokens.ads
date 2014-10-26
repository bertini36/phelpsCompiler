with declaracions.arbre;
package Phelps_Tokens is

subtype yystype is declaracions.arbre.pnode;
    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, Id, Literal,
         Pc_Accio, Pc_Es, Pc_Inicia,
         Pc_Fi, Pc_Entrada, Pc_Sortida,
         Pc_Tipus, Pc_Constant, Pc_Nou,
         Pc_Coleccio, Pc_Abast, Pc_De,
         Pc_Registre, Pc_Fregistre, Pc_Mentre,
         Pc_Fer, Pc_Fmentre, Pc_Si,
         Pc_Llavors, Pc_Sino, Pc_Fsi,
         Op_No, Op_I, Op_O,
         S_Dospunts, S_Assig, S_Oprel,
         S_Mes, S_Menys, S_Prod,
         S_Div, S_Mod, S_Parobert,
         S_Partancat, S_Abast, S_Punt,
         S_Coma, S_Punticoma, S_Menys_Unitari );

    Syntax_Error : exception;

end Phelps_Tokens;
