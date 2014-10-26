package body semantica.missatges is

   --Obrir i tancar fitxer log
   procedure prepara(nom: string) is
   begin
      create(f, out_file, nom & ".log");
   end prepara;

   procedure conclou is
   begin
      close(f);
   end conclou;

   --Procediments i funcions auxiliars
   procedure mostraPos(li,co: in natural) is
   begin
      put_line(f, "ERROR DE TIPUS: En la línia"&integer'image(li)&", columna"& integer'image(co)&":");
   end mostraPos;

   function tSubToString (tsb: in tSub) return String is
   begin
      case tsb is
         when ts_bool => return "booleà";
         when ts_car => return "caràcter";
         when ts_ent => return "enter";
         when ts_arr => return "col.lecció";
         when ts_rec => return "registre";
         when ts_nul => return "NULL";
      end case;
   end tSubToString;

   function tdToString (td: in tDescr) return String is
   begin
      case td is
         when d_const => return "constant";
         when d_var => return "variable";
         when d_proc => return "procediment";
         when d_tipus => return "tipus";
         when d_argc => return "argument";
         when d_camp => return "camp";
         when d_index => return "índex";
         when d_nula => return "tipus universal";
      end case;
   end tdToString;

   --Missatges globals
   procedure mg_nomFitxerIncorrecte is
   begin
      put_line(f, "ERROR: Nom del fitxer incorrecte.");
   end mg_nomFitxerIncorrecte;

   procedure mg_sintaxiCorrecte is
   begin
      put_line(f, "Sintaxi correcte.");
   end mg_sintaxiCorrecte;

   procedure mg_errorSintactic is
   begin
      put_line(f, "ERROR: Sintaxi incorrecte.");
   end mg_errorSintactic;

   procedure mg_comprTipusCorrecte is
   begin
      put_line(f, "Comprovació de tipus correcte.");
   end mg_comprTipusCorrecte;

   procedure mg_errorComprTipus is
   begin
      put_line(f, "ERROR: Comprovació de tipus incorrecte.");
   end mg_errorComprTipus;

   procedure mg_codiIntermigCorrecte is
   begin
      put_line(f, "Codi intermig generat.");
   end mg_codiIntermigCorrecte;

   procedure mg_errorCodiIntermig is
   begin
      put_line(f, "ERROR COMPILADOR: Generació de codi intermig fallida.");
   end mg_errorCodiIntermig;

   procedure mg_codiAssembladorCorrecte is
   begin
      put_line(f, "Codi assemblador generat.");
   end mg_codiAssembladorCorrecte;

   procedure mg_errorCodiAssemblador is
   begin
      put_line(f, "ERROR COMPILADOR: Generació de codi assemblador fallida.");
   end mg_errorCodiAssemblador;

   procedure mg_errorCompiladorComprTipus is
   begin
      put_line(f, "ERROR COMPILADOR: Error durant la comprovació de tipus.");
   end mg_errorCompiladorComprTipus;

   --Missatges d'error CT
   procedure me_argumentsProgPrincipal is
   begin
      put_line(f,"ERROR DE TIPUS: A la declaració del programa principal:");
      put_line(f,"   El programa principal no pot tenir arguments.");
   end me_argumentsProgPrincipal;

   procedure me_idProcDif(li,co: in natural; id1, id2: in idnom) is
   begin
      mostraPos(li,co);
      if id1 = null_idnom or id2 = null_idnom or id1 = error_idnom or id2 = error_idnom then
         put_line(f,"   El nom de l'inici del procediment no es el mateix que el del final.");
      else
         put_line(f,"   El nom de l'inici del procediment '"&consultaNom(tn, id1)
                  &"' no es el mateix que el del final '"&consultaNom(tn, id2)&"'.");
      end if;
   end me_idProcDif;

   procedure me_jaExisteix(li,co: in natural; id: in idnom; msg: in string) is
   begin
      mostraPos(li,co);
      if id = null_idnom or id = error_idnom then
         put_line(f,"   El nom ja existeix.");
      else
         put_line(f,"   El nom de "&msg&" '"&consultaNom(tn,id)&"' ja existeix.");
      end if;
   end me_jaExisteix;

   procedure me_noEsTipus(li,co: in natural; id: in idnom) is
   begin
      mostraPos(li,co);
      if id = null_idnom or id = error_idnom then
         put_line(f,"   No és un nom de tipus.");
      else
         put_line(f,"   '"&consultaNom(tn,id)&"' no és un nom de tipus.");
      end if;
   end me_noEsTipus;

   procedure me_tipusDistints(li,co: in natural; id1,id2: in idnom) is
   begin
      mostraPos(li,co);
      if id1 = null_idnom or id2 = null_idnom or id1 = error_idnom or id2 = error_idnom then
         put_line(f,"   Els tipus no son compatibles.");
      else
         put_line(f,"   El tipus '"&consultaNom(tn,id2)
               &"' no es compatible amb el tipus '"&consultaNom(tn,id1)&"'.");
      end if;
   end me_tipusDistints;

   procedure me_tipusNoCompatibles(li,co: in natural; tsb1, tsb2: in tSub) is
   begin
      mostraPos(li,co);
      put_line(f,"   El tipus '"&tSubToString(tsb1)&"' no és compatible amb el tipus '"&tSubToString(tsb2)&"'.");
   end me_tipusNoCompatibles;

   procedure me_noEsEnter(li,co: in natural; tsb: in tSub) is
   begin
      mostraPos(li,co);
      put_line(f,"   S'esperava un 'enter' i no '"&tSubToString(tsb)&"'.");
   end me_noEsEnter;

   procedure me_noEsConstant(li,co: in natural; td: in tDescr) is
   begin
      mostraPos(li,co);
      put_line(f,"   S'esperava una constant i no '"&tdToString(td)&"'.");
   end me_noEsConstant;

   procedure me_tsbNoEscalar(li,co: in natural; id: in idnom) is
   begin
      mostraPos(li,co);
      if id = null_idnom or id = error_idnom then
         put_line(f,"   Aquest tipus no és escalar.");
      else
         put_line(f,"   El tipus '"&consultaNom(tn,id)&"' no és escalar.");
      end if;
   end me_tsbNoEscalar;

   procedure me_infMajorSup(li,co: in natural; id: in idnom) is
   begin
      mostraPos(li,co);
      if id = null_idnom or id = error_idnom then
         put_line(f,"   El límit inferior es major que el superior.");
      else
         put_line(f,"   En la definició del tipus '"&consultaNom(tn,id)
               &"' el límit inferior es major que el superior.");
      end if;
   end me_infMajorSup;

   procedure me_tsbNoBoolea(li,co: in natural; tsb: tSub) is
   begin
      mostraPos(li,co);
      put_line(f,"   S'esperava un booleà i no un "&tSubToString(tsb)&".");
   end me_tsbNoBoolea;

   procedure me_noEsVar(li,co: in natural) is
   begin
      mostraPos(li,co);
      put_line(f,"   La part esquerre de l'assignació ha de ser una variable.");
   end me_noEsVar;

   procedure me_noEsProc(li,co: in natural) is
   begin
      mostraPos(li,co);
      put_line(f,"   La referència no és un procedimient.");
   end me_noEsProc;

   procedure me_operandsNoEnters(li,co: in natural; op: string) is
   begin
      mostraPos(li,co);
      put_line(f,"   Els operands de "&op&" no són enters.");
   end me_operandsNoEnters;

   procedure me_operandsNoBooleans(li,co: in natural; op: string) is
   begin
      mostraPos(li,co);
      put_line(f,"   Els operands de "&op&" no són booleans.");
   end me_operandsNoBooleans;

   procedure me_assigProc(li,co: in natural) is
   begin
      mostraPos(li,co);
      put_line(f,"   Estas intentant assignar un procediment.");
   end me_assigProc;

   procedure me_constantEsEscalar(li,co: in natural) is
   begin
      mostraPos(li,co);
      put_line(f,"   Una constant ha de ser un escalar.");
   end me_constantEsEscalar;

   procedure me_procRequereixParametres(li, co: in natural) is
   begin
      mostraPos(li, co);
      put_line(f,"   Aquest procediment requereix paràmetres.");
   end me_procRequereixParametres;

   procedure me_procFaltenParametres(li, co: in natural) is
   begin
      mostraPos(li, co);
      put_line(f,"   A aquest procediment li falten paràmetres.");
   end me_procFaltenParametres;

   procedure me_procMassaParametres(li, co: in natural) is
   begin
      mostraPos(li, co);
      put_line(f,"   A aquest procediment li sobren paràmetres.");
   end me_procMassaParametres;

   procedure me_modeParametreIncorrecte(li, co: in natural) is
   begin
      mostraPos(li, co);
      put_line(f,"   El paràmetre no està en el mode correcte.");
   end me_modeParametreIncorrecte;

   procedure me_noEsArray(li, co: in natural) is
   begin
      mostraPos(li, co);
      put_line(f,"   Aquest tipus no és un array.");
   end me_noEsArray;

   procedure me_faltenIndexosArray(li, co: in natural) is
   begin
      mostraPos(li, co);
      put_line(f,"   En aquest array falten índexos.");
   end me_faltenIndexosArray;

   procedure me_noEsRecord(li, co: in natural) is
   begin
      mostraPos(li, co);
      put_line(f,"   Aquest tipus no es un record.");
   end me_noEsRecord;

   procedure me_noExisteixAquestCamp(li, co: in natural) is
   begin
      mostraPos(li, co);
      put_line(f,"   Aquest camp no existeix.");
   end me_noExisteixAquestCamp;

   procedure me_massaIndexosArray(li, co: in natural) is
   begin
      mostraPos(li, co);
      put_line(f,"   Aquest array te més índexos dels declarats.");
   end me_massaIndexosArray;

   procedure me_indexTipusIncorrecte(li, co: in natural; id1, id2: in idnom) is
   begin
      mostraPos(li,co);
      if id1 = null_idnom or id2 = null_idnom or id1 = error_idnom or id2 = error_idnom then
         put_line(f,"   El tipus de l'índex no és compatible amb el tipus de l'índex declarat.");
      else
         put_line(f,"   El tipus de l'índex '"&consultaNom(tn,id2)
               &"' no es compatible amb el tipus de l'índex declarat '"&consultaNom(tn,id1)&"'.");
      end if;
   end me_indexTipusIncorrecte;

   procedure me_indexTSubIncorrecte(li, co: in natural; tsb1, tsb2: in tSub) is
   begin
      mostraPos(li, co);
      put_line(f,"   El tipus de l'índex '"&tSubToString(tsb2)
               &"' no és compatible amb el tipus de l'índex declarat '"&tSubToString(tsb1)&"'.");
   end me_indexTSubIncorrecte;

   procedure me_referenciaNoDeclarada(li, co: in  natural) is
   begin
      mostraPos(li, co);
      put_line(f,"   Aquesta referència no està declarada.");
   end me_referenciaNoDeclarada;

   procedure me_assignacioTipusEstructurat(li, co: in natural) is
   begin
      mostraPos(li, co);
      put_line(f,"   No es poden fer assignacions de tipus estructurats.");
   end me_assignacioTipusEstructurat;

   procedure me_constantTsbNoEscalar(li,co: in natural; idtc: idnom) is
   begin
      mostraPos(li, co);
       if idtc = null_idnom or idtc = error_idnom then
          put_line(f, "   El tipus de la constant no és un tipus escalar.");
       else
          put_line(f, "   El tipus de la constant '"&consultaNom(tn,idtc)&"' no es un tipus escalar.");
      end if;
   end me_constantTsbNoEscalar;

   procedure me_constantTipusNoCompatibles(li,co: natural; tsb1, tsb2: tSub) is
   begin
      mostraPos(li, co);
      put_line(f, "   El tipus de la constant '"&tSubToString(tsb1)
               &"' no coincideix amb el tipus del valor assignat '"&tSubToString(tsb2)&"'.");
   end me_constantTipusNoCompatibles;

   procedure me_constantTipusDistints(li,co: in natural; idtc, idt: idnom) is
   begin
      mostraPos(li, co);
      if idtc = null_idnom or idt = null_idnom or idtc = error_idnom or idt = error_idnom then
          put_line(f, "   El tipus de la constant no coincideix amb el tipus del valor assignat.");
      else
         put_line(f, "   El tipus de la constant '"&consultaNom(tn, idtc)
                  &"' no coincideix amb el tipus del valor assignat '"&consultaNom(tn, idt)&"'.");
      end if;
   end me_constantTipusDistints;

   procedure me_noExisteixTipusConstant(li,co: in natural) is
   begin
      mostraPos(li, co);
      put_line(f, "   El tipus de la constant no existeix.");
   end me_noExisteixTipusConstant;

   procedure me_variableTipusNoCompatibles(lie,coe: in natural; rtsb,etsb: in tSub) is
   begin
      mostraPos(lie, coe);
      put_line(f,"   El tipus de la variable '"&tSubToString(rtsb)
               &"' no és compatible amb el tipus que se li està assignant '"&tSubToString(etsb)&"'.");
   end me_variableTipusNoCompatibles;

   procedure me_variableTipusDistints(li,co: in natural; id1,id2: in idnom) is
   begin
      mostraPos(li,co);
      if id1 = null_idnom or id2 = null_idnom or id1 = error_idnom or id2 = error_idnom then
         put_line(f,"   El tipus de la variable no es compatible amb el tipus del valor assignat.");
      else
         put_line(f,"   El tipus de la variable '"&consultaNom(tn,id1)
               &"' no es compatible amb el tipus del valor assignat '"&consultaNom(tn,id2)&"'.");
      end if;
   end me_variableTipusDistints;

   procedure me_procTipusDistints(li, co: in natural; id1, id2: in idnom) is
   begin
      mostraPos(li,co);
      if id1 = null_idnom or id2 = null_idnom or id1 = error_idnom or id2 = error_idnom then
         put_line(f,"   El tipus del paràmetre no és compatible amb el tipus del paràmetre declarat a la declaració del procediment.");
      else
         put_line(f,"   El tipus del paràmetre '"&consultaNom(tn,id2)
               &"' no es compatible amb el tipus del parametre declarat a la declaració del procediment '"&consultaNom(tn,id1)&"'.");
      end if;
   end me_procTipusDistints;

   procedure me_procTipusNoCompatibles(li, co: in natural; tsb1, tsb2: in tSub) is
   begin
      mostraPos(li, co);
      put_line(f,"   El tipus del paràmetre '"&tSubToString(tsb2)
               &"' no és compatible amb el tipus que te declarat al procediment '"&tSubToString(tsb1)&"'.");
   end me_procTipusNoCompatibles;

end semantica.missatges;
