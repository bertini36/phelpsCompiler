with Ada.Text_IO, declaracions.desc, declaracions, declaracions.t_noms;
use Ada.Text_IO, declaracions.desc, declaracions, declaracions.t_noms;

package semantica.missatges is

   --Obrir i tancar fitxer log
   procedure prepara(nom: string);
   procedure conclou;

   --Missatges globals
   procedure mg_nomFitxerIncorrecte;
   procedure mg_sintaxiCorrecte;
   procedure mg_errorSintactic;
   procedure mg_comprTipusCorrecte;
   procedure mg_errorComprTipus;
   procedure mg_codiIntermigCorrecte;
   procedure mg_errorCodiIntermig;
   procedure mg_codiAssembladorCorrecte;
   procedure mg_errorCodiAssemblador;
   procedure mg_errorCompiladorComprTipus;
   
   --Missatges d'error CT
   procedure me_argumentsProgPrincipal;
   procedure me_idProcDif(li, co: in natural; id1, id2: in idnom);
   procedure me_jaExisteix(li, co: in natural; id: in idnom; msg: in string);
   procedure me_noEsTipus(li, co: in natural; id: in idnom);
   procedure me_tipusDistints(li, co: in natural; id1,id2: in idnom);
   procedure me_tipusNoCompatibles(li, co: in natural; tsb1, tsb2: in tSub);
   procedure me_noEsEnter(li, co: in natural; tsb: in tSub);
   procedure me_noEsConstant(li, co: in natural; td: in tDescr);
   procedure me_tsbNoEscalar(li, co: in natural; id: in idnom);
   procedure me_infMajorSup(li, co: in natural; id: in idnom);
   procedure me_tsbNoBoolea(li, co: in natural; tsb: tSub);
   procedure me_noEsVar(li, co: in natural);
   procedure me_noEsProc(li, co: in natural);
   procedure me_operandsNoEnters(li, co: in natural; op: string);
   procedure me_operandsNoBooleans(li, co: in natural; op: string);
   procedure me_assigProc(li, co: in natural);
   procedure me_constantEsEscalar(li,co: in natural);
   procedure me_procRequereixParametres(li, co: in natural);
   procedure me_procFaltenParametres(li, co: in natural);
   procedure me_procMassaParametres(li, co: in natural);
   procedure me_modeParametreIncorrecte(li, co: in natural);
   procedure me_noEsArray(li, co: in natural);
   procedure me_faltenIndexosArray(li, co: in natural);
   procedure me_noEsRecord(li, co: in natural);
   procedure me_noExisteixAquestCamp(li, co: in natural);
   procedure me_massaIndexosArray(li, co: in natural);
   procedure me_indexTipusIncorrecte(li, co: in natural; id1, id2: in idnom);
   procedure me_indexTSubIncorrecte(li, co: in natural; tsb1, tsb2: in tSub);
   procedure me_referenciaNoDeclarada(li, co: in  natural);
   procedure me_assignacioTipusEstructurat(li, co: in natural);
   procedure me_constantTsbNoEscalar(li, co: in natural;idtc: idnom);
   procedure me_constantTipusNoCompatibles(li, co: in natural; tsb1, tsb2: in tSub);
   procedure me_constantTipusDistints(li, co: in natural; idtc, idt: idnom);
   procedure me_noExisteixTipusConstant(li, co: in natural);
   procedure me_variableTipusNoCompatibles(lie, coe: in natural; rtsb,etsb: in tSub);
   procedure me_variableTipusDistints(li, co: in natural; id1,id2: in idnom);
   procedure me_procTipusDistints(li, co: in natural; id1, id2: in idnom);
   procedure me_procTipusNoCompatibles(li, co: in natural; tsb1, tsb2: in tSub);

private
   f : File_Type;

end semantica.missatges;
