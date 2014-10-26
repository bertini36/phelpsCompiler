package body semantica.generacio_codi is

   procedure calculDespl is
      nProc: numProc;
   begin
      for nVar in 1..nv loop
         if tv(nVar).tv = t_var and then not tv(nVar).esParam then
            nProc := tv(nVar).np;
            tp(nProc).ocupVL := tp(nProc).ocupVL + tv(nVar).ocup;
            tv(nVar).desp := -tp(nProc).ocupVL;
         end if;
      end loop;
   end calculDespl;

   procedure prepara(nom: in string) is
   begin
      Ada.Text_IO.Create(assemblador, Ada.Text_IO.Out_File, nom & ".s");
      d_pilaProc.pbuida(pProc);
   end prepara;

   procedure conclou is
   begin
      Seq_IO.Close(file);
      Ada.Text_IO.Close(assemblador);
   end conclou;

   function novaEtiqueta return etiqueta is
   begin
      ne := ne + 1;
      return ne;
   end novaEtiqueta;

   procedure LD(nv: in numVar; reg: in string) is
      v: valor;
      d: despl;
      np: numproc;
      prof, profv4: natural;
   begin
      if tv(nv).tv = t_const then
         v := tv(nv).val;
         if v < 0 then
            Put_Line(assemblador, "movl $"&v'Img&", "&reg);
         else
            Put_Line(assemblador, "movl $"&v'Img(2..v'Img'Last)&", "&reg);
         end if;
      elsif tv(nv).np = d_pilaProc.cim(pProc) and then tv(nv).desp < 0 then
         d := tv(nv).desp;
         Put_Line(assemblador, "movl "&d'Img&"(%ebp), "&reg);
      elsif tv(nv).np = d_pilaProc.cim(pProc) and then tv(nv).desp > 0 then
         d := tv(nv).desp;
         Put_Line(assemblador, "movl "&d'Img(2..d'Img'Last)&"(%ebp), %esi");
         Put_Line(assemblador, "movl (%esi), "&reg);
      elsif tv(nv).np /= d_pilaProc.cim(pProc) and then tv(nv).desp < 0 then
         d := tv(nv).desp;
         np := tv(nv).np;
         prof := tp(np).prof;
         profv4 := prof * 4;
         Put_Line(assemblador, "movl $DISP, %esi");
         Put_Line(assemblador, "movl "&profv4'Img(2..profv4'Img'Last)&"(%esi), %esi");
         Put_Line(assemblador, "movl "&d'Img&"(%esi), "&reg);
      elsif tv(nv).np /= d_pilaProc.cim(pProc) and then tv(nv).desp > 0 then
         d := tv(nv).desp;
         np := tv(nv).np;
         prof := tp(np).prof;
         profv4 := prof * 4;
         Put_Line(assemblador, "movl $DISP, %esi");
         Put_Line(assemblador, "movl "&profv4'Img(2..profv4'Img'Last)&"(%esi), %esi");
         Put_Line(assemblador, "movl "&d'Img(2..d'Img'Last)&"(%esi), %esi");
         Put_Line(assemblador, "movl %(esi), "&reg);
      end if;
   end LD;

   procedure ST (reg: in string; nv: in numVar) is
      d: despl;
      np: numproc;
      prof, profv4: natural;
   begin
      if tv(nv).np = d_pilaProc.cim(pProc) and then tv(nv).desp < 0 then
         d := tv(nv).desp;
         Put_Line(assemblador, "movl "&reg&", "&d'Img&"(%ebp)");
      elsif tv(nv).np = d_pilaProc.cim(pProc) and then tv(nv).desp > 0 then
         d := tv(nv).desp;
         Put_Line(assemblador, "movl "&d'Img(2..d'Img'Last)&"(%ebp), %edi");
         Put_Line(assemblador, "movl "&reg&", (%edi)");
      elsif tv(nv).np /= d_pilaProc.cim(pProc) and then tv(nv).desp < 0 then
         d := tv(nv).desp;
         np := tv(nv).np;
         prof := tp(np).prof;
         profv4 := prof * 4;
         Put_Line(assemblador, "movl $DISP, %esi");
         Put_Line(assemblador, "movl "&profv4'Img(2..profv4'Img'Last)&"(%esi), %edi");
         Put_Line(assemblador, "movl "&reg&", "&d'Img&"(%edi)");
      elsif tv(nv).np /= d_pilaProc.cim(pProc) and then tv(nv).desp > 0 then
         d := tv(nv).desp;
         np := tv(nv).np;
         prof := tp(np).prof;
         profv4 := prof * 4;
         Put_Line(assemblador, "movl $DISP, %esi");
         Put_Line(assemblador, "movl "&profv4'Img(2..profv4'Img'Last)&"(%esi), %esi");
         Put_Line(assemblador, "movl "&d'Img(2..d'Img'Last)&"(%esi), %edi");
         Put_Line(assemblador, "movl "&reg&", (%edi)");
      end if;
   end ST;

   procedure LDA(nv: in numVar; reg: in string)is
      d: despl;
      np: numproc;
      prof, profv4: natural;
      e: etiqueta;
   begin
      if tv(nv).tv = t_const then
         e := tv(nv).etiq;
         if tv(nv).tsb = ts_ent or tv(nv).tsb = ts_bool then
            Put_Line(assemblador, "movl $c"&e'Img(2..e'Img'Last)&", "&reg);
         else
            Put_Line(assemblador, "movl $s"&e'Img(2..e'Img'Last)&", "&reg);
         end if;
      elsif tv(nv).np = d_pilaProc.cim(pProc) and then tv(nv).desp < 0 then
         d := tv(nv).desp;
         Put_Line(assemblador, "leal "&d'Img&"(%ebp), "&reg);
      elsif tv(nv).np = d_pilaProc.cim(pProc) and then tv(nv).desp > 0 then
         d := tv(nv).desp;
         Put_Line(assemblador, "movl "&d'Img(2..d'Img'Last)&"(%ebp), "&reg);
      elsif tv(nv).np /= d_pilaProc.cim(pProc) and then tv(nv).desp < 0 then
         d := tv(nv).desp;
         np := tv(nv).np;
         prof := tp(np).prof;
         profv4 := prof * 4;
         Put_Line(assemblador, "movl $DISP, %esi");
         Put_Line(assemblador, "movl "&profv4'Img(2..profv4'Img'Last)&"(%esi), %esi");
         Put_Line(assemblador, "leal "&d'Img&"(%esi), "&reg);
      elsif tv(nv).np /= d_pilaProc.cim(pProc) and then tv(nv).desp > 0 then
         d := tv(nv).desp;
         np := tv(nv).np;
         prof := tp(np).prof;
         profv4 := prof * 4;
         Put_Line(assemblador, "movl $DISP, %esi");
         Put_Line(assemblador, "movl "&profv4'Img(2..profv4'Img'Last)&"(%esi), %esi");
         Put_Line(assemblador, "movl "&d'Img(2..d'Img'Last)&"(%esi), "&reg);
      end if;
   end LDA;

   procedure tr_copia(a: in numvar; b: in numvar) is
   begin
      LD(b, "%eax");
      ST("%eax", a);
   end tr_copia;

   procedure tr_menysU(a: in numvar; b: in numvar) is
   begin
      LD(b, "%eax");
      Put_Line(assemblador, "xorl %ebx, %ebx");
      Put_Line(assemblador, "subl %eax, %ebx");
      ST("%ebx", a);
   end tr_menysU;

   procedure tr_not(a: in numvar; b: in numvar) is
   begin
      LD(b, "%eax");
      Put_Line(assemblador, "notl %eax");
      ST("%eax", a);
   end tr_not;

   procedure tr_consIndex(a: in numvar; b: in numvar; c: in numvar) is
   begin
      LDA(b, "%eax");
      LD(c, "%ebx");
      Put_Line(assemblador, "addl %ebx, %eax");
      Put_Line(assemblador, "movl (%eax), %ebx");
      ST("%ebx", a);
   end tr_consIndex;

   procedure tr_modIndex(a: in numvar; b: in numvar; c: in numvar) is
   begin
      LDA(a, "%edi");
      LD(b, "%eax");
      Put_Line(assemblador, "addl %eax, %edi");
      LD(c, "%eax");
      Put_Line(assemblador, "movl %eax, (%edi)");
   end tr_modIndex;

   procedure tr_suma(a: in numvar; b: in numvar; c: in numvar) is
   begin
      LD(b, "%eax");
      LD(c, "%ebx");
      Put_Line(assemblador, "addl %eax, %ebx");
      ST("%ebx", a);
   end tr_suma;

   procedure tr_resta(a: in numvar; b: in numvar; c: in numvar) is
   begin
      LD(b, "%eax");
      LD(c, "%ebx");
      Put_Line(assemblador, "subl %ebx, %eax");
      ST("%eax", a);
   end tr_resta;

   procedure tr_prod(a: in numvar; b: in numvar; c: in numvar) is
   begin
      LD(b, "%eax");
      LD(c, "%ebx");
      Put_Line(assemblador, "imull %eax, %ebx");
      ST("%ebx", a);
   end tr_prod;

   procedure tr_div(a: in numvar; b: in numvar; c: in numvar) is
   begin
      LD(b, "%eax");
      Put_Line(assemblador, "movl %eax, %edx");
      Put_Line(assemblador, "sarl $31, %edx");
      LD(c, "%ebx");
      Put_Line(assemblador, "idivl %ebx");
      ST("%eax", a);
   end tr_div;

   procedure tr_mod(a: in numvar; b: in numvar; c: in numvar) is
   begin
      LD(b, "%eax");
      Put_Line(assemblador, "movl %eax, %edx");
      Put_Line(assemblador, "sarl $31, %edx");
      LD(c, "%ebx");
      Put_Line(assemblador, "idivl %ebx");
      ST("%edx", a);
   end tr_mod;

   procedure tr_and(a: in numvar; b: in numvar; c: in numvar) is
   begin
      LD(b, "%eax");
      LD(c, "%ebx");
      Put_Line(assemblador, "andl %eax, %ebx");
      ST("%ebx", a);
   end tr_and;

   procedure tr_or(a: in numvar; b: in numvar; c: in numvar) is
   begin
      LD(b, "%eax");
      LD(c, "%ebx");
      Put_Line(assemblador, "orl %eax, %ebx");
      ST("%ebx", a);
   end tr_or;

   procedure tr_etiq(e: in etiqueta) is
   begin
      if e = eNula then --Es el principal
         Put_Line(assemblador, consultaNom(tn, tp(7).idProc)&": nop");
      else
         Put_Line(assemblador, "e"&e'Img(2..e'Img'Last)&": nop");
      end if;
   end tr_etiq;

   procedure tr_goto(e: in etiqueta) is
   begin
      Put_Line(assemblador, "jmp e"&e'Img(2..e'Img'Last));
   end tr_goto;

   procedure tr_ig(e: in etiqueta; a: in numvar; b: in numvar) is
      l: etiqueta;
   begin
      LD(a, "%eax");
      LD(b, "%ebx");
      Put_Line(assemblador, "cmpl %ebx, %eax");
      l := novaEtiqueta;
      Put_Line(assemblador, "jne l"&l'Img(2..l'Img'Last));
      Put_Line(assemblador, "jmp e"&e'Img(2..e'Img'Last));
      Put_Line(assemblador, "l"&l'Img(2..l'Img'Last)&": nop");
   end tr_ig;

   procedure tr_dif(e: in etiqueta; a: in numvar; b: in numvar) is
      l: etiqueta;
   begin
      LD(a, "%eax");
      LD(b, "%ebx");
      Put_Line(assemblador, "cmpl %ebx, %eax");
      l := novaEtiqueta;
      Put_Line(assemblador, "je l"&l'Img(2..l'Img'Last));
      Put_Line(assemblador, "jmp e"&e'Img(2..e'Img'Last));
      Put_Line(assemblador, "l"&l'Img(2..l'Img'Last)&": nop");
   end tr_dif;

   procedure tr_gt(e: in etiqueta; a: in numvar; b: in numvar) is
      l: etiqueta;
   begin
      LD(a, "%eax");
      LD(b, "%ebx");
      Put_Line(assemblador, "cmpl %ebx, %eax");
      l := novaEtiqueta;
      Put_Line(assemblador, "jle l"&l'Img(2..l'Img'Last));
      Put_Line(assemblador, "jmp e"&e'Img(2..e'Img'Last));
      Put_Line(assemblador, "l"&l'Img(2..l'Img'Last)&": nop");
   end tr_gt;

   procedure tr_ge(e: in etiqueta; a: in numvar; b: in numvar) is
      l: etiqueta;
   begin
      LD(a, "%eax");
      LD(b, "%ebx");
      Put_Line(assemblador, "cmpl %ebx, %eax");
      l := novaEtiqueta;
      Put_Line(assemblador, "jl l"&l'Img(2..l'Img'Last));
      Put_Line(assemblador, "jmp e"&e'Img(2..e'Img'Last));
      Put_Line(assemblador, "l"&l'Img(2..l'Img'Last)&": nop");
   end tr_ge;

   procedure tr_lt(e: in etiqueta; a: in numvar; b: in numvar) is
      l: etiqueta;
   begin
      LD(a, "%eax");
      LD(b, "%ebx");
      Put_Line(assemblador, "cmpl %ebx, %eax");
      l := novaEtiqueta;
      Put_Line(assemblador, "jge l"&l'Img(2..l'Img'Last));
      Put_Line(assemblador, "jmp e"&e'Img(2..e'Img'Last));
      Put_Line(assemblador, "l"&l'Img(2..l'Img'Last)&": nop");
   end tr_lt;

   procedure tr_le(e: in etiqueta; a: in numvar; b: in numvar) is
      l: etiqueta;
   begin
      LD(a, "%eax");
      LD(b, "%ebx");
      Put_Line(assemblador, "cmpl %ebx, %eax");
      l := novaEtiqueta;
      Put_Line(assemblador, "jg l"&l'Img(2..l'Img'Last));
      Put_Line(assemblador, "jmp e"&e'Img(2..e'Img'Last));
      Put_Line(assemblador, "l"&l'Img(2..l'Img'Last)&": nop");
   end tr_le;

   procedure tr_pmb(np: in numProc) is
      prof, prof4: natural;
      ocupVL: despl;
   begin
      d_pilaProc.empila(pProc, np);
      prof := tp(np).prof;
      prof4 := prof * 4;
      ocupVL := tp(np).ocupVL;
      Put_Line(assemblador, "movl $DISP, %esi");
      Put_Line(assemblador, "movl "&prof4'Img(2..prof4'Img'Last)&"(%esi), %eax");
      Put_Line(assemblador, "pushl %eax");
      Put_Line(assemblador, "pushl %ebp");
      Put_Line(assemblador, "movl %esp, %ebp");
      Put_Line(assemblador, "movl %ebp, "&prof4'Img(2..prof4'Img'Last)&"(%esi)");
      Put_Line(assemblador, "subl $"&ocupVL'Img(2..ocupVL'Img'Last)&", %esp");
   end tr_pmb;

   procedure tr_rtn(np: in numProc) is
      prof, prof4: natural;
   begin
      d_pilaProc.desempila(pProc);
      prof := tp(np).prof;
      prof4 := prof * 4;
      Put_Line(assemblador, "movl %ebp, %esp");
      Put_Line(assemblador, "popl %ebp");
      Put_Line(assemblador, "movl $DISP, %edi");
      Put_Line(assemblador, "popl %eax");
      Put_Line(assemblador, "movl %eax, "&prof4'Img(2..prof4'Img'Last)&"(%edi)");
      Put_Line(assemblador, "ret");
   end tr_rtn;

   procedure tr_call(np: in numProc) is
      eProc: etiqueta;
      nParam, nParam4: natural;
   begin
      if tp(np).tp = p_intern then
         eProc := tp(np).e;
         Put_Line(assemblador, "call e"&eProc'Img(2..eProc'Img'Last));
         nParam := tp(np).numParam;
         nParam4 := nParam * 4;
         Put_Line(assemblador, "movl $"&nParam4'Img(2..nParam4'Img'Last)&", %eax");
         Put_Line(assemblador, "addl %eax, %esp");
      else
         Put_Line(assemblador, "call _"&consultaNom(tn, tp(np).idProc));
         if np /= 6 then
            Put_Line(assemblador,"movl $4, %eax");
            Put_Line(assemblador,"addl %eax, %esp");
         end if;
      end if;
   end tr_call;

   procedure tr_params(a: in numvar) is
   begin
      LDA(a, "%eax");
      Put_Line(assemblador, "pushl %eax");
   end tr_params;

   procedure tr_paramc(a: in numvar; b: in numvar) is
   begin
      LDA(a, "%eax");
      LD(b, "%ebx");
      Put_Line(assemblador, "addl %ebx, %eax");
      Put_Line(assemblador, "pushl %eax");
   end tr_paramc;

   procedure tradueix(i3a: in instr3a) is
   begin
      case i3a.op is
         when op3_copia =>
            tr_copia(i3a.a1, i3a.b1);
         when op3_menysU =>
            tr_menysU(i3a.a1, i3a.b1);
         when op3_not =>
            tr_not(i3a.a1, i3a.b1);
         when op3_consIndx =>
            tr_consIndex(i3a.a2, i3a.b2, i3a.c2);
         when op3_modIndx =>
            tr_modIndex(i3a.a2, i3a.b2, i3a.c2);
         when op3_suma =>
            tr_suma(i3a.a2, i3a.b2, i3a.c2);
         when op3_resta =>
            tr_resta(i3a.a2, i3a.b2, i3a.c2);
         when op3_prod =>
            tr_prod(i3a.a2, i3a.b2, i3a.c2);
         when op3_div =>
            tr_div(i3a.a2, i3a.b2, i3a.c2);
         when op3_mod =>
            tr_mod(i3a.a2, i3a.b2, i3a.c2);
         when op3_and =>
            tr_and(i3a.a2, i3a.b2, i3a.c2);
         when op3_or =>
            tr_or(i3a.a2, i3a.b2, i3a.c2);
         when op3_etiq =>
            tr_etiq(i3a.e1);
         when op3_goto =>
            tr_goto(i3a.e1);
         when op3_lt =>
            tr_lt(i3a.e2, i3a.a3, i3a.b3);
         when op3_le =>
            tr_le(i3a.e2, i3a.a3, i3a.b3);
         when op3_ig =>
            tr_ig(i3a.e2, i3a.a3, i3a.b3);
         when op3_dif =>
            tr_dif(i3a.e2, i3a.a3, i3a.b3);
         when op3_ge =>
            tr_ge(i3a.e2, i3a.a3, i3a.b3);
         when op3_gt =>
            tr_gt(i3a.e2, i3a.a3, i3a.b3);
         when op3_pmb =>
            tr_pmb(i3a.np);
         when op3_rtn =>
            tr_rtn(i3a.np);
         when op3_call =>
            tr_call(i3a.np);
         when op3_params =>
            tr_params(i3a.a4);
         when op3_paramc =>
            tr_paramc(i3a.a5, i3a.b5);
         when op3_noop =>
            null;
      end case;
   end tradueix;

   procedure posaConstants is
      compConsNum, compConsStr: integer;
   begin
      Put_Line(assemblador, ".section .data");
      compConsNum := 0;
      compConsStr := 0;
      for i in 1..nv loop
         if tv(i).tv = t_const then
            case tv(i).tsb is
               when ts_ent | ts_bool =>
                  compConsNum := compConsNum + 1;
                  if(tv(i).val < 0) then
                     Put_Line(assemblador, "c"&compConsNum'Img(2..compConsNum'Img'Last)
                              &": .long "&tv(i).val'Img);
                  else
                     Put_Line(assemblador, "c"&compConsNum'Img(2..compConsNum'Img'Last)
                              &": .long "&tv(i).val'Img(2..tv(i).val'Img'Last));
                  end if;
                  tv(i).etiq := etiqueta(compConsNum);
               when ts_car =>
                  compConsStr := compConsStr + 1;
                  Put_Line(assemblador, "s"&compConsStr'Img(2..compConsStr'Img'Last)
                           &": .ascii """&Character'Val(tv(i).val)&"""");
                  tv(i).etiq := etiqueta(compConsStr);
               when ts_arr =>
                  compConsStr := compConsStr + 1;
                  Put_Line(assemblador, "s"&compConsStr'Img(2..compConsStr'Img'Last)
                           &": .asciz """&consultaString(tn, idstring(tv(i).val))&"""");
                  tv(i).etiq := etiqueta(compConsStr);
               when others =>
                  null;
            end case;
         end if;
       end loop;
   end posaConstants;

   procedure posaDisp is
   begin
      Put_Line(assemblador, ".section .bss");
      Put_Line(assemblador, ".comm DISP 400");
   end posaDisp;

   procedure initText is
   begin
      Put_Line(assemblador, ".section .text");
      Put_Line(assemblador, ".global "&consultaNom(tn, tp(7).idProc));
   end initText;

   procedure gen_codi (fname: in string) is
      i3a: instr3a;
      nom: string(1..fname'Last-4);
   begin
      nom := fname(fname'First..fname'Last-4);
      Seq_IO.Open(file, seq_IO.In_file, nom & ".c3a");
      calculDespl;
      posaConstants;
      posaDisp;
      initText;
      while not Seq_IO.End_Of_File(file) loop
         Seq_IO.Read(file, i3a);
         tradueix(i3a);
      end loop;
      mg_codiAssembladorCorrecte;
   exception
      when others=>
         mg_errorCodiAssemblador;
   end gen_codi;

end semantica.generacio_codi;
