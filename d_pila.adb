package body d_pila is

   -- Procediment pbuida que posa el punter cim de la pila a null
   procedure pbuida(s: out pila) is
      cim : pnode renames s.cim;
   begin
      cim := null;
   end pbuida;

   -- Procediment empila que enlla�a l'element indicat al inici de la llista
   procedure empila(s: in out pila; x: in elem) is
      cim : pnode renames s.cim;
      r : pnode;
   begin
      r := new node;
      r.all:= (x,cim);
      cim := r;
   exception
      when storage_error => raise desbordament_memoria;
   end empila;

   -- Procediment desempila que desenlla�a el primer element de la llista enlla�ada
   procedure desempila(s: in out pila) is
      cim : pnode renames s.cim;
   begin
      cim := cim.seg;
   exception
      when constraint_error => raise mal_us;
   end desempila;

   -- Funci� cim que retorna el primer element de la llista (cim de la pila)
   function cim(s: in pila) return elem is
      cim : pnode renames s.cim;
   begin
      return cim.x;
   exception
      when constraint_error => raise mal_us;
   end cim;

   -- Funci� esbuida que retorna un bole� que indica si la pila es buida
   function esbuida(s: in pila) return boolean is
      cim : pnode renames s.cim;
   begin
      return cim = null;
   end esbuida;

end d_pila;
