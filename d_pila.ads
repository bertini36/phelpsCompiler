generic
   type elem is private; -- Element generic que guardarem dins la pila

package d_pila is

   type pila is limited private;

   procedure pbuida(s: out pila);
   procedure empila(s: in out pila; x: in elem);
   procedure desempila(s: in out pila);
   function cim(s: in pila) return elem;
   function esbuida(s: in pila) return boolean;

   mal_us: exception;
   desbordament_memoria: exception;

private

   type node;
   type pnode is access node;

   -- Declaram el tipus node com un conjunt format per
   -- un element i un punter a un altre node
   type node is
      record
         x: elem;
         seg: pnode;
      end record;

   -- La pila consistirà en un punter al cim de la llista enllaçada
   type pila is
      record
         cim: pnode;
      end record;

end d_pila;
