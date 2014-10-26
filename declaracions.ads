package declaracions is

   max_noms: constant integer := 2000;
   max_str: constant integer := 200;
   
   max_nvar: constant integer := 1000;
   max_nproc: constant integer := 100;
   max_netiq: constant integer := 20000;
   
   max_prof: constant integer := 10;
   
   long_mitja_nom: constant integer := 6;
   long_mitja_str: constant integer := 30;

   type idnom is new integer range 0..max_noms;
   type idstring is new integer range 0..max_str;

   null_idnom: constant idnom := 0; --També servirà per per indicar que es un tipus universal
   null_idstring: constant idstring := 0; --Indica que no tenim id degut a un error de ct
   error_idnom: constant idnom := idnom'Last;
   
   type valor is new integer;
   type despl is new integer;
   
   type numVar is new integer range 0..max_nvar;
   varNula: constant numVar := 0;
   
   type numProc is new integer range 0..max_nproc;
   procNul: constant numProc := 0;
   
   type etiqueta is new integer range 0..max_netiq;
   eNula: constant etiqueta := 0;
   
end declaracions;
