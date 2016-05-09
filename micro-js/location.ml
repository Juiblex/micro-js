type t = int                                     
  
let compare = Pervasives.compare                 

let fresh = let r = ref 0 in fun () -> incr r; !r
