module Monad : sig
  module type BASE = sig
    (* We make all monadic types doubly-parameterized so that they
     * can layer nicely with Continuation, which needs the second
     * type parameter. *)
    type ('x,'a) m
    type ('x,'a) result
    type ('x,'a) result_exn
    val unit : 'a -> ('x,'a) m
    val bind : ('x,'a) m -> ('a -> ('x,'b) m) -> ('x,'b) m
    val run : ('x,'a) m -> ('x,'a) result
    (* run_exn tries to provide a more ground-level result, but may fail *)
    val run_exn : ('x,'a) m -> ('x,'a) result_exn
    (* To simplify the library, we require every monad to supply a plus and zero. These obey the following laws:
     *     zero >>= f   ===  zero
     *     plus zero u  ===  u
     *     plus u zero  ===  u
     * Additionally, they will obey one of the following laws:
     *     (Catch)   plus (unit a) v  ===  unit a
     *     (Distrib) plus u v >>= f   ===  plus (u >>= f) (v >>= f)
     * When no natural zero is available, use `let zero () = Util.undef`.
     * The Make functor automatically detects for zero >>= ..., and
     * plus zero _, plus _ zero; it also substitutes zero for pattern-match failures.
    *)
    val zero : unit -> ('x,'a) m
    (* zero has to be thunked to ensure results are always poly enough *)
    val plus : ('x,'a) m -> ('x,'a) m -> ('x,'a) m
  end
  module type S = sig
    include BASE
    val (>>=) : ('x,'a) m -> ('a -> ('x,'b) m) -> ('x,'b) m
    val (>>) : ('x,'a) m -> ('x,'b) m -> ('x,'b) m
    val join : ('x,('x,'a) m) m -> ('x,'a) m
    val apply : ('x,'a -> 'b) m -> ('x,'a) m -> ('x,'b) m
    val lift : ('a -> 'b) -> ('x,'a) m -> ('x,'b) m
    val lift2 :  ('a -> 'b -> 'c) -> ('x,'a) m -> ('x,'b) m -> ('x,'c) m
    val (>=>) : ('a -> ('x,'b) m) -> ('b -> ('x,'c) m) -> 'a -> ('x,'c) m
    val do_when :  bool -> ('x,unit) m -> ('x,unit) m
    val do_unless :  bool -> ('x,unit) m -> ('x,unit) m
    val forever : (unit -> ('x,'a) m) -> ('x,'b) m
    val sequence : ('x,'a) m list -> ('x,'a list) m
    val sequence_ : ('x,'a) m list -> ('x,unit) m
    val guard : bool -> ('x,unit) m
    val sum : ('x,'a) m list -> ('x,'a) m
  end
end

module Maybe_monad : sig
  type ('x,'a) result = 'a option
  type ('x,'a) result_exn = 'a
  include Monad.S with type ('x,'a) result := ('x,'a) result
                   and type ('x,'a) result_exn := ('x,'a) result_exn
  (* MaybeT transformer *)
  module T : functor (Wrapped : Monad.S) -> sig
    type ('x,'a) result = ('x,'a option) Wrapped.result
    type ('x,'a) result_exn = ('x,'a) Wrapped.result_exn
    include Monad.S with type ('x,'a) result := ('x,'a) result
                     and type ('x,'a) result_exn := ('x,'a) result_exn
    val elevate : ('x,'a) Wrapped.m -> ('x,'a) m
  end
end

module Identity_monad : sig
  (* expose only the implementation of type `'a result` *)
  type ('x,'a) result = 'a
  type ('x,'a) result_exn = 'a
  include Monad.S with type ('x,'a) result := ('x,'a) result
                   and type ('x,'a) result_exn := ('x,'a) result_exn
end

module List_monad : sig
  (* declare additional operation, while still hiding implementation of type m *)
  type ('x,'a) result = 'a list
  type ('x,'a) result_exn = 'a
  include Monad.S with type ('x,'a) result := ('x,'a) result
                   and type ('x,'a) result_exn := ('x,'a) result_exn
  val permute : ('x,'a) m -> ('x,('x,'a) m) m
  val select : ('x,'a) m -> ('x,'a * ('x,'a) m) m
  (* ListT transformer *)
  module T : functor (Wrapped : Monad.S) -> sig
    type ('x,'a) result = ('x,'a list) Wrapped.result
    type ('x,'a) result_exn = ('x,'a) Wrapped.result_exn
    include Monad.S with type ('x,'a) result := ('x,'a) result
                     and type ('x,'a) result_exn := ('x,'a) result_exn
    val elevate : ('x,'a) Wrapped.m -> ('x,'a) m
    (* note that second argument is an 'a list, not the more abstract 'a m *)
    (* type is ('a -> 'b W) -> 'a list -> 'b list W == 'b listT(W) *)
    val distribute : ('a -> ('x,'b) Wrapped.m) -> 'a list -> ('x,'b) m
    val permute : ('x,'a) m -> ('x,('x,'a) m) m
    val select : ('x,'a) m -> ('x,('a * ('x,'a) m)) m
    val expose : ('x,'a) m -> ('x,'a list) Wrapped.m
  end
end

module Error_monad(Err : sig
                     type err
                     exception Exc of err
                     (* val zero : unit -> err
                        val plus : err -> err -> err *)
                   end) : sig
  (* declare additional operations, while still hiding implementation of type m *)
  type err = Err.err
  type 'a error = Error of err | Success of 'a
  type ('x,'a) result = 'a error
  type ('x,'a) result_exn = 'a
  include Monad.S with type ('x,'a) result := ('x,'a) result
                   and type ('x,'a) result_exn := ('x,'a) result_exn
  val throw : err -> ('x,'a) m
  val catch : ('x,'a) m -> (err -> ('x,'a) m) -> ('x,'a) m
  (* ErrorT transformer *)
  module T : functor (Wrapped : Monad.S) -> sig
    type ('x,'a) result = ('x,'a) Wrapped.result
    type ('x,'a) result_exn = ('x,'a) Wrapped.result_exn
    include Monad.S with type ('x,'a) result := ('x,'a) result
                     and type ('x,'a) result_exn := ('x,'a) result_exn
    val elevate : ('x,'a) Wrapped.m -> ('x,'a) m
    val throw : err -> ('x,'a) m
    val catch : ('x,'a) m -> (err -> ('x,'a) m) -> ('x,'a) m
  end
end

module Reader_monad(Env : sig type env end) : sig
  (* declare additional operations, while still hiding implementation of type m *)
  type env = Env.env
  type ('x,'a) result = env -> 'a
  type ('x,'a) result_exn = env -> 'a
  include Monad.S with type ('x,'a) result := ('x,'a) result
                   and type ('x,'a) result_exn := ('x,'a) result_exn
  val ask : ('x,env) m
  val asks : (env -> 'a) -> ('x,'a) m
  (* lookup i == `fun e -> e i` would assume env is a functional type *)
  val local : (env -> env) -> ('x,'a) m -> ('x,'a) m
  (* ReaderT transformer *)
  module T : functor (Wrapped : Monad.S) -> sig
    type ('x,'a) result = env -> ('x,'a) Wrapped.result
    type ('x,'a) result_exn = env -> ('x,'a) Wrapped.result_exn
    include Monad.S with type ('x,'a) result := ('x,'a) result
                     and type ('x,'a) result_exn := ('x,'a) result_exn
    val elevate : ('x,'a) Wrapped.m -> ('x,'a) m
    val ask : ('x,env) m
    val asks : (env -> 'a) -> ('x,'a) m
    val local : (env -> env) -> ('x,'a) m -> ('x,'a) m
    val expose : ('x,'a) m -> env -> ('x,'a) Wrapped.m
  end
end

module State_monad(Store : sig type store end) : sig
  (* declare additional operations, while still hiding implementation of type m *)
  type store = Store.store
  type ('x,'a) result =  store -> 'a * store
  type ('x,'a) result_exn = store -> 'a
  include Monad.S with type ('x,'a) result := ('x,'a) result
                   and type ('x,'a) result_exn := ('x,'a) result_exn
  val get : ('x,store) m
  val gets : (store -> 'a) -> ('x,'a) m
  val put : store -> ('x,unit) m
  val puts : (store -> store) -> ('x,unit) m
  (* StateT transformer *)
  module T : functor (Wrapped : Monad.S) -> sig
    type ('x,'a) result = store -> ('x,'a * store) Wrapped.result
    type ('x,'a) result_exn = store -> ('x,'a) Wrapped.result_exn
    include Monad.S with type ('x,'a) result := ('x,'a) result
                     and type ('x,'a) result_exn := ('x,'a) result_exn
    val elevate : ('x,'a) Wrapped.m -> ('x,'a) m
    val get : ('x,store) m
    val gets : (store -> 'a) -> ('x,'a) m
    val put : store -> ('x,unit) m
    val puts : (store -> store) -> ('x,unit) m
    (* val passthru : ('x,'a) m -> (('x,'a * store) Wrapped.result * store -> 'b) -> ('x,'b) m *)
    val expose : ('x,'a) m -> store -> ('x,'a * store) Wrapped.m
  end
end

module Ref_monad(V : sig type value end) : sig
  type ref
  type value = V.value
  type ('x,'a) result = 'a
  type ('x,'a) result_exn = 'a
  include Monad.S with type ('x,'a) result := ('x,'a) result
                   and type ('x,'a) result_exn := ('x,'a) result_exn
  val newref : value -> ('x,ref) m
  val deref : ref -> ('x,value) m
  val change : ref -> value -> ('x,unit) m
  (* RefT transformer *)
  module T : functor (Wrapped : Monad.S) -> sig
    type ('x,'a) result = ('x,'a) Wrapped.result
    type ('x,'a) result_exn = ('x,'a) Wrapped.result_exn
    include Monad.S with type ('x,'a) result := ('x,'a) result
                     and type ('x,'a) result_exn := ('x,'a) result_exn
    val elevate : ('x,'a) Wrapped.m -> ('x,'a) m
    val newref : value -> ('x,ref) m
    val deref : ref -> ('x,value) m
    val change : ref -> value -> ('x,unit) m
  end
end

module Writer_monad(Log : sig
                      type log
                      val zero : log
                      val plus : log -> log -> log
                    end) : sig
  (* declare additional operations, while still hiding implementation of type m *)
  type log = Log.log
  type ('x,'a) result = 'a * log
  type ('x,'a) result_exn = 'a * log
  include Monad.S with type ('x,'a) result := ('x,'a) result
                   and type ('x,'a) result_exn := ('x,'a) result_exn
  val tell : log -> ('x,unit) m
  val listen : ('x,'a) m -> ('x,'a * log) m
  val listens : (log -> 'b) -> ('x,'a) m -> ('x,'a * 'b) m
  (* val pass : ('x,'a * (log -> log)) m -> ('x,'a) m *)
  val censor : (log -> log) -> ('x,'a) m -> ('x,'a) m
  (* WriterT transformer *)
  module T : functor (Wrapped : Monad.S) -> sig
    type ('x,'a) result = ('x,'a * log) Wrapped.result
    type ('x,'a) result_exn = ('x,'a * log) Wrapped.result_exn
    include Monad.S with type ('x,'a) result := ('x,'a) result
                     and type ('x,'a) result_exn := ('x,'a) result_exn
    val elevate : ('x,'a) Wrapped.m -> ('x,'a) m
    val tell : log -> ('x,unit) m
    val listen : ('x,'a) m -> ('x,'a * log) m
    val listens : (log -> 'b) -> ('x,'a) m -> ('x,'a * 'b) m
    val censor : (log -> log) -> ('x,'a) m -> ('x,'a) m
  end
end

module Continuation_monad : sig
  (* expose only the implementation of type `('r,'a) result` *)
  type ('r,'a) m
  type ('r,'a) result = ('r,'a) m
  type ('r,'a) result_exn = ('a -> 'r) -> 'r
  include Monad.S with type ('r,'a) result := ('r,'a) result
                   and type ('r,'a) result_exn := ('r,'a) result_exn
                   and type ('r,'a) m := ('r,'a) m
  val callcc : (('a -> ('r,'b) m) -> ('r,'a) m) -> ('r,'a) m
  val reset : ('a,'a) m -> ('r,'a) m
  val shift : (('a -> ('q,'r) m) -> ('r,'r) m) -> ('r,'a) m
  (* val abort : ('a,'a) m -> ('a,'b) m *)
  val abort : 'a -> ('a,'b) m
  val run0 : ('a,'a) m -> 'a
  (* ContinuationT transformer *)
  module T : functor (Wrapped : Monad.S) -> sig
    type ('r,'a) m
    type ('r,'a) result = ('a -> ('r,'r) Wrapped.m) -> ('r,'r) Wrapped.result
    type ('r,'a) result_exn = ('a -> ('r,'r) Wrapped.m) -> ('r,'r) Wrapped.result_exn
    include Monad.S with type ('r,'a) result := ('r,'a) result
                     and type ('r,'a) result_exn := ('r,'a) result_exn
                     and type ('r,'a) m := ('r,'a) m
    val elevate : ('x,'a) Wrapped.m -> ('x,'a) m
    val callcc : (('a -> ('r,'b) m) -> ('r,'a) m) -> ('r,'a) m
    (* TODO: reset,shift,abort,run0 *)
  end
end

module Tree_monad : sig
  (* We implement the type as `'a tree option` because it has a natural`plus`,
   * and the rest of the library expects that `plus` and `zero` will come together. *)
  type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree)
  type ('x,'a) result = 'a tree option
  type ('x,'a) result_exn = 'a tree
  include Monad.S with type ('x,'a) result := ('x,'a) result
                   and type ('x,'a) result_exn := ('x,'a) result_exn
  (* TreeT transformer *)
  module T : functor (Wrapped : Monad.S) -> sig
    type ('x,'a) result = ('x,'a tree option) Wrapped.result
    type ('x,'a) result_exn = ('x,'a tree) Wrapped.result_exn
    include Monad.S with type ('x,'a) result := ('x,'a) result
                     and type ('x,'a) result_exn := ('x,'a) result_exn
    val elevate : ('x,'a) Wrapped.m -> ('x,'a) m
    (* note that second argument is an 'a tree?, not the more abstract 'a m *)
    (* type is ('a -> 'b W) -> 'a tree? -> 'b tree? W == 'b treeT(W) *)
    val distribute : ('a -> ('x,'b) Wrapped.m) -> 'a tree option -> ('x,'b) m
    val expose : ('x,'a) m -> ('x,'a tree option) Wrapped.m
  end
end
