module type Value =
  sig
    type t
    val zero : t
    val unit : t
    val symb : t
    val neg : t -> t
    val est_zero : t -> bool
    val depuis_texte : string -> t
    val vers_texte : t -> string
    val additioner : t -> t -> t
    val soustraire : t -> t -> t
    val diviser : t -> t -> t
    val multiplier : t -> t -> t
    val print : t -> unit
  end
module Generic_matrix :
  functor (V : Value) ->
    sig
      type t = V.t array array
      type solution_equation =
          Erreur of string
        | Solution_systeme of (string * V.t) list
      val init : int -> int -> V.t array array
      val __protect : 'a array array -> bool
      val size : 'a array array -> int * int
      val foreach :
        ?line:('a array -> unit) ->
        'a array array -> (int -> int -> 'a -> unit) -> unit
      val print : V.t array array -> unit
      val foreach2 :
        ('a -> 'b -> V.t) ->
        'a array array -> 'b array array -> V.t array array
      val __vide_vers_identite : V.t array array -> V.t array array
      val identite : int -> V.t array array
      val additioner : V.t array array -> V.t array array -> V.t array array
      val soustraire : V.t array array -> V.t array array -> V.t array array
      val multiplier : V.t array array -> V.t array array -> V.t array array
      val multiplier_scalaire : V.t array array -> V.t -> V.t array array
      module Operation_elementaires :
        sig
          val multiplier_ligne : V.t array -> V.t -> V.t array
          val additioner_ligne : V.t array -> V.t array -> V.t array
          val neg_ligne : V.t array -> V.t array
          val echanger_ligne : 'a array -> int -> int -> unit
          val affecter_ligne : 'a array -> int -> 'a -> unit
        end
      val inverser : V.t array array -> V.t array array
      val solveur : string array array -> int -> int -> string array -> solution_equation
    end
module Test_int :
  sig
    type t = int
    val zero : int
    val unit : int
    val symb : int
    val neg : int -> int
    val est_zero : int -> bool
    val depuis_texte : string -> int
    val vers_texte : int -> string
    val additioner : int -> int -> int
    val soustraire : int -> int -> int
    val diviser : int -> int -> int
    val multiplier : int -> int -> int
    val print : int -> unit
  end
module Test_int_matrix :
  sig
    type t = Test_int.t array array
    type solution_equation =
        Erreur of string
      | Solution_systeme of (string * Test_int.t) list
    val init : int -> int -> Test_int.t array array
    val __protect : 'a array array -> bool
    val size : 'a array array -> int * int
    val foreach :
      ?line:('a array -> unit) ->
      'a array array -> (int -> int -> 'a -> unit) -> unit
    val print : Test_int.t array array -> unit
    val foreach2 :
      ('a -> 'b -> Test_int.t) ->
      'a array array -> 'b array array -> Test_int.t array array
    val __vide_vers_identite :
      Test_int.t array array -> Test_int.t array array
    val identite : int -> Test_int.t array array
    val additioner :
      Test_int.t array array ->
      Test_int.t array array -> Test_int.t array array
    val soustraire :
      Test_int.t array array ->
      Test_int.t array array -> Test_int.t array array
    val multiplier :
      Test_int.t array array ->
      Test_int.t array array -> Test_int.t array array
    val multiplier_scalaire :
      Test_int.t array array -> Test_int.t -> Test_int.t array array
    module Operation_elementaires :
      sig
        val multiplier_ligne :
          Test_int.t array -> Test_int.t -> Test_int.t array
        val additioner_ligne :
          Test_int.t array -> Test_int.t array -> Test_int.t array
        val neg_ligne : Test_int.t array -> Test_int.t array
        val echanger_ligne : 'a array -> int -> int -> unit
        val affecter_ligne : 'a array -> int -> 'a -> unit
      end
    val inverser : Test_int.t array array -> Test_int.t array array
  end
module Test_float :
  sig
    type t = float
    val zero : float
    val unit : float
    val symb : float
    val neg : float -> float
    val est_zero : float -> bool
    val depuis_texte : string -> float
    val vers_texte : float -> string
    val additioner : float -> float -> float
    val soustraire : float -> float -> float
    val diviser : float -> float -> float
    val multiplier : float -> float -> float
    val print : float -> unit
  end
module Test_float_matrix :
  sig
    type t = Test_float.t array array
    type solution_equation =
        Erreur of string
      | Solution_systeme of (string * Test_float.t) list
    val init : int -> int -> Test_float.t array array
    val __protect : 'a array array -> bool
    val size : 'a array array -> int * int
    val foreach :
      ?line:('a array -> unit) ->
      'a array array -> (int -> int -> 'a -> unit) -> unit
    val print : Test_float.t array array -> unit
    val foreach2 :
      ('a -> 'b -> Test_float.t) ->
      'a array array -> 'b array array -> Test_float.t array array
    val __vide_vers_identite :
      Test_float.t array array -> Test_float.t array array
    val identite : int -> Test_float.t array array
    val additioner :
      Test_float.t array array ->
      Test_float.t array array -> Test_float.t array array
    val soustraire :
      Test_float.t array array ->
      Test_float.t array array -> Test_float.t array array
    val multiplier :
      Test_float.t array array ->
      Test_float.t array array -> Test_float.t array array
    val multiplier_scalaire :
      Test_float.t array array -> Test_float.t -> Test_float.t array array
    module Operation_elementaires :
      sig
        val multiplier_ligne :
          Test_float.t array -> Test_float.t -> Test_float.t array
        val additioner_ligne :
          Test_float.t array -> Test_float.t array -> Test_float.t array
        val neg_ligne : Test_float.t array -> Test_float.t array
        val echanger_ligne : 'a array -> int -> int -> unit
        val affecter_ligne : 'a array -> int -> 'a -> unit
      end
    val inverser : Test_float.t array array -> Test_float.t array array
  end
