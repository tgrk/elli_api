-define(APP, ellija).

%% type conversion helpers
-define(i2l(I),  integer_to_list(I)).
-define(l2i(L),  list_to_integer(L)).
-define(b2i(B),  list_to_integer(binary_to_list(B))).
-define(b2l(B),  binary_to_list(B)).
-define(b2f(B),  list_to_float(binary_to_list(B))).
-define(f2b(F),  list_to_binary(float_to_list(F))).
-define(i2b(I),  list_to_binary(integer_to_list(I))).
-define(i2a(A),  list_to_atom(integer_to_list(A))).
-define(b2a(B),  list_to_atom(binary_to_list(B))).
-define(a2b(A),  atom_to_binary(A, latin1)).
-define(a2l(A),  atom_to_list(A)).
-define(l2b(L),  list_to_binary(L)).
-define(l2t(L),  list_to_tuple(L)).
-define(l2a(L),  list_to_atom(L)).
-define(l2f(L),  list_to_float(L)).
-define(io2b(L), iolist_to_binary(L)).

-record(config, {
                    host       :: binary(),
                    port       :: pos_integer(),
                    headers    :: list(),
                    routes     :: list(),
                    middleware :: map()
                }
).