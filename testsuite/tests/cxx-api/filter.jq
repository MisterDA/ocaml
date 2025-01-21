# filter reports
.[] | select(

# only errors and warnings

(.kind != "note")


# In file included from all-includes.h:34:
# /Users/antonin/Tarides/ocaml/trunk/runtime/caml/addrmap.h:27:41: error: declaration of 'value addrmap_entry::value' changes meaning of 'value' [-Wchanges-meaning]
#    27 | struct addrmap_entry { value key; value value; };
#       |                                         ^~~~~
# /Users/antonin/Tarides/ocaml/trunk/runtime/caml/addrmap.h:27:35: note: used here to mean 'typedef intnat value'
#    27 | struct addrmap_entry { value key; value value; };
#       |                                   ^~~~~
# In file included from /Users/antonin/Tarides/ocaml/trunk/runtime/caml/custom.h:20:
# /Users/antonin/Tarides/ocaml/trunk/runtime/caml/mlvalues.h:59:16: note: declared here
#    59 | typedef intnat value;
#       |                ^~~~~

and (.option != "-Wchanges-meaning")


# Flexible Array Members (FAM) are a C99 feature but not standard C++. It's a
# common extension.
#
# In file included from all-includes.h:50:
# /Users/antonin/Tarides/ocaml/trunk/runtime/caml/bigarray.h:86:10: error: ISO C++ forbids flexible array member 'dim' [-Werror=pedantic]
#    86 |   intnat dim[/* num_dims */]; /* Size in each dimension */
#       |          ^~~

and (.message | contains("flexible array member") | not)

)
