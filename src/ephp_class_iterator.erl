-module(ephp_class_iterator).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    get_class/0
]).

get_class() ->
    #class{
        name = <<"Iterator">>,
        implements = [<<"Traversable">>],
        type = interface,
        methods = [
            #class_method{name = <<"current">>},
            #class_method{name = <<"key">>},
            #class_method{name = <<"next">>},
            #class_method{name = <<"rewind">>},
            #class_method{name = <<"valid">>}
        ]
    }.
