-module(ephp_class_arrayaccess).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    get_class/0
]).

get_class() ->
    #class{
        name = <<"ArrayAccess">>,
        type = interface,
        methods = [
            #class_method{
                name = <<"offsetExists">>,
                args = [#variable{name = <<"offset">>}]
            },
            #class_method{
                name = <<"offsetGet">>,
                args = [#variable{name = <<"offset">>}]
            },
            #class_method{
                name = <<"offsetSet">>,
                args = [#variable{name = <<"offset">>},
                        #variable{name = <<"value">>}]
            },
            #class_method{
                name = <<"offsetUnset">>,
                args = [#variable{name = <<"offset">>}]
            }
        ]
    }.
