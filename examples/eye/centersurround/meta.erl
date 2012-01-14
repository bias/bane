-module(meta).
-export([eval/2]).

eval(String, Bindings)->
    {ok, ErlTokens, _EndLocation} = erl_scan:string(String),
    {ok, ErlAbsForm} = erl_parse:parse_exprs(ErlTokens),
    {value, _Value, _NewBindings} = erl_eval:exprs(ErlAbsForm, Bindings).
